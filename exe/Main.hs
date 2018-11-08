{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TupleSections              #-}

import           Control.Applicative ((<|>))
import           Data.Aeson          (FromJSON, eitherDecodeStrict, parseJSON,
                                      withObject, (.:))
import qualified Data.ByteString     as BS
import           Data.Foldable       (traverse_)
import           Data.List           (intersect, nub, (\\))
import           Data.List.Extra     (groupSort)
import           Data.List.NonEmpty  (NonEmpty ((:|)), toList)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (maybeToList)
import           Data.Text           (Text, intercalate, map, pack)
import           Data.Text.IO        (putStrLn)
import           GHC.Generics        (Generic)
import           Interval            (Interval, interval, measure, width)
import qualified Options.Applicative as Opts
import           Prelude             hiding (map, putStrLn)

main :: IO ()
main = do
  Options{..} <- Opts.execParser $
                   Opts.info (Opts.helper <*> optionsParser) Opts.fullDesc
  text <- case input of
            FileInput file -> BS.readFile file
            StdInput       -> BS.getContents
  Jaeger dat <- either fail pure $ eitherDecodeStrict text
  let stacks = buildStacks ignoreTags annotated =<< (buildFlames . buildLookup $ dat)
  traverse_ (putStrLn . drawStack) stacks

data Options = Options
  { input      :: Input
  , ignoreTags :: [Tag]
  , annotated  :: [ProcessID]
  }
data Input = FileInput FilePath | StdInput
optionsParser :: Opts.Parser Options
optionsParser = Options
  <$> (file <|> pure StdInput)
  <*> Opts.many tag
  <*> Opts.many ann
  where
    file = FileInput <$> Opts.strOption
             (  Opts.long "file"
             <> Opts.short 'f'
             <> Opts.metavar "FILENAME"
             <> Opts.help "Input file")
    tag = Tag <$> Opts.strOption
             (  Opts.short 'i'
             <> Opts.long "ignore"
             <> Opts.metavar "TAG-KEY"
             <> Opts.help "Ignore spans with this tag key")
    ann = ProcessID <$> Opts.strOption
             (  Opts.short 'a'
             <> Opts.long "annotated"
             <> Opts.metavar "ANN"
             <> Opts.help "Annotate this process when using the `chain` palette")

newtype Jaeger = Jaeger [Data]
instance FromJSON Jaeger where
  parseJSON = withObject "Jaeger" $ \v -> Jaeger <$> v .: "data"

newtype TraceID   = TraceID Text deriving newtype (Eq, Ord, FromJSON)
newtype SpanID    = SpanID  Text deriving newtype (Eq, Ord, FromJSON)
newtype ProcessID = ProcessID Text deriving newtype (Eq, FromJSON)
newtype Name      = Name { unName :: Text } deriving newtype (Eq, FromJSON)

data Data = Data
  { traceID :: TraceID
  , spans   :: [Span]
  } deriving (Generic, FromJSON)

data Span = Span
  { spanID        :: SpanID
  , operationName :: Name
  , references    :: [Reference]
  , startTime     :: Integer
  , duration      :: Integer
  , tags          :: [Tag]
  , processID     :: ProcessID
  } deriving (Generic, FromJSON)

data Reference = Reference
  { traceID :: TraceID
  , spanID  :: SpanID
  } deriving (Eq, Ord, Generic, FromJSON)

newtype Tag = Tag
  { key :: Text
  } deriving (Eq, Generic)
    deriving anyclass (FromJSON)

type Lookup = [(Reference, Span)]

buildLookup :: [Data] -> Lookup
buildLookup dat = do (Data t ss) <- dat
                     do s @ Span{..} <- ss
                        pure (Reference t spanID, s)

data Flame = Flame
  { time     :: Interval
  , name     :: Name
  , process  :: ProcessID
  , children :: [Flame]
  , tags     :: [Tag]
  }

selftime :: Flame -> Integer
selftime f = max 0 $ (width $ time f) - (measure $ time <$> children f)

-- We only support one parent per span.
--
-- https://github.com/opentracing/opentracing.io/issues/28
--
-- With multiple parents, a parent span can end at a point in time before a
-- child span. For example, in the case of doing a write which later triggers a
-- flush, the write might finish long before the flush even starts. This makes
-- it impossible to treat spans as a flame graph or traditional stack trace,
-- like you can in a single-parent world. This may make writing a GUI harder
-- since you can't do certain flame-graph-like visualizations.
buildFlames :: Lookup -> [Flame]
buildFlames ss = build <$> (maybeToList . lookupSpan =<< (nub $ roots ++ orphans))
  where
    roots :: [Reference]
    roots = fst <$> filter (\ (_, Span{..}) -> null references) ss
    orphans :: [Reference]
    orphans = do absent <- (Map.keys children) \\ (Map.keys spans)
                 concat $ maybeToList $ Map.lookup absent children
    spans :: Map Reference Span
    spans = Map.fromList ss
    children :: Map Reference [Reference]
    children = Map.fromList $ groupSort $ do (i, Span{..}) <- ss
                                             (, i) <$> references
    lookupSpan :: Reference -> Maybe (Reference, Span)
    lookupSpan ref = (ref,) <$> (Map.lookup ref spans)
    build (i, Span{..}) = Flame (interval startTime (startTime + duration))
                                operationName
                                processID
                                (build <$> deps i)
                                tags
    deps i = do refs <- maybeToList $ Map.lookup i children
                ref  <- refs
                maybeToList $ lookupSpan ref

-- https://github.com/brendangregg/FlameGraph/blob/master/flamegraph.pl
--
-- The input is stack frames and sample counts formatted as single lines.  Each
-- frame in the stack is semicolon separated, with a space and count at the end
-- of the line. Example input:
--
--  swapper;start_kernel;rest_init;cpu_idle;default_idle;native_safe_halt 1
--
-- The input functions can optionally have annotations at the end of each
-- function name, following a precedent by some tools (Linux perf's _[k]):
--
--   _[k] for kernel
--   _[i] for inlined
--   _[j] for jit
--   _[w] for waker
--
-- They are used merely for colors by some palettes, eg, flamegraph.pl
-- --color=java.

data Stack = Stack
  { frames    :: NonEmpty Name -- children (head) followed by parents (tail)
  , samples   :: Integer
  , annotated :: Bool
  }

buildStacks :: [Tag] -> [ProcessID] -> Flame -> [Stack]
buildStacks banned annotate = stacks []
  where
    stacks :: [Name] -> Flame -> [Stack]
    stacks parents f @ Flame{..} =
      if not . null $ intersect banned tags
      then []
      else Stack (name :| parents) (selftime f) (elem process annotate) :
           (stacks (name : parents) =<< children)

drawStack :: Stack -> Text
drawStack Stack{..} = intercalate ";" (map cleanup . unName <$>
                                       (reverse . toList $ frames))
                      <> ann <> " " <> (pack . show $ samples)
  where cleanup ' ' = '.'
        cleanup ';' = '.'
        cleanup '[' = '.'
        cleanup ']' = '.'
        cleanup c   = c
        ann = pack $ if annotated then "_[w]" else ""
