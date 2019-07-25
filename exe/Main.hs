{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}

import           Control.Applicative ((<|>))
import           Data.Aeson          (eitherDecodeStrict)
import qualified Data.ByteString     as BS
import           Data.Foldable       (traverse_)
import           Data.List           (intersect, nub, (\\))
import           Data.List.Extra     (groupSort)
import           Data.List.NonEmpty  (NonEmpty ((:|)), toList)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import           Data.Maybe          (maybeToList)
import           Data.Text           (Text, intercalate, map, pack)
import           Data.Text.IO        (putStrLn)
import           Jaeger.Data
import           Jaeger.Interval
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
  let processes = if qualify then buildProcesses dat else Map.empty
      spans = buildLookup dat
      stacks = buildFlames processes spans >>=
             buildStacks wall ignoreTags annotated
  traverse_ (putStrLn . drawStack) stacks

data Options = Options
  { input      :: Input
  , ignoreTags :: [Text]
  , annotated  :: [ProcessID]
  , qualify    :: Bool
  , wall       :: Bool
  }
data Input = FileInput FilePath | StdInput
optionsParser :: Opts.Parser Options
optionsParser = Options
  <$> (file <|> pure StdInput)
  <*> Opts.many tag
  <*> Opts.many ann
  <*> qual
  <*> wall
  where
    file = FileInput <$> Opts.strOption
             (  Opts.long "file"
             <> Opts.short 'f'
             <> Opts.metavar "FILENAME"
             <> Opts.help "Input file")
    tag = Opts.strOption
             (  Opts.short 'i'
             <> Opts.long "ignore"
             <> Opts.metavar "TAG-KEY"
             <> Opts.help "Ignore spans with this tag key")
    ann = ProcessID <$> Opts.strOption
             (  Opts.short 'a'
             <> Opts.long "annotated"
             <> Opts.metavar "ANN"
             <> Opts.help "Annotate this process when using the `chain` palette")
    qual = Opts.switch
             (  Opts.short 'q'
             <> Opts.long "qualify"
             <> Opts.help "Qualify span names by their process")
    wall = Opts.switch
             (  Opts.short 'w'
             <> Opts.long "walltime"
             <> Opts.help "Takes start/end times of children into account when calculating times.")

type Processes = Map (TraceID, ProcessID) Process

buildProcesses :: [Data] -> Processes
buildProcesses dats = Map.fromList $
  do Data{..} <- dats
     (pid, p) <- Map.toList processes
     pure ((traceID, pid), p)

type Lookup = [(Reference, Span)]

buildLookup :: [Data] -> Lookup
buildLookup dat = do (Data t ss _) <- dat
                     do s @ Span{..} <- ss
                        pure (Reference t spanID, s)

data Flame = Flame
  { time     :: Interval
  , name     :: Name
  , process  :: ProcessID
  , children :: [Flame]
  , tags     :: [Text]
  }

selftime :: Flame -> Integer
selftime f = max 0 $ (width $ time f) - (measure $ time <$> children f)

walltime :: Flame -> Integer
walltime f = end' - start' - (measure $ time <$> children f)
  where family = f : children f
        -- only start/end times from immediate children to avoid accumulation
        start' = minimum $ intervalStart . time <$> family
        end'   = maximum $ intervalEnd . time <$> family

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
buildFlames :: Processes -> Lookup -> [Flame]
buildFlames procs ss = build <$>
                       (maybeToList . lookupSpan =<< (nub $ roots ++ orphans))
  where
    roots :: [Reference]
    roots = fst <$> filter (\ (_, Span{..}) -> null references) ss
    orphans :: [Reference]
    orphans = Set.foldl' f [] absentees
      where absentees  = Map.keysSet children `Set.difference` Map.keysSet spans
            f os absent = maybe os (os ++) $ Map.lookup absent children
    spans :: Map Reference Span
    spans = Map.fromList ss
    children :: Map Reference [Reference]
    children = Map.fromList $ groupSort $ do (i, Span{..}) <- ss
                                             (, i) <$> references
    lookupSpan :: Reference -> Maybe (Reference, Span)
    lookupSpan ref = (ref,) <$> (Map.lookup ref spans)
    build (i, Span{..}) = Flame (interval startTime (startTime + duration))
                                (qualifiedName operationName (traceID, processID))
                                processID
                                (build <$> deps i)
                                (key <$> tags)
    deps i = do refs <- maybeToList $ Map.lookup i children
                ref  <- refs
                maybeToList $ lookupSpan ref
    qualifiedName (Name orig) pid = case Map.lookup pid procs of
      Nothing -> Name orig
      Just p  -> Name $ orig <> "..." <> (serviceName p)

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

buildStacks :: Bool -> [Text] -> [ProcessID] -> Flame -> [Stack]
buildStacks wall banned annotate = stacks []
  where
    stacks :: [Name] -> Flame -> [Stack]
    stacks parents f @ Flame{..} =
      if not . null $ intersect banned tags
      then []
      else Stack (name :| parents) t (elem process annotate) :
           (stacks (name : parents) =<< children)
           where t = if wall then walltime f else selftime f

drawStack :: Stack -> Text
drawStack Stack{..} = intercalate ";" ((\(Name n) -> map cleanup n) <$>
                                       (reverse . toList $ frames))
                      <> ann <> " " <> (pack . show $ samples)
  where cleanup ' ' = '.'
        cleanup ';' = '.'
        cleanup '[' = '.'
        cleanup ']' = '.'
        cleanup c   = c
        ann = pack $ if annotated then "_[w]" else ""
