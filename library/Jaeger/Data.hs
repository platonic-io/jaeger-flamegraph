{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}

module Jaeger.Data where

import           Data.Aeson
import           Data.Map.Strict     (Map)
import           Data.Text
import           GHC.Generics        (Generic)

newtype Jaeger = Jaeger [Data]
instance FromJSON Jaeger where
  parseJSON = withObject "Jaeger" $ \v -> Jaeger <$> v .: "data"
instance ToJSON Jaeger where
  toJSON (Jaeger dat) = object ["data" .= dat]

newtype TraceID   = TraceID Text deriving newtype (Eq, Ord, FromJSON, ToJSON)
newtype SpanID    = SpanID  Text deriving newtype (Eq, Ord, FromJSON, ToJSON)
newtype ProcessID = ProcessID Text deriving newtype (Eq, Ord, FromJSON, FromJSONKey, ToJSON, ToJSONKey)
newtype Name      = Name Text deriving newtype (Eq, FromJSON, ToJSON)

data Data = Data
  { traceID   :: TraceID
  , spans     :: [Span]
  , processes :: Map ProcessID Process
  } deriving (Generic, FromJSON, ToJSON)

data Process = Process
  { serviceName :: Text
  } deriving (Generic, FromJSON, ToJSON)

data Span = Span
  { spanID        :: SpanID
  , traceID       :: TraceID
  , operationName :: Name
  , references    :: [Reference]
  , startTime     :: Integer
  , duration      :: Integer
  , tags          :: [Tag]
  , processID     :: ProcessID
  } deriving (Generic, FromJSON, ToJSON)

data Reference = Reference
  { traceID :: TraceID
  , spanID  :: SpanID
  } deriving (Eq, Ord, Generic, FromJSON, ToJSON)

data Tag = Tag
  { key   :: Text
  , value :: Value -- arbitrary content
  } deriving (Eq, Generic, FromJSON, ToJSON)
