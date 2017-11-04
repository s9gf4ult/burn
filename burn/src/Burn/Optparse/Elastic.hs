module Burn.Optparse.Elastic where

import Burn.Optparse.Settings
import Burn.Types
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Int
import Data.List as L
import Data.Monoid
import Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Database.V5.Bloodhound.Types
import Options.Applicative
import Text.Inflections

data ElasticPomodoro = ElasticPomodoro
  { _epTimestamp :: POSIXTime
  , _epTags      :: [Text]
  , _epDuration  :: Int64
  }

deriveJSON
  defaultOptions
  { fieldLabelModifier = toUnderscore . L.drop 3 }
  ''ElasticPomodoro

elasticPomodoro :: PomodoroData UTCTime -> ElasticPomodoro
elasticPomodoro p = ElasticPomodoro
  { _epTimestamp = p ^. pdStarted . to utcTimeToPOSIXSeconds
  , _epTags      = p ^. pdTags . _Tags
  , _epDuration  = p ^. pdLen . to round
  }

data ElasticArgs = ElasticArgs
  { _esDataFile      :: FilePath
  , _esElasticServer :: Server
  , _esIndexName     :: IndexName
  } deriving (Eq, Show)

makeLenses ''ElasticArgs

elasticArgs :: Parser ElasticArgs
elasticArgs = ElasticArgs
  <$> pomodorosFile
  <*> elasticServer
  <*> elasticIndex

elasticServer :: Parser Server
elasticServer = (Server . T.pack) <$> strOption mods
  where
    mods = long "elastic-server" <> short 'e'
      <> help "Elastic Search server address, default is http://localhost:9200"
      <> value "http://localhost:9200"

elasticIndex :: Parser IndexName
elasticIndex = (IndexName . T.pack) <$> strOption mods
  where
    mods = long "index" <> short 'i'
      <> help "Index name for Elastic Search"
      <> value "burn"
