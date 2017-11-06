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
import Database.V5.Bloodhound.Types
import Options.Applicative
import Text.Inflections

data ElasticPomodoro = ElasticPomodoro
  { _epTimestamp :: String
  , _epWeekday   :: String
  , _epHourOfDay :: Double
  , _epTags      :: [Text]
  , _epDuration  :: Int64
  }

deriveJSON
  defaultOptions
  { fieldLabelModifier = toUnderscore . L.drop 3 }
  ''ElasticPomodoro

elasticPomodoro :: PomodoroData ZonedTime -> ElasticPomodoro
elasticPomodoro p = ElasticPomodoro
  { _epTimestamp = formatTime defaultTimeLocale "%FT%T" utc
  , _epWeekday   = formatTime defaultTimeLocale "%u" zoned
  , _epHourOfDay = hod
  , _epTags      = p ^. pdTags . _Tags
  , _epDuration  = p ^. pdLen . to round
  }
  where
    utc = zonedTimeToUTC zoned
    zoned = p ^. pdStarted
    hod = (realToFrac $ timeOfDayToTime $ localTimeOfDay $ zonedTimeToLocalTime zoned) / 3600

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
