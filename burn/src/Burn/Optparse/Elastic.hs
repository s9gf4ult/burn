module Burn.Optparse.Elastic where

import Burn.Optparse.Settings
import Burn.Statistics.Functions
import Burn.TH
import Burn.Types
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Generics.Labels ()
import Data.Int
import Data.List as L
import Data.Monoid
import Data.Text as T
import Data.Time
import Options.Applicative

data ElasticPomodoro = ElasticPomodoro
  { _epTimestamp     :: String
  , _epRealDay       :: String
  , _epWeekday       :: String
  , _epHourOfDay     :: Double
  , _epTags          :: [Text]
  , _epDuration      :: Int64
  , _epDurationHours :: Double
  }

deriveJSON
  defaultOptions
  { fieldLabelModifier = toUnderscore' . L.drop 3 }
  ''ElasticPomodoro

elasticPomodoro
  :: TimeOfDay
  -- ^ Day end to calculate week day correctly
  -> PomodoroData ZonedTime
  -> ElasticPomodoro
elasticPomodoro eod p = ElasticPomodoro
  { _epTimestamp     = formatTime defaultTimeLocale "%FT%T" utc
  , _epRealDay       = formatTime defaultTimeLocale "%F" day
  , _epWeekday       = formatTime defaultTimeLocale "%u" day
  , _epHourOfDay     = hod
  , _epTags          = p ^. #pdTags . #_Tags
  , _epDuration      = duration
  , _epDurationHours = realToFrac duration * 3600
  }
  where
    duration = p ^. #pdLen . to round
    utc = zonedTimeToUTC zoned
    zoned = p ^. #pdStarted
    day = timeDay eod zoned
    hod = (realToFrac $ timeOfDayToTime $ localTimeOfDay $ zonedTimeToLocalTime zoned) / 3600

-- data ElasticArgs = ElasticArgs
--   { _esDataFile      :: FilePath
--   , _esElasticServer :: Server
--   , _esIndexName     :: IndexName
--   , _esDayEnd        :: TimeOfDay
--   } deriving (Eq, Show)

-- makeLenses ''ElasticArgs

-- elasticArgs :: Parser ElasticArgs
-- elasticArgs = ElasticArgs
--   <$> pomodorosFile
--   <*> elasticServer
--   <*> elasticIndex
--   <*> dayEnd

-- elasticServer :: Parser Server
-- elasticServer = (Server . T.pack) <$> strOption mods
--   where
--     mods = long "elastic-server" <> short 'e'
--       <> help "Elastic Search server address, default is http://localhost:9200"
--       <> value "http://localhost:9200"

-- elasticIndex :: Parser IndexName
-- elasticIndex = (IndexName . T.pack) <$> strOption mods
--   where
--     mods = long "index" <> short 'i'
--       <> help "Index name for Elastic Search"
--       <> value "burn"
