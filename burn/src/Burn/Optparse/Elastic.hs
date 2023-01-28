module Burn.Optparse.Elastic where

import Burn.Statistics.Functions
import Database.Bloodhound.Types
import Burn.Types
import Control.Lens
import Data.Aeson
import Data.Generics.Labels ()
import Data.Int
import Data.Text as T
import Data.Time
import GHC.Generics (Generic)

data ElasticPomodoro = ElasticPomodoro
  { timestamp     :: String
  , realDay       :: String
  , weekday       :: String
  , hourOfDay     :: Double
  , tags          :: [Text]
  , duration      :: Int64
  , durationHours :: Double
  } deriving (Generic, FromJSON, ToJSON)

elasticPomodoro
  :: TimeOfDay
  -- ^ Day end to calculate week day correctly
  -> PomodoroData ZonedTime
  -> ElasticPomodoro
elasticPomodoro eod p = ElasticPomodoro
  { timestamp     = formatTime defaultTimeLocale "%FT%T" utcStarted
  , realDay       = formatTime defaultTimeLocale "%F" day
  , weekday       = formatTime defaultTimeLocale "%u" day
  , hourOfDay
  , tags          = p ^. #tags . #_Tags
  , duration
  , durationHours = realToFrac duration * 3600
  }
  where
    duration = p ^. #length . to round
    utcStarted = zonedTimeToUTC zoned
    zoned = p ^. #started
    day = timeDay eod zoned
    hourOfDay = (realToFrac $ timeOfDayToTime $ localTimeOfDay $ zonedTimeToLocalTime zoned) / 3600

data ElasticArgs = ElasticArgs
  { dataFile  :: FilePath
  , server    :: Server
  , indexName :: IndexName
  , dayEnd    :: TimeOfDay
  } deriving (Eq, Show, Generic)

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
