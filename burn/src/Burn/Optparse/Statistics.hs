module Burn.Optparse.Statistics where

import Burn.Optparse.Settings
import Burn.Types
import Control.Lens
import Data.Default
import Data.Monoid
import Data.Time
import Data.Traversable
import Options.Applicative


-- data StatFold
--   = StatSimple
--     -- ^ Fold collection to simple statistics set
--   | StatHistro

data StatQuery
  = StatDaily
    -- ^ Daily simple statistics, like mean, median and such stuff
  | StatWeekly
    -- ^ Weekly simple statistics
  | StatHourly
    -- ^ Simple hourly statistics

data StatArgs = StatArgs
  { _sSettings :: Settings
  , _sQuery    :: StatQuery
  }

makeLenses ''StatArgs

statQuery :: Parser StatQuery
statQuery = option parseStatistics
  $ long "query" <> short 'q' <> help "Statistics query"
  where
    -- FIXME: implement the parser
    parseStatistics = str >>= \case
      "daily" -> return StatDaily
      "weekly" -> return StatWeekly
      "hourly" -> return StatHourly

statArgs :: Parser StatArgs
statArgs = StatArgs <$> settings <*> statQuery
