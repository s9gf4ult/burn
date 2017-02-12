module Burn.Statistics.Query where

import Burn.Optparse.Statistics
import Burn.Statistics.Types
import Burn.Types
import Data.Time
import Data.Vector as V

execStatsQuery
  :: (Foldable f)
  => StatQuery
  -> f (PomodoroData ZonedTime)
  -> [StatsResult]
execStatsQuery q pd = (error "FIXME: ")
