module Burn.Statistics.Query where

import Burn.Optparse.Statistics
import Burn.Statistics.Functions
import Burn.Statistics.Types
import Burn.Types
import Control.Arrow
import Control.Lens
import Data.Foldable as F
import Data.List as L
import Data.Map.Strict as M
import Data.Time

splitPomodoros
  :: TimeOfDay
     -- ^ Day end
  -> [PomodoroData ZonedTime]
  -> Map Day [PomodoroData ZonedTime]
splitPomodoros eod
  = M.fromListWith (++)
  . L.map (views pdStarted (timeDay eod) &&& (:[]))

execStatsQuery
  :: (Foldable f)
  => TimeOfDay
  -> StatQuery
  -> f (PomodoroData ZonedTime)
  -> Maybe [StatsResult]
execStatsQuery eod q pd = do
  let
    grouped = splitPomodoros eod $ F.toList pd
  beg <- grouped ^? to M.toAscList . _head . _1
  end <- grouped ^? to M.toDescList . _head . _1
  let
    fromDay = calculateDaySpec beg end $ q ^. sqFromTo . fFrom
    toDay = calculateDaySpec beg end $ q ^. sqFromTo . fTo
    ranged = grouped ^.. to M.toList . folded
      . filtered (views _1 $ \day -> fromDay <= day && day <= toDay)
    -- FIXME: implement stats calc
    res = flip fmap ranged $ \(day, pom) ->
      StatsResult day $ SDSummary $ calculateStats 5
      $ pom ^.. folded . pdLen
  return res
