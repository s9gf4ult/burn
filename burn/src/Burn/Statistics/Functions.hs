module Burn.Statistics.Functions where

import Burn.Statistics.Types
import Burn.Types
import Control.Foldl as FL
import Control.Lens hiding (Fold)
import Data.Foldable as F
import Data.List as L
import Data.Set as S
import Data.Text as T
import Data.Time
import Data.Vector as V
import Statistics.Sample.Histogram

-- calculateStats
--   :: (Foldable f)
--   => Int
--      -- ^ Histrogram ranges
--   -> f NominalDiffTime
--   -> LenStatistics
-- calculateStats ranges = FL.fold go
--   where
--     go = LenStatistics
--       <$> FL.sum
--       <*> FL.minimum
--       <*> FL.maximum
--       <*> FL.length

-- histroFold :: Int -> Fold Double (Maybe (V.Vector Int))
-- histroFold ranges = Fold (flip (:)) [] go
--   where
--     go :: [Double] -> Maybe (V.Vector Int)
--     go [] = Nothing
--     go x =
--       let
--         lo = F.minimum x
--         hi = F.maximum x
--         h = histogram_ ranges lo hi $ V.fromList x
--       in Just h

-- | Calculates corrected day according to given day end
timeDay
  :: TimeOfDay
  -- ^ Day end
  -> ZonedTime
  -> Day
timeDay eod' zt =
  let
    u = zonedTimeToUTC zt
    shift = if timeOfDayToTime midnight <= eod && eod < timeOfDayToTime midday
      then owlShift
      else larkShift
    shifted = addUTCTime shift u
    shiftedZ = utcToZonedTime (zonedTimeZone zt) shifted
  in localDay $ zonedTimeToLocalTime shiftedZ
  where
    owlShift = negate $ realToFrac eod
    larkShift = realToFrac $ posixDayLength - eod
    eod = timeOfDayToTime eod'

posixDayLength :: DiffTime
posixDayLength = fromInteger 86400
