module Burn.Storage where

import Burn.Types
import Control.Arrow
import Control.Lens
import Data.ByteString.Lazy as BL
import Data.Csv
import Data.List as L
import Data.Map.Strict as M
import Data.Time
import Data.Vector (Vector)
import System.Directory

loadPomodors :: FilePath -> IO (Vector (PomodoroData ZonedTime))
loadPomodors fp' = do
  fp <- canonicalizePath fp'
  bl <- BL.readFile fp
  either fail return $ decode NoHeader bl

savePomodoros :: FilePath -> [PomodoroData ZonedTime] -> IO ()
savePomodoros fp' pomodors = do
  fp <- canonicalizePath fp'
  BL.appendFile fp $ encode pomodors

splitPomodoros
  :: TimeOfDay
     -- ^ Day end
  -> [PomodoroData ZonedTime]
  -> Map Day [PomodoroData ZonedTime]
splitPomodoros eod
  = M.fromListWith (++)
  . L.map (views pdStarted (timeDay eod) &&& (:[]))

posixDayLength :: DiffTime
posixDayLength = fromInteger 86400

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
