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
