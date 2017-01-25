module Burn.Cli where

import Burn.Client
import Control.Lens
import Data.Foldable
import Network.HTTP.Client
import Servant.Client

data Event = StartPomodoro | StartPause

evtStr :: String -> Either String Event
evtStr = \case
  "pomodoro" -> Right StartPomodoro
  "pause"    -> Right StartPause
  e          -> Left $ "Unknown command: " ++ e

data Command
  = Send [Event]

makePrisms ''Command

execute :: Command -> IO ()
execute command = do
  m <- newManager defaultManagerSettings
  let
    baseUri = BaseUrl Http "127.0.0.1" 1338 "" -- FIXME: get from params
    env = ClientEnv m baseUri
  case command of
    Send evts -> for_ evts $ \case
      StartPomodoro -> runClientM startPomodoro env
      StartPause    -> runClientM startPause env
