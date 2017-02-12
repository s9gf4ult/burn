module Burn.Cli where

import Burn.API
import Burn.Client
import Burn.Optparse
import Burn.Server (Payload(..), handlers)
import Burn.Storage
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Foldable
import Data.String
import Data.Time
import Data.Vector as V
import Network.HTTP.Client
import Network.Wai.Handler.Warp hiding (Settings)
import Servant.Client
import Servant.Server

executeCommand :: Manager -> HostPort -> Command -> IO ()
executeCommand m hp command = do
  let
    host    = hp ^. hpHost
    port    = hp ^. hpPort
    baseUri = BaseUrl Http host port ""
    env     = ClientEnv m baseUri
  void $ either throwIO return =<< case command of
    Pomodoro -> runClientM startPomodoro env
    Pause -> runClientM startPause env

initPayload :: Settings -> IO Payload
initPayload settings = do
  zt <- getZonedTime
  let
    now = zonedTimeToUTC zt
    eod = settings ^. sDayEnd
    day = timeDay eod zt
  pomodors <- loadPomodors $ settings ^. sDataFile
  let
    pMap = splitPomodoros eod $ V.toList pomodors
    todayZoned = pMap ^.. ix day . folded
    todayUtc = over (traversed . pdStarted) zonedTimeToUTC todayZoned
    state = mkServerState now & sTodayPomodors .~ todayUtc
  print todayZoned
  Payload <$> newTVarIO state <*> newTVarIO settings

runBurnServer :: ServerArgs -> IO ()
runBurnServer args = do
  payload <- initPayload $ args ^. saSettings
  let
    host = fromString $ args ^. saHostPort . hpHost
    port = args ^. saHostPort . hpPort
    s = defaultSettings & setPort port & setHost host
  runSettings s $ serve burnAPI $ handlers payload

runBurnClient :: ClientArgs -> IO ()
runBurnClient ca = case ca ^. caCommands of
  [] -> fail "List of commands must be non empty"
  commands -> do
    m <- newManager defaultManagerSettings
    for_ commands $ \c -> do
      executeCommand m (ca ^. caHostPort) c

burnCli :: Args -> IO ()
burnCli = \case
  Server h -> runBurnServer h
  Client c -> runBurnClient c
