module Burn.Cli where

import Burn.API
import Burn.Client
import Burn.Optparse
import Burn.Server (Payload(..), handlers)
import Burn.Statistics
import Burn.Storage
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Default
import Data.Foldable
import Data.Monoid
import Data.String
import Data.Text as T
import Data.Text.IO as T
import Data.Time
import Data.Vector as V
import Formatting
import Formatting.Time
import Network.HTTP.Client
import Network.Wai.Handler.Warp hiding (Settings)
import Servant.Client
import Servant.Server

executeCommand :: ClientEnv -> Command -> IO ()
executeCommand env command = do
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
    env <- hostPortClientEnv $ ca ^. caHostPort
    for_ commands $ \c -> do
      executeCommand env c

-- printStatResult :: StatsResult -> IO ()
-- printStatResult sr = T.putStrLn t
--   where
--     t = day <> ": " <> results
--     day = T.pack $ show $ sr ^. srTime
--     ls = sr ^. srStatData . _SDSummary
--     results
--       = (m " sum: " $ ls ^. sSum . re _Just)
--       <> (m " min: " $ ls ^. sMinLen)
--       <> (m " max: " $ ls ^. sMaxLen)
--       <> (m " median: " $ ls ^. sMedian)
--       <> (sformat (" count: " % shown) $ ls ^. sCount)
--     m t v = maybe "" (sformat (t % hms) . timeToTimeOfDay . realToFrac) v


runBurnStats :: StatArgs -> IO ()
runBurnStats (StatArgs s q) = do
  p <- loadPomodors $ s ^. sDataFile -- FIXME: from options
  printStatsQuery (s ^. sDayEnd) q p

burnCli :: Args -> IO ()
burnCli = \case
  Server h -> runBurnServer h
  Client c -> runBurnClient c
  Statistics s -> runBurnStats s
