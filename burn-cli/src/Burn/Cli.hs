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
import Control.Monad.Trans
import Data.Default
import Data.Foldable
import Data.List as L
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
import Prelude as P
import Servant.Client
import Servant.Server

executeCommand :: ClientEnv -> Command -> IO ()
executeCommand env command = do
  void $ either throwIO return =<< case command of
    CPomodoro -> runClientM startPomodoro env
    CPause -> runClientM startPause env
    CSetTags (Tags tags) -> runClientM (setTags tags) env

initPayload :: Settings -> IO Payload
initPayload settings = do
  zt <- getZonedTime
  let
    now = zonedTimeToUTC zt
    eod = settings ^. #dayEnd
    day = timeDay eod zt
  pomodors <- case settings ^. #dataFile of
    Just p -> loadPomodors p
    Nothing -> pure mempty
  let
    pMap = splitPomodoros eod $ V.toList pomodors
    todayZoned = pMap ^.. ix day . folded
    todayUtc = over (traversed . #started) zonedTimeToUTC todayZoned
    state = mkServerState now & #todayPomodoros .~ todayUtc
  print todayZoned
  Payload <$> newTVarIO state <*> newTVarIO settings

runBurnServer :: ServerArgs -> IO ()
runBurnServer args = do
  payload <- initPayload $ args ^. #settings
  let
    host = fromString $ args ^. #hostPort . #host
    port = args ^. #hostPort . #port
    s = defaultSettings & setPort port & setHost host
  runSettings s $ serve burnAPI $ handlers payload

runBurnClient :: ClientArgs -> IO ()
runBurnClient ca = case ca ^. #commands of
  [] -> fail "List of commands must be non empty"
  commands -> do
    env <- hostPortClientEnv $ ca ^. #hostPort
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
  p <- case s ^. #dataFile of
    Just f -> loadPomodors f
    Nothing -> pure mempty
  printStatsQuery (s ^. #dayEnd) q p

-- runElastic :: ElasticArgs -> IO ()
-- runElastic es = do
--   p <- loadPomodors $ es ^. esDataFile
--   let ids    = IndexDocumentSettings NoVersionControl Nothing
--       server = es ^. esElasticServer
--   withBH defaultManagerSettings server $ do

--     for_ (L.zip (V.toList p) [0..]) $ \(pomodoro, docId) -> do
--       reply <- indexDocument (es ^. esIndexName) (MappingName "pomodoros") ids
--         (elasticPomodoro (es ^. esDayEnd) pomodoro)
--         (DocId $ T.pack $ show docId)
--       lift $ do
--         unless (isSuccess reply)
--           $ print reply
--         when (docId `mod` 100 == 0)
--           $ P.putStrLn $ "Uploaded " <> show docId <> " documents"

burnCli :: Args -> IO ()
burnCli = \case
  Server h -> runBurnServer h
  Client c -> runBurnClient c
  Statistics s -> runBurnStats s
  -- Elastic es -> runElastic es
