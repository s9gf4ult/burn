module Burn.Cli where

import Burn.API
import Burn.Client
import qualified Burn.Optparse as Opt
import Burn.Server (Payload(..), handlers)
import Burn.Statistics
import Burn.Storage
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Data.Foldable
import qualified Data.List as L
import Data.Monoid
import Data.String
import qualified Data.Text as T
import Data.Time
import Data.Vector as V
import Database.Bloodhound
import Network.HTTP.Client
import Network.Wai.Handler.Warp hiding (Settings)
import Prelude as P
import Servant.Client
import Servant.Server

executeCommand :: ClientEnv -> Opt.Command -> IO ()
executeCommand env command = do
  void $ either throwIO return =<< case command of
    Opt.Pomodoro -> runClientM startPomodoro env
    Opt.Pause -> runClientM startPause env
    Opt.SetTags (Tags tagsList) -> runClientM (setTags tagsList) env
    Opt.SetOption opt -> runClientM (setOption opt) env

initPayload :: Settings -> IO Payload
initPayload initSettings = do
  zt <- getZonedTime
  let
    now = zonedTimeToUTC zt
    eod = initSettings ^. #dayEnd
    day = timeDay eod zt
  pomodors <- case initSettings ^. #dataFile of
    Just p -> loadPomodors p
    Nothing -> pure mempty
  let
    pMap = splitPomodoros eod $ V.toList pomodors
    todayZoned = pMap ^.. ix day . folded
    todayUtc = over (traversed . #started) zonedTimeToUTC todayZoned
    serverState = mkServerState now (toTimerSettings initSettings)
      & #todayPomodoros .~ todayUtc
  print todayZoned
  Payload <$> newTVarIO serverState <*> newTVarIO initSettings

runBurnServer :: Opt.ServerArgs -> IO ()
runBurnServer args = do
  payload <- initPayload $ args ^. #settings
  let
    eHost = fromString $ args ^. #hostPort . #host
    ePort = args ^. #hostPort . #port
    s = defaultSettings & setPort ePort & setHost eHost
  runSettings s $ serve burnAPI $ handlers payload

runBurnClient :: Opt.ClientArgs -> IO ()
runBurnClient ca = case ca ^. #commands of
  [] -> fail "List of commands must be non empty"
  commands -> do
    env <- hostPortClientEnv $ ca ^. #hostPort
    for_ commands $ \c -> do
      executeCommand env c

runBurnStats :: Opt.StatArgs -> IO ()
runBurnStats (Opt.StatArgs s q) = do
  p <- case s ^. #dataFile of
    Just f -> loadPomodors f
    Nothing -> pure mempty
  printStatsQuery (s ^. #dayEnd) q p

runElastic :: Opt.ElasticArgs -> IO ()
runElastic es = do
  p <- loadPomodors $ es ^. #dataFile
  let server = es ^. #server
  withBH defaultManagerSettings server $ do

    for_ (L.zip (V.toList p) [0::Integer .. ]) $ \(pomodoro, docId) -> do
      reply <- indexDocument (es ^. #indexName) defaultIndexDocumentSettings
        (Opt.elasticPomodoro (es ^. #dayEnd) pomodoro)
        (DocId $ T.pack $ show docId)
      lift $ do
        unless (isSuccess reply)
          $ print reply
        when (docId `mod` 100 == 0)
          $ P.putStrLn $ "Uploaded " <> show docId <> " documents"

burnCli :: Opt.Args -> IO ()
burnCli = \case
  Opt.Server h -> runBurnServer h
  Opt.Client c -> runBurnClient c
  Opt.Statistics s -> runBurnStats s
  Opt.Elastic es -> runElastic es
