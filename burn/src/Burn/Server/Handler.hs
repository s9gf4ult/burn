module Burn.Server.Handler where

import Burn.API
import Burn.Server.Transform
import Burn.Storage
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Base
import Control.Monad.Except
import Data.Default
import Data.Time
import Data.Vector as V
import Network.Wai.Handler.Warp (run)
import Servant


data Payload = Payload
  { _pState    :: TVar ServerState
  , _pSettings :: TVar Settings
  }

makeLenses ''Payload

initPayload :: IO Payload
initPayload = do
  zt <- getZonedTime
  let
    now = zonedTimeToUTC zt
    settings = def
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

burn :: IO ()
burn = do
  p <- initPayload
  run 1338 $ serve burnAPI $ handlers p

handlers :: Payload -> Server BurnAPI
handlers p
  =    startPomodoro p
  :<|> startPause p
  :<|> setTags p
  :<|> status p

handleMessage
  :: Event
  -> Payload
  -> ExceptT ServantErr IO ServerState
handleMessage evt p = liftBase $ do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  let msg = Message now evt
  (settings, (newSt, actions)) <- atomically $ do
    state <- readTVar $ p ^. pState
    settings <- readTVar $ p ^. pSettings
    let res@(newSt, _) = process settings tz msg state
    writeTVar (p ^. pState) newSt
    return (settings, res)
  print evt
  pomodors <- (traversed . traversed) utcToLocalZonedTime
    $ actions ^.. folded . _SavePomodoro
  savePomodoros (settings ^. sDataFile) pomodors
  return newSt

startPomodoro :: Payload -> Server StartPomodoroAPI
startPomodoro = handleMessage StartPomodoro

startPause :: Payload -> Server StartPauseAPI
startPause = handleMessage StartPause

setTags :: Payload -> Server SetTagsAPI
setTags p tags = handleMessage (SetTags tags) p

status :: Payload -> Server TickAPI
status = handleMessage Tick
