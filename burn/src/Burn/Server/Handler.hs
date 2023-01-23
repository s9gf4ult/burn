module Burn.Server.Handler where

import Burn.API
import Burn.Server.Transform
import Burn.Statistics
import Burn.Storage
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Base
import Data.Default
import Data.Foldable
import Data.Time
import Data.Vector as V
import Network.Wai.Handler.Warp (run)
import Servant

data Payload = Payload
  { _pState    :: TVar ServerState
  , _pSettings :: TVar Settings
  }

makeLenses ''Payload

handlers :: Payload -> Server BurnAPI
handlers p
  =    startPomodoro p
  :<|> startPause p
  :<|> setTags p
  :<|> status p

handleMessage
  :: Event
  -> Payload
  -> Handler ServerState
handleMessage evt p = liftBase $ do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  let msg = Message now evt
  (settings, (newSt, actions)) <- atomically $ do
    state <- readTVar $ p ^. pState
    settings <- readTVar $ p ^. pSettings
    check $ state ^. #lastMsg < now
    let res@(newSt, _) = process settings tz msg state
    writeTVar (p ^. pState) newSt
    return (settings, res)
  print evt
  pomodors <- (traversed . traversed) utcToLocalZonedTime
    $ actions ^.. folded . #_SavePomodoro
  for_ (settings ^. #dataFile) $ \p -> do
    savePomodoros p pomodors
  return newSt

startPomodoro :: Payload -> Server StartPomodoroAPI
startPomodoro = handleMessage StartPomodoro

startPause :: Payload -> Server StartPauseAPI
startPause = handleMessage StartPause

setTags :: Payload -> Server SetTagsAPI
setTags p tags = handleMessage (SetTags tags) p

status :: Payload -> Server TickAPI
status = handleMessage Tick
