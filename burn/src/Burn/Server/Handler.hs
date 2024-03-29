module Burn.Server.Handler where

import Burn.API
import Burn.Server.Transform
import Burn.Storage
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Base
import Data.Foldable
import Data.Time
import GHC.Generics (Generic)
import Servant

data Payload = Payload
  { state    :: TVar ServerState
  , settings :: TVar Settings
  } deriving Generic

handlers :: Payload -> Server BurnAPI
handlers p
  =    startPomodoro p
  :<|> startPause p
  :<|> setTags p
  :<|> setOption p
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
    state <- readTVar $ p ^. #state
    settings <- readTVar $ p ^. #settings
    check $ state ^. #lastMsg < now
    let res@(newSt, _) = process (settings ^. #dayEnd) tz msg state
    writeTVar (p ^. #state) newSt
    return (settings, res)
  print evt
  pomodors <- (traversed . traversed) utcToLocalZonedTime
    $ actions ^.. folded . #_SavePomodoro
  for_ (settings ^. #dataFile) $ \f -> do
    savePomodoros f pomodors
  return newSt

startPomodoro :: Payload -> Server StartPomodoroAPI
startPomodoro = handleMessage StartPomodoro

startPause :: Payload -> Server StartPauseAPI
startPause = handleMessage StartPause

setTags :: Payload -> Server SetTagsAPI
setTags p tags = handleMessage (SetTags tags) p

setOption :: Payload -> Server SetOptionAPI
setOption p opt = handleMessage (SetOption opt) p

status :: Payload -> Server TickAPI
status = handleMessage Tick
