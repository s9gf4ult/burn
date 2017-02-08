module Burn.Server.Handler where

import Burn.API
import Burn.Server.Transform
import Burn.Types
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Base
import Control.Monad.Except
import Data.Csv
import Data.Default
import Data.Maybe
import Data.Time
import Network.Wai.Handler.Warp (run)
import Servant
import System.Directory

import qualified Data.ByteString.Lazy as BL

data Payload = Payload
  { _pState    :: TVar ServerState
  , _pSettings :: TVar Settings
  }

makeLenses ''Payload

initPayload :: IO Payload
initPayload = Payload <$> newTVarIO def <*> newTVarIO def

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
  let msg = Message now evt
  (settings, (newSt, actions)) <- atomically $ do
    state <- readTVar $ p ^. pState
    settings <- readTVar $ p ^. pSettings
    let res@(newSt, _) = process settings msg state
    writeTVar (p ^. pState) newSt
    return (settings, res)
  print evt
  pomodors <- (traversed . traversed) utcToLocalZonedTime
    $ actions ^.. folded . _SavePomodoro
  savePomodoros (settings ^. sDataFile) pomodors
  return newSt

savePomodoros :: FilePath -> [PomodoroData ZonedTime] -> IO ()
savePomodoros fp' pomodors = do
  fp <- canonicalizePath fp'
  BL.appendFile fp $ encode pomodors



startPomodoro :: Payload -> Server StartPomodoroAPI
startPomodoro = handleMessage StartPomodoro

startPause :: Payload -> Server StartPauseAPI
startPause = handleMessage StartPause

setTags :: Payload -> Server SetTagsAPI
setTags p tags = handleMessage (SetTags tags) p

status :: Payload -> Server TickAPI
status = handleMessage Tick
