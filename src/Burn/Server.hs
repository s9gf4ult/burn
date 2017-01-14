module Burn.Server where

import Burn.API
import Burn.State
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Base
import Data.Default
import Data.Time
import Network.Wai.Handler.Warp (run)
import Servant

import qualified Burn.API.Types as A

data Payload = Payload
  { _pState :: TVar State
  , _pSettings :: TVar Settings
  }

makeLenses ''Payload

initPayload :: IO Payload
initPayload = Payload <$> newTVarIO def <*> newTVarIO def

burn :: IO ()
burn = do
  p <- initPayload
  run 1338 $ serve proxy $ handlers p
  where
    proxy = Proxy :: Proxy BurnAPI

handlers :: Payload -> Server BurnAPI
handlers p
  =    startPomodoro p
  :<|> startPause p
  :<|> setTags p
  :<|> status p

startPomodoro :: Payload -> Server StartPomodoroAPI
startPomodoro p = liftBase $ do
  now <- getCurrentTime
  let msg = Message now StartPomodoro
  res <- atomically $ do
    state <- readTVar $ p ^. pState
    settings <- readTVar $ p ^. pSettings
    let res@(newSt, _) = process settings msg state
    writeTVar (p ^. pState) newSt
    return res
  return $ mkStatus res

mkStatus :: (State, [Action]) -> A.Status
mkStatus = (error "FIXME: ")

startPause :: Payload -> Server StartPauseAPI
startPause = error "Not implemented: startPause"

setTags :: Payload -> Server SetTagsAPI
setTags = error "Not implemented: setTags"

status :: Payload -> Server StatusAPI
status = (error "FIXME: ")
