module Burn.Server where

import Burn.State
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

import qualified Burn.API as A
import qualified Data.ByteString.Lazy as BL

data Payload = Payload
  { _pState    :: TVar State
  , _pSettings :: TVar Settings
  }

makeLenses ''Payload

initPayload :: IO Payload
initPayload = Payload <$> newTVarIO def <*> newTVarIO def

burn :: IO ()
burn = do
  p <- initPayload
  run 1338 $ serve A.burnAPI $ handlers p

handlers :: Payload -> Server A.BurnAPI
handlers p
  =    startPomodoro p
  :<|> startPause p
  :<|> setTags p
  :<|> status p

handleMessage
  :: Event
  -> Payload
  -> ExceptT ServantErr IO A.Status
handleMessage evt p = liftBase $ do
  now <- getCurrentTime
  let msg = Message now evt
  (settings, res@(_, actions)) <- atomically $ do
    state <- readTVar $ p ^. pState
    settings <- readTVar $ p ^. pSettings
    let res@(newSt, _) = process settings msg state
    writeTVar (p ^. pState) newSt
    return (settings, res)
  print evt
  savePomodoros (settings ^. sDataFile) actions
  let reply = mkStatus now res
  return reply

savePomodoros :: FilePath -> [Action] -> IO ()
savePomodoros fp' actions = do
  fp <- canonicalizePath fp'
  let pomodoros = encode $ actions ^.. folded . _SavePomodoro
  BL.appendFile fp pomodoros



startPomodoro :: Payload -> Server A.StartPomodoroAPI
startPomodoro = handleMessage StartPomodoro

startPause :: Payload -> Server A.StartPauseAPI
startPause = handleMessage StartPause

setTags :: Payload -> Server A.SetTagsAPI
setTags p tags = handleMessage (SetTags tags) p

status :: Payload -> Server A.StatusAPI
status = handleMessage Tick

mkStatus :: UTCTime -> (State, [Action]) -> A.Status
mkStatus now (state, actions) = A.Status
  { A._asNotifications = catMaybes $ map toNotif actions
  , A._asState         = A.State
    { A._sTags          = state ^. sTags
    , A._sTodayPomodors = state ^. sTodayPomodors
    , A._sCounting      = counting
    }
  }
  where
    toNotif = \case
      NotifyPomodoroFinished -> Just A.PomodoroFinish
      NotifyPauseFinished    -> Just A.PauseFinish
      _                      -> Nothing
    counting = case state ^. sBurn of
      StartPos -> A.Waiting
      PomCounting _ c -> A.PomodoroCounting $ toApiCounting c
      PauseCounting c -> A.PauseCounting $ toApiCounting c
    toApiCounting c =
      let passed = diffUTCTime now $ c ^. cStarted
      in A.Counting
         { A._cPassed = passed
         , A._cLen = c ^. cLen
         }
