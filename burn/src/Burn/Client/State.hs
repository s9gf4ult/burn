module Burn.Client.State where

import Burn.Types
import Control.Lens
import Data.Default
import Data.Time


data ClientState = ClientState
  { _csNotificationShowed :: Bool
  } deriving (Eq, Ord, Show)

makeLenses ''ClientState

instance Default ClientState where
  def = ClientState False

-- | What notifications to show when state is changed
updateState
  :: UTCTime
     -- ^ Now
  -> ServerState
     -- ^ State came from server
  -> ClientState
  -> (ClientState, [Notification])
updateState now server client = case server ^. sBurn of
  Waiting              -> (ClientState False, [])
  PomodoroCounting _ c -> countingState PomodoroFinished c
  PauseCounting c      -> countingState PauseFinished c
  where
    countingState notif c =
      let
        ovr = overrun now c
        showNotif = ovr && not (client ^. csNotificationShowed)
      in (ClientState ovr, if showNotif then [notif] else [])

-- | Couning is overruned
overrun :: UTCTime -> Counting -> Bool
overrun now c =
  let passed = diffUTCTime now $ c ^. cStarted
  in passed >= (c ^. cLen)
