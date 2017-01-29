module Burn.Client.State where

import Burn.Types
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Time

-- | What notifications to show when state is changed
notifications
  :: UTCTime
  -> State
     -- ^ Old state
  -> State
     -- ^ New state
  -> [Notification]
notifications now old new = do
  (notification, l) <- [ (PomodoroFinished, _PomodoroCounting . _2)
                       , (PauseFinished, _PauseCounting)]
  let
    newO = fromMaybe False $ new ^? sBurn . l . to (overrun now)
    oldO = fromMaybe False $ old ^? sBurn . l . to (overrun now)
  guard $ newO && not oldO
  return notification

-- | Couning is overruned
overrun :: UTCTime -> Counting -> Bool
overrun now c =
  let passed = diffUTCTime now $ c ^. cStarted
  in passed > (c ^. cLen)
