module Burn.Server.Transform where

import Burn.Types
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Default
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text (Text)
import Data.Time

import qualified Data.Map.Strict as M
import qualified Data.Text as T


-- | Handles message and transforms state
process :: Settings -> Message -> State -> (State, [Action UTCTime])
process s msg state =
  let
    (newB, actions) = processBurn s msg (state ^. sTags) (state ^. sBurn)
    newTags = processTags msg $ state ^. sTags
    newCounted = processCounted actions $ state ^. sTodayPomodors
  in (State newTags newCounted newB, actions)

processTags
  :: Message
  -> [Text]
  -> [Text]
processTags msg tags = case msg ^. mEvent of
  SetTags newTags -> newTags
  _               -> tags

processCounted :: [Action UTCTime] -> [PomodoroData UTCTime] -> [PomodoroData UTCTime]
processCounted actions = (<> (actions ^.. folded . _SavePomodoro))

processBurn
  :: Settings
  -> Message
  -> [Text]                     -- ^ Current tags
  -> Burn
  -> (Burn, [Action UTCTime])
processBurn s (Message now evt) tags burn = case evt of
  Tick -> case burn of
    Waiting -> (Waiting, [])
    PomodoroCounting lastSaved c ->
      over _1 (PomodoroCounting lastSaved)
      $ tickCount NotifyPomodoroFinished c
    PauseCounting c ->
      over _1 PauseCounting $ tickCount NotifyPauseFinished c
  StartPomodoro -> case burn of
    Waiting ->
      let
        res = PomodoroCounting now $ Counting
          { _cStarted = now
          , _cLen = s ^. sPomodoroLen
          , _cFinished = False }
      in (res, [])
    b@(PomodoroCounting {}) -> (b, [])
    PauseCounting c ->
      let
        passed = diffUTCTime now $ c ^. cStarted
        stdPom = s ^. sPomodoroLen
        stdPause = s ^. sPauseLen
        -- (passed + (5 - cLen)) / 5 = x / 25 => x = 25 * (passed + (5 - cLen)) / 5
        pomLen = min (s ^. sPomodoroLen)
          $ stdPom * (passed + (stdPause - c ^. cLen)) / stdPause
        newC = Counting
          { _cLen      = pomLen
          , _cStarted  = now
          , _cFinished = pomLen <= 0 }
      in (PomodoroCounting now newC, [])
  StartPause -> case burn of
    Waiting ->
      let
        res = PauseCounting $ Counting
          { _cStarted = now
          , _cLen = s ^. sPauseLen
          , _cFinished = False }
      in (res, [])
    PomodoroCounting lastSaved' c ->
      let
        passed = diffUTCTime now $ c ^. cStarted
        stdPom = s ^. sPomodoroLen
        stdPause = s ^. sPauseLen
        -- (passed + (25 - cLen)) / 25 = x / 5 => x = 5 * (passed + (25 - cLen)) / 25
        pauseLen = min (s ^. sLongPause)
          $ stdPause * (passed + (stdPom - c ^. cLen)) / stdPom
        newC = Counting
          { _cStarted  = now
          , _cLen      = pauseLen
          , _cFinished = pauseLen <= 0 }
        lastSaved = max lastSaved' (c ^. cStarted)
        pomodoro = PomodoroData
          { _pdStarted = lastSaved
          , _pdLen     = diffUTCTime now $ lastSaved
          , _pdTags    = Tags tags }
      in (PauseCounting newC, [SavePomodoro pomodoro])
    PauseCounting c -> (PauseCounting c, [])
  SetTags newTags -> case burn of
    PomodoroCounting lastSaved' c
      | newTags /= tags ->
        let
          lastSaved = max lastSaved' $ c ^. cStarted
          pomodoro = PomodoroData
            { _pdStarted = lastSaved
            , _pdLen = diffUTCTime now lastSaved
            , _pdTags = Tags tags }
        in (PomodoroCounting now c, [SavePomodoro pomodoro])
    b -> (b, [])
  where
    tickCount :: Action UTCTime -> Counting -> (Counting, [Action UTCTime])
    tickCount stopAction c =
      let
        passed = diffUTCTime now $ c ^. cStarted
        overruned = passed >= (c ^. cLen)
        actions =
          if | c ^. cFinished -> []
             | overruned -> [stopAction]
             | otherwise -> []
      in ( set cFinished overruned c
         , actions )
