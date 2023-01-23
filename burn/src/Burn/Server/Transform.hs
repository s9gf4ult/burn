module Burn.Server.Transform where

import Burn.Types
import Control.Lens
import Control.Monad.State.Strict
import Data.Aeson
import Data.Aeson.TH
import Data.Default
import Data.Foldable
import Data.List as L
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Time

import qualified Data.Map.Strict as M
import qualified Data.Text as T


-- | Handles message and transforms state
process :: Settings -> TimeZone -> Message -> ServerState -> (ServerState, [Action])
process s tz msg state =
  let
    Message now _ = msg
    (burn, actions') = processBurn s msg (state ^. sTags) (state ^. sBurn)
    tags = processTags msg $ state ^. sTags
    actions  = splitDaylyActions s tz (state ^. sLastMsg) now actions'
    pomodors = processCounted actions $ state ^. sTodayPomodors
    newState = ServerState
      { _sTags          = tags
      , _sTodayPomodors = pomodors
      , _sBurn          = burn
      , _sLastMsg       = now }
  in (newState, actions)

splitDaylyActions
  :: Settings
  -> TimeZone
  -> UTCTime
  -- ^ time of previous msg
  -> UTCTime
  -- ^ now
  -> [Action]
  -> [Action]
splitDaylyActions s tz prev now actions =
  let
    low = min prev $ fromMaybe prev
      $ minimumOf (folded . _SavePomodoro . #started) actions
  in case timeBetween tz low now (s ^. sDayEnd) of
       Nothing -> actions
       Just de ->
         let pomodors = (actions ^.. folded . _SavePomodoro) >>= splitPomodoro de
             f p = p ^. #started >= de
             (a, b) = break f pomodors
         in map SavePomodoro a ++ [ResetTimers] ++ map SavePomodoro b

splitPomodoro :: UTCTime -> PomodoroData UTCTime -> [PomodoroData UTCTime]
splitPomodoro de pd =
  let
    pstart = pd ^. #started
    pend = addUTCTime (pd ^. #length) pstart
    result = if
      | pstart < de && de < pend ->
        let l1 = diffUTCTime de pstart
            l2 = diffUTCTime pend de
            tags = pd ^. #tags
        in [PomodoroData pstart l1 tags, PomodoroData de l2 tags]
      | otherwise -> [pd]
  in result

timeBetween
  :: TimeZone
  -> UTCTime
  -- ^ time of previous msg
  -> UTCTime
  -- ^ now
  -> TimeOfDay
  -- ^ day end
  -> Maybe UTCTime
  -- ^ end of day between given msg times
timeBetween tz prev now dayEnd =
  let
    days = nub $ map (localDay . utcToLocalTime tz) [prev, now]
    tods = flip map days $ \day -> localTimeToUTC tz $ LocalTime day dayEnd
    todsBetween = filter (\tod -> prev < tod && tod <= now) tods
  in maximumOf folded todsBetween

processTags
  :: Message
  -> [Text]
  -> [Text]
processTags msg tags = case msg ^. mEvent of
  SetTags newTags -> newTags
  _               -> tags

processCounted :: [Action] -> [PomodoroData UTCTime] -> [PomodoroData UTCTime]
processCounted actions = execState $ do
  for_ actions $ \action -> case action of
    SavePomodoro pd -> modify (pd:)
    ResetTimers     -> put []

processBurn
  :: Settings
  -> Message
  -> [Text]                     -- ^ Current tags
  -> Burn
  -> (Burn, [Action])
processBurn s (Message now evt) tags burn = case evt of
  Tick -> (burn, [])
  StartPomodoro -> case burn of
    Waiting ->
      let
        res = PomodoroCounting now $ Counting
          { started = now
          , length = s ^. sPomodoroLen
          }
      in (res, [])
    b@(PomodoroCounting {}) -> (b, [])
    PauseCounting c ->
      let
        passed = diffUTCTime now $ c ^. #started
        stdPom = s ^. sPomodoroLen
        stdPause = s ^. sPauseLen
        -- (passed + (5 - #length)) / 5 = x / 25 => x = 25 * (passed + (5 - #length)) / 5
        pomLen = min (s ^. sPomodoroLen)
          $ stdPom * (passed + (stdPause - c ^. #length)) / stdPause
        newC = Counting
          { length      = pomLen
          , started  = now
          }
      in (PomodoroCounting now newC, [])
  StartPause -> case burn of
    Waiting ->
      let
        res = PauseCounting $ Counting
          { started = now
          , length = s ^. sPauseLen
          }
      in (res, [])
    PomodoroCounting lastSaved' c ->
      let
        passed = diffUTCTime now $ c ^. #started
        stdPom = s ^. sPomodoroLen
        stdPause = s ^. sPauseLen
        -- (passed + (25 - #length)) / 25 = x / 5 => x = 5 * (passed + (25 - #length)) / 25
        pauseLen = min (s ^. sLongPause)
          $ stdPause * (passed + (stdPom - c ^. #length)) / stdPom
        newC = Counting
          { started  = now
          , length      = pauseLen
          }
        lastSaved = max lastSaved' (c ^. #started)
        pomodoro = PomodoroData
          { started = lastSaved
          , length     = diffUTCTime now $ lastSaved
          , tags    = Tags tags }
      in (PauseCounting newC, [SavePomodoro pomodoro])
    PauseCounting c -> (PauseCounting c, [])
  SetTags newTags -> case burn of
    PomodoroCounting lastSaved' c
      | newTags /= tags ->
        let
          lastSaved = max lastSaved' $ c ^. #started
          pomodoro = PomodoroData
            { started = lastSaved
            , length = diffUTCTime now lastSaved
            , tags = Tags tags }
        in (PomodoroCounting now c, [SavePomodoro pomodoro])
    b -> (b, [])
