module Burn.State where

import Control.Lens
import Data.Text as T
import Data.Time

data Counting = Counting
  { _cLen      :: NominalDiffTime
  , _cStarted  :: UTCTime
  , _cFinished :: Bool
    -- ^ True when stop action is sent
  }

makeLenses ''Counting

data Burn
  = StartPos
  | PomCounting
    { _bSavedFrom :: UTCTime
    , _bCounting  :: Counting
    }
  | PauseCounting
    { _bCounting :: Counting
    }

makeLenses ''Burn

data State = State
  { _sTags :: [Text]
  , _sBurn :: Burn
  }

data Event
  = Tick
  | StartPomodoro
  | StartPause
  | SetTags [Text]

data Message = Message
  { _mTime   :: UTCTime
  , _mAction :: Event
  }

makeLenses ''Message

data PomodoroData = PomodoroData
  { _pdStarted :: UTCTime
  , _pdLen     :: NominalDiffTime
  , _pdTags    :: [Text]
  }

data Action
  = SavePomodoro PomodoroData
  | NotifyPomodoroFinished
  | NotifyPauseFinished

data Settings = Settings
  { _sPomodoroLen :: NominalDiffTime
  , _sPauseLen    :: NominalDiffTime
  , _sLongPause   :: NominalDiffTime
  }

makeLenses ''Settings

-- | Handles message and transforms state
handle :: Settings -> Message -> State -> (State, [Action])
handle s msg (State tags burn) = case msg ^. mAction of
  SetTags newTags ->
    over _1 (State newTags) $ handleBurn s msg tags burn
  _ ->
    over _1 (State tags) $ handleBurn s msg tags burn

handleBurn
  :: Settings
  -> Message
  -> [Text]                     -- ^ Current tags
  -> Burn
  -> (Burn, [Action])
handleBurn s (Message time evt) tags burn = case evt of
  Tick -> case burn of
    StartPos -> (StartPos, [])
    PomCounting lastSaved c ->
      over _1 (PomCounting lastSaved)
      $ tickCount NotifyPomodoroFinished c
    PauseCounting c ->
      over _1 PauseCounting $ tickCount NotifyPauseFinished c
  StartPomodoro -> case burn of
    StartPos ->
      let
        res = PomCounting time $ Counting
          { _cStarted = time
          , _cLen = s ^. sPomodoroLen
          , _cFinished = False }
      in (res, [])
    b@(PomCounting {}) -> (b, [])
    PauseCounting c ->
      let
        passed = diffUTCTime time $ c ^. cStarted
        pomLen = min (s ^. sPomodoroLen)
          $ (s ^. sPomodoroLen) * passed / (c ^. cLen)
        newC = Counting
          { _cLen      = pomLen
          , _cStarted  = time
          , _cFinished = False }
      in (PomCounting time newC, [])
  StartPause -> case burn of
    StartPos ->
      let
        res = PauseCounting $ Counting
          { _cStarted = time
          , _cLen = s ^. sPauseLen
          , _cFinished = False }
      in (res, [])
    PomCounting lastSaved' c ->
      let
        passed = diffUTCTime time $ c ^. cStarted
        pauseLen = min (s ^. sLongPause)
          $ (s ^. sPauseLen) * passed / (s ^. sPomodoroLen)
        newC = Counting
          { _cStarted = time
          , _cLen = pauseLen
          , _cFinished = False }
        lastSaved = max lastSaved' (c ^. cStarted)
        pomodoro = PomodoroData
          { _pdStarted = c ^. cStarted
          , _pdLen = diffUTCTime time $ lastSaved
          , _pdTags = tags }
      in (PauseCounting newC, [SavePomodoro pomodoro])
    PauseCounting c -> (PauseCounting c, [])
  SetTags {} -> case burn of
    PomCounting lastSaved' c ->
      let
        lastSaved = max lastSaved' $ c ^. cStarted
        pomodoro = PomodoroData
          { _pdStarted = lastSaved
          , _pdLen = diffUTCTime time lastSaved
          , _pdTags = tags }
      in (PomCounting time c, [SavePomodoro pomodoro])
    b -> (b, [])
  where
    tickCount :: Action -> Counting -> (Counting, [Action])
    tickCount stopAction c =
      let
        passed = diffUTCTime time $ c ^. cStarted
        overruned = passed >= (c ^. cLen)
        actions =
          if | c ^. cFinished -> []
             | overruned -> [stopAction]
             | otherwise -> []
      in ( set cFinished overruned c
         , actions )
