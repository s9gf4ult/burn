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
  | PomCounting Counting
  | PauseCounting Counting

data Event
  = Tick
  | StartPomodoro
  | StartPause

data Message = Message
  { _mTime   :: UTCTime
  , _mAction :: Event
  }

data PomodoroData = PomodoroData
  { _pdStarted :: UTCTime
  , _pdLen     :: NominalDiffTime
  , _pdTags    :: [Text]
  }

data Action
  = SavePomodoro PomodoroData
  | PomodoroStop
  | PauseStop

data Settings = Settings
  { _sPomodoroLen :: NominalDiffTime
  , _sPauseLen :: NominalDiffTime
  }

makeLenses ''Settings

-- | Handles message and transforms state
handle :: Settings -> Message -> Burn -> (Burn, [Action])
handle s (Message time evt) burn = case evt of
  Tick -> case burn of
    StartPos -> (StartPos, [])
    PomCounting c -> tickCount PomCounting PomodoroStop c
    PauseCounting c -> tickCount PauseCounting PauseStop c
  StartPomodoro -> case burn of
    StartPos ->
      let res = PomCounting $ Counting
            { _cStarted = time
            , _cLen = s ^. sPomodoroLen
            , _cFinished = False
            }
      in (res, [])
    PomCounting c -> (PomCounting c, [])
    PauseCounting c ->
      let
        passed = diffUTCTime time $ c ^. cStarted
        pomLen =
          if passed >= (c ^. cLen)
          then s ^. sPomodoroLen
          else (s ^. sPomodoroLen) * passed / (c ^. cLen)
        newC = Counting
          { _cLen      = pomLen
          , _cStarted  = time
          , _cFinished = False }
      in (PomCounting newC, [])
  where
    tickCount up stopAction c =
      let
        passed = diffUTCTime time $ c ^. cStarted
        overruned = passed >= (c ^. cLen)
        actions =
          if | c ^. cFinished -> []
             | overruned -> [stopAction]
             | otherwise -> []
      in ( up $ set cFinished overruned c
         , actions )
