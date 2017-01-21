module Burn.State where

import Control.Lens
import Data.Default
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time

import qualified Data.Map.Strict as M
import qualified Data.Text as T

data Counting = Counting
  { _cLen      :: NominalDiffTime
  , _cStarted  :: UTCTime
  , _cFinished :: Bool
    -- ^ True when stop action is sent
  } deriving (Show)

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
  deriving (Show)

makePrisms ''Burn
makeLenses ''Burn

instance Default Burn where
  def = StartPos

data State = State
  { _sTags    :: [Text]
  , _sCounted :: [PomodoroData]
    -- ^ List of today's counted pomodoros
  , _sBurn    :: Burn
  } deriving (Show)

makeLenses ''State

instance Default State where
  def = State [] def def

data Event
  = Tick
  | StartPomodoro
  | StartPause
  | SetTags [Text]
  deriving (Show)

data Message = Message
  { _mTime  :: UTCTime
  , _mEvent :: Event
  } deriving (Show)

makeLenses ''Message

data PomodoroData = PomodoroData
  { _pdStarted :: UTCTime
  , _pdLen     :: NominalDiffTime
  , _pdTags    :: [Text]
  } deriving (Eq, Ord, Show)

makeLenses ''PomodoroData

data Action
  = SavePomodoro PomodoroData
  | NotifyPomodoroFinished
  | NotifyPauseFinished
  deriving (Eq, Ord, Show)

makePrisms ''Action

data Settings = Settings
  { _sPomodoroLen :: NominalDiffTime
  , _sPauseLen    :: NominalDiffTime
  , _sLongPause   :: NominalDiffTime
  } deriving (Eq, Ord, Show)

makeLenses ''Settings

instance Default Settings where
  def = Settings
    { _sPomodoroLen = 25 * 60
    , _sPauseLen    = 5 * 60
    , _sLongPause   = 15 * 60
    }

-- | Handles message and transforms state
process :: Settings -> Message -> State -> (State, [Action])
process s msg state =
  let
    (newB, actions) = processBurn s msg (state ^. sTags) (state ^. sBurn)
    newTas = processTags msg $ state ^. sTags
    newCounted



processTags
  :: Message
  -> [Text]
  -> [Text]
processTimes msg tags = case msg ^. mEvent of
  SetTags newTags -> newTags
  _               -> tags

processCounted :: [Action] -> [PomodoroData] -> [PomodoroData]
processCounted actions = (<> (actions ^.. folded . _SavePomodoro))

processBurn
  :: Settings
  -> Message
  -> [Text]                     -- ^ Current tags
  -> Burn
  -> (Burn, [Action])
processBurn s (Message now evt) tags burn = case evt of
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
        res = PomCounting now $ Counting
          { _cStarted = now
          , _cLen = s ^. sPomodoroLen
          , _cFinished = False }
      in (res, [])
    b@(PomCounting {}) -> (b, [])
    PauseCounting c ->
      let
        passed = diffUTCTime now $ c ^. cStarted
        pomLen = min (s ^. sPomodoroLen)
          $ (s ^. sPomodoroLen) * passed / (c ^. cLen)
        newC = Counting
          { _cLen      = pomLen
          , _cStarted  = now
          , _cFinished = False }
      in (PomCounting now newC, [])
  StartPause -> case burn of
    StartPos ->
      let
        res = PauseCounting $ Counting
          { _cStarted = now
          , _cLen = s ^. sPauseLen
          , _cFinished = False }
      in (res, [])
    PomCounting lastSaved' c ->
      let
        passed = diffUTCTime now $ c ^. cStarted
        lenTruncator = if
          | passed > s ^. sPomodoroLen -> s ^. sLongPause
          | otherwise                  -> s ^. sPauseLen
        pauseLen =
          min lenTruncator $ (s ^. sPauseLen) * passed / (c ^. cLen)
        newC = Counting
          { _cStarted = now
          , _cLen = pauseLen
          , _cFinished = False }
        lastSaved = max lastSaved' (c ^. cStarted)
        pomodoro = PomodoroData
          { _pdStarted = c ^. cStarted
          , _pdLen = diffUTCTime now $ lastSaved
          , _pdTags = tags }
      in (PauseCounting newC, [SavePomodoro pomodoro])
    PauseCounting c -> (PauseCounting c, [])
  SetTags {} -> case burn of
    PomCounting lastSaved' c ->
      let
        lastSaved = max lastSaved' $ c ^. cStarted
        pomodoro = PomodoroData
          { _pdStarted = lastSaved
          , _pdLen = diffUTCTime now lastSaved
          , _pdTags = tags }
      in (PomCounting now c, [SavePomodoro pomodoro])
    b -> (b, [])
  where
    tickCount :: Action -> Counting -> (Counting, [Action])
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
