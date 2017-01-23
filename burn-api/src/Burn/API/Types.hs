module Burn.API.Types where

import Control.Lens
import Data.Time
import Data.Aeson
import Data.Aeson.TH
import Data.Text as T

data Notification = PomodoroFinish | PauseFinish
  deriving (Eq, Show)

makePrisms ''Notification
deriveJSON defaultOptions ''Notification

type Seconds = Int

data Counting = Counting
  { _cPassed :: !NominalDiffTime
  , _cLen    :: !NominalDiffTime
  } deriving (Eq, Show)

makeLenses ''Counting
deriveJSON defaultOptions ''Counting

data WhatCounted
  = Waiting
  | PomodoroCounting !Counting
  | PauseCounting !Counting
  deriving (Eq, Show)

makePrisms ''WhatCounted
deriveJSON defaultOptions ''WhatCounted

data PomodoroData = PomodoroData
  { _pdStarted :: !UTCTime
  , _pdLen     :: !NominalDiffTime
  , _pdTags    :: ![Text]
  } deriving (Eq, Ord, Show)

makeLenses ''PomodoroData
deriveJSON defaultOptions ''PomodoroData

data State = State
  { _sTags          :: ![Text]
  , _sTodayPomodors :: ![PomodoroData]
  , _sCounting      :: !WhatCounted
  } deriving (Eq, Show)

makeLenses ''State
deriveJSON defaultOptions ''State

data Status = Status
  { _asNotifications :: ![Notification]
  , _asState         :: !State
  } deriving (Eq, Show)

makeLenses ''Status
deriveJSON defaultOptions ''Status
