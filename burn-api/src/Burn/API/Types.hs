module Burn.API.Types where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Text as T

data Notification = PomodoroFinish | PauseFinish
  deriving (Eq, Show)

makePrisms ''Notification
deriveJSON defaultOptions ''Notification

type Seconds = Int

data Counting = Counting
  { _cPassed :: !Seconds
  , _cLeft   :: !Seconds
  } deriving (Eq, Show)

makeLenses ''Counting
deriveJSON defaultOptions ''Counting

data WhatCounted
  = Waiting
  | PomdoroCounting !Counting
  | PauseCounting !Counting
  deriving (Eq, Show)

makePrisms ''WhatCounted
deriveJSON defaultOptions ''WhatCounted

data State = State
  { _sTags     :: ![Text]
  , _sCounting :: !WhatCounted
  } deriving (Eq, Show)

makeLenses ''State
deriveJSON defaultOptions ''State

data Status = Status
  { _asNotifications :: ![Notification]
  , _asState         :: !State
  } deriving (Eq, Show)

makeLenses ''Status
deriveJSON defaultOptions ''Status
