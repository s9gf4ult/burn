module Burn.API.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Text as T

data Notification = PomodoroFinish | PauseFinish
  deriving (Eq, Show)

deriveJSON defaultOptions ''Notification

type Seconds = Int

data Counting = Counting
  { _cPassed :: Seconds
  , _cLeft   :: Seconds
  } deriving (Eq, Show)

deriveJSON defaultOptions ''Counting

data WhatCounted
  = Waiting
  | PomdoroCounting Counting
  | PauseCounting Counting
  deriving (Eq, Show)

deriveJSON defaultOptions ''WhatCounted

data State = State
  { _sTags     :: [Text]
  , _sCounting :: WhatCounted
  } deriving (Eq, Show)

deriveJSON defaultOptions ''State

data Status = Status
  { _asNotifications :: [Notification]
  , _asState         :: State
  } deriving (Eq, Show)

deriveJSON defaultOptions ''Status
