module Burn.API.Types where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Csv
import Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)

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

newtype Tags = Tags [Text]
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance FromField Tags where
  parseField f = do
    t <- parseField f
    return $ Tags $ T.words t

instance ToField Tags where
  toField (Tags tt) = toField $ T.unwords tt

instance FromField NominalDiffTime where
  parseField f = fromInteger <$> parseField f

instance ToField NominalDiffTime where
  toField ndf = toField ((truncate ndf) :: Integer)

instance FromField UTCTime where
  parseField f = do
    ndf <- parseField f
    return $ posixSecondsToUTCTime ndf

instance ToField UTCTime where
  toField utc = toField $ utcTimeToPOSIXSeconds utc


data PomodoroData = PomodoroData
  { _pdStarted :: !UTCTime
  , _pdLen     :: !NominalDiffTime
  , _pdTags    :: !Tags
  } deriving (Eq, Ord, Show, Generic)

makeLenses ''PomodoroData
deriveJSON defaultOptions ''PomodoroData
instance FromRecord PomodoroData
instance ToRecord PomodoroData

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
