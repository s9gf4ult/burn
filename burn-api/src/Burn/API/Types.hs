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

iso8601 :: String
iso8601 = iso8601DateFormat $ Just "%H:%M:%S%Q%z"

instance FromField ZonedTime where
  parseField f = do
    t <- parseField f
    parseTimeM True defaultTimeLocale iso8601 t

instance ToField ZonedTime where
  toField = toField . formatTime defaultTimeLocale iso8601

data PomodoroData date = PomodoroData
  { _pdStarted :: !date
  , _pdLen     :: !NominalDiffTime
  , _pdTags    :: !Tags
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

makeLenses ''PomodoroData
deriveJSON defaultOptions ''PomodoroData
instance (FromField a) => FromRecord (PomodoroData a)
instance (ToField a) => ToRecord (PomodoroData a)

data State = State
  { _sTags          :: ![Text]
  , _sTodayPomodors :: ![PomodoroData ZonedTime]
  , _sCounting      :: !WhatCounted
  } deriving (Show)

makeLenses ''State
deriveJSON defaultOptions ''State

data Status = Status
  { _asNotifications :: ![Notification]
  , _asState         :: !State
  } deriving (Show)

makeLenses ''Status
deriveJSON defaultOptions ''Status
