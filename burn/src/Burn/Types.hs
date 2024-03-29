{-# OPTIONS_GHC -Wno-orphans #-}

module Burn.Types where

import Data.Aeson
import Data.Csv
import Data.Default
import Data.Generics.Labels ()
import Data.Generics.Product.Subtype
import Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)

data Notification
  = PomodoroFinished | PauseFinished
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Tags = Tags [Text]
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  toField = toField . utcTimeToPOSIXSeconds

iso8601 :: String
iso8601 = iso8601DateFormat $ Just "%H:%M:%S%Q%z"

instance FromField ZonedTime where
  parseField f = do
    t <- parseField f
    parseTimeM True defaultTimeLocale iso8601 t

instance ToField ZonedTime where
  toField = toField . formatTime defaultTimeLocale iso8601

data PomodoroData date = PomodoroData
  { started :: !date
  , length  :: !NominalDiffTime
  , tags    :: !Tags
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable, FromJSON, ToJSON)

instance (FromField a) => FromRecord (PomodoroData a)
instance (ToField a) => ToRecord (PomodoroData a)

data Counting = Counting
  { length  :: NominalDiffTime
  , started :: UTCTime
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data Burn
  = Waiting
  | PomodoroCounting
    { savedFrom :: UTCTime
    , counting  :: Counting
    }
  | PauseCounting
    { counting :: Counting
    }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Default Burn where
  def = Waiting

data TimerSettings = TimerSettings
  { pomodoroLength :: !NominalDiffTime
  , pauseLength    :: !NominalDiffTime
  , longPause      :: !NominalDiffTime
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Default TimerSettings where
  def = TimerSettings
    { pomodoroLength = 25 * 60
    , pauseLength    = 5 * 60
    , longPause      = 30 * 60
    }

toTimerSettings :: Settings -> TimerSettings
toTimerSettings = upcast

data ServerState = ServerState
  { tags           :: ![Text]
  , todayPomodoros :: ![PomodoroData UTCTime]
    -- ^ List of today's counted pomodoros
  , burn           :: !Burn
  , lastMsg        :: !UTCTime
  , settings       :: !TimerSettings
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

mkServerState
  :: UTCTime
     -- ^ now
  -> TimerSettings
  -> ServerState
mkServerState lastMsg settings = ServerState
  { tags = []
  , todayPomodoros = []
  , burn = def
  , lastMsg
  , settings
  }

data Event
  = Tick
  | StartPomodoro
  | StartPause
  | SetTags [Text]
  | SetOption SetOption
  deriving (Eq, Ord, Show, Generic)

data SetOption
  = SetPomodoroLength NominalDiffTime
  | SetPauseLength NominalDiffTime
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data Message = Message
  { time  :: !UTCTime
  , event :: !Event
  } deriving (Eq, Ord, Show, Generic)

data Action
  = SavePomodoro (PomodoroData UTCTime)
    -- ^ Command to save pomodoro
  | ResetTimers
    -- ^ Day is end so we must reset timers
  deriving (Eq, Ord, Show, Generic)

data Settings = Settings
  { pomodoroLength :: !NominalDiffTime
  , pauseLength    :: !NominalDiffTime
  , longPause      :: !NominalDiffTime
  , dayEnd         :: !TimeOfDay
  , dataFile       :: !(Maybe FilePath)
  } deriving (Eq, Ord, Show, Generic)

instance Default Settings where
  def = Settings
    { pomodoroLength = 25 * 60
    , pauseLength    = 5 * 60
    , longPause      = 15 * 60
    , dayEnd         = TimeOfDay 5 0 0
    , dataFile       = Nothing
    }
