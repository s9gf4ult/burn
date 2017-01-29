module Burn.Types where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Csv
import Data.Default
import Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)

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

data Counting = Counting
  { _cLen      :: NominalDiffTime
  , _cStarted  :: UTCTime
  , _cFinished :: Bool
    -- ^ True when stop action is sent
  } deriving (Show)

makeLenses ''Counting
deriveJSON
  defaultOptions
  ''Counting

data Burn
  = Waiting
  | PomodoroCounting
    { _bSavedFrom :: UTCTime
    , _bCounting  :: Counting
    }
  | PauseCounting
    { _bCounting :: Counting
    }
  deriving (Show)

makePrisms ''Burn
makeLenses ''Burn
deriveJSON
  defaultOptions
  ''Burn

instance Default Burn where
  def = Waiting

data State = State
  { _sTags          :: ![Text]
  , _sTodayPomodors :: ![PomodoroData UTCTime]
    -- ^ List of today's counted pomodoros
  , _sBurn          :: !Burn
  } deriving (Show)

makeLenses ''State
deriveJSON
  defaultOptions
  ''State

instance Default State where
  def = State [] def def

data Event
  = Tick
  | StartPomodoro
  | StartPause
  | SetTags [Text]
  deriving (Show)

makePrisms ''Event

data Message = Message
  { _mTime  :: UTCTime
  , _mEvent :: Event
  } deriving (Show)

makeLenses ''Message

data Action date
  = SavePomodoro (PomodoroData date)
  | DayEnd
  | NotifyPomodoroFinished
  | NotifyPauseFinished
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''Action

data Settings = Settings
  { _sPomodoroLen :: !NominalDiffTime
  , _sPauseLen    :: !NominalDiffTime
  , _sLongPause   :: !NominalDiffTime
  , _sDayEnd      :: !DiffTime
  , _sDataFile    :: !FilePath
  } deriving (Eq, Ord, Show)

makeLenses ''Settings

instance Default Settings where
  def = Settings
    { _sPomodoroLen = 25 * 60
    , _sPauseLen    = 5 * 60
    , _sLongPause   = 15 * 60
    , _sDayEnd      = secondsToDiffTime $ 5 * 3600 -- 5 am is a day end
    , _sDataFile    = "/home/razor/burn/pomodoros.csv" -- FIXME: make configurable
    }
