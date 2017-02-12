module Burn.Optparse.Server where

import Burn.Types
import Control.Lens
import Data.Default
import Data.Monoid
import Data.Time
import Options.Applicative

data HostPort = HostPort
  { _hpHost :: String
  , _hpPort :: Int
  } deriving (Eq, Ord, Show)

makeLenses ''HostPort

instance Default HostPort where
  def = HostPort "127.0.0.1" 1338

hostPort :: Parser HostPort
hostPort = HostPort
  <$> strOption host
  <*> option auto port
  where
    host = long "address" <> short 'a'
      <> help "Address to listen/connect" <> value (def ^. hpHost)
    port = long "port" <> short 'p'
      <> help "Port" <> value (def ^. hpPort)

data ServerArgs = ServerArgs
  { _saHostPort :: HostPort
  , _saSettings :: Settings
  } deriving (Eq, Ord, Show)

makeLenses ''ServerArgs

serverArgs :: Parser ServerArgs
serverArgs = ServerArgs
  <$> hostPort
  <*> settings

settings :: Parser Settings
settings = Settings
  <$> option timeReader pomLen
  <*> option timeReader pauseLen
  <*> option timeReader longPause
  <*> option todReader eod
  <*> strOption filePath
  where
    timeReader = do
      tod <- todReader
      return $ realToFrac $ timeOfDayToTime tod
    todReader = str >>= parseTimeM True defaultTimeLocale "%R"
    pomLen = long "pomodoro" <> help "Length of pomodoro"
      <> value (def ^. sPomodoroLen)
    pauseLen = long "pause" <> help "Length of pause"
      <> value (def ^. sPauseLen)
    longPause = long "long" <> help "Maximum length of calculated pause"
      <> value (def ^. sLongPause)
    eod = long "end-of-day" <> help "Time when your day realy ends"
      <> value (def ^. sDayEnd)
    filePath = long "file" <> short 'f' <> help "CSV file with pomodors to store"
      <> value (def ^. sDataFile)
