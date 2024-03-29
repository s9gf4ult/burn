module Burn.Optparse.Settings where

import Burn.Types
import Control.Lens
import Data.Default
import Data.Generics.Labels ()
import Data.Time
import GHC.Generics (Generic)
import Options.Applicative

data HostPort = HostPort
  { host :: String
  , port :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Default HostPort where
  def = HostPort "127.0.0.1" 1338

parseHostPort :: Parser HostPort
parseHostPort = HostPort
  <$> strOption host
  <*> option auto port
  where
    defHostPort :: HostPort
    defHostPort = def
    host = long "address" <> short 'a'
      <> help "Address to listen/connect" <> value (defHostPort ^. #host)
    port = long "port" <> short 'p'
      <> help "Port" <> value (defHostPort ^. #port)

pomodorosFile :: Parser (Maybe  FilePath)
pomodorosFile = optional $ strOption filePath
  where
    filePath = long "file" <> short 'f'
      <> help "CSV file with pomodors to store"

parseDayEnd :: Parser TimeOfDay
parseDayEnd = option todReader eod
  where
    eod = long "end-of-day" <> help "Time when your day realy ends"
      <> value ((def :: Settings) ^. #dayEnd)

todReader :: ReadM TimeOfDay
todReader = str >>= parseTimeM True defaultTimeLocale "%R"

parseSettings :: Parser Settings
parseSettings = Settings
  <$> option timeReader pomLen
  <*> option timeReader pauseLen
  <*> option timeReader longPause
  <*> parseDayEnd
  <*> pomodorosFile
  where
    timeReader = do
      mins <- auto
      pure $ secondsToNominalDiffTime (mins * 60)
    pomLen = long "pomodoro" <> help "Length of pomodoro in minutes"
      <> value (defSettings ^. #pomodoroLength)
    pauseLen = long "pause" <> help "Length of pause in minutes"
      <> value (defSettings ^. #pauseLength)
    longPause = long "long" <> help "Maximum length of calculated pause in minutes"
      <> value (defSettings ^. #longPause)
    defSettings :: Settings
    defSettings = def
