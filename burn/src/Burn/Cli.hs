module Burn.Cli where

import Burn.Types
import Control.Lens
import Data.Default
import Data.Monoid
import Data.Time
import Data.Traversable
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
    host = long "host" <> short 'h'
      <> help "Address to listen at" <> value (def ^. hpHost)
    port = long "port" <> short 'p'
      <> help "Port" <> value (def ^. hpPort)

data Command
  = Pomodoro
  | Pause
  deriving (Eq, Ord, Show)

readCommand :: String -> Either String Command
readCommand = \case
  "pomodoro" -> Right Pomodoro
  "pause"    -> Right Pause
  e          -> Left $ "Unknown command: " ++ e

commands :: Parser [Command]
commands = option go m
  where
    m = long "commands" <> short 'c'
      <> help "Space separated list of commands" <> value []
    go = do
      s <- str
      for (words s) $ \c -> case readCommand c of
        Left e -> readerError e
        Right a -> return a

data ClientArgs = ClientArgs
  { _caHostPort :: HostPort
  , _caCommands :: [Command]
  } deriving (Eq, Ord, Show)

makeLenses ''ClientArgs

clientArgs :: Parser ClientArgs
clientArgs = ClientArgs
  <$> hostPort
  <*> commands

data ServerArgs = ServerArgs
  { _saHostPort :: HostPort
  , _saSettings :: Settings
  } deriving (Eq, Ord, Show)

makeLenses ''ServerArgs

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

serverArgs :: Parser ServerArgs
serverArgs = ServerArgs
  <$> hostPort
  <*> settings

data Args
  = Server ServerArgs
  | Client ClientArgs

makePrisms ''Args

argsParser :: Parser Args
argsParser = helper <*> go
  where
    go = (Server <$> subparser server) <|> (Client <$> subparser client)
    server = command "server" $ info serverArgs $ progDesc "server options" <> fullDesc
    client = command "client" $ info clientArgs $ progDesc "client options" <> fullDesc

argsParserInfo :: ParserInfo Args
argsParserInfo = info argsParser
  $ fullDesc
  <> header "Burn is a pomodoro timer with statistics and client-server archive"
