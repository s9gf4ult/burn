module Burn.Cli where

import Control.Lens
import Data.Traversable
import Options.Applicative

data HostPort = HostPort
  { _hpHost :: String
  , _hpPort :: Int
  } deriving (Eq, Ord, Show)

makeLenses ''HostPort

hostPort :: Parser HostPort
hostPort = HostPort
  <$> strOption host
  <*> option auto port
  where
    host = long "host" <> short 'h'
      <> help "Address to listen at" <> value "0.0.0.0"
    port = long "port" <> short 'p'
      <> help "Port" <> value 1338

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
      <> help "List of commands" <> value []
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

data Args
  = Server HostPort
  | Client HostPort

makePrisms ''Args

argsParser :: Parser Args
argsParser = (Server <$> subparser server) <|> (Client <$> subparser client)
  where
    server = command "server" $ info hostPort mempty
    client = command "client" $ info hostPort mempty

argsParserInfo :: ParserInfo Args
argsParserInfo = info argsParser
  $ fullDesc
  <> header "Burn is a pomodoro timer with statistics and client-server archive"
