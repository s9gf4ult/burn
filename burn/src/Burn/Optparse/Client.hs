module Burn.Optparse.Client where

import Burn.Optparse.Settings
import Burn.Types
import Control.Lens
import Data.Default
import Data.Monoid
import Data.Time
import Data.Traversable
import Options.Applicative

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
