module Burn.Optparse.Client where

import Burn.Optparse.Settings
import Burn.Types
import Control.Lens
import Data.Monoid
import Data.Text
import Data.Traversable
import GHC.Generics (Generic)
import Options.Applicative

import qualified Data.Text as T

data Command
  = CPomodoro
  | CPause
  | CSetTags Tags
  deriving (Eq, Ord, Show)

readCommand :: Text -> Either String Command
readCommand t = case T.strip <$> T.words t of
  ["pomodoro"]  -> Right CPomodoro
  ["pause"]     -> Right CPause
  ("tags":tags) -> Right $ CSetTags $ Tags tags
  (x:_)         -> Left $ "unknown command \"" <> T.unpack x <> "\""
  []            -> Left "empty command string"

parseCommands :: Parser [Command]
parseCommands = option go m
  where
    m = long "commands" <> short 'c'
      <> help "Comma separated list of commands" <> value []
    go = do
      s <- T.pack <$> str
      for (T.splitOn "," s) $ \c -> case readCommand $ T.strip c of
        Left e -> readerError e
        Right a -> return a

data ClientArgs = ClientArgs
  { hostPort :: HostPort
  , commands :: [Command]
  } deriving (Eq, Ord, Show, Generic)


clientArgs :: Parser ClientArgs
clientArgs = ClientArgs
  <$> parseHostPort
  <*> parseCommands
