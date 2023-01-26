module Burn.Optparse.Client where

import Burn.Optparse.Settings
import qualified Burn.Types as T
import Control.Lens
import Data.Monoid
import Data.Text
import Data.Time
import Data.Traversable
import GHC.Generics (Generic)
import Options.Applicative

import qualified Data.Text as T

data Command
  = Pomodoro
  | Pause
  | SetTags T.Tags
  | SetOption T.SetOption
  deriving (Eq, Ord, Show)

readCommand :: Text -> Either String Command
readCommand t = case T.strip <$> T.words t of
  ["pomodoro"]  -> Right Pomodoro
  ["pause"]     -> Right Pause
  ("tags":tags) -> Right $ SetTags $ T.Tags tags
  ("set":option) -> SetOption <$> readSetOption option
  (x:_)         -> Left $ "unknown command \"" <> T.unpack x <> "\""
  []            -> Left "empty command string"

readSetOption :: [Text] -> Either String T.SetOption
readSetOption = \case
  ["pomodoro", len] -> T.SetPomodoroLength <$> readMinutes len
  ["pause", len] -> T.SetPauseLength <$> readMinutes len
  _ -> Left "Unknown option to set"

readMinutes :: Text -> Either String NominalDiffTime
readMinutes t = case t ^? to T.unpack . _Show of
  Nothing -> Left "Could not read the number"
  Just n  -> Right $ secondsToNominalDiffTime $ n * 60

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
