module Burn.Gtk.Cli where

import Burn.Optparse as Opt
import GHC.Generics (Generic)
import Options.Applicative

parseOpts :: Parser Opts
parseOpts = Opts
  <$> Opt.parseHostPort
  <*> optional (strOption $ long "pom-command" <> help "Run command on pomodoro end")
  <*> optional (strOption $ long "pause-command" <> help "Run command on pause end")

data Opts = Opts
  { hostPort        :: HostPort
  , pomodoroCommand :: Maybe String
  , pauseCommand    :: Maybe String
  } deriving (Show, Eq, Generic)
