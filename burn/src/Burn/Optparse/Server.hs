module Burn.Optparse.Server where

import Burn.Optparse.Settings as Opt
import Burn.Types
import GHC.Generics (Generic)
import Options.Applicative

data ServerArgs = ServerArgs
  { hostPort :: HostPort
  , settings :: Settings
  } deriving (Eq, Ord, Show, Generic)

parseServerArgs :: Parser ServerArgs
parseServerArgs = ServerArgs
  <$> Opt.parseHostPort
  <*> Opt.parseSettings
