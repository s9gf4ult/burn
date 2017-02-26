module Burn.Optparse.Server where

import Burn.Optparse.Settings
import Burn.Types
import Control.Lens
import Data.Default
import Data.Monoid
import Data.Time
import Options.Applicative

data ServerArgs = ServerArgs
  { _saHostPort :: HostPort
  , _saSettings :: Settings
  } deriving (Eq, Ord, Show)

makeLenses ''ServerArgs

serverArgs :: Parser ServerArgs
serverArgs = ServerArgs
  <$> hostPort
  <*> settings
