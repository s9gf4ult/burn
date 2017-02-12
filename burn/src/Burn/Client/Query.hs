module Burn.Client.Query where

import Burn.API
import Burn.Optparse
import Control.Lens
import Data.Text as T
import Network.HTTP.Client
import Servant.API
import Servant.Client

startPomodoro :: ClientM ServerState
startPause    :: ClientM ServerState
setTags       :: [Text] -> ClientM ServerState
status        :: ClientM ServerState
(startPomodoro
 :<|> startPause
 :<|> setTags
 :<|> status) = client burnAPI

hostPortClientEnv :: HostPort -> IO ClientEnv
hostPortClientEnv hp = do
  m <- newManager defaultManagerSettings
  let
    host    = hp ^. hpHost
    port    = hp ^. hpPort
    baseUri = BaseUrl Http host port ""
    env     = ClientEnv m baseUri
  return env
