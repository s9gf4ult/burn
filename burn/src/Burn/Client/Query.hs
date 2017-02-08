module Burn.Client.Query where

import Burn.API
import Data.Text as T
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
