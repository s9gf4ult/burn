module Burn.Client where

import Burn.API
import Data.Text as T
import Servant.API
import Servant.Client

startPomodoro :: ClientM State
startPause    :: ClientM State
setTags       :: [Text] -> ClientM State
status        :: ClientM State
(startPomodoro
 :<|> startPause
 :<|> setTags
 :<|> status) = client burnAPI
