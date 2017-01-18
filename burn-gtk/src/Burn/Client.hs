module Burn.Client where

import Burn.API
import Data.Text as T
import Servant.API
import Servant.Client

startPomodoro :: ClientM Status
startPause    :: ClientM Status
setTags       :: [Text] -> ClientM Status
status        :: ClientM Status
(startPomodoro
 :<|> startPause
 :<|> setTags
 :<|> status) = client burnAPI
