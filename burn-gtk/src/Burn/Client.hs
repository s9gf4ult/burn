module Burn.Client where

import Burn.API
import Data.Text as T
import Network.HTTP.Client
import Servant.API
import Servant.Client

startPomodoro :: Manager -> BaseUrl -> ClientM Status
startPause :: Manager -> BaseUrl -> ClientM Status
-- setTags :: Manager -> BaseUrl -> [Text] -> ClientM Status
status :: Manager -> BaseUrl -> ClientM Status
(startPomodoro
 :<|> startPause
 :<|> setTags
 :<|> status) = client burnAPI
