module Burn.API
  ( module Burn.API
  , module Burn.API.Types
  ) where

import Burn.API.Types
import Data.Proxy
import Data.Text as T
import Servant.API

type BurnAPI
  =    StartPomodoroAPI
  :<|> StartPauseAPI
  :<|> SetTagsAPI
  :<|> StatusAPI

burnAPI :: Proxy BurnAPI
burnAPI = Proxy

type StartPomodoroAPI
  = "action" :> "start_pomodoro" :> Post '[JSON] Status

type StartPauseAPI
  = "action" :> "start_pause" :> Post '[JSON] Status

type SetTagsAPI
  = "action" :> "set_tags" :> ReqBody '[JSON] [Text] :> Post '[JSON] Status

type StatusAPI
  = "status" :> Get '[JSON] Status