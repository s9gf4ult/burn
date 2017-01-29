module Burn.API
  ( module Burn.API
  , module Burn.Types
  ) where

import Burn.Types
import Data.Proxy
import Data.Text as T
import Servant.API

type BurnAPI
  =    StartPomodoroAPI
  :<|> StartPauseAPI
  :<|> SetTagsAPI
  :<|> TickAPI

burnAPI :: Proxy BurnAPI
burnAPI = Proxy

type StartPomodoroAPI
  = "action" :> "start_pomodoro" :> Post '[JSON] State

type StartPauseAPI
  = "action" :> "start_pause" :> Post '[JSON] State

type SetTagsAPI
  = "action" :> "set_tags" :> ReqBody '[JSON] [Text] :> Post '[JSON] State

type TickAPI
  = "tick" :> Get '[JSON] State
