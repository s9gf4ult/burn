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
  :<|> SetOptionAPI
  :<|> TickAPI

burnAPI :: Proxy BurnAPI
burnAPI = Proxy

type StartPomodoroAPI
  = "action" :> "start_pomodoro" :> Post '[JSON] ServerState

type StartPauseAPI
  = "action" :> "start_pause" :> Post '[JSON] ServerState

type SetTagsAPI
  = "action" :> "set_tags" :> ReqBody '[JSON] [Text] :> Post '[JSON] ServerState

type SetOptionAPI
  = "action" :> "set_option" :> ReqBody '[JSON] SetOption :> Post '[JSON] ServerState

type TickAPI
  = "tick" :> Get '[JSON] ServerState
