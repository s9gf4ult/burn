module Burn.API where

import Burn.API.Types
import Servant.API

type BurnAPI
  =    StartPomodoroAPI
  :<|> StartPauseAPI
  :<|> StatusAPI

type StartPomodoroAPI
  = "action" :> "start_pomodoro" :> Post '[JSON] Status

type StartPauseAPI
  = "action" :> "start_pause" :> Post '[JSON] Status

type StatusAPI
  = "status" :> Get '[JSON] Status
