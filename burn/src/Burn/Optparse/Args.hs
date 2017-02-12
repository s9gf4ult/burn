module Burn.Optparse.Args where

import Burn.Optparse.Client
import Burn.Optparse.Server
import Burn.Optparse.Statistics
import Burn.Types
import Control.Lens
import Data.Default
import Data.Monoid
import Data.Time
import Data.Traversable
import Options.Applicative

data Args
  = Server ServerArgs
  | Client ClientArgs
  -- | Statistic StatisticArgs

makePrisms ''Args

argsParser :: Parser Args
argsParser = helper <*> go
  where
    go = (Server <$> subparser server) <|> (Client <$> subparser client)
    server = command "server" $ info (helper <*> serverArgs)
      $ progDesc "server options" <> fullDesc
    client = command "client" $ info (helper <*> clientArgs)
      $ progDesc "client options" <> fullDesc

argsParserInfo :: ParserInfo Args
argsParserInfo = info argsParser
  $ fullDesc
  <> header "Burn is a pomodoro timer with statistics and client-server archive"
