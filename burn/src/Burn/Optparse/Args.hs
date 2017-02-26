module Burn.Optparse.Args where

import Burn.Optparse.Client
import Burn.Optparse.Server
import Burn.Optparse.Statistics
import Control.Lens
import Data.Monoid
import Options.Applicative

data Args
  = Server ServerArgs
  | Client ClientArgs
  | Statistics StatArgs

makePrisms ''Args

argsParser :: Parser Args
argsParser = helper <*> go
  where
    go = (Server <$> subparser server) <|> (Client <$> subparser client)
      <|> (Statistics <$> subparser stat)
    server = command "server" $ info (helper <*> serverArgs)
      $ progDesc "server options" <> fullDesc
    client = command "client" $ info (helper <*> clientArgs)
      $ progDesc "client options" <> fullDesc
    stat = command "stat" $ info (helper <*> statArgs)
      $ progDesc "statistics options" <> fullDesc

argsParserInfo :: ParserInfo Args
argsParserInfo = info argsParser
  $ fullDesc
  <> header "Burn is a pomodoro timer with statistics and client-server archive"
