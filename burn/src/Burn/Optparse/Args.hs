module Burn.Optparse.Args where

import Burn.Optparse.Client
import Burn.Optparse.Elastic
import Burn.Optparse.Server
import Burn.Optparse.Statistics
import Control.Lens
import Options.Applicative

data Args
  = Server ServerArgs
  | Client ClientArgs
  -- | Elastic ElasticArgs
  | Statistics StatArgs

makePrisms ''Args

argsParser :: Parser Args
argsParser = helper <*> go
  where
    go = (Server <$> subparser server) <|> (Client <$> subparser client)
      -- <|> (Elastic <$> subparser elastic)
      <|> (Statistics <$> subparser stat)
    server = command "server" $ info (helper <*> parseServerArgs)
      $ progDesc "start burn server" <> fullDesc
    client = command "client" $ info (helper <*> clientArgs)
      $ progDesc "send command to server" <> fullDesc
    -- elastic = command "elastic" $ info (helper <*> elasticArgs)
    --   $ progDesc "upload data to Elastic Search" <> fullDesc
    stat = command "stat" $ info (helper <*> statArgs)
      $ progDesc "run statistics query" <> fullDesc

argsParserInfo :: ParserInfo Args
argsParserInfo = info argsParser
  $ fullDesc
  <> header "Burn is a pomodoro timer with statistics and client-server archive"
