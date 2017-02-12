module Main where

import Burn.Cli
import Burn.CliRun
import Options.Applicative

main :: IO ()
main = do
  args <- execParser argsParserInfo
  burnCli args
