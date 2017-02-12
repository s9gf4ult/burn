module Main where

import Burn.Cli
import Burn.Optparse
import Options.Applicative

main :: IO ()
main = do
  args <- execParser argsParserInfo
  burnCli args
