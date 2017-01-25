module Main where

import System.Environment
import Burn.Cli

main :: IO ()
main = do
  args <- getArgs
  let cmds = traverse evtStr args
  case cmds of
    Left e -> print e
    Right c -> execute $ Send c
