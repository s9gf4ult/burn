module Burn.TH where

import           Control.Exception
import           Data.Text         as T
import           Text.Inflections

toUnderscore' :: String -> String
toUnderscore' = either throw T.unpack . toUnderscore . T.pack
