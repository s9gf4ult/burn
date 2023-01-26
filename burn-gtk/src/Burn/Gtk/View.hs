{-# LANGUAGE NoOverloadedStrings #-}

module Burn.Gtk.View where

import Control.Concurrent.STM.TVar
import GHC.Generics (Generic)
import Graphics.UI.Gtk

data View = View
  { main          :: !Window
  , startPomodoro :: !Button
  , startPause    :: !Button
  , counter       :: !Label
  , timeSpent     :: !Label
  , tags          :: !Entry
  , statusIcon    :: !StatusIcon
  , byTags        :: !Expander
  , tagsGrid      :: !(TVar (Maybe Grid))
  } deriving Generic

newView :: Builder -> IO View
newView b = do
  main          <- builderGetObject b castToWindow "main"
  startPomodoro <- builderGetObject b castToButton "pomodoro"
  startPause    <- builderGetObject b castToButton "pause"
  counter       <- builderGetObject b castToLabel "counter"
  timeSpent     <- builderGetObject b castToLabel "time_spent"
  tags          <- builderGetObject b castToEntry "tags"
  byTags        <- builderGetObject b castToExpander "by_tags"
  statusIcon    <- statusIconNew
  tagsGrid      <- newTVarIO Nothing
  return View{..}
