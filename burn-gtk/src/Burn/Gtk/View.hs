{-# LANGUAGE NoOverloadedStrings #-}

module Burn.Gtk.View where

import Control.Concurrent.STM.TVar
import Control.Lens
import Graphics.UI.Gtk

data View = View
  { _vMain          :: !Window
  , _vStartPomodoro :: !Button
  , _vStartPause    :: !Button
  , _vCounter       :: !Label
  , _vTimeSpent     :: !Label
  , _vTags          :: !Entry
  , _vStatusIcon    :: !StatusIcon
  , _vByTags        :: !Expander
  , _vTagsGrid      :: !(TVar (Maybe Grid))
  }

makeLenses ''View

newView :: Builder -> IO View
newView b = do
  _vMain          <- builderGetObject b castToWindow "main"
  _vStartPomodoro <- builderGetObject b castToButton "pomodoro"
  _vStartPause    <- builderGetObject b castToButton "pause"
  _vCounter       <- builderGetObject b castToLabel "counter"
  _vTimeSpent     <- builderGetObject b castToLabel "time_spent"
  _vTags          <- builderGetObject b castToEntry "tags"
  _vByTags        <- builderGetObject b castToExpander "by_tags"
  _vStatusIcon    <- statusIconNew
  _vTagsGrid      <- newTVarIO Nothing
  return View{..}
