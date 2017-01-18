module Main where

import Burn.Gtk.View
import Control.Lens
import Graphics.UI.Gtk
import Paths_burn_gtk

main :: IO ()
main = do
  _ <- initGUI
  bld <- builderNew
  xmlFile <- getDataFileName "glade/main.glade"
  builderAddFromFile bld xmlFile
  v <- newView bld
  widgetShowAll $ v ^. vMain
  mainGUI
