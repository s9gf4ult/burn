module Main where

import Burn.Gtk
import Control.Lens
import Control.Monad
import Graphics.UI.Gtk
import Paths_burn_gtk

main :: IO ()
main = do
  _ <- initGUI
  bld <- builderNew
  xmlFile <- getDataFileName "glade/main.glade"
  pixbufs <- join $ initPixbufs
    <$> getDataFileName "images/init-tomat.png"
    <*> getDataFileName "images/red-tomat.png"
    <*> getDataFileName "images/short-tomat.png"
  builderAddFromFile bld xmlFile
  v <- newView bld
  c <- newController v pixbufs
  connectSignals v c
  widgetShowAll $ v ^. vMain
  mainGUI
