module Burn.Gtk.Run where

import Burn.Gtk.Controller
import Burn.Gtk.View
import Burn.Optparse
import Control.Lens
import Control.Monad
import Data.Monoid
import Graphics.UI.Gtk
import Options.Applicative

hostPortInfo :: ParserInfo HostPort
hostPortInfo = info (helper <*> hostPort) $
  progDesc "Gtk client for Burn" <> fullDesc

burnGtk :: (String -> IO String) -> IO ()
burnGtk getDataFileName = do
  hp <- execParser hostPortInfo
  _ <- initGUI
  bld <- builderNew
  xmlFile <- getDataFileName "glade/main.glade"
  pixbufs <- join $ initPixbufs
    <$> getDataFileName "images/init-tomat.png"
    <*> getDataFileName "images/red-tomat.png"
    <*> getDataFileName "images/short-tomat.png"
  builderAddFromFile bld xmlFile
  v <- newView bld
  c <- newController hp v pixbufs
  connectSignals v c
  widgetShowAll $ v ^. #main
  mainGUI
