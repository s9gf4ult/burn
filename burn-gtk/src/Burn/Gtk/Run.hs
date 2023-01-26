module Burn.Gtk.Run where

import Burn.Gtk.Cli
import Burn.Gtk.Controller
import Burn.Gtk.View
import Control.Lens
import Control.Monad
import Graphics.UI.Gtk
import Options.Applicative

hostPortInfo :: ParserInfo Opts
hostPortInfo = info (helper <*> parseOpts) $
  progDesc "Gtk client for Burn" <> fullDesc

burnGtk :: (String -> IO String) -> IO ()
burnGtk getDataFileName = do
  opts <- execParser hostPortInfo
  _ <- initGUI
  bld <- builderNew
  xmlFile <- getDataFileName "glade/main.glade"
  pixbufs <- join $ initPixbufs
    <$> getDataFileName "images/init-tomat.png"
    <*> getDataFileName "images/red-tomat.png"
    <*> getDataFileName "images/short-tomat.png"
  builderAddFromFile bld xmlFile
  v <- newView bld
  c <- newController opts v pixbufs
  connectSignals v c
  widgetShowAll $ v ^. #main
  mainGUI
