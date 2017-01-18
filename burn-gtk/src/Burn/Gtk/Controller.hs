module Burn.Gtk.Controller where

import Burn.Gtk.View
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Base
import Graphics.UI.Gtk

-- | Hooks to run when on some events
data Controller = Controller
  { _cStartPomodoro :: IO ()
  , _cStartPause    :: IO ()
  , _cTick          :: IO ()
  }

makeLenses ''Controller

newController :: IO Controller
newController = return $ Controller {}

connectSignals :: View -> Controller -> IO ()
connectSignals v c = do
  _ <- on (v ^. vMain) deleteEvent (False <$ liftBase mainQuit)
  _ <- on (v ^. vStartPomodoro) buttonActivated $ c ^. cStartPomodoro
  _ <- on (v ^. vStartPause) buttonActivated $ c ^. cStartPause
  forkIO $ forever $ do
    threadDelay 1e6
    c ^. cTick

  return ()
