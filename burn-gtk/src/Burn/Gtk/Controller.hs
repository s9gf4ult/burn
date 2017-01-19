module Burn.Gtk.Controller where

import Burn.API
import Burn.Client
import Burn.Gtk.View
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Base
import Data.Foldable
import Data.Monoid
import Data.Text as T
import Formatting
import Graphics.UI.Gtk
import Network.HTTP.Client
import Servant.Client

-- | Hooks to run when on some events
data Controller = Controller
  { _cStartPomodoro :: IO ()
  , _cStartPause    :: IO ()
  , _cTick          :: IO ()
  }

makeLenses ''Controller

updateView :: View -> Status -> IO ()
updateView v st = do
  traverse_ showNotification $ st ^. asNotifications
  let
    s = st ^. asState
    tt :: Int -> Text
    tt seconds =
      let (mins, secs) = seconds `divMod` 60
      in sformat (left 0 '0' % ":" % left 2 '0') mins secs
    formatCounting c = (tt $ c ^. cPassed) <> "/" <> (tt $ c ^. cLeft)
    counterText = case s ^. sCounting of
      Waiting -> "--:--"
      PomdoroCounting c -> "P " <> formatCounting c
      PauseCounting c -> "  " <> formatCounting c
    timeSpent = "FIXME: implement" :: Text
  labelSetText (v ^. vCounter) counterText
  labelSetText (v ^. vTimeSpent) timeSpent
  entrySetText (v ^. vTags) $ T.unwords $ s ^. sTags
  where
    showNotification n = print n -- FIXME:

newController :: View -> IO Controller
newController v = do
  m <- newManager defaultManagerSettings
  let
    base = BaseUrl Http "127.0.0.1" 1338 "" -- FIXME: get from params
    env = ClientEnv m base
  return $ Controller
    { _cStartPomodoro = runClientM startPomodoro env >>= either print (updateView v)
    , _cStartPause = runClientM startPause env >>= either print (updateView v)
    , _cTick = runClientM status env >>= either print (updateView v)
    }

connectSignals :: View -> Controller -> IO ()
connectSignals v c = do
  _ <- on (v ^. vMain) deleteEvent (False <$ liftBase mainQuit)
  _ <- on (v ^. vStartPomodoro) buttonActivated $ c ^. cStartPomodoro
  _ <- on (v ^. vStartPause) buttonActivated $ c ^. cStartPause
  _ <- forkIO $ forever $ do
    threadDelay 1e6
    c ^. cTick

  return ()
