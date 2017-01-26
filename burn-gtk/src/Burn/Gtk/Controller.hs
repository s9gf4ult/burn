module Burn.Gtk.Controller where

import Burn.API
import Burn.Client
import Burn.Gtk.View
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Base
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Text as T
import Data.Time
import Formatting
import Graphics.UI.Gtk
import Network.HTTP.Client
import Servant.Client
import System.Process

-- | Hooks to run when on some events
data Controller = Controller
  { _cStartPomodoro :: IO ()
  , _cStartPause    :: IO ()
  , _cTick          :: IO ()
  }

makeLenses ''Controller

data Pixbufs = Pixbufs
  { _pInit     :: Pixbuf
  , _pPomodoro :: Pixbuf
  , _pPause    :: Pixbuf
  }

makeLenses ''Pixbufs

initPixbufs :: FilePath -> FilePath -> FilePath -> IO Pixbufs
initPixbufs initImg pom pause = do
  _pInit <- pixbufNewFromFile initImg
  _pPomodoro <- pixbufNewFromFile pom
  _pPause <- pixbufNewFromFile pause
  return $ Pixbufs{..}

formatTimeDiff :: NominalDiffTime -> Text
formatTimeDiff (truncate -> seconds) =
  let
    (mins', secs) = divMod (max 0 seconds) (60 :: Int)
    (hours', mins) = divMod mins' 60
    (days, hours) = divMod hours' 24
    ms = sformat (left 2 '0' % ":" % left 2 '0') mins secs
    hms = sformat (left 2 '0' % ":" % stext) hours ms
    dhms = sformat (shown % "d" % stext) days hms
    res = if
      | days  > 0 -> dhms
      | hours > 0 -> hms
      | otherwise -> ms
  in res

updateView :: View -> Pixbufs -> Status -> IO ()
updateView v pbs st = do
  traverse_ showNotification $ st ^. asNotifications
  let
    s = st ^. asState
    formatCounting c = (formatTimeDiff $ c ^. cPassed) <> "/" <> (formatTimeDiff $ c ^. cLen)
    counterText = case s ^. sCounting of
      Waiting -> "--:--"
      PomodoroCounting c -> "P " <> formatCounting c
      PauseCounting c -> "  " <> formatCounting c
    currentPomodor = fromMaybe 0 $ s ^? sCounting . _PomodoroCounting . cPassed
    todayPomodoros = sumOf (sTodayPomodors . folded . pdLen) s
    spentSeconds = currentPomodor + todayPomodoros
    timeSpent = formatTimeDiff $ spentSeconds
    pbuf = case s ^. sCounting of
      Waiting             -> pbs ^. pInit
      PomodoroCounting {} -> pbs ^. pPomodoro
      PauseCounting {}    -> pbs ^. pPause
  labelSetText (v ^. vCounter) counterText
  labelSetText (v ^. vTimeSpent) timeSpent
  entrySetText (v ^. vTags) $ T.unwords $ s ^. sTags
  statusIconSetFromPixbuf (v ^. vStatusIcon) pbuf
  statusIconSetTooltipText (v ^. vStatusIcon) $ Just counterText
  where
    showNotification = \case
      PomodoroFinish ->
        void $ rawSystem "notify-send" ["-t", "0", "Take a break!"]
      PauseFinish ->
        void $ rawSystem "notify-send" ["-t", "0", "Go to work, lazy ass!"]

newController :: View -> Pixbufs -> IO Controller
newController v pbs = do
  m <- newManager defaultManagerSettings
  let
    baseUri = BaseUrl Http "127.0.0.1" 1338 "" -- FIXME: get from params
    env = ClientEnv m baseUri
  return $ Controller
    { _cStartPomodoro = runClientM startPomodoro env >>= either print (updateView v pbs)
    , _cStartPause = runClientM startPause env >>= either print (updateView v pbs)
    , _cTick = runClientM status env >>= either print (updateView v pbs)
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
