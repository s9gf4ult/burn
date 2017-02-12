module Burn.Gtk.Controller where

import Burn.API
import Burn.Client
import Burn.Gtk.View
import Burn.Optparse
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Base
import Data.Default
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Text as T
import Data.Time
import Formatting hiding (now)
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

updateView :: View -> Pixbufs -> UTCTime -> ServerState -> IO ()
updateView v pbs now s = do
  print s
  let
    formatCounting c =
      let
        passed = formatTimeDiff $ diffUTCTime now $ c ^. cStarted
        len = formatTimeDiff $ c ^. cLen
      in passed <> "/" <> len
    counterText = case s ^. sBurn of
      Waiting -> "--:--"
      PomodoroCounting _ c -> "P " <> formatCounting c
      PauseCounting c -> "  " <> formatCounting c
    currentPomodor =
      let
        mPassed = s ^? sBurn . _PomodoroCounting . _2 . cStarted
          . to (diffUTCTime now)
      in fromMaybe 0 mPassed
    todayPomodoros = sumOf (sTodayPomodors . folded . pdLen) s
    spentSeconds = currentPomodor + todayPomodoros
    timeSpent = formatTimeDiff $ spentSeconds
    pbuf = case s ^. sBurn of
      Waiting             -> pbs ^. pInit
      PomodoroCounting {} -> pbs ^. pPomodoro
      PauseCounting {}    -> pbs ^. pPause
  postGUISync $ do
    labelSetText (v ^. vCounter) counterText
    labelSetText (v ^. vTimeSpent) timeSpent
    entrySetText (v ^. vTags) $ T.unwords $ s ^. sTags
    statusIconSetFromPixbuf (v ^. vStatusIcon) pbuf
    statusIconSetTooltipText (v ^. vStatusIcon) $ Just counterText

newController :: HostPort -> View -> Pixbufs -> IO Controller
newController hp v pbs = do
  env <- hostPortClientEnv hp
  clientState <- newTVarIO def
  let
    method clientCall = void $ forkIO $ do
      resp <- runClientM clientCall env
      case resp of
        Left e -> print e
        Right newSt -> do
          now <- getCurrentTime         -- FIXME: get from server
          updateView v pbs now newSt
          notifs <- atomically $ do
            oldClient <- readTVar clientState
            let (newClient, notifs) = updateState now newSt oldClient
            writeTVar clientState newClient
            return notifs
          traverse_ showNotification notifs

  return $ Controller
    { _cStartPomodoro = method startPomodoro
    , _cStartPause    = method startPause
    , _cTick          = method status
    }
  where
    showNotification = \case
      PomodoroFinished ->
        void $ rawSystem "notify-send" ["-t", "0", "Take a break!"]
      PauseFinished ->
        void $ rawSystem "notify-send" ["-t", "0", "Go to work, lazy ass!"]


connectSignals :: View -> Controller -> IO ()
connectSignals v c = do
  _ <- on (v ^. vMain) deleteEvent (False <$ liftBase mainQuit)
  _ <- on (v ^. vStartPomodoro) buttonActivated $ c ^. cStartPomodoro
  _ <- on (v ^. vStartPause) buttonActivated $ c ^. cStartPause
  _ <- forkIO $ forever $ do
    threadDelay 1e6
    c ^. cTick

  return ()
