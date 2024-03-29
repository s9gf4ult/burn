module Burn.Gtk.Controller where

import Burn.API
import Burn.Client as C
import Burn.Gtk.Cli
import Burn.Gtk.Model
import Burn.Gtk.View
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Base
import Data.Default
import Data.Foldable
import Data.Generics.Labels ()
import Data.List as L
import Data.Map.Strict as M
import Data.Set as S
import Data.Text as T
import Data.Time
import Formatting hiding (now)
import GHC.Generics (Generic)
import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.General.Enums
import Servant.Client
import System.Process

-- | Hooks to run when on some events
data Controller = Controller
  { startPomodoro :: !(IO ())
  , startPause    :: !(IO ())
  , tick          :: !(IO ())
  , setTags       :: !(Tags -> IO ())
  } deriving Generic

-- | Pixbufs to switch the tray icon
data Pixbufs = Pixbufs
  { init     :: Pixbuf
  , pomodoro :: Pixbuf
  , pause    :: Pixbuf
  } deriving Generic

initPixbufs :: FilePath -> FilePath -> FilePath -> IO Pixbufs
initPixbufs initImg pom pause = Pixbufs
  <$> pixbufNewFromFile initImg
  <*> pixbufNewFromFile pom
  <*> pixbufNewFromFile pause

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

-- | Infer pomodor from current server state
currentPomodor :: ServerState -> Maybe (PomodoroData UTCTime)
currentPomodor s =
  let
    counting = s ^? #burn . #_PomodoroCounting . _2
    started  = counting ^? _Just . #started
    now      = s ^. #lastMsg
    len      = diffUTCTime now <$> started
    tags     = Tags $ s ^. #tags
  in PomodoroData <$> started <*> len <*> pure tags

-- | Remove all packed witdgets and add new grid widget with given text
updateTagsCounts :: Expander -> TVar (Maybe Grid) -> [(Text, Text)] -> IO ()
updateTagsCounts byTags gridVar texts = do
  grid <- gridNew
  for_ (L.zip [0..] texts) $ \(row, (lText, rText)) -> do
    let
      lbl t = do
        w <- labelNew $ Just t
        Gtk.set w [widgetExpand := True]
        widgetSetHAlign w AlignStart
        widgetSetVAlign w AlignFill
        labelSetJustify w JustifyLeft
        return w
    lLabel <- lbl lText
    Gtk.set lLabel [widgetMarginLeft := 30]
    rLabel <- lbl rText
    gridAttach grid lLabel 0 row 1 1
    gridAttach grid rLabel 1 row 1 1
  oldGrid <- atomically $ do
    old <- readTVar gridVar
    writeTVar gridVar $ Just grid
    return old
  for_ oldGrid $ \og -> do
    widgetDestroy og
  containerAdd byTags grid
  widgetShowAll grid

byTagTimes :: [PomodoroData UTCTime] -> [(Text, Text)]
byTagTimes pomodors =
  let
    byTag = M.fromListWith (<>) $ do
      pmd <- pomodors
      tag :: Text <- pmd ^. #tags . #_Tags
      return (tag, [pmd])
    fmt (tag, p) =
      (tag, formatTimeDiff $ sumOf (folded . #length) p)
    noTag = fmt ("No tag", L.filter (\a -> a ^. #tags == Tags []) pomodors)
    tagged = fmt <$> M.toAscList byTag
  in noTag : tagged

updateView :: View -> Pixbufs -> TVar Model -> ServerState -> IO ()
updateView v pbs tModel s = do
  let
    now = s ^. #lastMsg
    formatCounting c =
      let
        passed = formatTimeDiff $ diffUTCTime now $ c ^. #started
        len = formatTimeDiff $ c ^. #length
      in passed <> "/" <> len
    counterText = case s ^. #burn of
      Waiting -> "--:--"
      PomodoroCounting _ c -> "P " <> formatCounting c
      PauseCounting c -> "  " <> formatCounting c
    allPomodors = (currentPomodor s ^.. _Just) ++ (s ^. #todayPomodoros)
    timeSpent = formatTimeDiff $ sumOf (folded . #length) allPomodors
    pbuf = case s ^. #burn of
      Waiting             -> pbs ^. #init
      PomodoroCounting {} -> pbs ^. #pomodoro
      PauseCounting {}    -> pbs ^. #pause
  mNewTags <- atomically $ do
    model <- readTVar tModel
    let
      newT = s ^. #tags
      oldT = model ^. mTags . #_Tags
    writeTVar tModel $ model & (mTags . #_Tags) .~ newT
    return $ if S.fromList oldT == S.fromList newT
             then Nothing
             else Just newT
  postGUISync $ do
    labelSetText (v ^. #counter) counterText
    labelSetText (v ^. #timeSpent) timeSpent
    for_ mNewTags $ \tags -> do
      entrySetText (v ^. #tags) $ T.unwords tags
    statusIconSetFromPixbuf (v ^. #statusIcon) pbuf
    statusIconSetTooltipText (v ^. #statusIcon) $ Just counterText
    updateTagsCounts (v ^. #byTags) (v ^. #tagsGrid) $ byTagTimes allPomodors

newController :: Opts -> View -> Pixbufs -> IO Controller
newController opts v pbs = do
  env <- hostPortClientEnv $ opts ^. #hostPort
  clientState <- newTVarIO def
  clientModel <- newTVarIO def
  let
    method clientCall = void $ forkIO $ do
      resp <- runClientM clientCall env
      case resp of
        Left e -> print e
        Right newSt -> do
          let now = newSt ^. #lastMsg
          updateView v pbs clientModel newSt
          notifs <- atomically $ do
            oldClient <- readTVar clientState
            let (newClient, notifs) = updateState now newSt oldClient
            writeTVar clientState newClient
            return notifs
          traverse_ showNotification notifs
    result = Controller
      { startPomodoro = method C.startPomodoro
      , startPause    = method C.startPause
      , tick          = method C.status
      , setTags       = \tags -> do
          atomically $ do
            modifyTVar clientModel $ mTags .~ tags
          method $ C.setTags $ tags ^. #_Tags
      }
  return result
  where
    showNotification = \case
      PomodoroFinished -> for_ (opts ^. #pomodoroCommand) $ \c ->
        system c
      PauseFinished -> for_ (opts ^. #pauseCommand) $ \c ->
        system c

connectSignals :: View -> Controller -> IO ()
connectSignals v c = do
  void $ on (v ^. #main) deleteEvent (False <$ liftBase mainQuit)
  void $ on (v ^. #startPomodoro) buttonActivated $ c ^. #startPomodoro
  void $ on (v ^. #startPause) buttonActivated $ c ^. #startPause
  void $ on (v ^. #tags) entryActivated $ do
    t <- entryGetText $ v ^. #tags
    (c ^. #setTags) $ Tags $ T.strip <$> T.words t

  _ <- forkIO $ forever $ do
    threadDelay 1e6
    c ^. #tick
  return ()
