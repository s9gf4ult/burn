module Burn.Gtk.Controller where

import Burn.API
import Burn.Client
import Burn.Gtk.Model
import Burn.Gtk.View
import Burn.Optparse
import Burn.Types
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
import Data.Monoid
import Data.Set as S
import Data.Text as T
import Data.Time
import Formatting hiding (now)
import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.General.Enums
import Servant.Client
import System.Process

-- | Hooks to run when on some events
data Controller = Controller
  { _cStartPomodoro :: !(IO ())
  , _cStartPause    :: !(IO ())
  , _cTick          :: !(IO ())
  , _cSetTags       :: !(Tags -> IO ())
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

-- | Infer pomodor from current server state
currentPomodor :: ServerState -> Maybe (PomodoroData UTCTime)
currentPomodor s =
  let
    counting = s ^? sBurn . _PomodoroCounting . _2
    started  = counting ^? _Just . cStarted
    now      = s ^. sLastMsg
    len      = diffUTCTime now <$> started
    tags     = Tags $ s ^. sTags
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
      tag :: Text <- pmd ^. #pdTags . #_Tags
      return (tag, [pmd])
    fmt (tag, p) =
      (tag, formatTimeDiff $ sumOf (folded . #pdLen) p)
    noTag = fmt ("No tag", L.filter (\a -> a ^. #pdTags == Tags []) pomodors)
    tagged = fmt <$> M.toAscList byTag
  in noTag : tagged

updateView :: View -> Pixbufs -> TVar Model -> ServerState -> IO ()
updateView v pbs tModel s = do
  let
    now = s ^. sLastMsg
    formatCounting c =
      let
        passed = formatTimeDiff $ diffUTCTime now $ c ^. cStarted
        len = formatTimeDiff $ c ^. cLen
      in passed <> "/" <> len
    counterText = case s ^. sBurn of
      Waiting -> "--:--"
      PomodoroCounting _ c -> "P " <> formatCounting c
      PauseCounting c -> "  " <> formatCounting c
    allPomodors = (currentPomodor s ^.. _Just) ++ (s ^. sTodayPomodors)
    timeSpent = formatTimeDiff $ sumOf (folded . #pdLen) allPomodors
    pbuf = case s ^. sBurn of
      Waiting             -> pbs ^. pInit
      PomodoroCounting {} -> pbs ^. pPomodoro
      PauseCounting {}    -> pbs ^. pPause
  mNewTags <- atomically $ do
    model <- readTVar tModel
    let
      newT = s ^. sTags
      oldT = model ^. mTags . #_Tags
    writeTVar tModel $ model & (mTags . #_Tags) .~ newT
    return $ if S.fromList oldT == S.fromList newT
             then Nothing
             else Just newT
  postGUISync $ do
    labelSetText (v ^. vCounter) counterText
    labelSetText (v ^. vTimeSpent) timeSpent
    for_ mNewTags $ \tags -> do
      entrySetText (v ^. vTags) $ T.unwords tags
    statusIconSetFromPixbuf (v ^. vStatusIcon) pbuf
    statusIconSetTooltipText (v ^. vStatusIcon) $ Just counterText
    updateTagsCounts (v ^. vByTags) (v ^. vTagsGrid) $ byTagTimes allPomodors

newController :: HostPort -> View -> Pixbufs -> IO Controller
newController hp v pbs = do
  env <- hostPortClientEnv hp
  clientState <- newTVarIO def
  clientModel <- newTVarIO def
  let
    method clientCall = void $ forkIO $ do
      resp <- runClientM clientCall env
      case resp of
        Left e -> print e
        Right newSt -> do
          let now = newSt ^. sLastMsg
          updateView v pbs clientModel newSt
          notifs <- atomically $ do
            oldClient <- readTVar clientState
            let (newClient, notifs) = updateState now newSt oldClient
            writeTVar clientState newClient
            return notifs
          traverse_ showNotification notifs
    result = Controller
      { _cStartPomodoro = method startPomodoro
      , _cStartPause    = method startPause
      , _cTick          = method status
      , _cSetTags = \tags -> do
          atomically $ do
            modifyTVar clientModel $ mTags .~ tags
          method $ setTags $ tags ^. #_Tags
      }
  return result
  where
    showNotification = \case
      PomodoroFinished ->
        void $ rawSystem "notify-send" ["-t", "0", "Take a break!"]
      PauseFinished ->
        void $ rawSystem "notify-send" ["-t", "0", "Go to work, lazy ass!"]

connectSignals :: View -> Controller -> IO ()
connectSignals v c = do
  void $ on (v ^. vMain) deleteEvent (False <$ liftBase mainQuit)
  void $ on (v ^. vStartPomodoro) buttonActivated $ c ^. cStartPomodoro
  void $ on (v ^. vStartPause) buttonActivated $ c ^. cStartPause
  void $ on (v ^. vTags) entryActivated $ do
    t <- entryGetText $ v ^. vTags
    (c ^. cSetTags) $ Tags $ T.strip <$> T.words t

  _ <- forkIO $ forever $ do
    threadDelay 1e6
    c ^. cTick
  return ()
