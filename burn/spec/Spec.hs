module Main where

import Burn.Client.State
import Burn.Server.Transform
import Burn.Types
import Control.Arrow
import Control.Lens
import Control.Monad.Base
import Control.Monad.State.Strict hiding (State)
import Data.Default
import Data.Time
import Data.Traversable
import Test.Tasty
import Test.Tasty.HUnit

data S = S
  { _sNow      :: !UTCTime
  , _sTimeZone :: !TimeZone
  , _sState    :: !ServerState
  , _sClient   :: !ClientState
  , _sSettings :: !Settings
  }

instance Default S where
  def = S now utc (mkServerState now) def def
    where
      now = UTCTime (fromGregorian 2020 10 10) 0

makeLenses ''S

type AcNot = Either Notification Action

type SIO = StateT S IO

runSIO :: SIO a -> Assertion
runSIO = void . flip runStateT def

mm :: NominalDiffTime -> NominalDiffTime
mm = (* 60)

spend :: NominalDiffTime -> SIO [AcNot]
spend diff = do
  tt <- for ticks $ \tick -> do
    res <- send Tick
    sNow %= addUTCTime tick
    return res
  rest <- send Tick
  return $ (join tt) ++ rest
  where
    ticks = replicate (truncate diff) 1
    -- Tick every second

spend' :: NominalDiffTime -> SIO ()
spend' = void . spend

send :: Event -> SIO [Either Notification Action]
send evt = do
  now <- use sNow
  let msg = Message now evt
  s         <- use sSettings
  tz        <- use sTimeZone
  actions   <- zoom sState $ state $ (snd &&& fst) . process s tz msg
  newServer <- use sState
  notifs    <- zoom sClient $ state $ (snd &&& fst) . updateState now newServer
  return $ (map Left notifs) ++ (map Right actions)

send' :: Event -> SIO ()
send' = void . send

isolate :: SIO a -> SIO a
isolate action = do
  s <- get
  lift $ evalStateT action s

pauseLenShould :: NominalDiffTime -> SIO ()
pauseLenShould expected = do
  plen <- preuse $ sState . sBurn . _PauseCounting . cLen
  liftBase $ assertEqual "Pause should be: " (Just expected) plen

pomodoroLenShould :: NominalDiffTime -> SIO ()
pomodoroLenShould expected = do
  pomLen <- preuse $ sState . sBurn . _PomodoroCounting . _2 . cLen
  liftBase $ assertEqual "Pomodoro should be: " (Just expected) pomLen

pomodoroSaved :: NominalDiffTime -> [AcNot] -> SIO ()
pomodoroSaved expected a = do
  let plen = sumOf (folded . _Right . _SavePomodoro . pdLen) a
  liftBase $ assertEqual "Saved pomodoro length: " expected plen

pomodoroFinished :: [AcNot] -> SIO ()
pomodoroFinished = expectNotification PomodoroFinished

pauseFinished :: [AcNot] -> SIO ()
pauseFinished = expectNotification PauseFinished

expectNotification :: Notification -> [AcNot] -> SIO ()
expectNotification n a = do
  let nots = a ^.. folded . _Left
  liftBase $ assertEqual "Pause finished action: " [n] nots

emptyActions :: [AcNot] -> SIO ()
emptyActions =
  liftBase . assertEqual "No actions: " []

resetTimers :: [AcNot] -> SIO ()
resetTimers acnot = do
  let ac = acnot ^.. folded . _Right
  liftBase $ ac @?= [ResetTimers]

todayPomodoros :: NominalDiffTime -> SIO ()
todayPomodoros psum = do
  p <- use $ sState . sTodayPomodors
  liftBase $ assertEqual "Today pomodoros" psum $ sumOf (folded . pdLen) p

shortPomodoro :: Assertion
shortPomodoro = runSIO $ do
  send' StartPomodoro
  emptyActions =<< spend (mm 20)
  pomodoroSaved (mm 20) =<< send StartPause
  pauseLenShould $ mm 4
  isolate $ do
    emptyActions =<< spend (mm 1)
    send' StartPomodoro
    pomodoroLenShould $ mm 10
    pomodoroFinished =<< spend (mm 10)
    pomodoroSaved (mm 10) =<< send StartPause
    pauseLenShould $ mm 5
  isolate $ do
    pauseFinished =<< spend (mm 4)
    send' StartPomodoro
    pomodoroLenShould $ mm 25

longPomodoro :: Assertion
longPomodoro = runSIO $ do
  send' StartPomodoro
  pomodoroFinished =<< spend (mm 35)
  pomodoroSaved (mm 35) =<< send StartPause
  pauseLenShould $ mm 7

  isolate $ do
    emptyActions =<< spend (mm 1)
    send' StartPomodoro
    pomodoroLenShould $ (mm -5)
    emptyActions =<< spend (mm 5)
    pomodoroSaved (mm 5) =<< send StartPause
    pauseLenShould $ mm 7

  isolate $ do
    emptyActions =<< spend (mm 2)
    send' StartPomodoro
    pomodoroLenShould 0
    emptyActions =<< spend (mm 5)
    pomodoroSaved (mm 5) =<< send StartPause
    pauseLenShould $ mm 6

  isolate $ do
    emptyActions =<< spend (mm 3)
    send' StartPomodoro
    pomodoroLenShould $ (mm 5)
    pomodoroFinished =<< spend (mm 5)
    pomodoroSaved (mm 5) =<< send StartPause
    pauseLenShould $ mm 5

  isolate $ do
    pauseFinished =<< spend (mm 7)
    send' StartPomodoro
    pomodoroLenShould $ (mm 25)
    emptyActions =<< spend (mm 5)
    pomodoroSaved (mm 5) =<< send StartPause
    pauseLenShould $ mm 1

shortPause :: Assertion
shortPause = runSIO $ do
  send' StartPomodoro
  pomodoroFinished =<< spend (mm 25)
  pomodoroSaved (mm 25) =<< send StartPause
  pauseLenShould $ mm 5
  isolate $ do
    emptyActions =<< spend (mm 1)
    emptyActions =<< send StartPomodoro
    pomodoroLenShould $ mm 5
  isolate $ do
    pauseFinished =<< spend (mm 5)
    emptyActions =<< send StartPomodoro
    pomodoroLenShould $ mm 25

setTags :: Assertion
setTags = runSIO $ do
  send' StartPomodoro
  emptyActions =<< spend (mm 10)
  pomodoroSaved (mm 10) =<< send (SetTags ["new", "tags"])
  emptyActions =<< spend (mm 10)
  pomodoroSaved (mm 10) =<< send (SetTags ["othe", "tags"])
  pomodoroFinished =<< spend (mm 5)
  pomodoroSaved (mm 5) =<< send StartPause

splitDay :: Assertion
splitDay = runSIO $ do
  sNow .= UTCTime (fromGregorian 2020 10 10)
    (secondsToDiffTime ((24 * 3600) - (60 * 25))) -- 25 min till day end
  sSettings . sDayEnd .= TimeOfDay 0 0 0 -- day end match with day end
  send' StartPomodoro
  isolate $ do
    emptyActions =<< spend (mm 24) -- minute till day end
    pomodoroSaved (mm 24) =<< send StartPause
    todayPomodoros (mm 24)
    resetTimers =<< spend (mm 2) -- minute in tomorrow
    todayPomodoros 0
  isolate $ do
    resetTimers =<< spend (mm 30) -- 5 minutes in tomorrow
    pomodoroSaved (mm 30) =<< send StartPause
    todayPomodoros (mm 5)

main :: IO ()
main = defaultMain $ testGroup "Test cases"
  [ testCase "shortPomodoro" shortPomodoro
  , testCase "longPomodoro" longPomodoro
  , testCase "shortPause" shortPause
  , testCase "setTags" setTags
  , testCase "splitDay" splitDay
  ]
