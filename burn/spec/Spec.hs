module Main where

import Burn.API.Types (PomodoroData(..), pdLen)
import Burn.State
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
  , _sState    :: !State
  , _sSettings :: !Settings
  }

instance Default S where
  def = S (UTCTime (fromGregorian 2020 10 10) 0) def def

makeLenses ''S

type SIO = StateT S IO

runSIO :: SIO a -> Assertion
runSIO = void . flip runStateT def

mm :: NominalDiffTime -> NominalDiffTime
mm = (* 60)

spend :: NominalDiffTime -> SIO [Action]
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

send :: Event -> SIO [Action]
send evt = do
  now <- use sNow
  let msg = Message now evt
  s <- use sSettings
  zoom sState $ state $ (snd &&& fst) . process s msg

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
  pomLen <- preuse $ sState . sBurn . _PomCounting . _2 . cLen
  liftBase $ assertEqual "Pomodoro should be: " (Just expected) pomLen

pomodoroSaved :: NominalDiffTime -> [Action] -> SIO ()
pomodoroSaved expected a = do
  let plen = a ^.. folded . _SavePomodoro . pdLen
  liftBase $ assertEqual "Saved pomodoro length: " [expected] plen

pomodoroFinished :: [Action] -> SIO ()
pomodoroFinished =
  liftBase . assertEqual "Pomodoro finished action: " [NotifyPomodoroFinished]

pauseFinished :: [Action] -> SIO ()
pauseFinished =
  liftBase . assertEqual "Pause finished action: " [NotifyPauseFinished]

emptyActions :: [Action] -> SIO ()
emptyActions =
  liftBase . assertEqual "No actions: " []

shortPomodoro :: Assertion
shortPomodoro = runSIO $ do
  send' StartPomodoro
  emptyActions =<< spend (mm 20)
  pomodoroSaved (mm 20) =<< send StartPause
  pauseLenShould $ mm 4
  isolate $ do
    emptyActions =<< spend (mm 1)
    send' StartPomodoro
    pomodoroLenShould $ mm 6.25
    pomodoroFinished =<< spend (mm 6.25)
    pomodoroSaved (mm 6.25) =<< send StartPause
    pauseLenShould $ mm 5
  isolate $ do
    pauseFinished =<< spend (mm 4)
    send' StartPomodoro
    pomodoroLenShould $ mm 25

longPomodoro :: Assertion
longPomodoro = runSIO $ do
  send' StartPomodoro
  pomodoroFinished =<< spend (mm 30)
  pomodoroSaved (mm 30) =<< send StartPause
  pauseLenShould $ mm 6
  isolate $ do
    emptyActions =<< spend (mm 1)
    send' StartPomodoro
    pomodoroLenShould $ (25 * 10) -- 60 * (25 * 1/ 6 )
  isolate $ do
    pauseFinished =<< spend (mm 6)
    send' StartPomodoro
    pomodoroLenShould $ mm 25

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



main :: IO ()
main = defaultMain $ testGroup "Test cases"
  [ testCase "shortPomodoro" shortPomodoro
  , testCase "longPomodoro" longPomodoro
  , testCase "shortPause" shortPause
  ]
