module Main where

import Burn.State
import Control.Arrow
import Control.Lens
import Control.Monad.Base
import Control.Monad.State.Strict hiding (State)
import Data.Default
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit

data S = S
  { _sNow      :: !UTCTime
  , _sState    :: !State
  , _sSettings :: !Settings
  }

instance Default S where
  def = S (UTCTime (fromGregorian 2020 10 10) 0) def def

type SIO = StateT S IO

makeLenses ''S

mm :: NominalDiffTime -> NominalDiffTime
mm = (* 60)

spend :: NominalDiffTime -> SIO ()
spend diff = sNow %= addUTCTime diff

send :: Event -> SIO [Action]
send evt = do
  now <- use sNow
  let msg = Message now evt
  s <- use sSettings
  zoom sState $ state $ (snd &&& fst) . process s msg

isolate :: SIO a -> SIO a
isolate action = do
  s <- get
  lift $ evalStateT action s

shortPomodoro :: Assertion
shortPomodoro = void $ flip runStateT def $ do
  send StartPomodoro
  spend $ mm 20
  a <- send StartPause
  let pmd = a ^? folded . _SavePomodoro . pdLen
  liftBase $ pmd @?= Just (mm 20)
  shortPause <- preuse $ sState . sBurn . _PauseCounting . cLen
  liftBase $ shortPause @?= Just (mm 4)
  isolate $ do
    spend $ mm 1
    send StartPomodoro
    plen <- preuse $ sState . sBurn . _PomCounting . _2 . cLen
    let expPlen = mm 6.25
    liftBase $ plen @?= Just expPlen
    spend $ expPlen
    b <- send StartPause
    liftBase $ (b ^? folded . _SavePomodoro . pdLen) @?= Just expPlen
    pauseLen <- preuse $ sState . sBurn . _PauseCounting . cLen
    liftBase $ pauseLen @?= Just (mm 5)



main :: IO ()
main = defaultMain $ testGroup "Test cases"
  [ testCase "shortPomodoro" shortPomodoro ]
