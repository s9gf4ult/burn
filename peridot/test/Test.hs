module Main where

import Data.DList as DL
import Data.Dependent.Map as DM
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.GADT.Compare.TH
import Data.Maybe
import Data.Vinyl
import Peridot
import Test.Tasty
import Test.Tasty.HUnit

data TestPayload val where
  IntValue   :: TestPayload Int
  FracValue  :: TestPayload Rational

deriveGEq ''TestPayload
deriveGCompare ''TestPayload

instance EqTag TestPayload Identity where
  eqTagged ra rb ia ib = compareTagged ra rb ia ib == EQ

instance OrdTag TestPayload Identity where
  compareTagged ra rb ia ib = case (ra, rb) of
    (IntValue, IntValue) -> compare ia ib
    (IntValue, _)                    -> GT
    (FracValue, FracValue)           -> compare ia ib
    (FracValue, _)                   -> LT

testFracs :: Collection '[ 'L ] (Root TestPayload) Identity
testFracs = List $ DL.fromList
  $ fmap (DM.singleton (Payload FracValue) . Identity) [0,0.1..10]

simpleFold :: Assertion
simpleFold = do
  let
    keys =
      StatisticsCount
      :& StatisticsFrac (Payload FracValue) StatMean
      :& StatisticsFrac (Payload FracValue) StatMedian
      :& RNil
    Right (Rec res) = colFold (foldOverStats Stats keys) testFracs
    Just count = DM.lookup (Stats $ StatisticsCount) res
    Just mean = DM.lookup (Stats $ StatisticsFrac (Payload FracValue) StatMean) res
    Just median = DM.lookup (Stats $ StatisticsFrac (Payload FracValue) StatMedian) res
    variance = DM.lookup (Stats $ StatisticsFrac (Payload FracValue) StatVariance) res
  count @?= 101
  mean @?= 5
  median @?= 5
  variance @?= Nothing          -- because we did not ask it

histroGrouping :: Assertion
histroGrouping = do
  let
    gkey = GroupHistro (Payload FracValue) (Histro 0 1)
    Right histro = colGroup (group Grouping gkey) testFracs
    statKeys = StatisticsCount :& StatisticsFrac (Payload FracValue) StatMean :& RNil
    Right counts' = colFold (foldOverStats Stats statKeys) histro
    counts = colUngroup counts'
  case counts of
    List (DL.toList -> recs) -> do
      length recs @?= 11
      let
        colCounts = catMaybes $ (flip fmap) recs $ \r -> do
          col <- DM.lookup (Grouping gkey) r
          cnt <- DM.lookup (Stats StatisticsCount) r
          return (col, cnt)
      take 10 colCounts @?= zip [0..9] (cycle [10])

main :: IO ()
main = defaultMain $ testGroup "simple"
  [ testCase "statistics fold" simpleFold
  , testCase "histro grouping" histroGrouping
  ]
