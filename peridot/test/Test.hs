module Main where

import Control.Monad
import Data.DList as DL
import Data.Dependent.Map as DM
import Data.Dependent.Sum
import Data.Fixed
import Data.Functor.Identity
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.Map.Strict as M
import Data.Maybe
import Data.Vinyl
import Peridot
import Test.Tasty
import Test.Tasty.HUnit

data TestRoot val where
  IntValue   :: TestRoot Int
  FracValue  :: TestRoot Rational
  StatValue  :: Statistics TestRoot val -> TestRoot val
  GroupValue :: Group TestRoot val -> TestRoot val

deriveGEq ''TestRoot
deriveGCompare ''TestRoot

instance EqTag TestRoot Identity where
  eqTagged ra rb ia ib = compareTagged ra rb ia ib == EQ

instance OrdTag TestRoot Identity where
  compareTagged ra rb ia ib = case (ra, rb) of
    (IntValue, IntValue)   -> compare ia ib
    (IntValue, _) -> GT

    (FracValue, FracValue) -> compare ia ib
    (FracValue, _) -> GT        -- IntValue is not possible here

    (StatValue _, IntValue) -> LT
    (StatValue _, FracValue) -> LT
    (StatValue sa, StatValue sb) -> compareTagged sa sb ia ib
    (StatValue _, _) -> GT

    (GroupValue ga, GroupValue gb) -> compareTagged ga gb ia ib
    (GroupValue _, _) -> LT


testFracs :: Collection '[ 'L ] TestRoot Identity
testFracs = List $ DL.fromList
  $ fmap (DM.singleton FracValue . Identity) [0,0.1..10]

simpleFold :: Assertion
simpleFold = do
  let
    keys =
      StatisticsCount
      :& StatisticsFrac FracValue StatMean
      :& StatisticsFrac FracValue StatMedian
      :& RNil
    Right (Rec res) = colFold (foldOverStats StatValue keys) testFracs
    Just count = DM.lookup (StatValue $ StatisticsCount) res
    Just mean = DM.lookup (StatValue $ StatisticsFrac FracValue StatMean) res
    Just median = DM.lookup (StatValue $ StatisticsFrac FracValue StatMedian) res
    variance = DM.lookup (StatValue $ StatisticsFrac FracValue StatVariance) res
  count @?= 101
  mean @?= 5
  median @?= 5
  variance @?= Nothing          -- because we did not ask it

histroGrouping :: Assertion
histroGrouping = do
  let
    gkey = GroupHistro FracValue (Histro 0 1)
    Right histro = colGroup (group GroupValue gkey) testFracs
    statKeys = StatisticsCount :& StatisticsFrac FracValue StatMean :& RNil
    Right counts' = colFold (foldOverStats StatValue statKeys) histro
    counts = colUngroup counts'
  case counts of
    List (DL.toList -> recs) -> do
      length recs @?= 11
      let
        colCounts = catMaybes $ (flip fmap) recs $ \r -> do
          col <- DM.lookup (GroupValue gkey) r
          cnt <- DM.lookup (StatValue StatisticsCount) r
          return (col, cnt)
      take 10 colCounts @?= zip [0..9] (cycle [10])

main :: IO ()
main = defaultMain $ testGroup "simple"
  [ testCase "statistics fold" simpleFold
  , testCase "histro grouping" histroGrouping
  ]
