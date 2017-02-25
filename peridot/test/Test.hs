module Main where

import Control.Monad
import Data.DList as DL
import Data.Dependent.Map as DM
import Data.Fixed
import Data.Functor.Identity
import Data.GADT.Compare.TH
import Data.Vinyl
import Peridot
-- import Test.HUnit.Base
import Test.Tasty
import Test.Tasty.HUnit

data TestRoot val where
  IntValue   :: TestRoot Int
  FracValue  :: TestRoot Rational
  StatValue  :: Statistics TestRoot val -> TestRoot val
  GroupValue :: Group TestRoot val -> TestRoot val

deriveGEq ''TestRoot
deriveGCompare ''TestRoot

testFracs :: Collection '[ 'L ] TestRoot Identity
testFracs = List $ DL.fromList
  $ fmap (DM.singleton FracValue . Identity) [0,0.1..10]

simpleFold :: TestTree
simpleFold = testCase "fold sum" $ do
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
  print $ "Median: " ++ show median

main :: IO ()
main = defaultMain $ testGroup "simple"
  [ simpleFold ]
