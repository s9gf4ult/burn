module Peridot.Statistics.Key where

import Data.GADT.Compare
import Peridot.Auxiliary

data StatOrd
  = StatMax
  | StatMin
  deriving (Eq, Ord, Show)

data StatFrac
  = StatMedian
  | StatSum
  | StatMean
  | StatVariance
  deriving (Eq, Ord, Show)

data Statistics (root :: * -> *) val where
  StatisticsOrd
    :: (Ord val)
    => root val
    -> StatOrd
    -> Statistics root val
  StatisticsFrac
    :: (Ord val, Fractional val)
    => root val
    -> StatFrac
    -> Statistics root val
  StatisticsCount
    :: Statistics rest Int

instance GEq root => GEq (Statistics root) where
  geq a b = case a of
    StatisticsOrd ra sa -> case b of
      StatisticsOrd rb sb -> case geq ra rb of
        Just Refl | sa == sb -> Just Refl
        _ -> Nothing
      _ -> Nothing
    StatisticsFrac ra sa -> case b of
      StatisticsFrac rb sb -> case geq ra rb of
        Just Refl | sa == sb -> Just Refl
        _ -> Nothing
      _ -> Nothing
    StatisticsCount -> case b of
      StatisticsCount -> Just Refl
      _ -> Nothing

instance GCompare root => GCompare (Statistics root) where
  gcompare a b = case a of
    StatisticsOrd ra sa -> case b of
      StatisticsOrd rb sb -> case gcompare ra rb of
        GEQ -> hardenOrdering $ compare sa sb
        GLT -> GLT
        GGT -> GGT
      _ -> GGT
    StatisticsFrac ra sa -> case b of
      StatisticsOrd {} -> GLT
      StatisticsFrac rb sb -> case gcompare ra rb of
        GEQ -> hardenOrdering $ compare sa sb
        GLT -> GLT
        GGT -> GGT
      _ -> GGT
    StatisticsCount -> case b of
      StatisticsCount -> GEQ
      _               -> GLT

type WrapStats root = forall val. Statistics root val -> root val
