module Peridot.Statistics.Key where

import Data.Dependent.Sum
import Data.Functor.Identity
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

instance (GEq root) => EqTag (Statistics root) Identity where
  eqTagged sa sb ia ib = case sa of
    StatisticsOrd ra orda -> case sb of
      StatisticsOrd rb ordb -> case geq ra rb of
        Just Refl -> orda == ordb && ia == ib
        Nothing -> False
      _ -> False
    StatisticsFrac ra fa -> case sb of
      StatisticsFrac rb fb -> case geq ra rb of
        Just Refl -> fa == fb && ia == ib
        Nothing -> False
      _ -> False
    StatisticsCount -> case sb of
      StatisticsCount -> ia == ib
      _ -> False

instance (GCompare root) => OrdTag (Statistics root) Identity where
  compareTagged sa sb ia ib = case sa of
    StatisticsOrd ra orda -> case sb of
      StatisticsOrd rb ordb -> case gcompare ra rb of
        GEQ -> case compare orda ordb of
          EQ -> compare ia ib
          x -> x
        x -> weakenOrdering x
      _ -> GT
    StatisticsFrac ra fa -> case sb of
      StatisticsOrd {} -> LT
      StatisticsFrac rb fb -> case gcompare ra rb of
        GEQ -> case compare fa fb of
          EQ -> compare ia ib
          x -> x
        x -> weakenOrdering x
      _ -> GT
    StatisticsCount -> case sb of
      StatisticsCount -> compare ia ib
      _ -> LT


type WrapStats root = forall val. Statistics root val -> root val
