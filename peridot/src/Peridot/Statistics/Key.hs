module Peridot.Statistics.Key where

import Data.Dependent.Sum
import Data.Functor.Classes
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

instance (GEq root, Eq1 f) => EqTag (Statistics root) f where
  eqTagged sa sb ia ib = case sa of
    StatisticsOrd ra orda -> case sb of
      StatisticsOrd rb ordb -> case geq ra rb of
        Just Refl -> orda == ordb && eq1 ia ib
        Nothing -> False
      _ -> False
    StatisticsFrac ra fa -> case sb of
      StatisticsFrac rb fb -> case geq ra rb of
        Just Refl -> fa == fb && eq1 ia ib
        Nothing -> False
      _ -> False
    StatisticsCount -> case sb of
      StatisticsCount -> eq1 ia ib
      _ -> False

instance (GCompare root, Ord1 f) => OrdTag (Statistics root) f where
  compareTagged sa sb ia ib = case sa of
    StatisticsOrd ra orda -> case sb of
      StatisticsOrd rb ordb -> case gcompare ra rb of
        GEQ -> case compare orda ordb of
          EQ -> compare1 ia ib
          x -> x
        x -> weakenOrdering x
      _ -> GT
    StatisticsFrac ra fa -> case sb of
      StatisticsOrd {} -> LT
      StatisticsFrac rb fb -> case gcompare ra rb of
        GEQ -> case compare fa fb of
          EQ -> compare1 ia ib
          x -> x
        x -> weakenOrdering x
      _ -> GT
    StatisticsCount -> case sb of
      StatisticsCount -> compare1 ia ib
      _ -> LT


type WrapStats root = forall val. Statistics root val -> root val
