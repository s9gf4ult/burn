module Peridot.Statistics.Key where

import Data.GADT.Compare

data StatOrd
  = StatMax
  | StatMin

data StatFrac
  = StatMedian
  | StatSum
  | StatMean
  | StatVariance

data Statistics (root :: * -> *) val where
  StatisticsOrd
    :: (Ord val)
    => StatOrd
    -> root val
    -> Statistics root val
  StatisticsFrac
    :: (Ord val, Fractional val)
    => StatFrac
    -> root val
    -> Statistics root val
  StatisticsCount
    :: Statistics rest Int

instance GEq root => GEq (Statistics root)
instance GCompare root => GCompare (Statistics root)

type WrapStats root = forall val. Statistics root val -> root val
