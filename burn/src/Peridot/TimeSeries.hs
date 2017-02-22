module Peridot.TimeSeries where

import Control.Foldl as FL
import Data.Dependent.Map as DM
import Data.Functor.Identity
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.Time
import Peridot.Types

data StatAgg
  = StatSum
  | StatCount
  | StatMax
  | StatMin

data Statistics rest val where
  StatisticsAgg   :: StatAgg -> TimeSeries rest val -> Statistics rest val
  StatisticsCount :: Statistics rest Int

data Group rest val where
  GroupAbs :: TimeSeries rest val -> Group rest val
  -- ^ Grouping by absolute value. Each group in result will have
  -- records having same value by specified key


-- | The main key for all time series keys. Extendable with custom
-- type.
data TimeSeries (rest :: * -> *) (a :: *) where
  Len :: TimeSeries rest NominalDiffTime
  Start :: TimeSeries rest UTCTime
  Stats :: Statistics rest value -> TimeSeries rest value
  Groupping :: Group rest value -> TimeSeries rest value
  Rest :: rest val -> TimeSeries rest val

instance (GEq rest) => GEq (TimeSeries rest) -- FIXME: implement
instance (GCompare rest) => GCompare (TimeSeries rest) -- FIXME: implement


type TSRecord rest = Record (TimeSeries rest) Identity
type TSRecFolder rest = RecFolder (TimeSeries rest) Identity
type TSGrouper rest = RecGrouper (TimeSeries rest) Identity


aggFold :: StatAgg -> FL.Fold a a
aggFold = (error "FIXME: ")

mkStatFoldl
  :: Statistics rest val
  -> Either String (FL.Fold (TSRecord rest) (TSRecord rest))
mkStatFoldl = \case
  StatisticsCount -> Right $ fmap toRecord $ FL.length
    where
      toRecord i = DM.singleton (Stats StatisticsCount) $ Identity i
  key@(StatisticsAgg agg ts) -> traverse aggRecord $ aggFold agg
    where
      aggRecord r = case DM.lookup ts r of
        Nothing -> Left "Not found key" -- FIXME: make errors usable
        Just v -> Right $ DM.singleton (Stats key) $ Identity v


  -- SCount -> (DM.singleton (TSStatisticsKey SCount) . TSStatisticsValue . CountValue) <$> FL.length
  -- -- FIXME: implement the rest of constructors

-- squashStatFoldl :: Sing (stat :: [Statistics]) -> FL.Fold TSRecord TSRecord
-- squashStatFoldl = \case
--   SCons stat rest -> DM.union <$> mkStatFoldl stat <*> squashStatFoldl rest
--   SNil            -> pure $ DM.empty

-- tsRecordFolder :: Sing (stat :: [Statistics]) -> TSRecFolder
-- tsRecordFolder stat = FL.fold (squashStatFoldl stat)
