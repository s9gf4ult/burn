module Peridot.Statistics.Fold where

import Control.Foldl as FL
import Data.DList
import Data.Dependent.Map as DM
import Data.Functor.Identity
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.List as L
import Data.Profunctor
import Data.Time
import Data.Vector as V
import Data.Vinyl
import Peridot.Auxiliary
import Peridot.Core
import Peridot.Diffable
import Peridot.Statistics.Key

ordFold :: (Ord a) => StatOrd -> FL.Fold a (Maybe a)
ordFold = \case
  StatMax -> FL.maximum
  StatMin -> FL.minimum

fracFold :: (Ord a, Fractional a) => StatFrac -> FL.Fold a (Maybe a)
fracFold = \case
  StatMedian   -> median
  StatSum      -> Just <$> FL.sum
  StatMean     -> Just <$> FL.mean
  StatVariance -> Just <$> FL.variance

mkStatFoldl
  :: (GCompare root)
  => WrapStats root
  -> Statistics root val
  -> ErrFold (SimpleRec root) (SimpleRec root)
mkStatFoldl wrap key = case key of
  StatisticsCount -> generalize $ fmap toRecord $ FL.length
    where
      toRecord i = DM.singleton (wrap StatisticsCount) $ Identity i
  StatisticsFrac agg ts -> go ts $ fracFold agg
  StatisticsOrd agg ts  -> go ts $ ordFold agg
  where
    go ts f =
      let
        fromRec r = case DM.lookup ts r of
          Nothing -> Left "key not found" -- FIXME: usefull error
          Just v  -> Right $ runIdentity v
        toRec = \case
          Just v  -> DM.singleton (wrap key) $ Identity v
          Nothing -> DM.empty
      in lmapM fromRec $ generalize $ fmap toRec f

squashStatFoldl
  :: (GCompare root)
  => WrapStats root
  -> Rec (Statistics root) vals
  -> ErrFold (SimpleRec root) (SimpleRec root)
squashStatFoldl wrap = \case
  stat :& rest -> DM.union <$> mkStatFoldl wrap stat <*> squashStatFoldl wrap rest
  RNil         -> pure DM.empty

foldOverStats
  :: (GCompare root)
  => WrapStats root
  -> Rec (Statistics root) vals
  -> DList (SimpleRec root)
  -> Either String (SimpleRec root)
foldOverStats wrap stats dlist = FL.foldM (squashStatFoldl wrap stats) dlist
