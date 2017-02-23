module Peridot.TimeSeries.Fold where


import Control.Foldl as FL
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
import Peridot.TimeSeries.Keys

ordFold :: (Ord a, DiffFrac a) => StatOrd -> FL.Fold a (Maybe a)
ordFold = \case
  StatMax -> FL.maximum
  StatMin -> FL.minimum
  StatMedian -> median


mkStatFoldl
  :: (GCompare rest)
  => Statistics rest val
  -> ErrFold (TSRecord rest) (TSRecord rest)
mkStatFoldl = \case
  StatisticsCount -> generalize $ fmap toRecord $ FL.length
    where
      toRecord i = DM.singleton (Stats StatisticsCount) $ Identity i
  key@(StatisticsOrd agg ts) ->
    lmapM fromRec $ generalize $ fmap toRec $ ordFold agg
    where
      fromRec r = case DM.lookup ts r of
        Nothing -> Left "key not found" -- FIXME: usefull error
        Just v  -> Right $ runIdentity v
      toRec = \case
        Just v  -> DM.singleton (Stats key) $ Identity v
        Nothing -> DM.empty

squashStatFoldl
  :: (GCompare rest)
  => Rec (Statistics rest) vals
  -> ErrFold (TSRecord rest) (TSRecord rest)
squashStatFoldl = \case
  stat :& rest -> DM.union <$> mkStatFoldl stat <*> squashStatFoldl rest
  RNil         -> pure DM.empty
