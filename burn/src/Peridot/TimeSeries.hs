module Peridot.TimeSeries where

import Control.Foldl as FL
import Data.Dependent.Map as DM
import Data.Functor.Identity
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.Profunctor
import Data.Time
import Data.Vinyl
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

-- | Foldl which can throw error
type ErrFold a b = FL.FoldM (Either String) a b

lmapM :: (Monad m) => (a -> m b) -> FL.FoldM m b r -> FL.FoldM m a r
lmapM amb (FoldM step start end) = FoldM step' start end
  where
    step' x a = do
      b <- amb a
      step x b

aggFold :: (Num a) => StatAgg -> FL.Fold a a
aggFold = \case
  StatSum -> FL.sum

mkStatFoldl
  :: (GCompare rest, Num val)
  => Statistics rest val
  -> ErrFold (TSRecord rest) (TSRecord rest)
mkStatFoldl = \case
  StatisticsCount -> generalize $ fmap toRecord $ FL.length
    where
      toRecord i = DM.singleton (Stats StatisticsCount) $ Identity i
  key@(StatisticsAgg agg ts) ->
    lmapM fromRec $ generalize $ fmap toRec $ aggFold agg
    where
      fromRec r = case DM.lookup ts r of
        Nothing -> Left "key not found" -- FIXME: usefull error
        Just v  -> Right $ runIdentity v
      toRec v = DM.singleton (Stats key) $ Identity v

squashStatFoldl
  :: (GCompare rest)
  => Rec (Statistics rest) vals
  -> ErrFold (TSRecord rest) (TSRecord rest)
squashStatFoldl = \case
  stat :& rest -> DM.union <$> mkStatFoldl stat <*> squashStatFoldl rest
  RNil         -> pure DM.empty
