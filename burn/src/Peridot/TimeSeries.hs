module Peridot.TimeSeries where

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
import Peridot.Types

data StatOrd
  = StatMax
  | StatMin
  | StatMedian

data Statistics rest val where
  StatisticsOrd
    :: (Ord val, Medium val)
    => StatOrd
    -> TimeSeries rest val
    -> Statistics rest val
  StatisticsCount
    :: Statistics rest Int

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

-- | Class of types the medium can be calculated for
class Medium a where
  medium :: a -> a -> a

instance {-# OVERLAPPING #-} Medium UTCTime where
  medium a b
    | a >= b = addUTCTime (diffUTCTime a b / 2) b

-- | Default instance for all numbers
instance {-# OVERLAPPABLE #-} (Num a, Fractional a)
  => Medium a where
  medium a b = (a + b) / 2

median :: (Ord a, Medium a) => FL.Fold a (Maybe a)
median = go <$> FL.revList
  where
    go = \case
      [] -> Nothing
      x ->
        let
          v = V.fromList $ L.sort x
          vl = V.length v
          res =
            let (d, m) = vl `divMod` 2
            in case m of
              0 -> medium (V.unsafeIndex v (pred d)) (V.unsafeIndex v d)
              1 -> V.unsafeIndex v d
              _ -> error "impossible reminder"
        in Just res

ordFold :: (Ord a, Medium a) => StatOrd -> FL.Fold a (Maybe a)
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
