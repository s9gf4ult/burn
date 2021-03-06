module Peridot.Auxiliary where

import Control.Foldl as FL
import Data.GADT.Compare
import Data.List as L
import Data.Vector as V
import Peridot.Diffable

hardenOrdering :: Ordering -> GOrdering a a
hardenOrdering = \case
  GT -> GGT
  LT -> GLT
  EQ -> GEQ


median :: (Ord a, Fractional a) => FL.Fold a (Maybe a)
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
              0 -> ((V.unsafeIndex v (pred d)) + (V.unsafeIndex v d)) / 2
              1 -> V.unsafeIndex v d
              _ -> error "impossible reminder"
        in Just res


lmapM :: (Monad m) => (a -> m b) -> FL.FoldM m b r -> FL.FoldM m a r
lmapM amb (FoldM step start end) = FoldM step' start end
  where
    step' x a = do
      b <- amb a
      step x b
