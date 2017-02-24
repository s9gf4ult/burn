module Peridot.Auxiliary where

import Control.Foldl as FL
import Data.List as L
import Data.Vector as V
import Peridot.Diffable

-- medium :: (Fractional a) => a -> a -> a
-- medium a b = case compare a b of
--   EQ -> a
--   GT -> addDiff (diff a b / 2) b
--   LT -> addDiff (diff b a / 2) a

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
