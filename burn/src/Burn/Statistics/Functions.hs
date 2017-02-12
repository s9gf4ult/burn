module Burn.Statistics.Functions where

import Burn.Statistics.Types
import Burn.Types
import Control.Foldl as FL
import Control.Lens hiding (Fold)
import Data.Foldable as F
import Data.List as L
import Data.Set as S
import Data.Text as T
import Data.Time
import Data.Vector as V
import Statistics.Sample.Histogram

calculateStats
  :: (Foldable f)
  => Int
     -- ^ Histrogram ranges
  -> f NominalDiffTime
  -> LenStatistics
calculateStats ranges = FL.fold go
  where
    go = LenStatistics
      <$> FL.sum
      <*> FL.minimum
      <*> FL.maximum
      <*> median
      <*> FL.length

median :: (Ord a, Num a, Fractional a) => Fold a (Maybe a)
median = Fold (flip (:)) [] go
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
              0 -> (V.unsafeIndex v (pred d) + V.unsafeIndex v d) / 2
              1 -> V.unsafeIndex v d
              _ -> error "impossible module"
        in Just res

histroFold :: Int -> Fold Double (Maybe (V.Vector Int))
histroFold ranges = Fold (flip (:)) [] go
  where
    go :: [Double] -> Maybe (V.Vector Int)
    go [] = Nothing
    go x =
      let
        lo = F.minimum x
        hi = F.maximum x
        h = histogram_ ranges lo hi $ V.fromList x
      in Just h
