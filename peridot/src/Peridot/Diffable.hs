module Peridot.Diffable where

import Data.Fixed
import Data.Ratio
import Data.Time

class Diffable a where
  type Diff a :: *
  type Diff a = a

  diff :: a -> a -> Diff a
  default diff :: (Num a, Diff a ~ a) => a -> a -> Diff a
  diff = (-)

  addDiff :: Diff a -> a -> a
  default addDiff :: (Num a, Diff a ~ a) => Diff a -> a -> a
  addDiff = (+)

instance Diffable Integer
instance Diffable Int
instance Diffable Float
instance Diffable Double
instance (HasResolution a) => Diffable (Fixed a)
instance (Integral a) => Diffable (Ratio a)

instance Diffable UTCTime where
  type Diff UTCTime = NominalDiffTime
  diff = diffUTCTime
  addDiff = addUTCTime

instance Diffable TimeOfDay where
  type Diff TimeOfDay = DiffTime
  diff a b = timeOfDayToTime a - timeOfDayToTime b
  addDiff d a = timeToTimeOfDay $ d + timeOfDayToTime a

class ( Diffable a, RealFrac (Diff a)) =>  DiffRealFrac a where
instance ( Diffable a, RealFrac (Diff a)) =>  DiffRealFrac a where
