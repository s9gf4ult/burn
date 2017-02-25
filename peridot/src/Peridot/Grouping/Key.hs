module Peridot.Grouping.Key where

import Peridot.Diffable

data Histro a = Histro
  { _histroAnchor :: a
    -- ^ Where should be start of one of columns
  , _histroWidth  :: Diff a
    -- ^ Width of columns
  }

deriving instance (Eq a, Eq (Diff a)) => Eq (Histro a)
deriving instance (Ord a, Ord (Diff a)) => Ord (Histro a)
deriving instance (Show a, Show (Diff a)) => Show (Histro a)

data Group (root :: * -> *) val where
  GroupAbs :: (Ord val) => root val -> Group root val
  -- ^ Grouping by absolute value. Each group in result will have
  -- records having same value by specified key
  GroupHistro :: (DiffRealFrac val) => root val -> Histro val -> Group root Integer
  -- ^ Histrogram grouping by some diffable value. Result is index of
  -- column 'val' belongs to.

type WrapGroup root = forall val. Group root val -> root val
