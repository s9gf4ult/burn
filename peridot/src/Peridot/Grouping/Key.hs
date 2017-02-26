module Peridot.Grouping.Key where

import Data.Dependent.Sum
import Data.Functor.Classes
import Data.GADT.Compare
import Peridot.Auxiliary
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
  GroupHistro :: (DiffRealFrac val, Ord val) => root val -> Histro val -> Group root Integer
  -- ^ Histrogram grouping by some diffable value. Result is index of
  -- column 'val' belongs to.

instance (GEq root) => GEq (Group root) where
  geq a b = case a of
    GroupAbs ra -> case b of
      GroupAbs rb -> geq ra rb
      _ -> Nothing
    GroupHistro ra ha -> case b of
      GroupHistro rb hb -> case geq ra rb of
        Just Refl | ha == hb  -> Just Refl
        _ -> Nothing
      _ -> Nothing

instance (GCompare root) => GCompare (Group root) where
  gcompare a b = case a of
    GroupAbs ra -> case b of
      GroupAbs rb -> gcompare ra rb
      _ -> GGT
    GroupHistro ra ha -> case b of
      GroupHistro rb hb -> case gcompare ra rb of
        GEQ -> hardenOrdering $ compare ha hb
        GLT -> GLT
        GGT -> GGT
      _ -> GLT

instance (GEq root, Eq1 f) => EqTag (Group root) f where
  eqTagged ga gb ia ib = case (ga, gb) of
    (GroupAbs ra, GroupAbs rb) -> case geq ra rb of
      Just Refl -> eq1 ia ib
      Nothing -> False
    (GroupAbs {}, _) -> False
    (GroupHistro ra ha, GroupHistro rb hb) -> case geq ra rb of
      Just Refl -> ha == hb && eq1 ia ib
      Nothing   -> False
    (GroupHistro {}, _) -> False

instance (GCompare root, Ord1 f) => OrdTag (Group root) f where
  compareTagged ga gb ia ib = case (ga, gb) of
    (GroupAbs ra, GroupAbs rb) -> case gcompare ra rb of
      GEQ -> compare1 ia ib
      x -> weakenOrdering x
    (GroupAbs {}, _) -> GT
    (GroupHistro ra ha, GroupHistro rb hb) -> case gcompare ra rb of
      GEQ -> case compare ha hb of
        EQ -> compare1 ia ib
        x -> x
      x -> weakenOrdering x
    (GroupHistro {}, _) -> LT

type WrapGroup root = forall val. Group root val -> root val
