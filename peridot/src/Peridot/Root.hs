module Peridot.Root where

import Data.Dependent.Sum
import Data.Functor.Classes
import Data.GADT.Compare
import Peridot.Grouping
import Peridot.Statistics

data Root (payload :: * -> *) val where
  Payload  :: payload val                   -> Root payload val
  Stats    :: Statistics (Root payload) val -> Root payload val
  Grouping :: Group (Root payload) val      -> Root payload val

instance (GEq payload) => GEq (Root payload) where
  geq a b = case a of
    Payload pa -> case b of
      Payload pb -> geq pa pb
      _ -> Nothing
    Stats sa -> case b of
      Stats sb -> geq sa sb
      _ -> Nothing
    Grouping ga -> case b of
      Grouping gb -> geq ga gb
      _ -> Nothing

instance (GCompare payload) => GCompare (Root payload) where
  gcompare a b = case a of
    Payload pa -> case b of
      Payload pb -> gcompare pa pb
      _ -> GGT
    Stats sa -> case b of
      Payload {} -> GLT
      Stats sb -> gcompare sa sb
      _ -> GGT
    Grouping ga -> case b of
      Grouping gb -> gcompare ga gb
      _ -> GLT

instance (GEq payload, EqTag payload f, Eq1 f) => EqTag (Root payload) f where
  eqTagged a b ia ib = case a of
    Payload pa -> case b of
      Payload pb -> eqTagged pa pb ia ib
      _ -> False
    Stats sa -> case b of
      Stats sb -> eqTagged sa sb ia ib
      _ -> False
    Grouping ga -> case b of
      Grouping gb -> eqTagged ga gb ia ib
      _ -> False

instance (OrdTag payload f, Ord1 f) => OrdTag (Root payload) f where
  compareTagged a b ia ib = case a of
    Payload pa -> case b of
      Payload pb -> compareTagged pa pb ia ib
      _          -> GT
    Stats sa -> case b of
      Payload {} -> LT
      Stats sb   -> compareTagged sa sb ia ib
      _          -> GT
    Grouping ga -> case b of
      Grouping gb -> compareTagged ga gb ia ib
      _           -> LT
