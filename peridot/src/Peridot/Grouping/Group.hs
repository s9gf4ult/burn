module Peridot.Grouping.Group where

import Data.DList as DL
import Data.Dependent.Map as DM
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Map.Strict as M
import Data.Monoid
import Peridot.Core
import Peridot.Diffable
import Peridot.Grouping.Key

absGroup
  :: (OrdTag root Identity, Ord val)
  => WrapGroup root
  -> root val
  -> RecGrouper ErrMonad root Identity
absGroup wrap key dlist =
  fmap (M.fromListWith (<>)) $ traverse go $ DL.toList dlist
  where
    go r = case DM.lookup key r of
      Nothing  -> Left "not found key" -- FIXME: more usefull error
      Just val -> Right (wrap (GroupAbs key) :=> val, pure r)

histroGroup
  :: (OrdTag root Identity, DiffRealFrac val, Ord val)
  => WrapGroup root
  -> root val
  -> Histro val
  -> RecGrouper ErrMonad root Identity
histroGroup wrap key histro@(Histro anchor width) dlist =
  fmap (M.fromListWith (<>)) $ traverse go $ DL.toList dlist
  where
    go r = case DM.lookup key r of
      Nothing -> Left "not found key" -- FIXME: more usefull error
      Just (Identity val) ->
        let col = floor $ diff val anchor / width
        in Right (wrap (GroupHistro key histro) :=> Identity col, pure r)

group
  :: (OrdTag root Identity)
  => WrapGroup root
  -> Group root val
  -> RecGrouper ErrMonad root Identity
group wrap = \case
  GroupAbs key           -> absGroup wrap key
  GroupHistro key histro -> histroGroup wrap key histro
