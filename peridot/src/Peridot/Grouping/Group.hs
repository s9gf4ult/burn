module Peridot.Grouping.Group where

import Data.DList as DL
import Data.Dependent.Map as DM
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Map.Strict as M
import Data.Monoid
import Peridot.Core

absGroup :: (OrdTag root Identity) => root val -> RecGrouper ErrMonad root Identity
absGroup key dlist = fmap (M.fromListWith (<>)) $ traverse go $ DL.toList dlist
  where
    go r = case DM.lookup key r of
      Nothing  -> Left "not found key" -- FIXME: more usefull error
      Just val -> Right (key :=> val, pure r)
