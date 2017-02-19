module Peridot.Types where

import Data.DList as DL
import Data.Dependent.Map as DM
import Data.Map as M

-- | The record is just map of some arbitrary values, all values are
-- optional. In most cases 'k' is GADT with posible keys and 'f' is
-- just Identity, but more complex schema may be used.
type Record k f = DMap k f

-- | Shape tag for collection. May be either list or group (which is
-- map)
data S k = L | G k

data Collection (s :: [S key]) (k :: key -> *) (f :: key -> *) where
  Rec     :: Record k f                   -> Collection '[] k f
  List    :: DList (Record k f)           -> Collection '[ 'L ] k f
  Grouped :: Map (f a) (Collection s k f) -> Collection ('G a ': s) k f

type RecFolder k f = DList (Record k f) -> Record k f

type family FoldShape (s :: [S a]) :: [S a] where
  FoldShape '[ 'L ] = '[]
  FoldShape ('G a ': s) = ('G a) ': (FoldShape s)

colFold :: RecFolder k f -> Collection s k f -> Collection (FoldShape s) k f
colFold f = \case
  Rec _     -> error "FIXME: make colFold typesafe"
  List dl   -> Rec $ f dl
  Grouped m -> Grouped $ fmap (colFold f) m

type RecGrouper a k f = DList (Record k f) -> Map (f a) (Collection '[ 'L ] k f)

type family GroupShape (a :: k) (s :: [S k]) :: [S k] where
  GroupShape a '[ 'L ] = '[ 'G a, 'L ]
  GroupShape a ( 'G b ': s) = ('G b) ': (GroupShape a s)

colGroup :: RecGrouper a k f -> Collection s k f -> Collection (GroupShape a s) k f
colGroup f = \case
  Rec _     -> error "FIXME: make colGroup typesafe"
  List dl   -> Grouped $ f dl
  Grouped m -> Grouped $ fmap (colGroup f) m

class ColUngroup (s :: [S a]) k f where
  type UngroupKey s :: a
  type UngroupShape s :: [S a]
  colUngroup
    :: k (UngroupKey s)
    -> Collection s k f
    -> Collection (UngroupShape s) k f

instance (GCompare k) => ColUngroup '[ 'G a ] k f where
  type UngroupKey '[ 'G a ] = a
  type UngroupShape '[ 'G a ] = '[ 'L ]
  colUngroup key = \case
    Grouped m -> List $ DL.fromList $ fmap merge $ M.toList m
      where
        merge :: (f a, Collection '[] k f) -> Record k f
        merge (fa, Rec dm) = DM.insert key fa dm

instance (ColUngroup (b ': s) k f) => ColUngroup ('G a ': b ': s) k f where
  type UngroupKey ('G a ': b ': s) = UngroupKey (b ': s)
  type UngroupShape ('G a ': b ': s) = ('G a) ': (UngroupShape (b ': s))
  colUngroup key = \case
    Grouped m -> Grouped $ fmap (colUngroup key) m
