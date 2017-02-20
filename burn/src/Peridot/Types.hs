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

class ColFold (s :: [S a]) k f where
  type FoldShape (s :: [S key]) :: [S key]
  colFold
    :: RecFolder k f
    -> Collection s k f
    -> Collection (FoldShape s) k f

instance ColFold '[ 'L ] k f where
  type FoldShape '[ 'L ] = '[]
  colFold f = \case
    List dl -> Rec $ f dl

instance (ColFold rest k f) => ColFold ('G a ': rest) k f where
  type FoldShape ('G a ': rest) = 'G a ': (FoldShape rest)
  colFold f = \case
    Grouped m -> Grouped $ fmap (colFold f) m

type RecGrouper a k f = DList (Record k f) -> Map (f a) (DList (Record k f))

class ColGroup (s :: [S key]) k f where
  type GroupShape (a :: key) (s :: [S key]) :: [S key]
  colGroup
    :: RecGrouper a k f
    -> Collection s k f
    -> Collection (GroupShape a s) k f

instance ColGroup '[ 'L ] k f where
  type GroupShape a '[ 'L ] = '[ 'G a, 'L ]
  colGroup f = \case
    List dl -> Grouped $ fmap List $ f dl

instance (ColGroup rest k f) => ColGroup ('G a ': rest) k f where
  type GroupShape key ('G a ': rest) = ('G a) ': (GroupShape key rest)
  colGroup f = \case
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
