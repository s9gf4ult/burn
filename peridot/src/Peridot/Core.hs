module Peridot.Core where

import Control.Foldl as FL
import Data.DList as DL
import Data.Dependent.Map as DM
import Data.Functor.Identity
import Data.Map as M

-- | The record is just map of some arbitrary values, all values are
-- optional. In most cases 'k' is GADT with posible keys and 'f' is
-- just Identity, but more complex schema may be used.
type Record k f = DMap k f

-- | Shape tag for collection. May be either list or group (which is
-- map)
data S = L | G

data Collection (s :: [S]) (k :: key -> *) (f :: key -> *) where
  Rec     :: Record k f                   -> Collection '[] k f
  List    :: DList (Record k f)           -> Collection '[ 'L ] k f
  Grouped :: Map (DSum k f) (Collection s k f) -> Collection ('G ': s) k f

type RecFolder m k f = DList (Record k f) -> m (Record k f)

class ColFold (m :: * -> *) (s :: [S]) k f where
  type FoldShape (s :: [S]) :: [S]
  colFold
    :: RecFolder m k f
    -> Collection s k f
    -> m (Collection (FoldShape s) k f)

instance (Functor m) => ColFold m '[ 'L ] k f where
  type FoldShape '[ 'L ] = '[]
  colFold f = \case
    List dl -> Rec <$> f dl

instance (ColFold m rest k f, Applicative m) => ColFold m ('G ': rest) k f where
  type FoldShape ('G ': rest) = 'G ': (FoldShape rest)
  colFold f = \case
    Grouped m -> Grouped <$> traverse (colFold f) m

type RecGrouper m k f = DList (Record k f) -> m (Map (DSum k f) (DList (Record k f)))

class ColGroup (m :: * -> *) (s :: [S]) k f where
  type GroupShape (s :: [S]) :: [S]
  colGroup
    :: RecGrouper m k f
    -> Collection s k f
    -> m (Collection (GroupShape s) k f)

instance (Functor m) => ColGroup m '[ 'L ] k f where
  type GroupShape '[ 'L ] = '[ 'G, 'L ]
  colGroup f = \case
    List dl -> (Grouped . fmap List) <$> f dl

instance (ColGroup m rest k f, Applicative m) => ColGroup m ('G ': rest) k f where
  type GroupShape ('G ': rest) = 'G ': (GroupShape rest)
  colGroup f = \case
    Grouped m -> Grouped <$> traverse (colGroup f) m

class ColUngroup (s :: [S]) k f where
  type UngroupShape s :: [S]
  colUngroup :: Collection s k f -> Collection (UngroupShape s) k f

instance (GCompare k) => ColUngroup '[ 'G ] k f where
  type UngroupShape '[ 'G ] = '[ 'L ]
  colUngroup = \case
    Grouped m -> List $ DL.fromList $ fmap merge $ M.toList m
      where
        merge :: (DSum k f, Collection '[] k f) -> Record k f
        merge (ka :=> fa, Rec dm) = DM.insert ka fa dm

instance (ColUngroup (b ': s) k f) => ColUngroup ('G ': b ': s) k f where
  type UngroupShape ('G ': b ': s) = 'G ': (UngroupShape (b ': s))
  colUngroup = \case
    Grouped m -> Grouped $ fmap colUngroup m

-- TODO: make errors more usable
type ErrMonad = Either String
-- | Foldl which can throw error
type ErrFold a b = FL.FoldM ErrMonad a b


type SimpleRec f = Record f Identity
