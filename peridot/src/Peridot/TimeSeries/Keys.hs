module Peridot.TimeSeries.Keys where


import Control.Foldl as FL
import Data.Dependent.Map as DM
import Data.Functor.Identity
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.List as L
import Data.Profunctor
import Data.Time
import Data.Vector as V
import Data.Vinyl
import Peridot.Core
import Peridot.Diffable


-- -- | The main key for all time series keys. Extendable with custom
-- -- type.
-- data TimeSeries (rest :: * -> *) (a :: *) where
--   Len       :: TimeSeries rest NominalDiffTime
--   Start     :: TimeSeries rest UTCTime
--   Stats     :: Statistics rest value -> TimeSeries rest value
--   Groupping :: Group rest value -> TimeSeries rest value
--   Rest      :: rest val -> TimeSeries rest val

-- instance (GEq rest) => GEq (TimeSeries rest) -- FIXME: implement
-- instance (GCompare rest) => GCompare (TimeSeries rest) -- FIXME: implement

-- type TSRecord rest = Record (TimeSeries rest) Identity
-- type TSRecFolder rest = RecFolder (TimeSeries rest) Identity
-- type TSGrouper rest = RecGrouper (TimeSeries rest) Identity
