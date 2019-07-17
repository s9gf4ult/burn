module Burn.Statistics.Types where

import Burn.Types
import Data.Time
import Control.Foldl as FL
import Control.Lens hiding (Fold)
import Data.Foldable as F
import Data.GADT.Compare.TH
import Data.List as L
import Data.Set as S
import Data.Text as T
import Data.Time
import Data.Vector as V
import Statistics.Sample.Histogram

data Week = Week
  { _wYear :: Integer
  , _wWeek :: Int
  } deriving (Eq, Ord, Show)

data PKey val where
  PStart     :: PKey UTCTime
  PLen       :: PKey NominalDiffTime
  PLocalTime :: PKey LocalTime
  PDay       :: PKey Day
  -- ^ Considering real day end
  PWeek      :: PKey Week
  PWeekDay   :: PKey Int
  -- ^ 1..7 for Monday..Sunday

deriveGEq ''PKey
deriveGCompare ''PKey

-- data TimeRange = TimeRange
--   { _trStart :: UTCTime
--   , _trLen   :: NominalDiffTime
--   } deriving (Eq, Show)

-- makeLenses ''TimeRange

-- instance Ord TimeRange where
--   compare (TimeRange a b) (TimeRange a' b') = case compare a a' of
--     EQ -> compare b b'
--     x  -> x

-- -- | Precondition: income list is sorted by start time
-- invertTimeRanges :: [TimeRange] -> [TimeRange]
-- invertTimeRanges (a:b:rest) =
--   let
--     d = diffUTCTime (b ^. trStart) (a ^. trStart)
--     rlen = max 0 $ d - (a ^. trLen)
--     rstart = addUTCTime rlen $ a ^. trStart
--   in if
--     | rlen > 0  -> TimeRange rstart rlen : invertTimeRanges (b:rest)
--     | otherwise -> invertTimeRanges (b:rest)
-- invertTimeRanges _ = []

-- data LenStatistics = LenStatistics
--   { _sSum        :: !NominalDiffTime
--   , _sMinLen     :: !(Maybe NominalDiffTime)
--   , _sMaxLen     :: !(Maybe NominalDiffTime)
--   , _sCount      :: !Int
--   } deriving (Eq, Ord, Show)

-- makeLenses ''LenStatistics

-- data StatData
--   = SDSummary LenStatistics
--   deriving (Eq, Ord, Show)

-- makePrisms ''StatData

-- data StatsResult = StatsResult
--   { _srTime :: Day  -- FIXME: more variance
--   , _srStatData :: StatData
--   } deriving (Eq, Ord, Show)

-- makeLenses ''StatsResult
