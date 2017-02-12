module Burn.Statistics.Types where

import Burn.Types
import Control.Foldl as FL
import Control.Lens hiding (Fold)
import Data.Foldable as F
import Data.List as L
import Data.Set as S
import Data.Text as T
import Data.Time
import Data.Vector as V
import Statistics.Sample.Histogram

data TimeRange = TimeRange
  { _trStart :: UTCTime
  , _trLen   :: NominalDiffTime
  } deriving (Eq, Show)

makeLenses ''TimeRange

instance Ord TimeRange where
  compare (TimeRange a b) (TimeRange a' b') = case compare a a' of
    EQ -> compare b b'
    x  -> x


-- | Precondition: income list is sorted by start time
invertTimeRanges :: [TimeRange] -> [TimeRange]
invertTimeRanges (a:b:rest) =
  let
    d = diffUTCTime (b ^. trStart) (a ^. trStart)
    rlen = max 0 $ d - (a ^. trLen)
    rstart = addUTCTime rlen $ a ^. trStart
  in if
    | rlen > 0  -> TimeRange rstart rlen : invertTimeRanges (b:rest)
    | otherwise -> invertTimeRanges (b:rest)
invertTimeRanges _ = []


data LenStatistics = LenStatistics
  { _sSum        :: !NominalDiffTime
  , _sMinLen     :: !(Maybe NominalDiffTime)
  , _sMaxLen     :: !(Maybe NominalDiffTime)
  , _sMedian     :: !(Maybe NominalDiffTime)
  , _sCount      :: !Int
  } deriving (Eq, Ord, Show)

makeLenses ''LenStatistics
