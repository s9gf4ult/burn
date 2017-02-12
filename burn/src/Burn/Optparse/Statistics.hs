module Burn.Optparse.Statistics where

import Burn.Types
import Control.Lens
import Data.Default
import Data.Monoid
import Data.Time
import Data.Traversable
import Options.Applicative

data DaySpec
  = Beginning
  | Now
  | WeekEnd DaySpec
  | WeekBeginning DaySpec
  | AddDays Integer DaySpec
  | AddWeeks Int DaySpec
  | MonthEnd DaySpec
  | MonthBeginning DaySpec
  | AddMonths Int DaySpec
  | ThisTime Day
  deriving (Eq, Ord, Show)

makePrisms ''DaySpec

calculateDaySpec
  :: Day
     -- ^ Begginning of region
  -> Day
     -- ^ Now
  -> DaySpec
  -> Day
calculateDaySpec beg now ds = case ds of
  Beginning -> beg
  Now -> now
  AddDays d spec -> addDays d $ calculateDaySpec beg now spec
  ThisTime d -> d

data FromTo = FromTo
  { _fFrom :: DaySpec
  , _fTo   :: DaySpec
  } deriving (Eq, Ord, Show)

makeLenses ''FromTo

-- | What data to put into histrogram and how values are grouped
data Histro = Histro
  deriving (Eq, Ord, Show)

data Groupping
  = NoGroupping
  | GroupByDay
  | GroupByWeek
  | GroupByMonth
  deriving (Eq, Ord, Show)

makePrisms ''Groupping

data Summary = Summary
  { _sGrouping :: Groupping
  } deriving (Eq, Ord, Show)

makeLenses ''Summary

data Stats
  = StatsHistro Histro
  | StatsSummary Summary
  deriving (Eq, Ord, Show)

makePrisms ''Stats

data StatQuery = StatQuery
  { _sqFilter :: FromTo
  , _sqStats  :: Stats
  } deriving (Eq, Ord, Show)

makeLenses ''StatQuery

statQuery :: Parser StatQuery
statQuery = option parseStatistics
  $ long "query" <> short 'q' <> help "Statistics query"
  where
    -- FIXME: implement the parser
    parseStatistics = return $ StatQuery
      { _sqFilter = FromTo Beginning Now
      , _sqStats = StatsSummary $ Summary GroupByDay }
