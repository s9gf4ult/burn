module Peridot.TimeSeries where

import Control.Foldl as FL
import Data.Dependent.Map as DM
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.Singletons
import Data.Singletons.Prelude (Sing(..))
import Data.Singletons.TH
import Data.Time
import Peridot.Types

singletons [d|
    data Over
      = OverLen
      | OverStart

    data Statistics
      = Count
      | Min Over
      | Max Over
      | Sum Over|]

type StatisticsKey = Sing Statistics
type OverKey = Sing Over

data OverValue (a :: Over) where
  OverLenValue :: NominalDiffTime -> OverValue 'OverLen
  OverStartValue :: UTCTime -> OverValue 'OverStart

data StatisticsValue (a :: Statistics) where
  CountValue :: Int -> StatisticsValue 'Count
  MinValue :: OverValue over -> StatisticsValue ('Min over)
  MaxValue :: OverValue over -> StatisticsValue ('Max over)
  SumValue :: OverValue over -> StatisticsValue ('Sum over)

-- | Type
data TimeSeries a
  = Start
  | Len
  | TSStatistics Statistics
  | Payload a

-- | Instnatiate this TF for your extended data to define key
type family Key (a :: k) :: *
type instance Key Void = Proxy Void

type family Value (a :: k) :: *
type instance Value Void = Proxy Void

data TimeSeriesKey (a :: TimeSeries rest) where
  StartKey        :: TimeSeriesKey 'Start
  LenKey          :: TimeSeriesKey 'Len
  TSStatisticsKey :: Sing stat -> TimeSeriesKey ('TSStatistics stat)
  PayloadKey      :: Key rest -> TimeSeriesKey ('Payload rest)

instance GEq TimeSeriesKey -- FIXME: implement
instance GCompare TimeSeriesKey -- FIXME: implement

data TimeSeriesValue (a :: TimeSeries rest) where
  StartValue        :: UTCTime              -> TimeSeriesValue 'Start
  LenValue          :: NominalDiffTime      -> TimeSeriesValue 'Len
  TSStatisticsValue :: StatisticsValue stat -> TimeSeriesValue ('TSStatistics stat)
  PayloadValue      :: Value rest           -> TimeSeriesValue ('Payload rest)

type TSRecord = Record TimeSeriesKey TimeSeriesValue
type TSRecFolder = RecFolder TimeSeriesKey TimeSeriesValue
type TSGrouper key = RecGrouper key TimeSeriesKey TimeSeriesValue

mkStatFoldl :: Sing (stat :: Statistics) -> FL.Fold TSRecord TSRecord
mkStatFoldl = \case
  SCount -> (DM.singleton (TSStatisticsKey SCount) . TSStatisticsValue . CountValue) <$> FL.length
  -- FIXME: implement the rest of constructors

squashStatFoldl :: Sing (stat :: [Statistics]) -> FL.Fold TSRecord TSRecord
squashStatFoldl = \case
  SCons stat rest -> DM.union <$> mkStatFoldl stat <*> squashStatFoldl rest
  SNil            -> pure $ DM.empty

tsRecordFolder :: Sing (stat :: [Statistics]) -> TSRecFolder
tsRecordFolder stat = FL.fold (squashStatFoldl stat)
