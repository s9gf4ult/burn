module Peridot.TimeSeries where

import Data.Singletons.TH
import Data.Time
import Peridot.Types

singletons [d|
    data Over
      = OverLen
      | OverStart

    data Statistis
      = Count
      | Min Over
      | Max Over
      | Sum Over|]

type StatisticsKey = Sing Statistis
type OverKey = Sing Over

data OverValue (a :: Over) where
  OverLenValue :: NominalDiffTime -> OverValue 'OverLen
  OverStartValue :: UTCTime -> OverValue 'OverStart

data StatisticsValue (a :: Statistis) where
  CountValue :: Int -> StatisticsValue 'Count
  MinValue :: OverValue over -> StatisticsValue ('Min over)
  MaxValue :: OverValue over -> StatisticsValue ('Max over)
  SumValue :: OverValue over -> StatisticsValue ('Sum over)

-- | Type
data TimeSeries a
  = Start
  | Len
  | TSStatistics Statistis
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

data TimeSeriesValue (a :: TimeSeries rest) where
  StartValue        :: UTCTime              -> TimeSeriesValue 'Start
  LenValue          :: NominalDiffTime      -> TimeSeriesValue 'Len
  TSStatisticsValue :: StatisticsValue stat -> TimeSeriesValue ('TSStatistics stat)
  PayloadValue      :: Value rest           -> TimeSeriesValue ('Payload rest)
