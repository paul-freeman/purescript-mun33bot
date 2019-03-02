module Main where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.JSDate (jsdateLocal)
import Data.JSDate as JSDate
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Now as Effect.Now

type TemporalEntry = { instant :: Instant, entry :: Instant }
type DateData = { day :: Number, month :: Number, year :: Number }
type DateTimeData = { millisecond :: Number, second :: Number, minute :: Number, hour :: Number, day :: Number, month :: Number, year :: Number }

makeEntry :: Instant -> Effect TemporalEntry
makeEntry entry = pure <<< { instant:_, entry } =<< now

now :: Effect Instant
now = Effect.Now.now

noon :: Effect (Maybe Instant)
noon = do
  dateTime <- JSDate.now
  makeDateInstant
    { year: JSDate.getUTCFullYear dateTime
    , month: JSDate.getUTCMonth dateTime
    , day: JSDate.getUTCDate dateTime
    }

makeDateInstant :: DateData -> Effect (Maybe Instant)
makeDateInstant { year, month, day } =
  makeDateTimeInstant { year, month, day, hour: 12.0, minute: 0.0, second: 0.0, millisecond: 0.0 }

makeDateTimeInstant :: DateTimeData -> Effect (Maybe Instant)
makeDateTimeInstant dateTimeData =
  JSDate.toInstant <$> jsdateLocal dateTimeData
