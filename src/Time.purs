module Time
  ( DateData
  , TimeData
  , DateTimeData
  , now
  , noon
  , makeDateInstant
  , makeDateTimeInstant
  ) where

import Prelude

import Data.Date as Date
import Data.DateTime (DateTime(..), Time(..))
import Data.DateTime as DateTime
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Enum (fromEnum, toEnum)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Minutes(..)) as Duration
import Effect (Effect)
import Effect.Now as Effect.Now

type DateData =
  { year  :: Int
  , month :: Int
  , day   :: Int
  }
type TimeData =
  { hour        :: Int
  , minute      :: Int
  , second      :: Int
  , millisecond :: Int
  }
type DateTimeData =
  { data :: DateData
  , time :: TimeData
  }

now :: Effect Instant
now = Effect.Now.now

noon :: Effect (Maybe Instant)
noon = do
  DateTime date _ <- Effect.Now.nowDateTime
  makeDateInstant
    { year: fromEnum $ Date.year date
    , month: fromEnum $ Date.month date
    , day: fromEnum $ Date.day date
    }

makeDateInstant :: DateData -> Effect (Maybe Instant)
makeDateInstant date =
  makeDateTimeInstant date { hour: 12, minute: 0, second: 0, millisecond: 0 }

makeDateTimeInstant :: DateData -> TimeData -> Effect (Maybe Instant)
makeDateTimeInstant { year, month, day } { hour, minute, second, millisecond } =
  let
    mDateTime =
      ado
        date <- join $ Date.exactDate <$> toEnum year <*> toEnum month <*> toEnum day
        time <- Time <$> toEnum hour <*> toEnum minute <*> toEnum second <*> toEnum millisecond
      in DateTime date time
  in
    case mDateTime of
      Nothing -> pure $ Nothing
      Just dateTime -> do
        offset <- JSDate.getTimezoneOffset $ JSDate.fromDateTime dateTime
        pure $ map Instant.fromDateTime $ DateTime.adjust (Duration.Minutes offset) $ dateTime
