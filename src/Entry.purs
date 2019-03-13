module Entry
  ( Entry(..)
  , EntryData(..)
  , makeInstantEntry
  , makeDateEntry
  , makeDateTimeEntry
  ) where

import Prelude



import Data.DateTime.Instant (Instant)
import Data.JSDate as JSDate
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Time as Time

newtype Entry =
  Entry
  { timestamp :: Instant
  , entryData :: EntryData
  }

newtype EntryData =
  EntryData
  { time :: Instant
  , amount :: Number
  }

instance showEntry :: Show Entry where
  show (
    Entry
    { entryData: EntryData
      { time
      , amount
      }
    }
  ) =
    (JSDate.toString $ JSDate.fromInstant time)
      <> ": $" <> show amount

makeInstantEntry :: { amount :: Number } -> Effect Entry
makeInstantEntry { amount } = do
  t <- Time.now
  pure $ Entry
    { timestamp: t
    , entryData: EntryData
      { time: t
      , amount
      }
    }

makeDateEntry
  ::{ amount :: Number
    , date ::
      { day :: Int
      , month :: Int
      , year :: Int
      }
    }
  -> Effect (Maybe Entry)
makeDateEntry { date, amount } =
  Time.now >>= \timestamp ->
    Time.makeDateInstant date >>=
      traverse \time ->
        pure $ Entry { timestamp, entryData: EntryData { time, amount } }

makeDateTimeEntry
  ::{ amount :: Number
    , date ::
      { day :: Int
      , month :: Int
      , year :: Int
      }
    , time ::
      { hour :: Int
      , minute :: Int
      , second :: Int
      , millisecond :: Int
      }
    }
  -> Effect (Maybe Entry)
makeDateTimeEntry { amount, date, time: time' } =
  Time.now >>= \timestamp ->
    Time.makeDateTimeInstant date time' >>=
      traverse \time ->
        pure $ Entry { timestamp, entryData: EntryData { time, amount } }
