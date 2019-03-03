module Entry
  ( Entry
  , makeInstantEntry
  , makeDateEntry
  ) where

import Prelude

import Control.Apply (lift2)
import Data.DateTime.Instant (Instant)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Optic.Core (set, (..))
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

makeDateEntry :: { amount :: Number, date :: { day :: Int, month :: Int, year :: Int } } -> Effect (Maybe Entry)
makeDateEntry { date, amount } =
  let
    _Entry f (Entry o) = Entry <$> f o
    entryData f o = f o.entryData <#> \entryData' -> o{entryData = entryData'}
    _EntryData f (EntryData o) = EntryData <$> f o
    time f o = f o.time <#> \time' -> o{time = time'}
    changeTime = set (_Entry..entryData.._EntryData..time)
  in
  ado
    n <- Time.now
    t <- Time.makeDateInstant date
  in
    (lift2 changeTime) t $ Just $
      Entry
        { timestamp: n
        , entryData: EntryData
          { time: n
          , amount
          }
        }
