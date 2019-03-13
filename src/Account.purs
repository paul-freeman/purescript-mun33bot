module Account
  ( Account
  , mkAccount
  )
where

import Prelude

import Data.DateTime as DateTime
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Foldable (class Foldable, foldl)
import Data.List (toUnfoldable)
import Data.List as List
import Data.List.Types (List, (:))
import Data.Maybe (maybe)
import Data.String (joinWith)
import Data.Time.Duration (Days(..), convertDuration)
import Entry (Entry(..), EntryData(..))

newtype Account =
  Account
  { entries :: List Entry
  , n :: Int
  , age :: Days
  , frequency ::
    { x :: Days }
  }

instance showAccount :: Show Account where
  show (Account { entries, n, age, frequency: { x } }) =
    "Account:\n" <>
    joinWith "\n" (show <$> toUnfoldable entries) <> "\n" <>
    joinWith " | " [show n, show age, show x ] <> "\n"

mkAccount :: forall f. Foldable f => Instant -> f Entry -> Account
mkAccount now =
  foldl
    (processEntry now)
    (Account
      { entries: List.Nil
      , n: 0
      , age: -30.0 # Days
      , frequency:
        { x: 30.0 # Days }
      })

processEntry :: Instant -> Account -> Entry -> Account
processEntry now (Account { entries, n, age }) (entry@Entry { entryData: EntryData entryData } ) =
  let
    entryDateTime = toDateTime entryData.time
    nowDateTime = toDateTime now
    oneMonthBefore = maybe bottom identity <<< DateTime.adjust (-30.0 # Days)
  in
    Account
      { entries: entry : entries
      , n: n + 1
      , age: convertDuration $ DateTime.diff entryDateTime nowDateTime :: Days
      , frequency:
        { x: convertDuration $
              DateTime.diff
                entryDateTime
                (maybe
                  (oneMonthBefore nowDateTime)
                  (toDateTime <<< \(Entry { entryData: EntryData { time } }) -> time)
                  (List.head entries)
                )
                :: Days
        }
      }
