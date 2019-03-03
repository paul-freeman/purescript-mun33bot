module Test.Main where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Console (log, logShow)
import Entry (makeDateEntry)

main :: Effect Unit
main = do
  entries <-
    ado
      e1 <- makeDateEntry { date: { day:  6, month: 7, year: 2017 }, amount: 39.25 }
      e2 <- makeDateEntry { date: { day: 13, month: 7, year: 2017 }, amount: 42.94 }
      e3 <- makeDateEntry { date: { day: 20, month: 7, year: 2017 }, amount: 42.33 }
      e4 <- makeDateEntry { date: { day: 27, month: 7, year: 2017 }, amount: 54.20 }
      e5 <- makeDateEntry { date: { day: 32, month: 7, year: 2017 }, amount: 54.20 }
    in [ e1, e2, e3, e4, e5 ]
  traverse_ (maybe (log "error") (logShow)) entries
