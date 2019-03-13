module Test.Main where

import Prelude

import Account (mkAccount)
import Data.Array (catMaybes, filter) as A
import Data.Maybe (Maybe, isJust)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Console (logShow)
import Entry (Entry, makeDateTimeEntry)
import Time as Time

main :: Effect Unit
main =
  let
    time = { hour: 0, minute: 0, second: 0, millisecond: 0 }
    m d m y a = makeDateTimeEntry { date: { day:  d, month: m, year: y }, time, amount: a }
    (entries :: Effect (Array (Maybe Entry))) = sequence
      [ m  5 12 2016 117.30
      , m 28  2 2017 122.40
      , m  4  6 2017  86.70
      , m 30  8 2017  86.70
      , m  7 12 2017  86.70
      , m  8  3 2018 117.30
      , m 28  5 2018 127.50
      , m 29  8 2018  86.70
      , m 29 11 2018  66.30
      , m 28  2 2019 111.15
      ]
  in do
    now <- Time.now
    (e :: Array (Maybe Entry)) <- entries
    logShow $ mkAccount now $ A.catMaybes e