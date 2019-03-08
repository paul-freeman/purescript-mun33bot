module Test.Main where

import Prelude

import Account (mkAccount)
import Data.Array (filter) as A
import Data.Maybe (Maybe, isJust)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (logShow)
import Entry (Entry, makeDateEntry)

main :: Effect Unit
main =
  let
    m d m y a = makeDateEntry { date: { day:  d, month: m, year: y }, amount: a }
    (entries :: Effect (Array (Maybe Entry))) = traverse (\go -> go >>= pure)
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
  in
    logShow <<< mkAccount <<< A.filter isJust =<< entries