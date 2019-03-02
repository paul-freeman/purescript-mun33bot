module Test.Main where

import Prelude

import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Now (now)
import Main (noon)
import Test.Unit.Console (consoleLog)

main :: Effect Unit
main = do
  consoleLog <<< append " now: " <<< show <<< map JSDate.toString <<< map JSDate.fromInstant <<< Just =<< now
  consoleLog <<< append "noon: " <<< show <<< map JSDate.toString <<< map JSDate.fromInstant =<< noon
