module Test.Main where

import Prelude

import Data.Foldable (sequence_)
import Data.Maybe (Maybe, maybe)
import Data.Monoid (power)
import Effect (Effect)
import Effect.Console (log)
import Mote (Mote, Plan, bracket, group, test, only, plan, skip)
import Mote.Entry (Bracket, unBracket)
import Mote.Plan (foldPlan)

main :: Effect Unit
main = interpret $ plan spec

interpret :: Plan Effect (Effect Unit) -> Effect Unit
interpret = run 0
  where
    run depth =
      foldPlan
        (\{ label, bracket, value } -> do
          log (indent depth label)
          withBracket depth bracket value)
        (\label -> log (indent depth ("Skip: " <> label)))
        (\{ label, bracket, value } -> do
          log (indent depth label)
          withBracket depth bracket (run (depth + 1) value))
        sequence_
    indent :: Int -> String -> String
    indent depth s = power "--" depth <> s

    withBracket
      :: forall a
       . Int
      -> Maybe (Bracket Effect)
      -> Effect a
      -> Effect a
    withBracket depth mbracket act = maybe act go mbracket
      where
        go :: Bracket Effect -> Effect a
        go = unBracket \before after -> do
          r <- before
          result <- act
          after r
          pure result

spec :: Mote Effect (Effect Unit) Unit
spec = do
  group "A bunch of stuff" do
    skip $ test "Do a setup thing" do
      pure unit
    skip $ group "Some less stuff" do
      test "A thing" do
        pure unit
      only $ test "Another thing" do
        pure unit
      test "A final thing" do
        pure unit
    group "Some other less stuff" do
      bracket { before: log "> Sneak before", after: const (log "> Sneak after") } do
        test "A other thing" do
          log "Do some test bidnezz"
          pure unit
        test "A other thing 2" do
          log "Do some test bidnezz"
          pure unit
      test "Another other thing" do
        pure unit
