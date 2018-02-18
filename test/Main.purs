module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (for_, sequence_)
import Mote.Monad (Mote, group, item, only, plan, skip)
import Mote.Plan (Plan(..), PlanItem(..))

spec :: forall t. Monad t => Mote (t Unit) Unit
spec = do
  group "A bunch of stuff" do
    skip $ item "Do a setup thing" do
      pure unit
    skip $ group "Some less stuff" do
      item "A thing" do
        pure unit
      only $ item "Another thing" do
        pure unit
      item "A final thing" do
        pure unit
    group "Some other less stuff" do
      item "A other thing" do
        pure unit
      item "Another other thing" do
        pure unit

interpret
  :: forall eff
   . Plan (Eff (console :: CONSOLE | eff) Unit)
  -> Eff (console :: CONSOLE | eff) Unit
interpret = go ""
  where
    go prefix (Plan items) = for_ items case _ of
      Item { label, before, value, after } -> do
        log $ prefix <> label
        sequence_ before
        value
        sequence_ after
      Skip label -> do
        log $ prefix <> "Skip: " <> label
      Group { label, before, value, after } -> do
        log $ prefix <> label
        sequence_ before
        go (prefix <> "  ") value
        sequence_ after

main :: Eff (console :: CONSOLE) Unit
main = interpret $ plan spec
