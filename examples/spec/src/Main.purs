module Example.Spec where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Const (Const)
import Data.Foldable (sequence_)
import Mote (Mote, plan, group, test)
import Mote.Plan (Plan, foldPlan)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldContain) as A
import Test.Spec.Assertions.Aff (expectError) as AF
import Test.Spec.Assertions.String (shouldContain, shouldNotContain) as AS
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: forall eff. Eff (RunnerEffects eff) Unit
main = run [ consoleReporter ] do
  interpret assertionSpec

-- | Interpreter runs a `Mote` to produce a `Spec`
interpret :: forall eff. Mote (Const Void) (Aff eff Unit) Unit -> Spec eff Unit
interpret = go <<< plan
  where
    go :: Plan (Const Void) (Aff eff Unit) -> Spec eff Unit
    go =
      foldPlan
        (\{ label, value } -> it label value)
        (\label -> pending label)
        (\{ label, value } -> describe label (go value))
        sequence_

-- | Spec lifted from the tests in `purescript-spec`, but costructed using the
-- | `Mote` DSL rather than `Spec`.
assertionSpec :: forall eff. Mote (Const Void) (Aff eff Unit) Unit
assertionSpec =
  group "Test" $
    group "Spec" $
      group "Assertions" do

        group "String" do
          group "shouldContain" do
            test "accepts strings that contains substrings" $
              "foobar" `AS.shouldContain` "foo"
            test "rejects strings that does not contain substrings" $
              AF.expectError $ "baz" `AS.shouldContain` "foo"

          group "shouldNotContain" do
            test "accepts strings that does not contain substrings" $
              "foobar" `AS.shouldNotContain` "baz"
            test "rejects strings that contains substrings" $
              AF.expectError $ "bazbar" `AS.shouldNotContain` "baz"

        group "Foldable" do
          group "for some foldable" do
            let f = ["haha", "nono"]
            let contained = "nono"
            let notcontained = "zzz"

            group "shouldContain" do
              test "accepts f that contains a" $
                f `A.shouldContain` contained
              test "rejects f that does not contain a" $
                AF.expectError $ f `A.shouldContain` notcontained


        group "Aff" $
          group "expectError" do
            test "returns unit when given an error" $
              AF.expectError $ throwError $ error "omg"
            test "returns an error when given a non-error" $
              AF.expectError $ AF.expectError $ pure "ok"
