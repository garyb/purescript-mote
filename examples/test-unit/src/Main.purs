module Example.TestUnit where

import Prelude

import Control.Monad.Aff (never)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref as Ref
import Data.Const (Const)
import Data.Foldable (sequence_)
import Mote (Mote, group, test, plan)
import Mote.Plan (Plan, foldPlan)
import Test.QuickCheck (Result, (===))
import Test.Unit (Test, TestSuite, timeout)
import Test.Unit as TU
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTestWith, run)
import Test.Unit.Output.Fancy as Fancy
import Test.Unit.Output.Simple as Simple
import Test.Unit.Output.TAP as TAP
import Test.Unit.QuickCheck (quickCheck)

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

-- | Interpreter runs a `Mote` to produce a `TestSuite`
interpret :: forall eff. Mote (Const Void) (Test eff) Unit -> TestSuite eff
interpret = go <<< plan
  where
    go :: Plan (Const Void) (Test eff) -> TestSuite eff
    go =
      foldPlan
        (\{ label, value } -> TU.test label value)
        (\label -> TU.testSkip label (pure unit))
        (\{ label, value } -> TU.suite label (go value))
        sequence_

-- | Tests lifted from the tests in `purescript-test-unit`, but costructed using the
-- | `Mote` DSL rather than `Spec`.
tests :: forall eff. Ref.Ref Int -> Mote (Const Void) (Test (random :: RANDOM, ref :: Ref.REF | eff)) Unit
tests ref = do
  test "basic asserts" do
    Assert.assert "wasn't true" true
    Assert.assertFalse "wasn't false" false
  test "timeout" do
    Assert.expectFailure "didn't time out" $ timeout 100 never
  test "equal" do
    Assert.equal "omg" "omg"
    Assert.expectFailure "should be unequal" $ Assert.equal "omg" "wat"
  test "quickcheck" do
    quickCheck theCommutativeProperty
  group "a test suite" do
    test "a test in a test suite" do
      pure unit
  test "tests run only once: part 1" do
    liftEff $ Ref.modifyRef ref (_ + 1)
  test "tests run only once: part deux" do
    Assert.equal 1 =<< liftEff (Ref.readRef ref)
  group "another suite" do
    test "this should not run" do
      pure unit
    test "a test in another test suite" do
      pure unit

main :: forall e. Eff (avar :: AVAR, console :: CONSOLE, random :: RANDOM, ref :: Ref.REF, testOutput :: TESTOUTPUT | e) Unit
main = run do
  ref <- liftEff $ Ref.newRef 0
  runTestWith Fancy.runTest $ interpret $ tests ref
  liftEff $ Ref.writeRef ref 0
  runTestWith Simple.runTest $ interpret $ tests ref
  liftEff $ Ref.writeRef ref 0
  runTestWith TAP.runTest $ interpret $ tests ref
