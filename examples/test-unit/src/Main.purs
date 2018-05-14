module Example.TestUnit where

import Prelude

import Effect.Aff (never)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Data.Const (Const)
import Data.Foldable (sequence_)
import Mote (Mote, group, test, plan)
import Mote.Plan (Plan, foldPlan)
import Test.QuickCheck (Result, (===))
import Test.Unit (Test, TestSuite, timeout)
import Test.Unit as TU
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTestWith, run)
import Test.Unit.Output.Fancy as Fancy
import Test.Unit.Output.Simple as Simple
import Test.Unit.Output.TAP as TAP
import Test.Unit.QuickCheck (quickCheck)

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

-- | Interpreter runs a `Mote` to produce a `TestSuite`
interpret :: Mote (Const Void) Test Unit -> TestSuite
interpret = go <<< plan
  where
    go :: Plan (Const Void) Test -> TestSuite
    go =
      foldPlan
        (\{ label, value } -> TU.test label value)
        (\label -> TU.testSkip label (pure unit))
        (\{ label, value } -> TU.suite label (go value))
        sequence_

-- | Tests lifted from the tests in `purescript-test-unit`, but costructed using the
-- | `Mote` DSL rather than `Spec`.
tests :: Ref.Ref Int -> Mote (Const Void) Test Unit
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
    liftEffect $ Ref.modify (_ + 1) ref
  test "tests run only once: part deux" do
    Assert.equal 1 =<< liftEffect (Ref.read ref)
  group "another suite" do
    test "this should not run" do
      pure unit
    test "a test in another test suite" do
      pure unit

main :: Effect Unit
main = run do
  ref <- liftEffect $ Ref.new 0
  runTestWith Fancy.runTest $ interpret $ tests ref
  liftEffect $ Ref.write 0 ref
  runTestWith Simple.runTest $ interpret $ tests ref
  liftEffect $ Ref.write 0 ref
  runTestWith TAP.runTest $ interpret $ tests ref
