module Mote.Plan where

import Prelude

import Mote.Entry (Entry)

-- | The plan for running a test suite.
newtype Plan a b = Plan (Array (PlanItem a b))

-- | An item in a `Plan`.
data PlanItem a b
  = Test (Entry a b)
  | Skip String
  | Group (Entry a (Plan a b))

-- | Eliminates each `PlanItem` constructor and sequences actions within a
-- | `Plan`.
-- |
-- | This function can be used to inspect the plan, or build derivatives of it,
-- | or to define an interpreter for the plan that will actually run the tests
-- | within.
-- |
-- | - The first function handles tests.
-- | - The second function handles skipped tests.
-- | - The third function handles groups of tests.
-- | - The fourth function deals with sequencing the resulting values from the
-- |   previous handlers.
-- |
-- | This fold only applies one layer at a time, so when building an interpreter
-- | it will need to be called recursively within the group handler.
foldPlan
  :: forall a b i r
   . (Entry a b -> i)
  -> (String -> i)
  -> (Entry a (Plan a b) -> i)
  -> (Array i -> r)
  -> Plan a b
  -> r
foldPlan goTest goSkip goGroup sequence (Plan items) =
  sequence $ flip map items case _ of
    Test entry -> goTest entry
    Skip label -> goSkip label
    Group entry -> goGroup entry
