module Mote.Plan where

import Prelude

import Mote.Entry (Entry)

-- | The plan for running a test suite.
newtype Plan b t = Plan (Array (PlanItem b t))

-- | An item in a `Plan`.
data PlanItem b t
  = Test (Entry b t)
  | Skip String
  | Group (Entry b (Plan b t))

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
  :: forall b t i r
   . (Entry b t -> i)
  -> (String -> i)
  -> (Entry b (Plan b t) -> i)
  -> (Array i -> r)
  -> Plan b t
  -> r
foldPlan goTest goSkip goGroup sequence (Plan items) =
  sequence $ flip map items case _ of
    Test entry -> goTest entry
    Skip label -> goSkip label
    Group entry -> goGroup entry
