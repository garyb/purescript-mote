module Mote.Plan where

import Prelude

import Mote.Entry (Entry)

newtype Plan a b = Plan (Array (PlanItem a b))

data PlanItem a b
  = Item (Entry a b)
  | Skip String
  | Group (Entry a (Plan a b))

foldPlan
  :: forall a b i r
   . (Entry a b -> i)
  -> (String -> i)
  -> (Entry a (Plan a b) -> i)
  -> (Array i -> r)
  -> Plan a b
  -> r
foldPlan goItem goSkip goGroup squash (Plan items) =
  squash $ flip map items case _ of
    Item entry -> goItem entry
    Skip label -> goSkip label
    Group entry -> goGroup entry
