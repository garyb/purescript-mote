module Mote.Plan where

import Mote.Entry (Entry)

newtype Plan t = Plan (Array (PlanItem t))

data PlanItem t
  = Item (Entry t t)
  | Skip String
  | Group (Entry t (Plan t))
