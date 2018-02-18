module Mote.Entry where

import Prelude

import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (Maybe(..))

-- | Generic entry type used in the description of groups and items in both
-- | `Suite` and `Plan` representations.
type Entry a b =
  { label :: String
  , bracket :: Maybe (Exists (Bracket a))
  , value :: b
  }

-- | Creates an entry with the specifed label and no bracket action.
entry :: forall a b. String -> b -> Entry a b
entry label value = { label, bracket: Nothing, value }

-- | A data type used to carry bracketing for an entry. The `r` type is
-- | existentially hidden later so that the resources created and freed within
-- | an entry can vary between items in a suite or plan.
data Bracket a r = Bracket (a r) (r -> a Unit)

-- | Creates a bracket value for the `Entry` record. The first argument runs
-- | before the group/item, possibly generating some kind of resource r. The
-- | second argument runs on test completion, accepting the `r` generated in
-- | allocation to allow it to de-allocate/clean up.
bracket :: forall a r. a r -> (r -> a Unit) -> Exists (Bracket a)
bracket f = mkExists <<< Bracket f

-- | Unwraps an existentially hidden `Bracket` value.
unBracket
  :: forall a o
   . (forall r. a r -> (r -> a Unit) -> o)
  -> Exists (Bracket a)
  -> o
unBracket f = runExists \(Bracket g h) -> f g h
