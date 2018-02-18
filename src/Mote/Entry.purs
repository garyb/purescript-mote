module Mote.Entry where

import Prelude

import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (Maybe(..))

-- | Generic entry type used in the description of groups and items in both
-- | `Description` and `Plan` representations.
type Entry b v =
  { label :: String
  , bracket :: Maybe (Exists (Bracket b))
  , value :: v
  }

-- | Creates an entry with the specifed label and no bracket action.
entry :: forall b v. String -> v -> Entry b v
entry label value = { label, bracket: Nothing, value }

-- | A data type used to carry bracketing for an entry. The `r` type is
-- | existentially hidden later so that the resources created and freed within
-- | an entry can vary between items in a suite or plan.
data Bracket b r = Bracket (b r) (r -> b Unit)

-- | Creates a bracket value for the `Entry` record. The first argument runs
-- | before the group/item, possibly generating some kind of resource r. The
-- | second argument runs on test completion, accepting the `r` generated in
-- | allocation to allow it to de-allocate/clean up.
bracket :: forall b r. b r -> (r -> b Unit) -> Exists (Bracket b)
bracket f = mkExists <<< Bracket f

-- | Unwraps an existentially hidden `Bracket` value.
unBracket
  :: forall b o
   . (forall r. b r -> (r -> b Unit) -> o)
  -> Exists (Bracket b)
  -> o
unBracket f = runExists \(Bracket g h) -> f g h
