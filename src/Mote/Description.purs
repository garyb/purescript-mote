module Mote.Description where

import Prelude

import Data.Maybe (Maybe(..))
import Mote.Entry (Entry)
import Mote.Entry as Entry

-- | The data structure backing the `MoteT` DSL.
data Description a b
  = Test RunMode (Entry a b)
  | Group RunMode (Entry a (Array (Description a b)))

-- | Smart constructor for `Test` with a basic entry.
test :: forall a b. String -> b -> Description a b
test label = Test Normal <<< Entry.entry label

-- | Smart constructor for `Group` with a basic entry.
group :: forall a b. String -> Array (Description a b) -> Description a b
group label = Group Normal <<< Entry.entry label

-- | The run mode option for a `Description` item. Used when building a `Plan`
-- | from a `Description`, this directs which tests should be skipped.
data RunMode = Normal | Only | Skip

-- | Sets the `RunMode` of a `Description`.
setRunMode :: forall a b. RunMode -> Description a b -> Description a b
setRunMode mode = case _ of
  Test _ entry ->
    Test mode entry
  Group _ entry ->
    Group mode entry

-- | Sets the `bracket` value of a `Description`.
setBracket
  :: forall a b r
   . { before :: a r, after :: r -> a Unit }
  -> Description a b
  -> Description a b
setBracket { before, after } = case _ of
  Test mode entry ->
    Test mode (entry { bracket = Just (Entry.bracket before after) })
  Group mode entry ->
    Group mode (entry { bracket = Just (Entry.bracket before after) })
