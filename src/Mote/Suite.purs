module Mote.Suite where

import Prelude

import Data.Maybe (Maybe(..))
import Mote.Entry (Entry)
import Mote.Entry as Entry

-- | The data structure backing the `MoteT` DSL.
data Suite a b
  = Item RunMode (Entry a b)
  | Group RunMode (Entry a (Array (Suite a b)))

-- | Smart constructor for `Item` with a basic entry.
item :: forall a b. String -> b -> Suite a b
item label = Item Normal <<< Entry.entry label

-- | Smart constructor for `Group` with a basic entry.
group :: forall a b. String -> Array (Suite a b) -> Suite a b
group label = Group Normal <<< Entry.entry label

-- | The run mode option for a `Suite`-item. Used when building a `Plan` from a
-- | `Suite`, this directs which tests should be skipped.
data RunMode = Normal | Only | Skip

-- | Modifies the `RunMode` of a `Suite`.
setRunMode :: forall a b. RunMode -> Suite a b -> Suite a b
setRunMode mode = case _ of
  Item _ entry ->
    Item mode entry
  Group _ entry ->
    Group mode entry

-- | Modifies the `bracket` value of a `Suite`.
setBracket
  :: forall a b r
   . { before :: a r, after :: r -> a Unit }
  -> Suite a b
  -> Suite a b
setBracket { before, after } = case _ of
  Item mode entry ->
    Item mode (entry { bracket = Just (Entry.bracket before after) })
  Group mode entry ->
    Group mode (entry { bracket = Just (Entry.bracket before after) })
