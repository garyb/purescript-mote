module Mote.Description where

import Prelude

import Data.Maybe (Maybe(..))
import Mote.Entry (Entry)
import Mote.Entry as Entry

-- | The data structure backing the `MoteT` DSL.
data Description b t
  = Test RunMode (Entry b t)
  | Group RunMode (Entry b (Array (Description b t)))

derive instance functorDescription :: Functor (Description b)

-- | Basic constructor for `Test` with a label and value.
test :: forall b t. String -> t -> Description b t
test label = Test Normal <<< Entry.entry label

-- | Basic constructor for `Group` with a label and inner entries.
group
  :: forall b t
  . String
  -> Array (Description b t)
  -> Description b t
group label = Group Normal <<< Entry.entry label

-- | The run mode option for a `Description` item. Used when building a `Plan`
-- | from a `Description`, this directs which tests should be skipped.
data RunMode = Normal | Only | Skip

-- | Sets the `RunMode` of a `Description`.
setRunMode
  :: forall b t
   . RunMode
  -> Description b t
  -> Description b t
setRunMode mode = case _ of
  Test _ entry ->
    Test mode entry
  Group _ entry ->
    Group mode entry

-- | Sets the `bracket` value of a `Description`.
setBracket
  :: forall b t r
   . { before :: b r, after :: r -> b Unit }
  -> Description b t
  -> Description b t
setBracket { before, after } = case _ of
  Test mode entry ->
    Test mode (entry { bracket = Just (Entry.bracket before after) })
  Group mode entry ->
    Group mode (entry { bracket = Just (Entry.bracket before after) })
