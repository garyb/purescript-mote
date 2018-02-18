module Mote.Suite where

import Prelude

import Mote.Entry (Entry, entry)

data RunMode = Normal | Only | Skip

data Suite a
  = Item RunMode (Entry a a)
  | Group RunMode (Entry a (Array (Suite a)))

item :: forall a. String -> a -> Suite a
item label = Item Normal <<< entry label

group :: forall a. String -> Array (Suite a) -> Suite a
group label = Group Normal <<< entry label

setRunMode :: forall a. RunMode -> Suite a -> Suite a
setRunMode mode = case _ of
  Item _ i -> Item mode i
  Group _ g -> Group mode g
