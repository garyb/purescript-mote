module Mote.Entry where

import Data.Maybe (Maybe(..))

type Entry a b =
  { label :: String
  , before :: Maybe a
  , value :: b
  , after :: Maybe a
  }

entry :: forall a b. String -> b -> Entry a b
entry label value = { label, before: Nothing, value, after: Nothing }
