module Mote.Monad where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.Writer (class MonadTrans, WriterT, censor, runWriterT, tell)
import Data.Array (mapMaybe)
import Data.Foldable (any)
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Data.These (These(..), theseLeft, theseRight)
import Data.Tuple (Tuple(..))
import Mote.Plan as Plan
import Mote.Suite (RunMode(..), Suite(..))
import Mote.Suite as Suite

newtype MoteT t m a = MoteT (WriterT (Array (Suite t)) m a)

type Mote t = MoteT t Identity

derive newtype instance functorMoteT :: Functor m => Functor (MoteT t m)
derive newtype instance applyMoteT :: Apply m => Apply (MoteT t m)
derive newtype instance applicativeMoteT :: Applicative m => Applicative (MoteT t m)
derive newtype instance bindMoteT :: Bind m => Bind (MoteT t m)
derive newtype instance monadMoteT :: Monad m => Monad (MoteT t m)
derive newtype instance monadTransMoteT :: MonadTrans (MoteT t)
derive newtype instance monadAskMoteT :: MonadAsk r m => MonadAsk r (MoteT t m)
derive newtype instance monadReaderMoteT :: MonadReader r m => MonadReader r (MoteT t m)

group :: forall t m a. Monad m => String -> MoteT t m a -> MoteT t m a
group label (MoteT ss) = MoteT (censor (pure <<< Suite.group label) ss)

item :: forall m t. Monad m => String -> t -> MoteT t m Unit
item label = MoteT <<< tell <<< pure <<< Suite.item label

only :: forall t m a. Monad m => MoteT t m a -> MoteT t m a
only = setRunMode Only

skip :: forall t m a. Monad m => MoteT t m a -> MoteT t m a
skip = setRunMode Skip

setRunMode :: forall t m a. Monad m => RunMode -> MoteT t m a -> MoteT t m a
setRunMode mode (MoteT ss) = MoteT (censor (map (Suite.setRunMode mode)) ss)

plan :: forall t a. Mote t a -> Plan.Plan t
plan = un Identity <<< planT

planT :: forall t m a. Monad m => MoteT t m a -> m (Plan.Plan t)
planT (MoteT wma) = do
  Tuple _ entries <- runWriterT wma
  pure (go entries)
  where
    go :: Array (Suite t) -> Plan.Plan t
    go ss =
      let ps = map goSuite ss
      in Plan.Plan (mapMaybe (if any isThat ps then theseRight else theseLeft) ps)

    goSuite :: Suite t -> These (Plan.PlanItem t) (Plan.PlanItem t)
    goSuite = case _ of
      Item Skip entry ->
        let a = Plan.Skip entry.label in Both a a
      Item Normal entry ->
        Both (Plan.Item entry) (Plan.Skip entry.label)
      Item Only entry ->
        That (Plan.Item entry)
      Group Skip entry ->
        let a = Plan.Skip entry.label in Both a a
      Group Normal { label, before, value, after } ->
        Both
          (Plan.Group { label, before, value: go value, after })
          (Plan.Skip label)
      Group Only { label, before, value, after } ->
        That (Plan.Group { label, before, value: go value, after })

    isThat :: forall x y. These x y -> Boolean
    isThat = case _ of
      That _ -> true
      _ -> false
