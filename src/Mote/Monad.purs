module Mote.Monad where

import Prelude

import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.Writer (class MonadTrans, WriterT, censor, runWriterT, tell)
import Data.Array (mapMaybe)
import Data.Foldable (any)
import Data.Identity (Identity(..))
import Data.Newtype (class Newtype, un)
import Data.These (These(..), theseLeft, theseRight)
import Data.Tuple (snd)
import Mote.Plan as Plan
import Mote.Suite (RunMode(..), Suite(..))
import Mote.Suite as Suite

-- | The main `MoteT` / `Mote` monadic DSL used to describe groups (suites) and
-- | items (tests).
-- |
-- | After description via this DSL a `Plan` can be generated, that can then
-- | finally be interpreted in the target monad.
newtype MoteT x y m a = MoteT (WriterT (Array (Suite x y)) m a)

-- | A non-effectful version of `MoteT`. This is for cases where groups and
-- | items can be described purely.
type Mote x y = MoteT x y Identity

derive instance newtypeMoteT :: Newtype (MoteT x y m a) _
derive newtype instance functorMoteT :: Functor m => Functor (MoteT x y m)
derive newtype instance applyMoteT :: Apply m => Apply (MoteT x y m)
derive newtype instance applicativeMoteT :: Applicative m => Applicative (MoteT x y m)
derive newtype instance bindMoteT :: Bind m => Bind (MoteT x y m)
derive newtype instance monadMoteT :: Monad m => Monad (MoteT x y m)
derive newtype instance monadTransMoteT :: MonadTrans (MoteT x y)
derive newtype instance monadAskMoteT :: MonadAsk r m => MonadAsk r (MoteT x y m)
derive newtype instance monadReaderMoteT :: MonadReader r m => MonadReader r (MoteT x y m)
derive newtype instance monadEffMoteT :: MonadEff eff m => MonadEff eff (MoteT x y m)

-- | Describes a new group (suite).
group :: forall x y m a. Monad m => String -> MoteT x y m a -> MoteT x y m a
group label (MoteT ss) = MoteT (censor (pure <<< Suite.group label) ss)

-- | Describes a new item (test).
item :: forall x y m. Monad m => String -> y -> MoteT x y m Unit
item label = MoteT <<< tell <<< pure <<< Suite.item label

-- | Marks the following group(s) and/or item(s) to be skipped when generating
-- | a plan.
skip :: forall x y m a. Monad m => MoteT x y m a -> MoteT x y m a
skip (MoteT ss) = MoteT (censor (map (Suite.setRunMode Skip)) ss)

-- | Marks the following group(s) and/or item(s) to be added to a plan while
-- | skipping any other siblings that are not also tagged with `only`.
only :: forall x y m a. Monad m => MoteT x y m a -> MoteT x y m a
only (MoteT ss) = MoteT (censor (map (Suite.setRunMode Only)) ss)

-- | Specifies actions to run before and after the following group(s) and/or
-- | item(s).
-- |
-- | The bracketing is applied to every following group or item individually;
-- | it will be repeated when each group or item is run.
bracket
  :: forall x y m a r
   . Monad m
  => { before :: x r, after :: r -> x Unit }
  -> MoteT x y m a
  -> MoteT x y m a
bracket b (MoteT ss) = MoteT (censor (map (Suite.setBracket b)) ss)

-- | Generate a `Plan` from a `Mote`. The result of this can then be
-- | interpreted to actually run the suites and tests described in the `Mote`.
plan :: forall x y a. Mote x y a -> Plan.Plan x y
plan = un Identity <<< planT

-- | Generate a `Plan` from a `MoteT`, running effects as necessary. The result
-- | of this can then be interpreted to actually run the suites and tests
-- | described in the `MoteT`.
planT :: forall x y m a. Monad m => MoteT x y m a -> m (Plan.Plan x y)
planT (MoteT wma) = go <<< snd <$> runWriterT wma
  where
    go :: Array (Suite x y) -> Plan.Plan x y
    go ss =
      let ps = map goSuite ss
      in Plan.Plan (mapMaybe (if any isThat ps then theseRight else theseLeft) ps)

    goSuite :: Suite x y -> These (Plan.PlanItem x y) (Plan.PlanItem x y)
    goSuite = case _ of
      Item Skip entry ->
        let a = Plan.Skip entry.label in Both a a
      Item Normal entry ->
        Both (Plan.Item entry) (Plan.Skip entry.label)
      Item Only entry ->
        That (Plan.Item entry)
      Group Skip entry ->
        let a = Plan.Skip entry.label in Both a a
      Group Normal { label, bracket: b, value } ->
        Both
          (Plan.Group { label, bracket: b, value: go value })
          (Plan.Skip label)
      Group Only { label, bracket: b, value } ->
        That (Plan.Group { label, bracket: b, value: go value })

    isThat :: forall l r. These l r -> Boolean
    isThat = case _ of
      That _ -> true
      _ -> false
