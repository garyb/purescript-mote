module Mote.Monad where

import Prelude

import Effect.Class (class MonadEffect)
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.Writer (class MonadTrans, WriterT, censor, mapWriterT, runWriterT, tell)
import Data.Array (mapMaybe)
import Data.Foldable (any)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, un)
import Data.These (These(..), theseLeft, theseRight)
import Data.Tuple (snd)
import Mote.Description (RunMode(..), Description(..))
import Mote.Description as Description
import Mote.Plan as Plan

-- | The main `MoteT` / `Mote` monadic DSL used to describe tests and groups of
-- | tests.
-- |
-- | After description via this DSL a `Plan` can be generated, that can then
-- | finally be interpreted into some target monad.
-- |
-- | - The `bracket` type represents a type of kind `Type -> Type` kind in which
-- |   bracketing can be dealt with. This type is higher kinded as the "before"
-- |   part of a bracket is expressed by a type like `bracket r`, so that the
-- |   "after" part can consume it as `r -> bracket Unit` later. If bracketing
-- |   is not required, setting this to `Const Void` is a good way to
-- |   communicate that it is unused/unusable.
-- | - The `test` type represents the type of tests. The kind here is only
-- |   required to be `Type` but will usually be something like `m Unit` (where
-- |   `m` here is some monad to run the tests in, not the `m` of `MoteT`).
-- | - The `m` is an underlying monad that can be used to perform effects while
-- |   constructing the test suite. This allows for tests to be generated from
-- |   the filesystem, reading from a `Reader`-based environment, etc.
-- |
-- | The `bracket` and `test` types are separated to allow for tests and
-- | bracketing to have different constraints and capabilities. In some cases
-- | it might be desirable to run tests with an alternative `Reader`, or
-- | restrict the kind of effects tests are allowed to perform compared with
-- | the bracketing code.
newtype MoteT bracket test m a = MoteT (WriterT (Array (Description bracket test)) m a)

-- | A non-effectful version of `MoteT`. This is for cases where groups and
-- | tests can be described purely.
type Mote bracket test = MoteT bracket test Identity

derive instance newtypeMoteT :: Newtype (MoteT bracket test m a) _
derive newtype instance functorMoteT :: Functor m => Functor (MoteT bracket test m)
derive newtype instance applyMoteT :: Apply m => Apply (MoteT bracket test m)
derive newtype instance applicativeMoteT :: Applicative m => Applicative (MoteT bracket test m)
derive newtype instance bindMoteT :: Bind m => Bind (MoteT bracket test m)
derive newtype instance monadMoteT :: Monad m => Monad (MoteT bracket test m)
derive newtype instance monadTransMoteT :: MonadTrans (MoteT bracket test)
derive newtype instance monadAskMoteT :: MonadAsk r m => MonadAsk r (MoteT bracket test m)
derive newtype instance monadReaderMoteT :: MonadReader r m => MonadReader r (MoteT bracket test m)
derive newtype instance monadEffectMoteT :: MonadEffect m => MonadEffect (MoteT bracket test m)

-- | Changes the `m` effect monad used during test suite construction.
hoist
  :: forall bracket test m n
   . (m ~> n)
  -> MoteT bracket test m
  ~> MoteT bracket test n
hoist nat = over MoteT (mapWriterT nat)

mapTest
  :: forall bracket test test' m
   . Functor m
  => (test -> test')
  -> MoteT bracket test m
  ~> MoteT bracket test' m
mapTest f = over MoteT (mapWriterT (map (map (map (map f)))))

-- | Describes a new group. Groups can contain further groups or tests, or a
-- | combination of both.
group
  :: forall bracket test m a
   . Monad m
  => String
  -> MoteT bracket test m a
  -> MoteT bracket test m a
group label (MoteT ss) = MoteT (censor (pure <<< Description.group label) ss)

-- | Describes a new test.
test
  :: forall bracket test m
   . Monad m
  => String
  -> test
  -> MoteT bracket test m Unit
test label = MoteT <<< tell <<< pure <<< Description.test label

-- | Marks the following group(s) and/or test(s) to be skipped when generating
-- | a plan.
skip
  :: forall bracket test m a
   . Monad m
  => MoteT bracket test m a
  -> MoteT bracket test m a
skip (MoteT ss) = MoteT (censor (map (Description.setRunMode Skip)) ss)

-- | Marks the following group(s) and/or test(s) to be added to a plan while
-- | skipping any other siblings that are not also tagged with `only`.
only
  :: forall bracket test m a
   . Monad m
  => MoteT bracket test m a
  -> MoteT bracket test m a
only (MoteT ss) = MoteT (censor (map (Description.setRunMode Only)) ss)

-- | Specifies actions to run before and after the following group(s) and/or
-- | test(s).
-- |
-- | The bracketing is applied to every following group or test individually;
-- | it will be repeated when each group or test is run.
bracket
  :: forall bracket test m a resource
   . Monad m
  => { before :: bracket resource, after :: resource -> bracket Unit }
  -> MoteT bracket test m a
  -> MoteT bracket test m a
bracket b (MoteT ss) = MoteT (censor (map (Description.setBracket b)) ss)

-- | Generate a `Plan` from a `Mote`. The result of this can then be
-- | interpreted to actually run the suites and tests described in the `Mote`.
plan :: forall bracket test a. Mote bracket test a -> Plan.Plan bracket test
plan = un Identity <<< planT

-- | Generate a `Plan` from a `MoteT`, running effects as necessary. The result
-- | of this can then be interpreted to actually run the suites and tests
-- | described in the `MoteT`.
planT
  :: forall bracket test m a
   . Monad m
  => MoteT bracket test m a
  -> m (Plan.Plan bracket test)
planT (MoteT wma) = loop <<< snd <$> runWriterT wma
  where
    loop :: Array (Description bracket test) -> Plan.Plan bracket test
    loop ss =
      let ps = map go ss
      in Plan.Plan (mapMaybe (if any isThat ps then theseRight else theseLeft) ps)

    go
      :: Description bracket test
      -> These (Plan.PlanItem bracket test) (Plan.PlanItem bracket test)
    go = case _ of
      Test Skip entry ->
        let a = Plan.Skip entry.label in Both a a
      Test Normal entry ->
        Both (Plan.Test entry) (Plan.Skip entry.label)
      Test Only entry ->
        That (Plan.Test entry)
      Group Skip { label, value } ->
        let a = Plan.Group { label, bracket: Nothing, value: goSkip value }
        in Both a a
      Group Normal { label, bracket: b, value } ->
        Both
          (Plan.Group { label, bracket: b, value: loop value })
          (Plan.Skip label)
      Group Only { label, bracket: b, value } ->
        That (Plan.Group { label, bracket: b, value: loop value })

    goSkip :: Array (Description bracket test) -> Plan.Plan bracket test
    goSkip a = Plan.Plan $ a <#> case _ of
      Test _ { label } ->
        Plan.Skip label
      Group _ { label, value } ->
        Plan.Group { label, bracket: Nothing, value: goSkip value }

    isThat :: forall l r. These l r -> Boolean
    isThat = case _ of
      That _ -> true
      _ -> false
