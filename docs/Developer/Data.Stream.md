# Notation Note

We use unconventional type variable notations to show that the types are
absolutely identical and to show the incremental  progression one type to
another.

# Related docs

See the state machines doc.

# Function-shaped types

```haskell
(->) a b   -- the function constructor itself; kind * -> * -> *
```

# Arrows

`Arrow` lifts the idea of "function-shaped computation" into a class. The class
has kind `* -> * -> *`, so instances have the *shape* `a b c` (process from `b`
to `c`).

Hierarchy (all built on `Category`):

```
Category a            id, (.)
  Arrow a             arr, first  (+ second, (***), (&&&))
    ArrowZero a       zeroArrow
    ArrowPlus a       (<+>)
    ArrowChoice a     left  (+ right, (+++), (|||))
    ArrowApply a      app                -- equivalent to Monad
    ArrowLoop a       loop               -- fixed-point / feedback
ArrowMonad a          -- newtype: any ArrowApply gives a Monad
```

Canonical instances:

```haskell
(->)      a b    -- pure functions
Kleisli m a b    -- effectful actions: a -> m b
```

# Reified functions

Plain `a -> b` wrapped in a newtype to give it instances:

```haskell
newtype Endo   a   = Endo   { appEndo   :: a -> a }   -- Monoid via (id, .)
newtype Reader a b = Reader { runReader :: a -> b }   -- env-passing (the non-T one)
```

# Reified Actions: Category and MonadTrans

There are two different ways of representing `a -> m b`, identical at the value
level, different in parameter order:

```haskell
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }   -- Category shape
newtype ReaderT a m b = ReaderT { runReaderT :: a -> m b }   -- Transformer shape
```

Identical at the value level. Same `Functor`/`Applicative`/`Monad` instances —
the last slot (`b`) is the free one either way, so parameter order is
irrelevant for these three classes.

The order picks which *additional* abstraction the partially-applied type fits:

| Drop `b` → kind `* -> *` | Drop `b, m` → remaining shape | Fits |
|---|---|---|
| `Kleisli m a` (Monad) | `Kleisli m` :: `* -> * -> *` | `Category`, `Arrow` |
| `ReaderT a m` (Monad) | `ReaderT a` :: `(* -> *) -> * -> *` | `MonadTrans` |

A clearer naming would advertise the extra axis: e.g. `ReaderC`
(category-shaped) vs `ReaderT` (transformer-shaped). Or use Arrow without
suffix and transformer with T suffix.

## Producer Functions

A *producer* is a state-machine transition function that takes only a
state value and returns an output together with the next state. Feeding
the returned state back in produces the next output, and so on -- so a
state machine with no external input is just a generator of a stream:

```haskell
produce :: (s -> m (a, s)) -> s -> Stream m a
```

The step function here, `s -> m (a, s)`, is precisely what `StateT` reifies:

```haskell
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
```

So `StateT` *is* a producer function -- one without termination or
filtering -- wrapped in a newtype.

A producer can be viewed as a magical stateful reader where the state
advances every time you read it. Thus StateT can be viewed as a natural
progression of ReaderT into a state machine.

We can add termination capability by using `(s -> m (Maybe (a, s)))`, if the
function returns Nothing that means it has stopped producing. We can add
filtering by making the output type `Maybe a` instead of `a`.

Instead of using the Maybe type we can flatten the states into a single
constructor:
```haskell
data Outcome s b = Yield b s | Skip s | Stop
data Producer a m b = Producer (a -> m (Outcome a b))
-- instance MonadTrans (Producer a)
```

This is a **generalization** of a `StateT` action: `StateT` is the
special case that always `Yield`s, never skipping or stopping. The
richer type can represent a full stream-generation loop, with `Yield`,
`Skip`, and `Stop` as the primitives of a small looping DSL -- `Stop`
is `break`, `Skip` is `continue`, `Yield` emits a value.

Combinations of MaybeT and StateT can be used to represent different
capabilities:
```haskell
MaybeT (StateT a m) b  ≅  a -> m (Maybe b, a)        -- Yield / Skip  (filtering; state always kept)
StateT a (MaybeT m) b  ≅  a -> m (Maybe (b, a))      -- Yield / Stop  (termination; state discarded)
```

Our type is equivalent to:
```haskell
Outcome a b     ≅  Maybe (Maybe b, a)
Producer a m b  ≅  a -> m (Maybe (Maybe b, a))
                ≅  StateT a (MaybeT m) (Maybe b)
```

Producers compose with a `Monad` instance, much like `StateT`, giving
a functional representation of an imperative loop.

The parameter order `Producer a m b` is deliberate: `Producer a` then
has kind `(* -> *) -> * -> *`, so `instance MonadTrans (Producer a)`
typechecks.

Following the monad trnasformer convention the type can be called as
ProducerT.

# Producer Limitation

Category instance is not possible for Producer type.

We cannot compose two producers to generate another producer:
```
compose :: Producer m a b -> Producer m b c -> Producer m a c
compose (Producer step1) (Producer step2) = Producer step
  where
    step a = do
      r1 <- step1 a              -- step1 :: a -> m (Step a b)
      case r1 of
        Yield b s1 -> do          -- s1 :: a  (producer 1's next state)
          r2 <- step2 b           -- step2 :: b -> m (Step b c)
          case r2 of
            Yield c s2 ->         -- s2 :: b  (producer 2's next state)
              pure (Yield c s1)   -- we put s1 in the result — fits, type a ✓
              -- BUT: s2 :: b is DROPPED. We have nowhere to store it.
            Skip s2  -> pure (Skip s1)   -- s2 dropped again
            Stop     -> pure Stop
        Skip s1 -> pure (Skip s1)
        Stop    -> pure Stop
```

We cannot run nested loops. We can run the outer loop, emitting one element per
inner step, or run the inner loop for one outer step -- but not both. Neither
producer's state can be threaded through the other's loop. To make that
possible, the state must be either hidden behind an existential or carried
forward in a continuation.

# The Unfold Type

While it is possible to build a streaming system using states and Producer
functions alone, the approach quickly becomes unwieldy. The composition problem
identified in the previous section — the inability to thread multiple loop
states simultaneously — points to a deeper issue: the Producer type exposes its
state type as a user-visible parameter, leaving no room to carry auxiliary
state required by composition.

This is precisely where Haskell's type system comes to the rescue. Well-chosen
types hide internal complexity behind a clean interface, replacing raw
mechanical details with laws and higher-level guarantees. A good example of
this in Streamly is the concurrent stream types such as `Async` and `Ahead`,
which hide all concurrency machinery behind a type whose behaviour is governed
entirely by the `Monad` instance — the user composes with `>>=` and the
scheduling happens invisibly.

The key mechanism here is the **existential type**. An existential hides a type
variable so that it is scoped entirely within the type's implementation and can
never escape to the outside. The user sees only the interface; the concrete
choice of internal type is an implementation detail.

Applying this idea to producers: instead of exposing the state type `s` as a
parameter, we introduce an *injection function* `a -> m s` that maps the
user-visible seed value `a` into an arbitrary internal state `s`. Because `s`
is existentially quantified, each `Unfold` value can choose whatever internal
state representation it needs. In particular, during composition we are free to
define a composite state type that carries *both* the outer and inner loop
states simultaneously — exactly what was missing from the naive `compose`
attempt.

```haskell
data Unfold m a b = forall s. Unfold (Producer m s b) (a -> m s)
```

An `Unfold m a b` therefore bundles:
- a **Producer** `Producer m s b` that knows how to advance the internal state
  and yield output values of type `b`, and
- an **injection** `a -> m s` that initialises the internal state from a
  user-supplied seed of type `a`.

Because the state type `s` is hidden, the `Unfold` type admits `Category` and
`Arrow` instances -- the composition problem is solved.

```haskell
instance Category (Unfold m)
instance Arrow    (Unfold m)
```

```
-- unlikely but possible
newtype UnfoldT a m b = UnfoldT (Unfold m a b)
instance MonadTrans (UnfoldT a)
```

## Composing Two Unfolds

The earlier failure to compose two Producers came down to having nowhere to
store the inner Producer's state. With `Unfold`, we can define a composite
state that explicitly tracks which phase of the two-level loop we are in:

```haskell
data ConcatState s1 s2 = ConcatOuter s1 | ConcatInner s1 s2
```

`ConcatOuter s1` means we are still stepping the outer unfold (holding only its
state `s1`), while `ConcatInner s1 s2` means the outer unfold has yielded a
value, we have initialised the inner unfold from it, and we are now stepping
the inner unfold — carrying *both* states simultaneously.

The composition itself, which drives each element of the outer unfold through
the entire inner unfold before advancing, is written:

```haskell
unfoldEach :: Monad m => Unfold m b c -> Unfold m a b -> Unfold m a c
unfoldEach (Unfold step2 inject2) (Unfold step1 inject1) = Unfold step inject
  where
    inject x = ConcatOuter <$> inject1 x

    step (ConcatOuter st) = do
        r <- step1 st
        case r of
            Yield x s -> Skip . ConcatInner s <$> inject2 x
            Skip  s   -> return $ Skip (ConcatOuter s)
            Stop      -> return Stop

    step (ConcatInner ost ist) = do
        r <- step2 ist
        return $ case r of
            Yield x s -> Yield x  (ConcatInner ost s)
            Skip    s -> Skip     (ConcatInner ost s)
            Stop      -> Skip     (ConcatOuter ost)
```

The injection initialises the composite state in the `ConcatOuter` phase. The
stepper handles two cases:

- **`ConcatOuter`** — advance the outer unfold. On a `Yield`, inject the
  produced value into the inner unfold to obtain its initial state, and
  transition to `ConcatInner`. On `Skip` or `Stop`, propagate accordingly.
- **`ConcatInner`** — advance the inner unfold, threading both the outer state
  `ost` and the evolving inner state through the result. When the inner unfold
  reaches `Stop`, discard its state and return to `ConcatOuter` with the saved
  outer state, ready to pull the next element from the outer unfold.

The outer state is therefore never dropped: it is preserved inside
`ConcatInner` for the entire duration of the inner loop, and recovered when the
inner loop finishes — precisely the capability that was impossible with the raw
`Producer` type.

## Summary

The `Unfold` type can be thought of as an *injection-composed Producer*: it
decouples the user-visible seed type from the internal state type, giving the
implementation the freedom to use whatever state representation composition
demands. The existential quantification of `s` is what makes `Category` and
`Arrow` instances possible, and what turns an otherwise awkward low-level
mechanism into a composable, law-abiding abstraction.

## Monadic vs Pure Inject

The inject function in `Unfold` can be pure (`a -> s`) or monadic (`a
-> m s`). Pure inject makes conversion to `Stream` trivial, because
`Stream` uses a pure state:

```
unfold :: Unfold m a b -> a -> Stream m b
unfold (Unfold ustep inject) seed = Stream (\_ st -> ustep st) (inject seed)
```

With monadic inject, `unfold` needs an extra state to run the effect
once before entering the loop:

```
data UnfoldState s = UnfoldNothing | UnfoldJust s

unfold :: Applicative m => Unfold m a b -> a -> Stream m b
unfold (Unfold ustep inject) seed = Stream step UnfoldNothing

    where

    step _ UnfoldNothing = Skip . UnfoldJust <$> inject seed
    step _ (UnfoldJust st) = do
        (\case
            Yield x s -> Yield x (UnfoldJust s)
            Skip s    -> Skip (UnfoldJust s)
            Stop      -> Stop) <$> ustep st
```

This extra state and the one-time startup `Skip` are eliminated by
SpecConstr and the simplifier, so the steady-state cost is zero. The
complication is localized in one library function with controlled fusion
pragmas.

So why not make inject pure and accept the small simplification it
brings elsewhere? Because the same problem reappears in `lmapM`, in a
worse place.

With monadic inject, `lmapM` is a one-line Kleisli composition. The
effect runs once before the loop starts, and the existing state machine
is reused unchanged:

```
lmapM :: Monad m => (a -> m c) -> Unfold m c b -> Unfold m a b
lmapM f (Unfold ustep uinject) = Unfold ustep (f >=> uinject)
```

With pure inject, `lmapM` must defer the effect into the first step by
introducing a two-state machine — the same `Nothing`/`Just` pattern we
had in `unfold`, now relocated:

```
data LMapMState a s = LMapMPre a | LMapMRun s

lmapM :: Monad m => (a -> m c) -> Unfold m c b -> Unfold m a b
lmapM f (Unfold ustep uinject) = Unfold step LMapMPre

    where

    {-# INLINE_LATE step #-}
    step (LMapMPre a) = do
        r <- f a
        return $ Skip $ LMapMRun $ uinject r

    step (LMapMRun s) = do
        r <- ustep s
        return $ case r of
            Yield x s1 -> Yield x (LMapMRun s1)
            Skip s1    -> Skip (LMapMRun s1)
            Stop       -> Stop
```

In principle the same fusion that erases `UnfoldState` should erase
`LMapMState`. In practice it does not, reliably. Unfolds are used inside
nested stream loops, and `lmapM` states get introduced into inner loops
— each `lmapM` adding another layer of state that fusion must flatten.

### Benchmark

```
lmapM :: Monad m => Int -> Int -> m ()
lmapM size start =
    drainTransformationDefault (size + start) (UF.lmapM (return . (+) 1)) start

    where

    drainTransformationDefault to = drainTransformation (UF.second to UF.enumerateFromToIntegral)
    drainTransformation unf f seed = drainGeneration (f unf) seed
    drainGeneration unf seed = UF.fold FL.drain unf seed
```

With monadic inject, the core is a clean unboxed loop:

```
Rec {
main_$s$wgo
  = \ sc_s7a1 sc1_s7a0 sc2_s79Z eta_s79m ->
      case <=# sc1_s7a0 sc2_s79Z of {
        __DEFAULT -> eta_s79m;
        1# -> main_$s$wgo sc_s7a1 (+# sc1_s7a0 sc_s7a1) sc2_s79Z eta_s79m
      }
end Rec }

$wmain = \ s_s79q -> main_$s$wgo 1# 1# 100000# s_s79q

main1
  = \ s_s79q ->
      case $wmain s_s79q of ww_s79t { __DEFAULT -> (# ww_s79t, () #) }
```

With pure inject, a boxed `Int` (`I#`) survives in the loop, causing
per-iteration allocation:

```
main2 = I# 1#

Rec {
main_$s$wgo
  = \ sc_s79u sc1_s79v sc2_s79w sc3_s79x eta_s78I ->
      case sc2_s79w of { I# x_i77D ->
      case <=# x_i77D sc1_s79v of {
        __DEFAULT -> eta_s78I;
        1# ->
          main_$s$wgo
            sc_s79u
            sc1_s79v
            (case sc3_s79x of { I# y_i6zW -> I# (+# x_i77D y_i6zW) })
            sc3_s79x
            eta_s78I
      }
      }
end Rec }

main1
  = \ s_s78M ->
      case main_$s$wgo 1# 100000# main2 main2 s_s78M of ww_s78P
      { __DEFAULT ->
      (# ww_s78P, () #)
      }
```

The wrapping and unwrapping through `I#` degrades runtime roughly 10x
-- from 32 µs to 300 µs.

GHC could in principle be improved to eliminate this, but we prefer
not to fight the compiler when an equivalent design avoids the problem
entirely.

### Why not have both `Unfold` and `UnfoldM`?

Splitting the type would duplicate the entire combinator API surface,
force users to choose and convert between variants, and fragment the
fusion story across two code paths. Since monadic inject delivers the
same generated core as pure inject for the pure case (see the `unfold`
example above), there is no efficiency argument for the split -- only a
maintenance and ergonomics cost.

Monadic state generation is also common in practice: resource-acquiring
unfolds like `readFile`, `connect`, and `withFile` all need an effect
to produce the seed, and they need it to run once, eagerly, with
predictable bracketing semantics. Monadic inject is the right default
for these, and pays nothing extra for the pure case.

# The Stream Type

```haskell
data Stream m b = forall s. Stream (Producer m s b) s
```

A `Stream` bundles a Producer together with its current state into a
single value. This existential packaging gives `Stream` the shape of
a *value* rather than a *function*: users work with streams directly,
without ever having to deal with the state type.

The cost of hiding the state is compiler visibility. Because the step
function and the state are sealed inside the same existential box, the
compiler cannot see them independently. Every time a stream is passed
around, the box must travel whole -- opened to run one step, then
resealed to be passed on. The state cannot be unboxed or fused across
call boundaries.

`Unfold` avoids this cost precisely because it is a glorified
function. When an Unfold is passed as an argument, the call site
supplies the initial state explicitly, making it visible to the
compiler. The state can be unboxed, and the step function can be
inlined and fused with surrounding code. This is the primary advantage
of `Unfold` over `Stream` — not expressiveness, but optimizer
transparency.

## Passing Functions as Dynamic Values

```haskell
concatMapM :: Monad m => (a -> m (Stream m b)) -> Stream m a -> Stream m b
```

When `concatMapM` calls its argument on a value of type `a`, the result is a
`Stream` -- an existentially boxed pair of a step function and a state. That
step function must now be stored in the outer state machine's state and called
later, at a different site. The compiler sees it only as an opaque value
flowing through the heap, not as a statically known function, so it cannot
inline or specialise it at the call site. The result is a boxed function call
that returns boxed constructors, and neither layer of boxing can be eliminated.

This is the fundamental overhead of `concatMap` on `Stream`. By contrast,
`unfoldEach` takes the inner producer as an `Unfold` argument -- a statically
known function -- so the compiler retains full visibility, can inline the step
function, and can unbox the state. The nested loop fuses.

Passing functions statically allows fusion, passing functions as dynamic values
does not allow fusion.

# Resumability of Unfolds and Streams

The seed of an Unfold holds the initial state of the state machine. For
example, when streaming an array, the array together with the current
position is the initial state. If the Unfold stops in the middle, how do
we restart it from the same position?

There are two cases:

- **Mutable state** (IORef, file handle, socket): advancing the state
mutates the reference, so when the Unfold stops, the reference already
holds the current position. No special support is needed.
- **Immutable state** (e.g. an `(Array, Pos)` tuple): we need to extract
the current state so it can be re-injected to resume from the same
point.

For the immutable case, we might try adding an extraction function to
the Unfold type:

```haskell
data ReUnfold m a b =
    -- ReUnfold step inject extract
    forall s. ReUnfold (Producer s b) (a -> m s) (s -> m a)
```

However, `extract :: s -> m a` maps the internal state back to the
seed type `a`, which requires `a` to be capable of representing every
intermediate state the machine can reach. For example, composing two
Unfolds with `unfoldEach` produces a nested loop whose intermediate
states are:

```haskell
data NestedLoop s1 s2 = OuterLoop s1 | InnerLoop s1 s2

unfoldEach :: Monad m =>
    ReUnfold m a b -> ReUnfold m b c -> ReUnfold m (NestedLoop a b) c
```

The seed of the composed Unfold must grow to `NestedLoop a b` to
represent any resumable position. In general, `a` must equal `s`,
making the existential hiding of `s` vacuous. The internal state and
the seed collapse into the same thing -- which is exactly the original
`Producer` type. **`Producer` is already the resumable Unfold, with
fully transparent state.**

The `Stream` type, on the other hand, bundles the step function together
with its current internal state, so the state never needs to escape to
the caller. This is what makes `uncons` and `foldBreak` possible --
a stream can be paused and resumed without exposing any intermediate
state. The cost is that the hidden state prevents the inter-producer
loop fusion that Unfold's transparent state enables.

A Stream is a complete state machine whereas an Unfold is just the
processing function of the state machine without the state.

## Unfold vs Stream Usage

We should not use "unfold" to generate stream, rather we should use
stream functions directly. Streams use pure state and do not need
simplification of the additional state complications introduced by the
monadic injection function. "unfold" should be rarely used. Similary, we
should not use functions like "fromStream" create unfolds.  They uncons
the stream which is inherently non-fusible operation.

## Unfold vs Stream Usage

Prefer direct stream combinators over `unfold` for generating
streams. An Unfold carries an explicit monadic injection step that
adds complexity without benefit when an equivalent stream generation
combinator already exists.

Similarly, avoid `fromStream` and related functions that construct an
Unfold by unconsing a Stream. Unconsing is inherently non-fusible: it
breaks the loop structure that makes Unfold composition efficient. If
you find yourself reaching for `fromStream`, the right tool is likely a
stream combinator instead.

As a rule of thumb: Use Unfold when flattening nested streams; use Stream
combinators to construct and transform streams directly.Use Unfold when
flattening nested streams; use Stream combinators to construct and
transform streams directly.

## Type Summary

```
data Outcome s b = Yield b s | Skip s | Stop
data Producer a m b = Producer (a -> m (Outcome a b))
data Unfold m a b = forall s. Unfold (Producer m s b) (a -> m s)
data Stream m b = forall s. Stream (Producer m s b) s
```

## Naming

Other alternatives for Yield, Emit.
Alternatives for Outcome, StepResult.

## Stream Configuration

The implicit state parameter in the Direct and CPS stream
definitions can be used to pass the implicit stream pipeline level
configuration. For example, this could be a global limit on maxThreads
in a pipeline. The config can be specified using config modifier combinators
within the pipeline in the same way as we did in pre-0.9 .

If we explicitly specify an output (streams) or input channel (folds)
via the config we can even reuse that instead of automatic allocation,
if it is not specified in the stream config then we can auto allocate
the channel if it is a parallel combinator. This way we can implement
n-ary stream combining combinators as well instead of binary only.  This
way parallel combinators need not even have a config argument as config
can be specified via in-pipeline combinators for downstream components.
