## Consumer Functions

## Folds

Folds are consumers that consume a stream and produce a single final result.
These are the simplest of the consumers.
```
consume :: (s -> a -> s) -> (s -> b) -> s -> [a] -> b
consumeM :: (s -> a -> m s) -> (s -> b) -> s -> Stream m a -> m b
```

## Reified Consumer

data Consumer a m s =
    Consumer (s -> a -> m s)

## Consumer with abstract state

data Refold s a m b =
    Refold
        (s -> a -> m s) -- step
        s -- initial
        (s -> b) -- final extract

## Fold

A Moore style state machine where output is a function of state.

data Fold m a b =
    Fold
        (s -> a -> m s) -- step
        s -- initial
        (s -> b) -- final extract

Add rmapM support requires a monadic extract:
data Fold m a b =
    Fold
        (s -> a -> m s)
        s
        (s -> m b)

With local resource allocation or monadic initialization (e.g. time dependent
initial state or initial state derived from network or DB):
data Fold m a b =
    Fold
        (s -> a -> m s)
        (m s)
        (s -> m b)

The final extraction function doubles up as resource finalization function as
well, therefore, it should be called only once at the end.

## Consumers with termination

With termination support (e.g. take 5):
```
data Step s = Partial s | Done s

data Fold m a b =
    Fold
        (s -> a -> m (Step s))
        (m s)
        (s -> m b)
```

With termination in initial (e.g. take 0):
```
data Fold m a b =
    Fold
        (s -> a -> m (Step s))
        (m (Step s))
        (s -> m b)
```

## Auto finish vs input end

There are two ways in which a fold can finish:
* It decided to stop by itself while the input is still remaining
* The input stream ended and we now need the fold to finish up

First case the fold returns "Done s", and in the second case we need to call
the "final" function to extract the value.

We can simplify the type to make it safer, so that we do not return the state
if the fold returns "Done":
```
data Step s b = Partial s | Done b

data Fold m a b =
    Fold
        (s -> a -> m (Step s b))
        (m (Step s b))
        (s -> m b)
```

Now if the fold returns "Done b" it has to ensure that any resource
finalization is done because the driver has no way to do that. If the
stream ends any resources are finalized by the final extract function.

## Why not Mealy style?

Why not yield output at each step?
```
data Step s b = Partial s b | Done b
```

## Input Skipping Support

Combinators like unfoldEach on the input of a fold cannot be supported without
input skipping. Draining at the end of a fold can be supported without input
skipping but draining at the end of a scan requires input skipping.

Input skipping essentially supports a local loop inside a fold without
consuming input and with fusion. This introduces a producer phase inside the
fold.

Input skipping makes the type more complex and this feature therefore comes
under the pipe type. See the pipe design doc for this.

## Fold as Scan

See the scan design doc.

Note filtering can be supported by the fold type as it is. However, if we use
the fold as a scan by adding a per input extract function, then we encounter a
problem due to filtering and we need a richer type for proper filtering.
Therefore, we need to lift folds into a richer type for scanning.

### Stream vs Fold Step Types

NOTE: Extract + Partial == Yield. The most crucial difference in Stream.Step
and Fold.Step is that the producer terminal carries no value (Stop) while the
consumer terminal does (Done b). That's the covariant/contravariant polarity
showing up structurally -- it lives entirely in the terminal constructor.

NOTE: To distinguish the difference in types we use Yield, Skip, Stop for
Stream and Partial, Continue and Done for folds.

Note: Continue and Skip correspond to each other. Continue says there is no
output in this turn, do not extract. Similarly, Skip says there is no output to
feed to the next computation, just loop around. However, there is one glitch,
the "do not extract" is not enforced by types, the driver may still try to
extract. We can use "Partial s b" to solve this issue but that causes
performance issues.

## Why `Fold m` and `Unfold m` do not have `Profunctor` instances

`lmap` and `rmap` can be defined for both `Fold m` and `Unfold m`, and
`Unfold m` can additionally support `Strong` and `Choice`. However, a
`Profunctor` typeclass instance is only useful when `p` is abstract —
i.e. when writing code polymorphic over any `Profunctor p`. The main
payoff is profunctor optics, where types like `Lens` and `Prism` are
quantified over `p`:

    type Lens s t a b = forall p. Strong p => p a b -> p s t

For `Fold` to participate meaningfully it would need a `Strong` instance:

    first' :: Fold m a b -> Fold m (a, c) (b, c)

This has no coherent implementation. A fold collapses a stream of `(a, c)`
pairs into a single `b`, leaving no canonical `c` to pair with the result.
`Strong` assumes a one-to-one input/output relationship that `Fold`, as a
many-to-one consumer, cannot satisfy. Without `Strong`, `Fold` only reaches
`Iso`-level optics, which reduce to `lmap` and `rmap` anyway.

`Unfold m` fares better: being a one-to-many producer, the `c` in `(a, c)`
can be threaded through the step state and paired with every emitted element,
making `Strong` coherent. But even here, the optics machinery reduces to
`lmap (view lens)` at the call site — no more concise or general than calling
`lmap` directly. The main consumers of polymorphic profunctor code are optics
libraries built around `(->)` and `Star`, not stream processors, so the
scenario of passing `Unfold` as an abstract `p` rarely arises in practice.

The instances are therefore omitted to avoid implying an integration with
profunctor optics that does not exist in practice.
