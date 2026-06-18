## Consumer Functions

The simplest consumer:
```
consume :: (s -> a -> m s) -> (s -> b) -> m s -> Stream m a -> m b
```

With termination support:
```
data Outcome s = Yield s | Stop
data Consumer s a m b =
    Consumer
        (s -> a -> m (Outcome s b))
        (s -> b) -- final extract
```

With input skip support:
```
data Outcome s = Yield s | Skip s | Stop
data Consumer s a m b =
    Consumer
        (s -> a -> m (Outcome s b)) -- Yield continuation
        (s -> m (Outcome s b)) -- Skip continuation
        (s -> b) -- final extract
```

Another alternative is the driver can pass "Maybe a" in the input of the
continuation. But that has a problem, the driver can pass Nothing or Maybe a
arbitrarily without any check. Even using separate continuations have the same
problem. Returning an inject function solves that problem. the driver is forced
to use the inject function or pass the state as is.
```
data Outcome s = Yield s | Skip s | Stop
data Consumer s a m b =
    Consumer
        (s -> Maybe a -> m (Outcome s b)) -- Yield continuation
        (s -> b) -- final extract
```

Another possiblity is to use two separate state types, "s1" and "s2". When it
skips input it will return "s2", therefore, you have to call skip continuation,
there is no other choice.

Instead of using a separate continuation we can return an inject function
instead (a -> s).

Thus the type becomes:
```
data Outcome s a b = Yield (a -> s) | Skip s | Stop b
data Consumer s a m b =
    Consumer
        (s -> m (Outcome s a b))
        (s -> m b) -- final extract, when forced to stop
```

The initial state type can be pure "s". In that case the driver will
have to always start without an input, if the consumer requires an input then
it will immediately ask for input. This is sort of an initialization state.

An alternative design is to use an initial continuation of type "m (Outcome s a
b)". This is what we do today. We need the "Outcome" type rather than simple
"s" because the consumer may stop immediately or may skip the input. The
initial action can be executed in the underlying monad, like in case of unfold.
What are the advantages/disadvantages of these two alternatives? Will adding an
initial state in each fold be more overhead because most of the time folds will
start with an input, so they will need an empty skip state in the beginning?

Also, if we do not have the input skipping functionality e.g. in the simplest
Fold type, then we need a separate "initial" step anyway so that it can return
"Stop b" even without an input.

We do not need a Skip loop when we extract? therefore the extract type is
simple "s -> m b". Otherwise we will need it to return "Skip s | Stop b".

if we yield "a -> s" will it be able to fuse? This is a dynamic function as it
has the current state of the fold baked in.

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
