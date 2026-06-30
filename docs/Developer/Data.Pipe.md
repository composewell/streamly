# Uni-directional pipes

### Uni-directional vs Bi-directional Pipe

Note: a bidirectional pipe cannot fuse, so there is no fusible version,
a continuation or free-monadic version is possible.  We have no
plan to provide one: the added complexity and runtime cost are not
justified, and equivalent programs can be expressed more easily and more
efficiently with simpler, fusible abstractions such as Unfold, Stream,
Scanl, and Fold -- extending up to a uni-directional Pipe.

Why it cannot fuse? In short, you need to dynamically interleave two
independent flows.  A stream is a step function over an explicit state
(s -> m (Step s a)), and a pipeline fuses because GHC inlines and
statically composes those steppers into one loop with the combined state
as a product, no intermediate constructors. That requires the data flow
to be statically resolvable in one direction. A bidirectional pipe has
a data-dependent feedback loop -- at each point, whether control passes
upstream or downstream depends on a runtime value (a request vs. a
respond). You can't collapse that into a single static loop; you need
dynamic dispatch on the direction, which forces a CPS or free-monad
encoding, which reintroduces exactly the heap-allocated constructors and
closures that fusion exists to eliminate.

## Input Skipping in Folds

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

## Generalizing StreamScan to a pipe

## Generalizing a FoldScan to a pipe

## Generalized Transducer

With input/output termination and input/output filtering support:
```
-- There are overall 5 outcomes,
-- [Skip input, ask input] x [Skip output, yield output] and Stop.
-- We can either use flat constructors or nested -- which is better for fusion?
-- Flat is better if the code does not fuse, one less indirection.
-- data Outcome s b = SkipInput (Maybe b) s | AskInput (Maybe b) s | Stop
data Outcome s b =
      YieldAwait b s
    | Yield b s
    | Await s
    | Skip s
    | ProducerDone
    | ConsumerDone b
data Transducer s a m b =
    Transducer
        (s -> a -> m (Outcome s b)) -- AskInput/await continuation
        (s -> m (Outcome s b)) -- SkipInput/yield continuation
-- instance MonadTrans (Transducer s a)
```

By removing input skip we can have a 3 constructor type and similarly by
removing output skip. We can possibly use these simpler types and lift them to
the more complex type using an adaptor. Similarly we can have an even simpler
type by removing both input and output skip -- this is our current Fold/Scanl
type. Simpler types can be automatically converted to more complex types, but
vice-versa cannot be done as that will lose information. Thus we need to use
the more complex type only if required.

We can make the type simpler by using a "Maybe b" output type and a
"Maybe a" input type always. But that means all streams will have an
explicit Maybe output and all consumers will have an explicit Maybe
input which is not very ergonomic. The abstraction is supposed to hide
this complexity.

Should we name the result on the function e.g. TransitionResult,
ProducerResult etc for distinct outcome types?

Since we have Producer and Consumer as special cases of this, the name
Transducer makes better sense than Transition.

## OpenScan

Reification of the above transduce function as a data type with termination
support and open injectable state.
```
data OpenScan m c r a b = forall s. OpenScan (Transducer s r a m b) (c -> m s)
```

Equivalently instead of using the Skip constructor we can use a `Maybe b` type
instead if we want to filter outputs (consumer mode). And we can use `Maybe a`
type if we want to allow the machine to be cranked without any input (producer
mode).

Injection is monadic to keep lmapM fusible. See Stream docs for details.

## Scan

Scan is a specialization of OpenScan where the state is closed:
```
data Scan m r a b = forall s. Scan (Transducer s r a m b) s
newtype ScanT r a m b = ScanT (Scan m r a b)
```

In a scan the driver cranks the machine and decides whether to supply an input
or crank without one. On each crank the machine decides whether to produce an
output or pass without one. So the driver controls the input side and the
machine controls the output side, but neither can seize control of the cranking
rhythm: the machine cannot enter a producer phase where it forces the driver to
keep cranking until the machine signals it is ready to accept input again.
(That producer phase is what distinguishes a Pipe.)

The type will be equivalent to:
```
data Final s b =
      FinalYield b s
    | FinalSkip s
    | FinalStop
    | FinalDone b

data Scan m a b = forall s.
    Scan
        (m (Outcome s b)) -- initial
        (s -> m (Outcome s b)) -- yield
        (s -> a -> m (Outcome s b)) -- await
        (s -> m (Final s b)) -- final
```

Or we can use a pure initial `s` and start with yield? But we need to
distinguish initialization from intermediate producer phase.

## Pipes

Pipe: Arbitrary consumption and producer modes

unfoldAccum/concatMapAccum is a restricted Pipe: it has the fixed
rhythm of consuming one input and unfolding/concatenating its
sub-stream, whereas a Pipe can interleave consumption and production
arbitrarily. (Note: a concatMapAccum whose sub-streams are opaque
Streams cannot be expressed in the fused Pipe representation, because
the sub-stream's existential state cannot be merged; the fused form
corresponds to sub-streams produced by Unfold.)

