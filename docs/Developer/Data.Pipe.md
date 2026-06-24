# Uni-directional pipes

## Generalizing MapAccum to a transforming-pipe

## Generalizing a Scanl to a reducing-pipe

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

