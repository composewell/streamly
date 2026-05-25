## Related docs

See the state machines doc.

# Transition Functions

The simplest transducer is a Mealy style scan without a terminal state
emission or we can say Moore without an initial state emission:
```
transduce :: (s -> a -> m (s, b)) -> m s -> Stream m a -> Stream m b
```

With termination and output filtering support:
```
data Outcome s b = Yield b s | Skip s | Stop
data Transition s a m b = Transition (s -> a -> m (Outcome s b))
-- instance MonadTrans (Transition s a)
```

We should have both SkipInput and SkipOutout. Alternatively it can be supported
by Maybe type input and output. The catMaybes can be used to strip the Maybe to
get a normal type on the surface.

Should we name the result on the function e.g. TransitionResult, ProducerResult
etc?

## OpenScan (Simplest Transducer)

Reification of the above transduce function as a data type with termination
support and open injectable state.
```
data OpenScan m c a b = forall s. OpenScan (Transition s a m b) (c -> m s)
```

Equivalently instead of using the Skip constructor we can use a `Maybe b` type
instead if we want to filter outputs (consumer mode). And we can use `Maybe a`
type if we want to allow the machine to be cranked without any input (producer
mode).

Injection is monadic to keep lmapM fusible. See Stream docs for details.

## Scan

Scan is a OpenScan with a closed state:
```
data Scan m a b = forall s. Scan (Transition s a m b) s
newtype ScanT a m b = ScanT (Scan m a b)
```

In a scan the driver cranks the machine and decides whether to supply an input
or crank without one. On each crank the machine decides whether to produce an
output or pass without one. So the driver controls the input side and the
machine controls the output side, but neither can seize control of the cranking
rhythm: the machine cannot enter a producer phase where it forces the driver to
keep cranking until the machine signals it is ready to accept input again.
(That producer phase is what distinguishes a Pipe.)

## Scanl (Moore and Mealy Machines)

### Moore Style Scan

foldl and scanl in Streamly are Moore machine style representations:
```
foldl :: (b -> a -> b) -> b -> Stream m a -> m b
scanl :: (s -> a -> s) -> s -> Stream m a -> Stream m s
```

A Moore machine emits the states of the machine, the output is solely a
function of the state. scanl emits the initial value of the accumulator
followed by one value per input, the next state of the machine. If the input
stream size is n the output stream size is n + 1.

The Scanl type in streamly is a data representation of strict left
scan. The Fold type is a data representation of strict left fold.

### Mealy Style mapAccum

mapAccum is a Mealy machine style representation:
```
mapAccum :: (s -> a -> (s, b)) -> s -> Stream m a -> Stream m b
mapAccumM :: (s -> a -> m (s, b)) -> m s -> Stream m a -> Stream m b
mapAccumEnd :: (s -> a -> (s, b)) -> s -> (s -> b) -> Stream m a -> Stream m b
mapAccumEndM :: (s -> a -> m (s, b)) -> m s -> (s -> m b) -> Stream m a -> Stream m b
```

Here, the state and the output are separate, each input generates the next
state and an output. Notice, the step function has the same shape as mapAccumL,
but mapAccumL returns the final accumulator separately while the mapAccumEnd
implementation above emits a final value into the stream. mapAccum emits values
only on inputs, it does not emit an initial value.  However, the extraction
function (s -> b) in mapAccumEnd generates an additional terminal value.  While
in Moore we have an additional initial value, in Mealy we have an additional
final value.

`mapAccumEnd` emits one output per input (from the step's b), followed by one
terminal value (from extract).  If the input stream size is n the output
stream size is n + 1.

mapAccumEnd (Mealy) and scanl (Moore) can be interconverted.

### Implementation Optimizations (Scan Type)

We can write a variant of `scanl` that does not emit the initial value
(postscan), and similarly a variant of scan that does not emit the terminal
value. Use cases where we do not require these additional values can benefit
from these more efficient simpler variants.

In a scanl, the driver must emit the initial value before processing any
input.  Since this cannot be folded into the per-input step, the driver
needs a distinct pre-input state to run the initialization effect and
transition into the normal wait-for-input loop. That state remains in
the dispatch path for the lifetime of the stream.

In postscan, the initial value is not emitted, so this extra state can
usually be eliminated. However, a monadic initial value still requires a
separate initialization state to execute the effect exactly once before
entering the main state machine.

Here is a snippet from real postscan code:

```
    step _ (ScanInit st) = do
        res <- initial
        return
            $ case res of
                  Partial fs -> Skip $ ScanDo st fs
                  Done b -> Yield b ScanDone
    step gst (ScanDo st fs) = do
        res <- sstep st
        case res of
            Yield x s -> do
    ...
```

However, when the state is pure, GHC fusion optimizations can
potentially eliminate the initial state, if it can happen reliably we do
not need a separate postscan type for this simplification. In several
cases we have actually seen fusion issues or inefficiencies due to this
initial state where it is not required.

The Mealy side is symmetric at the other end. In a scan without terminal
state extraction and with pure finalizer we do not need a separate
state. However, a monadic finalizer forces a post-input state even
without terminal state extraction.

A Moore machine without the initial value and a Mealy machine without
the terminal value are the simplest to implement and the most reliably
fused. This is basically the `Scan` type we discussed earlier.

### Scanl Type

Both Moore and Mealy can represent transformations as well as consumers.
Operations like map and filter where only transitions are important, and
empty stream does not require any special processing are efficiently
expressible by Mealy style without extraction or Moore without an
initial step. On the other hand operations like sum where empty stream
sums to 0 are more convenient in Moore style or Mealy with a final
extraction step.

Streamly's Scanl type is Moore style, it emits an initial value extracted from
the initial state. The extract function extracts the output from the state, it
can be called after every `Partial` step to get the output. It also has a
finalizer action, which is called only when the scan is terminated externally
rather than completing on its own (via Done). In that case `final` can be used
to perform any resource finalization, it does not return any output, just an
effect, and should be called only once. It supports resource bracketing by
resource allocation in initial state and release in the final.  However always
emitting a  value in the initial step can make implementation of operations
like map and filter more complicated than they need to be. Separate types can
be used for different use cases which would be less ergonomic but can be
optimized better.

```
data Step s b = Partial !s | Done !b

data Scanl m a b =
  -- | @Scanl@ @step@ @initial@ @extract@ @final@
  forall s. Scanl (s -> a -> m (Step s b)) (m (Step s b)) (s -> m b) (s -> m ())
```

We require the general Scanl type for resource bracketing.  For fusion
optimizations, a postscan (no initial emission) type may be helpful for the
case when no resource bracketing is required.

## concatScanlM and concatMapAccumM

The Mealy formulation generalizes to concatMapAccum, without a final state
emission:

    concatMapAccumM :: (s -> a -> m (s, Stream m b)) -> m s -> Stream m a -> Stream m b

This is essentially a nested state machine, a state machine composed of many
state machines.

The Moore style formulation of the same would require an extract function, the
extract function is called once per input as the state advances:

    concatScanlM :: (s -> a -> m s) -> s -> (s -> Stream m b) -> Stream m a -> Stream m b

This represents a uni-directional pipe, we can represent it using a data type
UniPipe.

## Pipes

Pipe: Arbitrary consumption and producer modes

unfoldAccum/concatMapAccum is a restricted Pipe: it has the fixed
rhythm of consuming one input and unfolding/concatenating its
sub-stream, whereas a Pipe can interleave consumption and production
arbitrarily. (Note: a concatMapAccum whose sub-streams are opaque
Streams cannot be expressed in the fused Pipe representation, because
the sub-stream's existential state cannot be merged; the fused form
corresponds to sub-streams produced by Unfold.)

## Scan Variants

scanl, prescanl, postscanl:
scanl, provides the initial value of the accumulator and the next value
on each input. postscanl drops the initial value. prescanl drops the
final value.

scan is similar to postscanl in that it does not (and cannot) emit an
initial value.

There are strict variants of these like scanl', monadic variants like scanlM',
variants where we use the first input as accumulator like scanl1' .

## Scan and Fold Modules

* The core scan module representing a stateful transformation or state
  machine without emitting an initial or terminal value is to be named as
  `Scan`.
* The `Scanl` type and module is a further specialization of the `Scan`
  type which adds an emission of an initial value to the state machine.
* The Fold type is exactly the same as the `Scanl` type except that it
  does not emit any intermediate values.
* Since there is no Foldr, the fold module was named Fold. Ideally it can be
  called `Foldl` to mirror the `Scanl` naming. But we are not going to change
  it now.

## Where the runners live

The scan runner for Stream lives in the Stream module, for fold in the Fold
module, for unfold in the Unfold module and so on. If we put the runners in the
scan module then we will need name disambiguation like scanStream, scanFold,
scanScan, scanUnfold etc.

## Scan Naming in Data.Stream

Scanl type runners:

    Stream.scanl: running a Scanl type
    Stream.postscanl: running a Scanl type dropping the initial value
        (equivalent to Scanl.toScan followed by Stream.scan)

Note these are the Stream runners and remain unchanged. Separately, in the
Scanl module the combinator to scan the input of a scan (the operation
currently named `scanl`, not the runner) is to be renamed to `compose` as it
composes two scans together to create a single scan by feeding the input from
one to the other, it chains them. This is similar to how we compose the `Scan`
type as well as the `Pipe` type. It makes sense to have a special name for this
operation rather than using `scanl`. The restart-on-termination variant
`scanlMany` is renamed to `composeMany` accordingly, so that the `scanl` root
is fully freed. And `scanlMaybe` to `composeMaybe`.

* Scanl.scanl -> compose
* Scanl.scanlMany -> composeMany
* Scanl.postscanl -> postcompose
* Scanl.postscanlMany -> postcomposeMany

Directly running a scan using a step function
    Stream.scanl'
    Stream.postscanl'
    ...

The proposal is to not expose these and just suggest using the Scanl
constructors, for the following reasons, (1) there is no performance advantage,
(2) we are giving too many options to the user, (3) the names create
confusion with the Scanl runners, (4) if we have these in Data.Stream then why
not in Data.Scanl, Data.Unfold and Data.Fold as well, (5) we have Scanl
constructors with exact same names as the Data.List APIs therefore
discoverability of traditional APIs should not be a problem.

Directly running a mapAccum using a step function

    Stream.mapAccum(M)

The reified type for mapAccum is the `Scan` type. Similar to the above
operations we should not expose these direct operations, rather we should use
the `Scan` type to run these using the `scan` runner. The `Scan` type can have
constructors named `mapAccum` and variants of that, also constructors named
`postscanl'` and variants make sense for the `Scan` type.

Directly running a concatMapAccum using a step function

    Stream.unfoldAccum(M)
    Stream.concatMapAccum(M)

These are stateful 1-to-n transformations. unfoldAccum is the fused version and
concatMapAccum is the unfused version because of the existential type in the
Stream. The reified type that can represent unfoldAccum is the `Pipe` type.
However, the Pipe type is more general because it can do n-to-1
transformations and 1-to-n as well i.e. it can fold and produce. There is no
reified type corresponding to concatMapAccum.

Scanl constructors:

    Scanl.mkScanl
    Scanl.mkScanl1
    ...

The proposal is to change these names to "scanl'", "scanl1'" etc for the
following reasons, (1) consistency with the Fold module constructors, (2) once
we rename the `scanl` composition operation to `compose`, the `scanl` root is
free, so "scanl'" is no longer in conflict with any operation and can be used
as a constructor.

Another alternative is to change the Fold constructor names adding the prefix
mk, reasons against doing that, (1) wider deprecation change, (2) the change is
not required otherwise because in the Fold module there is no "fold/foldl"
runner so there is no naming confusion, (3) these names maintain
discoverability of the replacements of traditional fold functions from
Data.List.

## Scan Type

The `Scan` type is for representing a stateful transformation or state
machine without emitting an initial or terminal value. This can be thought of
as the mapAccum type reified.

Scan type runners:

    Stream.scan: running a Scan type

Scan type constructors:
    Scan.postscanl': constructor using Moore style step function
    Scan.postscanlM': monadic version of postscanl'
    Scan.mapAccum: constructor using Mealy style step function
    Scan.mapAccumM: monadic version of mapAccum

Scan type adapters:
    Scanl.toScan: convert Scanl to Scan
    Scanl.fromScan: convert Scan to Scanl
    Fold.fromScan: convert Scan to Fold

## Constructor Naming in Data.Scanl

Note, currently we cannot use the name "scanl" as a constructor
as we have an operation of the same name in the same module for
scanning the input of a scan. So the "mk" prefix makes the distinction.
Also prime is not necessary in constructor names as the scan is always
strict, there is no choice of strict vs lazy.

However, once we rename that composition operation from "scanl" to "compose"
(as proposed above), the "scanl" root is no longer used by any operation, so we
can drop the mk prefix and use the exact same names as the Data.List scan
operations. That way it will become consistent with the Fold module and the
names will be discoverable and familiar.

## TODO

* Formulate the Scanl/Fold type using the Scan type.

* Formulate the Step type as `Partial s b` so that we do not need the extract
  function. We won't have to thread around the `b` anymore as we do not need to
  return it in `final`.

* Change the final function in Scanl constructor to not return output type.
  This makes sense in folds but in scans a single element generation at the end
  is half-hearted solution. Either we should have the ability to generate a
  stream in the beginning as well as in the end or nothing. The initial as well
  as final operations should be of Step type. In addition, we should be able to
  specify these as unfolds which we can internally adapt to the Step type.
  This will make all the drivers complicated though especially the combinators.
  Maybe we can just return the singleton residual state as the last
  element and then externally unfold the output of the scan into a
  stream.

  For supporting something like scanlMAfter' we may need to emit the final state
  during extract. We may have to return `Maybe b` in extract or `Stream m b`.
  The `final` function has to work on the state such that it cannot return the
  last emitted value, it will always return the values after the last
  emitted value that may have to be drained.

* scanlMAfter' can be implemented by something like modifyLast after generating
  the final state value in a Moore machine. We can also generate a stream in
  the end using unfoldLast.

* Monadic inject in Unfold forces a separate init state when converting to a
  stream. We can remove the monadic inject or create a pure type or ensure that
  the additional state fuses in the most efficient way.

* Decide on names of scant/foldt -- mkScanl, mkFold might be better
  After that stop exporting the Fold and Scanl constructors so that we can
  change it freely.

*  The Step type maybe different in Producer and Transition. Similarly,
   will the Step type be different for Scanl? In that case we may need such a
   function type for Scanl as well. For Scanl and Fold it would be the same.

* Downgrade the constraints in Unfold/Stream/Fold/Scan/Scanl where possible.

* Transition (transducer) function specializations -- Emitter/Producer,
  Collector/Consumer.
