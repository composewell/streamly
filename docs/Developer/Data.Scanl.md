## Scans as Moore and Mealy Machines

foldl and scanl in Streamly are Moore machine style representations:

    foldl :: (b -> a -> b) -> b -> Stream m a -> m b
    scanl :: (s -> a -> s) -> s -> Stream m a -> Stream m s

A Moore machine emits the states of the machine, the output is solely a
function of the state. scanl emits the initial value of the accumulator
followed by one value per input, the next state of the machine. If the input
stream size is n the output stream size is n + 1.

The Scanl type in streamly is a data representation of strict left
scan. The Fold type is a data representation of strict left fold.

mapAccum is a Mealy machine style representation:

    mapAccum :: (s -> a -> (s, b)) -> s -> Stream m a -> Stream m b
    mapAccumM :: (s -> a -> m (s, b)) -> m s -> Stream m a -> Stream m b
    mapAccumEnd :: (s -> a -> (s, b)) -> s -> (s -> b) -> Stream m a -> Stream m b
    mapAccumEndM :: (s -> a -> m (s, b)) -> m s -> (s -> m b) -> Stream m a -> Stream m b

Here, the state and the output are separate, each input generates the next
state and an output. Notice, the step function has the same shape as mapAccumL,
but mapAccumL returns the final accumulator separately while scan emits a final
value into the stream. scan emits values only on inputs, it does not emit an
initial value.  However, the extraction function (s -> b) generates an
additional terminal value.  While in Moore we have an additional initial value,
in Mealy we have an additional final value.

`scan` emits one output per input (from the step's b), followed by one
terminal value (from extract).  If the input stream size is n the output
stream size is n + 1.

scan (Mealy) and scanl (Moore) can be interconverted.

## Moore vs Mealy Machine

Moore:   observes states
Mealy:   observes transitions

Moore naturally exposes an initial observation.
Mealy naturally supports terminal extraction.

Moore and Mealy both can represent stream transformations as well as folds
equally well. The Mealy formulation is usually more straightforward to think as
an explicit state machine. We have an initial state, each input causes a state
transition and an output.

The simplest to think about is a Mealy style scan without extraction:

```
    mapAccum :: (s -> a -> (s, b)) -> s -> Stream m a -> Stream m b
```

## Implementation Optimizations

We can write a variant of `scanl` that does not emit the initial
value (postscan), and similarly a variant of scan that does not emit
the terminal value. Use cases where we do not require
these additional values can benefit from these more efficient simpler
variants.

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
fused.

## Scanl Type

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

## concatScanM and concatMapAccumM

The Mealy formulation generalizes to concatMapAccum, without a final state
emission:

    concatMapAccumM :: (s -> a -> m (s, Stream m b)) -> m s -> Stream m a -> Stream m b

This is essentially a nested state machine, a state machine composed of many
state machines.

The Moore style formulation of the same would require an extract function, the
extract function is called once per input as the state advances:

    concatScanM :: (s -> a -> m s) -> s -> (s -> Stream m b) -> Stream m a -> Stream m b

This represents a uni-directional pipe, we can represent it using a data type
UniPipe.

## Scan Types

Fold    = terminal observation
Scanl   = Moore observation stream
mapAccum = Mealy transition stream
concatScan = Moore style nested state machine
concatMapAccum = Mealy style nested state machine

## Scan Variants

scanl, prescanl, postscanl:
scanl, provides the initial value of the accumulator and the next value
on each input. postscanl drops the initial value. prescanl drops the
final value.

scan is similar to postscanl in that it does not (and cannot) emit an
initial value.

There are strict variants of these like scanl', monadic variants like scanlM',
variants where we use the first input as accumulator like scanl1' .

## Scanl Module Name

* Since there is no Foldr, the fold module was named Fold.
* Currently there is an inconsistency because fold module is called `Fold` and
  the scan module is called `Scanl`. Either the fold module should be `Foldl`
  or the scan module should be `Scan`.
* There is not going to be a Scanr as well so the scan module could have been
  named just `Scan`, but it was probably a consistency mistake to not name it
  that.

We could rename it now or live with it. Let's live with it, not a big deal.

## Where the runners live

The scan runner for Stream lives in the Stream module, for fold in the Fold
module, for unfold in the Unfold module and so on. If we put the runners in the
scan module then we will need name disambiguation like scanStream, scanFold,
scanScan, scanUnfold etc.

## Scan Naming in Data.Stream

Scanl type runners:

    Stream.scanl: running a Scanl type
    Stream.postscanl: running a Scanl type dropping the initial value

The proposal is to change `scanl` to `scan`, to disambiguate it a little more
than single character difference from the constructor "scanl'" in the Scanl
module. Note that this will also require a change in Fold, Scanl, Unfold
modules. And across all variants like scanlMany, scanlMaybe etc.

We keep `postscanl` as it is for future introduction of a `Postscan` type. If
we introduce that then we will have to use the `postscan` name for that, and
for running Scanl as postscan we can use a `toPostscan` converter for
converting from Scanl to Postscan and then run it using `postscan`. In that
case we can retire the `postscanl` variants.

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

Scanl constructors:

    Scanl.mkScanl
    Scanl.mkScanl1
    ...

The proposal is to change these names to "scanl'", "scanl1'" etc for the
following reasons, (1) consistency with the Fold module constructors, (2) once
we change the runner `scanl` to `scan`, the `scanl` root is free, so "scanl'"
is no longer in conflict with any operation and can be used as a constructor.

Another alternative is to change the Fold constructor names adding the prefix
mk, reasons against doing that, (1) wider deprecation change, (2) in the Fold
module there is no "fold/foldl" runner so there is no naming confusion, (3)
these names maintain discoverability of the replacements of traditional fold
functions from Data.List.

Directly running a mapAccum using a step function

    Stream.mapAccum(M)
    Stream.concatMapAccum(M)

There is no reified type for mapAccum.

## Postscan Type

This type does not exist as of now, but there is a possibility that we might
add it for performance reasons.

Postscan type (separate type) runners and constructors:

    Stream.postscan: running a Postscan type
    Postscan.postscanl': constructor using a step function
    Scanl.toPostscan: convert Scanl to Postscan and run using postscan

## Constructor Naming in Data.Scanl

Note, currently we cannot use the name "scanl" as a constructor
as we have an operation of the same name in the same module for
scanning the input of a scan. So the "mk" prefix makes the distinction.
Also prime is not necessary in constructor names as the scan is always
strict, there is no choice of strict vs lazy.

However, once we rename that operation from "scanl" to "scan" (as proposed
above), the "scanl" root is no longer used by any operation, so we can drop the
mk prefix and use the exact same names as the Data.List scan operations. That
way it will become consistent with the Fold module and the names will be
discoverable and familiar.

## State Machines: At a Glance

* Stream consumers, producers, and transformations can be represented
  as state machines.
* A state machine transition is represented by a function.
* iterate (basic producer step): f :: s -> s
* Builder (foldr style) step (consumer or transformation): f :: a -> b -> b
* Accumulator (foldl style) step (consumer or transformation): f :: b -> a -> b
* Moore step: f :: s -> a -> s, state transformation, output implicit in state
* Mealy step: f :: s -> a -> (s, b), state transformation with explicit output

* Moore style stream producer (iterate)
  * iterate can be viewed as a degenerate Moore machine with no external input.
  * Moore style producer step (iterate): f :: s -> s
  * Moore style stream producer (iterate): f :: (s -> s) -> s -> Stream m s
  * Endo represents the transition algebra used by iterate.

* Moore style stream consumer (iterate)
  * When input ~ state, append becomes a degenerate Moore transition.
  * Moore style step (append): f :: s -> s -> s
  * Moore style transformation: f :: (s -> s -> s) -> s -> Stream m s -> Stream m s
  * Moore style consumer (fold): f :: (s -> s -> s) -> s -> Stream m s -> s
  * Semigroup can be used to fold by additionally supplying an initial value
  * Monoid can be used to fold without supplying an additional initial value (e.g. Sum)

* Mealy style stream producer (unfold)
  * unfold can be viewed as a degenerate Mealy machine with no input.
  * Unfold step: f :: s -> (s, b), Mealy with no input
  * Unfold: Mealy machine with no input, explicit state and termination
  * unfold: f :: (s -> (s, b)) -> s -> Stream m b
  * The data representation is Unfold
  * A stream can be represented as a Mealy machine with no input,
    using existential state and termination.
  * The streamly representation is `Stream` type.

* Moore stream transducer (scan):
  * treating `s` as input in iterate style fold is restrictive
  * it can only do `Stream m s -> Stream m s` transformation
  * so use a separate input
  * output derived from state after transition
  * scan using the same state and output type:
  * f :: (s -> a -> s) -> s -> Stream m a -> Stream m s
  * scan using a separate state and output type:
  * f :: (s -> a -> s) -> s -> (s -> b) -> Stream m a -> Stream m b

* Mealy stream transducer (mapAccum):
  * treating `s` as output is restrictive
  * So split the state and output
  * output produced by the transition itself 
  * mapAccum: Mealy machine with existential state and termination
  * Scan step: f :: s -> a -> (s, b)
  * Scan without final extract:
    * f :: (s -> a -> (s, b)) -> s -> Stream m a -> Stream m b
  * Scan with final extract:
    * f :: (s -> a -> (s, b)) -> s -> (s -> b) -> Stream m a -> Stream m b

* Streamly Scanl (conceptual):
  * Scan with initial projection and final extraction
    * f step initial project final
    * f :: (s -> a -> s) -> s -> (s -> b) -> (s -> b) -> Stream m a -> Stream m b

* Other state machine formulations:
    * Moore with s ~ (s, b), split state and output
    * Dropping first element and "fmap snd" gives Mealy
    * f :: ((s, b) -> a -> (s, b)) -> (s, b) -> Stream m a -> Stream m (s, b)
    * But we do not need to pass the output back, only the state
    * f :: (s -> a -> (s, b)) -> (s, b) -> Stream m a -> Stream m (s, b)
    * If we do not need the final state
    * f :: (s -> a -> (s, b)) -> (s, b) -> Stream m a -> Stream m b
    * If we do not need the initial output
    * f :: (s -> a -> (s, b)) -> s -> Stream m a -> Stream m b

* State machines composed of state machines (concatMapAccum)
    * Moore style:
      * f :: (s -> a -> m s) -> s -> (s -> Stream m b) -> Stream m a -> Stream m b
    * Mealy style:
      * f :: (s -> a -> (s, Stream m b)) -> s -> Stream m a -> Stream m b

## TODO

* Formulate the Step type as `Partial s b` so that we do not need the extract
  function. We won't have to thread around the `b` anymore as we do not need to
  return it in `final`.

* Formulate the Scanl/Fold type using a lower level Moore/Mealy step, like
  Unfold in streams. We can then wrap that into another type to emit the
  initial value. That will allow use to reuse the same underlying machinery for
  both Postscanl and Scanl. We can name Postscanl as Transduce?

* Change the final function in Scanl constructor to not return output type.
