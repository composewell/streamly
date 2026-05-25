## Related documents

See Stream and Scan design docs.

## Moore vs Mealy Machine

Moore:   observes states
Mealy:   observes transitions

Moore naturally exposes an initial observation.
Mealy naturally supports terminal extraction.

Moore and Mealy both can represent stream transformations as well as folds
equally well. The Mealy formulation is usually more straightforward to think as
an explicit state machine. We have an initial state, each input causes a state
transition and an output.

Producer: A state machine that produces output without any input
Transducer: A state machine that produces output on input

## Streaming Types

Unfold: injectible state stream generation
Stream: opaque state stream

OpenScan: injectible state transducer
Scan: opaque state transducer

OpenScanl: injectible state Moore machine (with initial state)
Scanl: opaque state Moore machine

Refold: OpenScanl with only the final output
Fold: Scanl with only the final output
Parser: A Fold with error and backtracking

Alternatives naming:

* Producer (composes on output side)
* Unfold
* Stream
* Transition
* OpenMealy <-> UnfoldScan
* MealyScan <-> ProducerScan <-> StreamScan?
* OpenScan
* Scan
* OpenScanl <-> RefoldScan <-> OpenMoore
* Scanl <-> FoldScan <-> MooreScan <-> ConsumerScan
* Fold
* Refold <-> OpenFold
* Consumer (composes on the input side)

Duality: A stream can always stop without generating any output, a fold
always has an output to start with.

Even richer folds and producers:
* Parser

Rejected:
* Step (too generic) <-> STF (state transition function, too cryptic)
* Rescan <-> RawScan
* Transform (too common)
* RawFold

Nested streams:

* Pipe
* MoorePipe, MealyPipe?
* Process (bidirectional pipe)

Refold -- resumable fold, folding to a different accumulator, we supply the
accumulator to fold to.

We can use Refold for folds, but OpenScan for scans. Not necessarily
has to be the same root.  Encapsulated vs Open. We pass the function
and the value is supplied on call site, inside the nesting. So there is
no nesting. Rescanl could be OpenMoore and we can have an OpenMealy as
well.

A scan has two variants depending on whether we emit the initial state of the
machine or the final state in addition to transitions. Scan only emits
transitions. The variant naming axes: left, right; l, r;  pre, post; head,
tail; initial, final; before, after. we could use scanLeft, scanRight but
"right" might be confused with "scanr", though scanr is an obsolete, much less
used thing, should go into obsolescence.

Symmetric types like "scan" and "pipe" can name their connecting
functions as "compose", whereas assymetric ones can use like
"unfoldEach" for unfold and "foldMany" for folds. Anyway if we use
"scanl" to scan a scan then we cannot use "pipe" to pipe a pipe; compose
works. For restartable scan we can use "composeResume", which can work
for pipe as well.

compose <-> scanInput, flipped scanInput is scanOutput

## Basic State Machines

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
  * so separate the input from state
  * output is derived from state after transition
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

## Advanced State Machines

### Producers

```
data ProducerStatus s b = Produce b s | SkipOutput s | StopOutput
data Producer s m b = Producer (s -> m (ProducerStatus s b))
data Unfold m a b = forall s. Unfold (Producer m s b) (a -> m s)
data Stream m b = forall s. Stream (Producer m s b) s
```

### Scans

### Consumers

```
data ConsumerStatus s b = Consume b s | StopInput
data Consumer s a m b = Consumer (s -> a -> m (ConsumerStatus s b))
data Fold m a b = forall s. Stream (Consumer s a m b) s
```

## lmap Naming

Now coming to a better lmap. lmap is inverted because we need to first
think of map and then invert it. that's the problem, it creates a lot
of indirection. We should be thinking straight and fresh instead of thinking
the inverse process of an already established dual. I would use the term
"plug" instead of lmap, which means we are plugging the thing between
the input and the object. For example, "plug fst u", plug the "fst"
function into the unfold "u", so what we are going to give would go into
fst and fst's output goes into u. This is straight forward to think.
it's like the electrical adaptor goes into a socket and our device's
plug goes into the adapter.

