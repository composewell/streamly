## Related documents

See Stream and Scan design docs.

## Moore vs Mealy Machines

From basic automata theory:

Moore:   observes states
Mealy:   observes transitions

Moore naturally exposes an initial observation.
Mealy naturally supports terminal extraction.

Moore and Mealy both can represent stream transformations as well
as folds equally well. The Mealy formulation is usually more
straightforward to think as an explicit state machine. We have an
initial state, each input causes a state transition and an output.

## Basic Types of Machines

Producer: A state machine that produces output without any input
Transducer: A state machine that produces output on each input
Consumer: A state machine that consumes and then provides one output at the end.

### Pull vs Push Machines

State machines like `Stream` and `Fold` are simply specifications or
data. They specify a state and a function. The state machine needs to be
driven or cranked by a driver to do something useful with it.

Here we divide machines based on how they are driven. This is an
operational aspect rather than functional.  If the driver is pulling
output from a machine then we call the machine as pull style and if the
driver is pushing input elements to it then we call it push style.

Streams have only one way of cranking them. We call the producer step
function and collect the output, we call this pulling i.e. we are
pulling from a stream.

```
+--------+             +--------+
|        |    pull     |        |
| Stream |------------>| Driver |
|        |             |        |
+--------+             +--------+
```

Folds also have only one way of cranking them. We call the step
function supplying it the input and continue doing that and finally
we collect the output of the fold, we say that we are pushing to the
fold. So streams are components that are to be pulled from and folds are
components that are to be pushed to.

```
+--------+             +--------+
|        |    push     |        |
| Driver |------------>|  Fold  |
|        |             |        |
+--------+             +--------+
```

Producers are to be pulled from and consumers are to be pushed to. So
the push and pull division is really about producers and consumers. Pure
producers are always pull style and pure consumers are always push
style.

A complete pipeline always has a driver in the middle which pulls
from a stream and pushes to the fold.

```
+--------+             +--------+             +--------+
|        |    pull     |        |    push     |        |
| Stream |------------>| Driver |------------>|  Fold  |
|        |             |        |             |        |
+--------+             +--------+             +--------+
```

In streamly, Stream.fold or Fold.drive are the examples of a driver
connecting a stream and fold.

Certain non-composable operations (e.g. Stream.last) may collapse
the driver and the fold together, or similarly they may collapse the
stream and the driver together -- so we may not see this division
clearly. However, those special monolithic cases, in general, the driver
sits in the middle.

Transducers may be of two different types because they are consumers
as well as producers.

```
+--------+      +------------+      +--------+      +------------+      +--------+
|        | pull |            | pull |        | push |            | push |        |
| Stream |----->| Transducer |----->| Driver |----->| Transducer |----->|  Fold  |
|        |      |            |      |        |      |            |      |        |
+--------+      +------------+      +--------+      +------------+      +--------+
```

The first transducer sits between the stream and the driver. Therefore,
its output end is designed for pulling and its input end is designed
for pulling from the stream. We call it pull-style or pull-through
transducer. The second transducer is a push-style (or push-through)
transducer it has to be pushed to and on the other end it has another
component to push to. The first transducer and the stream fuse together
to become a stream, the second transducer and the fold fuse together to
become a fold, thus we can reduce this diagram to the previous simple
stream and fold diagram. Each style has certain characterstics that the
other does not have.

This is an important division in designing and using the types for best
efficiency and fusion. Even though functionally the two transducers may
be equivalent, the push-pull division is important for compositional
efficiency and that is a fundamental guiding principle for streamly
design.

### Mealy and Moore Machines

Even though the push-pull division is independent of Moore or Mealy
axis, Mealy is naturally suitable for pull style and Moore is naturally
suitable for push style machines. Why is that?

How does the pipeline work? The driver pulls one element at a time from
a stream and pushes one element at a time to the fold. Why the driver
is pulling an element from the stream? Because it needs to consume it
immediately, pushing it to the fold. Therefore, the yield from Stream
looks like `Yield a s` rather than `Yield s` and then `extract s`. The
first one is eager style and the latter is lazy style. If we use the
lazy style the state will become unncessarily complicated to hold the
element until the driver extracts it, also extract will have to modify
the state to remove the element.

When the driver is pushing to an accumulator the situation is different,
the output is the accumulator, it is part of the state and the driver
does not need to modify the state when reading the output, state
modification happens only on the push. So lazy extraction makes sense.

### Push-Pull Compositions

A streaming pipeline is completed by joining pull elements with push
elements. For example, Stream has to be pulled from and then push into
a fold, and that completes the loop and we get an IO action with the
result of the fold. A simple pipeline is just a stream and fold, one
pull and one push element and the driver sitting in between the two
cranking the two machines.

### Push-Pull Transducers

Transducers are of two types based on how the impedance matching occurs.
A stream transducer's input end composes well with a stream and the
output end used for driving the composed entity as a stream. A fold
transducer's input end is used by the driver to push input to it and its
output end fuses well with a fold such that the composed entity acts
as a fold. We can call these as stream scan and fold scan. These are
one-way transducers because they compose well only on one side.

The most basic and familiar example of a producer side transducer is
"foldr" and a consumer side transducer is "foldl".

A unified transducer is possible but becomes more complex in
implementation and difficult to handle for the compiler for
fusing. Streamly follows the principle of least power and provides
fusible, modular components at different levels of power. A pipe is a
two-way transducer in streamly and it can fuse with both streams and
folds as well as pipes but introduces more complexity in all aspects --
implementation details, fusing, efficiency.

### Nested Push-Pull Compositions

Pipelines more complex than a simple stream and fold combined can be
created and that's where nesting comes into picture. Nested stream
combinators like unfoldEach or concatMap and similar class of operations
combine pull elements with pull elements, and they combine naturally
into a nested pull element.  On the fold side we have foldMany and
similar class of operations which embed push elements inside push
elements and combine it into a push style elements. In all of these
cases the protocol matches and the boundaries naturally align with each
other and the combined protocol remains the same.

On the stream side foldMany and similar class of operations nest push
elements inside a pull element i.e. we pull from one and push into many
and combine this into a pull style element.

### Producer Consumer Asymmetry

There is difference between producers and consumers, while producers
have only one end i.e. the output end, consumers always have two ends,
input and output.  This creates an asymmetry in implementation as
well. Producers are covariant only whereas consumers are contravariant
as well as covariant. This makes the producer side transducers
simpler than the consumer side transducers. For example, producer
side transducers require only a Skip output function, consumer side
transducers require a Skip output and Skip input as well, and the latter
is more complex to implement because it requires the machine step to be
split into two parts one pure producer step and the other consumer step.

Because of this asymmetry the "foldMany" operation which embeds
consumers inside producers is fully fusible whereas "unfoldEach"
operation on folds which embeds producers inside consumers does not
fuse, theoretically it can fuse if we add the SkipInput functionality
in the consumers but we do not do that for simplicity and leave this
complexity to pipes, because anyway by adding that the consumer
transducers become full fledged pipes.

Another impact of this asymmetry is that on the producer side we can
build transducers that can drain their buffered data as a stream in the
end, however this is not possible for simple consumer side transducers
(pipes can do it though). And the fundamental reason for this is the
same we need a SkipInput functionality to be able to do this and that
makes it a pipe.

## Basic State Machines

These state machine elements and implementations already exist in
Haskell basic libraries and this is what the modular functional
programming is built on. The elements mentioned here are mostly what the
haskell lists (Data.List) provide already. Though these are the simplest
and most basic state machines therefore cannot express more complicated
programming scenarios, that's where streamly comes in, it enriches these
with additional functionality to be able to do more powerful tasks with
the same efficiency.

Haskell's data types and operations are not just modular and composable
quite often they are literal translations from mathematical and
theoretical computer science concepts. Scans and folds in the base
package are an example of direct translations of Moore and Mealy
machines from automata theory.

* Stream consumers, producers, and transformations can be represented
  as state machines.
* A state machine transition is represented by a function.
* iterate (basic producer step): f :: s -> s
* Builder (foldr style) step (consumer or transformation): f :: a -> b -> b
* Accumulator (foldl style) step (consumer or transformation): f :: b -> a -> b
* Moore step: f :: s -> a -> s, state transformation, output implicit in state
* Mealy step: f :: s -> a -> (s, b), state transformation with explicit output

### Producers (Pull style)

* `iterate` as the most basic producer:
  * iterate can be viewed as a degenerate Moore machine with no external input.
  * Moore style producer step (iterate): f :: s -> s
  * Moore style stream producer (iterate): f :: (s -> s) -> s -> Stream m s
  * Endo represents the transition algebra used by iterate.

* Mealy style stream producer (unfold)
  * unfold can be viewed as a degenerate Mealy machine with no input.
  * Unfold step: f :: s -> (s, b), Mealy with no input
  * Unfold: Mealy machine with no input, explicit state and termination
  * unfold: f :: (s -> (s, b)) -> s -> Stream m b
  * The data representation is Unfold
  * A stream can be represented as a Mealy machine with no input,
    using existential state and termination.
  * The streamly representation is `Stream` type.

* Mealy style stream transducer (mapAccum):
  * treating `s` as output is restrictive
  * So split the state and output
  * output produced by the transition itself
  * mapAccum: Mealy machine with existential state and termination
  * Scan step: f :: s -> a -> (s, b)
  * Scan without final extract:
    * f :: (s -> a -> (s, b)) -> s -> Stream m a -> Stream m b
  * Scan with final extract:
    * f :: (s -> a -> (s, b)) -> s -> (s -> b) -> Stream m a -> Stream m b

### Consumers (Push style)

* Moore style stream consumer (iterate)
  * When input ~ state, append becomes a degenerate Moore transition.
  * Moore style step (append): f :: s -> s -> s
  * Moore style transformation: f :: (s -> s -> s) -> s -> Stream m s -> Stream m s
  * Moore style consumer (fold): f :: (s -> s -> s) -> s -> Stream m s -> s
  * Semigroup can be used to fold by additionally supplying an initial value
  * Monoid can be used to fold without supplying an additional initial value (e.g. Sum)

* Moore stream transducer (scan):
  * treating `s` as input in iterate style fold is restrictive
  * it can only do `Stream m s -> Stream m s` transformation
  * so separate the input from state
  * output is derived from state after transition
  * scan using the same state and output type:
  * f :: (s -> a -> s) -> s -> Stream m a -> Stream m s
  * scan using a separate state and output type:
  * f :: (s -> a -> s) -> s -> (s -> b) -> Stream m a -> Stream m b

* Streamly Scanl (conceptual):
  * Scan with initial projection and final extraction
    * f step initial project final
    * f :: (s -> a -> s) -> s -> (s -> b) -> (s -> b) -> Stream m a -> Stream m b

### Moore to Mealy

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

### Nested

* State machines composed of state machines (concatMapAccum)
    * Nested state machines
    * Moore style:
      * f :: (s -> a -> m s) -> s -> (s -> Stream m b) -> Stream m a -> Stream m b
    * Mealy style:
      * f :: (s -> a -> (s, Stream m b)) -> s -> Stream m a -> Stream m b

## Streamly State Machines

Streamly state machines are richer versions of the above basic state machines.
Streamly adds termination capability, filtering capability and several other
capabilities to create more advanced state machines to cover more practical use
cases.

See the stream-fold-duality document for better understanding of the symmetry
of types across this spectrum.

## State Machine Types

Full spectrum of the state machine types in streamly, listed as a
progression from producers (streams), to pull-through transducers, to
push-through transducers, to consumers (folds):

### Producers

Stream Step:
    data Step s a = Yield a s | Skip s | Stop

Producer:
    Produces a stream of `a` from a seed `s`.
    one-to-many function style.
    composes on output side (covariant)
    newtype Producer s m a = Producer (s -> m (Step s a))

Unfold:
    Produces a stream of `b` from a seed `a`.
    More composable than producer due to state abstraction.
    one-to-many function style.
    data Unfold m a b = forall s. Unfold (Producer s m b) (a -> m s)

Stream:
    More composable than unfold due to seed abstraction.
    data Stream m b = forall s. Stream (Producer s m b) s

### Stream (pull-through) Transducers

In all these types Step is the Stream Step.

Stream Transducer:
    Mealy style transducer
    Same as Consumer type except that this is mealy style.
    newtype Transducer s m a b =
        Transducer (s -> a -> m (Step s b))

Stream Mapper:
    Mealy Transducer with state abstraction.
    More composable than Transducer.
    Simpler version of StreamScan.
    Transformation: with empty to empty possibility.
    Similar to mapAccum.
    data Mapper m a b = forall s. Mapper (Transducer s m a b) s

-- XXX Need a separate Step type for this with a Stop b
StreamScan:
    A Mealy style state machine.
    Always produces a result.
    Transformation: Empty to non-empty.
    Similar to mapAccumL.
    Name is PullScan or StreamScan and Scanl as FoldScan?
    This can nest production.
    The extract function of Moore is performed by Yield in this.
    data MealyScan m a b = forall s.
        MealyScan
            (s -> a -> m (Step s b)) -- transducer step
            s -- initial
            (s -> m (Step s b)) -- final/drain

### Fold (push-through) Transducers

In all these Step is the Scanl Step:
    data Step s b =
          Partial s -- emit an output
        | Continue s -- do not emit an output
        | Done b

FoldScan (Scanl):
    A Moore style state machine.
    A push style scan.
    Mapper with an initializer.
    A reducing or folding tool.
    Always produces a result.
    Empty to non-empty transformation.
    Similar to the scanl operation.
    This can nest consumption.
    Can do resource allocation and cleanup.
    data Scanl m a b =
      forall s. Scanl
        (s -> a -> m (Step s b)) -- consumer/reducer step
        (m (Step s b)) -- initial
        (s -> m b) -- extract
        (s -> m ()) -- cleanup if source terminates

Fold Mapper:
    Moore style transducer.
    More composable than Consumer because of state abstraction.
    Scanl with non-monadic initial, and no cleanup
    data FoldMapper m a b = forall s.
        FoldMapper
            (s -> a -> m (Step s b)) -- step
            s -- initial
            (s -> m b) -- extract

Fold Transducer (Consumer):
    Moore style transducer.
    Produces a final value `b` from a stream of `a`.
    many-to-one function style.
    composes on input side (contravariant)
    Same as transducer type except with a Fold style Step.
    The Moore style transducer requires an extract to derive output from state.
    newtype Consumer s a m b =
        Consumer
            (s -> a -> m (Step s b))
            (s -> m b)

### Consumers

In all these Step is the Scanl Step:

Fold: Scanl with only the final output
    More composable than Refold due to accumulator abstraction.
    Scanl, keeping only the last value.
    data Fold m a b =
      -- | @Fold@ @step@ @initial@ @final@
      forall s. Fold
        (s -> a -> m (Step s b))
        (m (Step s b))
        (s -> m b)

Refold:
    Produces a final value `b` from a stream of `a`.
    More composable than Consumer due to state abstraction.
    many-to-one function style.
    Uses a starting value of the accumulator.
    data Refold m c a b =
      -- | @Fold @ @ step @ @ inject @ @ extract@
      forall s. Refold
        (s -> a -> m (Step s b))
        (c -> m (Step s b))
        (s -> m b)

### Parsers

Parser: An augmentation of the Fold type with error and backtracking info.

data Initial s b
    = IPartial !s
    | IDone !b
    | IError !String

data Step s b =
      SPartial !Int !s
    | SContinue !Int !s
    | SDone !Int !b
    | SError !String

data Final s b
    = FDone !Int !b
    | FContinue !Int !s
    | FError !String

    data Parser a m b =
        forall s. Parser
            (s -> a -> m (Step s b))
            (m (Initial s b))
            (s -> m (Final s b))

## Symmetric Transducers

Pipes are transducers that do not distinguish betwee producer or consumer, they
can attach with streams and folds alike.

### Uni-directional Pipe

A pipe represents arbitrarily interleaved (or nested) mappers and reducers:

* Pipe

### Bi-directional Process

* Process (proposed name for a bidirectional pipe)

## Naming

Alternatives for Yield => Emit.
Alternatives for Step => Outcome, StepResult.

* Transducer <-> Transition
* OpenMealy <-> UnfoldScan
* MealyScan <-> ProducerScan <-> StreamScan?
* OpenScanl <-> RefoldScan <-> OpenMoore
* Scanl <-> FoldScan <-> MooreScan <-> ConsumerScan
* Refold <-> OpenFold

Alternative names:
* Step (too generic) <-> STF (state transition function, too cryptic)
* Rescan <-> RawScan
* Transform (too common)
* RawFold

### Refolds

Refold -- resumable fold, folding to a different accumulator, we supply the
accumulator to fold to.

We can use Refold for folds, but OpenScan for scans. Not necessarily
has to be the same root.  Encapsulated vs Open. We pass the function
and the value is supplied on call site, inside the nesting. So there is
no nesting. Rescanl could be OpenMoore and we can have an OpenMealy as
well.

### Symmetric vs Asymmetric Ends

Types with symmetric endpoints (Stream-to-Stream) like "MapAccum"
and "Pipe" can name their connecting functions as "compose", whereas
asymmetric (producer or reducer) ones can use like "unfoldEach" for
unfold and "foldMany" or "foldRepeat" for folds. Anyway if we use
"scanl" for an operation to scan a scan then we cannot use the name
"pipe" to pipe a pipe; compose works for all cases. For restartable scan
we can use "composeRepeat", which can work for pipe as well.

compose <-> scanInput, flipped scanInput is scanOutput
