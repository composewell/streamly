## Related docs

See the state machines doc.

## Use Cases

Scan operations:
* source terminates before sink
* sink terminates before source
* ability to stop without output (empty stream output)
* ability to give output without input (empty stream input)
* perform running "sum" operation

Pipe operations:
* perform output filtering? consume without producing.
* perform input filtering? produce without consuming.
* delay-by-k emitting, with drain at the end, involves buffering and draining
  i.e. consume without producing and produce without consuming. Similary, emit
  sorted groups of k items.

## Scanl (Consumer Scan)

### Moore Style Scan

foldl and scanl combinators in Streamly Stream module are Moore machine style
representations:
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

### Scanl Type

Scanl is just a reification of the Moore style `scanl` operation above.

Scanl emits an initial value extracted from the initial state. The extract
function extracts the output from the state, it can be called after every
`Partial` step to get the output.

It also has a finalizer action, which is called only when the scan is
terminated externally, rather than completing on its own (via Done). In that
case `final` can be used to perform any resource finalization, it does not
return any output, just an effect, and should be called only once.

It supports resource bracketing by resource allocation in initial state and
release in the final. However always emitting a  value in the initial step can
make implementation of operations like map and filter more complicated than
they need to be. Separate types can be used for different use cases which would
be less ergonomic but can be optimized better.

```
data Step s b = Partial !s | Done !b

data Scanl m a b =
  -- | @Scanl@ @step@ @initial@ @extract@ @final@
  forall s. Scanl (s -> a -> m (Step s b)) (m (Step s b)) (s -> m b) (s -> m ())
```

We require the general Scanl type for resource bracketing.  For fusion
optimizations, a postscan (no initial emission) type may be helpful for the
case when no resource bracketing is required.

### Scanl-fold relationship

Folds are a special case of Scanl and the relationship must follow a
strict law -- a fold can always be obtained by keeping the last element
of a Scanl.

This relatiohship necessitates that we always extract and emit a final
value in a Scanl when the stream stops. This means that if the scan stops
voluntarily then the output has n+1 elements if the input has n elements.
However, if the scan is force extracted when the stream stops then the output
will have n+2 elements, the additional element corresponds to the missing
transitions that never occurred because of early abort of the stream. In many
cases the additional element may be the same as the element before it, that
happens when the scan has nothing buffered in the state and there was nothing
additional to do if the stream stops at any point.

Now the question is - should we keep this as the default and only
behavior of the scan or should we provide this as on option? Can this
duplicate element emission on abort cause issues for certain consumers
of scan?

So the type must be:
data Scanl m a b =
  -- | @Scanl@ @step@ @initial@ @extract@ @final@
  forall s. Scanl (s -> a -> m (Step s b)) (m (Step s b)) (s -> m b) (s -> m b)

### Buffering in Scans

Let's say we implement a delay-by-k scan i.e. we buffer k elements and keep
emitting one if the buffer is full. In such a case when the input stops or the
scan is done we need to flush more than one element. So the final extract
should look like a producer step i.e. `s -> m (Step s b)`.

### Skipping input

Another example is sort in groups of k, so we need to buffer k and then emit
them all k in a sorted order. To implement such scans we need to be able to
skip inputs and act as producer for some time and then start accepting input
but not produce output. So in general we need both input and output skipping.

## Moore-Mealy vs Producer-Consumer

The Moore vs Mealy representation dimension is orthogonal to the Producer vs
Consumer dimension. We can use Moore representation in both consumer or
producer or Mealy representation in both. However Moore is more suitable (and
efficient) for consumers and Mealy is more suitable for producers.

One point to note is that Moore and Mealy become the same if we remove the
initial or final step. So if that step is not important both of them degenerate
to the same representation.

Operations like map and filter where only transitions are important, and
empty stream does not require any special processing are efficiently
expressible by Mealy style without extraction or Moore without an
initial step. On the other hand operations like sum where empty stream
sums to 0 are more convenient in Moore style or Mealy with a final
extraction step.

## MapAccum (Producer Scan)

Alternate name Mapping or Mapper. Scanl is Reducer.

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

### The MapAccum type

This is a reification of mapAccum operation. The Step here is the Stream/Step:
data Step s a = Yield a s | Skip s | Stop

data MapAccum m a b =
  -- | @MapAccum@ @step@ @initial@
  forall s. MapAccum (s -> a -> m (Step s b)) s

Naming: StreamScan for MapAccum and FoldScan for Scanl is another choice .
Scanp/Scanc is another choice. MealyScan and MooreScan is another choice but
the name Moore and Mealy are orthogonal to the producer/consumer dimension
which is the critical distinction of these two. Moore/Mealy encoding can be
changed.

### Stream-MapAccum relationship

If we drive this scan with input () we can derive a stream producer from it.
But it is still fundamentally different as a stream is self driving and the
scan needs to be driven by an input.

### Limitations

What it can do?
* it gets a Category instance, because it can represent "identity" and that is
  because it does not have to produce output without input thus "MapAccum m a
  a" is implementable, if we can produce an output without input then
  "MapAccum m a a" will have to manufacture a value of type "a" on empty
  input.
* With Category it gets the Arrow instance as well using unzip/teeWith.
* It can implement scanl1, but not scanl .
* Can do: map, filter, stateful scanl1-style scans, take/drop/takeWhile, and
  tee/unzip/fork via Arrow.

What we cannot do with a MapAccum?

* It can produce at most one output element for each input element.
  So this type maps, filters, scans, takes/drops -- but it cannot expand
  (concatMap, unfoldMany, replicate-each, intersperse-multiples).
* It cannot fold. It cannot yield an output on an empty stream e.g. sum fold
  returning 0 on empty input stream, so folds are not possible. Adding an
  extraction on input stream termination i.e. `final :: s -> m (Maybe b)` can
  solve that, but then we will Category instance.
* It cannot split streams in chunks. Without a "Done b", Stop will have to be
  emitted on the subsequent input, in which case the next input is lost. So
  without "Done b" it cannot be used to fold streams in chunks without losing
  elements.

So all it can do it is fork/unzip streams into parallel streams and process
different parts in different branches e.g. tee and unzip operations. It cannot
split streams in chunks. So this is a transformation arrow, not really a
consumer.

## MapAccum vs Scanl

The fundamental difference between MapAccum and Scanl is not Moore/Mealy
encoding, that does not matter from the point of view of the functionality. The
fundamental difference is the "Done b" vs "Stop" constructor which makes one of
them a fold and the other one a transformation arrow. That's the key
difference, the rest is mostly superficial. The "map" in MapAccum indicates it
is a transformation.

Can we unify MapAccum and Scanl:
```
data Step s b = Partial s | SkipOutput s | Done b | Stop

data Scanl m a b =
  -- | @Scanl@ @step@ @initial@ @extract@ @final@
  forall s. Scanl (s -> a -> m (Step s b)) (m (Step s b)) (s -> m b) (s -> m b)
```

1. On `Partial s` we use extract to get the output
2. On SkipOutput, we do not extract and the next fold in the pipeline also
   skips, the entire chain skips upto the driver.
3. On Done b the driver is done and emits result b.
4. On Stop driver is done without emitting a result. Note, we need to have a
   distinct constructor Done/Stop to emit or not emit the result.
5. If the producer finishes, we use "final s" to extract the final output.

It is a more complex type and we are hoping that the SkipOutput fuses away,
because in most of the fold use cases we do not need it, and the same about
Done and Stop.

But more than that the bigger problem is that the type becomes more powerful,
more flexible, but less structured, now sometimes it can give you a result and
sometimes not, which makes it more error prone when using.

Better than building this we can just use the full Pipe type.

## SkipInput in Scanl?

Should we add SkipInput to the Scanl type -- does it have duality with the
SkipOutput on MapAccum side? Cardinality change is the transformation axis not
a reducer axis. A fold reduces; it doesn't alter stream length. So insert and
delete both belong to the arrow, not the fold. Adding SkipInput to Scanl builds
out Scanl-the-transformer, and now "the transformer" is split across two types
by encoding -- Moore-Scanl inserts, Mealy-MapAccum deletes -- and a user who
wants filter-then-expand needs both.

A better way is to keep all cardinality on the arrow and let Scanl stay a fold:
MapAccum carries Skip (delete = filter), Scanl reduces, and the "missing"
insert comes from other combinators on the producer side. Stateless expand is
unfoldEach. So the real dual of the delete-arrow isn't an inserting Scanl; it's
unfoldEach/Unfold. MapAccum arrow deletes, producer inserts, fold reduces --
nobody becomes a Pipe.

A stateful expansion is perhaps a concatMapWithSharedState or an
unfoldEachWithSharedState rather than a Scanl. SkipInput in Scanl is also much
more complicated to implement, in addition to changing the Step type we also
have to add a producer and consumer step. Which perhaps indicates that this is
not the right abstraction.

Also we need to perhaps look more carefully what is dual to filtering?

---

The two step consume/produce solution nicely solves the "unfoldMany" fusion use
case. It can also be used for stateful expansion using concatScan. or rather
concatMapAccum using a MapAccum with two steps. Perhaps the concatMapAccum is
the producer side mirror use case of the unfoldMany use case on the consumer
side?

## Implementation Optimizations

We can write a variant of `scanl` that does not emit the initial value
(postscan), and similarly a variant of MapAccum that does not emit the
terminal value. Use cases where we do not require these additional values can
benefit from these more efficient simpler variants.

In a scanl, the driver must emit the initial value before processing any
input.  Since this cannot be folded into the per-input step, the driver
needs a distinct pre-input state to run the initialization effect and
transition into the normal wait-for-input loop. That state remains in
the dispatch path for the lifetime of the stream.

In postscan, the initial value is not emitted, so this extra state can
be eliminated. However, a monadic initial value still requires a
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

However, when the state is pure, GHC fusion optimizations can potentially
eliminate the initial state, if it can happen reliably we do not need a
separate type with pure state and no Step result for this simplification. In
several cases we have actually seen fusion issues or inefficiencies due to this
initial state where it is not required.

The Mealy side is symmetric at the other end. In a MapAccum without terminal
state extraction and with pure finalizer we do not need a separate state.
However, a monadic finalizer forces a post-input state even without terminal
state extraction.

## concatScanlM and concatMapAccumM

The Mealy formulation generalizes to concatMapAccum, without a final state
emission:

    concatMapAccumM :: (s -> a -> m (s, Stream m b)) -> m s -> Stream m a -> Stream m b

This is essentially a nested state machine, a state machine composed of many
state machines.

The Moore style formulation of the same would require an extract function, the
extract function is called once per input as the state advances:

    concatScanlM :: (s -> a -> m s) -> s -> (s -> Stream m b) -> Stream m a -> Stream m b

This represents a more structured pipe. It runs a consumer loop and after
consuming each element it runs a producer loop. Each consumed element can be
expanded into 0 or more output elements statefully.

We can also write fusible alternatives of the above as:
    concatMapAccumM :: Producer s m a b -> m s -> Stream m a -> Stream m b

## Scan Variants

scanl, prescanl, postscanl:
scanl, provides the initial value of the accumulator and the next value
on each input. postscanl drops the initial value. prescanl drops the
final value.

mapAccum is similar to postscanl in that it does not (and cannot) emit an
initial value.

There are strict variants of these like scanl', monadic variants like scanlM',
variants where we use the first input as accumulator like scanl1' .

## Scan and Fold Modules

* `MapAccum`: The module representing a stateful transformation.
* `Scanl`: stateful reducer with intermediate state emissions. It can be named
  just "Scan" as well.
* The Fold type is exactly the same as the `Scanl` type except that it
  does not emit any intermediate values.
* Since there is no Foldr, the fold module was named Fold. Ideally it can be
  called `Foldl` to mirror the `Scanl` naming. But we are not going to change
  it now. It is perhaps better to name "Scanl" as "Scan".

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
