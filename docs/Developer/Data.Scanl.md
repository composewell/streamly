## Related docs

See the state machines doc.

## Use Cases

Scan operations:
* source terminates before sink
* sink terminates before source
* ability to stop without output (empty stream output)
* ability to give output without input (empty stream input)
* perform sum-so-far or running "sum" operation

Pipe operations:
* perform output filtering? consume without producing.
* perform input filtering? produce without consuming.
* delay-by-k emitting, with drain at the end, involves buffering and draining
  i.e. consume without producing and produce without consuming. Similary, emit
  sorted groups of k items.

## Moore scans (scanl)

A Moore machine emits the states of the machine, the output is solely a
function of the state. foldl and scanl combinators in Streamly Stream
module are Moore machine style representations:
```haskell
foldl :: (b -> a -> b) -> b -> Stream m a -> m b
scanl :: (s -> a -> s) -> s -> Stream m a -> Stream m s
```

### scanl and postscan

The `scanl` operation emits the initial value of the accumulator
followed by one value per input, the next state of the machine. If the
input stream size is n the output stream size is `n + 1`.  A postscan
does not emit the initial value of the accumulator and is therefore a
n-to-n transformation.

## Reified Transducer

```haskell
data MooreTransducer s m a b =
    MooreTransducer (s -> a -> m s) (s -> b)

newtype MealyTransducer s m a b =
    MealyTransducer (s -> a -> m (s, b))
```

## Transducers with abstract state

```haskell
data MooreTransducer m a b =
    forall s. MooreTransducer (s -> a -> m s) (s -> b) s

newtype MealyTransducer m a b =
    forall s. MealyTransducer (s -> a -> m (s, b)) s
```

## Fold Scan

We call the push-through transducer a fold scan as it attaches naturally
to a fold.  The Scanl type in streamly is a fold scan. It is simply an
extension of the Fold type and a reification of the Moore style `scanl`
operation above. Read the fold design doc to get the basics from where
we are going to start in this section.

A Fold with extraction of intermediate states.
```haskell
data Step s b = Partial s | Done b

data FoldScan m a b =
    FoldScan
        (s -> a -> m (Step s b))
        (m (Step s b))
        (s -> m b) -- extract
```

RULES:
* `extract` can be called any time on `s` to get the latest value.
* `extract` should be called at most once on "s" as it is effectful.

## Resource Cleanup

Now that extract can be called after every step, it can no longer act as
a finalizer, as was the case in folds. So we need a separate finalizer
which would be called if the input stream ends. If the scan terminates
on its own, returning a `Done`, it will cleanup before terminating.
```haskell
data FoldScan m a b =
    FoldScan
        (s -> a -> m (Step s b))
        (m (Step s b))
        (s -> m b) -- extract
        (s -> m ()) -- final
```

## Drain phase in the end?

Let's say we implement a delay-by-k scan i.e. we buffer k elements and keep
emitting one if the buffer is full. In such a case when the input stops or the
scan is done we need to flush more than one element. So the final extract
should look like a producer step i.e. `s -> m (Step s b)`.

### Adding final drain phase to scan

The "final" callback returns a stream:
```
final :: s -> m (Stream.Step s b)
```

We cannot use Fold.Step here because we may have to finish with a possible
empty stream and there is no way to represent that using Fold.Step.

When a driver is driving a scan and the input stream is finished, it has to
drain the "final" stream of the scan in a loop emitting one value at a time in
the output stream. When two scans are composed, there are two cases:

1) If the second scan finishes first then there is nothing to drive, the first
scan can just cleanup itself and return Done.

2) If the first scan finishes first it will feed the result into the second
scan and then second scans result is to be extracted by the driver. However,
since first scan is finished, afterwards we also have to drain the second scan
without consuming further input. But there is no way to do that.

So for a scan to be composable without SkipInput feature, it has to be
one-input one-output, and "final" can only be of type "final :: s -> m ()" only
a resource cleanup op not a draining one.

Very few scans require this draining capability especially the classify and
demux scans, but everyone needs to handle this. Should we use the pipe type
for drainable scans? Or solve that problem differently?

### Why two scan types?

Note: scanl1 is naturally expressed as a MealyScan. If we express it
using Scanl, then we have to have the return type as "Maybe a" because
scanl has to emit an output and that would be Nothing in case of empty
stream. In other words MealyScan is natural when there is no initial
value e.g. the "latest/last" scan using mealy is a possibly empty stream
whereas using Scanl it would be a "Maybe a" stream. However, we can
use "catMaybes" on the resulting stream from Scanl to get the same
effect. Scanl always gives you a non-empty stream which would be forced
to be a Maybe if we have an empty stream input.

The other real reason for two types — probably the one that actually pays rent
— is fusion-side matching. A Mealy scan's bundled step :: s -> a -> Step s b
has the same shape as the stream's own producer step, so it fuses into a stream
pipeline with no impedance. A Moore scan's separate extract has Fold's shape,
so it fuses into the consumer side. Two types let each fuse cleanly into the
side it's used on instead of paying for an adapter. That's concrete and
measurable, unlike the expressiveness story.

classifyScan, demuxScan etc have something to drain at the end. Such scans
cannot drain the buffer in the end in the push scan implementations. Push scans
are driven by pushers, if there is nothing to push, they cannot produce
anything. On the other hand pull scans are driven by a puller, as long as there
someone to pull it will keep unraveling. Scans which have something to drain at
the end therefore are easily representable using the pull scans. Therefore push
and pull are the real divisions between types rather than Moore or MEaly. Pull
+ pull types fuse well and do not have a dead end, similarly push + push works
well. Interleaved push and pull require a more intricate driver which is what
we implement in a pipe. Without a pipe we can still express a lot of problems
if we take care of composing pull-pull or push-push or nest push inside pull.
The stream side types use a pull protocol by using "Yield s b" and "Stop", the
fold side types use a push protocol by using "Partial s" and "Done b". And we
cannot really unify these unless we create a more complicated pipe type for
interleaving push and pull.

I think the critical point here is push vs pull rather than Moore vs Mealy,
those are just representations. The problem is we cannot embed a pull inside
push, because there is nobody to drive it. However if we embed a pull inside
pull, the pulling driver will always pull whatever is there in it.

So the library conclusion is forced and simple: a scan that can drain must
expose a pull protocol, and scans compose-with-drain only in pull form. Push
scans are the strictly-input-clocked subset — fine, fuse well, compose cheaply,
but no drain past end-of-input, and crucially no drain at an interior
end-of-input either, which is what kills push-composition of buffering stages.
Keep both because they're different protocols with different powers, not
because they're different encodings. And the dividing capability has a one-line
statement now: can a stage produce when nothing is feeding it? Pull yes, push
no — because production-without-feed is a pull, and a pull is only ever driven
by a puller.

### StreamScan vs FoldScan

Mapper/Transducer is strictly a transformation arrow because it does not have
an initialization or finalization.

MealyScan has no way to emit an output before the first input, therefore, the
type restricts it to a Mealy style scan. That is the reason the initial is "s"
and not (Stream.Step s b), and because of that Mealy cannot stop without input
which is a distinguishing feature compared to Moore which can stop without
input. So Mealy has to consume one input, and Moore has to produce one output.

If we use Step type for "initial" then we will be able to implement Moore or
Mealy with the MealyScan type and we can stop without input as well which will
put this type at parity with the "Scanl" type. And if we have a requirement for
some resource allocation in the beginning then we will have to make "initial"
monadic.

The Scanl type is more general than a strict Moore machine. Because the initial
type is a Step we have all possibilities, it can emit at initial or it can Skip
using Continue. If we do not include Continue in the "initial" result type then
we can restrict this from acting as Mealy. Note that if we make the "initial"
of MealyScan as Stream.Step then we can implement Moore as well with that type.
So the only crucial difference between these two types in that case is
"extract" vs "Yield" mechanism and "Stop" vs "Done b" for termination. Note
that the "final" in Mealy is a machine required feature as well as an
abort/finalization mechanism. Whereas in Scanl it is only an abort/finalization
mechanism.

So essentially these two types are equivalent. However there may be some
operational differences due to fusion characteristics of the two when composed
with other types. However, if we have Step type in "initial" and "final" both
then that line also gets blurred because that requires introduction of an
additional state in both cases. We need to check practically (GHC core) if
there is a difference due to different representations.

Note that we can restrict the Mealy from being used as Moore and vice versa by
choosing a restricted type in the "initial" action instead of the general
"Step" type, the way we do it in the Parser type.

Scanl can be used as a transformation if we use it with postscan which discards
the initial result, but we have to be careful with implementation in presence
of filtering with Continue, postscan should drop the first state transition
irrespective of whether it is Continue or Partial. Similarly, MealyScan can be
used as a transformation by dropping the last element, however, for that to
hold we need to make sure that we use a custom "Final" type (or singleton
action s -> m b) so that it emits only a single element in the drain phase.

The Mapper type is simpler than Scanl or MealyScan unless we add
initialization/finalization to it but we do not need to do that because that is
what makes this type distinct. So Mapper can perhaps give better fusion with
the Stream type.

-----

Stream scan can partition and distribute the stream but it cannot chunk it. For
chunking we need a push style scan because there is nobody to pull there. On
the other hand FoldScan can be used for both partitioning as well as chunking.
However, stream scan can be used to drain buffered data, while FoldScan cannot
be used for that.

## StreamScan vs FoldScan

The fundamental difference between MapAccum and Scanl is not Moore/Mealy
encoding, that does not matter from the point of view of the functionality. The
fundamental difference is the "Done b" vs "Stop" constructor which makes one of
them a fold and the other one a transformation arrow. That's the key
difference, the rest is mostly superficial. The "map" in MapAccum indicates it
is a transformation.

Can we unify MapAccum and Scanl:
```haskell
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

## Adding filtering

Like streams can filter output, filtering should be a first class feature of
scans as well. In case of scans filtering means we do not allow certain inputs
to pass through the scan. If an input is not allowed to go through then we
should also not emit an output corresponding to that input.

To support this we add a "Continue" constructor to Step result:
```haskell
data Initial s b = IPartial s | IDone b
data Step s b = Partial s | Continue s | Done b

data FoldScan m a b =
    FoldScan
        (s -> a -> m (Step s b))
        (m (Initial s b))
        (s -> m b) -- extract
        (s -> m ()) -- final
```

We have kept the return type of initial the same as before and only
added "Continue" to the return type of `Step`.  If step returns
"Continue" the driver does not call extract and does not emit the
output.

## teeWith when filtering

Both the branches of a tee can independently filter, how do we zip
the results?  Zipping makes sense only when we zip the corresponding
elements of both the branches. There are multiple ways to do that:

1) innerJoin: zip only when both values are available, filter out the rest
2) outerJoin: we can add a combinator to convert "Continue" constructor
   to Nothing and "Partial" to Just and then zip normally, this will
   convert the original streams to Maybe streams where Nothing means
   filtered, and then we can zip.

We can think of using the the last available value, a last available
value is always possible if "initial" cannot result in "Continue".

## teeMerge with filtering

Filtering occurs in both the branches independently and the output
gets combined into a single stream, this is straightforward with easy
semantics.

## Category Composition

```haskell
compose :: Scanl m a b -> Scanl m a b -> Scanl m a
```

There are two types of composition possible, postscan style and scanl style.
postscan style composition is straightforward. Composed "initial" initialize
both the scans but the initial value is not used. If right initial returns
"Done" the composed initial returns "Done". If left initial returns "Done" that
means the input to right scan is over and we extract it and return Done.
Similarly, step composition is straightforward with no issues.

However, we run into a problem if we use "scanl" style composition of two
scans. In this case, in the composed "initial", the initial value of the left
scan is to be fed to the step of the right scan. But step now has a "Continue"
so it can possibly return "Continue" and filter out the output, and since we
are in "initial" we have no way to express "Continue" in initial.

Before we solve this porblem we need to ask -- is "scanl" style composition
useful?

## scanl composition use case

Use case for composing in a scanl style instead of postscanl.

s1 = Scanl.const initialBalance (some business-default starting balance), s2
= running total scan. Eager composition says: "the composed scan's very
first reported output is the running total as if the initial balance had
already been deposited -- before the first real transaction arrives." That's
actually a coherent and plausibly desired behavior: you want the composed
scan, when driven, to report initialBalance as output zero, then
initialBalance + tx1 as output one, etc. A postscan-composed version would
instead report nothing until the first real transaction, then 0 + tx1
(missing the seed entirely, because stage 2 never got seeded with stage 1's
default at all) -- which is a real, observable difference in behavior, not
just an extra/missing element: postscan composition here silently drops the
seed value's contribution, because stage 2 only ever sees real inputs, never
stage 1's eager default.

This looks like a real use case and it would be nice to support this style of
composition.

### Continue in initial

Let's address the use case of "scanl" style category composition by adding the
"Continue" result type in "initial" as well. Now initial can filter out the
result and we can express our previously impossible case by emitting "Continue"
from initial.

### Possibly Empty output

But if "initial" as well as step both can emit "Continue" then it is possible
that the scan goes without emitting anything, so empty scan output is now
possible, a scan no longer emits at least one value. If all the outputs are
filtered and even the last one, when the scan terminates, is to be filtered out
then how do we express scan termination because the only way we can terminate
is with a value "Done b".

Another case for the same is category composition in "scanl" style -- if the
left scan returns "Done" in initial and the right scan returns "Continue" then
what should the composed "initial" return? The only possibility is a "Stop"
without a return value.

The key point is that when filtering is a possiblity, it is always possible
that everything got filtered and there is nothing to return on termination in
which case we need a way to terminate without return value. Therefore, we need
another constructor e.g. "Stop" or "Halt" to express that.

So our final type becomes:
```haskell
data Step s b = Partial s | Continue s | Done b | Halt

data FoldScan m a b =
    FoldScan
        (s -> a -> m (Step s b))
        (m (Step s b))
        (s -> m b) -- extract
        (s -> m ()) -- final
```

Rename as a subset of pipe terminology:
* Partial => YieldAwait, Next
* Continue => Await, Skip, Omit, SkipOutput
* Halt => Stop
* Rename "Done" => StopWith, DoneWith, Return

Now "scanl" style composition is possible and the composed scan will emit an
initial value on "scanl" only if all the downstream scans in the pipeline emit
a "Partial" in their initial as well as in the first step, even if one of them
emits a "Continue" then the composed "initial" cannot emit a concrete value, it
has to emit "Continue" i.e. it becomes a postscan.  Whether the composed scan
emits an initial value or not is a dynamic decision depending on the states of
the scans downstream in the pipeline. In fact, with Continue possible in
initial, now any scan may choose to omit the initial value.

Supporting this is not cheap as all initial compositions will now have to
handle the "Continue" and "Done" cases as well even though those are corner
cases.

### scanl or postscanl drivers

Whether the initial value is emitted or not is decided by what type of scan
driver it is -- scanl or postscanl. This is not a choice of the scan it is a
choice of the driver. However, if a scan returns "Continue" then the driver
cannot emit it, it means that this scan does not support emission of initial
value, therefore even "scanl" behavior becomes like postscanl. Now "scanl"
becomes a best-effort, the scan author overrides it.

## Filtering via extract (Alternate Design)

One way to support filtering would be to change to change the signature
of extract to `extract :: s -> m (Maybe b)`. This will have the
benefit that now we do not need a Continue constructor in initial or
step.  However, this would be more costly because now we will have to
remember the filtering persistently in the state.  Instead of emitting
a "Continue" and forgetting about it, we will have to mutate the state
to say that currently we are in a output skip state. This may have a
significant impact on performance. And the state is now always wrapped
in a Maybe, so we always have to deconstruct that wrapper first in every
call. So does not look like a winning design.

## Skipping input

See the pipe document for skip input discssion.

Another example is sort in groups of k, so we need to buffer k and then emit
them all k in a sorted order. To implement such scans we need to be able to
skip inputs and act as producer for some time and then start accepting input
but not produce output. So in general we need both input and output skipping.

### SkipInput in Scanl?

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

The "Skip" on the stream side performs a dual role, (1) filtering the output,
(2) local looping. Note that these are two independent functionality but
because of the producer being a covariant type they can be achieved by the same
constructor. However, on the fold side these two functionality become
independent, (1) filtering requires the Continue constructor, (2) local looping
requires a "Loop" constructor and a producer step, the complexity makes us
defer the looping functionality to the pipe type. In streams we got that
looping for free.

### Why Moore style emission in Scanl?

Why not use  "Partial s b" in Scanl? This will fix the problem of extracting on
"Continue", and multiple extractions. However there are a number of problems
with that:
* parseMany etc, need to cache the output instead
* the output is not lazily extracted, it has to be strictly available extracted
  even if not demanded.
* After Partial the output should remain extractable all the time from the last
  Partial, that's the contract. So if the last emission was a "Continue" we
  should be able to extract the output if the stream ends right after that or
  in the "Partial s b" case we will have to cache it, which is a performance
  hit.

### Scan-fold relationship

Folds are a special case of Scanl. Scans may or may not produce an
output.  Folds aways produce an output. Folds can always be converted
to a scan by extracting the intermediate states of the fold. However,
we may not always be able to covert a scan to a fold because it may
not produce a value. However, if we know that a scan is guaranteed to
produce a value it can be cast into a fold by retaining the last value
of the scan.

Note that if a filtered fold is converted to a scan we will observe the
outputs corresponding to the filtered elements as well unless we add a
"Continue" constructor to folds too. If you do not want that then
convert it to a scan before filtering.

NOTE: do we need Fold and Scanl both, now that we have Continue in the step
type? Now extract in folds will not result in error as long as the fold is
written correctly to return Continue when extraction is not possible.
Similarly, do we need Refold and OpenReducer both?

## StreamScan (Producer Scan)

Alternate name Mapping or Mapper. Scanl is Reducer.

### Mealy Style mapAccum

mapAccum is a Mealy machine style representation:
```haskell
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

### The StreamScan type

This is a reification of mapAccum operation. The Step here is the Stream/Step:
data Step s a = Yield a s | Skip s | Stop

```haskell
data StreamScan m a b =
  -- | @MapAccum@ @step@ @initial@
  forall s. MapAccum (s -> a -> m (Step s b)) s
```

Naming: StreamScan for MapAccum and FoldScan for Scanl is another choice .
Scanp/Scanc is another choice. MealyScan and MooreScan is another choice but
the name Moore and Mealy are orthogonal to the producer/consumer dimension
which is the critical distinction of these two. Moore/Mealy encoding can be
changed.

### Stream-StreamScan relationship

If we drive a StreamScan with input () we can derive a stream producer
from it.  But it is still fundamentally different than a stream as a
stream is self driving and the scan needs to be driven by an input.

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

```haskell
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

```haskell
concatMapAccumM :: (s -> a -> m (s, Stream m b)) -> m s -> Stream m a -> Stream m b
```

This is essentially a nested state machine, a state machine composed of many
state machines.

The Moore style formulation of the same would require an extract function, the
extract function is called once per input as the state advances:

```haskell
concatScanlM :: (s -> a -> m s) -> s -> (s -> Stream m b) -> Stream m a -> Stream m b
```

This represents a more structured pipe. It runs a consumer loop and after
consuming each element it runs a producer loop. Each consumed element can be
expanded into 0 or more output elements statefully.

We can also write fusible alternatives of the above as:
```haskell
concatMapAccumM :: Producer s m a b -> m s -> Stream m a -> Stream m b
```

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

* Stream.scanl: running a Scanl type
* Stream.postscanl: running a Scanl type dropping the initial value
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
* Scanl.scanlMany -> composeMany -> composeRepeat
* Scanl.postscanl -> postcompose
* Scanl.postscanlMany -> postcomposeMany

Directly running a scan using a step function
```
    Stream.scanl'
    Stream.postscanl'
    ...
```

The proposal is to not expose these and just suggest using the Scanl
constructors, for the following reasons, (1) there is no performance advantage,
(2) we are giving too many options to the user, (3) the names create
confusion with the Scanl runners, (4) if we have these in Data.Stream then why
not in Data.Scanl, Data.Unfold and Data.Fold as well, (5) we have Scanl
constructors with exact same names as the Data.List APIs therefore
discoverability of traditional APIs should not be a problem.

Directly running a mapAccum using a step function
```
    Stream.mapAccum(M)
```

The reified type for mapAccum is the `Scan` type. Similar to the above
operations we should not expose these direct operations, rather we should use
the `Scan` type to run these using the `scan` runner. The `Scan` type can have
constructors named `mapAccum` and variants of that, also constructors named
`postscanl'` and variants make sense for the `Scan` type.

Directly running a concatMapAccum using a step function
```
    Stream.unfoldAccum(M)
    Stream.concatMapAccum(M)
```

These are stateful 1-to-n transformations. unfoldAccum is the fused version and
concatMapAccum is the unfused version because of the existential type in the
Stream. The reified type that can represent unfoldAccum is the `Pipe` type.
However, the Pipe type is more general because it can do n-to-1
transformations and 1-to-n as well i.e. it can fold and produce. There is no
reified type corresponding to concatMapAccum.

Scanl constructors:
```
    Scanl.mkScanl
    Scanl.mkScanl1
    ...
```

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
* Stream.scan: running a Scan type

Scan type constructors:
* Scan.postscanl': constructor using Moore style step function
* Scan.postscanlM': monadic version of postscanl'
* Scan.mapAccum: constructor using Mealy style step function
* Scan.mapAccumM: monadic version of mapAccum

Scan type adapters:
* Scanl.toScan: convert Scanl to Scan
* Scanl.fromScan: convert Scan to Scanl
* Fold.fromScan: convert Scan to Fold

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
