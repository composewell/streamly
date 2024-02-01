# Streamly

This document is yet to be written. It is supposed to provide an overall
design overview for developers.

## Turing Complete

concatIterate: stream level looping.
partition (use an a -> Bool predicate): stream level conditional

Graph traversal using streamly: show fs tree traversal with dir cycles. Show
bfs and dfs traversal of the entire graph.

concatIterateWith: using state threaded around. Can this be done using a scan
instead? similarly mapMWith state.

## Stream/Pipe/Scan/Fold/Parser

Unfold/Refold

Stream m a: Yield, Skip, Stop, final
Scan m a b: Yield, Skip, Stop, initial, final
Pipe m a b: Yield, Skip, Stop, Switch, initial, final
Fold m a b: Skip, Done, final (no yield)

Stream is pulled by the driver.
    Driver stops: stream is left for GC, can use final
    Stream stops: Stop - driver cleans up

Fold is pushed by the driver.
    Driver stops: final value is pulled using final
    Fold stops: Done - driver gets the final value

Stream + Scan/Pipe = Stream
Scan/Pipe + Fold = Fold
Scan/Pipe + Scan/Pipe = Scan/Pipe

Scan does not backtrack.
Pipe does not backtrack.
Fold can backtrack but does not fail.
Parsers can backtrack and fail.

Scans/pipes can compose as tee/wye.
Folds can compose as tee.
Parsers do not compose as tee.

Folds can be used to split.
Parsers can be used to split.

We will need folds corresponding to all the scans so that we can use those in
splitting, and in fold tee and split compositions. Mostly we can convert any
scan to fold using the "last" fold. last is more like Stream.fold for scans.

We can possibly use tee/splitWith etc on scans themselves and automatically use
the last fold?

foldBreakD cannot be used on backtracking folds, only on scans.

With terminating scans we can also have scanMany, in foldMany like operations
also we can use "last" automatically.

Scans and pipes also need initial and final to allocate and release resources.

## Types

Basic producer and consumer types:

```
Stream m a
Fold m a b
```

`Stream m a` generates a stream consisting of values of type `a`. `Fold m a b`
consumes a stream consisting of values of type `a`. These two are duals of each
other.

The types do not look of dual shapes, the asymmetry is because of the fact that
folds have to return a value and streams have to consume a value. Folds make
the return type explicit in the type whereas streams encode that in the
generator function signature.

```
fromList :: [a] -> Stream m a
toList :: Fold m a [a]
```

Both folds and streams have an opaque internal state. In case of streams the
function argument automatically gets injected into the state. In case of folds
we need to explictly extract the output from the state.

Unfolds are a variation over stream where we make a component of the state as
global/static over the whole computation rather than keeping it dynamic via the
function argument. Similarly, refolds make a component of the fold state
global/static.

Returning the remaining seed value from the Stream type is like taking a
starting value of accumulator in each fold. That is equivalent to like the sum
fold being written as `sum :: Int -> Fold m a b` and all other folds also
written like that. But we do not do that.

Unfolds are a variation on stream that extend only the nature of the initial
value of state, making it static, it does not return the remaining seed.
Producer extends it to return the remaining seed. Refold on the other hand
introduces two things at the same time, we can now supply the initial value of
the accumulator (which we did not do before) and we make it static at the same
time. Refold and Producer are dual. What is the dual of Unfold? It would be
making the nature of "extract" static in the Fold? Basically exposing the state
type?

What is the duality in Folds and Streams in terms of the Monad instance? Folds
use a Result monad whereas Streams use a monad to compose stream elements.
ConcatMap starts another stream when one stream finishes, it then starts
generating another stream from the next seed. In folds, one fold finishes and
then the next fold starts taking previous fold's result into account.

Using stream concat if the previous seed has a leftover we cannot take that
into account in the next stream generation because streams do not return the
leftover state in the end. But we can use foldBreak in the base monad to be
able to achieve that. Or we can use Producer in the base monad.

However, foldBreak only gives you the leftover output stream. Sometimes we have
leftover input which may have to be combined with the next input. That can only
be done by the producer or "Stream a m r" monad. For example if we have a
stream of arrays and we are parsing it using Word8 folds then we may have some
leftover array that has to be combined with the next array. But we can combine
the leftover stream with the next stream instead of combining inputs.

With "Stream a m r" we can extract the residual seed at any point of time from
the stream.  We can extract it from a partially consumed stream e.g. from the
one returned by foldBreak. Extracting the residual seed gives us freedom to use
it in a different way, rather than using it in a specific type fo stream. For
example we may generate a Word8 stream from an Array and consume it using some
Word8 folds. But then we may want to extract the residue and generate an Array
chunks stream from it to consume via Array folds.

-- Without the result
Source m a <=> Sink m a (ListT)
  Source => Unfold m a b  (Unsink?)
  Sink m a => Resink m a b (static starting accum)

-- With result
-- fromList/fromFoldable seem to be the only generator functions that can
-- possibly return a result. And unfold from a Producer. Basically only the
-- functions that generate from a functor.
Stream a m r <=> Fold a m r
  Stream => ... m c a b  (Unfold?) (ReUnfold/ReStream? - resumable unfold/stream)
  Fold => Refold m c a b

Parser a m r -- error and backtracking (random access)
Generator a m r -- error and backtracking (random access)

## Tricky Parts

The state-passing through each API is currently fragile. Every time we run a
stream we need to be careful about the state we are passing to it. In case of
folds where there is no incoming state, we start with the initial state
`defState`. When we have an incoming state passed to us there are two cases:

1. When we are building a concurrent stream that needs to share the same `SVar`
   we pass the incoming state as is.
2. In all other cases we must not share the SVar and every time we pass on the
   state to run a stream we must use `adaptState` to reset the `SVar` in the
   state.

When in doubt just use `adaptState` on the state before passing it on, we will at
most lose concurrency but the behavior will be correct.

There is no type level enforcement about this as of now, and therefore we need
to be careful when coding. There are specific tests to detect and report any
problems due to this, all transform operations must be added to those tests.

## Direct vs CPS Types

In future we can remove the rewrite rules to make this explicitly StreamK, but
then the existing implementations may start showing bad performance. If we want
to separate these we should do that now.

Fused (Direct) (Data.Stream.Fused)
  Stream (Monad and semigroup won't scale)
  Unfold (fused n-ary appends/interleave/cross for finite n)

Nested (CPS - currently Data.Stream)
  Consability
  Append (infinite)
  Interleave (finite)
  Zip (finite)
  Cross (finite)

Concurrent
  Mostly nested
  Can use direct in some cases?

Data.Stream (fused)
Data.Stream.CPS (CPS.fromDirect/fromBasic/fromRaw) (Data.Stream.Cont?) (StreamK)
Data.Stream.Chunked (ArrayStream) (Direct inside CPS) (Data.Stream.Container?)
Data.Stream.Concurrent
Data.Unfold (fused with nesting) (Data.Stream.Unfold?)

CPS will supply the cons, append and concatMap operations. We can supply the
cross monad as the default monad instance. An appending monad is also possible
using a result type.

Data.Pipe (fused, consumer and producer)
Data.Pipe.CPS (fused)

Data.Fold
Data.Fold.CPS
Data.Fold.Chunked
Data.Fold.Concurrent
Data.Refold (fused with nesting)

Data.Parser
Data.Parser.CPS
Data.Parser.Chunked

Remove the rewrite rules from Data.Stream and just make it direct streams. We
can always add rewrite rules in future, but removing would be disruptive.

One problem with rewrite rules is the problem in recursive use of functions
that are using fromStreamK/toStreamK.

This will also solve the problem of using append2, interleave2 etc, now we can
have the direct versions and CPS versions of these.

We do not usually need to convert CPS to direct. But it can be useful in some
direct style operations if one stream is CPS and other is direct. We can
possibly get some perf boost by fusing the direct style stream in that
operation. e.g. appendD or zipWithD operations.

The exception operations are only supported by StreamD. Therefore, if we want
to use exception operations for StreamK we need to convert it to D. This means
we require both ways conversion.

Nested may not be the correct characterization for CPS streams, because we can
do nesting with Unfolds as well. But unfolds require a visible state. Also CPS
can do infinite appends but unfolds cannot. Just call it Data.Stream.CPS? or
Data.Stream.Functional (vs Data for basic streams).

Type naming:
  StreamK - CPS (ContStream)
  StreamZ - zipping (ZipStream)
  StreamC - Chunked (ArrayStream)

## Stream Types

Possible types and how they can possibly and usefully compose:

```
Stream m,output
    => output nesting (Stream m a)
Stream m,output,result
    => output nesting, output appending
Stream m,output,state injection
    => output nesting, static optimization (Unfold m a b)
Stream m,output,state injection/extraction
    => unfolding (stop and resume) (Producer m a b)

Fold m,input
    => distributing (Sink m a)
Fold m,input,result
    => distributing, splitting (Fold m a b)
Fold m,input,result,state injection
    => distributing, splitting, static optimization (Refold m a b)
```

Fold

Even though Source and Sink are the duals, because of our programming model,
Source makes sense as a default type and Fold makes more sense than Sink as
a default type.

The ListT style monad has the drawback that the second stream can be run
multiple times, which in some cases may not work as expected because the stream
is stateful. Can the appending monad be more useful than ListT?

# Stream combining operations

-- Consumer => Refold? Intuitive dual to Unfold. Consumer is too general and
includes Fold as well.

-- Use the term iterate for applying function like types e.g. Unfold or Refold.
And use "many" for applying data like types e.g. Stream or Fold?

* Unfold -> Unfold -> Unfold (many) -- expand/concat
* Unfold -> Stream -> Stream (unfoldMany) -- expandMany/concatMany, iterating
* Unfold -> Stream -> Stream -- sequencing unfolds/chain/sequence
    -- Not sure how to do this but the idea is to apply different unfolds to
    -- different elements in the stream.
    -- Is stream Zip the dual of fold append?

* Stream -> Stream -> Stream (serial et al)
* Stream -> Fold -> Stream (foldMany) -- iterating
* Stream -> Fold -> Stream -- foldOn, sequencing

* Stream -> Pipe -> Stream -- iterating/sequencing versions
* Pipe -> Pipe -> Pipe
* Pipe -> Fold -> Fold -- iterating/sequencing versions

* Fold -> Stream -> Fold (toFold) -- streamOn, sequencing
* Fold -> Stream -> Fold -- streamMany, iterating
* Fold -> Fold -> Fold (splitWith et al) -- serial

* Fold -> Consumer -> Fold (appendConsumer) -- sequencing consumers/chain
* Fold -> Consumer -> Fold  -- consumeMany/reduceMany -- iterating a consumer
* Consumer -> Consumer -> Consumer (append) -- reduce

Parser is an extension of Fold adding seeking (backtracking) and error.
Zipper (Source) is an extension of Stream adding seeking and error.

data Step s a =
    Yield a s
  | Skip s
  | Stop
data Stream m a =
    forall s. UnStream (State K.Stream m a -> s -> m (Step s a)) s

-- A file zipper could natively seek while a stream can be buffered in memory
-- to convert it into a zipper

data Seek s =
    Partial Int s   -- Move back and trim
  | Continue Int s  -- Move back

Zipper m a = Zipper
  (s -> m (Step s a))  -- step
  (Seek s -> m s)      -- operation on buffer
  s

----

To avoid the left associated <> issue we should only allow a stream of streams
using cons and then concat it to get a stream of elements.

cons :: Stream m a -> Stream m (Stream m a) -> Stream m (Stream m a)
concat :: Stream m (Stream m a) -> Stream m a

The problem with that is that `serial` will now look like:

```
s1 `serial` s2
concat $ s1 `cons` s2 `cons` nil
concat $ s1 `cons` pure s2
cat [s1, s2] -- this is an ergonomic alternative
tac [s1, s2] -- could do the same in reverse
catWith cfg [s1, s2] -- with SVar configuration
-- we can also possibly use overloaded lists

-- To be consistent with fromList, we can use catList
-- Or just use "list" and "cat" for these to be short?
fromList [a1,a2,...]
catList [s1,s2,...]
catListWith ?
```

In that case instead of "serial", "async" etc we will need "Serial.concat",
"Async.concat" etc.

-----

Pipes would be the equivalent of dlists providing both cons and snoc. But we
cannot run those without closing one end using a stream of fold just like
dlists. So that would be replacement of the builder type PR.

# Folds, Scans, Streams

Scans/Accumulators/Folds/Parsers
--------------------------------

We can upgrade an Accum to fold to parser.

We can at least have an internal representation for Accum and use accumulators
to implement sum/length etc folds.

Parser Done/Error in initial
----------------------------

Only monadic/applicative needs this. Should we use a different type to simplify
non-applicative use cases? In those cases initial can only have a Partial
result.

Parser scans
------------

Initial needs a "Continue" constructor to implement scans on parsers. As a
parser can always return a continue in initial when we feed the fold's initial
result to it.

post scanning folds can be much simpler, we call these scans. The initial
action of pos scanning folds need not return a Done or partial result, just a
state. There is no initial value in these. This can make operations like scan
and foldManyPost simpler for postscans because we can avoid a state due to
the special case of initialization.

Stateless and Stateful folds
----------------------------

The initial action in folds is monadic (m s) whereas in streams it is
non-monadic (s). That's the reason we need a separate "initial" step in folds
whereas in streams there is no initial state. However, "bracket" is more like
folds where we need a initialization state.

We can possibly have a fold type that has a non-monadic state, it will
be more like scans. We do not need an initialization. Folds with monadic
initialization and extraction would be like bracketed folds, which have
a initialization and cleanup. These can possibly be expressed using bracket and
refold? So we can separate the functionalities and modularize it. Folds without
monadic state can possibly be optimized better, especially when scanning
streams using folds.

Most of our folds have non-monadic initial, therefore, it would be easy
to make this change.

Use internal subtypes
---------------------

For example we can use an overall Fold as a consumer type but internally
we can have:

* Accumulator  -- Partial
* Terminator   -- Partial, Done
* Scanner      -- Partial, Done, Continue, Stop
* Parser       -- Partial n, Done n, Continue n, Stop n, Error
* Pipe         -- Partial, Done, Continue, Stop, Skip, Error

Continue => SkipOut
Skip => SkipIn

Then convert the fold into different types for type specific operations.
When converting we can statically check that a down conversion does not discard
a constructor.

If we use separate cases for Partial 0, Done 0, Continue 0, then the parser
code can be directly specialized to the fold code by the compiler.

Returning the remaining seed
----------------------------

Like in folds accumulator is returned by elimination. In streams seed is
returned by generation functions. For example, fromList would have a return
type of list. When we fold such a stream it would return the fold result and
the stream result (the remaining seed).

Unfold and Refold
-----------------

Unfold a m b -- take a seed of type b and return the same type

  Same as Producer m a b

  If we make the general Producer type as "Producer i a m b" then the current
  Unfold type would be equivalent to "Producer i a m ()". Just like ListT is
  equivalent to "Stream a m ()". So Unfold is to ListT as Producer is to
  "Stream a m b".

Refold a m b -- take an accumulator of type b and return the same type

  Same as Refold m b a b

The current Unfold type basically discards the result, so the result
type is (). That makes several compositions in Unfold possible which are
not easily possible in Producer type (e.g. concat). it is the same way
in which ListT discards the result.

Why Stream corresponds to Fold
------------------------------

Why Unfold is not the dual of Fold?

We can argue that Unfold generates a stream from an initial seed and
exactly in the same way a Fold reduces a stream to a final value. So
Unfold and Fold are duals.

But if we look at the API behavior - Stream concat does not fuse, in the same
way Fold concat does not fuse. However, unfoldMany fuses and in the same way
refoldMany fuses. Both Unfold and Refold have a static input which make these
similar concepts on producer and consumer sides.

If we make a small change in Stream type, we can see the duality in Stream and
Fold.

Stream m r a <=> Fold m a r

Stream m r a : generates a stream of type a with an opaque generator
state s whose initial value is opaque. The type r is the final extracted
result from the state. basically the remains of the state. We can use a
(extract :: s -> r) for that.

Similarly, in a Fold type the initial value of the state is opaque and we keep
building the state and finally extract the final result "r" using (extract :: s
-> r).

Duality can guide us what the types should be:

We get 4 useful variations:

MonadTrans
  List Monad/CoMonad
  Stream r m a  <------ (ListT m a) (List m a)
  Fold r m a            (SinkT m a) (Sink m a)

  Can we meaningfully use the type r in this case? if not then the type just
  becomes Stream m a, Fold m a.

  The way we iterate using a Fold, in the same way we can concat/iterate
  streams using the result of the previous stream in generating the next
  one.

  ResultMonad
  Stream a m r   -- Stream/Producer
  Fold a m r     -- Fold/Consumer

Profunctor/Category/Arrow
  List Monad/CoMonad
  Stream m r a
  Fold m r a

  Can we meaningfully use the type r in this case? if not then the type just
  becomes Stream m a, Fold m a.

  ResultMonad
  Stream m a r
  Fold m a r <------

We can use one standard type which is the most useful, and then we can have
other newtypes for alternative compositions.


When naming we also need to keep in mind that we need a consistent naming for
unfold/refold/parsers etc.

Summary of abstractions:
------------------------
--
-- Unfold - Generation with static input
-- Stream - combine multiple producers
-- Scan - transformation without termination (map and filter)
-- Fold - combine multiple consumers
-- Refold - elimination with static input
--
-- Generator - a more powerful stream (with seeking and failure)
-- Parser - a more powerful fold (with seeking and failure)
--
-- Pipe - can generate, transform or fold
--
-- Duality (Correspondence):
--
-- Unfold <=> Refold
-- Stream <=> Fold
-- Generator <=> Parser
-- Scan
-- Universal: Pipe
--
-- Nested transformation duality:
--
-- The stream abstraction can generate streams and multiply (expand) them
-- (nesting).  intersperse, concatMap etc. expand the streams. However, the
-- Skip constructor also allows reducing the streams (filtering). concatMap
-- itself can support filtering and no surprise that concatMap requires Skip
-- constructor to implement.
--
-- What is analogous to Skip in folds? We should be able to Skip consuming.
-- Where can something like this be useful? Implementing the consumer analogue
-- to concatMap i.e. consume many elements in one go (unfoldMany for folds).
-- That will require the driver to skip supplying input to the fold. The fold
-- will be consuming internally generated input.
--
-- Since you cannot have a monad instance without concatMap in streams, can you
-- have a comonad instance without Skip/unfoldMany in folds? With unfoldMany we
-- could consume an array stream one array at a time. Interestingly we can do
-- the same by using concatMap on the array stream and then consuming the
-- resulting stream.

{-
data Step s b =
      Partial b s
    | Skip b s
    | Continue s
    | Done !b

data Scan m a b =
    -- | @Scan@ @step@ @initial@ @extract@
    forall s. Scan
        (s -> m (Step s b))       -- producer
        (s -> a -> m (Step s b))  -- consumer
        (m (Step s b))            -- initial
        (s -> m (Step s b))       -- drain
-}

-- However, this type can be used to implement unfoldMany for folds without
-- having to use a recursive step function.
--
-- If we have a separate scan type with Partial, Continue and Done, we can use
-- a fold type without Continue i.e. Partial/Skip/Done. Note folds would become
-- dual to streams. Scan has no nesting, fold does not need Continue as we do
-- not scan using intermdiate values. Also scans would be used for scanning
-- with intermediate output whereas folds would be used to scan with terminal
-- output, that will also solve an oddity.
--
-- However, parsers with 5 constructors would become a tough beast.

Apply Unfold after a Fold
-------------------------

We can have the following:

* Fold before Fold => many
* Unfold before Unfold => many
* Unfold before Fold => convert (Fold m a b) to (Fold m f a b)
* Fold before Unfold => convert Unfold m a b to Pipe m c b
    Or we can just apply the fold on a stream (Stream -> Stream) and
    then apply unfold on the resulting stream. Using stream as the
    intermediate representation. But streams are not as composable.

Operations to implement
-----------------------

unfoldMany for folds - expand the container input in folds into its elements
before the fold consumes it. This is easy to do for non-terminating folds. Once
we have backtracking in folds then it should be possible for terminating folds
as well.

We can possibly generalize the element folds to a functor general fold.

(Fold m f a b) would expand (f a) to "a" before applying the fold. So f
should be unfoldable.

Extending scan to pipe
----------------------

Scan should always have a extract function. This is just an extension of
regular scans to provide the additional functionality of draining. That
way scan just becomes equivalent to Fold type. For example, we needed
scanlMAfter to achieve this functionality.

postscanlMAfter' :: Monad m
    => (b -> a -> m b) -> m b -> (b -> m b) -> Stream m a -> Stream m b

However, classify functions may have to emit a stream rather than a single
value "b" at extract, therefore, extract will become an unfold.

pipe :: Monad m
    => (s -> a -> m (Step s b)) -> m s -> (s -> m (Step s b)) -> Stream m a -> Stream m b

The Step of unfold and fold are the same. However, we need a handover
of state from the fold to the unfold. Therefore, we will need
extract/Stop/Done to return the state as well. This basically makes
these producer and consumer types. Or maybe in the pipe Done can just return
the unfold state s2, and the Stop of unfold can return the state s1 of the
fold. Or we can call it "Block" instead of Done/Stop. And then we can keep the
Done/Stop constructor to actually stop the pipe so it can also behave like an
unfold and a fold, making it the most general type encompassing all others.

So a pipe is essentially a combination of producer and consumer or ReUnfold and
ReFold.

When the fold part of the pipe is done we start the unfold from the state
returned by the fold. When the unfold part is done we start the fold from the
state returned by it, and so on. The pipe runs forever.

demuxDefaultWith
----------------

Terminating folds do not make sense to be used in demuxDefaultWith. We need to
run "initial" dynamically when a value arrives, but if initial returns DONE we
cannot backtrack. So only non-terminating ones will work.

Conclusions:
* terminating requires backtracking.
* parallel combinators (tee, demux, partition) do not work well with
  terminating folds because of unlimited and independent backtracking. Partial
  forever folds would work well in these.

We can make the Tee type as the partial fold type. And we can keep the Fold
type as a backtracking/terminating fold. A Tee can be upgraded to Fold and a
Fold can be upgraded to Parser.

Renaming:

* Fold (non-terminating)
* --- (terminating and backtracking but no error)
* Parser (with error)

If possible we can represent the terminating folds by parser itself?

Scan a stream using a Map of folds
----------------------------------

Compose folds in a Map. The resulting fold would be a fold that can skip
producing output using Continue. It would produce an output only when one of
the folds terminates. We can use such a fold to scan a stream. Currently only
parsers can use Continue. We can add Continue without backtracking to folds.

Concurrent Folds
----------------

Like in concurrent streams we make them by using a consM, in the same way we
can make concurrent folds by using a accumM accumulator function that
accumulates concurrently:

foldlS :: IsStream t => (t m b -> a -> t m b) -> t m b -> t m a -> t m b

t m b is the lazy representation of the output stream. It could be just the
buffer to which we append 'a'.

Note that the argument of foldlS is snoc which is just flipped cons.

Unfold vs Stream
----------------

Unfold has an additional static input. stream has only dynamic input that can
be passed to the combinators as argument.

The type of the input in case of Unfold is known, it is the type "a" in Unfold
m a b. However like stream we can also pass the input as argument to Unfold but
then it will behave in the same way as stream.

So the static input is constant? No, we can in fact provide it dynamically
using lmap on the unfold. This should work the same way as passing an argument,
but this would get optimized well?

Streams and folds duality
-------------------------

Even in streams we can have an initial/step/inject/done in general.

  - initial () => state (for Unfolds initial x => state)
  - step state => state
  - extract state => a (Something that you do at each step)
  - done state => ()

The way "extract" from the state works better for folds - can we do the same in
streams and will it work better, not threading the output in some cases,
instead just thread the state?

The initial and done can help implementing bracket.

The initial in streams is pure (i.e. s) and in folds we have it (m s). We can
get away with pure s in streams because we have Skip, we can always have a
initi state do something in that and then Skip to the next state. The Continue
in parsers can do the same? the driver would have to call the "step" again with
the same input.

The way in streams we do not need initial to have Step type can we do the same
in folds/parsers? We can use Skip to achieve it?

Direct vs Continuation folds
----------------------------

Direct folds have this flow:

  - initial () => state (for Fold2 initial x => state)
  - step state a => state
  - extract state => output (Something that you do at each step)
  - done state => () (for Fold2 done state => y)

In direct folds initial can be called even if there was not input to the fold.
However, continuations are always driven only by an input, so the equivalent of
initial may not be called even without an input.

There is no equivalent of done in CPS. One way of doing that would be
to always yield a state along with the output. And also have a "done"
continuation which will be called at exit. All the "done" continuations till
now will have to be composed and called as a chain. Just like the done in a
fold will initiate the done of the parent fold and so on. Similalrly, the
initial of a fold calls the initial of inner fold and so on.

The equivalent of initial in CPS would be to use an initial action that
generates the state without an input.

Fold docs
---------

technically we should have types for:

* Accumulators
* terminating parsers
* terminating parsers with leftover
* parsers with backtracking

However, to not introduce too much complexity we have folded the first three
types into a single type. Because of this in some cases we may have partial
functions and return errors because we are not enforcing some conditions by
types.

Types
-----

Duals:

Stream m a

-- Composability on output or input side
Fold m a b <=> Unfold m a b

-- Richer form of fold/unfold
Parser m a b <=> Generator m a b

Fold2 m c a b <=> Unfold2 m c a b

data Stream m a =
    forall s. Stream (s -> m (Step s a)) (m s)

data Unfold m a b =
    forall s. Unfold (s -> m (Step s b)) (a -> m s)

data Generator m a b =

-- emits a final value from the state (additional state output)
-- UnfoldEx/UnfoldX (Unfold extended)
data Unfold2 c m a b =
    forall s. Unfold2 (s -> m (Step s b)) (a -> m s) (s -> m c)

data Sink m a =
  forall s. Fold (s -> a -> m s) (m s)

data Fold m a b =
  forall s. Fold (s -> a -> m s) (m s) (s -> m b)

data Parser m a b =

-- takes an initial value as input (additional state input)
-- FoldEx/FoldX (Fold Extended)
data Fold2 c m a b =
  forall s. Fold2 (s -> a -> m s) (c -> m s) (s -> m b)

Pipe = Fold + Unfold with two existential states or a shared state.

Pipe/fold transform issue
-------------------------

Its not clear whether a Done or Done1 should be returned here. If the
Pipe is a one-to-one map then we can perhaps return a Done1 here. If the
pipe can produce n items for each input then the fold may have consumed
some inputs and some may have been left. If we have consumed some of the
output that pipe produced then we cannot say Done1.

For now maybe we can error out in this case, saying only accumulators or
folds not returning Done1 are supported by this operation.

concatSmapM vs unfoldrM
-----------------------

concatSmapM is a generalization of unfoldrM, we should make the signatures of
both consistent to make this more evident:

unfoldrM :: (s -> m (Maybe (a, s))) -> s -> t m a
smapM :: (s -> a -> m (s, b)) -> m s -> t m a -> t m b
sfilterM :: (s -> a -> m (s, Maybe b)) -> m s -> t m a -> t m b
concatSmapM0 :: (s -> a -> m (s, t m b)) -> m s -> t m a -> t m b
concatSmapM1 :: (s -> a -> m (Maybe (s, t m b))) -> m s -> t m a -> t m b
    -- this one has the ability to say "Done" without consuming all input
concatSmapM2 :: (s -> a -> m (Step s (t m b))) -> m s -> t m a -> t m b
    -- this is basically parseMany, it has the ability to backtrack
    -- this could be the general pipe, we can replace "t m b" with Unfold

To be fixed:
* monadic seed in unfoldrM
* (a, s) vs (s, a) in all
* m (s, Maybe b) vs m (Maybe (s, b)) vs m (t m (s, b))

(s, Maybe b) is the only possible choice in map, because we have to yield the state
whether or not we yield the value. By the same logic in unfoldrM it could be
(Maybe s, b), when we do not yield the state it stops. But its the same as
Maybe (s, b) too.

Pipe type
---------

stateful map and filter, this can only reduce the stream, but not expand it:

sfilterM :: (IsStream t, Monad m) =>
       (s -> a -> m (s, Maybe b))
    -> m s
    -> t m a
    -> t m b
sfilterM step initial stream =

This can consume and produce multiple items, so this is most general, like a
pipe. However, we cannot compose multiple of these.

concatSmapM :: (IsStream t, Monad m) =>
       (s -> a -> m (s, t m b))
    -> m s
    -> t m a
    -> t m b
concatSmapM step initial stream =

Unfold and Fold duality
-----------------------

Like Fold (Parsers) have a CPS representation for sequential consumption
we have a CPS representation (builder) for unfolds. We should have a CPS
representation for Unfolds and conversions from CPS to direct and vice
versa. Of course, since unfolds are functions they will naturally have a dlist
style representation.

Similarly, Generators will have a CPS representation, dual to parsers.

Distributing a stream to multiple folds
---------------------------------------

We can distribute an RT FD to multiple folds.

To distribute a stream to multiple folds we need a good referentially
transparent abstraction like a Zipper or RT FD and provide that RT
object to each fold.

We can fold the source stream to a zipper or file in memory. The head and tail
boundaries of the file/zipper are trimmed/grown based on the folds.

If all folds have consumed up to a certain point then the zipper will be
trimmed. If any fold needs to go beyond the latest point of the stream then the
zipper is grown.

Growing the zipper can be blocked if the buffer has grown beyond a specified
limit so that it does not grow arbitrarily consuming all memory. The waiters
are unblocked when the buffer is trimmed from the back.

We can treat this as an in-memory file or it can even be on disk or backed by
disk. The folds are like "tail"ing the file.

That's how we can also implement teeWith for parsers.

In fact such a zipper is the comonoid structure that we look for. It can be
copied in a referentially transparent manner.

We can fold a stream to Zipper just like we fold it to an SVar. And then use
the Zipper as a referentially transparent/shared structure across mutliple
folds in a comonadic composition.

A Zipper would be more like a SVar. SVar/Channel <-> Stream, Zipper <->
Array.  Channel is more like a distribution only without buffering for
backtracking whereas Zipper is buffering like a pool or dam and then
distribute.

Scan type
---------

@fvr do you want to take up creation of the Scan type? It would be
a strict subset of the Pipe type just like Fold is a strict subset
of Parser. It would be much simpler than the Pipe type. Most of the
transformations (one to one maps and filters) can be represented by this
type.

Pipes/Scans/Unfolds
-------------------

We can use concatUnfold on a stream. The unfold can unzip the element of the
stream into multiple elements or copy it to multiple elements, then apply
different transformations to each such streams and join them back. Thus we get
composable scans.

delay Fold
----------

Emit elements of a stream after a delay of n elements. This just needs
buffering of n elements and emit the oldest.

it would be a parse as it won't return any result until n elements. And then in
the end it needs to return results even without input to drain the buffer. Do
we need a mechanism to drain the parser? Instead of the extract function we can
use a "step" without input and we keep calling it until the parse returns a
Stop or Error. basically "extract" in a loop. more like unfold.

A more generalized form of it would be a pipe. The pipe can switch from consume
to produce mode at any time. So it basically has the "step" and "extract" in
loops built in.

Parsers/Fold/Unfolds/Generators
-------------------------------

Folds => comonads
Parsers => monads + comonads

Unfolds => monads
Generators => comonads + monads

Join of two streams
-------------------

* https://en.wikipedia.org/wiki/Relational_algebra
* https://en.wikipedia.org/wiki/Join_(SQL)
* https://sqlserverplanet.com/optimization/sql-server-join-algorithms
* http://dcx.sap.com/1101/en/dbusage_en11/join-methods-optimizer-queryopt.html

Joins
-----

We need to have combinators to represent all joins. For streams as well as
arrays. Look at APL as well.

* https://github.com/google-research/dex-lang another array processing lang

Nonempty
--------

NonEmpty list should be called "List1".
We can have NonEmpty streams as well using the 1 suffix.

Comonad
-------

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b

We can make it more useful by using a comonad in a monad like this:

class Functor (t m) => Comonad (t m) where
  extract :: t m a -> m a
  duplicate :: t m a -> t m (t m a)
  extend :: (t m a -> m b) -> t m a -> t m b

We can see that:
    * extract is a fold
    * extend is concatFold or splitting a stream using a fold.

When t m = Fold m a
extract :: Fold m a b -> m b
duplicate :: Fold m a b -> Fold m a (Fold m a b)
extend :: (Fold m a b -> m c) -> Fold m a b -> Fold m a c
    -- this is foldChunks :: Fold m b c -> Fold m a b -> Fold m a c

For the regular non-terminating fold type, (Fold m a b -> m c) could be the
terminating function. For example it can keep extracting the state from an
"any" fold and terminate when it returns True.

The extend function may make more sense for pipes. (Fold m a b -> m c) could be
a pipe folding function.
extend :: (Pipe m a b -> m c) -> Pipe m a b -> Pipe m a c

For example, we can have an "eq" pipe which will only scan the stream
for equality:

eq :: Monad m => (a -> Bool) -> Pipe m a Bool
eq predicate = ...

Producer and Consumer duals
---------------------------

Stream:
    Serial:

      append - next source starts producing when the first ends, if one errors
        out whole composition errors out.
      interleave - interleaved consumption

      Error handling:
        1) if one errors out the whole composition errors out.
        2) if one errors out the next one starts - all errors are returned in
           the end or we can have handlers for each error.
           or we can return a stream of errors along with the output stream.

    concurrent versions:
      ahead
      async

      Error handling:
        1) if one errors out whole composition errors out
        2) individual error handler or all errors are returned separately
           or we can return a stream of errors along with the output stream.

    Nested:
      concatMap/Applicative/Monad - nested loop based production
      Note that concatMap requires buffering the stream for reuse in the next
      iteration. Whereas concatUnfold can be used to generate the stream again
      and again from a pure seed.

      Error handling:
        1) if one errors out then the whole composition errors out
        2) if a stream errors out we still continue composing with as much as
           was produced till now. We return all the errors or have error
           handlers installed. Or we can return a stream of errors along with
           output stream.

    alternative - backtrack, buffer a source until it produces full output,
      discard, backtrack and start the next source if it errors out. For
      example, if we are downloading a file from one mirror and we fail
      then we delete the file and download from another mirror. or a
      search query. alternative is apt name for an alternative source.
      Or we could be reading from Raid mirror using either a fallback option or
      using a parallel read with race.

    Zip: take from both and zip both
    merge : take from both and reorder

Fold:

    In streams we can express a finite stream, an empty stream with Stop
    and a sequence otherwise. The Fold type is equivalent to infinite
    streams. If we add termination it will become equivalent to finite
    streams.

    There is no explicit error handling in streams, we have it through
    exceptions. If we add an error constructor as well it will become
    equivalent to parse. But even in parsers a Maybe return value would be
    enough we have an either only for describing the error. The Maybe return
    type of parser corresponds to the Stop constructor of the stream.

    Ok so what is the stream of dual of parser backtracking? Like we have the
    Back and Keep constructors in parsers what are the corresponding features
    in stream production?  In non-determinism we open up multiple stream
    sources and go through them to select the possible choices, that is the
    backtracking in streams. All these sources need to be buffered for
    backtracking. On the parser side as well may have multiple nested levels of
    buffering for backtracking. Here as well we have multiple levels of nested
    buffering.

    Similar to parsing, where using "Keep" we let the first level of buffering
    go, we can have a way in stream production to let the head of the stream go
    once we are done with the elements at the head of the stream at a given
    nesting level. Similar to "Keep" in a stream we can inform the source how
    much it can release

    If we have backtracking in streams we can compose a stream from alternative
    sources. This could be useful in generative producers (e.g. generating a
    Haskell program syntax) dual to parsers. If we have multiple possible
    choices we can try one and go back if that does not produce the right
    structure. Isn't that non-determinism? Is the dual of parser backtracking,
    non-determinism?

    yes, non-determinism is the dual of parsing. non-determinism is a
    tool for generation of valid outputs that the parser can accept. We
    select from a number of possible choices. We may need a selection
    function there instead of selecting in a prespecified manner. We may
    choose at random or we may shuffle every time.  Or we may split a
    domain into multiple ranges. So a selection function is something we
    may want to attach to each stream source.

    Also, we may need a backtracking on error mechanism. We may select some
    choices which lead to a dead end. This corresponds to the error scenario in
    case of parsing. Here we need to go back and select different choices. The
    construction logic may have certain constraints which lead to an invalid
    combination.

    Like "Keep" and "Back" in parsing we can have "Forward" and "Backward" in
    non-determinism. For example if we are solving:

    x + y = 10

    Then if we have x = 1, and y is 2 in a sorted Int space then we can
    move y stream forward by 9 indices. if after that we find:

    x + y = 100

    Then we can move it back using some strategy e.g. mid point 1 + 9 / 2 = 5
    When we are not interested in a part of the input we can also say "Delete"
    with a range or set. This requires the input to be mutable. We can also
    have "Reset" to start from the beginning again.

    This can also help implement binary search. And if the streams are arrays
    this should be easy to move back and forward. Also we have primitives to
    get rid of the source buffer space that is not needed anymore. e.g. any
    space beyond index 10 can be dropped.

    Quicksort would be via parsers route. We can divide the input buffer
    in two parts and send it to two separate folds for mutation and each fold
    will keep on doing that. We will have a recursive combinator to be applied
    to do this  like we have concatMap/foldChunks etc. But we can also have a
    custom strategy of divide and rule to work on shared mutable buffers.

    We can call the stream type with feedback propagation a Generator vs
    Parser.

Pipes
-----

We represent unfold with an Unfold, a fold with a Fold. Now its time to
represent a map with a Map, a scan with a Scan and a concatMap with a
ConcatMap.

Map/FMap - stateless transformation
Scan - a stateful one to one transformation
Filter - ConcatMap can do filtering as well, but it may not be very efficient.
So we can have a Filter type for efficient composable filtering. this can be a
single replacement to "filter" and "lfilter".

ConcatMap - stateless one to many transformation. It can blow up one
element to a stream or it can skip many elements of the stream.
ConcatScan/Pipe - stateful one to many transformation.

we can also have a concatScan function that threads around a state in
concatMap. However it is just "usingStateT s . concatMap (a -> t (StateT m) b)"

similarly a map can be upgraded to a scan "usingStateT s . mapM (a -> (StateT m) b)"

Breaking pipes in two stages
----------------------------

A scan like composition would be to translate the input using a fold/scan and
it yields elements at times. Sometimes skips at other times yields depending on
the state.

Such a type is a bit limited as just takes decisions based on the past state.
To extend it further we can apply unfolds on the yielded elements. When
composing multiple such scans we can apply different folds on each branch/scan.
This would be a concatUnfold for scans. An unfold applied at the end of such a
type would give rise to a pipe.

Transformations
---------------

The output of a fold needs to be a stream, a stream of lazily built containers.
This output is then fed to the next unfold, this unfold would share the lazily
built container, and process its part that is built till now. The unfold would
then emit another stream of its output. it can again build a stream of lazily
built containers.

Take the example of camel casing. We need to first emit the lines in a file,
each line then need to be split in words. So we start with a "split and fold".
The folds in this would build a line as a lazy structure. This line should be
fed to another split which should be an unfold because we need to emit a stream
of words.

The first fold should be: Fold m a (f a)
The unfold should be: Unfold m (f a) word

Or the first fold can provide an unfold as output? Fold m a (Unfold m () word)
We should have combinators that can transform Fold m a b to Fold m a (Unfold m () c)
combine :: Fold m a b -> Unfold m b c -> Fold m a (Unfold m () c)

    S.splitWithSuffix (== '\n') (concatLineToWords (unfoldLineToWords m (Stream m Char) [Char]))

Unfold, Stream, Sink, Fold
--------------------------

* Unfold is a generalization of Stream where we can provide a seed.
* A Fold is generalization of Sink where we can consume the result.

We use unfold to convert an Unfold into a Stream.
We should use a "collect/consume" to convert a Fold into a Sink.

Our current Sink type is wrong. It should be:

data Sink m a =
  -- | @Fold @ @ step @ @ initial @
  forall s. Fold (s -> a -> m s) (m s)

Categories of transformations
-----------------------------

* Fold : stream -> m a
* Unfold : m a -> stream
* Pipe : stream -> stream
* Functor : m a -> m b

Unfold-Fold vs Fold-Unfold
--------------------------

Unfold followed by a Fold is a value transformation. We transform a value "a"
to a value "b". Whereas a Fold followed by an Unfold is a stream
transformation.

We can make the transformation routines take a Fold and Unfold or bundle them
in a Pipe and use the Pipe.

Transpose
---------

interleaving roundRobin n streams would transpose them. To transpose arrays we
can just stream the rows and interleave them and then chunk by column size and
turn the stream into an array of arrays.

We can add a transpose operation in stream module, to imitate the
Data.List.transpose.

interleaving/de-interleaving streams
------------------------------------

insertAfterEach => insertAfterAll => appendToAll
insertBeforeEach => insertBeforeAll => prependToAll?

-- this is a special case of interleave
intersperse
interspersePre/interspersePrefix
interspersePost/intersperseSuffix

One element goes to one fold and the next to the other fold. The fold with
interspersed elems can check if they are all the same and error out the whole
composition if it encounters a different one.
separate
separatePre
separatePost

Unfold naming
-------------

supply => input => both
supplyFirst => fstInput => inputFst => first
supplySecond => sndInput => inputSnd => second

discardFirst => asSnd => inputAsSnd => toSecond => asSecond
discardSecond => asFst => inputAsFst => toFirst => asFirst

Renaming
--------

parallel
parallelLeft
parallelMin (parallelRace)

wSerial
wSerialLeft
wSerialMin

Can we use MonadComprehensions to use joins in a more idiomatic manner?

Modularize the hashJoins, create a low level function that accepts one stream
and one hash. If someone has a prebuilt hash they can use this.

all/any/and/or for stream and fold composition
----------------------------------------------

parallel => parallelAll
parallelMin => parallelAny => parallelOr
parallelFst => parallelAbsorb => parallelWith

The same compositions we can do with fold's applicative compositions:
terminate when all folds terminate
terminate when any fold terminates
terminate when first fold terminates

Clocked/Sampled composition. We can sample all composed based on the first one
which is a clock. This will be a non-destructive read i.e. we can read the same
value multiple times until it is replaced by another value. So the read is
basically a peek. We can have a peek operation just like uncons. Or we can have
a combinator that turns a stream into a peeked stream i.e. it automatically
replenishes the value read.

zipWithLatest => sampleOn

# intersperse example in terms of merge

merge the stream and infinite separator stream. end by the first stream.

Combining streams
-----------------

We need a way to combine streams such that if one of the streams stop, then the
combined stream stops. For example in the "intervalsOf" implementation one of
the stream is infinite and the other one is finite. We need to stop when the
finite stream is over.

1) when the first one stops, the stream stops
2) when the second one stops, the stream stops
3) when either stops, the stream stops
4) when both stop, the stream stops

This is applicable to interleaved/parallel/async composition. This is not
applicable to serial/ahead composition.

Actually this is the dual of partial folds/parsers. In partial folds we need to
combine them in such a way that when one fold ends we end the composition.
Similalrly, this is the producer side dual of that, if one stream ends early we
end the composed stream.

Destructive vs non-destructive read
-----------------------------------

destructive read/extract on a fold would read the current value of the fold and
hren reinitialize the fold to start accumulating again. This can be useful in
many cases. This is especially useful in sampling. That's why extract needs to
be monadic. Some folds when read again and again would give a different value
every time.

The simplest could be a "time" or "clock" fold. When we sample the time fold it
would give the curren time. We can set the frequency of the fold and it would
update the time according to the frequency or tick.

Consider a lastN fold. We may want to sample the fold at some time to get the
lastN values till that time. If it is an array the extract function would
freeze/copy the array and give a snapshot at that time. The driver woul keep
driving and pushing the elements in the ring buffer.

Similarly consider a firstN fold. the fold may keep accumulating, but we can
read at some point and the fold starts accumulating from the next element
instead of appending to the old. This is tumbling window like behavior.
Another variant of it could be non-destructive read where we keep on
accumulating even after read.

A non-destructive lastN/firstN read on each element push would be like scanl
and scanr.

We can provide a "sample" function just like "runFold" function. Sample would
extract and reinitialize the fold. Assuming the fold is continuously being
driven automatically.

These are actually pipes where we are pushing from one end and lazily
evaluating from the other.

- A scan generates an output on every push to the fold.
- A fold on demand non-destructive
- A sample on demand destructive

SVar as a pipe
--------------

An SVar is actually a special type of pipe. One side of it is a fold which
accumulates the input in a list/array. The other side of the SVar is an unfold.
The fold side is thread-safe as multiple threads can drive the fold at the same
time. The unfold side is thread safe against multiple writers. This is actually
a multiple writer single reader pipe.

Interleaving
------------

-- In general interleave m elements of first stream and n elements of second
-- stream. it is element unaware.
insertAfterEach (interleave a (repeat b))
insertBeforeEach (interleave (repeat b) a)
insertBetweenEach (intersperse)

-- element aware interleaving is mergeBy

deinterleaving
--------------

-- in general element unaware deinterleave m elements of first stream and n
-- elements of second stream.

removeBetweenEach (deintersperse)
removeBeforeEach (deinterleavePrefixed)
removeAfterEach (deinterleaveSuffixed)
deinterleave (send alternate elements to different folds) (partition/alternate)

-- in general element aware deinterleave will have two consumers, one for a
-- separator and one for the rest. The separator fold can tell till what point
-- definitely does not belong to the separator (this can be done via the
-- Continue or Yield result everytime it consumes an element) and that can be yielded to the
-- other consumer. The rest of the input remains buffered until the separator
-- fold tells whether it matches the separator or not. If it matches then the
-- separator fold consumes it and the buffer can be released and we start fresh
-- again. This can be used to split streams in general.

We can either merge back the results of the two folds, in which case it becomes
split keeping the separators, or we can drop one of the streams and it becomes
split without separators. To merge back we need pipes.

-- We can use these combinators to achieve the dropping versions of split:
-- Sequences (find and remove sequences wherever they are found in the stream)
removeInfixes
removeSuffixes
removePrefixes

Unfold/Transform/Fold
---------------------

Currently we have

* Streams as sources or producers - only mapped covariantly
* Scans (scanl/scanr) as transformation - mapped covariantly or contravariantly
* Folds for elimination or consumption - only mapped contravariantly.

Our source/producer can perhaps be represented as Unfoldr data type dual to the
Fold data type on the consumption side. Does Unfoldl also make sense for a
source. Perhaps the way foldr does not make sense at the elimination end
unfoldl does not make sense at the source end. In the middle both foldr and
foldl can happen. foldr transforms/reconstructs and foldl is again lifted into
a stream via a scan.

Sinks are just a special case of Folds. What is the dual of a Sink on the
production side? A producer that does not take an input to produce?

Scan is the most general. We should be able to implement Stream as well as Fold
in terms of Scan. Sink can be implemented in terms of Fold.

* https://hackage.haskell.org/package/deferred-folds-0.9.10

Filtering Scans?
----------------

Can we have scans that perform filtering as well? For example, when we are
doing a buffered groupBy we can emit the a result only when the group is
complete and not on every input. General scans emit a value on each input.

unfold/scan
-----------

StreamD is in fact an Unfold with a step and state.

like a Sink can be turned into a Fold, we should be able to turn an unfold into
a scan. or is it a Scan into an Unfold?

can we use an Unfold type and generate a stream using an "unfold" on an Unfold
type just like we use fold on a "Fold" type? We can also call it "Source" the
way we use "Sink".

unfold and generator
--------------------

Should we be using an unfold and a Generator type for priduction side u he way
we use fold and Fold type on the consumer side? Genertor would be a covariant
functor and we can the perhaps combine the covariant and contravariant functors
in many different ways. See the contravaiant package Compose module.

The Generators can be combined to produce Generators the same way we combine
Folds.

