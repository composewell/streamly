# Streams

THIS DOC IS NOT READY.

<!-- Composing and processing streams -->
<!-- TODO add interworking examples -->

## Stream and lists

Streamly simplifies streaming and makes it as intuitive as plain lists. Unlike
other streaming libraries, no fancy types are required.  Streamly is simply a
generalization of Haskell lists to monadic streaming optionally with concurrent
composition. The basic stream type in streamly `SerialT m a` can be considered
as a list type `[a]` parameterized by the monad `m`. For example, `SerialT IO
a` is a moral equivalent of `[a]` in the IO monad. `SerialT Identity a`, is
equivalent to pure lists.  Streams are constructed very much like lists, except
that they use `nil` and `cons` instead of `[]` and `:`.  Unlike lists, streams
can be constructed from monadic effects, not just pure elements.  Streams are
processed just like lists, with list like combinators, except that they are
monadic and work in a streaming fashion. In other words streamly just completes
what lists lack, you do not need to learn anything new. Please see [streamly vs
lists](/docs/User/HowTo/streamly-vs-lists.md) for a detailed comparison.

Not surprisingly, the monad instance of streamly is a list transformer, with
concurrency capability.

## The Stream and StreamK Types

The stream API offered by Streamly is similar to Haskell lists' API, we
can say that streams are more powerful and effectful lists. If performance
matters we should prefer using the `Stream` type if the size of the stream is
statically known, for dynamic construction the `StreamK` type should be used.

## Generating

Streams can be generated from:

* In memory storage containers e.g. lists or arrays
* From persistent storage, handles, files or directories, database
* From network, sockets
* From functions, algorithmic generation using a computation

Streams from different sources can be combined together into a single
stream or a single stream can be split into multiple streams, each
stream transformed in a different way and then the results combined back
into a single stream.

'nil' represents an empty stream and 'consM' or its operator form '|:' adds
a monadic action at the head of the stream.

>>> Stream.toList Stream.nil
[]

Stream.toList $ getLine |: getLine |: Stream.nil
hello
world
["hello","world"]

To create a singleton stream from a pure value use 'fromPure' and to
create a singleton stream from a monadic action use 'fromEffect'.

>>> Stream.toList $ Stream.fromPure 1
[1]

Stream.toList $ Stream.fromEffect getLine
hello
["hello"]

To create a stream from pure values in a 'Foldable' container use
'fromFoldable' which is equivalent to a fold using 'cons' and 'nil':

>>> Stream.toList $ Stream.fromFoldable [1..3]
[1,2,3]

>>> Stream.toList $ Prelude.foldr Stream.cons Stream.nil [1..3]
[1,2,3]

To create a stream from monadic actions in a 'Foldable' container just use a
right fold using 'consM' and 'nil':

>>> Stream.drain $ Prelude.foldr (|:) Stream.nil [putStr "Hello ", putStrLn "world!"]
Hello world!

For more ways to construct a stream see the module "Streamly.Prelude".

## Eliminating

We have already seen 'drain' and toList to eliminate a stream in the
examples above.  'drain' runs a stream discarding the results i.e. only
for effects.  'toList' runs the stream and collects the results in a list.

For other ways to eliminate a stream see the @Folding@ section in
"Streamly.Prelude" module.

## Transformation

Transformation over a stream is the equivalent of a @for@ loop construct in
imperative paradigm. We iterate over every element in the stream and perform
certain transformations for each element.  Transformations may involve
mapping functions over the elements, filtering elements from the stream or
folding all the elements in the stream into a single value. Streamly streams
are exactly like lists and you can perform all the transformations in the
same way as you would on lists.

Here is a simple console echo program that just echoes every input line,
forever:

>>> :{
echo =
      Stream.repeatM getLine
    & Stream.mapM putStrLn
    & Stream.drain
:}

The following code snippet reads lines from standard input, filters blank
lines, drops the first non-blank line, takes the next two, up cases them,
numbers them and prints them:

>>> import Data.Char (toUpper)
>>> :{
main =
      Stream.repeatM getLine
    & Stream.filter (not . null)
    & Stream.drop 1
    & Stream.take 2
    & fmap (map toUpper)
    & Stream.zipWith (\n s -> show n ++ " " ++ s) (Stream.fromFoldable [1..])
    & Stream.mapM putStrLn
    & Stream.drain
:}


## Semigroup

We can combine two streams into a single stream using semigroup composition
operation '<>'.  Streams can be combined in many different ways as described
in the following sections, the '<>' operation behaves differently depending
on the stream type in effect. The stream type and therefore the composition
style can be changed at any point using one of the type combinators as
discussed earlier.

## Serial

The 'Semigroup' operation '<>' of the 'Serial' type combines the two streams
in a /serial depth first/ manner. We use the 'fromSerial' type combinator to
effect 'Serial' style of composition. We can also use an explicit 'Serial'
type annotation for the stream to achieve the same effect.  However, since
'Serial' is the default type unless explicitly specified by using a
combinator, we can omit using an explicit combinator or type annotation for
this style of composition.

When two streams with multiple elements are combined in this manner, the
monadic actions in the two streams are performed sequentially i.e. first all
actions in the first stream are performed sequentially and then all actions
in the second stream are performed sequentially. We call it
/serial depth first/ as the full depth of one stream is fully traversed
before we move to the next. The following example prints the sequence 1, 2,
3, 4:

>>> stream1 = print 1 |: print 2 |: Stream.nil
>>> stream2 = print 3 |: print 4 |: Stream.nil
>>> Stream.drain $ stream1 <> stream2
1
2
3
4

All actions in both the streams are performed serially in the same thread.

The polymorphic version of the binary operation '<>' of the 'Serial' type is
'serial'. We can use 'serial' to join streams in a sequential manner
irrespective of the type of stream:

>>> Stream.drain $ stream1 `Stream.serial` stream2
1
2
3
4


## Interleaved

The 'Semigroup' operation '<>' of the 'WSerial' type combines the two
streams in a /serial breadth first/ manner. We use the fromWSerial type
combinator to effect 'WSerial' style of composition. We can also use the
'WSerial' type annotation for the stream to achieve the same effect.

When two streams with multiple elements are combined in this manner, we
traverse all the streams in a breadth first manner i.e. one action from each
stream is performed and yielded to the resulting stream before we come back
to the first stream again and so on.
The following example prints the sequence 1, 3, 2, 4

>>> stream1 = print 1 |: print 2 |: Stream.nil
>>> stream2 = print 3 |: print 4 |: Stream.nil
>>> Stream.drain $ Stream.fromWSerial $ stream1 <> stream2
1
3
2
4

Even though the monadic actions of the two streams are performed in an
interleaved manner they are all performed serially in the same thread.

The polymorphic version of the 'WSerial' binary operation '<>' is called
'wSerial'. We can use 'wSerial' to join streams in an interleaved manner
irrespective of the type, notice that we have not used the fromWSerial
combinator in the following example:

>>> Stream.drain $ stream1 `Stream.wSerial` stream2
1
3
2
4

Note that this composition cannot be used to fold infinite number of streams
since it requires preserving the state until a stream is finished.

## Monoid

We can use 'Monoid' instances to fold a container of streams in the desired
style using 'fold' or 'foldMap'.  We have also provided some fold utilities
to fold streams using the polymorphic combine operations:

* 'concatFoldableWith' is like 'fold', it folds a 'Foldable' container of
streams using the given composition operator.
* 'concatMapFoldableWith' is like 'foldMap', it folds like
@concatFoldableWith@ but also maps a function before folding.
* 'concatForFoldableWith' is like @concatMapFoldableWith@ but the container
argument comes before the function argument.

All of the following are equivalent:

>>> :{
traced = Stream.fromEffect . print
main = do
 Stream.drain $ foldMap traced [1..10]
 Stream.drain $ Stream.concatFoldableWith Stream.serial (map traced [1..10])
 Stream.drain $ Stream.concatMapFoldableWith Stream.serial traced [1..10]
 Stream.drain $ Stream.concatForFoldableWith Stream.serial [1..10] traced
:}


## Nesting

Till now we discussed ways to apply transformations on a stream or to merge
streams together to create another stream. We mentioned earlier that
transforming a stream is similar to a @for@ loop in the imperative paradigm.
We will now discuss the concept of a nested composition of streams which is
analogous to nested @for@ loops in the imperative paradigm. Functional
programmers call this style of composition a list transformer or @ListT@.
Logic programmers call it a logic monad or non-deterministic composition,
but for ordinary imperative minded people like me it is easier to think in
terms of good old nested @for@ loops.

## Monad

In functional programmer's parlance the 'Monad' instances of different
'IsStream' types implement non-determinism, exploring all possible
combination of choices from both the streams. From an imperative
programmer's point of view it behaves like nested loops i.e.  for each
element in the first stream and for each element in the second stream
execute the body of the loop.

The 'Monad' instances of 'Serial', 'WSerial', 'Async' and 'WAsync'
stream types support different flavors of nested looping.  In other words,
they are all variants of list transformer.  The nesting behavior of these
types correspond exactly to the way they merge streams as we discussed in
the previous section.


## regularSerial

The 'Monad' composition of the 'CrossStream' type behaves like a standard list
transformer.

Let's start with an example with a simple @for@ loop without any nesting.
For simplicity of illustration we are using streams of pure values in all
the examples.  However, the streams could also be made of monadic actions
instead.

>>> :{
Stream.drain $ unCrossStream $ do
    x <- CrossStream (Stream.fromFoldable [3,2,1])
    CrossStream (Stream.fromEffect $ print x)
:}
3
2
1

As we can see, the code after the @fromFoldable@ statement is run three
times, once for each value of @x@ drawn from the stream. All the three
iterations are serial and run in the same thread one after another. In
imperative terms this is equivalent to a @for@ loop with three iterations.

We can write the console echo program that we wrote earlier using the monad
instance:

>>> :{
main =
    Stream.drain $ unCrossStream $ do
        x <- CrossStream (Stream.repeatM getLine)
        CrossStream (Stream.fromEffect $ putStrLn x)
:}

When multiple streams are composed using this style they nest in a DFS
manner:

>>> :{
Stream.drain $ unCrossStream $ do
  x <- CrossStream (Stream.fromFoldable [1,2])
  y <- CrossStream (Stream.fromFoldable [3,4])
  CrossStream (Stream.fromEffect $ print (x, y))
:}
(1,3)
(1,4)
(2,3)
(2,4)

i.e. inner loop iterations ((1,3), (1,4)) are executed before we proceed to
the next iteration of the outer loop ((2,3), (2,4)). This behaves just like
nested @for@ loops in imperative programming.

Notice that this is analogous to merging streams of type 'Serial' or merging
streams using 'serial'.

## interleavedNesting

TBD: generate the WSerial type using the TH utils.

The 'Monad' composition of 'WSerial' type interleaves the iterations of
outer and inner loops in a nested loop composition.

>> :{
Stream.drain $ getWSerial $ do
     x <- WSerialStream (Stream.fromFoldable [1,2])
     y <- WSerialStream (Stream.fromFoldable [3,4])
     WSerialStream (Stream.fromEffect $ print (x, y))
:}
(1,3)
(2,3)
(1,4)
(2,4)

Note that (2,3) is preferred to (1,4).  This works exactly the same way as
the merging of two streams using 'wSerial' works.

The fromWSerial type combinator can be used to switch to this style of
composition. Alternatively, a type annotation can be used to specify the
type of the stream as 'WSerial'.


## monadExercise

>>> :{
composed :: Stream IO ()
composed = unCrossStream $ do
    sz <- CrossStream sizes
    cl <- CrossStream colors
    sh <- CrossStream shapes
    CrossStream (Stream.fromEffect $ print (sz, cl, sh))
    where
    sizes  = Stream.fromFoldable [1, 2, 3]
    colors = Stream.fromFoldable ["red", "green", "blue"]
    shapes = Stream.fromFoldable ["triangle", "square", "circle"]
:}


## functor

'fmap' transforms a stream by mapping a function on all elements of the
stream. 'fmap' behaves in the same way for all stream types, it is always
serial.

>>> (Stream.toList $ fmap show $ Stream.fromFoldable [1..10]) >>= print
["1","2","3","4","5","6","7","8","9","10"]

Also see functions 'mapM' and 'sequence' from "Streamly.Prelude" module
which can map actions concurrently depending on the type of the input stream.

## applicative

Applicative is precisely the same as the 'ap' operation of 'Monad'. For
zipping applicatives separate types 'ZipStream' and 'ZipAsync' are
provided.

The following is an example of 'CrossStream' applicative, it runs all iterations
serially:

>>> p n = Stream.fromEffect (print n >> pure n)
>>> s1 = p 1 <> p 2
>>> s2 = p 3 <> p 4

>>> (Stream.toList $ unCrossStream $ (,) <$> CrossStream s1 <*> CrossStream s2) >>= print
1
3
4
2
3
4
[(1,3),(1,4),(2,3),(2,4)]

Similarly, 'WSerial' applicative runs the iterations in an interleaved
order but being serial it too takes a total of 17 seconds:

>> (Stream.toList $ Stream.WSerialStream $ (,) <$> WSerialStream s1 <*> WSerialStream s2) >>= print
1
3
2
3
4
4
[(1,3),(2,3),(1,4),(2,4)]

## zipping

Zipping is a special transformation where the corresponding elements of two
streams are combined together using a zip function producing a new stream of
outputs. Two different types are provided for serial and concurrent zipping.
These types provide an applicative instance that can be used to lift
functions to zip the argument streams.
Also see the zipping functions in the "Streamly.Prelude" module.

## serialzip

The applicative instance of 'ZipSerial' type zips streams serially.
'fromZipSerial' type combinator can be used to switch to serial applicative
zip composition:

>>> p n = Stream.fromEffect (print n >> pure n)
>>> s1 = Stream.fromSerial $ p 1 <> p 2
>>> s2 = Stream.fromSerial $ p 3 <> p 4
>>> (Stream.toList $ Stream.fromZipSerial $ (,) <$> s1 <*> s2) >>= print
1
3
2
4
[(1,3),(2,4)]


## furtherReading

* Read the concurrent streams tutorial
* See the examples in <https://github.com/composewell/streamly-examples streamly-examples> repo.
