# Streaming Pipelines as Functional Loops

Streaming pipelines are the functional equivalent of loops in imperative
programming.  In imperative programming when we have to process a
sequence of data items we run a loop over all the items. Each iteration
of the loop examines a single element of data to change the state of the
program or produce an output.

<!-- Write a C loop equivalent to the stream code below -->

## Data as Stream

In the streaming paradigm, we represent the user data that we have to
loop over as a stream.  A stream is a representation of potentially
infinite sequence of data items.

A finite stream consisting of integer data elements 1,2,3:

```haskell
>>> s1 = Stream.enumerateFromTo 1 3 :: Stream IO Int
```

An infinite stream consisting of integers from 1 to infinity:

```haskell
>>> s2 = Stream.enumerateFrom 1 :: Stream IO Int
```

## Stream Transformation

We have represented data as stream, now we have to operate on it. To
operate on the data in the stream we use stream transformation
functions, for example, if we have to increment each element in the
stream by one we use the `fmap` function. After applying the `fmap (+1)`
function on the stream we get an output stream in which each element of
the input stream is incremented by 1. Stream `s3` consists of elements
2,3,4:

```haskell
s3 =
    Stream.enumerateFromTo 1 3 -- Stream IO Int
      & fmap (+1)              -- Stream IO Int
```

Another example of a stream transformation operation is `take`, it trims
the stream to the specified number of elements. For example, the stream
`s4` in the code below consists of only two items 1,2:

```haskell
s4 =
    Stream.enumerateFrom 1 3 -- Stream IO Int
      & Stream.take 2        -- Stream IO Int
```

## Modular Streaming Operations

Similar to `fmap` and `take` there are many transformation operations
available in the library to perform different type of operations on the
stream. Each operation performs a specific job. We can combine multiple such
operations successively in a pipeline of operations to perform a desired
operation on the stream. For example, if we want to increment each element by 1
and want to take only first two items, then we can do it as follows. The
resulting stream `s5` consists of items 2,4.

```haskell
s5 =
    Stream.enumerateFromTo 1 3 -- Stream IO Int
      & fmap (+1)              -- Stream IO Int
      & Stream.take 2          -- Stream IO Int
```

Each stream transformation operation maintains its own internal state
which is hidden from the programmer. In an imperative language loop we
will have to maintain the state explicitly in a monolithic loop, for
example if we have to stop after processing 2 elements we will have to
maintain a counter and check that counter in each iteration. Here we are
able to hide the state locally in each transformation operation and we
can build a larger processing loop by putting together smaller parts in
a modular fashion.

The programmer only needs to pick the operations required for the
job and put them together. The resulting code is highly modular,
maintainable and readable. You can add an operation in the pipeline
without worrying about breaking anything else as the state for each
operation is private and cannot be meddled with by the programmer.

## Stream Consumption

Previously, we looked at operations that transform a stream to another
stream. Now let us look at another class of operations that consume a
stream and produce a single value or a single structure from it. This is
known as consuming the stream or eliminating the stream. The entities
that consume the stream are called consumers or folds (they help fold a
stream into a single value).

The `sum` fold consumes a stream of integers and adds them all, when
done it returns the sum. The `Stream.fold` operation takes a stream
and a fold and connects them together, feeding the stream to the fold
and then returning the result. We refer to this operation as the fold
driver. The variable `total` contains the value `1+2+3` i.e. 6.

```haskell
total <-
    Stream.enumerateFromTo 1 3 -- Stream IO Int
      & Stream.fold Fold.sum   -- IO Int
```

## The Streaming Pipeline

Using all the operations described above we can create a data processing
pipeline that increments each data item one by 1, takes the first two
elements, adds them and prints the result:

```haskell
main =
    Stream.enumerateFromTo 1 3 -- Stream IO Int
      & fmap (+1)              -- Stream IO Int
      & Stream.take 2          -- Stream IO Int
      & Stream.fold Fold.sum   -- IO Int
      >>= print                -- IO ()
```

The following snippet provides a simple stream composition example that reads
numbers from stdin, prints the squares of even numbers and exits if an even
number more than 9 is entered.

``` haskell
import qualified Streamly.Data.Stream as Stream
import Data.Function ((&))

main = Stream.drain $
       Stream.repeatM getLine
     & fmap read
     & Stream.filter even
     & Stream.takeWhile (<= 9)
     & fmap (\x -> x * x)
     & Stream.mapM print
```

## Data Flow Programming

We have demonstrated above how you can compose a pipeline of functions
or stream processors to process an input stream of data to produce an
output stream. We call it a form of dataflow programming as data flows
through the processing logic. In imperative programming there is no
clear separation of data and logic. The logic can arbitrarily examine
and mutate data which creates a problem due to complex interleaving of
state and logic in the program.

Imperative looping is a low level and monolithic concept, it is
difficult for programmers to implement and is error prone. Whereas
streams are high level, declarative, structured and modular way of
expressing what you usually do with low level loops. Streams allow
you to write different parts of the loop as modular combinators and
then compose them to create bigger loops. Streamly uses stream fusion
optimizations to ensure that the composed loop has the same performance
as a hand-written monolithic loop.

## Performance Optimizations

The streaming pipeline is translated into actual imperative loops by
the Haskell compiler GHC. The programmer writes a very high level
declarative code while the heavy lifting is done by the compiler to fuse
all that code together in a tight imperative loop.

You might think that the code above may not perform as well as a
handwritten code for the same job. But you will be surprised how
efficiently GHC optimizes this code using a technique called stream
fusion which is based on two important optimizations done by GHC namely,
case-of-case and spec-constructor. There are no intermediate streams,
no constructor allocations, all heap values are unboxed while being
processed in the loop, thus resulting in the same code as a C compiler
would generate from a handwritten C loop. Essentially, the imperative
loop is written by the compiler using the high level instructions
provided by the programmer.
