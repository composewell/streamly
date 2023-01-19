<!--
(c) 2022, Composewell Technologies.
SPDX-License-Identifer: BSD-3-Clause
-->

# The IO type

IO is a built-in type for performing IO actions in Haskell. An IO may be
printing something on console, reading/writing to a file or network. All
functions that perform IO have a signature that returns `IO a`.

`IO a` represents an action that when executed returns a value of
type `a`.  Unlike pure Haskell functions a function returning IO is
stateful. As we discussed in the Haskell lazy evaluation section, there
is no particular sequence in which expressions are evaluated in Haskell,
parts of expressions are evaluated on-demand as needed. The IO type
ensures that actions that are composed using IO are evaluated in the
same order as specified by the programmer.

Also, any function calling an IO function must have the type IO, it is
enforced by Haskell. Thus,  the IO type guarantees that all IO actions in your
entire program are sequenced. This ensures that the effect of any action
is visible to the next action. For example if the previous action wrote
something to a file, the next action should be able to read what the previous
action wrote.

The IO type is a Monad, Monad types can compose actions using a `do`
block. The meaning of the `do` block depends on the particular Monad
type. In IO Monad the `do` block is used to sequence actions. For
example:

```
main = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn name
```

In this example, the three actions are strictly sequenced. "What is your name?"
would be printed on the console before we try to read something using
`getLine`. In other words, the IO type lets you execute instructions just like
in an imperative language, one after the other instead of the Haskell's natural
need based evaluation.

## Looping with Lists

In functional programming we avoid explicit loops using recursion, as we
saw in the lists chapter of this tutorial, we represent the sequence of
data we operate on using lists, and then we use functional combinators
like map, fold and filter on the list as an alternative to the looping
constructs as in imperative languages.

```
main =
    [1..3]       -- [Int]
      & map (+1) -- [Int]
      & take 2   -- [Int]
      & sum      -- Int
      & print    -- IO
```

That's an elegant way to write loops. Loops are composed as a sequence
of pipelines, each pipeline performing a particular transformation
(`map` and `take`) on the data flowing through the loop and finally we
fold (`sum`) the data as we want.

## IO Looping with Lists

The above example, works elegantly when we are transforming the data
using pure (non-IO) functions. But in almost all practical applications
we would like to perform some IO in the loop.  We would expect looping to
work as elegantly for IO actions as for pure functions.

Haskell lists do not allow elegant composition of loops that involve IO
actions.

Let's take an example of the `mapM` combinator for lists, it maps an IO
action on a list. The action is performed on all elements of the list in
a strict sequence as required by the semantics of IO, and then results
are returned as a list. For example:

```
main = do
  xs <-
      [1..3]                               -- [Int]
    & mapM (\x -> print x >> pure (x + 1)) -- IO [Int]
  print xs
```

We cannot compose a `take` after the `mapM` as we did for pure looping
example above. The loop has to be broken at `mapM` and then we have to
operate on the list returned by `mapM`:

```
main = do
  xs <-
      [1..3]                               -- [Int]
    & mapM (\x -> print x >> pure (x + 1)) -- IO [Int]
  xs
    & take 2             -- [Int]
    & sum                -- Int
    & print              -- IO ()
```

This is not very elegant. However, the bigger problem with this is -
the processing does not occur one element at a time, instead we apply
a `mapM` on all elements of the list first, collect all the results
and then do any further processing on the returned list. The entire
evaluated list is buffered in memory, there is no pipelining. 

This does not scale, and this is not how we want IO loops to
behave. Also, it collects the entire list in memory, therefore,
uses memory proortional to the number of elements processed in the
loop. And, if the list is too large or infinite, it won't even in fit
in memory. This simply does not work in any non-trivial practical
application.

## IO Looping with Streams

As we saw above, lists are good for pure functions but not for
IO. Streamly streams are designed such that they work like lists
but work as elegantly for the IO case as well. The only significant
difference between lists and streams is IO capability:

```
[] a        -- List of type 'a'
Stream IO a -- Stream of type 'a' for IO
```

Streamly streams are similar to lists in syntax and semantics, the
functionality of streams is a superset of standard Haskell lists, they
support all the combinators that lists do, support IO, concurrent
execution of IO actions, and many more combinators for real world
practical applications. Moreover, you get all of this with the same
compositional elegance and modularity as pure lists.

Let's write the pure transformation example we gave earlier using
streams, in this example the stream uses the IO type but there are no
actual IO actions performed, this is just a pure transformation pipeline
like lists:

```
main =
    Stream.enumerateFromTo 1 3 -- Stream IO Int
      & fmap (+1)              -- Stream IO Int
      & Stream.take 2          -- Stream IO Int
      & Stream.sum             -- IO Int
      >>= print                -- IO ()
```

Let's now add an IO action to print the value before incrementing it:

```
main =
    Stream.enumerateFromTo 1 3                      -- Stream IO Int
      & Stream.mapM (\x -> print x >> pure (x + 1)) -- Stream IO Int
      & Stream.take 2                               -- Stream IO Int
      & Stream.sum                                  -- IO Int
      >>= print                                     -- IO ()
```

We can do it in the same way as for the pure case. We just switched
`fmap` with `mapM`. We could not do this using lists as we saw in the
previous section.

## When to use Streams

Contrary to what many people think, streams are not meant for some
special use cases, they are fundamental to functional programming,
just like lists are. They are nothing but an elegant, modular looping
mechanism when IO is involved. In any non-trivial application you need
looping with IO, therefore, you need streams. The alternative is to use
explicit recursion and a monolithic loop, making your program inelegant
and not modular.

Ideally, streams should be in the Haskell standard library `base` as
any serious Haskell application would need streams to write it in the
Haskell way.
