# Haskell Intro

Before we start, we assume basic knowledge of Haskell syntax, constructs and
lazy evaluation. The [Haskell wikibook](https://en.wikibooks.org/wiki/Haskell)
may be a good place to start and get familiar with Haskell.

In this section we will go through some of Haskell concepts that may be useful
or relevant for the purposes of this tutorial.

## Lists

List is a built-in data type that represents a sequence of items of the same
type.

```
>>> numbers = [1, 2, 3, 4, 5]
>>> numbers
[1,2,3,4,5]
```

## Unevaluated Expressions

When we assign a value to a name, the name refers to the unevaluated
expression and not the result of evaluating that expression. For example:

```
>>> infiniteList = [1..]
```

The name `infiniteList` refers to an expression which when evaluated
would generate the list. `infiniteList` is not an actual physical list
but just a formula to generate the list `when needed`. That is the
reason we can represent an infinite list without requiring infinite
amount of memory.

Let's try to get the length of this list:

```
>>> len = length infiniteList
```

The name `len` refers to an expression that when evaluated would give us
the length of the list.  Till now Haskell has been saying that I duly
noted how to do what you want to do but I will do it only when you need
it. Let's ask it to print `len` now:

```
>>> len
...hangs forever
```

Hard pressed, Haskell now has no choice but to start evaluating the
expression but we know it can never finish because the list is infinite.

All expressions in Haskell are unevaluated by default.  Haskell
evaluates the expressions only when it is absolutely required e.g. when
we have to print something, or write it to a file or to network.

### Aside

This is how Haskell is fundamentally different from other languages. In
most languages, data is always stored as pure data representation
in memory. Functions operate on data and return the resulting
output immediately.

The natural way to represent data in Haskell is functions that have
the ability to generate data on-demand when evaluated. Functions are
composed together to create new unevaluated expressions, therefore, the
entire program is an unevaluated expression.

## On-demand Evaluation

Let us illustrate expression evaluation in Haskell with a few examples. Write
this little program in a file `example.hs`:

```
main =
    let largeList = [1..1000000::Int]
        n = length largeList
     in print n
```

Compile and run it:

```
$ ghc -O example.hs
[1 of 1] Compiling Main             ( example.hs, example.o )
Linking example ...
$ ./example +RTS -s
1000000
          51,480 bytes allocated in the heap
              24 bytes copied during GC
          44,328 bytes maximum residency (1 sample(s))
```

`largeList` is an expression representing a million element list, but
the program takes only 44K maximum memory. The reason is we never evaluate
and store the entire list in memory.  The length function evaluates
`largeList` one element at a time to count them, as soon as an element is
counted it can be garbage collected as it is not used anywhere else in the
program.

## Retained Heap Memory

Let's try this program instead:

```
main = do
    let largeList = [1..1000000::Int]
        x1 = last largeList
        x2 = head largeList
     in print (x1, x2)
```

```
$ ./example +RTS -s
(1000000,1)
      64,051,552 bytes allocated in the heap
             400 bytes copied during GC
      26,233,360 bytes maximum residency (4 sample(s))
```

This program is showing 26MB maximum residency. Why is it so? To
evaluate the first element of the tuple, `x1`, we need to evaluate
the entire list to get to the last element. However, the head of the
list is referenced by the element `x2` as well. Therefore, we need to
retain the head until `x2` is printed and freed. But head of the list is
referencing the entire list, therefore, the entire list is retained in
memory between the evaluation of `x1` and `x2`.

If we change the print statement to `(x2, x1)` then we do not need to
retain the entire list between the two.

### Forcing Evaluation

Even if there is no explicit dependency in the program, the programmer can
force the evaluation of an expression using `seq`:

```
main = do
    let largeList = [1..1000000::Int]
        x1 = last largeList
        x2 = head largeList
     in print (x1 `seq` x2)
```

Similar to the example in previous section, this also retains the entire list
in memory because `x1` is forced to be evaluated before `x2`.

### Reusing without retaining

We can write the same program using `Streamly`:

```
import Data.Functor.Identity (runIdentity)
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Fold as Fold

main = do
    let largeList = Stream.fromList [1..1000000::Int]
        x1 = runIdentity $ Stream.fold Fold.latest largeList
        x2 = runIdentity $ Stream.fold Fold.one largeList
     in print (x1, x2)
```

This program has the same behavior as the list based program, and for
the same reasons.  However, we can compose the folds using the `Tee`
type such that we distribute a single instance of the list to both the
folds, thus interleaving both the fold computations and avoiding the
retainment of intermediate list:

```
import Data.Functor.Identity (runIdentity)
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Fold as Fold

main = do
    let largeList = Stream.fromList [1..1000000::Int]
        x = Fold.unTee $ (,) <$> Fold.Tee Fold.latest <*> Fold.Tee Fold.one
     in print $ runIdentity $ Stream.fold x largeList
```

This program uses constant small memory footprint irrespective of the
order of the elements in the tuple. Because of the interleaving of the
two folds on the same stream, we do not have to retain memory in the
intermediate step.

Streamly covers all the functionality of standard Haskell lists but in addition
provides powerful composition tools for better efficiency.

### More Examples

The following program evaluates the list only up to 5 elements. The list is
evaluated only when the function `take` demands a value from it, `take`
terminates after taking 5 elements, therefore, the list is evaluated only up to
5 elements.

```
>>> take 5 infiniteList
[1,2,3,4,5]
```

Note that the function

or somehow force the evaluation, and
they are evaluated partially only as much as we need.

## Strings
