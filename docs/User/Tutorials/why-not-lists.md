<!--
(c) 2022, Composewell Technologies.
SPDX-License-Identifer: BSD-3-Clause
-->

# Haskell Lazy Evaluation

Haskell is fundamentally different from other languages. In most
languages, a variable name refers to some concrete data in memory;  in
Haskell, a variable may refer to concrete data or it could refer to
a computation or expression which when evaluated would produce the
concrete data that the variable is referring to. When the value of the
variable is needed, the computation is evaluated and replaced by
the result.

When a Haskell program starts, the entire program is an unevaluated
expression which is evaluated on-demand as needed. The top level
expression is forced to produce data when a result is demanded from it
for IO.  Parts of the expression are reduced as needed, the reduction
process can be controlled by the programmer.

## Unevaluated Expressions

When we assign a value to a name, the name refers to the unevaluated
expression and not what it would produce when evaluated. For example:

```
>>> infiniteList = [1..]
```

The name `infiniteList` refers to an expression which when evaluated
would generate the list. `infiniteList` is not an actual physical list
but just an expression to generate the list `when needed`. Because we
are not storing the actual list in memory, we can represent an infinite
list without requiring infinite amount of memory.

Let's try to get the length of this list:

```
>>> len = length infiniteList
```

Like before, the name `len` refers to an expression that when evaluated
would give us the length of the list.  Till now Haskell has been saying
that I duly noted how to do what you want to do but I will do it only
when you need it. Let's ask it to do that now:

```
>>> len
...hangs forever
```

Now, Haskell has no choice but to start evaluating the expression but we
know it can never finish because the list is infinite.

All expressions in Haskell are unevaluated by default.  Haskell
evaluates the expressions only when it is absolutely required e.g. when
we have to print something, or write it to a file or to network.

## On-demand Expression Evaluation

Let us illustrate expression evaluation in Haskell with an example. Write
this little program in a file `example.hs`:

```haskell
main =
    let largeList = [1..1000000::Int]
        n = length largeList
     in print n
```

The expression `largeList` is never evaluated to generate the entire
list at once. It is evaluated on-demand one element at a time by the
`length` function. The `length` function is in turn evaluated by the
`print` function when it evaluates its argument `n`. The `print`
function is evaluated by the top level program because it wants to print
`n` to the console.

We can verify that the whole list is not stored in memory by looking at
the memory usage of this program. Let's compile and run it:

```
$ ghc -O example.hs
$ ./example +RTS -s
1000000
          51,480 bytes allocated in the heap
              24 bytes copied during GC
          44,328 bytes maximum residency (1 sample(s))
```

`largeList` is an expression representing a million element list, but
the program takes only 44K maximum memory.  The `length` function
evaluates `largeList` one element at a time to count them, as soon as
an element is counted it can be garbage collected as there are no more
references to it in the entire program.

## Expressions Holding up Heap Memory

Let's try this program instead:

```haskell
main = do
    let largeList = [1..1000000::Int]
        x1 = last largeList
        x2 = head largeList
     in print (x1, x2)
```

```
$ ghc -O example.hs
$ ./example +RTS -s
(1000000,1)
      64,051,552 bytes allocated in the heap
             400 bytes copied during GC
      26,233,360 bytes maximum residency (4 sample(s))
```

This program is showing 26MB maximum residency instead of 44K
earlier. Why is it so? To evaluate the first element of the tuple, `x1`,
we need to evaluate the entire list to get to the last element. However,
the head of the list is referenced by the element `x2` as well which
is yet to be evaluated and used. Therefore, we need to retain the head
until `x2` is printed and freed. But head of the list is referencing the
next element in the list which in turn is referring to the next element
and so on, therefore, the entire list is retained in memory between the
evaluation of `x1` and `x2`.

If we change the print statement to `(x2, x1)` then we do not need to
retain the entire list between the two evaluations.

When memory is unexpectedly retained in a program it is known as a
"space-leak" in the Haskell parlance.

## Forcing Expression Evaluation

Even if there is no explicit dependency in the program forcing
evaluation of an expression, the programmer can force the evaluation of
an expression using `seq`:

```haskell
main = do
    let largeList = [1..1000000::Int]
        x1 = last largeList
        x2 = head largeList
     in print (x1 `seq` x2)
```

Similar to the example in previous section, this example also retains
the entire list in memory because `seq` forces `x1` to be evaluated
before `x2` and then returns `x2`, the evaluated list is held until `x2` is
returned.

## Avoiding Unnecessary Memory Retention

Memory retention can be avoided by changing the way we construct the
expression to process the data. We can use Streamly to avoid memory
retention in the above program.

Let's first write it using streamly and reproduce the same problem:

```haskell
import Data.Functor.Identity (runIdentity)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold

main = do
    let largeList = Stream.fromList [1..1000000::Int]
        x1 = runIdentity $ Stream.fold Fold.latest largeList
        x2 = runIdentity $ Stream.fold Fold.one largeList
     in print (x1, x2)
```

This program has the same behavior as the list based program, and for
the same reasons.

```
$ ghc -O2 example.hs
$ ./example +RTS -s
(Just 1000000,Just 1)
      64,052,520 bytes allocated in the heap
             400 bytes copied during GC
      26,233,360 bytes maximum residency (4 sample(s))
```

However, we can compose the two folds using the `teeWith` combinator
such that we distribute a each element of the list to both the folds
simultaneously, thus avoiding the retainment of the entire list:

```haskell
import Data.Functor.Identity (runIdentity)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold

main = do
    let largeList = Stream.fromList [1..1000000::Int]
        x = Fold.teeWith (,) Fold.latest Fold.one
     in print $ runIdentity $ Stream.fold x largeList
```

```
$ ghc -O2 example.hs
$ ./example +RTS -s
(Just 1000000,Just 1)
      64,052,736 bytes allocated in the heap
             280 bytes copied during GC
          44,328 bytes maximum residency (2 sample(s))
```

This program uses constant small memory footprint (44K) irrespective of
the order of the elements in the tuple. When an element is generated by
the list it is supplied to both the folds before generating the next
element, therefore, it can be immediately freed.

Streamly covers all the functionality of standard Haskell lists but in addition
provides powerful composition tools for better efficiency.
