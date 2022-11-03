# Frequently Asked Questions

This document provides idioms or examples to solve common programming
problems using streamly. To start with please go through [Streamly Quick
Overview](/docs/User/Tutorials/Introduction.md) and [`Streamly examples repository`][streamly-examples].
This document provides additional examples.

## Distribute and Zip Concurrently

Say, you have an input stream, and each element of the input stream is to be
transformed in multiple ways using monadic functions. Then you want to zip the
results together and consume the resulting stream concurrently.

First, distribute an item to multiple effectful consumer concurrently, for
example:

```haskell
import Data.Function ((&))
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent as Concur

f1 x =
      Prelude.map ($ x) [return . (+ 1), return . (+ 2)]
    & Stream.fromList
    & Concur.sequenceWith (Concur.ordered True)
    & Stream.fold Fold.toList
```

Alternatively, if you want to concurrently zip different types of outputs, you
can use `Applicative` composition:

-- XXX Use TypeGen accodingly?
```haskell
import Data.Maybe (fromJust)
f2 x =
    (,,)
      <$> Stream.fromEffect (return $ show x)
      <*> Stream.fromEffect (return $ x + 1)
      <*> Stream.fromEffect (return $ fromIntegral x / 2)
  & Stream.fromAsync
  & Stream.head
  & fmap fromJust
```

Then apply the function `f` concurrently to your input stream:

```haskell
g f xs =
  Stream.fromList xs
    & fmap f
    & Concur.sequenceWith (Concur.ordered True)
    & Stream.toList
```

```
>>> g f1 [1,2,3,4::Int]
[[2,3],[3,4],[4,5],[5,6]]

>>> g f2 [1,2,3,4::Int]
[("1",2,0.5),("2",3,1.0),("3",4,1.5),("4",5,2.0)]
```

## Sliding Window

```
>>> :{
  Stream.fromList [1,2,3,4,5::Int]
& Stream.scan (Array.writeLastN 2)
& Stream.fold Fold.list
:}
[[],[1],[1,2],[2,3],[3,4],[4,5]]
```
