# Frequently Asked Questions

This document provides idioms or examples to solve common programming
problems using streamly. To start with please go through the [Learn By
Examples](/docs/User/Tutorials/learn-by-examples.md) page and [`Examples Package`](https://github.com/composewell/streamly-examples).
This document provides additional examples.

## Distribute and Zip Concurrently

Transform a stream in multiple ways, generating multiple transformed
streams and then zip the corresponding elements of each resulting stream
together to create a single transformed stream.

Distributing a value to a stream of consumers concurrently:

```haskell docspec
>>> :set -XFlexibleContexts
>>> import Data.Function ((&))
>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.Stream.Prelude as Stream

>>> :{
f1 x =
    Stream.fromList [return . (+ 1), return . (+ 2)] -- Stream of functions
        & fmap ($ x)                                 -- Stream of lazy actions
        & Stream.parSequence (Stream.ordered True)   -- Evaluate concurrently
        & Stream.fold Fold.toList                    -- Fold to list
:}
```

Use `parZipWith` to zip streams concurrently. Here, we zip three singleton
streams:

```haskell docspec
>>> :{
f2 x =
  let app = Stream.parZipWith id ($)
  in (,,)
      `fmap` Stream.fromEffect (return $ show x)
      `app`  Stream.fromEffect (return $ x + 1)
      `app`  Stream.fromEffect (return $ fromIntegral x / 2)
  & Stream.fold Fold.one
:}
```

Applying a function concurrently to your input stream:

```haskell docspec
>>> :{
g f xs =
  Stream.fromList xs
    & Stream.parMapM (Stream.ordered True) f
    & Stream.fold Fold.toList
:}
```

You can now use the concurrent map to pipe each element through multiple
transformations using the distribute/zip operations.

```haskell docspec
>>> g f1 [1,2,3,4::Int]
[[2,3],[3,4],[4,5],[5,6]]

>>> g f2 [1,2,3,4::Int]
[Just ("1",2,0.5),Just ("2",3,1.0),Just ("3",4,1.5),Just ("4",5,2.0)]
```

Instead of using `parZipWith` directly, you can use `mkZipType` to
create a zip Applicative newtype so that you can use the `Applicative`
instance.

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Trans.Control (MonadBaseControl)
import Streamly.Data.Stream.Prelude (MonadAsync, Stream)
import qualified Streamly.Data.Stream.Prelude as Stream
import Streamly.Data.Stream.MkType

app :: MonadAsync m => Stream m (a -> b) -> Stream m a -> Stream m b
app = Stream.parZipWith id ($)
$(mkZipType "ZipConcurrent" "app" True)
```

## Sliding Window

The `createOfLast` fold can be used to create a stream of sliding windows.

```haskell docspec
>>> :set -Wno-deprecations
>>> import qualified Streamly.Data.Array as Array
>>> :{
  Stream.fromList [1,2,3,4,5::Int]
& Stream.scan (Array.createOfLast 2)
& Stream.fold Fold.toList
:}
[fromList [],fromList [1],fromList [1,2],fromList [2,3],fromList [3,4],fromList [4,5]]
```

Also see the "Streamly.Internal.Data.Fold.Window" module for widnow based folds.
