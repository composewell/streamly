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

- Create a stream of actions.
- Sequence the actions concurrently.
  - Note that we respect the order of input. (`Config.ordered True`)

```haskell ghci
{-# LANGUAGE FlexibleContexts #-}

import Data.Function ((&))
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent as Concur

f1 x =
    Prelude.map ($ x) [return . (+ 1), return . (+ 2)] -- List of actions
        & Stream.fromList                              -- Stream of actions
        & Concur.sequenceWith (Concur.ordered True)    -- Sequence the actions concurrently
        & Stream.fold Fold.toList                      -- Fold the resulting stream into a list
```

Alternatively, if you want to concurrently zip different types of outputs, you
can use `zipWith`:

- Create 2 streams of actions.
- Sequence the actions concurrently.
- Zip the resulting streams.

```haskell ghci
import Data.Maybe (fromJust)

f2 x =
  let actionStream1 = Stream.fromPure (return $ show x)
      actionStream2 = Stream.fromPure (return $ x + 1)
      actionStream3 = Stream.fromPure (return $ fromIntegral x / 2)
   in Stream.zipWith (\a (b, c) -> (a, b, c))
          (Concur.sequence actionStream1)
          (Stream.zipWith (,)
               (Concur.sequence actionStream2)
               (Concur.sequence actionStream3))
          & Stream.fold Fold.one
          & fmap fromJust
```

Alternatively, generate a type with concurrent zipping property to use the
`Applicative` instance,

```haskell
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Function ((&))
import Data.Maybe (fromJust)
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream.Concurrent (MonadAsync)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent
import qualified Streamly.Data.Stream.Concurrent as Concur
import qualified Streamly.Internal.Data.Stream.TypeGen as StreamTypeGen

applyConcurrent :: MonadAsync m => Stream m (a -> b) -> Stream m a -> Stream m b
applyConcurrent = Concur.zipWith ($)

$(StreamTypeGen.mkZippingType "ZipConcurrent" "applyConcurrent" True)

f2 x =
  let actionStream1 = Stream.fromEffect (return $ show x)
      actionStream2 = Stream.fromEffect (return $ x + 1)
      actionStream3 = Stream.fromEffect (return $ fromIntegral x / 2)
   in ((,,,)
           <$> actionStream1
           <*> actionStream2
           <*> actionStream3)
          & Stream.fold Fold.one
          & fmap fromJust
```

Then apply the function `f` concurrently to your input stream:

```haskell ghci
g f xs =
  Stream.fromList xs
    & Concur.mapMWith (Concur.ordered True) f -- Map the actions concurrently
    & Stream.fold Fold.toList
```

```haskell docspec
>>> g f1 [1,2,3,4::Int]
[[2,3],[3,4],[4,5],[5,6]]

>>> g f2 [1,2,3,4::Int]
[("1",2,0.5),("2",3,1.0),("3",4,1.5),("4",5,2.0)]
```

## Sliding Window

```haskell docspec
>>> import qualified Streamly.Internal.Data.Array as Array
>>> :{
  Stream.fromList [1,2,3,4,5::Int]
& Stream.scan (Array.writeLastN 2)
& Stream.fold Fold.toList
:}
[fromList [],fromList [1],fromList [1,2],fromList [2,3],fromList [3,4],fromList [4,5]]
```
