{-# LANGUAGE CPP #-}

-- |
-- Module      : Streamly.Data.Unfold
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Fast, composable stream producers with ability to terminate, supporting
-- nested stream fusion. Nested stream operations like
-- 'Streamly.Data.Stream.concatMap' in the "Streamly.Data.Stream" module do not
-- fuse, however, the 'Streamly.Data.Stream.unfoldMany' operation, using the
-- 'Unfold' type, is a fully fusible alternative to
-- 'Streamly.Data.Stream.concatMap'.
--
-- Please refer to "Streamly.Internal.Data.Unfold" for more functions that have
-- not yet been released.
--
-- Exception combinators are not exposed, we would like to encourage the use of
-- 'Stream' type instead whenever exception handling is required. We can
-- consider exposing the unfold exception functions if there is a compelling
-- use case to use unfolds instead of stream.

module Streamly.Data.Unfold
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Overview
    -- $overview

    -- * Unfold Type
      Unfold

    -- * Unfolds
    -- One to one correspondence with
    -- "Streamly.Internal.Data.Stream.Generate"

    -- ** Basic Constructors
    , unfoldrM
    , unfoldr
    , function
    , functionM

    -- ** Generators
    -- | Generate a monadic stream from a seed.
    , repeatM
    , replicateM
    , iterateM

    -- ** From Containers
    , fromList
    , fromListM
    , fromStream

    -- * Combinators
    -- ** Mapping on Input
    , lmap
    , lmapM

    -- ** Mapping on Output
    , mapM

    -- ** Filtering
    , takeWhileM
    , takeWhile
    , take
    , filter
    , filterM
    , drop
    , dropWhile
    , dropWhileM

    -- ** Zipping
    , zipWith

    -- ** Cross Product
    , crossWith

    -- ** Nesting
    , many

    -- ** Enumerating 'Num' Types
    , enumerate
    , enumerateTo
    )
where

import Prelude hiding
    ( concat, map, mapM, takeWhile, take, filter, const, drop, dropWhile
    , zipWith
    )
import Streamly.Internal.Data.Unfold

#include "DocTestDataUnfold.hs"

-- $overview
--
-- An 'Unfold' is a source or a producer of a stream of values.  It takes a
-- seed value as an input and unfolds it into a sequence of values.
--
-- For example, the 'fromList' Unfold generates a stream of values from a
-- supplied list.  Unfolds can be converted to 'Streamly.Internal.Data.Stream'
-- using the 'Stream.unfold' operation.
--
-- >>> stream = Stream.unfold Unfold.fromList [1..100]
-- >>> Stream.fold Fold.sum stream
-- 5050
--
-- The input seed of an unfold can be transformed using 'lmap':
--
-- >>> u = Unfold.lmap (fmap (+1)) Unfold.fromList
-- >>> Stream.fold Fold.toList $ Stream.unfold u [1..5]
-- [2,3,4,5,6]
--
-- Output stream of an 'Unfold' can be transformed using transformation
-- combinators. For example, to retain only the first two elements of an
-- unfold:
--
-- >>> u = Unfold.take 2 Unfold.fromList
-- >>> Stream.fold Fold.toList $ Stream.unfold u [1..100]
-- [1,2]
--
-- Unfolds can be nested efficiently. For example, to implement nested looping:
--
-- >>> u1 = Unfold.lmap fst Unfold.fromList
-- >>> u2 = Unfold.lmap snd Unfold.fromList
-- >>> u = Unfold.crossWith (,) u1 u2
-- >>> Stream.fold Fold.toList $ Stream.unfold u ([1,2,3], [4,5,6])
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
--
-- 'Unfold' @u1@ generates a stream from the first list in the input tuple,
-- @u2@ generates another stream from the second list. The combines 'Unfold'
-- @u@ nests the two streams i.e. for each element in first stream, for each
-- element in second stream apply the supplied function (i.e. @(,)@) to the
-- pair of elements.
--
-- This is the equivalent of the nested looping construct from imperative
-- languages, also known as the cross product of two streams in functional
-- parlance.
--
-- Please see "Streamly.Internal.Data.Unfold" for additional @Pre-release@
-- functions.
--
-- == Creating New Unfolds
--
-- There are many commonly used unfolds provided in this module. However, you
-- can always create your own as well.  An 'Unfold' is just a data
-- representation of a stream generator function. It consists of an @inject@
-- function which covnerts the supplied seed into an internal state of the
-- unfold, and a @step@ function which takes the state and generates the next
-- output in the stream. For those familiar with the list "Data.List.unfoldr"
-- function, this is a data representation of the same.
--
-- Smart constructor functions are provided in this module for constructing new
-- 'Unfolds'. For example, you can use the 'Unfold.unfoldr' constructor to
-- create an 'Unfold' from a pure step function, unfoldr uses 'id' as the
-- @inject@ function.
--
-- Let's define a simple pure step function:
--
-- >>> :{
--  f [] = Nothing
--  f (x:xs) = Just (x, xs)
-- :}
--
-- Create an 'Unfold' from the step function:
--
-- >>> u = Unfold.unfoldr f
--
-- Run the 'Unfold':
--
-- >>> Stream.fold Fold.toList $ Stream.unfold u [1,2,3]
-- [1,2,3]
--
-- The 'Unfold.unfoldr' smart constructor is essentially the same as the list
-- "Data.List.unfoldr" function. We can use the same step function in both::
--
-- >>> Data.List.unfoldr f [1,2,3]
-- [1,2,3]
--
-- == Unfolds vs. Streams
--
-- The 'Unfold' abstraction for representing streams was introduced in Streamly
-- to provide C like performance for nested looping of streams. 'Unfold' and
-- 'Stream' abstractions are similar with the following differences:
--
-- * 'Stream' is less efficient than 'Unfold' for nesting.
-- * 'Stream' is more powerful than 'Unfold'.
-- * 'Stream' API is more convenient for programming
--
-- Unfolds can be easily converted to streams using 'Stream.unfold', however,
-- vice versa is not possible. To provide a familiar analogy, 'Unfold' is to
-- 'Stream' as 'Applicative' is to 'Monad'.
--
-- To demonstrate the efficiency of unfolds, the nested loop example in the
-- previous section can be implemented with concatMap or Monad instance of
-- streams as follows:
--
-- @
--  do
--      x <- Stream.unfold Unfold.fromList [1,2,3]
--      y <- Stream.unfold Unfold.fromList [4,5,6]
--      return (x, y)
-- @
--
-- As you can see, this is more convenient to write than using the 'crossWith'
-- unfold combinator. However, this turns out to be many times slower than the
-- unfold implementation. The Unfold version is equivalent in performance to
-- the C implementation of the same nested loop. Similarly, unfolds can be
-- nested with streams using the 'unfoldMany' combinator which is a much more
-- efficient alternative to the 'concatMap' operation.
--
-- Streams use a hybrid implementation approach using direct style as well as
-- CPS. Unfolds do not use CPS, therefore, lack the power that is afforded to
-- streams by CPS. The CPS implementation allows infinitely scalable @cons@ and
-- @append@ operations in streams. It is also used to implement concurrency in
-- streams.
--
-- To summarize, unfolds are a high performance solution to the nesting
-- problem. Since streams provide a more palatable API for programming, work
-- with streams unless you need unfolds for better performance in nesting
-- situations. There is little difference in the way in which unfolds and
-- streams are written, it is easy to adapt a stream to an unfold. If you are
-- writing an unfold you can convert it to stream for free using
-- 'Stream.unfold'.

-- | Unfolds @from@ generating a stream starting with the element
  -- @from@, enumerating up to 'maxBound' when the type is 'Bounded' or
  -- generating an infinite stream when the type is not 'Bounded'.
  --
  -- >>> import qualified Streamly.Data.Stream as Stream
  -- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
  --
  -- @
  -- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.unfold Unfold.enumerateFrom (0 :: Int)
  -- [0,1,2,3]
  --
  -- @
  --
  -- For 'Fractional' types, enumeration is numerically stable. However, no
  -- overflow or underflow checks are performed.
  --
  -- @
  -- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.unfold Unfold.enumerateFrom 1.1
  -- [1.1,2.1,3.1,4.1]
  --
  -- @
  --
{-# INLINE enumerate #-}
enumerate :: (Monad m, Enumerable a) => Unfold m a a
enumerate = enumerateFrom

-- | Unfolds @(from, to)@ generating a finite stream starting with the element
  -- @from@, enumerating the type up to the value @to@. If @to@ is smaller than
  -- @from@ then an empty stream is returned.
  --
  -- >>> import qualified Streamly.Data.Stream as Stream
  -- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
  --
  -- @
  -- >>> Stream.fold Fold.toList $ Stream.unfold Unfold.enumerateFromTo (0, 4)
  -- [0,1,2,3,4]
  --
  -- @
  --
  -- For 'Fractional' types, the last element is equal to the specified @to@
  -- value after rounding to the nearest integral value.
  --
  -- @
  -- >>> Stream.fold Fold.toList $ Stream.unfold Unfold.enumerateFromTo (1.1, 4)
  -- [1.1,2.1,3.1,4.1]
  --
  -- >>> Stream.fold Fold.toList $ Stream.unfold Unfold.enumerateFromTo (1.1, 4.6)
  -- [1.1,2.1,3.1,4.1,5.1]
  --
  -- @
  --
{-# INLINE enumerateTo #-}
enumerateTo :: (Monad m, Enumerable a) => Unfold m (a, a) a
enumerateTo = enumerateFromTo
