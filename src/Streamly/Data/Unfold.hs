#include "inline.hs"

-- |
-- Module      : Streamly.Data.Unfold
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- An 'Unfold' is a source or a producer of a stream of values.  It takes a
-- seed value as an input and unfolds it into a sequence of values.
--
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
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
-- = Creating New Unfolds
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
-- = Unfolds vs. Streams
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

-- What makes streams less efficient is also what makes them more conveient to
-- use and powerful. The stream data type (Stream m a) bundles the state along
-- with the stream generator function making it opaque, whereas an unfold
-- exposes the state (Unfold m s a) to the user. This allows the Unfold to be
-- unfolded (inlined) inside a nested loop without having to bundle the state
-- and the generator together, the stream state can be saved and passed
-- independent of the generator function. On the other hand in a stream type we
-- have to bundle the stream state and the generator function together to save
-- the stream. This makes it inefficient because it requires boxing and
-- constructor allocation. However, this makes streams more convenient as we do
-- not need to pass around the state/seed separately.
--
module Streamly.Data.Unfold
    (
    -- * Unfold Type
      Unfold

    -- * Unfolds
    -- One to one correspondence with
    -- "Streamly.Internal.Data.Stream.IsStream.Generate"

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
    )
where

import Prelude hiding
    ( concat, map, mapM, takeWhile, take, filter, const, drop, dropWhile
    , zipWith
    )
import Streamly.Internal.Data.Stream.IsStream.Type (IsStream)
import Streamly.Internal.Data.Unfold hiding (fromStream)

import qualified Streamly.Internal.Data.Stream.IsStream.Type as IsStream
import qualified Streamly.Internal.Data.Unfold as Unfold

-- XXX Using Unfold.fromStreamD seems to be faster (using cross product test
-- case) than using fromStream even if it is implemented using fromStreamD.
-- Check if StreamK to StreamD rewrite rules are working correctly when
-- implementing fromStream using fromStreamD.

-- | Convert a stream into an 'Unfold'. Note that a stream converted to an
-- 'Unfold' may not be as efficient as an 'Unfold' in some situations.
--
-- /Since: 0.8.0/
--
{-# INLINE_NORMAL fromStream #-}
fromStream :: (IsStream t, Applicative m) => Unfold m (t m a) a
fromStream = lmap IsStream.toStream Unfold.fromStreamK
