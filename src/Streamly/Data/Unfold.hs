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
-- >>> import qualified Streamly.Prelude as Stream
--
-- For example, the 'fromList' Unfold generates a stream of values from a
-- supplied list.  Unfolds can be converted to 'Streamly.Prelude.SerialT'
-- stream using the Stream.unfold operation.
--
-- >>> stream = Stream.unfold Unfold.fromList [1..100]
-- >>> Stream.sum stream
-- 5050
--
-- All the serial stream generation operations in "Streamly.Prelude"
-- can be expressed using unfolds:
--
-- > Stream.fromList = Stream.unfold Unfold.fromList [1..100]
--
-- Conceptually, an 'Unfold' is just like "Data.List.unfoldr". Let us write a
-- step function to unfold a list using "Data.List.unfoldr":
--
-- >>> :{
--  f [] = Nothing
--  f (x:xs) = Just (x, xs)
-- :}
--
-- >>> Data.List.unfoldr f [1,2,3]
-- [1,2,3]
--
-- Unfold.unfoldr is just the same, it uses the same step function:
--
-- >>> Stream.toList $ Stream.unfold (Unfold.unfoldr f) [1,2,3]
-- [1,2,3]
--
-- The input of an unfold can be transformed using 'lmap':
--
-- >>> u = Unfold.lmap (fmap (+1)) Unfold.fromList
-- >>> Stream.toList $ Stream.unfold u [1..5]
-- [2,3,4,5,6]
--
-- 'Unfold' streams can be transformed using transformation combinators. For
-- example, to retain only the first two elements of an unfold:
--
-- >>> u = Unfold.take 2 Unfold.fromList
-- >>> Stream.toList $ Stream.unfold u [1..100]
-- [1,2]
--
-- Multiple unfolds can be combined in several interesting ways. For example,
-- to generate nested looping as in imperative languages (also known as cross
-- product of the two streams):
--
-- >>> u1 = Unfold.lmap fst Unfold.fromList
-- >>> u2 = Unfold.lmap snd Unfold.fromList
-- >>> u = Unfold.crossWith (,) u1 u2
-- >>> Stream.toList $ Stream.unfold u ([1,2,3], [4,5,6])
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
--
-- Nested loops using unfolds provide C like performance due to complete stream
-- fusion.
--
-- Please see "Streamly.Internal.Data.Unfold" for additional @Pre-release@
-- functions.
--
-- = Unfolds vs. Streams
--
-- Unfolds' raison d'etre is their efficiency in nested stream operations due
-- to complete stream fusion.  'Streamly.Prelude.concatMap' or the 'Monad'
-- instance of streams use stream generation operations of the shape @a -> t m
-- b@ and then flatten the resulting stream.  This implementation is more
-- powerful but does not allow for complete stream fusion.  Unfolds provide
-- less powerful but more efficient 'Streamly.Prelude.unfoldMany', 'many' and
-- 'crossWith' operations as an alternative to a subset of use cases of
-- 'concatMap' and 'Applicative' stream operations.
--
-- "Streamly.Prelude" exports polymorphic stream generation operations that
-- provide the same functionality as unfolds in this module.  Since unfolds can
-- be easily converted to streams, several modules in streamly provide only
-- unfolds for serial stream generation.  We cannot use unfolds exclusively for
-- stream generation as they do not support concurrency.

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
import Streamly.Internal.Data.Unfold

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
