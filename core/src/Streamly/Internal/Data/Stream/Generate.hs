{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.Generate
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.Generate
    (
    -- * Primitives
      Stream.nil
    , Stream.nilM
    , Stream.cons
    , Stream.consM

    -- * From 'Unfold'
    , unfold

    -- * Unfolding
    , unfoldr
    , unfoldrM

    -- * From Values
    , Stream.fromPure
    , Stream.fromEffect
    , repeat
    , repeatM
    , replicate
    , replicateM

    -- * Enumeration
    , Enumerable (..)
    , enumerate
    , enumerateTo

    -- * Time Enumeration
    , times
    , timesWith
    , absTimes
    , absTimesWith
    , relTimes
    , relTimesWith
    , durations
    , timeout

    -- * Iteration
    , iterate
    , iterateM

    -- * Cyclic Elements
    , mfix

    -- * From Containers
    , Bottom.fromList
    , fromFoldable

    -- * From memory
    , fromPtr
    , fromPtrN
    , fromByteStr#
 -- , fromByteArray#
    , fromUnboxedIORef
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO)
import Data.Word (Word8)
import Foreign.Storable (Storable)
import GHC.Exts (Addr#, Ptr (Ptr))
import Streamly.Internal.Data.Stream.Bottom
    (absTimesWith, relTimesWith, timesWith)
import Streamly.Internal.Data.Stream.Enumerate
    (Enumerable(..), enumerate, enumerateTo)
import Streamly.Internal.Data.Stream.Type
    (Stream, fromStreamD, fromStreamK, toStreamK)
import Streamly.Internal.Data.Time.Units (AbsTime, RelTime64, addToAbsTime64)
import Streamly.Internal.Data.Unboxed (Unbox)
import Streamly.Internal.Data.Unfold.Type (Unfold)

import qualified Streamly.Internal.Data.IORef.Unboxed as Unboxed
import qualified Streamly.Internal.Data.Stream.Bottom as Bottom
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Stream.Transform as Stream (sequence)

import Prelude hiding (iterate, replicate, repeat, take)

-- $setup
-- >>> :m
-- >>> import Control.Concurrent (threadDelay)
-- >>> import Data.Function (fix, (&))
-- >>> import Data.Semigroup (cycle1)
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import GHC.Exts (Ptr (Ptr))

------------------------------------------------------------------------------
-- From Unfold
------------------------------------------------------------------------------

-- | Convert an 'Unfold' into a stream by supplying it an input seed.
--
-- >>> s = Stream.unfold Unfold.replicateM (3, putStrLn "hello")
-- >>> Stream.fold Fold.drain s
-- hello
-- hello
-- hello
--
{-# INLINE unfold #-}
unfold :: Monad m => Unfold m a b -> a -> Stream m b
unfold unf = Stream.fromStreamD . D.unfold unf

------------------------------------------------------------------------------
-- Generation by Unfolding
------------------------------------------------------------------------------

-- |
-- >>> :{
-- unfoldr step s =
--     case step s of
--         Nothing -> Stream.nil
--         Just (a, b) -> a `Stream.cons` unfoldr step b
-- :}
--
-- Build a stream by unfolding a /pure/ step function @step@ starting from a
-- seed @s@.  The step function returns the next element in the stream and the
-- next seed value. When it is done it returns 'Nothing' and the stream ends.
-- For example,
--
-- >>> :{
-- let f b =
--         if b > 2
--         then Nothing
--         else Just (b, b + 1)
-- in Stream.fold Fold.toList $ Stream.unfoldr f 0
-- :}
-- [0,1,2]
--
{-# INLINE_EARLY unfoldr #-}
unfoldr :: Monad m => (b -> Maybe (a, b)) -> b -> Stream m a
unfoldr step seed = fromStreamD (D.unfoldr step seed)
{-# RULES "unfoldr fallback to StreamK" [1]
    forall a b. D.toStreamK (D.unfoldr a b) = K.unfoldr a b #-}

-- | Build a stream by unfolding a /monadic/ step function starting from a
-- seed.  The step function returns the next element in the stream and the next
-- seed value. When it is done it returns 'Nothing' and the stream ends. For
-- example,
--
-- >>> :{
-- let f b =
--         if b > 2
--         then return Nothing
--         else return (Just (b, b + 1))
-- in Stream.fold Fold.toList $ Stream.unfoldrM f 0
-- :}
-- [0,1,2]
--
{-# INLINE unfoldrM #-}
unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> Stream m a
unfoldrM step = fromStreamD . D.unfoldrM step

------------------------------------------------------------------------------
-- From Values
------------------------------------------------------------------------------

-- |
-- Generate an infinite stream by repeating a pure value.
--
{-# INLINE_NORMAL repeat #-}
repeat :: Monad m => a -> Stream m a
repeat = fromStreamD . D.repeat

-- |
-- >>> repeatM = Stream.sequence . Stream.repeat
-- >>> repeatM = fix . Stream.consM
-- >>> repeatM = cycle1 . Stream.fromEffect
--
-- Generate a stream by repeatedly executing a monadic action forever.
--
-- >>> :{
-- repeatAction =
--        Stream.repeatM (threadDelay 1000000 >> print 1)
--      & Stream.take 10
--      & Stream.fold Fold.drain
-- :}
--
-- /Pre-release/
--
{-# INLINE_NORMAL repeatM #-}
repeatM :: Monad m => m a -> Stream m a
repeatM = Stream.sequence . repeat

-- |
-- >>> replicate n = Stream.take n . Stream.repeat
--
-- Generate a stream of length @n@ by repeating a value @n@ times.
--
{-# INLINE_NORMAL replicate #-}
replicate :: Monad m => Int -> a -> Stream m a
replicate n = fromStreamD . D.replicate n

-- |
-- >>> replicateM n = Stream.sequence . Stream.replicate n
--
-- Generate a stream by performing a monadic action @n@ times.
-- /Pre-release/
{-# INLINE_NORMAL replicateM #-}
replicateM :: Monad m => Int -> m a -> Stream m a
replicateM n = Stream.sequence . replicate n

------------------------------------------------------------------------------
-- Time Enumeration
------------------------------------------------------------------------------

-- | @times@ returns a stream of time value tuples with clock of 10 ms
-- granularity. The first component of the tuple is an absolute time reference
-- (epoch) denoting the start of the stream and the second component is a time
-- relative to the reference.
--
-- >>> f = Fold.drainBy (\x -> print x >> threadDelay 1000000)
-- >>> Stream.fold f $ Stream.take 3 $ Stream.times
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE times #-}
times :: MonadIO m => Stream m (AbsTime, RelTime64)
times = timesWith 0.01

-- | @absTimes@ returns a stream of absolute timestamps using a clock of 10 ms
-- granularity.
--
-- >>> f = Fold.drainBy print
-- >>> Stream.fold f $ Stream.delayPre 1 $ Stream.take 3 $ Stream.absTimes
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE absTimes #-}
absTimes :: MonadIO m => Stream m AbsTime
absTimes = fmap (uncurry addToAbsTime64) times

-- | @relTimes@ returns a stream of relative time values starting from 0,
-- using a clock of granularity 10 ms.
--
-- >>> f = Fold.drainBy print
-- >>> Stream.fold f $ Stream.delayPre 1 $ Stream.take 3 $ Stream.relTimes
-- RelTime64 (NanoSecond64 ...)
-- RelTime64 (NanoSecond64 ...)
-- RelTime64 (NanoSecond64 ...)
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE relTimes #-}
relTimes ::  MonadIO m => Stream m RelTime64
relTimes = fmap snd times

-- | @durations g@ returns a stream of relative time values measuring the time
-- elapsed since the immediate predecessor element of the stream was generated.
-- The first element of the stream is always 0. @durations@ uses a clock of
-- granularity @g@ specified in seconds. A low granularity clock is more
-- expensive in terms of CPU usage. The minimum granularity is 1 millisecond.
-- Durations lower than 1 ms will be 0.
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Unimplemented/
--
{-# INLINE durations #-}
durations :: -- Monad m =>
    Double -> t m RelTime64
durations = undefined

-- | Generate a singleton event at or after the specified absolute time. Note
-- that this is different from a threadDelay, a threadDelay starts from the
-- time when the action is evaluated, whereas if we use AbsTime based timeout
-- it will immediately expire if the action is evaluated too late.
--
-- /Unimplemented/
--
{-# INLINE timeout #-}
timeout :: -- Monad m =>
    AbsTime -> t m ()
timeout = undefined

------------------------------------------------------------------------------
-- Iterating functions
------------------------------------------------------------------------------

-- |
-- >>> iterate f x = x `Stream.cons` iterate f x
--
-- Generate an infinite stream with @x@ as the first element and each
-- successive element derived by applying the function @f@ on the previous
-- element.
--
-- >>> Stream.fold Fold.toList $ Stream.take 5 $ Stream.iterate (+1) 1
-- [1,2,3,4,5]
--
{-# INLINE_NORMAL iterate #-}
iterate :: Monad m => (a -> a) -> a -> Stream m a
iterate step = fromStreamD . D.iterate step

-- |
-- >>> iterateM f m = m >>= \a -> return a `Stream.consM` iterateM f (f a)
--
-- Generate an infinite stream with the first element generated by the action
-- @m@ and each successive element derived by applying the monadic function
-- @f@ on the previous element.
--
-- >>> :{
-- Stream.iterateM (\x -> print x >> return (x + 1)) (return 0)
--     & Stream.take 3
--     & Stream.fold Fold.toList
-- :}
-- 0
-- 1
-- [0,1,2]
--
{-# INLINE iterateM #-}
iterateM :: Monad m => (a -> m a) -> m a -> Stream m a
iterateM step = fromStreamD . D.iterateM step

-- | We can define cyclic structures using @let@:
--
-- >>> let (a, b) = ([1, b], head a) in (a, b)
-- ([1,1],1)
--
-- The function @fix@ defined as:
--
-- >>> fix f = let x = f x in x
--
-- ensures that the argument of a function and its output refer to the same
-- lazy value @x@ i.e.  the same location in memory.  Thus @x@ can be defined
-- in terms of itself, creating structures with cyclic references.
--
-- >>> f ~(a, b) = ([1, b], head a)
-- >>> fix f
-- ([1,1],1)
--
-- 'Control.Monad.mfix' is essentially the same as @fix@ but for monadic
-- values.
--
-- Using 'mfix' for streams we can construct a stream in which each element of
-- the stream is defined in a cyclic fashion. The argument of the function
-- being fixed represents the current element of the stream which is being
-- returned by the stream monad. Thus, we can use the argument to construct
-- itself.
--
-- In the following example, the argument @action@ of the function @f@
-- represents the tuple @(x,y)@ returned by it in a given iteration. We define
-- the first element of the tuple in terms of the second.
--
-- >>> import Streamly.Internal.Data.Stream as Stream
-- >>> import System.IO.Unsafe (unsafeInterleaveIO)
--
-- >>> :{
-- main = Stream.fold (Fold.drainBy print) $ Stream.mfix f
--     where
--     f action = do
--         let incr n act = fmap ((+n) . snd) $ unsafeInterleaveIO act
--         x <- Stream.sequence $ Stream.fromList [incr 1 action, incr 2 action]
--         y <- Stream.fromList [4,5]
--         return (x, y)
-- :}
--
-- Note: you cannot achieve this by just changing the order of the monad
-- statements because that would change the order in which the stream elements
-- are generated.
--
-- Note that the function @f@ must be lazy in its argument, that's why we use
-- 'unsafeInterleaveIO' on @action@ because IO monad is strict.
--
-- /Not fused/
--
-- /Pre-release/
{-# INLINE mfix #-}
mfix :: Monad m => (m a -> Stream m a) -> Stream m a
mfix f = fromStreamK $ K.mfix (toStreamK . f)

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- |
-- >>> fromFoldable = Prelude.foldr Stream.cons Stream.nil
--
-- Construct a stream from a 'Foldable' containing pure values:
--
-- /Not fused/
--
{-# INLINE fromFoldable #-}
fromFoldable :: Foldable f => f a -> Stream m a
fromFoldable = fromStreamK . K.fromFoldable

------------------------------------------------------------------------------
-- From pointers
------------------------------------------------------------------------------

-- | Keep reading 'Storable' elements from 'Ptr' onwards.
--
-- /Unsafe:/ The caller is responsible for safe addressing.
--
-- /Pre-release/
{-# INLINE fromPtr #-}
fromPtr :: (MonadIO m, Storable a) => Ptr a -> Stream m a
fromPtr = Stream.fromStreamD . D.fromPtr

-- | Take @n@ 'Storable' elements starting from 'Ptr' onwards.
--
-- >>> fromPtrN n = Stream.take n . Stream.fromPtr
--
-- /Unsafe:/ The caller is responsible for safe addressing.
--
-- /Pre-release/
{-# INLINE fromPtrN #-}
fromPtrN :: (MonadIO m, Storable a) => Int -> Ptr a -> Stream m a
fromPtrN n = Stream.fromStreamD . D.take n . D.fromPtr

-- | Read bytes from an 'Addr#' until a 0 byte is encountered, the 0 byte is
-- not included in the stream.
--
-- >>> fromByteStr# addr = Stream.takeWhile (/= 0) $ Stream.fromPtr $ Ptr addr
--
-- /Unsafe:/ The caller is responsible for safe addressing.
--
-- Note that this is completely safe when reading from Haskell string
-- literals because they are guaranteed to be NULL terminated:
--
-- >>> Stream.fold Fold.toList $ Stream.fromByteStr# "\1\2\3\0"#
-- [1,2,3]
--
{-# INLINE fromByteStr# #-}
fromByteStr# :: MonadIO m => Addr# -> Stream m Word8
fromByteStr# addr =
    Stream.fromStreamD $ D.takeWhile (/= 0) $ D.fromPtr $ Ptr addr

-- | Construct a stream by reading an 'Unboxed' 'IORef' repeatedly.
--
-- /Pre-release/
--
{-# INLINE fromUnboxedIORef #-}
fromUnboxedIORef :: (MonadIO m, Unbox a) => Unboxed.IORef a -> Stream m a
fromUnboxedIORef = fromStreamD . Unboxed.toStreamD
