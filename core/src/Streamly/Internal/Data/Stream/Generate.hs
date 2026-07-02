{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Stream.Generate
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
--               (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

-- A few combinators in this module have been adapted from the vector package
-- (c) Roman Leshchinskiy. See the notes in specific combinators.
--
module Streamly.Internal.Data.Stream.Generate
  (
    -- * Primitives
      nil
    , cons

    -- * Unfolding
    , unfoldr
    , unfoldrM

    -- * From Values
    , repeat
    , repeatM
    , replicate
    , replicateM

    -- * Time Enumeration
    , times
    , timesWith
    , absTimes
    , absTimesWith
    , relTimes
    , relTimesWith
    , durations
    , timeout

    -- * From Generators
    -- | Generate a monadic stream from a seed.
    , fromIndices
    , fromIndicesM
    , generate
    , generateM

    -- * Iteration
    , iterate
    , iterateM

    -- * From Containers
    -- | Transform an input structure into a stream.

    , fromListM
    , fromFoldable
    , fromFoldableM

    -- * From Pointers
    , fromPtr
    , fromPtrN
    , fromCString#
    , fromW16CString#

    -- * Deprecated
    , fromByteStr#
    )
where

#include "inline.hs"
#include "ArrayMacros.h"

import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable (peek), sizeOf)
import GHC.Exts (Addr#, Ptr (Ptr))
import Streamly.Internal.Data.Time.Clock
    (Clock(Monotonic), asyncClock, readClock)
import Streamly.Internal.Data.Time.Units
    (toAbsTime, AbsTime, toRelTime64, RelTime64, addToAbsTime64)
import Streamly.Internal.System.IO (unsafeInlineIO)

#ifdef USE_UNFOLDS_EVERYWHERE
import qualified Streamly.Internal.Data.Unfold as Unfold
#else
import qualified Streamly.Internal.Data.Producer as Producer
#endif

import Data.Word
import Prelude hiding (iterate, repeat, replicate, take, takeWhile)
import Streamly.Internal.Data.Stream.Type

#include "DocTestDataStream.hs"

------------------------------------------------------------------------------
-- Primitives
------------------------------------------------------------------------------

-- XXX implement in terms of nilM?

-- | A stream that terminates without producing any output or side effect.
--
-- >>> Stream.toList Stream.nil
-- []
--
{-# INLINE_NORMAL nil #-}
nil :: Applicative m => Stream m a
nil = Stream (\_ _ -> pure Stop) ()

infixr 5 `cons`

-- XXX implement in terms of consM?
-- cons x = consM (return x)
-- From an implementation perspective, StreamK.cons translates into a
-- functional call whereas fused cons translates into a conditional branch
-- (jump). However, the overhead of the function call in StreamK.cons only
-- occurs once, while the overhead of the conditional branch in fused cons is
-- incurred for each subsequent element in the stream. As a result,
-- StreamK.cons has a time complexity of O(n), while fused cons has a time
-- complexity of O(n^2), where @n@ represents the number of 'cons' used.

-- When composing a few elements together statically, a balanced tree composed
-- using 'cons' and 'append' is more efficient than a right associated one
-- composed using 'cons':
--
-- >>> s1 = 1 `Stream.cons` 2 `Stream.cons` Stream.nil
-- >>> s2 = 2 `Stream.cons` 3 `Stream.cons` Stream.nil
-- >>> s = s1 `Stream.append` s2
--
-- However, generating a stream using a case statement or indexing into a
-- static literal array would be the best. Check if the case statement
-- translates to a look up table or a binary search.

-- | WARNING! O(n^2) time complexity wrt number of elements. Use the O(n)
-- complexity StreamK.'Streamly.Data.StreamK.cons' unless you want to
-- statically fuse just a few elements.
--
-- Fuse a pure value at the head of an existing stream::
--
-- >>> s = 1 `Stream.cons` Stream.fromList [2,3]
-- >>> Stream.toList s
-- [1,2,3]
--
-- Definition:
--
-- >>> cons x xs = return x `Stream.consM` xs
--
{-# INLINE_NORMAL cons #-}
cons :: Applicative m => a -> Stream m a -> Stream m a
cons x (Stream step state) = Stream step1 Nothing
    where
    {-# INLINE_LATE step1 #-}
    step1 _ Nothing = pure $ Yield x (Just state)
    step1 gst (Just st) = do
          (\case
            Yield a s -> Yield a (Just s)
            Skip  s   -> Skip (Just s)
            Stop      -> Stop) <$> step gst st

------------------------------------------------------------------------------
-- Unfolding
------------------------------------------------------------------------------

-- Adapted from vector package

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
-- in Stream.toList $ Stream.unfoldrM f 0
-- :}
-- [0,1,2]
--
{-# INLINE_NORMAL unfoldrM #-}
unfoldrM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
unfoldrM next = unfold (Unfold.unfoldrM next)
#else
unfoldrM next = Stream step
  where
    {- HLINT ignore "Eta reduce" -}
    {-# INLINE_LATE step #-}
    step _ st = Producer.unfoldrM next st
#endif

-- | Build a stream by unfolding a /pure/ step function @step@ starting from a
-- seed @s@.  The step function returns the next element in the stream and the
-- next seed value. When it is done it returns 'Nothing' and the stream ends.
-- For example,
--
-- >>> :{
-- let f b =
--         if b > 2
--         then Nothing
--         else Just (b, b + 1)
-- in Stream.toList $ Stream.unfoldr f 0
-- :}
-- [0,1,2]
--
{-# INLINE_LATE unfoldr #-}
unfoldr :: Monad m => (s -> Maybe (a, s)) -> s -> Stream m a
unfoldr f = unfoldrM (return . f)

------------------------------------------------------------------------------
-- From values
------------------------------------------------------------------------------

-- |
-- >>> repeatM act = Stream.iterateM (const act) act
-- >>> repeatM = Stream.sequence . Stream.repeat
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
{-# INLINE_NORMAL repeatM #-}
repeatM :: Monad m => m a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
repeatM = unfold Unfold.repeatM
#else
repeatM x = Stream (\_ _ -> x >>= \r -> return $ Yield r ()) ()
#endif

-- |
-- Generate an infinite stream by repeating a pure value.
--
-- >>> repeat = Stream.iterate id
-- >>> repeat x = Stream.repeatM (pure x)
--
{-# INLINE_NORMAL repeat #-}
repeat :: Monad m => a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
repeat x = repeatM (pure x)
#else
repeat x = Stream (\_ _ -> return $ Yield x ()) ()
#endif

-- Adapted from the vector package

-- |
-- >>> replicateM n = Stream.sequence . Stream.replicate n
--
-- Generate a stream by performing a monadic action @n@ times.
{-# INLINE_NORMAL replicateM #-}
replicateM :: Monad m => Int -> m a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
replicateM n p = unfold Unfold.replicateM (n, p)
#else
replicateM n p = Stream step n
  where
    {-# INLINE_LATE step #-}
    step _ (i :: Int)
      | i <= 0    = return Stop
      | otherwise = do
          x <- p
          return $ Yield x (i - 1)
#endif

-- |
-- >>> replicate n = Stream.take n . Stream.repeat
-- >>> replicate n x = Stream.replicateM n (pure x)
--
-- Generate a stream of length @n@ by repeating a value @n@ times.
--
{-# INLINE_NORMAL replicate #-}
replicate :: Monad m => Int -> a -> Stream m a
replicate n x = replicateM n (return x)

------------------------------------------------------------------------------
-- Time Enumeration
------------------------------------------------------------------------------

-- | @timesWith g@ returns a stream of time value tuples. The first component
-- of the tuple is an absolute time reference (epoch) denoting the start of the
-- stream and the second component is a time relative to the reference.
--
-- The argument @g@ specifies the granularity of the relative time in seconds.
-- A lower granularity clock gives higher precision but is more expensive in
-- terms of CPU usage. Any granularity lower than 1 ms is treated as 1 ms.
--
-- >>> import Control.Concurrent (threadDelay)
-- >>> f = Fold.drainMapM (\x -> print x >> threadDelay 1000000)
-- >>> Stream.fold f $ Stream.take 3 $ Stream.timesWith 0.01
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE_NORMAL timesWith #-}
timesWith :: MonadIO m => Double -> Stream m (AbsTime, RelTime64)
timesWith g = Stream step Nothing

    where

    {-# INLINE_LATE step #-}
    step _ Nothing = do
        clock <- liftIO $ asyncClock Monotonic g
        a <- liftIO $ readClock clock
        return $ Skip $ Just (clock, a)

    step _ s@(Just (clock, t0)) = do
        a <- liftIO $ readClock clock
        -- XXX we can perhaps use an AbsTime64 using a 64 bit Int for
        -- efficiency.  or maybe we can use a representation using Double for
        -- floating precision time
        return $ Yield (toAbsTime t0, toRelTime64 (a - t0)) s

-- | @absTimesWith g@ returns a stream of absolute timestamps using a clock of
-- granularity @g@ specified in seconds. A low granularity clock is more
-- expensive in terms of CPU usage.  Any granularity lower than 1 ms is treated
-- as 1 ms.
--
-- >>> f = Fold.drainMapM print
-- >>> Stream.fold f $ Stream.delayPre 1 $ Stream.take 3 $ Stream.absTimesWith 0.01
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE absTimesWith #-}
absTimesWith :: MonadIO m => Double -> Stream m AbsTime
absTimesWith = fmap (uncurry addToAbsTime64) . timesWith

-- | @relTimesWith g@ returns a stream of relative time values starting from 0,
-- using a clock of granularity @g@ specified in seconds. A low granularity
-- clock is more expensive in terms of CPU usage.  Any granularity lower than 1
-- ms is treated as 1 ms.
--
-- >>> f = Fold.drainMapM print
-- >>> Stream.fold f $ Stream.delayPre 1 $ Stream.take 3 $ Stream.relTimesWith 0.01
-- RelTime64 (NanoSecond64 ...)
-- RelTime64 (NanoSecond64 ...)
-- RelTime64 (NanoSecond64 ...)
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE relTimesWith #-}
relTimesWith :: MonadIO m => Double -> Stream m RelTime64
relTimesWith = fmap snd . timesWith

-- | @times@ returns a stream of time value tuples with clock of 10 ms
-- granularity. The first component of the tuple is an absolute time reference
-- (epoch) denoting the start of the stream and the second component is a time
-- relative to the reference.
--
-- >>> f = Fold.drainMapM (\x -> print x >> threadDelay 1000000)
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
-- >>> f = Fold.drainMapM print
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
-- >>> f = Fold.drainMapM print
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

-------------------------------------------------------------------------------
-- From Generators
-------------------------------------------------------------------------------

{-# INLINE_NORMAL fromIndicesM #-}
fromIndicesM :: Monad m => (Int -> m a) -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
fromIndicesM gen = unfold (Unfold.fromIndicesM gen) 0
#else
fromIndicesM gen = Stream step 0
  where
    {-# INLINE_LATE step #-}
    step _ i = do
       x <- gen i
       return $ Yield x (i + 1)
#endif

{-# INLINE fromIndices #-}
fromIndices :: Monad m => (Int -> a) -> Stream m a
fromIndices gen = fromIndicesM (return . gen)

-- Adapted from the vector package
{-# INLINE_NORMAL generateM #-}
generateM :: Monad m => Int -> (Int -> m a) -> Stream m a
generateM n gen = n `seq` Stream step 0
  where
    {-# INLINE_LATE step #-}
    step _ i | i < n     = do
                           x <- gen i
                           return $ Yield x (i + 1)
             | otherwise = return Stop

{-# INLINE generate #-}
generate :: Monad m => Int -> (Int -> a) -> Stream m a
generate n gen = generateM n (return . gen)

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

-- | Generate an infinite stream with the first element generated by the action
-- @m@ and each successive element derived by applying the monadic function @f@
-- on the previous element.
--
-- >>> :{
-- Stream.iterateM (\x -> print x >> return (x + 1)) (return 0)
--     & Stream.take 3
--     & Stream.toList
-- :}
-- 0
-- 1
-- [0,1,2]
--
{-# INLINE_NORMAL iterateM #-}
iterateM :: Monad m => (a -> m a) -> m a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
iterateM step = unfold (Unfold.iterateM step)
#else
iterateM step = Stream (\_ st -> st >>= \(!x) -> return $ Yield x (step x))
#endif

-- | Generate an infinite stream with @x@ as the first element and each
-- successive element derived by applying the function @f@ on the previous
-- element.
--
-- >>> Stream.toList $ Stream.take 5 $ Stream.iterate (+1) 1
-- [1,2,3,4,5]
--
{-# INLINE_NORMAL iterate #-}
iterate :: Monad m => (a -> a) -> a -> Stream m a
iterate step st = iterateM (return . step) (return st)

-------------------------------------------------------------------------------
-- From containers
-------------------------------------------------------------------------------

-- | Convert a list of monadic actions to a 'Stream'
{-# INLINE_LATE fromListM #-}
fromListM :: Monad m => [m a] -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
fromListM = unfold Unfold.fromListM
#else
fromListM = Stream step
  where
    {-# INLINE_LATE step #-}
    step _ (m:ms) = m >>= \x -> return $ Yield x ms
    step _ []     = return Stop
#endif

-- |
-- >>> fromFoldable = Prelude.foldr Stream.cons Stream.nil
--
-- Construct a stream from a 'Foldable' containing pure values:
--
-- /WARNING: O(n^2), suitable only for a small number of
-- elements in the stream/
--
{-# INLINE fromFoldable #-}
fromFoldable :: (Monad m, Foldable f) => f a -> Stream m a
fromFoldable = Prelude.foldr cons nil

-- |
-- >>> fromFoldableM = Prelude.foldr Stream.consM Stream.nil
--
-- Construct a stream from a 'Foldable' containing pure values:
--
-- /WARNING: O(n^2), suitable only for a small number of
-- elements in the stream/
--
{-# INLINE fromFoldableM #-}
fromFoldableM :: (Monad m, Foldable f) => f (m a) -> Stream m a
fromFoldableM = Prelude.foldr consM nil

-------------------------------------------------------------------------------
-- From pointers
-------------------------------------------------------------------------------

-- | Keep reading 'Storable' elements from an immutable 'Ptr' onwards.
--
-- /Unsafe:/ The caller is responsible for safe addressing.
--
-- /Pre-release/
{-# INLINE fromPtr #-}
fromPtr :: forall m a. (Monad m, Storable a) => Ptr a -> Stream m a
fromPtr = Stream step

    where

    {-# INLINE_LATE step #-}
    step _ p = do
        let !x = unsafeInlineIO $ peek p
        return $ Yield x (PTR_NEXT(p, a))

-- | Take @n@ 'Storable' elements starting from an immutable 'Ptr' onwards.
--
-- >>> fromPtrN n = Stream.take n . Stream.fromPtr
--
-- /Unsafe:/ The caller is responsible for safe addressing.
--
-- /Pre-release/
{-# INLINE fromPtrN #-}
fromPtrN :: (Monad m, Storable a) => Int -> Ptr a -> Stream m a
fromPtrN n = take n . fromPtr

-- | Read bytes from an immutable 'Addr#' until a 0 byte is encountered, the 0
-- byte is not included in the stream.
--
-- >>> :set -XMagicHash
-- >>> fromCString# addr = Stream.takeWhile (/= 0) $ Stream.fromPtr $ (Ptr addr :: Ptr Word8)
--
-- /Unsafe:/ The caller is responsible for safe addressing.
--
-- Note that this is completely safe when reading from Haskell string
-- literals because they are guaranteed to be NULL terminated:
--
-- >>> Stream.toList $ Stream.fromCString# "\1\2\3\0"#
-- [1,2,3]
--
{-# INLINE fromCString# #-}
fromCString# :: Monad m => Addr# -> Stream m Word8
fromCString# addr = takeWhile (/= 0) $ fromPtr $ Ptr addr

{-# DEPRECATED fromByteStr# "Please use fromCString# instead" #-}
{-# INLINE fromByteStr# #-}
fromByteStr# :: Monad m => Addr# -> Stream m Word8
fromByteStr# = fromCString#

-- | Read Word16 from an immutable 'Addr#' until a 0 Word16 is encountered, the
-- 0 Word16 is not included in the stream.
--
-- >>> :set -XMagicHash
-- >>> fromW16CString# addr = Stream.takeWhile (/= 0) $ Stream.fromPtr $ (Ptr addr :: Ptr Word16)
--
-- /Unsafe:/ The caller is responsible for safe addressing.
--
{-# INLINE fromW16CString# #-}
fromW16CString# :: Monad m => Addr# -> Stream m Word16
fromW16CString# addr = takeWhile (/= 0) $ fromPtr $ Ptr addr
