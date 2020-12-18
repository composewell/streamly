{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Generate
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.IsStream.Generate
    (
    -- ** From Values
      yield
    , yieldM
    , repeat
    , repeatM
    , replicate
    , replicateM

    -- ** Enumeration
    -- , Enumerable (..)
    -- , enumerate
    -- , enumerateTo

    -- ** Time Enumeration
    , times
    , absTimes
    , absTimesWith
    , relTimes
    , relTimesWith
    , durations
    , ticks
    , timeout
    , currentTime

    -- ** From Generators
    , unfoldr
    , unfoldrM
    , unfold
    , unfold0
    , fromIndices
    , fromIndicesM

    -- ** Iteration
    , iterate
    , iterateM

    -- ** Cyclic Elements
    , K.mfix

    -- ** From Containers
    , P.fromList
    , fromListM
    , K.fromFoldable
    , fromFoldableM
    , fromPrimIORef

    -- * Deprecated
    , each
    , fromHandle
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Data.Void (Void)
import Streamly.Internal.Data.Unfold.Types (Unfold)
import Streamly.Internal.Data.SVar (MonadAsync, Rate (..))
import Streamly.Internal.Data.Stream.Prelude (fromStreamS)
import Streamly.Internal.Data.Stream.StreamD (fromStreamD)
import Streamly.Internal.Data.Stream.StreamK (IsStream(consM))
import Streamly.Internal.Data.Stream.Serial (SerialT, WSerialT)
import Streamly.Internal.Data.Stream.Zip (ZipSerialM)
import Streamly.Internal.Data.Time.Units
       ( AbsTime
       , RelTime64, addToAbsTime64)
import Streamly.Internal.Data.IORef.Prim (Prim, IORef)

import qualified Streamly.Internal.Data.Stream.Prelude as P
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamD as D (toStreamK)
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as D
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Prelude
import qualified System.IO as IO
#ifdef USE_STREAMK_ONLY
import qualified Streamly.Internal.Data.Stream.StreamK as S
#else
import qualified Streamly.Internal.Data.Stream.StreamD as S (toStreamK)
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as S
#endif

import Prelude hiding
       ( filter, drop, dropWhile, take, takeWhile, zipWith, foldr
       , foldl, map, mapM, mapM_, sequence, all, any, sum, product, elem
       , notElem, maximum, minimum, head, last, tail, length, null
       , reverse, iterate, init, and, or, lookup, foldr1, (!!)
       , scanl, scanl1, replicate, concatMap, span, splitAt, break
       , repeat, concat, mconcat)

------------------------------------------------------------------------------
-- Generation by Unfolding
------------------------------------------------------------------------------

-- |
-- @
-- unfoldr step s =
--     case step s of
--         Nothing -> 'K.nil'
--         Just (a, b) -> a \`cons` unfoldr step b
-- @
--
-- Build a stream by unfolding a /pure/ step function @step@ starting from a
-- seed @s@.  The step function returns the next element in the stream and the
-- next seed value. When it is done it returns 'Nothing' and the stream ends.
-- For example,
--
-- @
-- let f b =
--         if b > 3
--         then Nothing
--         else Just (b, b + 1)
-- in toList $ unfoldr f 0
-- @
-- @
-- [0,1,2,3]
-- @
--
-- @since 0.1.0
{-# INLINE_EARLY unfoldr #-}
unfoldr :: (Monad m, IsStream t) => (b -> Maybe (a, b)) -> b -> t m a
unfoldr step seed = fromStreamS (S.unfoldr step seed)
{-# RULES "unfoldr fallback to StreamK" [1]
    forall a b. S.toStreamK (S.unfoldr a b) = K.unfoldr a b #-}

-- | Build a stream by unfolding a /monadic/ step function starting from a
-- seed.  The step function returns the next element in the stream and the next
-- seed value. When it is done it returns 'Nothing' and the stream ends. For
-- example,
--
-- @
-- let f b =
--         if b > 3
--         then return Nothing
--         else print b >> return (Just (b, b + 1))
-- in drain $ unfoldrM f 0
-- @
-- @
--  0
--  1
--  2
--  3
-- @
-- When run concurrently, the next unfold step can run concurrently with the
-- processing of the output of the previous step.  Note that more than one step
-- cannot run concurrently as the next step depends on the output of the
-- previous step.
--
-- @
-- (asyncly $ S.unfoldrM (\\n -> liftIO (threadDelay 1000000) >> return (Just (n, n + 1))) 0)
--     & S.foldlM' (\\_ a -> threadDelay 1000000 >> print a) ()
-- @
--
-- /Concurrent/
--
-- /Since: 0.1.0/
{-# INLINE_EARLY unfoldrM #-}
unfoldrM :: (IsStream t, MonadAsync m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM = K.unfoldrM

{-# RULES "unfoldrM serial" unfoldrM = unfoldrMSerial #-}
{-# INLINE_EARLY unfoldrMSerial #-}
unfoldrMSerial :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> SerialT m a
unfoldrMSerial = Serial.unfoldrM

{-# RULES "unfoldrM wSerial" unfoldrM = unfoldrMWSerial #-}
{-# INLINE_EARLY unfoldrMWSerial #-}
unfoldrMWSerial :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> WSerialT m a
unfoldrMWSerial = Serial.unfoldrM

{-# RULES "unfoldrM zipSerial" unfoldrM = unfoldrMZipSerial #-}
{-# INLINE_EARLY unfoldrMZipSerial #-}
unfoldrMZipSerial :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> ZipSerialM m a
unfoldrMZipSerial = Serial.unfoldrM

-- | Convert an 'Unfold' into a stream by supplying it an input seed.
--
-- >>> unfold (UF.replicateM 10) (putStrLn "hello")
--
-- /Since: 0.7.0/
{-# INLINE unfold #-}
unfold :: (IsStream t, Monad m) => Unfold m a b -> a -> t m b
unfold unf x = fromStreamD $ D.unfold unf x

-- | Convert an 'Unfold' with a closed input end into a stream.
--
-- /Internal/
{-# INLINE unfold0 #-}
unfold0 :: (IsStream t, Monad m) => Unfold m Void b -> t m b
unfold0 unf = unfold unf (error "unfold0: unexpected void evaluation")

------------------------------------------------------------------------------
-- Specialized Generation
------------------------------------------------------------------------------

-- Faster than yieldM because there is no bind.
--
-- |
-- @
-- yield a = a \`cons` nil
-- @
--
-- Create a singleton stream from a pure value.
--
-- The following holds in monadic streams, but not in Zip streams:
--
-- @
-- yield = pure
-- yield = yieldM . pure
-- @
--
-- In Zip applicative streams 'yield' is not the same as 'pure' because in that
-- case 'pure' is equivalent to 'repeat' instead. 'yield' and 'pure' are
-- equally efficient, in other cases 'yield' may be slightly more efficient
-- than the other equivalent definitions.
--
-- @since 0.4.0
{-# INLINE yield #-}
yield :: IsStream t => a -> t m a
yield = K.yield

-- |
-- @
-- yieldM m = m \`consM` nil
-- @
--
-- Create a singleton stream from a monadic action.
--
-- @
-- > toList $ yieldM getLine
-- hello
-- ["hello"]
-- @
--
-- @since 0.4.0
{-# INLINE yieldM #-}
yieldM :: (Monad m, IsStream t) => m a -> t m a
yieldM = K.yieldM

-- |
-- @
-- fromIndices f = let g i = f i \`cons` g (i + 1) in g 0
-- @
--
-- Generate an infinite stream, whose values are the output of a function @f@
-- applied on the corresponding index.  Index starts at 0.
--
-- @
-- > S.toList $ S.take 5 $ S.fromIndices id
-- [0,1,2,3,4]
-- @
--
-- @since 0.6.0
{-# INLINE fromIndices #-}
fromIndices :: (IsStream t, Monad m) => (Int -> a) -> t m a
fromIndices = fromStreamS . S.fromIndices

--
-- |
-- @
-- fromIndicesM f = let g i = f i \`consM` g (i + 1) in g 0
-- @
--
-- Generate an infinite stream, whose values are the output of a monadic
-- function @f@ applied on the corresponding index. Index starts at 0.
--
-- /Concurrent/
--
-- @since 0.6.0
{-# INLINE_EARLY fromIndicesM #-}
fromIndicesM :: (IsStream t, MonadAsync m) => (Int -> m a) -> t m a
fromIndicesM = K.fromIndicesM

{-# RULES "fromIndicesM serial" fromIndicesM = fromIndicesMSerial #-}
{-# INLINE fromIndicesMSerial #-}
fromIndicesMSerial :: MonadAsync m => (Int -> m a) -> SerialT m a
fromIndicesMSerial = fromStreamS . S.fromIndicesM

-- |
-- @
-- replicateM = take n . repeatM
-- @
--
-- Generate a stream by performing a monadic action @n@ times. Same as:
--
-- @
-- drain $ serially $ S.replicateM 10 $ (threadDelay 1000000 >> print 1)
-- drain $ asyncly  $ S.replicateM 10 $ (threadDelay 1000000 >> print 1)
-- @
--
-- /Concurrent/
--
-- @since 0.1.1
{-# INLINE_EARLY replicateM #-}
replicateM :: (IsStream t, MonadAsync m) => Int -> m a -> t m a
replicateM = K.replicateM

{-# RULES "replicateM serial" replicateM = replicateMSerial #-}
{-# INLINE replicateMSerial #-}
replicateMSerial :: MonadAsync m => Int -> m a -> SerialT m a
replicateMSerial n = fromStreamS . S.replicateM n

-- |
-- @
-- replicate = take n . repeat
-- @
--
-- Generate a stream of length @n@ by repeating a value @n@ times.
--
-- @since 0.6.0
{-# INLINE_NORMAL replicate #-}
replicate :: (IsStream t, Monad m) => Int -> a -> t m a
replicate n = fromStreamS . S.replicate n

-- |
-- Generate an infinite stream by repeating a pure value.
--
-- @since 0.4.0
{-# INLINE_NORMAL repeat #-}
repeat :: (IsStream t, Monad m) => a -> t m a
repeat = fromStreamS . S.repeat

-- |
-- @
-- repeatM = fix . consM
-- repeatM = cycle1 . yieldM
-- @
--
-- Generate a stream by repeatedly executing a monadic action forever.
--
-- @
-- drain $ serially $ S.take 10 $ S.repeatM $ (threadDelay 1000000 >> print 1)
-- drain $ asyncly  $ S.take 10 $ S.repeatM $ (threadDelay 1000000 >> print 1)
-- @
--
-- /Concurrent, infinite (do not use with 'parallely')/
--
-- @since 0.2.0
{-# INLINE_EARLY repeatM #-}
repeatM :: (IsStream t, MonadAsync m) => m a -> t m a
repeatM = K.repeatM

{-# RULES "repeatM serial" repeatM = repeatMSerial #-}
{-# INLINE repeatMSerial #-}
repeatMSerial :: MonadAsync m => m a -> SerialT m a
repeatMSerial = fromStreamS . S.repeatM

-- |
-- @
-- iterate f x = x \`cons` iterate f x
-- @
--
-- Generate an infinite stream with @x@ as the first element and each
-- successive element derived by applying the function @f@ on the previous
-- element.
--
-- @
-- > S.toList $ S.take 5 $ S.iterate (+1) 1
-- [1,2,3,4,5]
-- @
--
-- @since 0.1.2
{-# INLINE_NORMAL iterate #-}
iterate :: (IsStream t, Monad m) => (a -> a) -> a -> t m a
iterate step = fromStreamS . S.iterate step

-- |
-- @
-- iterateM f m = m >>= \a -> return a \`consM` iterateM f (f a)
-- @
--
-- Generate an infinite stream with the first element generated by the action
-- @m@ and each successive element derived by applying the monadic function
-- @f@ on the previous element.
--
-- When run concurrently, the next iteration can run concurrently with the
-- processing of the previous iteration. Note that more than one iteration
-- cannot run concurrently as the next iteration depends on the output of the
-- previous iteration.
--
-- @
-- drain $ serially $ S.take 10 $ S.iterateM
--      (\\x -> threadDelay 1000000 >> print x >> return (x + 1)) (return 0)
--
-- drain $ asyncly  $ S.take 10 $ S.iterateM
--      (\\x -> threadDelay 1000000 >> print x >> return (x + 1)) (return 0)
-- @
--
-- /Concurrent/
--
-- /Since: 0.1.2/
--
-- /Since: 0.7.0 (signature change)/
{-# INLINE_EARLY iterateM #-}
iterateM :: (IsStream t, MonadAsync m) => (a -> m a) -> m a -> t m a
iterateM = K.iterateM

{-# RULES "iterateM serial" iterateM = iterateMSerial #-}
{-# INLINE iterateMSerial #-}
iterateMSerial :: MonadAsync m => (a -> m a) -> m a -> SerialT m a
iterateMSerial step = fromStreamS . S.iterateM step

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- |
-- @
-- fromListM = 'Prelude.foldr' 'K.consM' 'K.nil'
-- @
--
-- Construct a stream from a list of monadic actions. This is more efficient
-- than 'fromFoldableM' for serial streams.
--
-- @since 0.4.0
{-# INLINE_EARLY fromListM #-}
fromListM :: (MonadAsync m, IsStream t) => [m a] -> t m a
fromListM = fromStreamD . D.fromListM
{-# RULES "fromListM fallback to StreamK" [1]
    forall a. D.toStreamK (D.fromListM a) = fromFoldableM a #-}

-- |
-- @
-- fromFoldableM = 'Prelude.foldr' 'consM' 'K.nil'
-- @
--
-- Construct a stream from a 'Foldable' containing monadic actions.
--
-- @
-- drain $ serially $ S.fromFoldableM $ replicateM 10 (threadDelay 1000000 >> print 1)
-- drain $ asyncly  $ S.fromFoldableM $ replicateM 10 (threadDelay 1000000 >> print 1)
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite containers)/
--
-- @since 0.3.0
{-# INLINE fromFoldableM #-}
fromFoldableM :: (IsStream t, MonadAsync m, Foldable f) => f (m a) -> t m a
fromFoldableM = Prelude.foldr consM K.nil

-- | Same as 'fromFoldable'.
--
-- @since 0.1.0
{-# DEPRECATED each "Please use fromFoldable instead." #-}
{-# INLINE each #-}
each :: (IsStream t, Foldable f) => f a -> t m a
each = K.fromFoldable

-- | Read lines from an IO Handle into a stream of Strings.
--
-- @since 0.1.0
{-# DEPRECATED fromHandle
   "Please use Streamly.FileSystem.Handle module (see the changelog)" #-}
fromHandle :: (IsStream t, MonadIO m) => IO.Handle -> t m String
fromHandle h = go
  where
  go = K.mkStream $ \_ yld _ stp -> do
        eof <- liftIO $ IO.hIsEOF h
        if eof
        then stp
        else do
            str <- liftIO $ IO.hGetLine h
            yld str go

-- | Construct a stream by reading a 'Prim' 'IORef' repeatedly.
--
-- /Internal/
--
{-# INLINE fromPrimIORef #-}
fromPrimIORef :: (IsStream t, MonadIO m, Prim a) => IORef a -> t m a
fromPrimIORef = fromStreamD . D.fromPrimIORef

------------------------------------------------------------------------------
-- Time related
------------------------------------------------------------------------------

-- | @timesWith g@ returns a stream of time value tuples. The first component
-- of the tuple is an absolute time reference (epoch) denoting the start of the
-- stream and the second component is a time relative to the reference.
--
-- The argument @g@ specifies the granularity of the relative time in seconds.
-- A lower granularity clock gives higher precision but is more expensive in
-- terms of CPU usage. Any granularity lower than 1 ms is treated as 1 ms.
--
-- @
-- >>> S.mapM_ (\x -> print x >> threadDelay 1000000) $ S.timesWith 0.01
-- > (AbsTime (TimeSpec {sec = 2496295, nsec = 536223000}),RelTime64 (NanoSecond64 0))
-- > (AbsTime (TimeSpec {sec = 2496295, nsec = 536223000}),RelTime64 (NanoSecond64 1002028000))
-- > (AbsTime (TimeSpec {sec = 2496295, nsec = 536223000}),RelTime64 (NanoSecond64 1996656000))
-- @
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Internal/
--
{-# INLINE timesWith #-}
timesWith :: (IsStream t, MonadAsync m) => Double -> t m (AbsTime, RelTime64)
timesWith g = fromStreamD $ D.times g

-- | @times@ returns a stream of time value tuples with clock of 10 ms
-- granularity. The first component of the tuple is an absolute time reference
-- (epoch) denoting the start of the stream and the second component is a time
-- relative to the reference.
--
-- @
-- >>> S.mapM_ (\x -> print x >> threadDelay 1000000) $ S.times
-- > (AbsTime (TimeSpec {sec = 2496295, nsec = 536223000}),RelTime64 (NanoSecond64 0))
-- > (AbsTime (TimeSpec {sec = 2496295, nsec = 536223000}),RelTime64 (NanoSecond64 1002028000))
-- > (AbsTime (TimeSpec {sec = 2496295, nsec = 536223000}),RelTime64 (NanoSecond64 1996656000))
-- @
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Internal/
--
{-# INLINE times #-}
times :: (IsStream t, MonadAsync m) => t m (AbsTime, RelTime64)
times = timesWith 0.01

-- | @absTimesWith g@ returns a stream of absolute timestamps using a clock of
-- granularity @g@ specified in seconds. A low granularity clock is more
-- expensive in terms of CPU usage.  Any granularity lower than 1 ms is treated
-- as 1 ms.
--
-- @
-- >>> S.mapM_ print $ S.delayPre 1 $ S.absTimesWith 0.01
-- @
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Internal/
--
{-# INLINE absTimesWith #-}
absTimesWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m AbsTime
absTimesWith = fmap (uncurry addToAbsTime64) . timesWith

-- | @absTimes@ returns a stream of absolute timestamps using a clock of 10 ms
-- granularity.
--
-- @
-- >>> S.mapM_ print $ S.delayPre 1 $ S.absTimes
-- @
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Internal/
--
{-# INLINE absTimes #-}
absTimes :: (IsStream t, MonadAsync m, Functor (t m)) => t m AbsTime
absTimes = fmap (uncurry addToAbsTime64) times

{-# DEPRECATED currentTime "Please use absTimes instead" #-}
{-# INLINE currentTime #-}
currentTime :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m AbsTime
currentTime = absTimesWith

-- | @relTimesWith g@ returns a stream of relative time values starting from 0,
-- using a clock of granularity @g@ specified in seconds. A low granularity
-- clock is more expensive in terms of CPU usage.  Any granularity lower than 1
-- ms is treated as 1 ms.
--
-- @
-- >>> S.mapM_ print $ S.delayPre 1 $ S.relTimesWith 0.01
-- > RelTime64 (NanoSecond64 0)
-- > RelTime64 (NanoSecond64 91139000)
-- > RelTime64 (NanoSecond64 204052000)
-- @
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Internal/
--
{-# INLINE relTimesWith #-}
relTimesWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m RelTime64
relTimesWith = fmap snd . timesWith

-- | @relTimes@ returns a stream of relative time values starting from 0,
-- using a clock of granularity 10 ms.
--
-- @
-- >>> S.mapM_ print $ S.delayPre 1 $ S.relTimes
-- @
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Internal/
--
{-# INLINE relTimes #-}
relTimes :: (IsStream t, MonadAsync m, Functor (t m)) => t m RelTime64
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
durations :: -- (IsStream t, MonadAsync m) =>
    Double -> t m RelTime64
durations = undefined

-- | Generate ticks at the specified rate. The rate is adaptive, the tick
-- generation speed can be increased or decreased at different times to achieve
-- the specified rate.  The specific behavior for different styles of 'Rate'
-- specifications is documented under 'Rate'.  The effective maximum rate
-- achieved by a stream is governed by the processor speed.
--
-- /Unimplemented/
--
{-# INLINE ticks #-}
ticks :: -- (IsStream t, MonadAsync m) =>
    Rate -> t m ()
ticks = undefined

-- | Generate a singleton event at or after the specified absolute time. Note
-- that this is different from a threadDelay, a threadDelay starts from the
-- time when the action is evaluated, whereas if we use AbsTime based timeout
-- it will immediately expire if the action is evaluated too late.
--
-- /Unimplemented/
--
{-# INLINE timeout #-}
timeout :: -- (IsStream t, MonadAsync m) =>
    AbsTime -> t m ()
timeout = undefined
