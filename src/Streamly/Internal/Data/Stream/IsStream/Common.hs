{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans  #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Common
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Bottom level IsStream module that can be used by all other upper level
-- IsStream modules.

module Streamly.Internal.Data.Stream.IsStream.Common {-# DEPRECATED "Please use \"Streamly.Data.Stream.*\" instead." #-}
    (
    -- * Generation
      fromPure
    , fromEffect
    , repeatM
    , timesWith
    , absTimesWith
    , relTimesWith

    -- * Elimination
    , foldContinue
    , fold

    -- * Transformation
    , map
    , scanlMAfter'
    , postscanlMAfter'
    , postscanlM'
    , smapM
    , foldManyPost
    -- $smapM_Notes
    , take
    , takeWhile
    , takeEndBy
    , drop
    , findIndices
    , intersperseM
    , interjectSuffix
    , reverse
    , reverse'

    -- * Concurrent
    , mkAsync
    , mkParallel
    , parallelFst

    -- * Nesting
    , concatM
    , concatMapM
    , concatMap
    , splitOnSeq

    -- * Zipping
    , zipWithM
    , zipWith

    -- * Deprecated
    , yield
    , yieldM
    )
where

#include "inline.hs"

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Storable (Storable)
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Array (Array)
import Streamly.Internal.Data.Fold (Fold (..))
import Streamly.Internal.Data.Stream.IsStream.Combinators (maxYields)
import Streamly.Internal.Data.Stream.IsStream.Type
    (IsStream(..), fromStreamD, toStreamD)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Time.Units (AbsTime, RelTime64, addToAbsTime64)
import Streamly.Internal.System.IO (defaultChunkSize)
import Streamly.Data.MutByteArray (Unbox)

import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.Stream.Async as Async
import qualified Streamly.Internal.Data.Stream.IsStream.Type as IsStream
import qualified Streamly.Internal.Data.Stream.Parallel as Par
import qualified Streamly.Internal.Data.StreamK as K
    (fromPure, fromEffect, repeatMWith, reverse)
import qualified Streamly.Internal.Data.Stream as D
    (repeatM, timesWith, foldAddLazy, map, scanlMAfter', postscanlMAfter'
    , postscanlM', take,  takeWhile, takeEndBy, drop, findIndices
    , fromStreamK, toStreamK, concatMapM, concatMap, foldManyPost, splitOnSeq
    , zipWithM, zipWith, intersperseM, reverse, fold)

import Prelude hiding (take, takeWhile, drop, reverse, concatMap, map, zipWith)

--
-- $setup
-- >>> :m
-- >>> :set -fno-warn-deprecations
-- >>> import Control.Concurrent (threadDelay)
-- >>> import Control.Monad (join)
-- >>> import Control.Monad.Trans.Class (lift)
-- >>> import Data.Function (fix, (&))
-- >>> import Data.Semigroup (cycle1)
-- >>> import Prelude hiding (take, takeWhile, drop, reverse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Data.Array as Array
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Unfold as Unfold

------------------------------------------------------------------------------
-- Generation
------------------------------------------------------------------------------

-- Faster than fromEffect because there is no bind.
--
-- |
-- @
-- fromPure a = a \`cons` nil
-- @
--
-- Create a singleton stream from a pure value.
--
-- The following holds in monadic streams, but not in Zip streams:
--
-- @
-- fromPure = pure
-- fromPure = fromEffect . pure
-- @
--
-- In Zip applicative streams 'fromPure' is not the same as 'pure' because in that
-- case 'pure' is equivalent to 'repeat' instead. 'fromPure' and 'pure' are
-- equally efficient, in other cases 'fromPure' may be slightly more efficient
-- than the other equivalent definitions.
--
-- /Since: 0.8.0 (Renamed yield to fromPure)/
--
{-# INLINE fromPure #-}
fromPure :: IsStream t => a -> t m a
fromPure = fromStream . K.fromPure

-- | Same as 'fromPure'
--
-- @since 0.4.0
{-# DEPRECATED yield "Please use fromPure instead." #-}
{-# INLINE yield #-}
yield :: IsStream t => a -> t m a
yield = fromPure

-- |
-- @
-- fromEffect m = m \`consM` nil
-- @
--
-- Create a singleton stream from a monadic action.
--
-- @
-- > Stream.toList $ Stream.fromEffect getLine
-- hello
-- ["hello"]
-- @
--
-- /Since: 0.8.0 (Renamed yieldM to fromEffect)/
--
{-# INLINE fromEffect #-}
fromEffect :: (Monad m, IsStream t) => m a -> t m a
fromEffect = fromStream . K.fromEffect

-- | Same as 'fromEffect'
--
-- @since 0.4.0
{-# DEPRECATED yieldM "Please use fromEffect instead." #-}
{-# INLINE yieldM #-}
yieldM :: (Monad m, IsStream t) => m a -> t m a
yieldM = fromEffect
-- |
-- >>> repeatM = fix . consM
-- >>> repeatM = cycle1 . fromEffect
--
-- Generate a stream by repeatedly executing a monadic action forever.
--
-- >>> :{
-- repeatAsync =
--        Stream.repeatM (threadDelay 1000000 >> print 1)
--      & Stream.take 10
--      & Stream.fromAsync
--      & Stream.drain
-- :}
--
-- /Concurrent, infinite (do not use with 'fromParallel')/
--
-- @since 0.2.0
{-# INLINE_EARLY repeatM #-}
repeatM :: (IsStream t, MonadAsync m) => m a -> t m a
repeatM = K.repeatMWith IsStream.consM

{-# RULES "repeatM serial" repeatM = repeatMSerial #-}
{-# INLINE repeatMSerial #-}
repeatMSerial :: MonadAsync m => m a -> SerialT m a
repeatMSerial = fromStreamD . D.repeatM

------------------------------------------------------------------------------
-- Generation - Time related
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
-- >>> import Streamly.Internal.Data.Stream.IsStream.Common as Stream (timesWith)
-- >>> Stream.mapM_ (\x -> print x >> threadDelay 1000000) $ Stream.take 3 $ Stream.timesWith 0.01
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE timesWith #-}
timesWith :: (IsStream t, MonadAsync m) => Double -> t m (AbsTime, RelTime64)
timesWith g = fromStreamD $ D.timesWith g

-- | @absTimesWith g@ returns a stream of absolute timestamps using a clock of
-- granularity @g@ specified in seconds. A low granularity clock is more
-- expensive in terms of CPU usage.  Any granularity lower than 1 ms is treated
-- as 1 ms.
--
-- >>> Stream.mapM_ print $ Stream.delayPre 1 $ Stream.take 3 $ absTimesWith 0.01
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE absTimesWith #-}
absTimesWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m AbsTime
absTimesWith = fmap (uncurry addToAbsTime64) . timesWith

-- | @relTimesWith g@ returns a stream of relative time values starting from 0,
-- using a clock of granularity @g@ specified in seconds. A low granularity
-- clock is more expensive in terms of CPU usage.  Any granularity lower than 1
-- ms is treated as 1 ms.
--
-- >>> Stream.mapM_ print $ Stream.delayPre 1 $ Stream.take 3 $ Stream.relTimesWith 0.01
-- RelTime64 (NanoSecond64 ...)
-- RelTime64 (NanoSecond64 ...)
-- RelTime64 (NanoSecond64 ...)
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE relTimesWith #-}
relTimesWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m RelTime64
relTimesWith = fmap snd . timesWith

------------------------------------------------------------------------------
-- Elimination - Running a Fold
------------------------------------------------------------------------------

-- | We can create higher order folds using 'foldContinue'. We can fold a
-- number of streams to a given fold efficiently with full stream fusion. For
-- example, to fold a list of streams on the same sum fold:
--
-- > concatFold = Prelude.foldl Stream.foldContinue Fold.sum
--
-- > fold f = Fold.extractM . Stream.foldContinue f
--
-- /Internal/
{-# INLINE foldContinue #-}
foldContinue :: Monad m => Fold m a b -> SerialT m a -> Fold m a b
foldContinue f s = D.foldAddLazy f $ IsStream.toStreamD s

-- | Fold a stream using the supplied left 'Fold' and reducing the resulting
-- expression strictly at each step. The behavior is similar to 'foldl''. A
-- 'Fold' can terminate early without consuming the full stream. See the
-- documentation of individual 'Fold's for termination behavior.
--
-- >>> Stream.fold Fold.sum (Stream.enumerateFromTo 1 100)
-- 5050
--
-- Folds never fail, therefore, they produce a default value even when no input
-- is provided. It means we can always fold an empty stream and get a valid
-- result.  For example:
--
-- >>> Stream.fold Fold.sum Stream.nil
-- 0
--
-- However, 'foldMany' on an empty stream results in an empty stream.
-- Therefore, @Stream.fold f@ is not the same as @Stream.head . Stream.foldMany
-- f@.
--
-- @fold f = Stream.parse (Parser.fromFold f)@
--
-- @since 0.7.0
{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> SerialT m a -> m b
fold fl strm = D.fold fl $ IsStream.toStreamD strm

------------------------------------------------------------------------------
-- Transformation
------------------------------------------------------------------------------

-- |
-- @
-- map = fmap
-- @
--
-- Same as 'fmap'.
--
-- @
-- > D.toList $ D.map (+1) $ D.fromList [1,2,3]
-- [2,3,4]
-- @
--
-- @since 0.4.0
{-# INLINE map #-}
map :: (IsStream t, Monad m) => (a -> b) -> t m a -> t m b
map f = fromStreamD . D.map f . toStreamD

-- | @scanlMAfter' accumulate initial done stream@ is like 'scanlM'' except
-- that it provides an additional @done@ function to be applied on the
-- accumulator when the stream stops. The result of @done@ is also emitted in
-- the stream.
--
-- This function can be used to allocate a resource in the beginning of the
-- scan and release it when the stream ends or to flush the internal state of
-- the scan at the end.
--
-- /Pre-release/
--
{-# INLINE scanlMAfter' #-}
scanlMAfter' :: (IsStream t, Monad m)
    => (b -> a -> m b) -> m b -> (b -> m b) -> t m a -> t m b
scanlMAfter' step initial done stream =
    fromStreamD $ D.scanlMAfter' step initial done $ toStreamD stream

{-# INLINE postscanlMAfter' #-}
postscanlMAfter' :: (IsStream t, Monad m)
    => (b -> a -> m b) -> m b -> (b -> m b) -> t m a -> t m b
postscanlMAfter' step initial done stream =
    fromStreamD $ D.postscanlMAfter' step initial done $ toStreamD stream

-- XXX this needs to be concurrent
-- | Like 'postscanl'' but with a monadic step function and a monadic seed.
--
-- >>> postscanlM' f z xs = Stream.drop 1 $ Stream.scanlM' f z xs
--
-- /Since: 0.7.0/
--
-- /Since: 0.8.0 (signature change)/
{-# INLINE postscanlM' #-}
postscanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> m b -> t m a -> t m b
postscanlM' step z m = fromStreamD $ D.postscanlM' step z $ toStreamD m

-- $smapM_Notes
--
-- The stateful step function can be simplified to @(s -> a -> m b)@ to provide
-- a read-only environment. However, that would just be 'mapM'.
--
-- The initial action could be @m (s, Maybe b)@, and we can also add a final
-- action @s -> m (Maybe b)@. This can be used to get pre/post scan like
-- functionality and also to flush the state in the end like scanlMAfter'.
-- We can also use it along with a fusible version of bracket to get
-- scanlMAfter' like functionality. See issue #677.
--
-- This can be further generalized to a type similar to Fold/Parser, giving it
-- filtering and parsing capability as well (this is in fact equivalent to
-- parseMany):
--
-- smapM :: (s -> a -> m (Step s b)) -> m s -> t m a -> t m b
--

-- | A stateful 'mapM', equivalent to a left scan, more like mapAccumL.
-- Hopefully, this is a better alternative to @scan@. Separation of state from
-- the output makes it easier to think in terms of a shared state, and also
-- makes it easier to keep the state fully strict and the output lazy.
--
-- See also: 'scanlM''
--
-- /Pre-release/
--
{-# INLINE smapM #-}
smapM :: (IsStream t, Monad m) =>
       (s -> a -> m (s, b))
    -> m s
    -> t m a
    -> t m b
smapM step initial stream =
    -- XXX implement this directly instead of using scanlM'
    -- Once we have postscanlM' with monadic initial we can use this code
    -- let r = postscanlM'
    --              (\(s, _) a -> step s a)
    --              (fmap (,undefined) initial)
    --              stream
    let r = postscanlM'
                (\(s, _) a -> step s a)
                (fmap (,undefined) initial)
                stream
     in map snd r

------------------------------------------------------------------------------
-- Transformation - Trimming
------------------------------------------------------------------------------

-- | Take first 'n' elements from the stream and discard the rest.
--
-- @since 0.1.0
{-# INLINE take #-}
take :: (IsStream t, Monad m) => Int -> t m a -> t m a
take n m = fromStreamD $ D.take n $ toStreamD
    (maxYields (Just (fromIntegral n)) m)

-- | End the stream as soon as the predicate fails on an element.
--
-- @since 0.1.0
{-# INLINE takeWhile #-}
takeWhile :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m a
takeWhile p m = fromStreamD $ D.takeWhile p $ toStreamD m

{-# INLINE takeEndBy #-}
takeEndBy :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m a
takeEndBy p m = fromStreamD $ D.takeEndBy p $ toStreamD m

-- | Discard first 'n' elements from the stream and take the rest.
--
-- @since 0.1.0
{-# INLINE drop #-}
drop :: (IsStream t, Monad m) => Int -> t m a -> t m a
drop n m = fromStreamD $ D.drop n $ toStreamD m

------------------------------------------------------------------------------
-- Searching
------------------------------------------------------------------------------

-- | Find all the indices where the element in the stream satisfies the given
-- predicate.
--
-- > findIndices = fold Fold.findIndices
--
-- @since 0.5.0
{-# INLINE findIndices #-}
findIndices :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m Int
findIndices p m = fromStreamD $ D.findIndices p (toStreamD m)

------------------------------------------------------------------------------
-- Transformation by Inserting
------------------------------------------------------------------------------

-- intersperseM = intersperseMWith 1

-- | Insert an effect and its output before consuming an element of a stream
-- except the first one.
--
-- >>> Stream.toList $ Stream.trace putChar $ Stream.intersperseM (putChar '.' >> return ',') $ Stream.fromList "hello"
-- h.,e.,l.,l.,o"h,e,l,l,o"
--
-- Be careful about the order of effects. In the above example we used trace
-- after the intersperse, if we use it before the intersperse the output would
-- be he.l.l.o."h,e,l,l,o".
--
-- >>> Stream.toList $ Stream.intersperseM (putChar '.' >> return ',') $ Stream.trace putChar $ Stream.fromList "hello"
-- he.l.l.o."h,e,l,l,o"
--
-- @since 0.5.0
{-# INLINE intersperseM #-}
intersperseM :: (IsStream t, MonadAsync m) => m a -> t m a -> t m a
intersperseM m = fromStreamD . D.intersperseM m . toStreamD

-- | Intersperse a monadic action into the input stream after every @n@
-- seconds.
--
-- @
-- > import Control.Concurrent (threadDelay)
-- > Stream.drain $ Stream.interjectSuffix 1 (putChar ',') $ Stream.mapM (\x -> threadDelay 1000000 >> putChar x) $ Stream.fromList "hello"
-- h,e,l,l,o
-- @
--
-- /Pre-release/
{-# INLINE interjectSuffix #-}
interjectSuffix
    :: (IsStream t, MonadAsync m)
    => Double -> m a -> t m a -> t m a
interjectSuffix n f xs = xs `parallelFst` repeatM timed
    where timed = liftIO (threadDelay (round $ n * 1000000)) >> f

------------------------------------------------------------------------------
-- Transformation by Reordering
------------------------------------------------------------------------------

-- XXX Use a compact region list to temporarily store the list, in both reverse
-- as well as in reverse'.
--
-- /Note:/ 'reverse'' is much faster than this, use that when performance
-- matters.
--
-- | Returns the elements of the stream in reverse order.  The stream must be
-- finite. Note that this necessarily buffers the entire stream in memory.
--
-- >>> reverse = Stream.foldlT (flip Stream.cons) Stream.nil
--
-- /Since 0.7.0 (Monad m constraint)/
--
-- /Since: 0.1.1/
{-# INLINE reverse #-}
reverse :: (IsStream t, Monad m) => t m a -> t m a
reverse s = fromStreamD $ D.reverse $ toStreamD s

-- | Like 'reverse' but several times faster, requires a 'Storable' instance.
--
-- /Pre-release/
{-# INLINE reverse' #-}
reverse' :: (IsStream t, MonadIO m, Unbox a) => t m a -> t m a
-- reverse' s = fromStreamD $ D.reverse' $ toStreamD s
reverse' =
        fromStreamD
        . A.concatRev -- unfoldMany A.readerRev
        . D.fromStreamK
        . K.reverse
        . D.toStreamK
        . A.chunksOf defaultChunkSize
        . toStreamD

------------------------------------------------------------------------------
-- Concurrent Transformations and Combining
------------------------------------------------------------------------------

-- | Make the stream producer and consumer run concurrently by introducing a
-- buffer between them. The producer thread evaluates the input stream until
-- the buffer fills, it terminates if the buffer is full and a worker thread is
-- kicked off again to evaluate the remaining stream when there is space in the
-- buffer.  The consumer consumes the stream lazily from the buffer.
--
-- /Since: 0.2.0 (Streamly)/
--
-- @since 0.8.0
--
{-# INLINE_NORMAL mkAsync #-}
mkAsync :: (IsStream t, MonadAsync m) => t m a -> t m a
mkAsync = fromStreamD . Async.mkAsyncD . toStreamD

-- Compare with mkAsync. mkAsync uses an Async style SVar whereas this uses a
-- parallel style SVar for evaluation. Currently, parallel style cannot use
-- rate control whereas Async style can use rate control. In async style SVar
-- the worker thread terminates when the buffer is full whereas in Parallel
-- style it blocks.
--
-- | Make the stream producer and consumer run concurrently by introducing a
-- buffer between them. The producer thread evaluates the input stream until
-- the buffer fills, it blocks if the buffer is full until there is space in
-- the buffer. The consumer consumes the stream lazily from the buffer.
--
-- @mkParallel = IsStream.fromStreamD . mkParallelD . IsStream.toStreamD@
--
-- /Pre-release/
--
{-# INLINE_NORMAL mkParallel #-}
mkParallel :: (IsStream t, MonadAsync m) => t m a -> t m a
mkParallel = fromStreamD . Par.mkParallelD . toStreamD

-- | Like `parallel` but stops the output as soon as the first stream stops.
--
-- /Pre-release/
{-# INLINE parallelFst #-}
parallelFst :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
parallelFst m1 m2 = fromStream $ Par.parallelFstK (toStream m1) (toStream m2)

------------------------------------------------------------------------------
-- Combine streams and flatten
------------------------------------------------------------------------------

-- | Map a stream producing monadic function on each element of the stream
-- and then flatten the results into a single stream. Since the stream
-- generation function is monadic, unlike 'concatMap', it can produce an
-- effect at the beginning of each iteration of the inner loop.
--
-- @since 0.6.0
{-# INLINE concatMapM #-}
concatMapM :: (IsStream t, Monad m) => (a -> m (t m b)) -> t m a -> t m b
concatMapM f m = fromStreamD $ D.concatMapM (fmap toStreamD . f) (toStreamD m)

-- | Map a stream producing function on each element of the stream and then
-- flatten the results into a single stream.
--
-- >>> concatMap f = Stream.concatMapM (return . f)
-- >>> concatMap f = Stream.concatMapWith Stream.serial f
-- >>> concatMap f = Stream.concat . Stream.map f
--
-- @since 0.6.0
{-# INLINE concatMap #-}
concatMap ::(IsStream t, Monad m) => (a -> t m b) -> t m a -> t m b
concatMap f m = fromStreamD $ D.concatMap (toStreamD . f) (toStreamD m)

-- | Given a stream value in the underlying monad, lift and join the underlying
-- monad with the stream monad.
--
-- >>> concatM = Stream.concat . Stream.fromEffect
-- >>> concatM = Stream.concat . lift    -- requires (MonadTrans t)
-- >>> concatM = join . lift             -- requires (MonadTrans t, Monad (t m))
--
-- See also: 'concat', 'sequence'
--
--  /Internal/
--
{-# INLINE concatM #-}
concatM :: (IsStream t, Monad m) => m (t m a) -> t m a
concatM generator = concatMapM (\() -> generator) (fromPure ())

-- | Like 'foldMany' but appends empty fold output if the fold and stream
-- termination aligns:
--
-- >>> f = Fold.take 2 Fold.sum
-- >>> Stream.toList $ Stream.foldManyPost f $ Stream.fromList []
-- [0]
-- >>> Stream.toList $ Stream.foldManyPost f $ Stream.fromList [1..9]
-- [3,7,11,15,9]
-- >>> Stream.toList $ Stream.foldManyPost f $ Stream.fromList [1..10]
-- [3,7,11,15,19,0]
--
-- /Pre-release/
--
{-# INLINE foldManyPost #-}
foldManyPost
    :: (IsStream t, Monad m)
    => Fold m a b
    -> t m a
    -> t m b
foldManyPost f m = fromStreamD $ D.foldManyPost f (toStreamD m)

------------------------------------------------------------------------------
-- Split stream and fold
------------------------------------------------------------------------------

-- $
-- > intercalate . splitOnSeq == id
-- prop> (\f -> unsafePerformIO . Stream.toList . f . Stream.fromList) (Stream.intercalate Unfold.fromList " " .  Stream.splitOnSeq (Array.fromList " ") Fold.toList) xs == xs

-- | Like 'splitOn' but the separator is a sequence of elements instead of a
-- single element.
--
-- For illustration, let's define a function that operates on pure lists:
--
-- >>> splitOnSeq' pat xs = Stream.toList $ Stream.splitOnSeq (Array.fromList pat) Fold.toList (Stream.fromList xs)
--
-- >>> splitOnSeq' "" "hello"
-- ["h","e","l","l","o"]
--
-- >>> splitOnSeq' "hello" ""
-- [""]
--
-- >>> splitOnSeq' "hello" "hello"
-- ["",""]
--
-- >>> splitOnSeq' "x" "hello"
-- ["hello"]
--
-- >>> splitOnSeq' "h" "hello"
-- ["","ello"]
--
-- >>> splitOnSeq' "o" "hello"
-- ["hell",""]
--
-- >>> splitOnSeq' "e" "hello"
-- ["h","llo"]
--
-- >>> splitOnSeq' "l" "hello"
-- ["he","","o"]
--
-- >>> splitOnSeq' "ll" "hello"
-- ["he","o"]
--
-- 'splitOnSeq' is an inverse of 'intercalate'. The following law always holds:
--
-- > intercalate . splitOnSeq == id
--
-- The following law holds when the separator is non-empty and contains none of
-- the elements present in the input lists:
--
-- > splitOnSeq . intercalate == id
--
-- >>> splitOnSeq pat f = Stream.foldManyPost (Fold.takeEndBySeq_ pat f)
--
-- /Pre-release/

-- XXX We can use a polymorphic vector implemented by Array# to represent the
-- sequence, that way we can avoid the Storable constraint. If we still need
-- Storable Array for performance, we can use a separate splitOnArray API for
-- that. We can also have an API where the sequence itself is a lazy stream, so
-- that we can search files in files for example.
{-# INLINE splitOnSeq #-}
splitOnSeq
    :: (IsStream t, MonadIO m, Storable a, Unbox a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOnSeq patt f m =
    IsStream.fromStreamD $ D.splitOnSeq patt f (IsStream.toStreamD m)
-- XXX This may have a problem if the stream terminates and we start extracting
-- the fold and then the fold terminates before consuming all the buffered
-- input.  We have no way to supply the buffered input back to the driver.
-- splitOnSeq patt f =
    -- foldManyPost (Fold.takeEndBySeq_ patt f)

------------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------------

-- | Like 'zipWith' but using a monadic zipping function.
--
-- @since 0.4.0
{-# INLINE zipWithM #-}
zipWithM :: (IsStream t, Monad m) => (a -> b -> m c) -> t m a -> t m b -> t m c
zipWithM f m1 m2 =
    IsStream.fromStreamD
        $ D.zipWithM f (IsStream.toStreamD m1) (IsStream.toStreamD m2)

-- | Stream @a@ is evaluated first, followed by stream @b@, the resulting
-- elements @a@ and @b@ are then zipped using the supplied zip function and the
-- result @c@ is yielded to the consumer.
--
-- If stream @a@ or stream @b@ ends, the zipped stream ends. If stream @b@ ends
-- first, the element @a@ from previous evaluation of stream @a@ is discarded.
--
-- @
-- > D.toList $ D.zipWith (+) (D.fromList [1,2,3]) (D.fromList [4,5,6])
-- [5,7,9]
-- @
--
-- @since 0.1.0
{-# INLINE zipWith #-}
zipWith :: (IsStream t, Monad m) => (a -> b -> c) -> t m a -> t m b -> t m c
zipWith f m1 m2 =
    IsStream.fromStreamD
        $ D.zipWith f (IsStream.toStreamD m1) (IsStream.toStreamD m2)
