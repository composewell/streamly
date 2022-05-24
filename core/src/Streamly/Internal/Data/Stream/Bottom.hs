-- |
-- Module      : Streamly.Internal.Data.Stream.Bottom
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Bottom level Stream module that can be used by all other upper level
-- Stream modules.

module Streamly.Internal.Data.Stream.Bottom
    (
    -- * Generation
      fromPure
    , fromEffect

    -- * Elimination
    , foldContinue
    , foldBreak

    -- * Transformation
    , map
    , scanlMAfter'
    , postscanlMAfter'
    , postscanlM'
    , smapM
    -- $smapM_Notes
    , take
    , takeWhile
    , takeEndBy
    , drop
    , findIndices
    , intersperseM
    , reverse
    , reverse'

    -- * Nesting
    , concatM
    , concatMapM
    , concatMap

    -- * Zipping
    , zipWithM
    , zipWith
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Storable (Storable)
import Streamly.Internal.Data.Fold.Type (Fold (..))
import Streamly.Internal.System.IO (defaultChunkSize)

import qualified Streamly.Internal.Data.Array.Foreign.Type as A
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamD as D

import Prelude hiding (take, takeWhile, drop, reverse, concatMap, map, zipWith)

import Streamly.Internal.Data.Stream.Type

--
-- $setup
-- >>> :m
-- >>> import Control.Concurrent (threadDelay)
-- >>> import Control.Monad (join)
-- >>> import Control.Monad.Trans.Class (lift)
-- >>> import Data.Function (fix, (&))
-- >>> import Data.Semigroup (cycle1)
-- >>> import Prelude hiding (take, takeWhile, drop, reverse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> import Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Array.Foreign as Array
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Unfold as Unfold

------------------------------------------------------------------------------
-- Elimination - Running a Fold
------------------------------------------------------------------------------

-- | We can create higher order folds using 'foldContinue'. We can fold a
-- number of streams to a given fold efficiently with full stream fusion. For
-- example, to fold a list of streams on the same sum fold:
--
-- >>> concatFold = Prelude.foldl Stream.foldContinue Fold.sum
--
-- >>> fold f = Fold.finish . Stream.foldContinue f
--
-- /Internal/
{-# INLINE foldContinue #-}
foldContinue :: Monad m => Fold m a b -> Stream m a -> Fold m a b
foldContinue f s = D.foldContinue f $ toStreamD s

-- | Fold a stream using the supplied left 'Fold' and reducing the resulting
-- expression strictly at each step. The behavior is similar to 'foldl''. A
-- 'Fold' can terminate early without consuming the full stream. See the
-- documentation of individual 'Fold's for termination behavior.
--
-- | Like 'fold' but also returns the remaining stream.
--
-- /Inhibits stream fusion/
--
{-# INLINE foldBreak #-}
foldBreak :: Monad m => Fold m a b -> Stream m a -> m (b, Stream m a)
{-
-- XXX This shows quadratic performance when used recursively perhaps because
-- of StreamK to StreamD conversions not getting eliminated sue to recursion.
foldBreak fl (Stream strm) = fmap f $ D.foldBreak fl $ D.fromStreamK strm

    where

    f (b, str) = (b, Stream (D.toStreamK str))
-}
foldBreak fl strm = fmap f $ K.foldBreak fl (toStreamK strm)

    where

    f (b, str) = (b, fromStreamK str)

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
-- > K.toList $ K.map (+1) $ K.fromList [1,2,3]
-- [2,3,4]
-- @
--
-- @since 0.4.0
{-# INLINE map #-}
map :: (Monad m) => (a -> b) -> Stream m a -> Stream m b
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
scanlMAfter' :: (Monad m)
    => (b -> a -> m b) -> m b -> (b -> m b) -> Stream m a -> Stream m b
scanlMAfter' step initial done stream =
    fromStreamD $ D.scanlMAfter' step initial done $ toStreamD stream

{-# INLINE postscanlMAfter' #-}
postscanlMAfter' :: (Monad m)
    => (b -> a -> m b) -> m b -> (b -> m b) -> Stream m a -> Stream m b
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
postscanlM' :: (Monad m) => (b -> a -> m b) -> m b -> Stream m a -> Stream m b
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
-- smapM :: (s -> a -> m (Step s b)) -> m s -> Stream m a -> Stream m b
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
smapM :: (Monad m) =>
       (s -> a -> m (s, b))
    -> m s
    -> Stream m a
    -> Stream m b
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
take :: (Monad m) => Int -> Stream m a -> Stream m a
take n m = fromStreamD $ D.take n $ toStreamD m

-- | End the stream as soon as the predicate fails on an element.
--
-- @since 0.1.0
{-# INLINE takeWhile #-}
takeWhile :: (Monad m) => (a -> Bool) -> Stream m a -> Stream m a
takeWhile p m = fromStreamD $ D.takeWhile p $ toStreamD m

{-# INLINE takeEndBy #-}
takeEndBy :: (Monad m) => (a -> Bool) -> Stream m a -> Stream m a
takeEndBy p m = fromStreamD $ D.takeEndBy p $ toStreamD m

-- | Discard first 'n' elements from the stream and take the rest.
--
-- @since 0.1.0
{-# INLINE drop #-}
drop :: (Monad m) => Int -> Stream m a -> Stream m a
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
findIndices :: (Monad m) => (a -> Bool) -> Stream m a -> Stream m Int
findIndices p m = fromStreamD $ D.findIndices p (toStreamD m)

------------------------------------------------------------------------------
-- Transformation by Inserting
------------------------------------------------------------------------------

-- intersperseM = intersperseBySpan 1

-- | Insert an effect and its output before consuming an element of a stream
-- except the first one.
--
-- >>> Stream.fold Fold.toList $ Stream.trace putChar $ Stream.intersperseM (putChar '.' >> return ',') $ Stream.unfold Unfold.fromList "hello"
-- h.,e.,l.,l.,o"h,e,l,l,o"
--
-- Be careful about the order of effects. In the above example we used trace
-- after the intersperse, if we use it before the intersperse the output would
-- be he.l.l.o."h,e,l,l,o".
--
-- >>> Stream.fold Fold.toList $ Stream.intersperseM (putChar '.' >> return ',') $ Stream.trace putChar $ Stream.unfold Unfold.fromList "hello"
-- he.l.l.o."h,e,l,l,o"
--
-- @since 0.5.0
{-# INLINE intersperseM #-}
intersperseM :: (Monad m) => m a -> Stream m a -> Stream m a
intersperseM m = fromStreamD . D.intersperseM m . toStreamD

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
reverse :: (Monad m) => Stream m a -> Stream m a
reverse s = fromStreamD $ D.reverse $ toStreamD s

-- | Like 'reverse' but several times faster, requires a 'Storable' instance.
--
-- /Pre-release/
{-# INLINE reverse' #-}
reverse' :: (MonadIO m, Storable a) => Stream m a -> Stream m a
-- reverse' s = fromStreamD $ D.reverse' $ toStreamD s
reverse' =
        fromStreamD
        . A.flattenArraysRev -- unfoldMany A.readRev
        . D.fromStreamK
        . K.reverse
        . D.toStreamK
        . A.arraysOf defaultChunkSize
        . toStreamD

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
concatMapM :: (Monad m) => (a -> m (Stream m b)) -> Stream m a -> Stream m b
concatMapM f m = fromStreamD $ D.concatMapM (fmap toStreamD . f) (toStreamD m)

-- | Map a stream producing function on each element of the stream and then
-- flatten the results into a single stream.
--
-- >>> concatMap f = Stream.concatMapM (return . f)
-- >>> concatMap f = Stream.concatMapWith Stream.append f
-- >>> concatMap f = Stream.concat . Stream.map f
-- >>> concatMap f = Stream.unfoldMany (Unfold.lmap f Unfold.fromStream)
--
-- @since 0.6.0
{-# INLINE concatMap #-}
concatMap ::(Monad m) => (a -> Stream m b) -> Stream m a -> Stream m b
concatMap f m = fromStreamD $ D.concatMap (toStreamD . f) (toStreamD m)

-- | Given a stream value in the underlying monad, lift and join the underlying
-- monad with the stream monad.
--
-- >>> concatM = Stream.concat . Stream.fromEffect
-- >>> concatM = Stream.concat . lift    -- requires (MonadTrans t)
-- >>> concatM = join . lift             -- requires (MonadTrans t, Monad (Stream m))
--
-- See also: 'concat', 'sequence'
--
--  /Internal/
--
{-# INLINE concatM #-}
concatM :: (Monad m) => m (Stream m a) -> Stream m a
concatM generator = concatMapM (\() -> generator) (fromPure ())

------------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------------

-- | Like 'zipWith' but using a monadic zipping function.
--
-- @since 0.4.0
{-# INLINE zipWithM #-}
zipWithM :: (Monad m) => (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
zipWithM f m1 m2 =
    fromStreamD
        $ D.zipWithM f (toStreamD m1) (toStreamD m2)

-- | Stream @a@ is evaluated first, followed by stream @b@, the resulting
-- elements @a@ and @b@ are then zipped using the supplied zip function and the
-- result @c@ is yielded to the consumer.
--
-- If stream @a@ or stream @b@ ends, the zipped stream ends. If stream @b@ ends
-- first, the element @a@ from previous evaluation of stream @a@ is discarded.
--
-- @
-- > K.toList $ K.zipWith (+) (K.fromList [1,2,3]) (K.fromList [4,5,6])
-- [5,7,9]
-- @
--
-- @since 0.1.0
{-# INLINE zipWith #-}
zipWith :: (Monad m) => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWith f m1 m2 =
    fromStreamD
        $ D.zipWith f (toStreamD m1) (toStreamD m2)
