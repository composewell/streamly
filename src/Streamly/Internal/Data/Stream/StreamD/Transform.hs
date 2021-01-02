#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Transform
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.StreamD.Transform
    (
    -- * Transformation
      transform

    -- ** By folding
    , foldrS
    , foldrT
    , foldlS
    , foldlT

    , scanlM'
    , scanlMAfter'
    , scanl'
    , scanlM
    , scanl
    , scanl1M'
    , scanl1'
    , scanl1M
    , scanl1

    , prescanl'
    , prescanlM'

    , postscanl
    , postscanlM
    , postscanl'
    , postscanlM'
    , postscanlMAfter'

    , postscanlx'
    , postscanlMx'
    , scanlMx'
    , scanlx'
    , postscanOnce
    , scanOnce

    -- * Reordering
    , reverse
    , reverse'

    -- * Filtering
    , filter
    , filterM
    , uniq
    , take
    , takeByTime
    , takeWhile
    , takeWhileM
    , drop
    , dropByTime
    , dropWhile
    , dropWhileM

    -- * Mapping
    , map
    , mapM
    , sequence
    , rollingMap
    , rollingMapM

    -- ** Special Maps
    , tap
    , tapOffsetEvery
    , tapAsync
    , tapRate
    , pollCounts

    -- * Inserting
    , intersperseM
    , intersperseM_
    , intersperse
    , intersperseSuffix
    , intersperseSuffix_
    , intersperseSuffixBySpan
    , insertBy

    -- * Deleting
    , deleteBy

    -- ** Map and Filter
    , mapMaybe
    , mapMaybeM

    -- * Parsers
    , parseMany
    , parseIterate

    -- * Concurrent Application
    , mkParallel
    , mkParallelD

    -- ** Conversions
    -- | Transform a stream into another type.
    , hoist
    , generally

    , liftInner
    , runReaderT
    , evalStateT
    , runStateT
    )
where

import Control.Concurrent (killThread, myThreadId, takeMVar, threadDelay)
import Control.Exception (assert, AsyncException)
import Control.Monad (void, when)
import Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Functor.Identity (Identity(..))
import Data.IORef (newIORef, mkWeakIORef)
import Data.Maybe (fromJust, isJust)
import Foreign.Storable (Storable(..))
import Fusion.Plugin.Types (Fuse(..))
import GHC.Types (SPEC(..))
import qualified Control.Monad.Catch as MC
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State.Strict as State

import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Pipe.Types (Pipe(..), PipeState(..))
import Streamly.Internal.Data.Stream.SVar (fromConsumer, pushToFold)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units
       (TimeUnit64, toRelTime64, diffAbsTime64)

import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as A
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.IORef.Prim as Prim
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified Streamly.Internal.Data.Pipe.Types as Pipe
import qualified Streamly.Internal.Data.Stream.StreamK as K

import qualified Prelude
import Prelude hiding
       ( drop, dropWhile, filter, map, mapM, reverse
       , scanl, scanl1, sequence, take, takeWhile, zipWith)

import Streamly.Internal.Data.Stream.StreamD.Type
import Streamly.Internal.Data.SVar

-------------------------------------------------------------------------------
-- General transformation
-------------------------------------------------------------------------------

{-# INLINE_NORMAL transform #-}
transform :: Monad m => Pipe m a b -> Stream m a -> Stream m b
transform (Pipe pstep1 pstep2 pstate) (Stream step state) =
    Stream step' (Consume pstate, state)

  where

    {-# INLINE_LATE step' #-}

    step' gst (Consume pst, st) = pst `seq` do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                res <- pstep1 pst x
                case res of
                    Pipe.Yield b pst' -> return $ Yield b (pst', s)
                    Pipe.Continue pst' -> return $ Skip (pst', s)
            Skip s -> return $ Skip (Consume pst, s)
            Stop   -> return Stop

    step' _ (Produce pst, st) = pst `seq` do
        res <- pstep2 pst
        case res of
            Pipe.Yield b pst' -> return $ Yield b (pst', st)
            Pipe.Continue pst' -> return $ Skip (pst', st)

------------------------------------------------------------------------------
-- Transformation by Folding
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Left Folds
------------------------------------------------------------------------------

{-# INLINE_NORMAL foldlT #-}
foldlT :: (Monad m, Monad (s m), MonadTrans s)
    => (s m b -> a -> s m b) -> s m b -> Stream m a -> s m b
foldlT fstep begin (Stream step state) = go SPEC begin state
  where
    go !_ acc st = do
        r <- lift $ step defState st
        case r of
            Yield x s -> go SPEC (fstep acc x) s
            Skip s -> go SPEC acc s
            Stop   -> acc

-- Note, this is going to have horrible performance, because of the nature of
-- the stream type (i.e. direct stream vs CPS). Its only for reference, it is
-- likely be practically unusable.
{-# INLINE_NORMAL foldlS #-}
foldlS :: Monad m
    => (Stream m b -> a -> Stream m b) -> Stream m b -> Stream m a -> Stream m b
foldlS fstep begin (Stream step state) = Stream step' (Left (state, begin))
  where
    step' gst (Left (st, acc)) = do
        r <- step (adaptState gst) st
        return $ case r of
            Yield x s -> Skip (Left (s, fstep acc x))
            Skip s -> Skip (Left (s, acc))
            Stop   -> Skip (Right acc)

    step' gst (Right (Stream stp stt)) = do
        r <- stp (adaptState gst) stt
        return $ case r of
            Yield x s -> Yield x (Right (Stream stp s))
            Skip s -> Skip (Right (Stream stp s))
            Stop   -> Stop

------------------------------------------------------------------------------
-- Prescans
------------------------------------------------------------------------------

-- XXX Is a prescan useful, discarding the last step does not sound useful?  I
-- am not sure about the utility of this function, so this is implemented but
-- not exposed. We can expose it if someone provides good reasons why this is
-- useful.
--
-- XXX We have to execute the stream one step ahead to know that we are at the
-- last step.  The vector implementation of prescan executes the last fold step
-- but does not yield the result. This means we have executed the effect but
-- discarded value. This does not sound right. In this implementation we are
-- not executing the last fold step.
{-# INLINE_NORMAL prescanlM' #-}
prescanlM' :: Monad m => (b -> a -> m b) -> m b -> Stream m a -> Stream m b
prescanlM' f mz (Stream step state) = Stream step' (state, mz)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, prev) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                acc <- prev
                return $ Yield acc (s, f acc x)
            Skip s -> return $ Skip (s, prev)
            Stop   -> return Stop

{-# INLINE prescanl' #-}
prescanl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> Stream m b
prescanl' f z = prescanlM' (\a b -> return (f a b)) (return z)

------------------------------------------------------------------------------
-- Monolithic postscans (postscan followed by a map)
------------------------------------------------------------------------------

-- The performance of a modular postscan followed by a map seems to be
-- equivalent to this monolithic scan followed by map therefore we may not need
-- this implementation. We just have it for performance comparison and in case
-- modular version does not perform well in some situation.
--
{-# INLINE_NORMAL postscanlMx' #-}
postscanlMx' :: Monad m
    => (x -> a -> m x) -> m x -> (x -> m b) -> Stream m a -> Stream m b
postscanlMx' fstep begin done (Stream step state) = do
    Stream step' (state, begin)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, acc) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                old <- acc
                y <- fstep old x
                v <- done y
                v `seq` y `seq` return (Yield v (s, return y))
            Skip s -> return $ Skip (s, acc)
            Stop   -> return Stop

{-# INLINE_NORMAL postscanlx' #-}
postscanlx' :: Monad m
    => (x -> a -> x) -> x -> (x -> b) -> Stream m a -> Stream m b
postscanlx' fstep begin done s =
    postscanlMx' (\b a -> return (fstep b a)) (return begin) (return . done) s

-- XXX do we need consM strict to evaluate the begin value?
{-# INLINE scanlMx' #-}
scanlMx' :: Monad m
    => (x -> a -> m x) -> m x -> (x -> m b) -> Stream m a -> Stream m b
scanlMx' fstep begin done s =
    (begin >>= \x -> x `seq` done x) `consM` postscanlMx' fstep begin done s

data PostScanState s f = PostScan s !f

{-# INLINE_NORMAL postscanOnce #-}
postscanOnce :: Monad m => FL.Fold m a b -> Stream m a -> Stream m b
postscanOnce (FL.Fold fstep begin done) (Stream step state) =
    Stream step' (PostScan state begin)

    where

    {-# INLINE_LATE step' #-}
    step' gst (PostScan st acc) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                old <- acc
                res <- fstep old x
                case res of
                    FL.Partial fs -> do
                        !v <- done fs
                        return $ Yield v $ PostScan s (return fs)
                    FL.Done _ -> return Stop
            Skip s -> return $ Skip $ PostScan s acc
            Stop -> return Stop

{-# INLINE scanOnce #-}
scanOnce :: Monad m
    => FL.Fold m a b -> Stream m a -> Stream m b
scanOnce fld@(FL.Fold _ begin done) s =
    (begin >>= \x -> x `seq` done x) `consM` postscanOnce fld s

{-# INLINE scanlx' #-}
scanlx' :: Monad m
    => (x -> a -> x) -> x -> (x -> b) -> Stream m a -> Stream m b
scanlx' fstep begin done s =
    scanlMx' (\b a -> return (fstep b a)) (return begin) (return . done) s

------------------------------------------------------------------------------
-- postscans
------------------------------------------------------------------------------

{-# INLINE_NORMAL postscanlM' #-}
postscanlM' :: Monad m => (b -> a -> m b) -> m b -> Stream m a -> Stream m b
postscanlM' fstep begin (Stream step state) =
    Stream step' Nothing
  where
    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        !x <- begin
        return $ Skip (Just (state, x))

    step' gst (Just (st, acc)) =  do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                !y <- fstep acc x
                return $ Yield y (Just (s, y))
            Skip s -> return $ Skip (Just (s, acc))
            Stop   -> return Stop

{-# INLINE_NORMAL postscanl' #-}
postscanl' :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
postscanl' f seed = postscanlM' (\a b -> return (f a b)) (return seed)

-- We can possibly have the "done" function as a Maybe to provide an option to
-- emit or not emit the accumulator when the stream stops.
--
-- TBD: use a single Yield point
--
{-# INLINE_NORMAL postscanlMAfter' #-}
postscanlMAfter' :: Monad m
    => (b -> a -> m b) -> m b -> (b -> m b) -> Stream m a -> Stream m b
postscanlMAfter' fstep initial done (Stream step1 state1) = do
    Stream step (Just (state1, initial))

    where

    {-# INLINE_LATE step #-}
    step gst (Just (st, acc)) = do
        r <- step1 (adaptState gst) st
        case r of
            Yield x s -> do
                old <- acc
                y <- fstep old x
                y `seq` return (Yield y (Just (s, return y)))
            Skip s -> return $ Skip $ Just (s, acc)
            Stop -> acc >>= done >>= \res -> return (Yield res Nothing)
    step _ Nothing = return Stop

{-# INLINE_NORMAL postscanlM #-}
postscanlM :: Monad m => (b -> a -> m b) -> m b -> Stream m a -> Stream m b
postscanlM fstep begin (Stream step state) = Stream step' Nothing
  where
    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        r <- begin
        return $ Skip (Just (state, r))

    step' gst (Just (st, acc)) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                y <- fstep acc x
                return (Yield y (Just (s, y)))
            Skip s -> return $ Skip (Just (s, acc))
            Stop   -> return Stop

{-# INLINE_NORMAL postscanl #-}
postscanl :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
postscanl f seed = postscanlM (\a b -> return (f a b)) (return seed)

{-# INLINE_NORMAL scanlM' #-}
scanlM' :: Monad m => (b -> a -> m b) -> m b -> Stream m a -> Stream m b
scanlM' fstep begin (Stream step state) = Stream step' Nothing
  where
    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        !x <- begin
        return $ Yield x (Just (state, x))
    step' gst (Just (st, acc)) =  do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                !y <- fstep acc x
                return $ Yield y (Just (s, y))
            Skip s -> return $ Skip (Just (s, acc))
            Stop   -> return Stop

{-# INLINE scanlMAfter' #-}
scanlMAfter' :: Monad m
    => (b -> a -> m b) -> m b -> (b -> m b) -> Stream m a -> Stream m b
scanlMAfter' fstep initial done s =
    (initial >>= \x -> x `seq` return x) `consM`
        postscanlMAfter' fstep initial done s

{-# INLINE scanl' #-}
scanl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> Stream m b
scanl' f seed = scanlM' (\a b -> return (f a b)) (return seed)

{-# INLINE_NORMAL scanlM #-}
scanlM :: Monad m => (b -> a -> m b) -> m b -> Stream m a -> Stream m b
scanlM fstep begin (Stream step state) = Stream step' Nothing
  where
    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        x <- begin
        return $ Yield x (Just (state, x))
    step' gst (Just (st, acc)) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                y <- fstep acc x
                return $ Yield y (Just (s, y))
            Skip s -> return $ Skip (Just (s, acc))
            Stop   -> return $ Stop

{-# INLINE scanl #-}
scanl :: Monad m => (b -> a -> b) -> b -> Stream m a -> Stream m b
scanl f seed = scanlM (\a b -> return (f a b)) (return seed)

{-# INLINE_NORMAL scanl1M #-}
scanl1M :: Monad m => (a -> a -> m a) -> Stream m a -> Stream m a
scanl1M fstep (Stream step state) = Stream step' (state, Nothing)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, Nothing) = do
        r <- step gst st
        case r of
            Yield x s -> return $ Yield x (s, Just x)
            Skip s -> return $ Skip (s, Nothing)
            Stop   -> return Stop

    step' gst (st, Just acc) = do
        r <- step gst st
        case r of
            Yield y s -> do
                z <- fstep acc y
                return $ Yield z (s, Just z)
            Skip s -> return $ Skip (s, Just acc)
            Stop   -> return Stop

{-# INLINE scanl1 #-}
scanl1 :: Monad m => (a -> a -> a) -> Stream m a -> Stream m a
scanl1 f = scanl1M (\x y -> return (f x y))

{-# INLINE_NORMAL scanl1M' #-}
scanl1M' :: Monad m => (a -> a -> m a) -> Stream m a -> Stream m a
scanl1M' fstep (Stream step state) = Stream step' (state, Nothing)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, Nothing) = do
        r <- step gst st
        case r of
            Yield x s -> x `seq` return $ Yield x (s, Just x)
            Skip s -> return $ Skip (s, Nothing)
            Stop   -> return Stop

    step' gst (st, Just acc) = acc `seq` do
        r <- step gst st
        case r of
            Yield y s -> do
                z <- fstep acc y
                z `seq` return $ Yield z (s, Just z)
            Skip s -> return $ Skip (s, Just acc)
            Stop   -> return Stop

{-# INLINE scanl1' #-}
scanl1' :: Monad m => (a -> a -> a) -> Stream m a -> Stream m a
scanl1' f = scanl1M' (\x y -> return (f x y))

------------------------------------------------------------------------------
-- Stateful map/scan
------------------------------------------------------------------------------

data RollingMapState s a = RollingMapInit s | RollingMapGo s a

{-# INLINE rollingMapM #-}
rollingMapM :: Monad m => (a -> a -> m b) -> Stream m a -> Stream m b
rollingMapM f (Stream step1 state1) = Stream step (RollingMapInit state1)
    where
    step gst (RollingMapInit st) = do
        r <- step1 (adaptState gst) st
        return $ case r of
            Yield x s -> Skip $ RollingMapGo s x
            Skip s -> Skip $ RollingMapInit s
            Stop   -> Stop

    step gst (RollingMapGo s1 x1) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield x s -> do
                !res <- f x x1
                return $ Yield res $ RollingMapGo s x
            Skip s -> return $ Skip $ RollingMapGo s x1
            Stop   -> return $ Stop

{-# INLINE rollingMap #-}
rollingMap :: Monad m => (a -> a -> b) -> Stream m a -> Stream m b
rollingMap f = rollingMapM (\x y -> return $ f x y)

------------------------------------------------------------------------------
-- Reordering
------------------------------------------------------------------------------

-- We can implement reverse as:
--
-- > reverse = foldlS (flip cons) nil
--
-- However, this implementation is unusable because of the horrible performance
-- of cons. So we just convert it to a list first and then stream from the
-- list.
--
-- XXX Maybe we can use an Array instead of a list here?
{-# INLINE_NORMAL reverse #-}
reverse :: Monad m => Stream m a -> Stream m a
reverse m = Stream step Nothing
    where
    {-# INLINE_LATE step #-}
    step _ Nothing = do
        xs <- foldl' (flip (:)) [] m
        return $ Skip (Just xs)
    step _ (Just (x:xs)) = return $ Yield x (Just xs)
    step _ (Just []) = return Stop

-- Much faster reverse for Storables
{-# INLINE_NORMAL reverse' #-}
reverse' :: forall m a. (MonadIO m, Storable a) => Stream m a -> Stream m a
{-
-- This commented implementation copies the whole stream into one single array
-- and then streams from that array, this is 3-4x faster than the chunked code
-- that follows.  Though this could be problematic due to unbounded large
-- allocations. We need to figure out why the chunked code is slower and if we
-- can optimize the chunked code to work as fast as this one. It may be a
-- fusion issue?
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (Ptr, plusPtr)
reverse' m = Stream step Nothing
    where
    {-# INLINE_LATE step #-}
    step _ Nothing = do
        arr <- A.fromStreamD m
        let p = aEnd arr `plusPtr` negate (sizeOf (undefined :: a))
        return $ Skip $ Just (aStart arr, p)

    step _ (Just (start, p)) | p < unsafeForeignPtrToPtr start = return Stop

    step _ (Just (start, p)) = do
        let !x = A.unsafeInlineIO $ do
                    r <- peek p
                    touchForeignPtr start
                    return r
            next = p `plusPtr` negate (sizeOf (undefined :: a))
        return $ Yield x (Just (start, next))
-}
reverse' m =
          A.flattenArraysRev
        $ fromStreamK
        $ K.reverse
        $ toStreamK
        $ A.fromStreamDArraysOf A.defaultChunkSize m

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE_NORMAL drop #-}
drop :: Monad m => Int -> Stream m a -> Stream m a
drop n (Stream step state) = Stream step' (state, Just n)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, Just i)
      | i > 0 = do
          r <- step gst st
          return $
            case r of
              Yield _ s -> Skip (s, Just (i - 1))
              Skip s    -> Skip (s, Just i)
              Stop      -> Stop
      | otherwise = return $ Skip (st, Nothing)

    step' gst (st, Nothing) = do
      r <- step gst st
      return $
        case r of
          Yield x s -> Yield x (s, Nothing)
          Skip  s   -> Skip (s, Nothing)
          Stop      -> Stop

data DropWhileState s a
    = DropWhileDrop s
    | DropWhileYield a s
    | DropWhileNext s

{-# INLINE_NORMAL dropWhileM #-}
dropWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
dropWhileM f (Stream step state) = Stream step' (DropWhileDrop state)
  where
    {-# INLINE_LATE step' #-}
    step' gst (DropWhileDrop st) = do
        r <- step gst st
        case r of
            Yield x s -> do
                b <- f x
                if b
                then return $ Skip (DropWhileDrop s)
                else return $ Skip (DropWhileYield x s)
            Skip s -> return $ Skip (DropWhileDrop s)
            Stop -> return Stop

    step' gst (DropWhileNext st) =  do
        r <- step gst st
        case r of
            Yield x s -> return $ Skip (DropWhileYield x s)
            Skip s    -> return $ Skip (DropWhileNext s)
            Stop      -> return Stop

    step' _ (DropWhileYield x st) = return $ Yield x (DropWhileNext st)

{-# INLINE dropWhile #-}
dropWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
dropWhile f = dropWhileM (return . f)

{-# INLINE_NORMAL filterM #-}
filterM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
filterM f (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- step gst st
        case r of
            Yield x s -> do
                b <- f x
                return $ if b
                         then Yield x s
                         else Skip s
            Skip s -> return $ Skip s
            Stop   -> return Stop

{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
filter f = filterM (return . f)

{-# INLINE_NORMAL uniq #-}
uniq :: (Eq a, Monad m) => Stream m a -> Stream m a
uniq (Stream step state) = Stream step' (Nothing, state)
  where
    {-# INLINE_LATE step' #-}
    step' gst (Nothing, st) = do
        r <- step gst st
        case r of
            Yield x s -> return $ Yield x (Just x, s)
            Skip  s   -> return $ Skip  (Nothing, s)
            Stop      -> return Stop
    step' gst (Just x, st)  = do
         r <- step gst st
         case r of
             Yield y s | x == y   -> return $ Skip (Just x, s)
                       | otherwise -> return $ Yield y (Just y, s)
             Skip  s   -> return $ Skip (Just x, s)
             Stop      -> return Stop

------------------------------------------------------------------------------
-- Transformation by Mapping
------------------------------------------------------------------------------

{-# INLINE_NORMAL sequence #-}
sequence :: Monad m => Stream m (m a) -> Stream m a
sequence (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' gst st = do
         r <- step (adaptState gst) st
         case r of
             Yield x s -> x >>= \a -> return (Yield a s)
             Skip s    -> return $ Skip s
             Stop      -> return Stop

------------------------------------------------------------------------------
-- Tapping/Distributing
------------------------------------------------------------------------------

data TapState fs st a
    = TapInit | Tapping !fs st | TapDone st

-- XXX Multiple yield points
{-# INLINE tap #-}
tap :: Monad m => Fold m a b -> Stream m a -> Stream m a
tap (Fold fstep initial extract) (Stream step state) = Stream step' TapInit

    where

    step' _ TapInit = do
        r <- initial
        return $ Skip (Tapping r state)
    step' gst (Tapping acc st) = do
        r <- step gst st
        case r of
            -- XXX Abstract Yield?
            Yield x s -> do
                res <- fstep acc x
                return
                    $ Yield x
                    $ case res of
                          FL.Partial fs -> Tapping fs s
                          FL.Done _ -> TapDone s
            Skip s -> return $ Skip (Tapping acc s)
            Stop -> do
                void $ extract acc
                return Stop
    step' gst (TapDone st) = do
        r <- step gst st
        return
            $ case r of
                  Yield x s -> Yield x (TapDone s)
                  Skip s -> Skip (TapDone s)
                  Stop -> Stop

data TapOffState fs s a
    = TapOffInit
    | TapOffTapping !fs s Int
    | TapOffDone s

-- XXX Multiple yield points
{-# INLINE_NORMAL tapOffsetEvery #-}
tapOffsetEvery :: Monad m
    => Int -> Int -> Fold m a b -> Stream m a -> Stream m a
tapOffsetEvery offset n (Fold fstep initial extract) (Stream step state) =
    Stream step' TapOffInit

    where

    {-# INLINE_LATE step' #-}
    step' _ TapOffInit = do
        r <- initial
        return $ Skip $ TapOffTapping r state (offset `mod` n)
    step' gst (TapOffTapping acc st count) = do
        r <- step gst st
        case r of
            Yield x s -> do
                next <-
                    if count <= 0
                    then do
                        res <- fstep acc x
                        return
                            $ case res of
                                  FL.Partial sres ->
                                    TapOffTapping sres s (n - 1)
                                  FL.Done _ -> TapOffDone s
                    else return $ TapOffTapping acc s (count - 1)
                return $ Yield x next
            Skip s -> return $ Skip (TapOffTapping acc s count)
            Stop -> do
                void $ extract acc
                return Stop
    step' gst (TapOffDone st) = do
        r <- step gst st
        return
            $ case r of
                  Yield x s -> Yield x (TapOffDone s)
                  Skip s -> Skip (TapOffDone s)
                  Stop -> Stop

{-# INLINE_NORMAL pollCounts #-}
pollCounts
    :: MonadAsync m
    => (a -> Bool)
    -> (Stream m Int -> Stream m Int)
    -> Fold m Int b
    -> Stream m a
    -> Stream m a
pollCounts predicate transf fld (Stream step state) = Stream step' Nothing
  where

    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        -- As long as we are using an "Int" for counts lockfree reads from
        -- Var should work correctly on both 32-bit and 64-bit machines.
        -- However, an Int on a 32-bit machine may overflow quickly.
        countVar <- liftIO $ Prim.newIORef (0 :: Int)
        tid <- forkManaged
            $ void $ foldOnce fld
            $ transf $ fromPrimIORef countVar
        return $ Skip (Just (countVar, tid, state))

    step' gst (Just (countVar, tid, st)) = do
        r <- step gst st
        case r of
            Yield x s -> do
                when (predicate x) $ liftIO $ Prim.modifyIORef' countVar (+ 1)
                return $ Yield x (Just (countVar, tid, s))
            Skip s -> return $ Skip (Just (countVar, tid, s))
            Stop -> do
                liftIO $ killThread tid
                return Stop

{-# INLINE_NORMAL tapRate #-}
tapRate ::
       (MonadAsync m, MonadCatch m)
    => Double
    -> (Int -> m b)
    -> Stream m a
    -> Stream m a
tapRate samplingRate action (Stream step state) = Stream step' Nothing
  where
    {-# NOINLINE loop #-}
    loop countVar prev = do
        i <-
            MC.catch
                (do liftIO $ threadDelay (round $ samplingRate * 1000000)
                    i <- liftIO $ Prim.readIORef countVar
                    let !diff = i - prev
                    void $ action diff
                    return i)
                (\(e :: AsyncException) -> do
                     i <- liftIO $ Prim.readIORef countVar
                     let !diff = i - prev
                     void $ action diff
                     throwM (MC.toException e))
        loop countVar i

    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        countVar <- liftIO $ Prim.newIORef 0
        tid <- fork $ loop countVar 0
        ref <- liftIO $ newIORef ()
        _ <- liftIO $ mkWeakIORef ref (killThread tid)
        return $ Skip (Just (countVar, tid, state, ref))

    step' gst (Just (countVar, tid, st, ref)) = do
        r <- step gst st
        case r of
            Yield x s -> do
                liftIO $ Prim.modifyIORef' countVar (+ 1)
                return $ Yield x (Just (countVar, tid, s, ref))
            Skip s -> return $ Skip (Just (countVar, tid, s, ref))
            Stop -> do
                liftIO $ killThread tid
                return Stop

-------------------------------------------------------------------------------
-- Concurrent tap
-------------------------------------------------------------------------------

-- | Create an SVar with a fold consumer that will fold any elements sent to it
-- using the supplied fold function.
{-# INLINE newFoldSVar #-}
newFoldSVar :: MonadAsync m => State t m a -> Fold m a b -> m (SVar t m a)
newFoldSVar stt f = do
    -- Buffer size for the SVar is derived from the current state
    sv <- newParallelVar StopAny (adaptState stt)
    -- Add the producer thread-id to the SVar.
    liftIO myThreadId >>= modifyThread sv
    void $ doFork (work sv) (svarMrun sv) (handleFoldException sv)
    return sv

    where

    {-# NOINLINE work #-}
    work sv = void $ foldOnce f $ fromProducer sv

{-# INLINE_NORMAL tapAsync #-}
tapAsync :: MonadAsync m => Fold m a b -> Stream m a -> Stream m a
tapAsync f (Stream step1 state1) = Stream step TapInit
    where

    drainFold svr = do
            -- In general, a Stop event would come equipped with the result
            -- of the fold. It is not used here but it would be useful in
            -- applicative and distribute.
            done <- fromConsumer svr
            when (not done) $ do
                liftIO $ withDiagMVar svr "teeToSVar: waiting to drain"
                       $ takeMVar (outputDoorBellFromConsumer svr)
                drainFold svr

    stopFold svr = do
            liftIO $ sendStop svr Nothing
            -- drain/wait until a stop event arrives from the fold.
            drainFold svr

    {-# INLINE_LATE step #-}
    step gst TapInit = do
        sv <- newFoldSVar gst f
        return $ Skip (Tapping sv state1)

    step gst (Tapping sv st) = do
        r <- step1 gst st
        case r of
            Yield a s ->  do
                done <- pushToFold sv a
                if done
                then do
                    -- XXX we do not need to wait synchronously here
                    stopFold sv
                    return $ Yield a (TapDone s)
                else return $ Yield a (Tapping sv s)
            Skip s -> return $ Skip (Tapping sv s)
            Stop -> do
                stopFold sv
                return $ Stop

    step gst (TapDone st) = do
        r <- step1 gst st
        return $ case r of
            Yield a s -> Yield a (TapDone s)
            Skip s    -> Skip (TapDone s)
            Stop      -> Stop

------------------------------------------------------------------------------
-- Inserting
------------------------------------------------------------------------------

data LoopState x s = FirstYield s
                   | InterspersingYield s
                   | YieldAndCarry x s

{-# INLINE_NORMAL intersperseM #-}
intersperseM :: Monad m => m a -> Stream m a -> Stream m a
intersperseM m (Stream step state) = Stream step' (FirstYield state)
  where
    {-# INLINE_LATE step' #-}
    step' gst (FirstYield st) = do
        r <- step gst st
        return $
            case r of
                Yield x s -> Skip (YieldAndCarry x s)
                Skip s -> Skip (FirstYield s)
                Stop -> Stop

    step' gst (InterspersingYield st) = do
        r <- step gst st
        case r of
            Yield x s -> do
                a <- m
                return $ Yield a (YieldAndCarry x s)
            Skip s -> return $ Skip $ InterspersingYield s
            Stop -> return Stop

    step' _ (YieldAndCarry x st) = return $ Yield x (InterspersingYield st)

{-# INLINE_NORMAL intersperseM_ #-}
intersperseM_ :: Monad m => m b -> Stream m a -> Stream m a
intersperseM_ m (Stream step1 state1) = Stream step (Left (pure (), state1))
  where
    {-# INLINE_LATE step #-}
    step gst (Left (eff, st)) = do
        r <- step1 gst st
        case r of
            Yield x s -> eff >> return (Yield x (Right s))
            Skip s -> return $ Skip (Left (eff, s))
            Stop -> return Stop

    step _ (Right st) = return $ Skip $ Left (void m, st)

data SuffixState s a
    = SuffixElem s
    | SuffixSuffix s
    | SuffixYield a (SuffixState s a)

{-# INLINE_NORMAL intersperseSuffix #-}
intersperseSuffix :: forall m a. Monad m => m a -> Stream m a -> Stream m a
intersperseSuffix action (Stream step state) = Stream step' (SuffixElem state)
    where
    {-# INLINE_LATE step' #-}
    step' gst (SuffixElem st) = do
        r <- step gst st
        return $ case r of
            Yield x s -> Skip (SuffixYield x (SuffixSuffix s))
            Skip s -> Skip (SuffixElem s)
            Stop -> Stop

    step' _ (SuffixSuffix st) = do
        action >>= \r -> return $ Skip (SuffixYield r (SuffixElem st))

    step' _ (SuffixYield x next) = return $ Yield x next

{-# INLINE_NORMAL intersperseSuffix_ #-}
intersperseSuffix_ :: Monad m => m b -> Stream m a -> Stream m a
intersperseSuffix_ m (Stream step1 state1) = Stream step (Left state1)
  where
    {-# INLINE_LATE step #-}
    step gst (Left st) = do
        r <- step1 gst st
        case r of
            Yield x s -> return $ Yield x (Right s)
            Skip s -> return $ Skip $ Left s
            Stop -> return Stop

    step _ (Right st) = m >> return (Skip (Left st))

data SuffixSpanState s a
    = SuffixSpanElem s Int
    | SuffixSpanSuffix s
    | SuffixSpanYield a (SuffixSpanState s a)
    | SuffixSpanLast
    | SuffixSpanStop

-- | intersperse after every n items
{-# INLINE_NORMAL intersperseSuffixBySpan #-}
intersperseSuffixBySpan :: forall m a. Monad m
    => Int -> m a -> Stream m a -> Stream m a
intersperseSuffixBySpan n action (Stream step state) =
    Stream step' (SuffixSpanElem state n)
    where
    {-# INLINE_LATE step' #-}
    step' gst (SuffixSpanElem st i) | i > 0 = do
        r <- step gst st
        return $ case r of
            Yield x s -> Skip (SuffixSpanYield x (SuffixSpanElem s (i - 1)))
            Skip s -> Skip (SuffixSpanElem s i)
            Stop -> if i == n then Stop else Skip SuffixSpanLast
    step' _ (SuffixSpanElem st _) = return $ Skip (SuffixSpanSuffix st)

    step' _ (SuffixSpanSuffix st) = do
        action >>= \r -> return $ Skip (SuffixSpanYield r (SuffixSpanElem st n))

    step' _ (SuffixSpanLast) = do
        action >>= \r -> return $ Skip (SuffixSpanYield r SuffixSpanStop)

    step' _ (SuffixSpanYield x next) = return $ Yield x next

    step' _ (SuffixSpanStop) = return Stop

{-# INLINE intersperse #-}
intersperse :: Monad m => a -> Stream m a -> Stream m a
intersperse a = intersperseM (return a)

{-# INLINE_NORMAL insertBy #-}
insertBy :: Monad m => (a -> a -> Ordering) -> a -> Stream m a -> Stream m a
insertBy cmp a (Stream step state) = Stream step' (state, False, Nothing)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, False, _) = do
        r <- step gst st
        case r of
            Yield x s -> case cmp a x of
                GT -> return $ Yield x (s, False, Nothing)
                _  -> return $ Yield a (s, True, Just x)
            Skip s -> return $ Skip (s, False, Nothing)
            Stop   -> return $ Yield a (st, True, Nothing)

    step' _ (_, True, Nothing) = return Stop

    step' gst (st, True, Just prev) = do
        r <- step gst st
        case r of
            Yield x s -> return $ Yield prev (s, True, Just x)
            Skip s    -> return $ Skip (s, True, Just prev)
            Stop      -> return $ Yield prev (st, True, Nothing)

------------------------------------------------------------------------------
-- Deleting
------------------------------------------------------------------------------

{-# INLINE_NORMAL deleteBy #-}
deleteBy :: Monad m => (a -> a -> Bool) -> a -> Stream m a -> Stream m a
deleteBy eq x (Stream step state) = Stream step' (state, False)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, False) = do
        r <- step gst st
        case r of
            Yield y s -> return $
                if eq x y then Skip (s, True) else Yield y (s, False)
            Skip s -> return $ Skip (s, False)
            Stop   -> return Stop

    step' gst (st, True) = do
        r <- step gst st
        case r of
            Yield y s -> return $ Yield y (s, True)
            Skip s -> return $ Skip (s, True)
            Stop   -> return Stop

------------------------------------------------------------------------------
-- Transformation by Map and Filter
------------------------------------------------------------------------------

-- XXX Will this always fuse properly?
{-# INLINE_NORMAL mapMaybe #-}
mapMaybe :: Monad m => (a -> Maybe b) -> Stream m a -> Stream m b
mapMaybe f = fmap fromJust . filter isJust . map f

{-# INLINE_NORMAL mapMaybeM #-}
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Stream m a -> Stream m b
mapMaybeM f = fmap fromJust . filter isJust . mapM f

------------------------------------------------------------------------------
-- Repeated parsing
------------------------------------------------------------------------------

{-# ANN type ParseChunksState Fuse #-}
data ParseChunksState x inpBuf st pst =
      ParseChunksInit inpBuf st
    | ParseChunksInitLeftOver inpBuf
    | ParseChunksStream st inpBuf !pst
    | ParseChunksBuf inpBuf st inpBuf !pst
    | ParseChunksYield x (ParseChunksState x inpBuf st pst)

{-# INLINE_NORMAL parseMany #-}
parseMany
    :: MonadThrow m
    => PRD.Parser m a b
    -> Stream m a
    -> Stream m b
parseMany (PRD.Parser pstep initial extract) (Stream step state) =
    Stream stepOuter (ParseChunksInit [] state)

    where

    {-# INLINE_LATE stepOuter #-}
    -- Buffer is empty, get the first element from the stream, initialize the
    -- fold and then go to stream processing loop.
    stepOuter gst (ParseChunksInit [] st) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> initial >>= return . Skip . ParseChunksBuf [x] s []
            Skip s -> return $ Skip $ ParseChunksInit [] s
            Stop   -> return Stop

    -- Buffer is not empty, go to buffered processing loop
    stepOuter _ (ParseChunksInit src st) = do
        initial >>= return . Skip . ParseChunksBuf src st []

    -- XXX we just discard any leftover input at the end
    stepOuter _ (ParseChunksInitLeftOver _) = return Stop

    -- Buffer is empty, process elements from the stream
    stepOuter gst (ParseChunksStream st buf pst) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    PR.Partial 0 pst1 ->
                        return $ Skip $ ParseChunksStream s [] pst1
                    PR.Partial n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let src0 = Prelude.take n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip $ ParseChunksBuf src s [] pst1
                    PR.Continue 0 pst1 ->
                        return $ Skip $ ParseChunksStream s (x:buf) pst1
                    PR.Continue n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip $ ParseChunksBuf src s buf1 pst1
                    PR.Done 0 b -> do
                        return $ Skip $
                            ParseChunksYield b (ParseChunksInit [] s)
                    PR.Done n b -> do
                        assert (n <= length (x:buf)) (return ())
                        let src = Prelude.reverse (Prelude.take n (x:buf))
                        return $ Skip $
                            ParseChunksYield b (ParseChunksInit src s)
                    PR.Error err -> throwM $ ParseError err
            Skip s -> return $ Skip $ ParseChunksStream s buf pst
            Stop   -> do
                b <- extract pst
                let src = Prelude.reverse buf
                return $ Skip $
                    ParseChunksYield b (ParseChunksInitLeftOver src)

    -- go back to stream processing mode
    stepOuter _ (ParseChunksBuf [] s buf pst) =
        return $ Skip $ ParseChunksStream s buf pst

    -- buffered processing loop
    stepOuter _ (ParseChunksBuf (x:xs) s buf pst) = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                return $ Skip $ ParseChunksBuf xs s [] pst1
            PR.Partial n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let src0 = Prelude.take n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ParseChunksBuf src s [] pst1
            PR.Continue 0 pst1 ->
                return $ Skip $ ParseChunksBuf xs s (x:buf) pst1
            PR.Continue n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ParseChunksBuf src s buf1 pst1
            PR.Done 0 b ->
                return $ Skip $ ParseChunksYield b (ParseChunksInit xs s)
            PR.Done n b -> do
                assert (n <= length (x:buf)) (return ())
                let src = Prelude.reverse (Prelude.take n (x:buf)) ++ xs
                return $ Skip $ ParseChunksYield b (ParseChunksInit src s)
            PR.Error err -> throwM $ ParseError err

    stepOuter _ (ParseChunksYield a next) = return $ Yield a next

{-# ANN type ConcatParseState Fuse #-}
data ConcatParseState x inpBuf st p =
      ConcatParseInit inpBuf st p
    | ConcatParseInitLeftOver inpBuf
    | ConcatParseStream st inpBuf p
    | ConcatParseBuf inpBuf st inpBuf p
    | ConcatParseYield x (ConcatParseState x inpBuf st p)

{-# INLINE_NORMAL parseIterate #-}
parseIterate
    :: MonadThrow m
    => (b -> PRD.Parser m a b)
    -> b
    -> Stream m a
    -> Stream m b
parseIterate func seed (Stream step state) =
    Stream stepOuter (ConcatParseInit [] state (func seed))

    where

    {-# INLINE_LATE stepOuter #-}
    -- Buffer is empty, go to stream processing loop
    stepOuter _ (ConcatParseInit [] st (PRD.Parser pstep initial extract)) = do
        initial >>= \r -> return $ Skip $ ConcatParseStream st []
            (PRD.Parser pstep (return r) extract)

    -- Buffer is not empty, go to buffered processing loop
    stepOuter _ (ConcatParseInit src st
                    (PRD.Parser pstep initial extract)) = do
        initial >>= \r -> return $ Skip $ ConcatParseBuf src st []
            (PRD.Parser pstep (return r) extract)

    -- XXX we just discard any leftover input at the end
    stepOuter _ (ConcatParseInitLeftOver _) = return Stop

    -- Buffer is empty process elements from the stream
    stepOuter gst (ConcatParseStream st buf
                    p@(PRD.Parser pstep initial extract)) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                pst <- initial
                pRes <- pstep pst x
                case pRes of
                    PR.Partial 0 pst1 ->
                        return $ Skip $ ConcatParseStream s []
                            (PRD.Parser pstep (return pst1) extract)
                    PR.Partial n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let src0 = Prelude.take n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip $ ConcatParseBuf src s []
                            (PRD.Parser pstep (return pst1) extract)
                    -- PR.Continue 0 pst1 ->
                    --     return $ Skip $ ConcatParseStream s (x:buf) pst1
                    PR.Continue n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip $ ConcatParseBuf src s buf1
                            (PRD.Parser pstep (return pst1) extract)
                    -- XXX Specialize for Stop 0 common case?
                    PR.Done n b -> do
                        assert (n <= length (x:buf)) (return ())
                        let src = Prelude.reverse (Prelude.take n (x:buf))
                        return $ Skip $
                            ConcatParseYield b (ConcatParseInit src s (func b))
                    PR.Error err -> throwM $ ParseError err
            Skip s -> return $ Skip $ ConcatParseStream s buf p
            Stop   -> do
                pst <- initial
                b <- extract pst
                let src = Prelude.reverse buf
                return $ Skip $
                    ConcatParseYield b (ConcatParseInitLeftOver src)

    -- go back to stream processing mode
    stepOuter _ (ConcatParseBuf [] s buf p) =
        return $ Skip $ ConcatParseStream s buf p

    -- buffered processing loop
    stepOuter _ (ConcatParseBuf (x:xs) s buf
                    (PRD.Parser pstep initial extract)) = do
        pst <- initial
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                return $ Skip $ ConcatParseBuf xs s []
                            (PRD.Parser pstep (return pst1) extract)
            PR.Partial n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let src0 = Prelude.take n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ConcatParseBuf src s []
                            (PRD.Parser pstep (return pst1) extract)
         -- PR.Continue 0 pst1 -> return $ Skip $ ConcatParseBuf xs s (x:buf) pst1
            PR.Continue n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ConcatParseBuf src s buf1
                            (PRD.Parser pstep (return pst1) extract)
            -- XXX Specialize for Stop 0 common case?
            PR.Done n b -> do
                assert (n <= length (x:buf)) (return ())
                let src = Prelude.reverse (Prelude.take n (x:buf)) ++ xs
                return $ Skip $ ConcatParseYield b
                                    (ConcatParseInit src s (func b))
            PR.Error err -> throwM $ ParseError err

    stepOuter _ (ConcatParseYield a next) = return $ Yield a next

-------------------------------------------------------------------------------
-- Concurrent application and fold
-------------------------------------------------------------------------------

{-# INLINE_NORMAL mkParallelD #-}
mkParallelD :: MonadAsync m => Stream m a -> Stream m a
mkParallelD m = Stream step Nothing
    where

    step gst Nothing = do
        sv <- newParallelVar StopNone gst
        toSVarParallel gst sv m
        -- XXX use unfold instead?
        return $ Skip $ Just $ fromSVar sv

    step gst (Just (UnStream step1 st)) = do
        r <- step1 gst st
        return $ case r of
            Yield a s -> Yield a (Just $ Stream step1 s)
            Skip s    -> Skip (Just $ Stream step1 s)
            Stop      -> Stop

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
-- /Internal/
--
{-# INLINE_NORMAL mkParallel #-}
mkParallel :: (K.IsStream t, MonadAsync m) => t m a -> t m a
mkParallel = fromStreamD . mkParallelD . toStreamD

------------------------------------------------------------------------------
-- Time related
------------------------------------------------------------------------------

-- XXX using getTime in the loop can be pretty expensive especially for
-- computations where iterations are lightweight. We have the following
-- options:
--
-- 1) Run a timeout thread updating a flag asynchronously and check that
-- flag here, that way we can have a cheap termination check.
--
-- 2) Use COARSE clock to get time with lower resolution but more efficiently.
--
-- 3) Use rdtscp/rdtsc to get time directly from the processor, compute the
-- termination value of rdtsc in the beginning and then in each iteration just
-- get rdtsc and check if we should terminate.
--
data TakeByTime st s
    = TakeByTimeInit st
    | TakeByTimeCheck st s
    | TakeByTimeYield st s

{-# INLINE_NORMAL takeByTime #-}
takeByTime :: (MonadIO m, TimeUnit64 t) => t -> Stream m a -> Stream m a
takeByTime duration (Stream step1 state1) = Stream step (TakeByTimeInit state1)
    where

    lim = toRelTime64 duration

    {-# INLINE_LATE step #-}
    step _ (TakeByTimeInit _) | lim == 0 = return Stop
    step _ (TakeByTimeInit st) = do
        t0 <- liftIO $ getTime Monotonic
        return $ Skip (TakeByTimeYield st t0)
    step _ (TakeByTimeCheck st t0) = do
        t <- liftIO $ getTime Monotonic
        return $
            if diffAbsTime64 t t0 > lim
            then Stop
            else Skip (TakeByTimeYield st t0)
    step gst (TakeByTimeYield st t0) = do
        r <- step1 gst st
        return $ case r of
             Yield x s -> Yield x (TakeByTimeCheck s t0)
             Skip s -> Skip (TakeByTimeCheck s t0)
             Stop -> Stop

data DropByTime st s x
    = DropByTimeInit st
    | DropByTimeGen st s
    | DropByTimeCheck st s x
    | DropByTimeYield st

{-# INLINE_NORMAL dropByTime #-}
dropByTime :: (MonadIO m, TimeUnit64 t) => t -> Stream m a -> Stream m a
dropByTime duration (Stream step1 state1) = Stream step (DropByTimeInit state1)
    where

    lim = toRelTime64 duration

    {-# INLINE_LATE step #-}
    step _ (DropByTimeInit st) = do
        t0 <- liftIO $ getTime Monotonic
        return $ Skip (DropByTimeGen st t0)
    step gst (DropByTimeGen st t0) = do
        r <- step1 gst st
        return $ case r of
             Yield x s -> Skip (DropByTimeCheck s t0 x)
             Skip s -> Skip (DropByTimeGen s t0)
             Stop -> Stop
    step _ (DropByTimeCheck st t0 x) = do
        t <- liftIO $ getTime Monotonic
        if diffAbsTime64 t t0 <= lim
        then return $ Skip $ DropByTimeGen st t0
        else return $ Yield x $ DropByTimeYield st
    step gst (DropByTimeYield st) = do
        r <- step1 gst st
        return $ case r of
             Yield x s -> Yield x (DropByTimeYield s)
             Skip s -> Skip (DropByTimeYield s)
             Stop -> Stop

-------------------------------------------------------------------------------
-- Hoisting the inner monad
-------------------------------------------------------------------------------

{-# INLINE_NORMAL hoist #-}
hoist :: Monad n => (forall x. m x -> n x) -> Stream m a -> Stream n a
hoist f (Stream step state) = (Stream step' state)
    where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- f $ step (adaptState gst) st
        return $ case r of
            Yield x s -> Yield x s
            Skip  s   -> Skip s
            Stop      -> Stop

{-# INLINE generally #-}
generally :: Monad m => Stream Identity a -> Stream m a
generally = hoist (return . runIdentity)

{-# INLINE_NORMAL liftInner #-}
liftInner :: (Monad m, MonadTrans t, Monad (t m))
    => Stream m a -> Stream (t m) a
liftInner (Stream step state) = Stream step' state
    where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- lift $ step (adaptState gst) st
        return $ case r of
            Yield x s -> Yield x s
            Skip s    -> Skip s
            Stop      -> Stop

{-# INLINE_NORMAL runReaderT #-}
runReaderT :: Monad m => m s -> Stream (ReaderT s m) a -> Stream m a
runReaderT env (Stream step state) = Stream step' (state, env)
    where
    {-# INLINE_LATE step' #-}
    step' gst (st, action) = do
        sv <- action
        r <- Reader.runReaderT (step (adaptState gst) st) sv
        return $ case r of
            Yield x s -> Yield x (s, return sv)
            Skip  s   -> Skip (s, return sv)
            Stop      -> Stop

{-# INLINE_NORMAL evalStateT #-}
evalStateT :: Monad m => m s -> Stream (StateT s m) a -> Stream m a
evalStateT initial (Stream step state) = Stream step' (state, initial)
    where
    {-# INLINE_LATE step' #-}
    step' gst (st, action) = do
        sv <- action
        (r, sv') <- State.runStateT (step (adaptState gst) st) sv
        return $ case r of
            Yield x s -> Yield x (s, return sv')
            Skip  s   -> Skip (s, return sv')
            Stop      -> Stop

{-# INLINE_NORMAL runStateT #-}
runStateT :: Monad m => m s -> Stream (StateT s m) a -> Stream m (s, a)
runStateT initial (Stream step state) = Stream step' (state, initial)
    where
    {-# INLINE_LATE step' #-}
    step' gst (st, action) = do
        sv <- action
        (r, sv') <- State.runStateT (step (adaptState gst) st) sv
        return $ case r of
            Yield x s -> Yield (sv', x) (s, return sv')
            Skip  s   -> Skip (s, return sv')
            Stop      -> Stop
