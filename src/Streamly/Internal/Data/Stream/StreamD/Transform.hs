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

    -- ** By folding (scans)
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

    -- * Zipping
    , indexed
    , indexedR
    , zipWith
    , zipWithM

    -- * Comparisons
    , eqBy
    , cmpBy

    -- * Merging
    , mergeBy
    , mergeByM

    -- * Transformation comprehensions
    , the

    -- * Concurrent Application
    , mkParallel
    , mkParallelD
    , newCallbackStream
    )
where

import Control.Concurrent (myThreadId)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromJust, isJust)
import Streamly.Internal.Data.Time.Units
       (TimeUnit64, toRelTime64, diffAbsTime64)
import Streamly.Internal.Data.Pipe.Types (Pipe(..), PipeState(..))
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)

import qualified Streamly.Internal.Data.Pipe.Types as Pipe
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.StreamK as K

import Prelude hiding
       ( drop, dropWhile, filter, map, mapM
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
-- Transformation by Folding (Scans)
------------------------------------------------------------------------------

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
-- Zipping
------------------------------------------------------------------------------

{-# INLINE_NORMAL indexed #-}
indexed :: Monad m => Stream m a -> Stream m (Int, a)
indexed (Stream step state) = Stream step' (state, 0)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, i) = i `seq` do
         r <- step (adaptState gst) st
         case r of
             Yield x s -> return $ Yield (i, x) (s, i+1)
             Skip    s -> return $ Skip (s, i)
             Stop      -> return Stop

{-# INLINE_NORMAL indexedR #-}
indexedR :: Monad m => Int -> Stream m a -> Stream m (Int, a)
indexedR m (Stream step state) = Stream step' (state, m)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, i) = i `seq` do
         r <- step (adaptState gst) st
         case r of
             Yield x s -> let i' = i - 1
                          in return $ Yield (i, x) (s, i')
             Skip    s -> return $ Skip (s, i)
             Stop      -> return Stop

{-# INLINE_NORMAL zipWithM #-}
zipWithM :: Monad m
    => (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
zipWithM f (Stream stepa ta) (Stream stepb tb) = Stream step (ta, tb, Nothing)
  where
    {-# INLINE_LATE step #-}
    step gst (sa, sb, Nothing) = do
        r <- stepa (adaptState gst) sa
        return $
          case r of
            Yield x sa' -> Skip (sa', sb, Just x)
            Skip sa'    -> Skip (sa', sb, Nothing)
            Stop        -> Stop

    step gst (sa, sb, Just x) = do
        r <- stepb (adaptState gst) sb
        case r of
            Yield y sb' -> do
                z <- f x y
                return $ Yield z (sa, sb', Nothing)
            Skip sb' -> return $ Skip (sa, sb', Just x)
            Stop     -> return Stop

#if __GLASGOW_HASKELL__ >= 801
{-# RULES "zipWithM xs xs"
    forall f xs. zipWithM @Identity f xs xs = mapM (\x -> f x x) xs #-}
#endif

{-# INLINE zipWith #-}
zipWith :: Monad m => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWith f = zipWithM (\a b -> return (f a b))

------------------------------------------------------------------------------
-- Merging
------------------------------------------------------------------------------

{-# INLINE_NORMAL mergeByM #-}
mergeByM
    :: (Monad m)
    => (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeByM cmp (Stream stepa ta) (Stream stepb tb) =
    Stream step (Just ta, Just tb, Nothing, Nothing)
  where
    {-# INLINE_LATE step #-}

    -- one of the values is missing, and the corresponding stream is running
    step gst (Just sa, sb, Nothing, b) = do
        r <- stepa gst sa
        return $ case r of
            Yield a sa' -> Skip (Just sa', sb, Just a, b)
            Skip sa'    -> Skip (Just sa', sb, Nothing, b)
            Stop        -> Skip (Nothing, sb, Nothing, b)

    step gst (sa, Just sb, a, Nothing) = do
        r <- stepb gst sb
        return $ case r of
            Yield b sb' -> Skip (sa, Just sb', a, Just b)
            Skip sb'    -> Skip (sa, Just sb', a, Nothing)
            Stop        -> Skip (sa, Nothing, a, Nothing)

    -- both the values are available
    step _ (sa, sb, Just a, Just b) = do
        res <- cmp a b
        return $ case res of
            GT -> Yield b (sa, sb, Just a, Nothing)
            _  -> Yield a (sa, sb, Nothing, Just b)

    -- one of the values is missing, corresponding stream is done
    step _ (Nothing, sb, Nothing, Just b) =
            return $ Yield b (Nothing, sb, Nothing, Nothing)

    step _ (sa, Nothing, Just a, Nothing) =
            return $ Yield a (sa, Nothing, Nothing, Nothing)

    step _ (Nothing, Nothing, Nothing, Nothing) = return Stop

{-# INLINE mergeBy #-}
mergeBy
    :: (Monad m)
    => (a -> a -> Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeBy cmp = mergeByM (\a b -> return $ cmp a b)

------------------------------------------------------------------------------
-- Transformation comprehensions
------------------------------------------------------------------------------

{-# INLINE_NORMAL the #-}
the :: (Eq a, Monad m) => Stream m a -> m (Maybe a)
the (Stream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield x s -> go' x s
            Skip s    -> go s
            Stop      -> return Nothing
    go' n st = do
        r <- step defState st
        case r of
            Yield x s | x == n -> go' n s
                      | otherwise -> return Nothing
            Skip s -> go' n s
            Stop   -> return (Just n)

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

-- Note: we can use another API with two callbacks stop and yield if we want
-- the callback to be able to indicate end of stream.
--
-- | Generates a callback and a stream pair. The callback returned is used to
-- queue values to the stream.  The stream is infinite, there is no way for the
-- callback to indicate that it is done now.
--
-- /Internal/
--
{-# INLINE_NORMAL newCallbackStream #-}
newCallbackStream :: (K.IsStream t, MonadAsync m) => m ((a -> m ()), t m a)
newCallbackStream = do
    sv <- newParallelVar StopNone defState

    -- XXX Add our own thread-id to the SVar as we can not know the callback's
    -- thread-id and the callback is not run in a managed worker. We need to
    -- handle this better.
    liftIO myThreadId >>= modifyThread sv

    let callback a = liftIO $ void $ send sv (ChildYield a)
    -- XXX we can return an SVar and then the consumer can unfold from the
    -- SVar?
    return (callback, fromStreamD (fromSVar sv))

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
