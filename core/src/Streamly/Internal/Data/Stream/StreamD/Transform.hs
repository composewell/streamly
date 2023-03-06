-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Transform
-- Copyright   : (c) 2018 Composewell Technologies
--               (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- "Streamly.Internal.Data.Pipe" might ultimately replace this module.

-- A few functions in this module have been adapted from the vector package
-- (c) Roman Leshchinskiy. See the notes in specific combinators.

module Streamly.Internal.Data.Stream.StreamD.Transform
    (
    -- * Piping
    -- | Pass through a 'Pipe'.
      transform

    -- * Mapping
    -- | Stateless one-to-one maps.
    , map
    , mapM
    , sequence

    -- * Mapping Effects
    , tap
    , tapOffsetEvery
    , trace
    , trace_

    -- * Folding
    , foldrS
    , foldlS

    -- * Scanning By 'Fold'
    , postscan
    , scan
    , scanMany

    -- * Splitting
    , splitOn

    -- * Scanning
    -- | Left scans. Stateful, mostly one-to-one maps.
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

    -- * Filtering
    -- | Produce a subset of the stream.
    , with
    , scanMaybe
    , filter
    , filterM
    , deleteBy
    , uniqBy
    , uniq
    , prune
    , repeated

    -- * Trimming
    -- | Produce a subset of the stream trimmed at ends.
    , take
    , takeWhile
    , takeWhileM
    , takeWhileLast
    , takeWhileAround
    , drop
    , dropWhile
    , dropWhileM
    , dropLast
    , dropWhileLast
    , dropWhileAround

    -- * Inserting Elements
    -- | Produce a superset of the stream.
    , insertBy
    , intersperse
    , intersperseM
    , intersperseMWith
    , intersperseMSuffix
    , intersperseMSuffixWith

    -- * Inserting Side Effects
    , intersperseM_
    , intersperseMSuffix_
    , intersperseMPrefix_

    , delay
    , delayPre
    , delayPost

    -- * Reordering
    -- | Produce strictly the same set but reordered.
    , reverse
    , reverseUnbox
    , reassembleBy

    -- * Position Indexing
    , indexed
    , indexedR

    -- * Time Indexing
    , timestampWith
    , timestamped
    , timeIndexWith
    , timeIndexed

    -- * Searching
    , findIndices
    , elemIndices
    , slicesBy

    -- * Rolling map
    -- | Map using the previous element.
    , rollingMap
    , rollingMapM
    , rollingMap2

    -- * Maybe Streams
    , mapMaybe
    , mapMaybeM
    , catMaybes

    -- * Either Streams
    , catLefts
    , catRights
    , catEithers
    )
where

#include "inline.hs"

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (fromLeft, isLeft, isRight, fromRight)
import Data.Functor ((<&>))
import Data.Maybe (fromJust, isJust)
import Fusion.Plugin.Types (Fuse(..))

import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Pipe.Type (Pipe(..), PipeState(..))
import Streamly.Internal.Data.SVar.Type (adaptState)
import Streamly.Internal.Data.Time.Units (AbsTime, RelTime64)
import Streamly.Internal.Data.Unboxed (Unbox)
import Streamly.Internal.System.IO (defaultChunkSize)

-- import qualified Data.List as List
import qualified Streamly.Internal.Data.Array.Type as A
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Pipe.Type as Pipe
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Prelude hiding
       ( drop, dropWhile, filter, map, mapM, reverse
       , scanl, scanl1, sequence, take, takeWhile, zipWith)

import Streamly.Internal.Data.Stream.StreamD.Generate
    (absTimesWith, relTimesWith)
import Streamly.Internal.Data.Stream.StreamD.Type

--
-- $setup
-- >>> :m
-- >>> import Control.Concurrent (threadDelay)
-- >>> import Control.Monad (void)
-- >>> import Control.Monad.IO.Class (MonadIO (liftIO))
-- >>> import Data.Either (fromLeft, fromRight, isLeft, isRight, either)
-- >>> import Data.Function ((&))
-- >>> import Data.Maybe (fromJust, isJust)
-- >>> import Prelude hiding (filter, drop, dropWhile, take, takeWhile, foldr, map, mapM, sequence, reverse, foldr1 , scanl, scanl1)
-- >>> import Streamly.Internal.Data.Stream (Stream)
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold (filtering)
-- >>> import qualified Streamly.Internal.Data.Fold.Window as Window
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
--
-- >>> hSetBuffering stdout LineBuffering

------------------------------------------------------------------------------
-- Piping
------------------------------------------------------------------------------

-- | Use a 'Pipe' to transform a stream.
--
-- /Pre-release/
--
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
-- Transformation Folds
------------------------------------------------------------------------------

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
-- Transformation by Mapping
------------------------------------------------------------------------------

-- |
-- >>> sequence = Stream.mapM id
--
-- Replace the elements of a stream of monadic actions with the outputs of
-- those actions.
--
-- >>> s = Stream.fromList [putStr "a", putStr "b", putStrLn "c"]
-- >>> Stream.fold Fold.drain $ Stream.sequence s
-- abc
--
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
-- Mapping side effects
------------------------------------------------------------------------------

data TapState fs st a
    = TapInit | Tapping !fs st | TapDone st

-- XXX Multiple yield points

-- | Tap the data flowing through a stream into a 'Fold'. For example, you may
-- add a tap to log the contents flowing through the stream. The fold is used
-- only for effects, its result is discarded.
--
-- @
--                   Fold m a b
--                       |
-- -----stream m a ---------------stream m a-----
--
-- @
--
-- >>> s = Stream.enumerateFromTo 1 2
-- >>> Stream.fold Fold.drain $ Stream.tap (Fold.drainMapM print) s
-- 1
-- 2
--
-- Compare with 'trace'.
--
{-# INLINE tap #-}
tap :: Monad m => Fold m a b -> Stream m a -> Stream m a
tap (Fold fstep initial extract) (Stream step state) = Stream step' TapInit

    where

    step' _ TapInit = do
        res <- initial
        return
            $ Skip
            $ case res of
                  FL.Partial s -> Tapping s state
                  FL.Done _ -> TapDone state
    step' gst (Tapping acc st) = do
        r <- step gst st
        case r of
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
        res <- initial
        return
            $ Skip
            $ case res of
                  FL.Partial s -> TapOffTapping s state (offset `mod` n)
                  FL.Done _ -> TapOffDone state
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

-- | Apply a monadic function to each element flowing through the stream and
-- discard the results.
--
-- >>> s = Stream.enumerateFromTo 1 2
-- >>> Stream.fold Fold.drain $ Stream.trace print s
-- 1
-- 2
--
-- Compare with 'tap'.
--
{-# INLINE trace #-}
trace :: Monad m => (a -> m b) -> Stream m a -> Stream m a
trace f = mapM (\x -> void (f x) >> return x)

-- | Perform a side effect before yielding each element of the stream and
-- discard the results.
--
-- >>> s = Stream.enumerateFromTo 1 2
-- >>> Stream.fold Fold.drain $ Stream.trace_ (print "got here") s
-- "got here"
-- "got here"
--
-- Same as 'intersperseMPrefix_' but always serial.
--
-- See also: 'trace'
--
-- /Pre-release/
{-# INLINE trace_ #-}
trace_ :: Monad m => m b -> Stream m a -> Stream m a
trace_ eff = mapM (\x -> eff >> return x)

------------------------------------------------------------------------------
-- Scanning with a Fold
------------------------------------------------------------------------------

data ScanState s f = ScanInit s | ScanDo s !f | ScanDone

-- | Postscan a stream using the given monadic fold.
--
-- The following example extracts the input stream up to a point where the
-- running average of elements is no more than 10:
--
-- >>> import Data.Maybe (fromJust)
-- >>> let avg = Fold.teeWith (/) Fold.sum (fmap fromIntegral Fold.length)
-- >>> s = Stream.enumerateFromTo 1.0 100.0
-- >>> :{
--  Stream.fold Fold.toList
--   $ fmap (fromJust . fst)
--   $ Stream.takeWhile (\(_,x) -> x <= 10)
--   $ Stream.postscan (Fold.tee Fold.latest avg) s
-- :}
-- [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0]
--
{-# INLINE_NORMAL postscan #-}
postscan :: Monad m => FL.Fold m a b -> Stream m a -> Stream m b
postscan (FL.Fold fstep initial extract) (Stream sstep state) =
    Stream step (ScanInit state)

    where

    {-# INLINE_LATE step #-}
    step _ (ScanInit st) = do
        res <- initial
        return
            $ case res of
                  FL.Partial fs -> Skip $ ScanDo st fs
                  FL.Done b -> Yield b ScanDone
    step gst (ScanDo st fs) = do
        res <- sstep (adaptState gst) st
        case res of
            Yield x s -> do
                r <- fstep fs x
                case r of
                    FL.Partial fs1 -> do
                        !b <- extract fs1
                        return $ Yield b $ ScanDo s fs1
                    FL.Done b -> return $ Yield b ScanDone
            Skip s -> return $ Skip $ ScanDo s fs
            Stop -> return Stop
    step _ ScanDone = return Stop

{-# INLINE scanWith #-}
scanWith :: Monad m
    => Bool -> Fold m a b -> Stream m a -> Stream m b
scanWith restart (Fold fstep initial extract) (Stream sstep state) =
    Stream step (ScanInit state)

    where

    {-# INLINE runStep #-}
    runStep st action = do
        res <- action
        case res of
            FL.Partial fs -> do
                !b <- extract fs
                return $ Yield b $ ScanDo st fs
            FL.Done b ->
                let next = if restart then ScanInit st else ScanDone
                 in return $ Yield b next

    {-# INLINE_LATE step #-}
    step _ (ScanInit st) = runStep st initial
    step gst (ScanDo st fs) = do
        res <- sstep (adaptState gst) st
        case res of
            Yield x s -> runStep s (fstep fs x)
            Skip s -> return $ Skip $ ScanDo s fs
            Stop -> return Stop
    step _ ScanDone = return Stop

-- XXX It may be useful to have a version of scan where we can keep the
-- accumulator independent of the value emitted. So that we do not necessarily
-- have to keep a value in the accumulator which we are not using. We can pass
-- an extraction function that will take the accumulator and the current value
-- of the element and emit the next value in the stream. That will also make it
-- possible to modify the accumulator after using it. In fact, the step function
-- can return new accumulator and the value to be emitted. The signature would
-- be more like mapAccumL.

-- | Strict left scan. Scan a stream using the given monadic fold.
--
-- >>> s = Stream.fromList [1..10]
-- >>> Stream.fold Fold.toList $ Stream.takeWhile (< 10) $ Stream.scan Fold.sum s
-- [0,1,3,6]
--
-- See also: 'usingStateT'
--

-- EXPLANATION:
-- >>> scanl' step z = Stream.scan (Fold.foldl' step z)
--
-- Like 'map', 'scanl'' too is a one to one transformation,
-- however it adds an extra element.
--
-- >>> s = Stream.fromList [1,2,3,4]
-- >>> Stream.fold Fold.toList $ scanl' (+) 0 s
-- [0,1,3,6,10]
--
-- >>> Stream.fold Fold.toList $ scanl' (flip (:)) [] s
-- [[],[1],[2,1],[3,2,1],[4,3,2,1]]
--
-- The output of 'scanl'' is the initial value of the accumulator followed by
-- all the intermediate steps and the final result of 'foldl''.
--
-- By streaming the accumulated state after each fold step, we can share the
-- state across multiple stages of stream composition. Each stage can modify or
-- extend the state, do some processing with it and emit it for the next stage,
-- thus modularizing the stream processing. This can be useful in
-- stateful or event-driven programming.
--
-- Consider the following monolithic example, computing the sum and the product
-- of the elements in a stream in one go using a @foldl'@:
--
-- >>> foldl' step z = Stream.fold (Fold.foldl' step z)
-- >>> foldl' (\(s, p) x -> (s + x, p * x)) (0,1) s
-- (10,24)
--
-- Using @scanl'@ we can make it modular by computing the sum in the first
-- stage and passing it down to the next stage for computing the product:
--
-- >>> :{
--   foldl' (\(_, p) (s, x) -> (s, p * x)) (0,1)
--   $ scanl' (\(s, _) x -> (s + x, x)) (0,1)
--   $ Stream.fromList [1,2,3,4]
-- :}
-- (10,24)
--
-- IMPORTANT: 'scanl'' evaluates the accumulator to WHNF.  To avoid building
-- lazy expressions inside the accumulator, it is recommended that a strict
-- data structure is used for accumulator.
--
{-# INLINE scan #-}
scan :: Monad m
    => FL.Fold m a b -> Stream m a -> Stream m b
scan = scanWith False

-- | Like 'scan' but restarts scanning afresh when the scanning fold
-- terminates.
--
{-# INLINE scanMany #-}
scanMany :: Monad m
    => FL.Fold m a b -> Stream m a -> Stream m b
scanMany = scanWith True

------------------------------------------------------------------------------
-- Scanning - Prescans
------------------------------------------------------------------------------

-- Adapted from the vector package.
--
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
postscanlx' fstep begin done =
    postscanlMx' (\b a -> return (fstep b a)) (return begin) (return . done)

-- XXX do we need consM strict to evaluate the begin value?
{-# INLINE scanlMx' #-}
scanlMx' :: Monad m
    => (x -> a -> m x) -> m x -> (x -> m b) -> Stream m a -> Stream m b
scanlMx' fstep begin done s =
    (begin >>= \x -> x `seq` done x) `consM` postscanlMx' fstep begin done s

{-# INLINE scanlx' #-}
scanlx' :: Monad m
    => (x -> a -> x) -> x -> (x -> b) -> Stream m a -> Stream m b
scanlx' fstep begin done =
    scanlMx' (\b a -> return (fstep b a)) (return begin) (return . done)

------------------------------------------------------------------------------
-- postscans
------------------------------------------------------------------------------

-- Adapted from the vector package.
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

{-# ANN type PScanAfterState Fuse #-}
data PScanAfterState m st acc =
      PScanAfterStep st (m acc)
    | PScanAfterYield acc (PScanAfterState m st acc)
    | PScanAfterStop

-- We can possibly have the "done" function as a Maybe to provide an option to
-- emit or not emit the accumulator when the stream stops.
--
-- TBD: use a single Yield point
--
{-# INLINE_NORMAL postscanlMAfter' #-}
postscanlMAfter' :: Monad m
    => (b -> a -> m b) -> m b -> (b -> m b) -> Stream m a -> Stream m b
postscanlMAfter' fstep initial done (Stream step1 state1) = do
    Stream step (PScanAfterStep state1 initial)

    where

    {-# INLINE_LATE step #-}
    step gst (PScanAfterStep st acc) = do
        r <- step1 (adaptState gst) st
        case r of
            Yield x s -> do
                !old <- acc
                !y <- fstep old x
                return (Skip $ PScanAfterYield y (PScanAfterStep s (return y)))
            Skip s -> return $ Skip $ PScanAfterStep s acc
            -- Strictness is important for fusion
            Stop -> do
                !v <- acc
                !res <- done v
                return (Skip $ PScanAfterYield res PScanAfterStop)
    step _ (PScanAfterYield acc next) = return $ Yield acc next
    step _ PScanAfterStop = return Stop

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

-- | Like 'scanl'' but with a monadic step function and a monadic seed.
--
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
scanlMAfter' :: Monad m
    => (b -> a -> m b) -> m b -> (b -> m b) -> Stream m a -> Stream m b
scanlMAfter' fstep initial done s =
    initial `consM` postscanlMAfter' fstep initial done s

-- >>> scanl' f z xs = z `Stream.cons` postscanl' f z xs

-- | Strict left scan. Like 'map', 'scanl'' too is a one to one transformation,
-- however it adds an extra element.
--
-- >>> Stream.toList $ Stream.scanl' (+) 0 $ Stream.fromList [1,2,3,4]
-- [0,1,3,6,10]
--
-- >>> Stream.toList $ Stream.scanl' (flip (:)) [] $ Stream.fromList [1,2,3,4]
-- [[],[1],[2,1],[3,2,1],[4,3,2,1]]
--
-- The output of 'scanl'' is the initial value of the accumulator followed by
-- all the intermediate steps and the final result of 'foldl''.
--
-- By streaming the accumulated state after each fold step, we can share the
-- state across multiple stages of stream composition. Each stage can modify or
-- extend the state, do some processing with it and emit it for the next stage,
-- thus modularizing the stream processing. This can be useful in
-- stateful or event-driven programming.
--
-- Consider the following monolithic example, computing the sum and the product
-- of the elements in a stream in one go using a @foldl'@:
--
-- >>> Stream.fold (Fold.foldl' (\(s, p) x -> (s + x, p * x)) (0,1)) $ Stream.fromList [1,2,3,4]
-- (10,24)
--
-- Using @scanl'@ we can make it modular by computing the sum in the first
-- stage and passing it down to the next stage for computing the product:
--
-- >>> :{
--   Stream.fold (Fold.foldl' (\(_, p) (s, x) -> (s, p * x)) (0,1))
--   $ Stream.scanl' (\(s, _) x -> (s + x, x)) (0,1)
--   $ Stream.fromList [1,2,3,4]
-- :}
-- (10,24)
--
-- IMPORTANT: 'scanl'' evaluates the accumulator to WHNF.  To avoid building
-- lazy expressions inside the accumulator, it is recommended that a strict
-- data structure is used for accumulator.
--
-- >>> scanl' step z = Stream.scan (Fold.foldl' step z)
-- >>> scanl' f z xs = Stream.scanlM' (\a b -> return (f a b)) (return z) xs
--
-- See also: 'usingStateT'
--
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
            Stop   -> return Stop

{-# INLINE scanl #-}
scanl :: Monad m => (b -> a -> b) -> b -> Stream m a -> Stream m b
scanl f seed = scanlM (\a b -> return (f a b)) (return seed)

-- Adapted from the vector package
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

-- Adapted from the vector package

-- | Like 'scanl1'' but with a monadic step function.
--
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

-- | Like 'scanl'' but for a non-empty stream. The first element of the stream
-- is used as the initial value of the accumulator. Does nothing if the stream
-- is empty.
--
-- >>> Stream.toList $ Stream.scanl1' (+) $ Stream.fromList [1,2,3,4]
-- [1,3,6,10]
--
{-# INLINE scanl1' #-}
scanl1' :: Monad m => (a -> a -> a) -> Stream m a -> Stream m a
scanl1' f = scanl1M' (\x y -> return (f x y))

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

-- | Modify a @Stream m a -> Stream m a@ stream transformation that accepts a
-- predicate @(a -> b)@ to accept @((s, a) -> b)@ instead, provided a
-- transformation @Stream m a -> Stream m (s, a)@. Convenient to filter with
-- index or time.
--
-- >>> filterWithIndex = Stream.with Stream.indexed Stream.filter
--
-- /Pre-release/
{-# INLINE with #-}
with :: Monad m =>
       (Stream m a -> Stream m (s, a))
    -> (((s, a) -> b) -> Stream m (s, a) -> Stream m (s, a))
    -> (((s, a) -> b) -> Stream m a -> Stream m a)
with f comb g = fmap snd . comb g . f

-- Adapted from the vector package

-- | Same as 'filter' but with a monadic predicate.
--
-- >>> f p x = p x >>= \r -> return $ if r then Just x else Nothing
-- >>> filterM p = Stream.mapMaybeM (f p)
--
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

-- | Include only those elements that pass a predicate.
--
-- >>> filter p = Stream.filterM (return . p)
-- >>> filter p = Stream.mapMaybe (\x -> if p x then Just x else Nothing)
-- >>> filter p = Stream.scanMaybe (Fold.filtering p)
--
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
filter f = filterM (return . f)
-- filter p = scanMaybe (FL.filtering p)

-- | Drop repeated elements that are adjacent to each other using the supplied
-- comparison function.
--
-- >>> uniq = Stream.uniqBy (==)
--
-- To strip duplicate path separators:
--
-- >>> input = Stream.fromList "//a//b"
-- >>> f x y = x == '/' && y == '/'
-- >>> Stream.fold Fold.toList $ Stream.uniqBy f input
-- "/a/b"
--
-- Space: @O(1)@
--
-- /Pre-release/
--
{-# INLINE uniqBy #-}
uniqBy :: Monad m =>
    (a -> a -> Bool) -> Stream m a -> Stream m a
-- uniqBy eq = scanMaybe (FL.uniqBy eq)
uniqBy eq = catMaybes . rollingMap f

    where

    f pre curr =
        case pre of
            Nothing -> Just curr
            Just x -> if x `eq` curr then Nothing else Just curr

-- Adapted from the vector package

-- | Drop repeated elements that are adjacent to each other.
--
-- >>> uniq = Stream.uniqBy (==)
--
{-# INLINE_NORMAL uniq #-}
uniq :: (Eq a, Monad m) => Stream m a -> Stream m a
-- uniq = scanMaybe FL.uniq
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

-- | Deletes the first occurrence of the element in the stream that satisfies
-- the given equality predicate.
--
-- >>> input = Stream.fromList [1,3,3,5]
-- >>> Stream.fold Fold.toList $ Stream.deleteBy (==) 3 input
-- [1,3,5]
--
{-# INLINE_NORMAL deleteBy #-}
deleteBy :: Monad m => (a -> a -> Bool) -> a -> Stream m a -> Stream m a
-- deleteBy cmp x = scanMaybe (FL.deleteBy cmp x)
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

-- | Strip all leading and trailing occurrences of an element passing a
-- predicate and make all other consecutive occurrences uniq.
--
-- >> prune p = Stream.dropWhileAround p $ Stream.uniqBy (x y -> p x && p y)
--
-- @
-- > Stream.prune isSpace (Stream.fromList "  hello      world!   ")
-- "hello world!"
--
-- @
--
-- Space: @O(1)@
--
-- /Unimplemented/
{-# INLINE prune #-}
prune ::
    -- (Monad m, Eq a) =>
    (a -> Bool) -> Stream m a -> Stream m a
prune = error "Not implemented yet!"

-- Possible implementation:
-- @repeated =
--      Stream.catMaybes . Stream.parseMany (Parser.groupBy (==) Fold.repeated)@
--
-- 'Fold.repeated' should return 'Just' when repeated, and 'Nothing' for a
-- single element.

-- | Emit only repeated elements, once.
--
-- /Unimplemented/
repeated :: -- (Monad m, Eq a) =>
    Stream m a -> Stream m a
repeated = undefined

------------------------------------------------------------------------------
-- Trimming
------------------------------------------------------------------------------

-- | Take all consecutive elements at the end of the stream for which the
-- predicate is true.
--
-- O(n) space, where n is the number elements taken.
--
-- /Unimplemented/
{-# INLINE takeWhileLast #-}
takeWhileLast :: -- Monad m =>
    (a -> Bool) -> Stream m a -> Stream m a
takeWhileLast = undefined -- fromStreamD $ D.takeWhileLast n $ toStreamD m

-- | Like 'takeWhile' and 'takeWhileLast' combined.
--
-- O(n) space, where n is the number elements taken from the end.
--
-- /Unimplemented/
{-# INLINE takeWhileAround #-}
takeWhileAround :: -- Monad m =>
    (a -> Bool) -> Stream m a -> Stream m a
takeWhileAround = undefined -- fromStreamD $ D.takeWhileAround n $ toStreamD m

-- Adapted from the vector package

-- | Discard first 'n' elements from the stream and take the rest.
--
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

-- Adapted from the vector package
data DropWhileState s a
    = DropWhileDrop s
    | DropWhileYield a s
    | DropWhileNext s

-- | Same as 'dropWhile' but with a monadic predicate.
--
{-# INLINE_NORMAL dropWhileM #-}
dropWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
-- dropWhileM p = scanMaybe (FL.droppingWhileM p)
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

-- | Drop elements in the stream as long as the predicate succeeds and then
-- take the rest of the stream.
--
{-# INLINE dropWhile #-}
dropWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
-- dropWhile p = scanMaybe (FL.droppingWhile p)
dropWhile f = dropWhileM (return . f)

-- | Drop @n@ elements at the end of the stream.
--
-- O(n) space, where n is the number elements dropped.
--
-- /Unimplemented/
{-# INLINE dropLast #-}
dropLast :: -- Monad m =>
    Int -> Stream m a -> Stream m a
dropLast = undefined -- fromStreamD $ D.dropLast n $ toStreamD m

-- | Drop all consecutive elements at the end of the stream for which the
-- predicate is true.
--
-- O(n) space, where n is the number elements dropped.
--
-- /Unimplemented/
{-# INLINE dropWhileLast #-}
dropWhileLast :: -- Monad m =>
    (a -> Bool) -> Stream m a -> Stream m a
dropWhileLast = undefined -- fromStreamD $ D.dropWhileLast n $ toStreamD m

-- | Like 'dropWhile' and 'dropWhileLast' combined.
--
-- O(n) space, where n is the number elements dropped from the end.
--
-- /Unimplemented/
{-# INLINE dropWhileAround #-}
dropWhileAround :: -- Monad m =>
    (a -> Bool) -> Stream m a -> Stream m a
dropWhileAround = undefined -- fromStreamD $ D.dropWhileAround n $ toStreamD m

------------------------------------------------------------------------------
-- Inserting Elements
------------------------------------------------------------------------------

-- | @insertBy cmp elem stream@ inserts @elem@ before the first element in
-- @stream@ that is less than @elem@ when compared using @cmp@.
--
-- >>> insertBy cmp x = Stream.mergeBy cmp (Stream.fromPure x)
--
-- >>> input = Stream.fromList [1,3,5]
-- >>> Stream.fold Fold.toList $ Stream.insertBy compare 2 input
-- [1,2,3,5]
--
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

data LoopState x s = FirstYield s
                   | InterspersingYield s
                   | YieldAndCarry x s

-- intersperseM = intersperseMWith 1

-- | Insert an effect and its output before consuming an element of a stream
-- except the first one.
--
-- >>> input = Stream.fromList "hello"
-- >>> Stream.fold Fold.toList $ Stream.trace putChar $ Stream.intersperseM (putChar '.' >> return ',') input
-- h.,e.,l.,l.,o"h,e,l,l,o"
--
-- Be careful about the order of effects. In the above example we used trace
-- after the intersperse, if we use it before the intersperse the output would
-- be he.l.l.o."h,e,l,l,o".
--
-- >>> Stream.fold Fold.toList $ Stream.intersperseM (putChar '.' >> return ',') $ Stream.trace putChar input
-- he.l.l.o."h,e,l,l,o"
--
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

-- | Insert a pure value between successive elements of a stream.
--
-- >>> input = Stream.fromList "hello"
-- >>> Stream.fold Fold.toList $ Stream.intersperse ',' input
-- "h,e,l,l,o"
--
{-# INLINE intersperse #-}
intersperse :: Monad m => a -> Stream m a -> Stream m a
intersperse a = intersperseM (return a)

-- | Insert a side effect before consuming an element of a stream except the
-- first one.
--
-- >>> input = Stream.fromList "hello"
-- >>> Stream.fold Fold.drain $ Stream.trace putChar $ Stream.intersperseM_ (putChar '.') input
-- h.e.l.l.o
--
-- /Pre-release/
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

-- | Intersperse a monadic action into the input stream after every @n@
-- elements.
--
-- >> input = Stream.fromList "hello"
-- >> Stream.fold Fold.toList $ Stream.intersperseMWith 2 (return ',') input
-- "he,ll,o"
--
-- /Unimplemented/
{-# INLINE intersperseMWith #-}
intersperseMWith :: -- Monad m =>
    Int -> m a -> Stream m a -> Stream m a
intersperseMWith _n _f _xs = undefined

data SuffixState s a
    = SuffixElem s
    | SuffixSuffix s
    | SuffixYield a (SuffixState s a)

-- | Insert an effect and its output after consuming an element of a stream.
--
-- >>> input = Stream.fromList "hello"
-- >>> Stream.fold Fold.toList $ Stream.trace putChar $ Stream.intersperseMSuffix (putChar '.' >> return ',') input
-- h.,e.,l.,l.,o.,"h,e,l,l,o,"
--
-- /Pre-release/
{-# INLINE_NORMAL intersperseMSuffix #-}
intersperseMSuffix :: forall m a. Monad m => m a -> Stream m a -> Stream m a
intersperseMSuffix action (Stream step state) = Stream step' (SuffixElem state)
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

-- | Insert a side effect after consuming an element of a stream.
--
-- >>> input = Stream.fromList "hello"
-- >>> Stream.fold Fold.toList $ Stream.intersperseMSuffix_ (threadDelay 1000000) input
-- "hello"
--
-- /Pre-release/
--
{-# INLINE_NORMAL intersperseMSuffix_ #-}
intersperseMSuffix_ :: Monad m => m b -> Stream m a -> Stream m a
intersperseMSuffix_ m (Stream step1 state1) = Stream step (Left state1)
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

-- | Like 'intersperseMSuffix' but intersperses an effectful action into the
-- input stream after every @n@ elements and after the last element.
--
-- >>> input = Stream.fromList "hello"
-- >>> Stream.fold Fold.toList $ Stream.intersperseMSuffixWith 2 (return ',') input
-- "he,ll,o,"
--
-- /Pre-release/
--
{-# INLINE_NORMAL intersperseMSuffixWith #-}
intersperseMSuffixWith :: forall m a. Monad m
    => Int -> m a -> Stream m a -> Stream m a
intersperseMSuffixWith n action (Stream step state) =
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

    step' _ SuffixSpanLast = do
        action >>= \r -> return $ Skip (SuffixSpanYield r SuffixSpanStop)

    step' _ (SuffixSpanYield x next) = return $ Yield x next

    step' _ SuffixSpanStop = return Stop

-- | Insert a side effect before consuming an element of a stream.
--
-- Definition:
--
-- >>> intersperseMPrefix_ m = Stream.mapM (\x -> void m >> return x)
--
-- >>> input = Stream.fromList "hello"
-- >>> Stream.fold Fold.toList $ Stream.trace putChar $ Stream.intersperseMPrefix_ (putChar '.' >> return ',') input
-- .h.e.l.l.o"hello"
--
-- Same as 'trace_'.
--
-- /Pre-release/
--
{-# INLINE intersperseMPrefix_ #-}
intersperseMPrefix_ :: Monad m => m b -> Stream m a -> Stream m a
intersperseMPrefix_ m = mapM (\x -> void m >> return x)

------------------------------------------------------------------------------
-- Inserting Time
------------------------------------------------------------------------------

-- XXX This should be in Prelude, should we export this as a helper function?

-- | Block the current thread for specified number of seconds.
{-# INLINE sleep #-}
sleep :: MonadIO m => Double -> m ()
sleep n = liftIO $ threadDelay $ round $ n * 1000000

-- | Introduce a delay of specified seconds between elements of the stream.
--
-- Definition:
--
-- >>> sleep n = liftIO $ threadDelay $ round $ n * 1000000
-- >>> delay = Stream.intersperseM_ . sleep
--
-- Example:
--
-- >>> input = Stream.enumerateFromTo 1 3
-- >>> Stream.fold (Fold.drainMapM print) $ Stream.delay 1 input
-- 1
-- 2
-- 3
--
{-# INLINE delay #-}
delay :: MonadIO m => Double -> Stream m a -> Stream m a
delay = intersperseM_ . sleep

-- | Introduce a delay of specified seconds after consuming an element of a
-- stream.
--
-- Definition:
--
-- >>> sleep n = liftIO $ threadDelay $ round $ n * 1000000
-- >>> delayPost = Stream.intersperseMSuffix_ . sleep
--
-- Example:
--
-- >>> input = Stream.enumerateFromTo 1 3
-- >>> Stream.fold (Fold.drainMapM print) $ Stream.delayPost 1 input
-- 1
-- 2
-- 3
--
-- /Pre-release/
--
{-# INLINE delayPost #-}
delayPost :: MonadIO m => Double -> Stream m a -> Stream m a
delayPost n = intersperseMSuffix_ $ liftIO $ threadDelay $ round $ n * 1000000

-- | Introduce a delay of specified seconds before consuming an element of a
-- stream.
--
-- Definition:
--
-- >>> sleep n = liftIO $ threadDelay $ round $ n * 1000000
-- >>> delayPre = Stream.intersperseMPrefix_. sleep
--
-- Example:
--
-- >>> input = Stream.enumerateFromTo 1 3
-- >>> Stream.fold (Fold.drainMapM print) $ Stream.delayPre 1 input
-- 1
-- 2
-- 3
--
-- /Pre-release/
--
{-# INLINE delayPre #-}
delayPre :: MonadIO m => Double -> Stream m a -> Stream m a
delayPre = intersperseMPrefix_. sleep

------------------------------------------------------------------------------
-- Reordering
------------------------------------------------------------------------------

-- | Returns the elements of the stream in reverse order.  The stream must be
-- finite. Note that this necessarily buffers the entire stream in memory.
--
-- Definition:
--
-- >>> reverse m = Stream.concatEffect $ Stream.fold Fold.toListRev m >>= return . Stream.fromList
--
{-# INLINE_NORMAL reverse #-}
reverse :: Monad m => Stream m a -> Stream m a
reverse m = concatEffect $ fold FL.toListRev m <&> fromList
{-
reverse m = Stream step Nothing
    where
    {-# INLINE_LATE step #-}
    step _ Nothing = do
        xs <- foldl' (flip (:)) [] m
        return $ Skip (Just xs)
    step _ (Just (x:xs)) = return $ Yield x (Just xs)
    step _ (Just []) = return Stop
-}

-- | Like 'reverse' but several times faster, requires an 'Unbox' instance.
--
-- /O(n) space/
--
-- /Pre-release/
{-# INLINE reverseUnbox #-}
reverseUnbox :: (MonadIO m, Unbox a) => Stream m a -> Stream m a
reverseUnbox =
    A.flattenArraysRev -- unfoldMany A.readRev
        . fromStreamK
        . K.reverse
        . toStreamK
        . A.chunksOf defaultChunkSize

-- | Buffer until the next element in sequence arrives. The function argument
-- determines the difference in sequence numbers. This could be useful in
-- implementing sequenced streams, for example, TCP reassembly.
--
-- /Unimplemented/
--
{-# INLINE reassembleBy #-}
reassembleBy
    :: -- Monad m =>
       Fold m a b
    -> (a -> a -> Int)
    -> Stream m a
    -> Stream m b
reassembleBy = undefined

------------------------------------------------------------------------------
-- Position Indexing
------------------------------------------------------------------------------

-- Adapted from the vector package

-- |
-- >>> f = Fold.foldl' (\(i, _) x -> (i + 1, x)) (-1,undefined)
-- >>> indexed = Stream.postscan f
-- >>> indexed = Stream.zipWith (,) (Stream.enumerateFrom 0)
-- >>> indexedR n = fmap (\(i, a) -> (n - i, a)) . indexed
--
-- Pair each element in a stream with its index, starting from index 0.
--
-- >>> Stream.fold Fold.toList $ Stream.indexed $ Stream.fromList "hello"
-- [(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]
--
{-# INLINE_NORMAL indexed #-}
indexed :: Monad m => Stream m a -> Stream m (Int, a)
-- indexed = scanMaybe FL.indexing
indexed (Stream step state) = Stream step' (state, 0)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, i) = i `seq` do
         r <- step (adaptState gst) st
         case r of
             Yield x s -> return $ Yield (i, x) (s, i+1)
             Skip    s -> return $ Skip (s, i)
             Stop      -> return Stop

-- Adapted from the vector package

-- |
-- >>> f n = Fold.foldl' (\(i, _) x -> (i - 1, x)) (n + 1,undefined)
-- >>> indexedR n = Stream.postscan (f n)
--
-- >>> s n = Stream.enumerateFromThen n (n - 1)
-- >>> indexedR n = Stream.zipWith (,) (s n)
--
-- Pair each element in a stream with its index, starting from the
-- given index @n@ and counting down.
--
-- >>> Stream.fold Fold.toList $ Stream.indexedR 10 $ Stream.fromList "hello"
-- [(10,'h'),(9,'e'),(8,'l'),(7,'l'),(6,'o')]
--
{-# INLINE_NORMAL indexedR #-}
indexedR :: Monad m => Int -> Stream m a -> Stream m (Int, a)
-- indexedR n = scanMaybe (FL.indexingRev n)
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

-------------------------------------------------------------------------------
-- Time Indexing
-------------------------------------------------------------------------------

-- Note: The timestamp stream must be the second stream in the zip so that the
-- timestamp is generated after generating the stream element and not before.
-- If we do not do that then the following example will generate the same
-- timestamp for first two elements:
--
-- Stream.fold Fold.toList $ Stream.timestamped $ Stream.delay $ Stream.enumerateFromTo 1 3

-- | Pair each element in a stream with an absolute timestamp, using a clock of
-- specified granularity.  The timestamp is generated just before the element
-- is consumed.
--
-- >>> Stream.fold Fold.toList $ Stream.timestampWith 0.01 $ Stream.delay 1 $ Stream.enumerateFromTo 1 3
-- [(AbsTime (TimeSpec {sec = ..., nsec = ...}),1),(AbsTime (TimeSpec {sec = ..., nsec = ...}),2),(AbsTime (TimeSpec {sec = ..., nsec = ...}),3)]
--
-- /Pre-release/
--
{-# INLINE timestampWith #-}
timestampWith :: (MonadIO m)
    => Double -> Stream m a -> Stream m (AbsTime, a)
timestampWith g stream = zipWith (flip (,)) stream (absTimesWith g)

-- TBD: check performance vs a custom implementation without using zipWith.
--
-- /Pre-release/
--
{-# INLINE timestamped #-}
timestamped :: (MonadIO m)
    => Stream m a -> Stream m (AbsTime, a)
timestamped = timestampWith 0.01

-- | Pair each element in a stream with relative times starting from 0, using a
-- clock with the specified granularity. The time is measured just before the
-- element is consumed.
--
-- >>> Stream.fold Fold.toList $ Stream.timeIndexWith 0.01 $ Stream.delay 1 $ Stream.enumerateFromTo 1 3
-- [(RelTime64 (NanoSecond64 ...),1),(RelTime64 (NanoSecond64 ...),2),(RelTime64 (NanoSecond64 ...),3)]
--
-- /Pre-release/
--
{-# INLINE timeIndexWith #-}
timeIndexWith :: (MonadIO m)
    => Double -> Stream m a -> Stream m (RelTime64, a)
timeIndexWith g stream = zipWith (flip (,)) stream (relTimesWith g)

-- | Pair each element in a stream with relative times starting from 0, using a
-- 10 ms granularity clock. The time is measured just before the element is
-- consumed.
--
-- >>> Stream.fold Fold.toList $ Stream.timeIndexed $ Stream.delay 1 $ Stream.enumerateFromTo 1 3
-- [(RelTime64 (NanoSecond64 ...),1),(RelTime64 (NanoSecond64 ...),2),(RelTime64 (NanoSecond64 ...),3)]
--
-- /Pre-release/
--
{-# INLINE timeIndexed #-}
timeIndexed :: (MonadIO m)
    => Stream m a -> Stream m (RelTime64, a)
timeIndexed = timeIndexWith 0.01

------------------------------------------------------------------------------
-- Searching
------------------------------------------------------------------------------

-- | Find all the indices where the element in the stream satisfies the given
-- predicate.
--
-- >>> findIndices p = Stream.scanMaybe (Fold.findIndices p)
--
{-# INLINE_NORMAL findIndices #-}
findIndices :: Monad m => (a -> Bool) -> Stream m a -> Stream m Int
findIndices p (Stream step state) = Stream step' (state, 0)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, i) = i `seq` do
      r <- step (adaptState gst) st
      return $ case r of
          Yield x s -> if p x then Yield i (s, i+1) else Skip (s, i+1)
          Skip s -> Skip (s, i)
          Stop   -> Stop

-- | Find all the indices where the value of the element in the stream is equal
-- to the given value.
--
-- >>> elemIndices a = Stream.findIndices (== a)
--
{-# INLINE elemIndices #-}
elemIndices :: (Monad m, Eq a) => a -> Stream m a -> Stream m Int
elemIndices a = findIndices (== a)

{-# INLINE_NORMAL slicesBy #-}
slicesBy :: Monad m => (a -> Bool) -> Stream m a -> Stream m (Int, Int)
slicesBy p (Stream step1 state1) = Stream step (Just (state1, 0, 0))

    where

    {-# INLINE_LATE step #-}
    step gst (Just (st, i, len)) = i `seq` len `seq` do
      r <- step1 (adaptState gst) st
      return
        $ case r of
              Yield x s ->
                if p x
                then Yield (i, len + 1) (Just (s, i + len + 1, 0))
                else Skip (Just (s, i, len + 1))
              Skip s -> Skip (Just (s, i, len))
              Stop -> if len == 0 then Stop else Yield (i, len) Nothing
    step _ Nothing = return Stop

------------------------------------------------------------------------------
-- Rolling map
------------------------------------------------------------------------------

data RollingMapState s a = RollingMapGo s a

-- | Like 'rollingMap' but with an effectful map function.
--
-- /Pre-release/
--
{-# INLINE rollingMapM #-}
rollingMapM :: Monad m => (Maybe a -> a -> m b) -> Stream m a -> Stream m b
-- rollingMapM f = scanMaybe (FL.slide2 $ Window.rollingMapM f)
rollingMapM f (Stream step1 state1) = Stream step (RollingMapGo state1 Nothing)

    where

    step gst (RollingMapGo s1 curr) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield x s -> do
                !res <- f curr x
                return $ Yield res $ RollingMapGo s (Just x)
            Skip s -> return $ Skip $ RollingMapGo s curr
            Stop   -> return Stop

-- rollingMap is a special case of an incremental sliding fold. It can be
-- written as:
--
-- > fld f = slidingWindow 1 (Fold.foldl' (\_ (x,y) -> f y x)
-- > rollingMap f = Stream.postscan (fld f) undefined

-- | Apply a function on every two successive elements of a stream. The first
-- argument of the map function is the previous element and the second argument
-- is the current element. When the current element is the first element, the
-- previous element is 'Nothing'.
--
-- /Pre-release/
--
{-# INLINE rollingMap #-}
rollingMap :: Monad m => (Maybe a -> a -> b) -> Stream m a -> Stream m b
-- rollingMap f = scanMaybe (FL.slide2 $ Window.rollingMap f)
rollingMap f = rollingMapM (\x y -> return $ f x y)

-- | Like 'rollingMap' but requires at least two elements in the stream,
-- returns an empty stream otherwise.
--
-- This is the stream equivalent of the list idiom @zipWith f xs (tail xs)@.
--
-- /Pre-release/
--
{-# INLINE rollingMap2 #-}
rollingMap2 :: Monad m => (a -> a -> b) -> Stream m a -> Stream m b
rollingMap2 f = catMaybes . rollingMap g

    where

    g Nothing _ = Nothing
    g (Just x) y = Just (f x y)

------------------------------------------------------------------------------
-- Maybe Streams
------------------------------------------------------------------------------

-- XXX Will this always fuse properly?

-- | Map a 'Maybe' returning function to a stream, filter out the 'Nothing'
-- elements, and return a stream of values extracted from 'Just'.
--
-- Equivalent to:
--
-- >>> mapMaybe f = Stream.catMaybes . fmap f
--
{-# INLINE_NORMAL mapMaybe #-}
mapMaybe :: Monad m => (a -> Maybe b) -> Stream m a -> Stream m b
mapMaybe f = fmap fromJust . filter isJust . map f

-- | Like 'mapMaybe' but maps a monadic function.
--
-- Equivalent to:
--
-- >>> mapMaybeM f = Stream.catMaybes . Stream.mapM f
--
-- >>> mapM f = Stream.mapMaybeM (\x -> Just <$> f x)
--
{-# INLINE_NORMAL mapMaybeM #-}
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Stream m a -> Stream m b
mapMaybeM f = fmap fromJust . filter isJust . mapM f

-- | In a stream of 'Maybe's, discard 'Nothing's and unwrap 'Just's.
--
-- >>> catMaybes = Stream.mapMaybe id
-- >>> catMaybes = fmap fromJust . Stream.filter isJust
--
-- /Pre-release/
--
{-# INLINE catMaybes #-}
catMaybes :: Monad m => Stream m (Maybe a) -> Stream m a
-- catMaybes = fmap fromJust . filter isJust
catMaybes (Stream step state) = Stream step1 state

    where

    {-# INLINE_LATE step1 #-}
    step1 gst st = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                return
                    $ case x of
                        Just a -> Yield a s
                        Nothing -> Skip s
            Skip s -> return $ Skip s
            Stop -> return Stop

-- | Use a filtering fold on a stream.
--
-- >>> scanMaybe f = Stream.catMaybes . Stream.postscan f
--
{-# INLINE scanMaybe #-}
scanMaybe :: Monad m => Fold m a (Maybe b) -> Stream m a -> Stream m b
scanMaybe f = catMaybes . postscan f

------------------------------------------------------------------------------
-- Either streams
------------------------------------------------------------------------------

-- | Discard 'Right's and unwrap 'Left's in an 'Either' stream.
--
-- >>> catLefts = fmap (fromLeft undefined) . Stream.filter isLeft
--
-- /Pre-release/
--
{-# INLINE catLefts #-}
catLefts :: Monad m => Stream m (Either a b) -> Stream m a
catLefts = fmap (fromLeft undefined) . filter isLeft

-- | Discard 'Left's and unwrap 'Right's in an 'Either' stream.
--
-- >>> catRights = fmap (fromRight undefined) . Stream.filter isRight
--
-- /Pre-release/
--
{-# INLINE catRights #-}
catRights :: Monad m => Stream m (Either a b) -> Stream m b
catRights = fmap (fromRight undefined) . filter isRight

-- | Remove the either wrapper and flatten both lefts and as well as rights in
-- the output stream.
--
-- >>> catEithers = fmap (either id id)
--
-- /Pre-release/
--
{-# INLINE catEithers #-}
catEithers :: Monad m => Stream m (Either a a) -> Stream m a
catEithers = fmap (either id id)

------------------------------------------------------------------------------
-- Splitting
------------------------------------------------------------------------------

-- | Split on an infixed separator element, dropping the separator.  The
-- supplied 'Fold' is applied on the split segments.  Splits the stream on
-- separator elements determined by the supplied predicate, separator is
-- considered as infixed between two segments:
--
-- >>> splitOn' p xs = Stream.fold Fold.toList $ Stream.splitOn p Fold.toList (Stream.fromList xs)
-- >>> splitOn' (== '.') "a.b"
-- ["a","b"]
--
-- An empty stream is folded to the default value of the fold:
--
-- >>> splitOn' (== '.') ""
-- [""]
--
-- If one or both sides of the separator are missing then the empty segment on
-- that side is folded to the default output of the fold:
--
-- >>> splitOn' (== '.') "."
-- ["",""]
--
-- >>> splitOn' (== '.') ".a"
-- ["","a"]
--
-- >>> splitOn' (== '.') "a."
-- ["a",""]
--
-- >>> splitOn' (== '.') "a..b"
-- ["a","","b"]
--
-- splitOn is an inverse of intercalating single element:
--
-- > Stream.intercalate (Stream.fromPure '.') Unfold.fromList . Stream.splitOn (== '.') Fold.toList === id
--
-- Assuming the input stream does not contain the separator:
--
-- > Stream.splitOn (== '.') Fold.toList . Stream.intercalate (Stream.fromPure '.') Unfold.fromList === id
--
{-# INLINE splitOn #-}
splitOn :: Monad m => (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
splitOn predicate f =
    -- We can express the infix splitting in terms of optional suffix split
    -- fold.  After applying a suffix split fold repeatedly if the last segment
    -- ends with a suffix then we need to return the default output of the fold
    -- after that to make it an infix split.
    --
    -- Alternately, we can also express it using an optional prefix split fold.
    -- If the first segment starts with a prefix then we need to emit the
    -- default output of the fold before that to make it an infix split, and
    -- then apply prefix split fold repeatedly.
    --
    -- Since a suffix split fold can be easily expressed using a
    -- non-backtracking fold, we use that.
    foldManyPost (FL.takeEndBy_ predicate f)
