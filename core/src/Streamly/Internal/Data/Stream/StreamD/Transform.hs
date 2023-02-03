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

    -- * Folding
    , foldrS
    , foldlS

    -- * Scanning By 'Fold'
    , postscan
    , scan
    , scanMany

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
    , scanMaybe
    , filter
    , filterM
    , deleteBy
    , uniq

    -- * Trimming
    -- | Produce a subset of the stream trimmed at ends.
    , take
    , takeWhile
    , takeWhileM
    , drop
    , dropWhile
    , dropWhileM

    -- * Inserting Elements
    -- | Produce a superset of the stream.
    , insertBy
    , intersperse
    , intersperseM
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

{-# INLINE scan #-}
scan :: Monad m
    => FL.Fold m a b -> Stream m a -> Stream m b
scan = scanWith False

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
    initial `consM` postscanlMAfter' fstep initial done s

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

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

-- Adapted from the vector package
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

-- Adapted from the vector package
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
-- Trimming
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- Inserting Elements
------------------------------------------------------------------------------

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

{-# INLINE intersperse #-}
intersperse :: Monad m => a -> Stream m a -> Stream m a
intersperse a = intersperseM (return a)

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

-- | intersperse after every n items
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
        . A.arraysOf defaultChunkSize

------------------------------------------------------------------------------
-- Position Indexing
------------------------------------------------------------------------------

-- Adapted from the vector package
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

-- Adapted from the vector package
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

-------------------------------------------------------------------------------
-- Time Indexing
-------------------------------------------------------------------------------

-- Note: The timestamp stream must be the second stream in the zip so that the
-- timestamp is generated after generating the stream element and not before.
-- If we do not do that then the following example will generate the same
-- timestamp for first two elements:
--
-- Stream.fold Fold.toList $ Stream.timestamped $ Stream.delay $ Stream.enumerateFromTo 1 3
--
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
-- /Pre-release/Monad
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

{-# INLINE rollingMapM #-}
rollingMapM :: Monad m => (Maybe a -> a -> m b) -> Stream m a -> Stream m b
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

-- | rollingMap is a special case of an incremental sliding fold. It can be
-- written as:
--
-- > fld f = slidingWindow 1 (Fold.foldl' (\_ (x,y) -> f y x)
-- > rollingMap f = Stream.postscan (fld f) undefined
--
{-# INLINE rollingMap #-}
rollingMap :: Monad m => (Maybe a -> a -> b) -> Stream m a -> Stream m b
rollingMap f = rollingMapM (\x y -> return $ f x y)

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
{-# INLINE_NORMAL mapMaybe #-}
mapMaybe :: Monad m => (a -> Maybe b) -> Stream m a -> Stream m b
mapMaybe f = fmap fromJust . filter isJust . map f

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
