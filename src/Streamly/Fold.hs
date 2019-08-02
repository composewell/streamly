{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Streamly.Fold
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- A left fold consumes a stream and reduces it to a single value.  Using the
-- fold combinators in "Streamly.Prelude" module only one fold operation can be
-- applied to a stream. This module provides a 'Fold' type that represents a
-- left fold. Multiple such folds can be combined in different ways; a stream
-- can then be supplied to the combined fold.
-- For example, a distributive applicative composition distributes the input to
-- the constituent folds and then combines the fold outputs.  Similarly, a
-- partitioning composition can partition the input among constituent folds.
-- All the combinators in this module are of true streaming nature, stream
-- elements are not unnecessarily buffered in memory, guaranteeing a constant
-- memory consumption.
--
-- Consider this module as the consumer side dual of the "Streamly.Prelude"
-- module.  "Streamly.Prelude" provides combinators that can combine stream
-- sources in interesting ways whereas this module provides combinators that
-- combine stream consumers in interesting ways. In other words,
-- "Streamly.Prelude" provides stream merging capabilities while
-- "Streamly.Fold" provides stream splitting capabilities.  Both the modules
-- are organized in the same way so that you can easily find the corresponding
-- operations.
--
-- > import qualified Streamly.Fold as FL
--
-- A left fold is represented by the type 'Fold'. @Fold m a b@ folds an
-- input stream consisting of values of type @a@ to a structure of type
-- @b@. The fold can be run using the 'foldl'' combinator and an input stream.
--
-- >>> FL.foldl' FL.sum (S.enumerateFromTo 1 100)
-- 5050

-- Also see the "Streamly.Sink" module that provides specialized left folds
-- that discard the outputs.
--
-- IMPORTANT: keep the signatures consistent with the folds in Streamly.Prelude

module Streamly.Fold
    (
    -- * Fold Type
      Fold -- (..)

    -- * Folding
    -- $termination

    , foldl'

    -- , tail
    -- , init

    -- ** Full Folds
    , drain
    , drainBy
    , last
    , length
    , sum
    , product
    , maximumBy
    , maximum
    , minimumBy
    , minimum
    -- , the
    , mean
    , variance
    , stdDev

    -- ** Full Folds (Monoidal)
    , mconcat
    , foldMap
    , foldMapM

    -- ** Full Folds (To Containers)
    -- | Avoid using these folds in scalable or performance critical
    -- applications, they buffer all the input in GC memory which can be
    -- detrimental to performance if the input is large.

    -- , toStream  -- experimental
    -- , toStreamRev  -- experimental

    , toList
    -- , toListRev  -- experimental

    -- ** Partial Folds
    -- , drainN
    -- , drainWhile
    -- , (!!)
    -- , genericIndex
    , index
    , head
    -- , findM
    , find
    , lookup
    , findIndex
    , elemIndex
    , null
    , elem
    , notElem
    -- XXX these are slower than right folds even when full input is used
    , all
    , any
    , and
    , or

    -- * Transformations
    -- | Unlike stream producer types (e.g. @SerialT m a@) which have only
    -- output side, folds have an input side as well as an output side.  In the
    -- type @Fold m a b@, the input type is @a@ and the output type is @b@.
    -- Transformations can be applied either on the input side or on the output
    -- side. The 'Functor' instance of a fold maps on the output of the fold:
    --
    -- >>> FL.foldl' (fmap show FL.sum) (S.enumerateFromTo 1 100)
    -- "5050"
    --
    -- However, the input side or contravariant transformations are more
    -- interesting for folds.  The following sections describe the input
    -- transformation operations on a fold.  The names of the operations are
    -- consistent with their covariant counterparts in "Streamly.Prelude", the
    -- only difference is that they are prefixed with 'l' which stands for
    -- 'left' assuming left side is the input side, notice that in @Fold m a b@
    -- the type variable @a@ is on the left side.

    -- ** Covariant Operations
    , sequence
    , mapM

    -- ** Mapping
    --, transform
    , lmap
    --, lsequence
    , lmapM

    -- ** Scanning
    , scanl'
    , postscanl'

    -- , lscanl'
    -- , lscanlM'
    -- , lscanl1'
    -- , lscanl1M'
    --
    -- , lpostscanl'
    -- , lpostscanlM'
    -- , lprescanl'
    -- , lprescanlM'

    -- -- ** Filtering
    -- , lfilter
    -- , lfilterM
    -- , ldeleteBy
    -- , luniq

    {-
    -- ** Mapping Filters
    , lmapMaybe
    , lmapMaybeM

    -- ** Scanning Filters
    , lfindIndices
    , lelemIndices

    -- ** Insertion
    -- | Insertion adds more elements to the stream.

    , linsertBy
    , lintersperseM

    -- ** Reordering
    , lreverse
    -}

    -- * Parsing
    -- ** Trimming
    , ltake
    -- , lrunFor -- time
    , ltakeWhile
    {-
    , ltakeWhileM
    , ldrop
    , ldropWhile
    , ldropWhileM
    -}

    -- ** Breaking

    -- By chunks
    -- , splitAt -- spanN
    -- , splitIn -- sessionN

    -- By elements
    -- , span  -- spanWhile
    -- , break -- breakBefore
    -- , breakAfter
    -- , breakOn
    -- , breakAround
    -- , spanBy
    -- , spanByRolling

    -- By sequences
    -- breakOnSeq

    -- ** Splitting
    -- | Streams can be split into segments in space or in time. We use the
    -- term @chunk@ to refer to a spatial length of the stream (spatial window)
    -- and the term @session@ to refer to a length in time (time window).

    -- In imperative terms, grouped folding can be considered as a nested loop
    -- where we loop over the stream to group elements and then loop over
    -- individual groups to fold them to a single value that is yielded in the
    -- output stream.

    -- , groupScan

    -- *** Chunks
    , chunksOf
    , sessionsOf

    -- , lchunksOf
    -- , lsessionsOf

    -- *** Using Element Separators
    -- On == Dropping the separator
    , splitOn
    , splitOnSuffix
    -- , splitOnPrefix

    -- By == Keeping the separator
    -- , splitBy
    , splitBySuffix
    -- , splitByPrefix
    , wordsBy -- stripAndCompactBy

    -- -- *** Using Sequence Separators
    -- , splitOnSeq
    -- , splitOnSuffixSeq
    -- , splitOnPrefixSeq

    -- Keeping the delimiters
    -- , splitBySeq
    -- , splitBySeqSuffix
    -- , splitBySeqPrefix
    -- , wordsBySeq

    -- Splitting using multiple sequence separators
    -- , splitOnAnySeq
    -- , splitOnAnySuffixSeq
    -- , splitOnAnyPrefixSeq

    -- ** Grouping
    , groups
    , groupsBy
    , groupsByRolling

    -- * Distributing
    -- |
    -- The 'Applicative' instance of a distributing 'Fold' distributes one copy
    -- of the stream to each fold and combines the results using a function.
    --
    -- @
    --
    --                 |-------Fold m a b--------|
    -- ---stream m a---|                         |---m (b,c,...)
    --                 |-------Fold m a c--------|
    --                 |                         |
    --                            ...
    -- @
    --
    -- To compute the average of numbers in a stream without going throught he
    -- stream twice:
    --
    -- >>> let avg = (/) <$> FL.sum <*> fmap fromIntegral FL.length
    -- >>> FL.foldl' avg (S.enumerateFromTo 1.0 100.0)
    -- 50.5
    --
    -- The 'Semigroup' and 'Monoid' instances of a distributing fold distribute
    -- the input to both the folds and combines the outputs using Monoid or
    -- Semigroup instances of the output types:
    --
    -- >>> import Data.Monoid (Sum)
    -- >>> FL.foldl' (FL.head <> FL.last) (fmap Sum $ S.enumerateFromTo 1.0 100.0)
    -- Just (Sum {getSum = 101.0})
    --
    -- The 'Num', 'Floating', and 'Fractional' instances work in the same way.

    , tee
    , distribute

    -- * Partitioning
    -- |
    -- Direct items in the input stream to different folds using a binary
    -- fold selector.

    -- , partitionByM
    -- , partitionBy
    , partition

    -- * Demultiplexing
    -- | Direct values in the input stream to different folds using an n-ary
    -- fold selector.

    , demux
    -- , demuxWith
    , demux_
    -- , demuxWith_

    -- * Classifying
    -- | In an input stream of key value pairs fold values for different keys
    -- in individual output buckets using the given fold.

    , classify
    -- , classifyWith

    -- * Unzipping
    , unzip
    -- These can be expressed using lmap/lmapM and unzip
    -- , unzipWith
    -- , unzipWithM

    -- -- * Nested Folds
    -- , concatMap
    -- , chunksOf
    -- , duplicate  -- experimental

    {-
    -- * Windowed Classification
    -- | Split the stream into windows or chunks in space or time. Each window
    -- can be associated with a key, all events associated with a particular
    -- key in the window can be folded to a single result. The stream is split
    -- into windows of specified size, the window can be terminated early if
    -- the closing flag is specified in the input stream.
    --
    -- The term "chunk" is used for a space window and the term "session" is
    -- used for a time window.

    -- ** Tumbling Windows
    -- | A new window starts after the previous window is finished.
    -- , classifyChunksOf
    -- , classifySessionsOf

    -- ** Keep Alive Windows
    -- | The window size is extended if an event arrives within the specified
    -- window size. This can represent sessions with idle or inactive timeout.
    -- , classifyKeepAliveChunks
    -- , classifyKeepAliveSessions

    {-
    -- ** Sliding Windows
    -- | A new window starts after the specified slide from the previous
    -- window. Therefore windows can overlap.
    , classifySlidingChunks
    , classifySlidingSessions
    -}
    -- ** Sliding Window Buffers
    -- , slidingChunkBuffer
    -- , slidingSessionBuffer
    -}
    )
where

import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception (SomeException(..), catch, mask)
import Control.Monad (void)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (control)
import Data.Functor.Identity (Identity)
import Data.Heap (Entry(..))
import Data.Map.Strict (Map)
import Data.Maybe (fromJust, isJust, isNothing)

import Foreign.Storable (Storable(..))
import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, replicate, concatMap, mconcat, foldMap, unzip,
               span, splitAt, break, mapM)

import qualified Data.Heap as H
import qualified Data.Map.Strict as Map
import qualified Prelude

import Streamly (MonadAsync, parallel)
import Streamly.Fold.Types (Fold(..))
import Streamly.Pipe.Types (Pipe (..), PipeState(..))
import Streamly.Mem.Array.Types (Array)
-- import Streamly.Mem.Ring (Ring)
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK (IsStream())
import Streamly.Time.Units
       (AbsTime, MilliSecond64(..), addToAbsTime, diffAbsTime, toRelTime,
       toAbsTime)

import Streamly.Strict

import qualified Streamly.Pipe.Types as Pipe
import qualified Streamly.Mem.Array.Types as A
import qualified Streamly.Prelude as S
import qualified Streamly.Streams.StreamD as D
import qualified Streamly.Streams.StreamK as K
import qualified Streamly.Streams.Prelude as P
import qualified Streamly.Streams.Parallel as Par

-- $termination
--
-- We can use the left folds in this module instead of the folds in
-- "Streamly.Prelude". For example the following two ways of folding are
-- equivalent in functionality and performance,
--
-- >>> FL.foldl' FL.sum (S.enumerateFromTo 1 100)
-- 5050
-- >>> S.sum (S.enumerateFromTo 1 100)
-- 5050
--
-- However, left folds are push type folds. That means we push the entire input
-- to a fold before we can get the output.  Therefore, the performance is
-- equivalent only for full folds like 'sum' and 'length'. For partial folds
-- like 'head' or 'any' the folds in "Streamly.Prelude" may be much more
-- efficient because they are implemented as right folds that terminate as soon
-- as we get the result. Note that when a full fold is composed with a partial
-- fold in parallel the performance is not impacted as we anyway have to
-- consume the whole stream due to the full fold.
--
-- >>> S.head (1 `S.cons` undefined)
-- Just 1
-- >>> FL.foldl' FL.head (1 `S.cons` undefined)
-- *** Exception: Prelude.undefined
--
-- However, we can wrap the fold in a scan to convert it into a lazy stream of
-- fold steps. We can then terminate the stream whenever we want.  For example,
--
-- >>> S.toList $ S.take 1 $ FL.scanl' FL.head (1 `S.cons` undefined)
-- [Nothing]
--
-- The following example extracts the input stream up to a point where the
-- running average of elements is no more than 10:
--
-- @
-- > S.toList
--   $ S.map (fromJust . fst)
--   $ S.takeWhile (\\(_,x) -> x <= 10)
--   $ FL.postscanl' ((,) \<$> FL.last \<*> avg) (S.enumerateFromTo 1.0 100.0)
-- @
-- @
--  [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0]
-- @

------------------------------------------------------------------------------
-- Running a Fold
------------------------------------------------------------------------------

-- XXX rename to foldStream to be consistent with foldArray for arrays? Should
-- we move it in the stream module (Prelude as of now) or should we move
-- foldArray as well here?

-- | Fold a stream using the supplied monadic fold.
--
-- >>> FL.foldl' FL.sum (S.enumerateFromTo 1 100)
-- 5050
--
-- @since 0.7.0
{-# INLINE foldl' #-}
foldl' :: Monad m => Fold m a b -> SerialT m a -> m b
foldl' (Fold step begin done) = P.foldlMx' step begin done

------------------------------------------------------------------------------
-- Transformations on fold inputs
------------------------------------------------------------------------------

-- | Flatten the monadic output of a fold to pure output.
--
-- @since 0.7.0
{-# INLINE sequence #-}
sequence :: Monad m => Fold m a (m b) -> Fold m a b
sequence (Fold step initial extract) = Fold step initial extract'
  where
    extract' x = do
        act <- extract x
        act >>= return

-- | Map a monadic function on the output of a fold.
--
-- @since 0.7.0
{-# INLINE mapM #-}
mapM :: Monad m => (b -> m c) -> Fold m a b -> Fold m a c
mapM f = sequence . fmap f

------------------------------------------------------------------------------
-- Transformations on fold inputs
------------------------------------------------------------------------------

-- | Apply a transformation on a 'Fold' using a 'Pipe'.
--
-- @since 0.7.0
{-# INLINE _transform #-}
_transform :: Monad m => Pipe m a b -> Fold m b c -> Fold m a c
_transform (Pipe pstep1 pstep2 pinitial) (Fold fstep finitial fextract) =
    Fold step initial extract

    where

    initial = Tuple' <$> return pinitial <*> finitial
    step (Tuple' ps fs) x = do
        r <- pstep1 ps x
        go fs r

        where
        -- XXX use SPEC?
        go acc (Pipe.Yield b (Consume ps')) = do
            acc' <- fstep acc b
            return (Tuple' ps' acc')

        go acc (Pipe.Yield b (Produce ps')) = do
            acc' <- fstep acc b
            r <- pstep2 ps'
            go acc' r

        go acc (Pipe.Continue (Consume ps')) = return (Tuple' ps' acc)

        go acc (Pipe.Continue (Produce ps')) = do
            r <- pstep2 ps'
            go acc r

    extract (Tuple' _ fs) = fextract fs

-- | @(lmap f fold)@ maps the function @f@ on the input of the fold.
--
-- >>> FL.foldl' (FL.lmap (\x -> x * x) FL.sum) (S.enumerateFromTo 1 100)
-- 338350
--
-- @since 0.7.0
{-# INLINABLE lmap #-}
lmap :: (a -> b) -> Fold m b r -> Fold m a r
lmap f (Fold step begin done) = Fold step' begin done
  where
    step' x a = step x (f a)

-- | @(lmapM f fold)@ maps the monadic function @f@ on the input of the fold.
--
-- @since 0.7.0
{-# INLINABLE lmapM #-}
lmapM :: Monad m => (a -> m b) -> Fold m b r -> Fold m a r
lmapM f (Fold step begin done) = Fold step' begin done
  where
    step' x a = f a >>= step x

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- | @_Fold1 step@ returns a new 'Fold' using just a step function that has the
-- same type for the accumulator and the element. The result type is the
-- accumulator type wrapped in 'Maybe'. The initial accumulator is retrieved
-- from the 'Foldable', the result is 'None' for empty containers.
{-# INLINABLE _Fold1 #-}
_Fold1 :: Monad m => (a -> a -> a) -> Fold m a (Maybe a)
_Fold1 step = Fold step_ (return Nothing') fromStrictMaybe
  where
    step_ mx a = return $ Just' $
        case mx of
            Nothing' -> a
            Just' x -> step x a

------------------------------------------------------------------------------
-- Left folds
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Run Effects
------------------------------------------------------------------------------

-- | A fold that drains all its input, running the effects and discarding the
-- results.
--
-- @since 0.7.0
{-# INLINABLE drain #-}
drain :: Monad m => Fold m a ()
drain = Fold step begin done
    where
    begin = return ()
    step _ _ = return ()
    done = return

-- |
-- > drainBy f = lmapM f drain
--
-- Drain all input after passing it through a monadic function. This is the
-- dual of mapM_ on stream producers.
--
-- @since 0.7.0
{-# INLINABLE drainBy #-}
drainBy ::  Monad m => (a -> m b) -> Fold m a ()
drainBy f = Fold (const (void . f)) (return ()) return

-- | Extract the last element of the input stream, if any.
--
-- @since 0.7.0
{-# INLINABLE last #-}
last :: Monad m => Fold m a (Maybe a)
last = _Fold1 (flip const)

------------------------------------------------------------------------------
-- To Summary
------------------------------------------------------------------------------

-- | Like 'length', except with a more general 'Num' return value
--
-- @since 0.7.0
{-# INLINABLE genericLength #-}
genericLength :: (Monad m, Num b) => Fold m a b
genericLength = Fold (\n _ -> return $ n + 1) (return 0) return

-- | Determine the length of the input stream.
--
-- @since 0.7.0
{-# INLINABLE length #-}
length :: Monad m => Fold m a Int
length = genericLength

-- | Determine the sum of all elements of a stream of numbers. Returns additive
-- identity (@0@) when the stream is empty. Note that this is not numerically
-- stable for floating point numbers.
--
-- @since 0.7.0
{-# INLINABLE sum #-}
sum :: (Monad m, Num a) => Fold m a a
sum = Fold (\x a -> return $ x + a) (return 0) return

-- | Determine the product of all elements of a stream of numbers. Returns
-- multiplicative identity (@1@) when the stream is empty.
--
-- @since 0.7.0
{-# INLINABLE product #-}
product :: (Monad m, Num a) => Fold m a a
product = Fold (\x a -> return $ x * a) (return 1) return

------------------------------------------------------------------------------
-- To Summary (Maybe)
------------------------------------------------------------------------------

-- | Determine the maximum element in a stream using the supplied comparison
-- function.
--
-- @since 0.7.0
{-# INLINABLE maximumBy #-}
maximumBy :: Monad m => (a -> a -> Ordering) -> Fold m a (Maybe a)
maximumBy cmp = _Fold1 max'
  where
    max' x y = case cmp x y of
        GT -> x
        _  -> y

-- |
-- @
-- maximum = 'maximumBy' compare
-- @
--
-- Determine the maximum element in a stream.
--
-- @since 0.7.0
{-# INLINABLE maximum #-}
maximum :: (Monad m, Ord a) => Fold m a (Maybe a)
maximum = _Fold1 max

-- | Computes the minimum element with respect to the given comparison function
--
-- @since 0.7.0
{-# INLINABLE minimumBy #-}
minimumBy :: Monad m => (a -> a -> Ordering) -> Fold m a (Maybe a)
minimumBy cmp = _Fold1 min'
  where
    min' x y = case cmp x y of
        GT -> y
        _  -> x

-- | Determine the minimum element in a stream using the supplied comparison
-- function.
--
-- @since 0.7.0
{-# INLINABLE minimum #-}
minimum :: (Monad m, Ord a) => Fold m a (Maybe a)
minimum = _Fold1 min

------------------------------------------------------------------------------
-- To Summary (Statistical)
------------------------------------------------------------------------------

-- | Compute a numerically stable arithmetic mean of all elements in the input
-- stream.
--
-- @since 0.7.0
{-# INLINABLE mean #-}
mean :: (Monad m, Fractional a) => Fold m a a
mean = Fold step (return begin) (return . done)
  where
    begin = Tuple' 0 0
    step (Tuple' x n) y = return $
        let n' = n + 1
        in Tuple' (x + (y - x) / n') n'
    done (Tuple' x _) = x

-- | Compute a numerically stable (population) variance over all elements in
-- the input stream.
--
-- @since 0.7.0
{-# INLINABLE variance #-}
variance :: (Monad m, Fractional a) => Fold m a a
variance = Fold step (return begin) (return . done)
  where
    begin = Tuple3' 0 0 0

    step (Tuple3' n mean_ m2) x = return $ Tuple3' n' mean' m2'
      where
        n'     = n + 1
        mean'  = (n * mean_ + x) / (n + 1)
        delta  = x - mean_
        m2'    = m2 + delta * delta * n / (n + 1)

    done (Tuple3' n _ m2) = m2 / n

-- | Compute a numerically stable (population) standard deviation over all
-- elements in the input stream.
--
-- @since 0.7.0
{-# INLINABLE stdDev #-}
stdDev :: (Monad m, Floating a) => Fold m a a
stdDev = sqrt variance

------------------------------------------------------------------------------
-- Monoidal left folds
------------------------------------------------------------------------------

-- | Fold an input stream consisting of monoidal elements using 'mappend'
-- and 'mempty'.
--
-- > FL.foldl FL.mconcat (S.map Sum $ S.enumerateFromTo 1 10)
--
-- @since 0.7.0
{-# INLINABLE mconcat #-}
mconcat :: (Monad m, Monoid a) => Fold m a a
mconcat = Fold (\x a -> return $ mappend x a) (return mempty) return

-- |
-- > foldMap f = map f mconcat
--
-- Make a fold from a pure function that folds the output of the function
-- using 'mappend' and 'mempty'.
--
-- > FL.foldl (FL.foldMap Sum) $ S.enumerateFromTo 1 10
--
-- @since 0.7.0
{-# INLINABLE foldMap #-}
foldMap :: (Monad m, Monoid b) => (a -> b) -> Fold m a b
foldMap f = lmap f mconcat

-- |
-- > foldMapM f = mapM f mconcat
--
-- Make a fold from a monadic function that folds the output of the function
-- using 'mappend' and 'mempty'.
--
-- > FL.foldM (FL.foldMapM (return . Sum)) $ S.enumerateFromTo 1 10
--
-- @since 0.7.0
{-# INLINABLE foldMapM #-}
foldMapM ::  (Monad m, Monoid b) => (a -> m b) -> Fold m a b
foldMapM act = Fold step begin done
    where
    done = return
    begin = return mempty
    step m a = do
        m' <- act a
        return $! mappend m m'

------------------------------------------------------------------------------
-- To Containers
------------------------------------------------------------------------------

-- | Folds the input stream to a list.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Array" instead.
--
-- @since 0.7.0

-- id . (x1 :) . (x2 :) . (x3 :) . ... . (xn :) $ []
{-# INLINABLE toList #-}
toList :: Monad m => Fold m a [a]
toList = Fold (\f x -> return $ f . (x :))
              (return id)
              (return . ($ []))

-- This is more efficient than 'toList'. toList is exactly the same as
-- reversing the list after toListRev.
--
-- | Buffers the input stream to a list in the reverse order of the input.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Array" instead.
--
-- @since 0.7.0

--  xn : ... : x2 : x1 : []
{-# INLINABLE _toListRev #-}
_toListRev :: Monad m => Fold m a [a]
_toListRev = Fold (\xs x -> return $ x:xs) (return []) return

-- | A fold that buffers its input to a pure stream.
--
-- /Warning!/ working on large streams accumulated as buffers in memory could
-- be very inefficient, consider using "Streamly.Array" instead.
--
-- @since 0.7.0
{-# INLINE _toStream #-}
_toStream :: Monad m => Fold m a (SerialT Identity a)
_toStream = Fold (\f x -> return $ f . (x `K.cons`))
                (return id)
                (return . ($ K.nil))

-- This is more efficient than 'toStream'. toStream is exactly the same as
-- reversing the stream after toStreamRev.
--
-- | Buffers the input stream to a pure stream in the reverse order of the
-- input.
--
-- /Warning!/ working on large streams accumulated as buffers in memory could
-- be very inefficient, consider using "Streamly.Array" instead.
--
-- @since 0.7.0

--  xn : ... : x2 : x1 : []
{-# INLINABLE _toStreamRev #-}
_toStreamRev :: Monad m => Fold m a (SerialT Identity a)
_toStreamRev = Fold (\xs x -> return $ x `S.cons` xs) (return S.nil) return

------------------------------------------------------------------------------
-- Partial Folds
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- To Elements
------------------------------------------------------------------------------

-- | Like 'index', except with a more general 'Integral' argument
--
-- @since 0.7.0
{-# INLINABLE genericIndex #-}
genericIndex :: (Integral i, Monad m) => i -> Fold m a (Maybe a)
genericIndex i = Fold step (return $ Left' 0) done
  where
    step x a = return $
        case x of
            Left'  j -> if i == j
                        then Right' a
                        else Left' (j + 1)
            _        -> x
    done x = return $
        case x of
            Left'  _ -> Nothing
            Right' a -> Just a

-- | Lookup the element at the given index.
--
-- @since 0.7.0
{-# INLINABLE index #-}
index :: Monad m => Int -> Fold m a (Maybe a)
index = genericIndex

-- | Extract the first element of the stream, if any.
--
-- @since 0.7.0
{-# INLINABLE head #-}
head :: Monad m => Fold m a (Maybe a)
head = _Fold1 const

-- | Returns the first element that satisfies the given predicate.
--
-- @since 0.7.0
{-# INLINABLE find #-}
find :: Monad m => (a -> Bool) -> Fold m a (Maybe a)
find predicate = Fold step (return Nothing') fromStrictMaybe
  where
    step x a = return $
        case x of
            Nothing' -> if predicate a
                        then Just' a
                        else Nothing'
            _        -> x

-- | In a stream of (key-value) pairs @(a, b)@, return the value @b@ of the
-- first pair where the key equals the given value @a@.
--
-- @since 0.7.0
{-# INLINABLE lookup #-}
lookup :: (Eq a, Monad m) => a -> Fold m (a,b) (Maybe b)
lookup a0 = Fold step (return Nothing') fromStrictMaybe
  where
    step x (a,b) = return $
        case x of
            Nothing' -> if a == a0
                        then Just' b
                        else Nothing'
            _ -> x

-- | Convert strict 'Either'' to lazy 'Maybe'
{-# INLINABLE hush #-}
hush :: Either' a b -> Maybe b
hush (Left'  _) = Nothing
hush (Right' b) = Just b

-- | Returns the first index that satisfies the given predicate.
--
-- @since 0.7.0
{-# INLINABLE findIndex #-}
findIndex :: Monad m => (a -> Bool) -> Fold m a (Maybe Int)
findIndex predicate = Fold step (return $ Left' 0) (return . hush)
  where
    step x a = return $
        case x of
            Left' i ->
                if predicate a
                then Right' i
                else Left' (i + 1)
            _       -> x

-- | Returns the first index where a given value is found in the stream.
--
-- @since 0.7.0
{-# INLINABLE elemIndex #-}
elemIndex :: (Eq a, Monad m) => a -> Fold m a (Maybe Int)
elemIndex a = findIndex (a ==)

------------------------------------------------------------------------------
-- To Boolean
------------------------------------------------------------------------------

-- | Return 'True' if the input stream is empty.
--
-- @since 0.7.0
{-# INLINABLE null #-}
null :: Monad m => Fold m a Bool
null = Fold (\_ _ -> return False) (return True) return

-- |
-- > any p = lmap p or
--
-- | Returns 'True' if any of the elements of a stream satisfies a predicate.
--
-- @since 0.7.0
{-# INLINABLE any #-}
any :: Monad m => (a -> Bool) -> Fold m a Bool
any predicate = Fold (\x a -> return $ x || predicate a) (return False) return

-- | Return 'True' if the given element is present in the stream.
--
-- @since 0.7.0
{-# INLINABLE elem #-}
elem :: (Eq a, Monad m) => a -> Fold m a Bool
elem a = any (a ==)

-- |
-- > all p = lmap p and
--
-- | Returns 'True' if all elements of a stream satisfy a predicate.
--
-- @since 0.7.0
{-# INLINABLE all #-}
all :: Monad m => (a -> Bool) -> Fold m a Bool
all predicate = Fold (\x a -> return $ x && predicate a) (return True) return

-- | Returns 'True' if the given element is not present in the stream.
--
-- @since 0.7.0
{-# INLINABLE notElem #-}
notElem :: (Eq a, Monad m) => a -> Fold m a Bool
notElem a = all (a /=)

-- | Returns 'True' if all elements are 'True', 'False' otherwise
--
-- @since 0.7.0
{-# INLINABLE and #-}
and :: Monad m => Fold m Bool Bool
and = Fold (\x a -> return $ x && a) (return True) return

-- | Returns 'True' if any element is 'True', 'False' otherwise
--
-- @since 0.7.0
{-# INLINABLE or #-}
or :: Monad m => Fold m Bool Bool
or = Fold (\x a -> return $ x || a) (return False) return

------------------------------------------------------------------------------
-- Scanning with a Fold
------------------------------------------------------------------------------

-- | Scan a stream using the given monadic fold.
--
-- @since 0.7.0
{-# INLINE scanl' #-}
scanl' :: Monad m => Fold m a b -> SerialT m a -> SerialT m b
scanl' (Fold step begin done) = P.scanlMx' step begin done

-- | Postscan a stream using the given monadic fold.
--
-- @since 0.7.0
{-# INLINE postscanl' #-}
postscanl' :: Monad m => Fold m a b -> SerialT m a -> SerialT m b
postscanl' (Fold step begin done) = P.postscanlMx' step begin done

-- XXX toPrescanl

------------------------------------------------------------------------------
-- Filtering
------------------------------------------------------------------------------

-- | Include only those elements that pass a predicate.
--
-- >>> FL.foldl (lfilter (> 5) FL.sum) [1..10]
-- 40
--
-- @since 0.7.0
{-# INLINABLE lfilter #-}
lfilter :: Monad m => (a -> Bool) -> Fold m a r -> Fold m a r
lfilter f (Fold step begin done) = Fold step' begin done
  where
    step' x a = if f a then step x a else return x

-- | Like 'lfilter' but with a monadic predicate.
--
-- @since 0.7.0
{-# INLINABLE _lfilterM #-}
_lfilterM :: Monad m => (a -> m Bool) -> Fold m a r -> Fold m a r
_lfilterM f (Fold step begin done) = Fold step' begin done
  where
    step' x a = do
      use <- f a
      if use then step x a else return x

-- | Take first 'n' elements from the stream and discard the rest.
--
-- @since 0.7.0
{-# INLINABLE ltake #-}
ltake :: Monad m => Int -> Fold m a b -> Fold m a b
ltake n (Fold step initial done) = Fold step' initial' done'
    where
    initial' = fmap (Tuple' 0) initial
    step' (Tuple' i r) a = do
        if i < n
        then do
            res <- step r a
            return $ Tuple' (i + 1) res
        else return $ Tuple' i r
    done' (Tuple' _ r) = done r

-- | Takes elements from the input as long as the predicate succeeds.
--
-- @since 0.7.0
{-# INLINABLE ltakeWhile #-}
ltakeWhile :: Monad m => (a -> Bool) -> Fold m a b -> Fold m a b
ltakeWhile predicate (Fold step initial done) = Fold step' initial' done'
    where
    initial' = fmap Left' initial
    step' (Left' r) a = do
        if predicate a
        then fmap Left' $ step r a
        else return (Right' r)
    step' r _ = return r
    done' (Left' r) = done r
    done' (Right' r) = done r

------------------------------------------------------------------------------
-- Grouping/Splitting
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Grouping without looking at elements
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------
--

-- | @splitAt n f1 f2@ composes folds @f1@ and @f2@ such that first @n@
-- elements of its input are consumed by fold @f1@ and the rest of the stream
-- is consumed by fold @f2@.
--
-- > let splitAt_ n xs = FL.foldl' (FL.splitAt n FL.toList FL.toList) $ S.fromList xs
--
-- >>> splitAt_ 6 "Hello World!"
-- > ("Hello ","World!")
--
-- >>> splitAt_ (-1) [1,2,3]
-- > ([],[1,2,3])
--
-- >>> splitAt_ 0 [1,2,3]
-- > ([],[1,2,3])
--
-- >>> splitAt_ 1 [1,2,3]
-- > ([1],[2,3])
--
-- >>> splitAt_ 3 [1,2,3]
-- > ([1,2,3],[])
--
-- >>> splitAt_ 4 [1,2,3]
-- > ([1,2,3],[])
--
-- This can be considered as a two-fold version of 'ltake' where we take both
-- the segments instead of discarding the leftover.
--
-- @since 0.7.0
{-# INLINE _splitAt #-}
_splitAt
    :: Monad m
    => Int
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
_splitAt n (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step init extract
    where
      init  = Tuple3' <$> return n <*> initialL <*> initialR

      step (Tuple3' i xL xR) input =
        if i > 0
        then stepL xL input >>= (\a -> return (Tuple3' (i - 1) a xR))
        else stepR xR input >>= (\b -> return (Tuple3' i xL b))

      extract (Tuple3' _ a b) = (,) <$> extractL a <*> extractR b

------------------------------------------------------------------------------
-- N-ary APIs
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Generalized grouping
------------------------------------------------------------------------------

-- This combinator is the most general grouping combinator and can be used to
-- implement all other grouping combinators.
--
-- XXX check if this can implement the splitOn combinator i.e. we can slide in
-- new elements, slide out old elements and incrementally compute the hash.
-- Also, can we implement the windowed classification combinators using this?
--
-- In fact this is a parse. Instead of using a special return value in the fold
-- we are using a mapping function.
--
-- Note that 'scanl'' (usually followed by a map to extract the desired value
-- from the accumulator) can be used to realize many implementations e.g. a
-- sliding window implementation. A scan followed by a mapMaybe is also a good
-- pattern to express many problems where we want to emit a filtered output and
-- not emit an output on every input.
--
-- Passing on of the initial accumulator value to the next fold is equivalent
-- to returning the leftover concept.

{-
-- | @groupScan splitter fold stream@ folds the input stream using @fold@.
-- @splitter@ is applied on the accumulator of the fold every time an item is
-- consumed by the fold. The fold continues until @splitter@ returns a 'Just'
-- value.  A 'Just' result from the @splitter@ specifies a result to be emitted
-- in the output stream and the initial value of the accumulator for the next
-- group's fold. This allows us to control whether to start fresh for the next
-- fold or to continue from the previous fold's output.
--
{-# INLINE groupScan #-}
groupScan
    :: (IsStream t, Monad m)
    => (x -> m (Maybe (b, x))) -> Fold m a x -> t m a -> t m b
groupScan split fold m = undefined
-}

-- | Group the input stream into groups of @n@ elements each and then fold each
-- group using the provided fold function.
--
-- >> S.toList $ FL.chunksOf 2 FL.sum (S.enumerateFromTo 1 10)
-- > [3,7,11,15,19]
--
-- This can be considered as an n-fold version of 'ltake' where we apply
-- 'ltake' repeatedly on the leftover stream until the stream exhausts.
--
-- @since 0.7.0
{-# INLINE chunksOf #-}
chunksOf
    :: (IsStream t, Monad m)
    => Int -> Fold m a b -> t m a -> t m b
chunksOf n f m = D.fromStreamD $ D.groupsOf n f (D.toStreamD m)

-- | Transform a fold from a pure input to a 'Maybe' input, consuming only
-- 'Just' values.
{-# INLINE lcatMaybes #-}
lcatMaybes :: Monad m => Fold m a b -> Fold m (Maybe a) b
lcatMaybes = lfilter isJust . lmap fromJust

-- XXX we can implement this by repeatedly applying the 'lrunFor' fold.
-- XXX add this example after fixing the serial stream rate control
-- >>> S.toList $ S.take 5 $ sessionsOf 1 FL.sum $ constRate 2 $ S.enumerateFrom 1
-- > [3,7,11,15,19]
--
-- | Group the input stream into windows of @n@ second each and then fold each
-- group using the provided fold function.
--
-- @since 0.7.0
{-# INLINE sessionsOf #-}
sessionsOf
    :: (IsStream t, MonadAsync m)
    => Double -> Fold m a b -> t m a -> t m b
sessionsOf n f xs =
    splitBySuffix isNothing (lcatMaybes f)
        (intersperseByTime n (return Nothing) (S.map Just xs))
    where
    intersperseByTime n' f' xs' = xs' `Par.parallelEndByFirst` S.repeatM timed
        where timed = liftIO (threadDelay (round $ n' * 1000000)) >> f'

------------------------------------------------------------------------------
-- Element Aware APIs
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------

-- | Break the input stream into two groups, the first group takes the input as
-- long as the predicate applied to the first element of the stream and next
-- input element holds 'True', the second group takes the rest of the input.
--
_spanBy
    :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
_spanBy cmp (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step init extract

    where
      init = Tuple3' <$> initialL <*> initialR <*> return (Tuple' Nothing True)

      step (Tuple3' a b (Tuple' (Just frst) isFirstG)) input =
        if cmp frst input && isFirstG
        then stepL a input
              >>= (\a' -> return (Tuple3' a' b (Tuple' (Just frst) isFirstG)))
        else stepR b input
              >>= (\a' -> return (Tuple3' a a' (Tuple' Nothing False)))

      step (Tuple3' a b (Tuple' Nothing isFirstG)) input =
        if isFirstG
        then stepL a input
              >>= (\a' -> return (Tuple3' a' b (Tuple' (Just input) isFirstG)))
        else stepR b input
              >>= (\a' -> return (Tuple3' a a' (Tuple' Nothing False)))

      extract (Tuple3' a b _) = (,) <$> extractL a <*> extractR b

-- | @span p f1 f2@ composes folds @f1@ and @f2@ such that @f1@ consumes the
-- input as long as the predicate @p@ is 'True'.  @f2@ consumes the rest of the
-- input.
--
-- > let span_ p xs = FL.foldl' (FL.span p FL.toList FL.toList) $ S.fromList xs
--
-- >>> span_ (< 1) [1,2,3]
-- > ([],[1,2,3])
--
-- >>> span_ (< 2) [1,2,3]
-- > ([1],[2,3])
--
-- >>> span_ (< 4) [1,2,3]
-- > ([1,2,3],[])
--
-- @since 0.7.0

-- This can be considered as a two-fold version of 'ltakeWhile' where we take
-- both the segments instead of discarding the leftover.
{-# INLINE span #-}
span
    :: Monad m
    => (a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
span p (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step init extract

    where
      init = Tuple3' <$> initialL <*> initialR <*> return True

      step (Tuple3' a b isFirstG) input =
        if isFirstG && p input
        then stepL a input >>= (\a' -> return (Tuple3' a' b True))
        else stepR b input >>= (\a' -> return (Tuple3' a a' False))

      extract (Tuple3' a b _) = (,) <$> extractL a <*> extractR b

-- |
-- > break p = span (not . p)
--
-- Break as soon as the predicate becomes 'True'. @break p f1 f2@ composes
-- folds @f1@ and @f2@ such that @f1@ stops consuming input as soon as the
-- predicate @p@ becomes 'True'. The rest of the input is consumed @f2@.
--
-- This is the binary version of 'splitBy'.
--
-- > let break_ p xs = FL.foldl' (FL.break p FL.toList FL.toList) $ S.fromList xs
--
-- >>> break_ (< 1) [3,2,1]
-- > ([3,2,1],[])
--
-- >>> break_ (< 2) [3,2,1]
-- > ([3,2],[1])
--
-- >>> break_ (< 4) [3,2,1]
-- > ([],[3,2,1])
--
-- @since 0.7.0
{-# INLINE _break #-}
_break
    :: Monad m
    => (a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
_break p = span (not . p)

-- | Like 'spanBy' but applies the predicate in a rolling fashion i.e.
-- predicate is applied to the previous and the next input elements.
{-# INLINE _spanRollingBy #-}
_spanRollingBy
    :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
_spanRollingBy cmp (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step init extract

  where
    init = Tuple3' <$> initialL <*> initialR <*> return Nothing

    step (Tuple3' a b (Just frst)) input =
      if cmp input frst
      then stepL a input >>= (\a' -> return (Tuple3' a' b (Just input)))
      else stepR b input >>= (\b' -> return (Tuple3' a b' (Just input)))

    step (Tuple3' a b Nothing) input =
      stepL a input >>= (\a' -> return (Tuple3' a' b (Just input)))

    extract (Tuple3' a b _) = (,) <$> extractL a <*> extractR b

------------------------------------------------------------------------------
-- N-ary APIs
------------------------------------------------------------------------------
--
-- | @groupsBy cmp f $ S.fromList [a,b,c,...]@ assigns the element @a@ to the
-- first group, if @a \`cmp` b@ is 'True' then @b@ is also assigned to the same
-- group.  If @a \`cmp` c@ is 'True' then @c@ is also assigned to the same
-- group and so on. When the comparison fails a new group is started. Each
-- group is folded using the fold @f@ and the result of the fold is emitted in
-- the output stream.
--
-- >>> S.toList $ FL.groupsBy (>) FL.toList $ S.fromList [1,3,7,0,2,5]
-- > [[1,3,7],[0,2,5]]
--
-- @since 0.7.0
{-# INLINE groupsBy #-}
groupsBy
    :: (IsStream t, Monad m)
    => (a -> a -> Bool)
    -> Fold m a b
    -> t m a
    -> t m b
groupsBy cmp f m = D.fromStreamD $ D.groupsBy cmp f (D.toStreamD m)

-- | Unlike @groupsBy@ this function performs a rolling comparison of two
-- successive elements in the input stream. @groupsByRolling cmp f $ S.fromList
-- [a,b,c,...]@ assigns the element @a@ to the first group, if @a \`cmp` b@ is
-- 'True' then @b@ is also assigned to the same group.  If @b \`cmp` c@ is
-- 'True' then @c@ is also assigned to the same group and so on. When the
-- comparison fails a new group is started. Each group is folded using the fold
-- @f@.
--
-- >>> S.toList $ FL.groupsByRolling (\a b -> a + 1 == b) FL.toList $ S.fromList [1,2,3,7,8,9]
-- > [[1,2,3],[7,8,9]]
--
-- @since 0.7.0
{-# INLINE groupsByRolling #-}
groupsByRolling
    :: (IsStream t, Monad m)
    => (a -> a -> Bool)
    -> Fold m a b
    -> t m a
    -> t m b
groupsByRolling cmp f m =  D.fromStreamD $ D.groupsRollingBy cmp f (D.toStreamD m)

-- |
-- > groups = groupsBy (==)
-- > groups = groupsByRolling (==)
--
-- Groups contiguous spans of equal elements together in individual groups.
--
-- >>> S.toList $ FL.groups FL.toList $ S.fromList [1,1,2,2]
-- > [[1,1],[2,2]]
--
-- @since 0.7.0
groups :: (IsStream t, Monad m, Eq a) => Fold m a b -> t m a -> t m b
groups = groupsBy (==)

------------------------------------------------------------------------------
-- Binary splitting on a separator
------------------------------------------------------------------------------

{-
-- | Find the first occurrence of the specified sequence in the input stream
-- and break the input stream into two parts, the first part consisting of the
-- stream before the sequence and the second part consisting of the sequence
-- and the rest of the stream.
--
-- > let breakOn_ pat xs = FL.foldl' (FL.breakOn pat FL.toList FL.toList) $ S.fromList xs
--
-- >>> breakOn_ "dear" "Hello dear world!"
-- > ("Hello ","dear world!")
--
{-# INLINE breakOn #-}
breakOn :: Monad m => Array a -> Fold m a b -> Fold m a c -> Fold m a (b,c)
breakOn pat f m = undefined
-}

------------------------------------------------------------------------------
-- N-ary split on a predicate
------------------------------------------------------------------------------

-- TODO: Use a Splitter configuration similar to the "split" package to make it
-- possible to express all splitting combinations. In general, we can have
-- infix/suffix/prefix/condensing of separators, dropping both leading/trailing
-- separators. We can have a single split operation taking the splitter config
-- as argument.

-- | Split a stream on separator elements determined by a predicate, dropping
-- the separator. Separator is considered as infixed between two segments, if
-- one side of the separator is missing then it is parsed as an empty stream.
-- With '-' representing elements and '.' as separator, 'splitOn' splits as
-- follows:
--
-- @
-- "--.--" => "--" "--"
-- "--."   => "--" ""
-- ".--"   => ""   "--"
-- @
--
-- @splitOn (== x)@ is an inverse of @intercalate (S.yield x)@
--
-- Let's use the following definition for illustration:
--
-- > splitOn' p xs = S.toList $ FL.splitOn p (FL.toList) (S.fromList xs)
--
-- >>> splitOn' (== '.') ""
-- [""]
--
-- >>> splitOn' (== '.') "."
-- ["",""]
--
-- >>> splitOn' (== '.') ".a"
-- > ["","a"]
--
-- >>> splitOn' (== '.') "a."
-- > ["a",""]
--
-- >>> splitOn' (== '.') "a.b"
-- > ["a","b"]
--
-- >>> splitOn' (== '.') "a..b"
-- > ["a","","b"]
--
-- @since 0.7.0

-- This can be considered as an n-fold version of 'breakOn' where we apply
-- 'breakOn' successively on the input stream, dropping the first element
-- of the second segment after each break.
--
{-# INLINE splitOn #-}
splitOn
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitOn predicate f m =
    D.fromStreamD $ D.splitBy predicate f (D.toStreamD m)

-- | Like 'splitOn' but the separator is considered as suffixed to the segments
-- in the stream. A missing suffix at the end is allowed. A separator at the
-- beginning is parsed as empty segment.  With '-' representing elements and
-- '.' as separator, 'splitOnSuffix' splits as follows:
--
-- @
--  "--.--." => "--" "--"
--  "--.--"  => "--" "--"
--  ".--."   => "" "--"
-- @
--
-- > splitOnSuffix' p xs = S.toList $ FL.splitSuffixBy p (FL.toList) (S.fromList xs)
--
-- >>> splitOnSuffix' (== '.') ""
-- []
--
-- >>> splitOnSuffix' (== '.') "."
-- [""]
--
-- >>> splitOnSuffix' (== '.') "a"
-- ["a"]
--
-- >>> splitOnSuffix' (== '.') ".a"
-- > ["","a"]
--
-- >>> splitOnSuffix' (== '.') "a."
-- > ["a"]
--
-- >>> splitOnSuffix' (== '.') "a.b"
-- > ["a","b"]
--
-- >>> splitOnSuffix' (== '.') "a.b."
-- > ["a","b"]
--
-- >>> splitOnSuffix' (== '.') "a..b.."
-- > ["a","","b",""]
--
-- > lines = splitOnSuffix (== '\n')
--
-- @since 0.7.0

-- This can be considered as an n-fold version of 'breakPost' where we apply
-- 'breakPost' successively on the input stream, dropping the first element
-- of the second segment after each break.
--
{-# INLINE splitOnSuffix #-}
splitOnSuffix
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitOnSuffix predicate f m =
    D.fromStreamD $ D.splitSuffixBy predicate f (D.toStreamD m)

-- | Like 'splitBy' but ignores repeated separators or separators in leading
-- or trailing position. Therefore, @"..a..b.."@ would be parsed as
-- @["a","b"]@.  In other words, it treats the input like words separated by
-- whitespace elements determined by the predicate.
--
-- > wordsBy' p xs = S.toList $ FL.wordsBy p (FL.toList) (S.fromList xs)
--
-- >>> wordsBy' (== ',') ""
-- > []
--
-- >>> wordsBy' (== ',') ","
-- > []
--
-- >>> wordsBy' (== ',') ",a,,b,"
-- > ["a","b"]
--
-- > words = wordsBy isSpace
--
-- @since 0.7.0
{-# INLINE wordsBy #-}
wordsBy
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
wordsBy predicate f m =
    D.fromStreamD $ D.wordsBy predicate f (D.toStreamD m)

-- | Like 'splitOnSuffix' but keeps the suffix attached to the resulting
-- splits.
--
-- > splitBySuffix' p xs = S.toList $ FL.splitBySuffix p (FL.toList) (S.fromList xs)
--
-- >>> splitBySuffix' (== '.') ""
-- []
--
-- >>> splitBySuffix' (== '.') "."
-- ["."]
--
-- >>> splitBySuffix' (== '.') "a"
-- ["a"]
--
-- >>> splitBySuffix' (== '.') ".a"
-- > [".","a"]
--
-- >>> splitBySuffix' (== '.') "a."
-- > ["a."]
--
-- >>> splitBySuffix' (== '.') "a.b"
-- > ["a.","b"]
--
-- >>> splitBySuffix' (== '.') "a.b."
-- > ["a.","b."]
--
-- >>> splitBySuffix' (== '.') "a..b.."
-- > ["a.",".","b.","."]
--
-- @since 0.7.0

-- This can be considered as an n-fold version of 'breakPost' where we apply
-- 'breakPost' successively on the input stream.
--
{-# INLINE splitBySuffix #-}
splitBySuffix
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitBySuffix predicate f m =
    D.fromStreamD $ D.splitSuffixBy' predicate f (D.toStreamD m)

------------------------------------------------------------------------------
-- Split on a delimiter sequence
------------------------------------------------------------------------------

-- Int list examples for splitOn:
--
-- >>> splitList [] [1,2,3,3,4]
-- > [[1],[2],[3],[3],[4]]
--
-- >>> splitList [5] [1,2,3,3,4]
-- > [[1,2,3,3,4]]
--
-- >>> splitList [1] [1,2,3,3,4]
-- > [[],[2,3,3,4]]
--
-- >>> splitList [4] [1,2,3,3,4]
-- > [[1,2,3,3],[]]
--
-- >>> splitList [2] [1,2,3,3,4]
-- > [[1],[3,3,4]]
--
-- >>> splitList [3] [1,2,3,3,4]
-- > [[1,2],[],[4]]
--
-- >>> splitList [3,3] [1,2,3,3,4]
-- > [[1,2],[4]]
--
-- >>> splitList [1,2,3,3,4] [1,2,3,3,4]
-- > [[],[]]

-- | Like 'splitOn' but the separator is a sequence of elements instead of a
-- single element.
--
-- For illustration, let's define a function that operates on pure lists:
--
-- @
-- splitOnSeq' pat xs = S.toList $ FL.splitOnSeq (A.fromList pat) (FL.toList) (S.fromList xs)
-- @
--
-- >>> splitOnSeq' "" "hello"
-- > ["h","e","l","l","o"]
--
-- >>> splitOnSeq' "hello" ""
-- > [""]
--
-- >>> splitOnSeq' "hello" "hello"
-- > ["",""]
--
-- >>> splitOnSeq' "x" "hello"
-- > ["hello"]
--
-- >>> splitOnSeq' "h" "hello"
-- > ["","ello"]
--
-- >>> splitOnSeq' "o" "hello"
-- > ["hell",""]
--
-- >>> splitOnSeq' "e" "hello"
-- > ["h","llo"]
--
-- >>> splitOnSeq' "l" "hello"
-- > ["he","","o"]
--
-- >>> splitOnSeq' "ll" "hello"
-- > ["he","o"]
--
-- 'splitOnSeq' is an inverse of 'intercalate'. The following law always holds:
--
-- > intercalate . splitOn == id
--
-- The following law holds when the separator is non-empty and contains none of
-- the elements present in the input lists:
--
-- > splitOn . intercalate == id
--
-- @since 0.7.0

-- XXX We can use a polymorphic vector implemented by Array# to represent the
-- sequence, that way we can avoid the Storable constraint. If we still need
-- Storable Array for performance, we can use a separate splitOnArray API for
-- that. We can also have an API where the sequence itself is a lazy stream, so
-- that we can search files in files for example.
{-# INLINE splitOnSeq #-}
splitOnSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOnSeq patt f m = D.fromStreamD $ D.splitOn patt f (D.toStreamD m)

{-
-- This can be implemented easily using Rabin Karp
-- | Split on any one of the given patterns.
{-# INLINE splitOnAny #-}
splitOnAny
    :: (IsStream t, Monad m, Storable a, Integral a)
    => [Array a] -> Fold m a b -> t m a -> t m b
splitOnAny subseq f m = undefined -- D.fromStreamD $ D.splitOnAny f subseq (D.toStreamD m)
-}

-- | Like 'splitSuffixBy' but the separator is a sequence of elements, instead
-- of a predicate for a single element.
--
-- > splitSuffixOn_ pat xs = S.toList $ FL.splitSuffixOn (A.fromList pat) (FL.toList) (S.fromList xs)
--
-- >>> splitSuffixOn_ "." ""
-- [""]
--
-- >>> splitSuffixOn_ "." "."
-- [""]
--
-- >>> splitSuffixOn_ "." "a"
-- ["a"]
--
-- >>> splitSuffixOn_ "." ".a"
-- > ["","a"]
--
-- >>> splitSuffixOn_ "." "a."
-- > ["a"]
--
-- >>> splitSuffixOn_ "." "a.b"
-- > ["a","b"]
--
-- >>> splitSuffixOn_ "." "a.b."
-- > ["a","b"]
--
-- >>> splitSuffixOn_ "." "a..b.."
-- > ["a","","b",""]
--
-- > lines = splitSuffixOn "\n"
--
-- @since 0.7.0
{-# INLINE _splitOnSuffixSeq #-}
_splitOnSuffixSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
_splitOnSuffixSeq patt f m =
    D.fromStreamD $ D.splitSuffixOn False patt f (D.toStreamD m)

{-
-- | Like 'splitOn' but drops any empty splits.
--
{-# INLINE wordsOn #-}
wordsOn
    :: (IsStream t, Monad m, Storable a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
wordsOn subseq f m = undefined -- D.fromStreamD $ D.wordsOn f subseq (D.toStreamD m)
-}

-- XXX use a non-monadic intersperse to remove the MonadAsync constraint.
--
-- | Like 'splitOnSeq' but splits the separator as well, as an infix token.
--
-- > splitOn'_ pat xs = S.toList $ FL.splitOn' (A.fromList pat) (FL.toList) (S.fromList xs)
--
-- >>> splitOn'_ "" "hello"
-- > ["h","","e","","l","","l","","o"]
--
-- >>> splitOn'_ "hello" ""
-- > [""]
--
-- >>> splitOn'_ "hello" "hello"
-- > ["","hello",""]
--
-- >>> splitOn'_ "x" "hello"
-- > ["hello"]
--
-- >>> splitOn'_ "h" "hello"
-- > ["","h","ello"]
--
-- >>> splitOn'_ "o" "hello"
-- > ["hell","o",""]
--
-- >>> splitOn'_ "e" "hello"
-- > ["h","e","llo"]
--
-- >>> splitOn'_ "l" "hello"
-- > ["he","l","","l","o"]
--
-- >>> splitOn'_ "ll" "hello"
-- > ["he","ll","o"]
--
-- @since 0.7.0
{-# INLINE _splitBySeq #-}
_splitBySeq
    :: (IsStream t, MonadAsync m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
_splitBySeq patt f m = S.intersperseM (foldl' f (A.read patt)) $ splitOnSeq patt f m

-- | Like 'splitSuffixOn' but keeps the suffix intact in the splits.
--
-- > splitSuffixOn'_ pat xs = S.toList $ FL.splitSuffixOn' (A.fromList pat) (FL.toList) (S.fromList xs)
--
-- >>> splitSuffixOn'_ "." ""
-- [""]
--
-- >>> splitSuffixOn'_ "." "."
-- ["."]
--
-- >>> splitSuffixOn'_ "." "a"
-- ["a"]
--
-- >>> splitSuffixOn'_ "." ".a"
-- > [".","a"]
--
-- >>> splitSuffixOn'_ "." "a."
-- > ["a."]
--
-- >>> splitSuffixOn'_ "." "a.b"
-- > ["a.","b"]
--
-- >>> splitSuffixOn'_ "." "a.b."
-- > ["a.","b."]
--
-- >>> splitSuffixOn'_ "." "a..b.."
-- > ["a.",".","b.","."]
--
-- @since 0.7.0
{-# INLINE _splitBySuffixSeq #-}
_splitBySuffixSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
_splitBySuffixSeq patt f m =
    D.fromStreamD $ D.splitSuffixOn True patt f (D.toStreamD m)

{-
-- This can be implemented easily using Rabin Karp
-- | Split post any one of the given patterns.
{-# INLINE splitSuffixOnAny #-}
splitSuffixOnAny
    :: (IsStream t, Monad m, Storable a, Integral a)
    => [Array a] -> Fold m a b -> t m a -> t m b
splitSuffixOnAny subseq f m = undefined
    -- D.fromStreamD $ D.splitPostAny f subseq (D.toStreamD m)
-}

------------------------------------------------------------------------------
-- Reorder in sequence
------------------------------------------------------------------------------

{-
-- Buffer until the next element in sequence arrives. The function argument
-- determines the difference in sequence numbers. This could be useful in
-- implementing sequenced streams, for example, TCP reassembly.
{-# INLINE reassembleBy #-}
reassembleBy
    :: (IsStream t, Monad m)
    => Fold m a b
    -> (a -> a -> Int)
    -> t m a
    -> t m b
reassembleBy = undefined
-}

------------------------------------------------------------------------------
-- Distributing
------------------------------------------------------------------------------
--
-- | Distribute one copy of the stream to each fold and zip the results.
--
-- @
--                 |-------Fold m a b--------|
-- ---stream m a---|                         |---m (b,c)
--                 |-------Fold m a c--------|
-- @
-- >>> FL.foldl' (FL.tee FL.sum FL.length) (S.enumerateFromTo 1.0 100.0)
-- (5050.0,100)
--
-- @since 0.7.0
{-# INLINE tee #-}
tee :: Monad m => Fold m a b -> Fold m a c -> Fold m a (b,c)
tee f1 f2 = (,) <$> f1 <*> f2

{-# INLINE foldNil #-}
foldNil :: Monad m => Fold m a [b]
foldNil = Fold step begin done  where
  begin = return []
  step _ _ = return []
  done = return

{-# INLINE foldCons #-}
foldCons :: Monad m => Fold m a b -> Fold m a [b] -> Fold m a [b]
foldCons (Fold stepL beginL doneL) (Fold stepR beginR doneR) =
    Fold step begin done

    where

    begin = Tuple' <$> beginL <*> beginR
    step (Tuple' xL xR) a = Tuple' <$> stepL xL a <*> stepR xR a
    done (Tuple' xL xR) = (:) <$> (doneL xL) <*> (doneR xR)

-- XXX use "List" instead of "[]"?, use Array for output to scale it to a large
-- number of consumers? For polymorphic case a vector could be helpful. For
-- Storables we can use arrays. Will need separate APIs for those.
--
-- | Distribute one copy of the stream to each fold and collect the results in
-- a container.
--
-- @
--
--                 |-------Fold m a b--------|
-- ---stream m a---|                         |---m [b]
--                 |-------Fold m a b--------|
--                 |                         |
--                            ...
-- @
--
-- >>> FL.foldl' (FL.distribute [FL.sum, FL.length]) (S.enumerateFromTo 1 5)
-- [15,5]
--
-- This is the consumer side dual of the producer side 'sequence' operation.
--
-- @since 0.7.0
{-# INLINE distribute #-}
distribute :: Monad m => [Fold m a b] -> Fold m a [b]
distribute [] = foldNil
distribute (x:xs) = foldCons x (distribute xs)

------------------------------------------------------------------------------
-- Partitioning
------------------------------------------------------------------------------
--
-- | Partition the input over two folds using an 'Either' partitioning
-- predicate.
--
-- @
--
--                                     |-------Fold b x--------|
-- -----stream m a --> (Either b c)----|                       |----(x,y)
--                                     |-------Fold c y--------|
-- @
--
-- Send input to either fold randomly:
--
-- >>> import System.Random (randomIO)
-- >>> randomly a = randomIO >>= \x -> return $ if x then Left a else Right a
-- >>> FL.foldl' (FL.partitionByM randomly FL.length FL.length) (S.enumerateFromTo 1 100)
-- (59,41)
--
-- Send input to the two folds in a proportion of 2:1:
--
-- @
-- import Data.IORef (newIORef, readIORef, writeIORef)
-- proportionately m n = do
--  ref <- newIORef $ cycle $ concat [replicate m Left, replicate n Right]
--  return $ \\a -> do
--      r <- readIORef ref
--      writeIORef ref $ tail r
--      return $ head r a
--
-- main = do
--  f <- proportionately 2 1
--  r <- FL.foldl' (FL.partitionByM f FL.length FL.length) (S.enumerateFromTo (1 :: Int) 100)
--  print r
-- @
-- @
-- (67,33)
-- @
--
-- This is the consumer side dual of the producer side 'mergeBy' operation.
--
-- @since 0.7.0
{-# INLINE partitionByM #-}
partitionByM :: Monad m
    => (a -> m (Either b c)) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionByM f (Fold stepL beginL doneL) (Fold stepR beginR doneR) =

    Fold step begin done

    where

    begin = Tuple' <$> beginL <*> beginR
    step (Tuple' xL xR) a = do
        r <- f a
        case r of
            Left b -> Tuple' <$> stepL xL b <*> return xR
            Right c -> Tuple' <$> return xL <*> stepR xR c
    done (Tuple' xL xR) = (,) <$> doneL xL <*> doneR xR

-- Note: we could use (a -> Bool) instead of (a -> Either b c), but the latter
-- makes the signature clearer as to which case belongs to which fold.
-- XXX need to check the performance in both cases.

-- | Same as 'partitionByM' but with a pure partition function.
--
-- Count even and odd numbers in a stream:
--
-- @
-- >>> let f = FL.partitionBy (\\n -> if even n then Left n else Right n)
--                       (fmap (("Even " ++) . show) FL.length)
--                       (fmap (("Odd "  ++) . show) FL.length)
--   in FL.foldl' f (S.enumerateFromTo 1 100)
-- ("Even 50","Odd 50")
-- @
--
-- @since 0.7.0
{-# INLINE partitionBy #-}
partitionBy :: Monad m
    => (a -> Either b c) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionBy f = partitionByM (return . f)

-- | Compose two folds such that the combined fold accepts a stream of 'Either'
-- and routes the 'Left' values to the first fold and 'Right' values to the
-- second fold.
--
-- > partition = partitionBy id
--
-- @since 0.7.0
{-# INLINE partition #-}
partition :: Monad m
    => Fold m b x -> Fold m c y -> Fold m (Either b c) (x, y)
partition = partitionBy id

{-
-- | Send one item to each fold in a round-robin fashion. This is the consumer
-- side dual of producer side 'mergeN' operation.
--
-- partitionN :: Monad m => [Fold m a b] -> Fold m a [b]
-- partitionN fs = Fold step begin done
-}

-- TODO Demultiplex an input element into a number of typed variants. We want
-- to statically restrict the target values within a set of predefined types,
-- an enumeration of a GADT. We also want to make sure that the Map contains
-- only those types and the full set of those types.
--
-- TODO Instead of the input Map it should probably be a lookup-table using an
-- array and not in GC memory. The same applies to the output Map as well.
-- However, that would only be helpful if we have a very large data structure,
-- need to measure and see how it scales.
--
-- This is the consumer side dual of the producer side 'mux' operation (XXX to
-- be implemented).

-- | Split the input stream based on a key field and fold each split using a
-- specific fold collecting the results in a map from the keys to the results.
-- Useful for cases like protocol handlers to handle different type of packets
-- using different handlers.
--
-- @
--
--                             |-------Fold m a b
-- -----stream m a-----Map-----|
--                             |-------Fold m a b
--                             |
--                                       ...
-- @
--
-- @since 0.7.0
{-# INLINE demuxWith #-}
demuxWith :: (Monad m, Ord k)
    => (a -> k) -> Map k (Fold m a b) -> Fold m a (Map k b)
demuxWith f kv = Fold step initial extract

    where

    initial = return kv
    step mp a =
        -- XXX should we raise an exception in Nothing case?
        -- Ideally we should enforce that it is a total map over k so that look
        -- up never fails
        -- XXX we could use a monadic update function for a single lookup and
        -- update in the map.
        let k = f a
        in case Map.lookup k mp of
            Nothing -> return mp
            Just (Fold step' acc extract') -> do
                !r <- acc >>= \x -> step' x a
                return $ Map.insert k (Fold step' (return r) extract') mp
    extract = Prelude.mapM (\(Fold _ acc e) -> acc >>= e)

-- | Fold a stream of key value pairs using a map of specific folds for each
-- key into a map from keys to the results of fold outputs of the corresponding
-- values.
--
-- @
-- > let table = Data.Map.fromList [(\"SUM", FL.sum), (\"PRODUCT", FL.product)]
--       input = S.fromList [(\"SUM",1),(\"PRODUCT",2),(\"SUM",3),(\"PRODUCT",4)]
--   in FL.foldl' (FL.demux table) input
-- One 1
-- Two 2
-- @
--
-- @since 0.7.0
{-# INLINE demux #-}
demux :: (Monad m, Ord k)
    => Map k (Fold m a b) -> Fold m (k, a) (Map k b)
demux fs = demuxWith fst (Map.map (lmap snd) fs)

-- | Split the input stream based on a key field and fold each split using a
-- specific fold without collecting the results. Useful for cases like protocol
-- handlers to handle different type of packets.
--
-- @
--
--                             |-------Fold m a ()
-- -----stream m a-----Map-----|
--                             |-------Fold m a ()
--                             |
--                                       ...
-- @
--
--
-- @since 0.7.0

-- demuxWith_ can be slightly faster than demuxWith because we do not need to
-- update the Map in this case. This may be significant only if the map is
-- large.
{-# INLINE demuxWith_ #-}
demuxWith_ :: (Monad m, Ord k)
    => (a -> k) -> Map k (Fold m a b) -> Fold m a ()
demuxWith_ f kv = Fold step initial extract

    where

    initial = do
        Prelude.mapM (\(Fold s i e) ->
            i >>= \r -> return (Fold s (return r) e)) kv
    step mp a =
        -- XXX should we raise an exception in Nothing case?
        -- Ideally we should enforce that it is a total map over k so that look
        -- up never fails
        case Map.lookup (f a) mp of
            Nothing -> return mp
            Just (Fold step' acc _) -> do
                _ <- acc >>= \x -> step' x a
                return mp
    extract mp = Prelude.mapM (\(Fold _ acc e) -> acc >>= e) mp >> return ()

-- | Given a stream of key value pairs and a map from keys to folds, fold the
-- values for each key using the corresponding folds, discarding the outputs.
--
-- @
-- > let prn = FL.drainBy print
-- > let table = Data.Map.fromList [(\"ONE", prn), (\"TWO", prn)]
--       input = S.fromList [(\"ONE",1),(\"TWO",2)]
--   in FL.foldl' (FL.demux_ table) input
-- One 1
-- Two 2
-- @
--
-- @since 0.7.0
{-# INLINE demux_ #-}
demux_ :: (Monad m, Ord k) => Map k (Fold m a ()) -> Fold m (k, a) ()
demux_ fs = demuxWith_ fst (Map.map (lmap snd) fs)

-- TODO If the data is large we may need a map/hashmap in pinned memory instead
-- of a regular Map. That may require a serializable constraint though. We can
-- have another API for that.
--
-- | Split the input stream based on a key field and fold each split using the
-- given fold. Useful for map/reduce, bucketizing the input in different bins
-- or for generating histograms.
--
-- @
-- > let input = S.fromList [(\"ONE",1),(\"ONE",1.1),(\"TWO",2), (\"TWO",2.2)]
--   in FL.foldl' (FL.classify FL.toListRev) input
-- fromList [(\"ONE",[1.1,1.0]),(\"TWO",[2.2,2.0])]
-- @
--
-- @since 0.7.0
{-# INLINE classifyWith #-}
classifyWith :: (Monad m, Ord k) => (a -> k) -> Fold m a b -> Fold m a (Map k b)
classifyWith f (Fold step initial extract) = Fold step' initial' extract'

    where

    initial' = return Map.empty
    step' kv a =
        let k = f a
        in case Map.lookup k kv of
            Nothing -> do
                x <- initial
                r <- step x a
                return $ Map.insert k r kv
            Just x -> do
                r <- step x a
                return $ Map.insert k r kv
    extract' = Prelude.mapM extract

-- | Given an input stream of key value pairs and a fold for values, fold all
-- the values belonging to each key.  Useful for map/reduce, bucketizing the
-- input in different bins or for generating histograms.
--
-- @
-- > let input = S.fromList [(\"ONE",1),(\"ONE",1.1),(\"TWO",2), (\"TWO",2.2)]
--   in FL.foldl' (FL.classify FL.toListRev) input
-- fromList [(\"ONE",[1.1,1.0]),(\"TWO",[2.2,2.0])]
-- @
--
-- @since 0.7.0

-- Same as:
--
-- > classify fld = classifyWith fst (lmap snd fld)
--
{-# INLINE classify #-}
classify :: (Monad m, Ord k) => Fold m a b -> Fold m (k, a) (Map k b)
classify fld = classifyWith fst (lmap snd fld)

------------------------------------------------------------------------------
-- Unzipping
------------------------------------------------------------------------------
--
-- | Like 'unzipWith' but with a monadic splitter function.
--
-- @since 0.7.0
{-# INLINE unzipWithM #-}
unzipWithM :: Monad m
    => (a -> m (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWithM f (Fold stepL beginL doneL) (Fold stepR beginR doneR) =
    Fold step begin done

    where

    step (Tuple' xL xR) a = do
        (b,c) <- f a
        Tuple' <$> stepL xL b <*> stepR xR c
    begin = Tuple' <$> beginL <*> beginR
    done (Tuple' xL xR) = (,) <$> doneL xL <*> doneR xR

-- | Split elements in the input stream into two parts using a pure splitter
-- function, direct each part to a different fold and zip the results.
--
-- @since 0.7.0
{-# INLINE unzipWith #-}
unzipWith :: Monad m
    => (a -> (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWith f = unzipWithM (return . f)

-- | Send the elements of tuples in a stream of tuples through two different
-- folds.
--
-- @
--
--                           |-------Fold m a x--------|
-- ---------stream of (a,b)--|                         |----m (x,y)
--                           |-------Fold m b y--------|
--
-- @
--
-- This is the consumer side dual of the producer side 'zip' operation.
--
-- @since 0.7.0
{-# INLINE unzip #-}
unzip :: Monad m => Fold m a x -> Fold m b y -> Fold m (a,b) (x,y)
unzip = unzipWith id

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------
--
-- | Modify the fold such that when the fold is done, instead of returning the
-- accumulator, it returns a fold. The returned fold starts from where we left
-- i.e. it uses the last accumulator value as the initial value of the
-- accumulator. Thus we can resume the fold later and feed it more input.
--
-- >> do
-- >    more <- FL.foldl (FL.duplicate FL.sum) (S.enumerateFromTo 1 10)
-- >    evenMore <- FL.foldl (FL.duplicate more) (S.enumerateFromTo 11 20)
-- >    FL.foldl evenMore (S.enumerateFromTo 21 30)
-- > 465
--
-- @since 0.7.0
{-# INLINABLE _duplicate #-}
_duplicate :: Applicative m => Fold m a b -> Fold m a (Fold m a b)
_duplicate (Fold step begin done) =
    Fold step begin (\x -> pure (Fold step (pure x) done))

{-
-- All the stream flattening transformations can also be applied to a fold
-- input stream.

-- | This can be used to apply all the stream generation operations on folds.
lconcatMap ::(IsStream t, Monad m) => (a -> t m b)
    -> Fold m b c
    -> Fold m a c
lconcatMap s f1 f2 = undefined
-}

-- All the grouping transformation that we apply to a stream can also be
-- applied to a fold input stream.

{-
-- | Group the input stream into groups of elements between @low@ and @high@.
-- Collection starts in chunks of @low@ and then keeps doubling until we reach
-- @high@. Each chunk is folded using the provided fold function.
--
-- This could be useful, for example, when we are folding a stream of unknown
-- size to a stream of arrays and we want to minimize the number of
-- allocations.
--
-- @
--
-- XXX we should be able to implement it with parsers/terminating folds.
--
{-# INLINE lchunksInRange #-}
lchunksInRange :: Monad m
    => Int -> Int -> Fold m a b -> Fold m b c -> Fold m a c
lchunksInRange low high (Fold step1 initial1 extract1)
                        (Fold step2 initial2 extract2) = undefined
-}

-- | Group the input stream into groups of @n@ elements each and then fold each
-- group using the provided fold function.
--
-- @
--
-- -----Fold m a b----|-Fold n a c-|-Fold n a c-|-...-|----Fold m a c
--
-- @
--
{-# INLINE _lchunksOf #-}
_lchunksOf :: Monad m => Int -> Fold m a b -> Fold m b c -> Fold m a c
_lchunksOf n (Fold step1 initial1 extract1) (Fold step2 initial2 extract2) =
    Fold step' initial' extract'

    where

    initial' = (Tuple3' 0) <$> initial1 <*> initial2
    step' (Tuple3' i r1 r2) a = do
        if i < n
        then do
            res <- step1 r1 a
            return $ Tuple3' (i + 1) res r2
        else do
            res <- extract1 r1
            acc2 <- step2 r2 res

            i1 <- initial1
            acc1 <- step1 i1 a
            return $ Tuple3' 1 acc1 acc2
    extract' (Tuple3' _ _ r) = extract2 r

-- | Group the input stream into windows of n second each and then fold each
-- group using the provided fold function.
--
-- For example, we can copy and distribute a stream to multiple folds where
-- each fold can group the input differently e.g. by one second, one minute and
-- one hour windows respectively and fold each resulting stream of folds.
--
-- @
--
-- -----Fold m a b----|-Fold n a c-|-Fold n a c-|-...-|----Fold m a c
--
-- @
{-# INLINE _lsessionsOf #-}
_lsessionsOf :: MonadAsync m => Double -> Fold m a b -> Fold m b c -> Fold m a c
_lsessionsOf n (Fold step1 initial1 extract1) (Fold step2 initial2 extract2) =
    Fold step' initial' extract'

    where

    -- XXX MVar may be expensive we need a cheaper synch mechanism here
    initial' = do
        i1 <- initial1
        i2 <- initial2
        mv1 <- liftIO $ newMVar i1
        mv2 <- liftIO $ newMVar (Right i2)
        t <- control $ \run ->
            mask $ \restore -> do
                tid <- forkIO $ catch (restore $ void $ run (timerThread mv1 mv2))
                                      (handleChildException mv2)
                run (return tid)
        return $ Tuple3' t mv1 mv2
    step' acc@(Tuple3' _ mv1 _) a = do
            r1 <- liftIO $ takeMVar mv1
            res <- step1 r1 a
            liftIO $ putMVar mv1 res
            return acc
    extract' (Tuple3' tid _ mv2) = do
        r2 <- liftIO $ takeMVar mv2
        liftIO $ killThread tid
        case r2 of
            Left e -> throwM e
            Right x -> extract2 x

    timerThread mv1 mv2 = do
        liftIO $ threadDelay (round $ n * 1000000)

        r1 <- liftIO $ takeMVar mv1
        i1 <- initial1
        liftIO $ putMVar mv1 i1

        res1 <- extract1 r1
        r2 <- liftIO $ takeMVar mv2
        res <- case r2 of
                    Left _ -> return r2
                    Right x -> fmap Right $ step2 x res1
        liftIO $ putMVar mv2 res
        timerThread mv1 mv2

    handleChildException ::
        MVar (Either SomeException a) -> SomeException -> IO ()
    handleChildException mv2 e = do
        r2 <- takeMVar mv2
        let r = case r2 of
                    Left _ -> r2
                    Right _ -> Left e
        putMVar mv2 r

------------------------------------------------------------------------------
-- Windowed classification
------------------------------------------------------------------------------

-- We divide the stream into windows or chunks in space or time and each window
-- can be associated with a key, all events associated with a particular key in
-- the window can be folded to a single result. The stream can be split into
-- windows by size or by using a split predicate on the elements in the stream.
-- For example, when we receive a closing flag, we can close the window.
--
-- A "chunk" is a space window and a "session" is a time window. Are there any
-- other better short words to describe them. An alternative is to use
-- "swindow" and "twindow". Another word for "session" could be "spell".
--
-- TODO: To mark the position in space or time we can have Indexed or
-- TimeStamped types. That can make it easy to deal with the position indices
-- or timestamps.

------------------------------------------------------------------------------
-- Keyed Sliding Windows
------------------------------------------------------------------------------

{-
{-# INLINABLE classifySlidingChunks #-}
classifySlidingChunks
    :: (IsStream t, MonadAsync m, Ord k)
    => Int              -- ^ window size
    -> Int              -- ^ window slide
    -> Fold m a b       -- ^ Fold to be applied to window events
    -> t m (k, a, Bool) -- ^ window key, data, close event
    -> t m (k, b)
classifySlidingChunks wsize wslide (Fold step initial extract) str
    = undefined

-- XXX Another variant could be to slide the window on an event, e.g. in TCP we
-- slide the send window when an ack is received and we slide the receive
-- window when a sequence is complete. Sliding is stateful in case of TCP,
-- sliding releases the send buffer or makes data available to the user from
-- the receive buffer.
{-# INLINABLE classifySlidingSessions #-}
classifySlidingSessions
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ timer tick in seconds
    -> Double         -- ^ time window size
    -> Double         -- ^ window slide
    -> Fold m a b     -- ^ Fold to be applied to window events
    -> t m (k, a, Bool, AbsTime) -- ^ window key, data, close flag, timestamp
    -> t m (k, b)
classifySlidingSessions tick interval slide (Fold step initial extract) str
    = undefined
-}

------------------------------------------------------------------------------
-- Sliding Window Buffers
------------------------------------------------------------------------------

-- These buffered versions could be faster than concurrent incremental folds of
-- all overlapping windows as in many cases we may not need all the values to
-- compute the fold, we can just compute the result using the old value and new
-- value.  However, we may need the buffer once in a while, for example for
-- string search we usually compute the hash incrementally but when the hash
-- matches the hash of the pattern we need to compare the whole string.
--
-- XXX we should be able to implement sequence based splitting combinators
-- using this combinator.

{-
-- | Buffer n elements of the input in a ring buffer. When t new elements are
-- collected, slide the window to remove the same number of oldest elements,
-- insert the new elements, and apply an incremental fold on the sliding
-- window, supplying the outgoing elements, the new ring buffer as arguments.
slidingChunkBuffer
    :: (IsStream t, Monad m, Ord a, Storable a)
    => Int -- window size
    -> Int -- window slide
    -> Fold m (Ring a, Array a) b
    -> t m a
    -> t m b
slidingChunkBuffer = undefined

-- Buffer n seconds worth of stream elements of the input in a radix tree.
-- Every t seconds, remove the items that are older than n seconds, and apply
-- an incremental fold on the sliding window, supplying the outgoing elements,
-- and the new radix tree buffer as arguments.
slidingSessionBuffer
    :: (IsStream t, Monad m, Ord a, Storable a)
    => Int    -- window size
    -> Int    -- tick size
    -> Fold m (RTree a, Array a) b
    -> t m a
    -> t m b
slidingSessionBuffer = undefined
-}

------------------------------------------------------------------------------
-- Keyed Session Windows
------------------------------------------------------------------------------

{-
-- | Keyed variable size space windows. Close the window if we do not receive a
-- window event in the next "spaceout" elements.
{-# INLINABLE classifyChunksBy #-}
classifyChunksBy
    :: (IsStream t, MonadAsync m, Ord k)
    => Int   -- ^ window spaceout (spread)
    -> Bool  -- ^ reset the spaceout when a chunk window element is received
    -> Fold m a b       -- ^ Fold to be applied to chunk window elements
    -> t m (k, a, Bool) -- ^ chunk key, data, last element
    -> t m (k, b)
classifyChunksBy spanout reset (Fold step initial extract) str = undefined

-- | Like 'classifyChunksOf' but the chunk size is reset if an element is
-- received within the chunk size window. The chunk gets closed only if no
-- element is received within the chunk window.
--
{-# INLINABLE classifyKeepAliveChunks #-}
classifyKeepAliveChunks
    :: (IsStream t, MonadAsync m, Ord k)
    => Int   -- ^ window spaceout (spread)
    -> Fold m a b       -- ^ Fold to be applied to chunk window elements
    -> t m (k, a, Bool) -- ^ chunk key, data, last element
    -> t m (k, b)
classifyKeepAliveChunks spanout = classifyChunksBy spanout True
-}

-- | @classifySessionsBy tick timeout reset f stream@ groups together all input
-- stream elements that belong to the same session. @timeout@ is the maximum
-- lifetime of a session in seconds. All elements belonging to a session are
-- purged after this duration.  If "reset" is 'Ture' then the timeout is reset
-- after every event received in the session. Session duration is measured
-- using the timestamp of the first element seen for that session.  To detect
-- session timeouts, a monotonic event time clock is maintained using the
-- timestamps seen in the inputs and a timer with a tick duration specified by
-- @tick@.
--
-- @session key@ is a key that uniquely identifies the session for the given
-- element, @timestamp@ characterizes the time when the input element was
-- generated, this is an absolute time measured from some @Epoch@. @session
-- close@ is a boolean indicating whether this element marks the closing of the
-- session. When an input element with @session close@ set to @True@ is seen
-- the session is purged immediately.
--
-- All the input elements belonging to a session are collected using the fold
-- @f@.  The session key and the fold result are emitted in the output stream
-- when the session is purged either via the session close event or via the
-- session liftime timeout.
--
-- @since 0.7.0
{-# INLINABLE classifySessionsBy #-}
classifySessionsBy
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ timer tick in seconds
    -> Double         -- ^ session timeout
    -> Bool           -- ^ reset the timeout when an event is received
    -> Fold m a b     -- ^ Fold to be applied to session events
    -> t m (k, a, Bool, AbsTime) -- ^ session key, timestamp, close event, data
    -> t m (k, b)
classifySessionsBy tick timeout reset (Fold step initial extract) str =
    S.concatMap (\(Tuple4' _ _ _ s) -> s) $ S.scanlM' sstep szero stream

    where

    timeoutMs = toRelTime (round (timeout * 1000) :: MilliSecond64)
    tickMs = toRelTime (round (tick * 1000) :: MilliSecond64)
    szero = Tuple4' (toAbsTime (0 :: MilliSecond64)) H.empty Map.empty S.nil

    -- Got a new stream input element
    sstep (Tuple4' evTime hp mp _) (Just (key, a, closing, ts)) =
        -- XXX we should use a heap in pinned memory to scale it to a large
        -- size
        --
        -- deleting a key from the heap is expensive, so we never delete a
        -- key, we just purge it from the Map and it gets purged from the
        -- heap on timeout. We just need an extra lookup in the Map when
        -- the key is purged from the heap, that should not be expensive.
        --
        -- To detect session inactivity we keep a timestamp of the latest event
        -- in the Map along with the fold result.  When we purge the session
        -- from the heap we match the timestamp in the heap with the timestamp
        -- in the Map, if the latest timestamp is newer and has not expired we
        -- reinsert the key in the heap.
        --
        -- XXX if the key is an Int, we can also use an IntMap for slightly
        -- better performance.
        --
        let accumulate v = do
                Tuple' _ old <- maybe (initial >>= return . Tuple' ts) return v
                new <- step old a
                return $ Tuple' ts new
        in if closing
           then do
                let (r, mp') = Map.updateLookupWithKey (\_ _ -> Nothing) key mp
                Tuple' _ acc <- accumulate r
                res <- extract acc
                return $ Tuple4' evTime hp mp' (S.yield (key, res))
           else do
                    let r = Map.lookup key mp
                    acc <- accumulate r
                    let mp' = Map.insert key acc mp
                    let hp' =
                            case r of
                                Nothing ->
                                    let expiry = addToAbsTime ts timeoutMs
                                    in H.insert (Entry expiry key) hp
                                Just _ -> hp
                    -- Event time is maintained as monotonically increasing
                    -- time. If we have lagged behind any of the timestamps
                    -- seen then we increase it to match the latest time seen
                    -- in the timestamps. We also increase it on timer ticks.
                    return $ Tuple4' (max evTime ts) hp' mp' S.nil

    -- Got a timer tick event
    -- XXX can we yield the entries without accumulating them?
    sstep (Tuple4' evTime heap sessions _) Nothing = do
        (hp', mp', out) <- go heap sessions S.nil
        return $ Tuple4' curTime hp' mp' out

        where

        curTime = addToAbsTime evTime tickMs
        go hp mp out = do
            let hres = H.uncons hp
            case hres of
                Just (Entry ts key, hp') -> do
                    let duration = diffAbsTime curTime ts
                    if duration >= timeoutMs
                    then do
                        let (r, mp') = Map.updateLookupWithKey
                                            (\_ _ -> Nothing) key mp
                        case r of
                            Nothing -> go hp' mp' out
                            Just (Tuple' latestTS acc) -> do
                                let dur = diffAbsTime curTime latestTS
                                if dur >= timeoutMs || not reset
                                then do
                                    sess <- extract acc
                                    go hp' mp' ((key, sess) `S.cons` out)
                                else
                                    -- reset the session timeout
                                    let expiry = addToAbsTime latestTS timeoutMs
                                        hp'' = H.insert (Entry expiry key) hp'
                                        mp'' = Map.insert key (Tuple' latestTS acc) mp'
                                    in go hp'' mp'' out
                    else return (hp, mp, out)
                Nothing -> return (hp, mp, out)

    -- merge timer events in the stream
    stream = S.map Just str `parallel` S.repeatM timer
    timer = do
        liftIO $ threadDelay (round $ tick * 1000000)
        return Nothing

-- | Like 'classifySessionsOf' but the session is kept alive if an event is
-- received within the session window. The session times out and gets closed
-- only if no event is received within the specified session window size.
--
-- @since 0.7.0
{-# INLINABLE _classifyKeepAliveSessions #-}
_classifyKeepAliveSessions
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ session inactive timeout
    -> Fold m a b     -- ^ Fold to be applied to session payload data
    -> t m (k, a, Bool, AbsTime) -- ^ session key, data, close flag, timestamp
    -> t m (k, b)
_classifyKeepAliveSessions timeout = classifySessionsBy 1 timeout True

------------------------------------------------------------------------------
-- Keyed tumbling windows
------------------------------------------------------------------------------

-- Tumbling windows is a special case of sliding windows where the window slide
-- is the same as the window size. Or it can be a special case of session
-- windows where the reset flag is set to False.

-- XXX instead of using the early termination flag in the stream, we can use an
-- early terminating fold instead.

{-
-- | Split the stream into fixed size chunks of specified size. Within each
-- such chunk fold the elements in buckets identified by the keys. A particular
-- bucket fold can be terminated early if a closing flag is encountered in an
-- element for that key.
--
-- @since 0.7.0
{-# INLINABLE classifyChunksOf #-}
classifyChunksOf
    :: (IsStream t, MonadAsync m, Ord k)
    => Int              -- ^ window size
    -> Fold m a b       -- ^ Fold to be applied to window events
    -> t m (k, a, Bool) -- ^ window key, data, close event
    -> t m (k, b)
classifyChunksOf wsize = classifyChunksBy wsize False
-}

-- | Split the stream into fixed size time windows of specified interval in
-- seconds. Within each such window, fold the elements in buckets identified by
-- the keys. A particular bucket fold can be terminated early if a closing flag
-- is encountered in an element for that key. Once a fold is terminated the key
-- and value for that bucket are emitted in the output stream.
--
-- Session @timestamp@ in the input stream is an absolute time from some epoch,
-- characterizing the time when the input element was generated.  To detect
-- session window end, a monotonic event time clock is maintained synced with
-- the timestamps with a clock resolution of 1 second.
--
-- @since 0.7.0
{-# INLINABLE _classifySessionsOf #-}
_classifySessionsOf
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ time window size
    -> Fold m a b     -- ^ Fold to be applied to window events
    -> t m (k, a, Bool, AbsTime) -- ^ window key, data, close flag, timestamp
    -> t m (k, b)
_classifySessionsOf interval = classifySessionsBy 1 interval False
