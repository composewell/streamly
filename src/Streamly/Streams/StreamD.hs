{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE MagicHash                 #-}

#if __GLASGOW_HASKELL__ >= 801
{-# LANGUAGE TypeApplications          #-}
#endif

#include "inline.hs"

-- |
-- Module      : Streamly.Streams.StreamD
-- Copyright   : (c) 2018 Harendra Kumar
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- Copyright   : (c) The University of Glasgow, 2009
-- Copyright   : (c) Bjoern Hoehrmann 2008-2009
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Direct style re-implementation of CPS style stream in StreamK module.  The
-- symbol or suffix 'D' in this module denotes the "Direct" style.  GHC is able
-- to INLINE and fuse direct style better, providing better performance than
-- CPS implementation.
--
-- @
-- import qualified Streamly.Streams.StreamD as D
-- @

-- Some of the functions in this file have been adapted from the vector
-- library,  https://hackage.haskell.org/package/vector.

module Streamly.Streams.StreamD
    (
    -- * The stream type
      Step (..)
    , Stream (..)

    -- * Construction
    , nil
    , cons

    -- * Deconstruction
    , uncons

    -- * Generation
    -- ** Unfolds
    , unfoldr
    , unfoldrM

    -- ** Specialized Generation
    -- | Generate a monadic stream from a seed.
    , repeat
    , replicate
    , replicateM
    , fromIndices
    , fromIndicesM
    , generate
    , generateM

    -- ** Enumerations
    , enumerateFromStepIntegral
    , enumerateFromIntegral
    , enumerateFromThenIntegral
    , enumerateFromToIntegral
    , enumerateFromThenToIntegral

    , enumerateFromStepNum
    , numFrom
    , numFromThen
    , enumerateFromToFractional
    , enumerateFromThenToFractional

    -- ** Conversions
    -- | Transform an input structure into a stream.
    -- | Direct style stream does not support @fromFoldable@.
    , yield
    , yieldM
    , fromList
    , fromListM
    , fromStreamK
    , fromStreamD

    -- * Elimination
    -- ** General Folds
    , foldrS
    , foldrT
    , foldrM
    , foldrMx
    , foldr
    , foldr1

    , foldl'
    , foldlM'
    , foldlS
    , foldlT
    , reverse
    , reverse'

    , foldlx'
    , foldlMx'

    -- ** Specialized Folds
    , tap
    , drain
    , null
    , head
    , tail
    , last
    , elem
    , notElem
    , all
    , any
    , maximum
    , maximumBy
    , minimum
    , minimumBy
    , findIndices
    , lookup
    , findM
    , find
    , (!!)

    -- ** Flattening nested streams
    , concatMapM
    , concatMap
    , ConcatMapUState (..)
    , concatMapU
    , ConcatUnfoldInterleaveState (..)
    , concatUnfoldInterleave
    , concatUnfoldRoundrobin
    , AppendState(..)
    , append
    , InterleaveState(..)
    , interleave
    , interleaveMin
    , interleaveFst
    , roundRobin -- interleaveFair?/ParallelFair
    , interleaveFstThenConcat

    -- ** Grouping
    , groupsOf
    , groupsBy
    , groupsRollingBy

    -- ** Splitting
    , splitBy
    , splitSuffixBy
    , wordsBy
    , splitSuffixBy'

    , splitOn
    , splitSuffixOn

    , splitInnerBy
    , splitInnerBySuffix

    -- ** Substreams
    , isPrefixOf
    , isSubsequenceOf
    , stripPrefix

    -- ** Map and Fold
    , mapM_

    -- ** Conversions
    -- | Transform a stream into another type.
    , toList
    , toListRev
    , toStreamK
    , toStreamD

    -- * Transformation
    , transform

    -- ** By folding (scans)
    , scanlM'
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

    , postscanlx'
    , postscanlMx'
    , scanlMx'
    , scanlx'

    -- * Filtering
    , filter
    , filterM
    , uniq
    , take
    , takeWhile
    , takeWhileM
    , drop
    , dropWhile
    , dropWhileM

    -- * Mapping
    , map
    , mapM
    , sequence

    -- * Inserting
    , intersperseM
    , intersperse
    , insertAfterEach
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

    -- * Exceptions
    , before
    , after
    , bracket
    , onException
    , finally
    , handle

    -- * UTF8 Encoding / Decoding transformations.
    , decodeUtf8
    , encodeUtf8
    , decodeUtf8Lenient
    , decodeUtf8Arrays
    , decodeUtf8ArraysLenient
    )
where

import Control.Exception (Exception)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans (MonadTrans(lift))
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
#if __GLASGOW_HASKELL__ >= 801
import Data.Functor.Identity (Identity)
#endif
import Data.Maybe (fromJust, isJust)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import GHC.Base (assert, Char(..), unsafeChr, ord)
import GHC.IO.Encoding.Failure (isSurrogate)
import GHC.Types (SPEC(..))
import GHC.Word (Word8(..))
import Prelude
       hiding (map, mapM, mapM_, repeat, foldr, last, take, filter,
               takeWhile, drop, dropWhile, all, any, maximum, minimum, elem,
               notElem, null, head, tail, zipWith, lookup, foldr1, sequence,
               (!!), scanl, scanl1, concatMap, replicate, enumFromTo, concat,
               reverse)

import qualified Control.Monad.Catch as MC

import Streamly.Memory.Array.Types (Array(..))
import Streamly.Fold.Types (Fold(..))
import Streamly.Pipe.Types (Pipe(..), PipeState(..))
import Streamly.SVar (MonadAsync, defState, adaptState)
import Streamly.Unfold.Types (Unfold(..))
import Streamly.Strict (Tuple'(..))

import Streamly.Streams.StreamD.Type

import qualified Streamly.Pipe.Types as Pipe
import qualified Streamly.Memory.Array.Types as A
import qualified Streamly.Memory.Ring as RB
import qualified Streamly.Streams.StreamK as K

import Foreign.Ptr (plusPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr (ForeignPtr, touchForeignPtr)

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

-- | An empty 'Stream'.
{-# INLINE_NORMAL nil #-}
nil :: Monad m => Stream m a
nil = Stream (\_ _ -> return Stop) ()

{-# INLINE_NORMAL consM #-}
consM :: Monad m => m a -> Stream m a -> Stream m a
consM m (Stream step state) = Stream step1 Nothing
    where
    {-# INLINE_LATE step1 #-}
    step1 _ Nothing   = m >>= \x -> return $ Yield x (Just state)
    step1 gst (Just st) = do
        r <- step gst st
        return $
          case r of
            Yield a s -> Yield a (Just s)
            Skip  s   -> Skip (Just s)
            Stop      -> Stop

-- XXX implement in terms of consM?
-- cons x = consM (return x)
--
-- | Can fuse but has O(n^2) complexity.
{-# INLINE_NORMAL cons #-}
cons :: Monad m => a -> Stream m a -> Stream m a
cons x (Stream step state) = Stream step1 Nothing
    where
    {-# INLINE_LATE step1 #-}
    step1 _ Nothing   = return $ Yield x (Just state)
    step1 gst (Just st) = do
        r <- step gst st
        return $
          case r of
            Yield a s -> Yield a (Just s)
            Skip  s   -> Skip (Just s)
            Stop      -> Stop

-------------------------------------------------------------------------------
-- Deconstruction
-------------------------------------------------------------------------------

-- Does not fuse, has the same performance as the StreamK version.
{-# INLINE_NORMAL uncons #-}
uncons :: Monad m => Stream m a -> m (Maybe (a, Stream m a))
uncons (UnStream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield x s -> return $ Just (x, Stream step s)
            Skip  s   -> go s
            Stop      -> return Nothing

------------------------------------------------------------------------------
-- Generation by unfold
------------------------------------------------------------------------------

{-# INLINE_NORMAL unfoldrM #-}
unfoldrM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Stream m a
unfoldrM next state = Stream step state
  where
    {-# INLINE_LATE step #-}
    step _ st = do
        r <- next st
        return $ case r of
            Just (x, s) -> Yield x s
            Nothing     -> Stop

{-# INLINE_LATE unfoldr #-}
unfoldr :: Monad m => (s -> Maybe (a, s)) -> s -> Stream m a
unfoldr f = unfoldrM (return . f)

------------------------------------------------------------------------------
-- Specialized Generation
------------------------------------------------------------------------------

repeat :: Monad m => a -> Stream m a
repeat x = Stream (\_ _ -> return $ Yield x ()) ()

{-# INLINE_NORMAL replicateM #-}
replicateM :: forall m a. Monad m => Int -> m a -> Stream m a
replicateM n p = Stream step n
  where
    {-# INLINE_LATE step #-}
    step _ (i :: Int)
      | i <= 0    = return Stop
      | otherwise = do
          x <- p
          return $ Yield x (i - 1)

{-# INLINE_NORMAL replicate #-}
replicate :: Monad m => Int -> a -> Stream m a
replicate n x = replicateM n (return x)

-- This would not work properly for floats, therefore we put an Integral
-- constraint.
-- | Can be used to enumerate unbounded integrals. This does not check for
-- overflow or underflow for bounded integrals.
{-# INLINE_NORMAL enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: (Integral a, Monad m) => a -> a -> Stream m a
enumerateFromStepIntegral from stride =
    from `seq` stride `seq` Stream step from
    where
        {-# INLINE_LATE step #-}
        step _ !x = return $ Yield x $! (x + stride)

-- We are assuming that "to" is constrained by the type to be within
-- max/min bounds.
{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: (Monad m, Integral a) => a -> a -> Stream m a
enumerateFromToIntegral from to =
    takeWhile (<= to) $ enumerateFromStepIntegral from 1

{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: (Monad m, Integral a, Bounded a) => a -> Stream m a
enumerateFromIntegral from = enumerateFromToIntegral from maxBound

data EnumState a = EnumInit | EnumYield a a a | EnumStop

{-# INLINE_NORMAL enumerateFromThenToIntegralUp #-}
enumerateFromThenToIntegralUp
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegralUp from next to = Stream step EnumInit
    where
    {-# INLINE_LATE step #-}
    step _ EnumInit =
        return $
            if to < next
            then if to < from
                 then Stop
                 else Yield from EnumStop
            else -- from <= next <= to
                let stride = next - from
                in Skip $ EnumYield from stride (to - stride)

    step _ (EnumYield x stride toMinus) =
        return $
            if x > toMinus
            then Yield x EnumStop
            else Yield x $ EnumYield (x + stride) stride toMinus

    step _ EnumStop = return Stop

{-# INLINE_NORMAL enumerateFromThenToIntegralDn #-}
enumerateFromThenToIntegralDn
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegralDn from next to = Stream step EnumInit
    where
    {-# INLINE_LATE step #-}
    step _ EnumInit =
        return $ if to > next
            then if to > from
                 then Stop
                 else Yield from EnumStop
            else -- from >= next >= to
                let stride = next - from
                in Skip $ EnumYield from stride (to - stride)

    step _ (EnumYield x stride toMinus) =
        return $
            if x < toMinus
            then Yield x EnumStop
            else Yield x $ EnumYield (x + stride) stride toMinus

    step _ EnumStop = return Stop

{-# INLINE_NORMAL enumerateFromThenToIntegral #-}
enumerateFromThenToIntegral
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegral from next to
    | next >= from = enumerateFromThenToIntegralUp from next to
    | otherwise    = enumerateFromThenToIntegralDn from next to

{-# INLINE_NORMAL enumerateFromThenIntegral #-}
enumerateFromThenIntegral
    :: (Monad m, Integral a, Bounded a)
    => a -> a -> Stream m a
enumerateFromThenIntegral from next =
    if next > from
    then enumerateFromThenToIntegralUp from next maxBound
    else enumerateFromThenToIntegralDn from next minBound

-- For floating point numbers if the increment is less than the precision then
-- it just gets lost. Therefore we cannot always increment it correctly by just
-- repeated addition.
-- 9007199254740992 + 1 + 1 :: Double => 9.007199254740992e15
-- 9007199254740992 + 2     :: Double => 9.007199254740994e15

-- Instead we accumulate the increment counter and compute the increment
-- everytime before adding it to the starting number.
--
-- This works for Integrals as well as floating point numbers, but
-- enumerateFromStepIntegral is faster for integrals.
{-# INLINE_NORMAL enumerateFromStepNum #-}
enumerateFromStepNum :: (Monad m, Num a) => a -> a -> Stream m a
enumerateFromStepNum from stride = Stream step 0
    where
    {-# INLINE_LATE step #-}
    step _ !i = return $ (Yield $! (from + i * stride)) $! (i + 1)

{-# INLINE_NORMAL numFrom #-}
numFrom :: (Monad m, Num a) => a -> Stream m a
numFrom from = enumerateFromStepNum from 1

{-# INLINE_NORMAL numFromThen #-}
numFromThen :: (Monad m, Num a) => a -> a -> Stream m a
numFromThen from next = enumerateFromStepNum from (next - from)

-- We cannot write a general function for Num.  The only way to write code
-- portable between the two is to use a 'Real' constraint and convert between
-- Fractional and Integral using fromRational which is horribly slow.
{-# INLINE_NORMAL enumerateFromToFractional #-}
enumerateFromToFractional
    :: (Monad m, Fractional a, Ord a)
    => a -> a -> Stream m a
enumerateFromToFractional from to =
    takeWhile (<= to + 1 / 2) $ enumerateFromStepNum from 1

{-# INLINE_NORMAL enumerateFromThenToFractional #-}
enumerateFromThenToFractional
    :: (Monad m, Fractional a, Ord a)
    => a -> a -> a -> Stream m a
enumerateFromThenToFractional from next to =
    takeWhile predicate $ numFromThen from next
    where
    mid = (next - from) / 2
    predicate | next >= from  = (<= to + mid)
              | otherwise     = (>= to + mid)

-------------------------------------------------------------------------------
-- Generation by Conversion
-------------------------------------------------------------------------------

{-# INLINE_NORMAL fromIndicesM #-}
fromIndicesM :: Monad m => (Int -> m a) -> Stream m a
fromIndicesM gen = Stream step 0
  where
    {-# INLINE_LATE step #-}
    step _ i = do
       x <- gen i
       return $ Yield x (i + 1)

{-# INLINE fromIndices #-}
fromIndices :: Monad m => (Int -> a) -> Stream m a
fromIndices gen = fromIndicesM (return . gen)

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

-- XXX we need the MonadAsync constraint because of a rewrite rule.
-- | Convert a list of monadic actions to a 'Stream'
{-# INLINE_LATE fromListM #-}
fromListM :: MonadAsync m => [m a] -> Stream m a
fromListM = Stream step
  where
    {-# INLINE_LATE step #-}
    step _ (m:ms) = m >>= \x -> return $ Yield x ms
    step _ []     = return Stop

{-# INLINE toStreamD #-}
toStreamD :: (K.IsStream t, Monad m) => t m a -> Stream m a
toStreamD = fromStreamK . K.toStream

------------------------------------------------------------------------------
-- Elimination by Folds
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Right Folds
------------------------------------------------------------------------------

{-# INLINE_NORMAL foldr1 #-}
foldr1 :: Monad m => (a -> a -> a) -> Stream m a -> m (Maybe a)
foldr1 f m = do
     r <- uncons m
     case r of
         Nothing   -> return Nothing
         Just (h, t) -> fmap Just (foldr f h t)

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
-- Specialized Folds
------------------------------------------------------------------------------

-- | Run a streaming composition, discard the results.
{-# INLINE_LATE drain #-}
drain :: Monad m => Stream m a -> m ()
-- drain = foldrM (\_ xs -> xs) (return ())
drain (Stream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield _ s -> go SPEC s
            Skip s    -> go SPEC s
            Stop      -> return ()

{-# INLINE_NORMAL null #-}
null :: Monad m => Stream m a -> m Bool
null m = foldrM (\_ _ -> return False) (return True) m

{-# INLINE_NORMAL head #-}
head :: Monad m => Stream m a -> m (Maybe a)
head m = foldrM (\x _ -> return (Just x)) (return Nothing) m

-- Does not fuse, has the same performance as the StreamK version.
{-# INLINE_NORMAL tail #-}
tail :: Monad m => Stream m a -> m (Maybe (Stream m a))
tail (UnStream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield _ s -> return (Just $ Stream step s)
            Skip  s   -> go s
            Stop      -> return Nothing

-- XXX will it fuse? need custom impl?
{-# INLINE_NORMAL last #-}
last :: Monad m => Stream m a -> m (Maybe a)
last = foldl' (\_ y -> Just y) Nothing

{-# INLINE_NORMAL elem #-}
elem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
-- elem e m = foldrM (\x xs -> if x == e then return True else xs) (return False) m
elem e (Stream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield x s
              | x == e    -> return True
              | otherwise -> go s
            Skip s -> go s
            Stop   -> return False

{-# INLINE_NORMAL notElem #-}
notElem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
notElem e s = fmap not (elem e s)

{-# INLINE_NORMAL all #-}
all :: Monad m => (a -> Bool) -> Stream m a -> m Bool
-- all p m = foldrM (\x xs -> if p x then xs else return False) (return True) m
all p (Stream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield x s
              | p x       -> go s
              | otherwise -> return False
            Skip s -> go s
            Stop   -> return True

{-# INLINE_NORMAL any #-}
any :: Monad m => (a -> Bool) -> Stream m a -> m Bool
-- any p m = foldrM (\x xs -> if p x then return True else xs) (return False) m
any p (Stream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield x s
              | p x       -> return True
              | otherwise -> go s
            Skip s -> go s
            Stop   -> return False

{-# INLINE_NORMAL maximum #-}
maximum :: (Monad m, Ord a) => Stream m a -> m (Maybe a)
maximum (Stream step state) = go Nothing state
  where
    go Nothing st = do
        r <- step defState st
        case r of
            Yield x s -> go (Just x) s
            Skip  s   -> go Nothing s
            Stop      -> return Nothing
    go (Just acc) st = do
        r <- step defState st
        case r of
            Yield x s
              | acc <= x  -> go (Just x) s
              | otherwise -> go (Just acc) s
            Skip s -> go (Just acc) s
            Stop   -> return (Just acc)

{-# INLINE_NORMAL maximumBy #-}
maximumBy :: Monad m => (a -> a -> Ordering) -> Stream m a -> m (Maybe a)
maximumBy cmp (Stream step state) = go Nothing state
  where
    go Nothing st = do
        r <- step defState st
        case r of
            Yield x s -> go (Just x) s
            Skip  s   -> go Nothing s
            Stop      -> return Nothing
    go (Just acc) st = do
        r <- step defState st
        case r of
            Yield x s -> case cmp acc x of
                GT -> go (Just acc) s
                _  -> go (Just x) s
            Skip s -> go (Just acc) s
            Stop   -> return (Just acc)

{-# INLINE_NORMAL minimum #-}
minimum :: (Monad m, Ord a) => Stream m a -> m (Maybe a)
minimum (Stream step state) = go Nothing state
  where
    go Nothing st = do
        r <- step defState st
        case r of
            Yield x s -> go (Just x) s
            Skip  s   -> go Nothing s
            Stop      -> return Nothing
    go (Just acc) st = do
        r <- step defState st
        case r of
            Yield x s
              | acc <= x  -> go (Just acc) s
              | otherwise -> go (Just x) s
            Skip s -> go (Just acc) s
            Stop   -> return (Just acc)

{-# INLINE_NORMAL minimumBy #-}
minimumBy :: Monad m => (a -> a -> Ordering) -> Stream m a -> m (Maybe a)
minimumBy cmp (Stream step state) = go Nothing state
  where
    go Nothing st = do
        r <- step defState st
        case r of
            Yield x s -> go (Just x) s
            Skip  s   -> go Nothing s
            Stop      -> return Nothing
    go (Just acc) st = do
        r <- step defState st
        case r of
            Yield x s -> case cmp acc x of
                GT -> go (Just x) s
                _  -> go (Just acc) s
            Skip s -> go (Just acc) s
            Stop   -> return (Just acc)

{-# INLINE_NORMAL (!!) #-}
(!!) :: (Monad m) => Stream m a -> Int -> m (Maybe a)
(Stream step state) !! i = go i state
  where
    go n st = do
        r <- step defState st
        case r of
            Yield x s | n < 0 -> return Nothing
                      | n == 0 -> return $ Just x
                      | otherwise -> go (n - 1) s
            Skip s -> go n s
            Stop   -> return Nothing

{-# INLINE_NORMAL lookup #-}
lookup :: (Monad m, Eq a) => a -> Stream m (a, b) -> m (Maybe b)
lookup e m = foldrM (\(a, b) xs -> if e == a then return (Just b) else xs)
                   (return Nothing) m

{-# INLINE_NORMAL findM #-}
findM :: Monad m => (a -> m Bool) -> Stream m a -> m (Maybe a)
findM p m = foldrM (\x xs -> p x >>= \r -> if r then return (Just x) else xs)
                   (return Nothing) m

{-# INLINE find #-}
find :: Monad m => (a -> Bool) -> Stream m a -> m (Maybe a)
find p = findM (return . p)

{-# INLINE_NORMAL findIndices #-}
findIndices :: Monad m => (a -> Bool) -> Stream m a -> Stream m Int
findIndices p (Stream step state) = Stream step' (state, 0)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, i) = do
      r <- step (adaptState gst) st
      return $ case r of
          Yield x s -> if p x then Yield i (s, i+1) else Skip (s, i+1)
          Skip s -> Skip (s, i+1)
          Stop   -> Stop

{-# INLINE toListRev #-}
toListRev :: Monad m => Stream m a -> m [a]
toListRev = foldl' (flip (:)) []

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
        xs <- toListRev m
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


------------------------------------------------------------------------------
-- Grouping/Splitting
------------------------------------------------------------------------------

{-# INLINE_NORMAL splitSuffixBy' #-}
splitSuffixBy' :: Monad m
    => (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
splitSuffixBy' predicate f (Stream step state) =
    Stream (stepOuter f) (Just state)

    where

    {-# INLINE_LATE stepOuter #-}
    stepOuter (Fold fstep initial done) gst (Just st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                acc <- initial
                acc' <- fstep acc x
                if (predicate x)
                then done acc' >>= \val -> return $ Yield val (Just s)
                else go SPEC s acc'

            Skip s    -> return $ Skip $ Just s
            Stop      -> return Stop

        where

        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    acc' <- fstep acc x
                    if (predicate x)
                    then done acc' >>= \val -> return $ Yield val (Just s)
                    else go SPEC s acc'
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \val -> return $ Yield val Nothing

    stepOuter _ _ Nothing = return Stop

{-# INLINE_NORMAL groupsBy #-}
groupsBy :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Stream m a
    -> Stream m b
groupsBy cmp f (Stream step state) = Stream (stepOuter f) (Just state, Nothing)

    where

    {-# INLINE_LATE stepOuter #-}
    stepOuter (Fold fstep initial done) gst (Just st, Nothing) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                acc <- initial
                acc' <- fstep acc x
                go SPEC x s acc'

            Skip s    -> return $ Skip $ (Just s, Nothing)
            Stop      -> return Stop

        where

        go !_ prev stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if cmp x prev
                    then do
                        acc' <- fstep acc x
                        go SPEC prev s acc'
                    else done acc >>= \r -> return $ Yield r (Just s, Just x)
                Skip s -> go SPEC prev s acc
                Stop -> done acc >>= \r -> return $ Yield r (Nothing, Nothing)

    stepOuter (Fold fstep initial done) gst (Just st, Just prev) = do
        acc <- initial
        acc' <- fstep acc prev
        go SPEC st acc'

        where

        -- XXX code duplicated from the previous equation
        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if cmp x prev
                    then do
                        acc' <- fstep acc x
                        go SPEC s acc'
                    else done acc >>= \r -> return $ Yield r (Just s, Just x)
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \r -> return $ Yield r (Nothing, Nothing)

    stepOuter _ _ (Nothing,_) = return Stop

{-# INLINE_NORMAL groupsRollingBy #-}
groupsRollingBy :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Stream m a
    -> Stream m b
groupsRollingBy cmp f (Stream step state) =
    Stream (stepOuter f) (Just state, Nothing)
    where

      {-# INLINE_LATE stepOuter #-}
      stepOuter (Fold fstep initial done) gst (Just st, Nothing) = do
          res <- step (adaptState gst) st
          case res of
              Yield x s -> do
                  acc <- initial
                  acc' <- fstep acc x
                  go SPEC x s acc'

              Skip s    -> return $ Skip $ (Just s, Nothing)
              Stop      -> return Stop

        where
          go !_ prev stt !acc = do
              res <- step (adaptState gst) stt
              case res of
                  Yield x s -> do
                      if cmp prev x
                        then do
                          acc' <- fstep acc x
                          go SPEC x s acc'
                        else
                          done acc >>= \r -> return $ Yield r (Just s, Just x)
                  Skip s -> go SPEC prev s acc
                  Stop -> done acc >>= \r -> return $ Yield r (Nothing, Nothing)

      stepOuter (Fold fstep initial done) gst (Just st, Just prev') = do
          acc <- initial
          acc' <- fstep acc prev'
          go SPEC prev' st acc'

        where
          go !_ prevv stt !acc = do
              res <- step (adaptState gst) stt
              case res of
                  Yield x s -> do
                      if cmp prevv x
                      then do
                          acc' <- fstep acc x
                          go SPEC x s acc'
                      else done acc >>= \r -> return $ Yield r (Just s, Just x)
                  Skip s -> go SPEC prevv s acc
                  Stop -> done acc >>= \r -> return $ Yield r (Nothing, Nothing)

      stepOuter _ _ (Nothing, _) = return Stop

{-# INLINE_NORMAL splitBy #-}
splitBy :: Monad m => (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
splitBy predicate f (Stream step state) = Stream (step' f) (Just state)

    where

    {-# INLINE_LATE step' #-}
    step' (Fold fstep initial done) gst (Just st) = initial >>= go SPEC st

        where

        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if predicate x
                    then done acc >>= \r -> return $ Yield r (Just s)
                    else do
                        acc' <- fstep acc x
                        go SPEC s acc'
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \r -> return $ Yield r Nothing

    step' _ _ Nothing = return Stop

-- XXX requires -funfolding-use-threshold=150 in lines-unlines benchmark
{-# INLINE_NORMAL splitSuffixBy #-}
splitSuffixBy :: Monad m
    => (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
splitSuffixBy predicate f (Stream step state) = Stream (step' f) (Just state)

    where

    {-# INLINE_LATE step' #-}
    step' (Fold fstep initial done) gst (Just st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                acc <- initial
                if predicate x
                then done acc >>= \val -> return $ Yield val (Just s)
                else do
                    acc' <- fstep acc x
                    go SPEC s acc'

            Skip s    -> return $ Skip $ Just s
            Stop      -> return Stop

        where

        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if predicate x
                    then done acc >>= \r -> return $ Yield r (Just s)
                    else do
                        acc' <- fstep acc x
                        go SPEC s acc'
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \r -> return $ Yield r Nothing

    step' _ _ Nothing = return Stop

{-# INLINE_NORMAL wordsBy #-}
wordsBy :: Monad m => (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
wordsBy predicate f (Stream step state) = Stream (stepOuter f) (Just state)

    where

    {-# INLINE_LATE stepOuter #-}
    stepOuter (Fold fstep initial done) gst (Just st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                if predicate x
                then return $ Skip (Just s)
                else do
                    acc <- initial
                    acc' <- fstep acc x
                    go SPEC s acc'

            Skip s    -> return $ Skip $ Just s
            Stop      -> return Stop

        where

        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if predicate x
                    then done acc >>= \r -> return $ Yield r (Just s)
                    else do
                        acc' <- fstep acc x
                        go SPEC s acc'
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \r -> return $ Yield r Nothing

    stepOuter _ _ Nothing = return Stop

-- String search algorithms:
-- http://www-igm.univ-mlv.fr/~lecroq/string/index.html

{-
-- TODO can we unify the splitting operations using a splitting configuration
-- like in the split package.
--
data SplitStyle = Infix | Suffix | Prefix deriving (Eq, Show)

data SplitOptions = SplitOptions
    { style    :: SplitStyle
    , withSep  :: Bool  -- ^ keep the separators in output
    -- , compact  :: Bool  -- ^ treat multiple consecutive separators as one
    -- , trimHead :: Bool  -- ^ drop blank at head
    -- , trimTail :: Bool  -- ^ drop blank at tail
    }
-}

data SplitOnState s a =
      GO_START
    | GO_EMPTY_PAT s
    | GO_SINGLE_PAT s a
    | GO_SHORT_PAT s
    | GO_KARP_RABIN s !(RB.Ring a) !(Ptr a)
    | GO_DONE

{-# INLINE_NORMAL splitOn #-}
splitOn
    :: forall m a b. (MonadIO m, Storable a, Enum a, Eq a)
    => Array a
    -> Fold m a b
    -> Stream m a
    -> Stream m b
splitOn patArr@Array{..} (Fold fstep initial done) (Stream step state) =
    Stream stepOuter GO_START

    where

    patLen = A.length patArr
    maxIndex = patLen - 1
    elemBits = sizeOf (undefined :: a) * 8

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ GO_START =
        if patLen == 0
        then return $ Skip $ GO_EMPTY_PAT state
        else if patLen == 1
            then do
                r <- liftIO $ (A.unsafeIndexIO patArr 0)
                return $ Skip $ GO_SINGLE_PAT state r
            else if sizeOf (undefined :: a) * patLen
                    <= sizeOf (undefined :: Word)
                then return $ Skip $ GO_SHORT_PAT state
                else do
                    (rb, rhead) <- liftIO $ RB.new patLen
                    return $ Skip $ GO_KARP_RABIN state rb rhead

    stepOuter gst (GO_SINGLE_PAT stt pat) = initial >>= go SPEC stt

        where

        go !_ st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    if pat == x
                    then do
                        r <- done acc
                        return $ Yield r (GO_SINGLE_PAT s pat)
                    else fstep acc x >>= go SPEC s
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \r -> return $ Yield r GO_DONE

    stepOuter gst (GO_SHORT_PAT stt) = initial >>= go0 SPEC 0 (0 :: Word) stt

        where

        mask :: Word
        mask = (1 `shiftL` (elemBits * patLen)) - 1

        addToWord wrd a = (wrd `shiftL` elemBits) .|. fromIntegral (fromEnum a)

        patWord :: Word
        patWord = mask .&. A.foldl' addToWord 0 patArr

        go0 !_ !idx wrd st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd' = addToWord wrd x
                    if idx == maxIndex
                    then do
                        if wrd' .&. mask == patWord
                        then do
                            r <- done acc
                            return $ Yield r (GO_SHORT_PAT s)
                        else go1 SPEC wrd' s acc
                    else go0 SPEC (idx + 1) wrd' s acc
                Skip s -> go0 SPEC idx wrd s acc
                Stop -> do
                    acc' <- if idx /= 0
                            then go2 wrd idx acc
                            else return acc
                    done acc' >>= \r -> return $ Yield r GO_DONE

        {-# INLINE go1 #-}
        go1 !_ wrd st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd' = addToWord wrd x
                        old = (mask .&. wrd) `shiftR` (elemBits * (patLen - 1))
                    acc' <- fstep acc (toEnum $ fromIntegral old)
                    if wrd' .&. mask == patWord
                    then done acc' >>= \r -> return $ Yield r (GO_SHORT_PAT s)
                    else go1 SPEC wrd' s acc'
                Skip s -> go1 SPEC wrd s acc
                Stop -> do
                    acc' <- go2 wrd patLen acc
                    done acc' >>= \r -> return $ Yield r GO_DONE

        go2 !wrd !n !acc | n > 0 = do
            let old = (mask .&. wrd) `shiftR` (elemBits * (n - 1))
            fstep acc (toEnum $ fromIntegral old) >>= go2 wrd (n - 1)
        go2 _ _ acc = return acc

    stepOuter gst (GO_KARP_RABIN stt rb rhead) = do
        initial >>= go0 SPEC 0 rhead stt

        where

        k = 2891336453 :: Word32
        coeff = k ^ patLen
        addCksum cksum a = cksum * k + fromIntegral (fromEnum a)
        deltaCksum cksum old new =
            addCksum cksum new - coeff * fromIntegral (fromEnum old)

        -- XXX shall we use a random starting hash or 1 instead of 0?
        patHash = A.foldl' addCksum 0 patArr

        -- rh == ringHead
        go0 !_ !idx !rh st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    rh' <- liftIO $ RB.unsafeInsert rb rh x
                    if idx == maxIndex
                    then do
                        let fold = RB.unsafeFoldRing (RB.ringBound rb)
                        let !ringHash = fold addCksum 0 rb
                        if ringHash == patHash
                        then go2 SPEC ringHash rh' s acc
                        else go1 SPEC ringHash rh' s acc
                    else go0 SPEC (idx + 1) rh' s acc
                Skip s -> go0 SPEC idx rh s acc
                Stop -> do
                    !acc' <- if idx /= 0
                             then RB.unsafeFoldRingM rh fstep acc rb
                             else return acc
                    done acc' >>= \r -> return $ Yield r GO_DONE

        -- XXX Theoretically this code can do 4 times faster if GHC generates
        -- optimal code. If we use just "(cksum' == patHash)" condition it goes
        -- 4x faster, as soon as we add the "RB.unsafeEqArray rb v" condition
        -- the generated code changes drastically and becomes 4x slower. Need
        -- to investigate what is going on with GHC.
        {-# INLINE go1 #-}
        go1 !_ !cksum !rh st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    old <- liftIO $ peek rh
                    let cksum' = deltaCksum cksum old x
                    acc' <- fstep acc old

                    if (cksum' == patHash)
                    then do
                        rh' <- liftIO (RB.unsafeInsert rb rh x)
                        go2 SPEC cksum' rh' s acc'
                    else do
                        rh' <- liftIO (RB.unsafeInsert rb rh x)
                        go1 SPEC cksum' rh' s acc'
                Skip s -> go1 SPEC cksum rh s acc
                Stop -> do
                    acc' <- RB.unsafeFoldRingFullM rh fstep acc rb
                    done acc' >>= \r -> return $ Yield r GO_DONE

        go2 !_ !cksum' !rh' s !acc' = do
            if RB.unsafeEqArray rb rh' patArr
            then do
                r <- done acc'
                return $ Yield r (GO_KARP_RABIN s rb rhead)
            else go1 SPEC cksum' rh' s acc'

    stepOuter gst (GO_EMPTY_PAT st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                acc <- initial
                acc' <- fstep acc x
                done acc' >>= \r -> return $ Yield r (GO_EMPTY_PAT s)
            Skip s -> return $ Skip (GO_EMPTY_PAT s)
            Stop -> return Stop

    stepOuter _ GO_DONE = return Stop

{-# INLINE_NORMAL splitSuffixOn #-}
splitSuffixOn
    :: forall m a b. (MonadIO m, Storable a, Enum a, Eq a)
    => Bool
    -> Array a
    -> Fold m a b
    -> Stream m a
    -> Stream m b
splitSuffixOn withSep patArr@Array{..} (Fold fstep initial done)
                (Stream step state) =
    Stream stepOuter GO_START

    where

    patLen = A.length patArr
    maxIndex = patLen - 1
    elemBits = sizeOf (undefined :: a) * 8

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ GO_START =
        if patLen == 0
        then return $ Skip $ GO_EMPTY_PAT state
        else if patLen == 1
             then do
                r <- liftIO $ (A.unsafeIndexIO patArr 0)
                return $ Skip $ GO_SINGLE_PAT state r
             else if sizeOf (undefined :: a) * patLen
                    <= sizeOf (undefined :: Word)
                  then return $ Skip $ GO_SHORT_PAT state
                  else do
                    (rb, rhead) <- liftIO $ RB.new patLen
                    return $ Skip $ GO_KARP_RABIN state rb rhead

    stepOuter gst (GO_SINGLE_PAT stt pat) = do
        -- This first part is the only difference between splitOn and
        -- splitSuffixOn.
        -- If the last element is a separator do not issue a blank segment.
        res <- step (adaptState gst) stt
        case res of
            Yield x s -> do
                acc <- initial
                if pat == x
                then do
                    acc' <- if withSep then fstep acc x else return acc
                    done acc' >>= \r -> return $ Yield r (GO_SINGLE_PAT s pat)
                else fstep acc x >>= go SPEC s
            Skip s    -> return $ Skip $ (GO_SINGLE_PAT s pat)
            Stop      -> return Stop

        where

        -- This is identical for splitOn and splitSuffixOn
        go !_ st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    if pat == x
                    then do
                        acc' <- if withSep then fstep acc x else return acc
                        r <- done acc'
                        return $ Yield r (GO_SINGLE_PAT s pat)
                    else fstep acc x >>= go SPEC s
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \r -> return $ Yield r GO_DONE

    stepOuter gst (GO_SHORT_PAT stt) = do

        -- Call "initial" only if the stream yields an element, otherwise we
        -- may call "initial" but never yield anything. initial may produce a
        -- side effect, therefore we will end up doing and discard a side
        -- effect.

        let idx = 0
        let wrd = 0
        res <- step (adaptState gst) stt
        case res of
            Yield x s -> do
                acc <- initial
                let wrd' = addToWord wrd x
                acc' <- if withSep then fstep acc x else return acc
                if idx == maxIndex
                then do
                    if wrd' .&. mask == patWord
                    then done acc' >>= \r -> return $ Yield r (GO_SHORT_PAT s)
                    else go0 SPEC (idx + 1) wrd' s acc'
                else go0 SPEC (idx + 1) wrd' s acc'
            Skip s -> return $ Skip (GO_SHORT_PAT s)
            Stop -> return Stop

        where

        mask :: Word
        mask = (1 `shiftL` (elemBits * patLen)) - 1

        addToWord wrd a = (wrd `shiftL` elemBits) .|. fromIntegral (fromEnum a)

        patWord :: Word
        patWord = mask .&. A.foldl' addToWord 0 patArr

        go0 !_ !idx wrd st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd' = addToWord wrd x
                    acc' <- if withSep then fstep acc x else return acc
                    if idx == maxIndex
                    then do
                        if wrd' .&. mask == patWord
                        then do
                            r <- done acc'
                            return $ Yield r (GO_SHORT_PAT s)
                        else go1 SPEC wrd' s acc'
                    else go0 SPEC (idx + 1) wrd' s acc'
                Skip s -> go0 SPEC idx wrd s acc
                Stop -> do
                    if (idx == maxIndex) && (wrd .&. mask == patWord)
                    then return Stop
                    else do
                        acc' <- if idx /= 0 && not withSep
                                then go2 wrd idx acc
                                else return acc
                        done acc' >>= \r -> return $ Yield r GO_DONE

        {-# INLINE go1 #-}
        go1 !_ wrd st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd' = addToWord wrd x
                        old = (mask .&. wrd) `shiftR` (elemBits * (patLen - 1))
                    acc' <- if withSep
                            then fstep acc x
                            else fstep acc (toEnum $ fromIntegral old)
                    if wrd' .&. mask == patWord
                    then done acc' >>= \r -> return $ Yield r (GO_SHORT_PAT s)
                    else go1 SPEC wrd' s acc'
                Skip s -> go1 SPEC wrd s acc
                Stop ->
                    -- If the last sequence is a separator do not issue a blank
                    -- segment.
                    if wrd .&. mask == patWord
                    then return Stop
                    else do
                        acc' <- if withSep
                                then return acc
                                else go2 wrd patLen acc
                        done acc' >>= \r -> return $ Yield r GO_DONE

        go2 !wrd !n !acc | n > 0 = do
            let old = (mask .&. wrd) `shiftR` (elemBits * (n - 1))
            fstep acc (toEnum $ fromIntegral old) >>= go2 wrd (n - 1)
        go2 _ _ acc = return acc

    stepOuter gst (GO_KARP_RABIN stt rb rhead) = do
        let idx = 0
        res <- step (adaptState gst) stt
        case res of
            Yield x s -> do
                acc <- initial
                acc' <- if withSep then fstep acc x else return acc
                rh' <- liftIO (RB.unsafeInsert rb rhead x)
                if idx == maxIndex
                then do
                    let fold = RB.unsafeFoldRing (RB.ringBound rb)
                    let !ringHash = fold addCksum 0 rb
                    if ringHash == patHash
                    then go2 SPEC ringHash rh' s acc'
                    else go0 SPEC (idx + 1) rh' s acc'
                else go0 SPEC (idx + 1) rh' s acc'
            Skip s -> return $ Skip (GO_KARP_RABIN s rb rhead)
            Stop -> return Stop

        where

        k = 2891336453 :: Word32
        coeff = k ^ patLen
        addCksum cksum a = cksum * k + fromIntegral (fromEnum a)
        deltaCksum cksum old new =
            addCksum cksum new - coeff * fromIntegral (fromEnum old)

        -- XXX shall we use a random starting hash or 1 instead of 0?
        patHash = A.foldl' addCksum 0 patArr

        -- rh == ringHead
        go0 !_ !idx !rh st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    acc' <- if withSep then fstep acc x else return acc
                    rh' <- liftIO (RB.unsafeInsert rb rh x)
                    if idx == maxIndex
                    then do
                        let fold = RB.unsafeFoldRing (RB.ringBound rb)
                        let !ringHash = fold addCksum 0 rb
                        if ringHash == patHash
                        then go2 SPEC ringHash rh' s acc'
                        else go1 SPEC ringHash rh' s acc'
                    else go0 SPEC (idx + 1) rh' s acc'
                Skip s -> go0 SPEC idx rh s acc
                Stop -> do
                    -- do not issue a blank segment when we end at pattern
                    if (idx == maxIndex) && RB.unsafeEqArray rb rh patArr
                    then return Stop
                    else do
                        !acc' <- if idx /= 0 && not withSep
                                 then RB.unsafeFoldRingM rh fstep acc rb
                                 else return acc
                        done acc' >>= \r -> return $ Yield r GO_DONE

        -- XXX Theoretically this code can do 4 times faster if GHC generates
        -- optimal code. If we use just "(cksum' == patHash)" condition it goes
        -- 4x faster, as soon as we add the "RB.unsafeEqArray rb v" condition
        -- the generated code changes drastically and becomes 4x slower. Need
        -- to investigate what is going on with GHC.
        {-# INLINE go1 #-}
        go1 !_ !cksum !rh st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    old <- liftIO $ peek rh
                    let cksum' = deltaCksum cksum old x
                    acc' <- if withSep
                            then fstep acc x
                            else fstep acc old

                    if (cksum' == patHash)
                    then do
                        rh' <- liftIO (RB.unsafeInsert rb rh x)
                        go2 SPEC cksum' rh' s acc'
                    else do
                        rh' <- liftIO (RB.unsafeInsert rb rh x)
                        go1 SPEC cksum' rh' s acc'
                Skip s -> go1 SPEC cksum rh s acc
                Stop -> do
                    if RB.unsafeEqArray rb rh patArr
                    then return Stop
                    else do
                        acc' <- if withSep
                                then return acc
                                else RB.unsafeFoldRingFullM rh fstep acc rb
                        done acc' >>= \r -> return $ Yield r GO_DONE

        go2 !_ !cksum' !rh' s !acc' = do
            if RB.unsafeEqArray rb rh' patArr
            then do
                r <- done acc'
                return $ Yield r (GO_KARP_RABIN s rb rhead)
            else go1 SPEC cksum' rh' s acc'

    stepOuter gst (GO_EMPTY_PAT st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                acc <- initial
                acc' <- fstep acc x
                done acc' >>= \r -> return $ Yield r (GO_EMPTY_PAT s)
            Skip s -> return $ Skip (GO_EMPTY_PAT s)
            Stop -> return Stop

    stepOuter _ GO_DONE = return Stop

data SplitState s arr
    = SplitInitial s
    | SplitBuffering s arr
    | SplitSplitting s arr
    | SplitYielding arr (SplitState s arr)
    | SplitFinishing

-- XXX An alternative approach would be to use a partial fold (Fold m a b) to
-- split using a splitBy like combinator. The Fold would consume upto the
-- separator and return any leftover which can then be fed to the next fold.
--
-- We can revisit this once we have partial folds/parsers.
--
-- | Performs infix separator style splitting.
{-# INLINE_NORMAL splitInnerBy #-}
splitInnerBy
    :: Monad m
    => (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> Stream m (f a)
    -> Stream m (f a)
splitInnerBy splitter joiner (Stream step1 state1) =
    (Stream step (SplitInitial state1))

    where

    {-# INLINE_LATE step #-}
    step gst (SplitInitial st) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s x1)
                    Just x2 -> Skip (SplitYielding x1 (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitInitial s)
            Stop -> return $ Stop

    step gst (SplitBuffering st buf) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                buf' <- joiner buf x1
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s buf')
                    Just x2 -> Skip (SplitYielding buf' (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitBuffering s buf)
            Stop -> return $ Skip (SplitYielding buf SplitFinishing)

    step _ (SplitSplitting st buf) = do
        (x1, mx2) <- splitter buf
        return $ case mx2 of
                Nothing -> Skip $ SplitBuffering st x1
                Just x2 -> Skip $ SplitYielding x1 (SplitSplitting st x2)

    step _ (SplitYielding x next) = return $ Yield x next
    step _ SplitFinishing = return $ Stop

-- | Performs infix separator style splitting.
{-# INLINE_NORMAL splitInnerBySuffix #-}
splitInnerBySuffix
    :: (Monad m, Eq (f a), Monoid (f a))
    => (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> Stream m (f a)
    -> Stream m (f a)
splitInnerBySuffix splitter joiner (Stream step1 state1) =
    (Stream step (SplitInitial state1))

    where

    {-# INLINE_LATE step #-}
    step gst (SplitInitial st) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s x1)
                    Just x2 -> Skip (SplitYielding x1 (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitInitial s)
            Stop -> return $ Stop

    step gst (SplitBuffering st buf) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                buf' <- joiner buf x1
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s buf')
                    Just x2 -> Skip (SplitYielding buf' (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitBuffering s buf)
            Stop -> return $
                if buf == mempty
                then Stop
                else Skip (SplitYielding buf SplitFinishing)

    step _ (SplitSplitting st buf) = do
        (x1, mx2) <- splitter buf
        return $ case mx2 of
                Nothing -> Skip $ SplitBuffering st x1
                Just x2 -> Skip $ SplitYielding x1 (SplitSplitting st x2)

    step _ (SplitYielding x next) = return $ Yield x next
    step _ SplitFinishing = return $ Stop

------------------------------------------------------------------------------
-- Substreams
------------------------------------------------------------------------------

{-# INLINE_NORMAL isPrefixOf #-}
isPrefixOf :: (Eq a, Monad m) => Stream m a -> Stream m a -> m Bool
isPrefixOf (Stream stepa ta) (Stream stepb tb) = go (ta, tb, Nothing)
  where
    go (sa, sb, Nothing) = do
        r <- stepa defState sa
        case r of
            Yield x sa' -> go (sa', sb, Just x)
            Skip sa'    -> go (sa', sb, Nothing)
            Stop        -> return True

    go (sa, sb, Just x) = do
        r <- stepb defState sb
        case r of
            Yield y sb' ->
                if x == y
                    then go (sa, sb', Nothing)
                    else return False
            Skip sb' -> go (sa, sb', Just x)
            Stop     -> return False

{-# INLINE_NORMAL isSubsequenceOf #-}
isSubsequenceOf :: (Eq a, Monad m) => Stream m a -> Stream m a -> m Bool
isSubsequenceOf (Stream stepa ta) (Stream stepb tb) = go (ta, tb, Nothing)
  where
    go (sa, sb, Nothing) = do
        r <- stepa defState sa
        case r of
            Yield x sa' -> go (sa', sb, Just x)
            Skip sa'    -> go (sa', sb, Nothing)
            Stop        -> return True

    go (sa, sb, Just x) = do
        r <- stepb defState sb
        case r of
            Yield y sb' ->
                if x == y
                    then go (sa, sb', Nothing)
                    else go (sa, sb', Just x)
            Skip sb' -> go (sa, sb', Just x)
            Stop     -> return False

{-# INLINE_NORMAL stripPrefix #-}
stripPrefix
    :: (Eq a, Monad m)
    => Stream m a -> Stream m a -> m (Maybe (Stream m a))
stripPrefix (Stream stepa ta) (Stream stepb tb) = go (ta, tb, Nothing)
  where
    go (sa, sb, Nothing) = do
        r <- stepa defState sa
        case r of
            Yield x sa' -> go (sa', sb, Just x)
            Skip sa'    -> go (sa', sb, Nothing)
            Stop        -> return $ Just (Stream stepb sb)

    go (sa, sb, Just x) = do
        r <- stepb defState sb
        case r of
            Yield y sb' ->
                if x == y
                    then go (sa, sb', Nothing)
                    else return Nothing
            Skip sb' -> go (sa, sb', Just x)
            Stop     -> return Nothing

------------------------------------------------------------------------------
-- Map and Fold
------------------------------------------------------------------------------

-- | Execute a monadic action for each element of the 'Stream'
{-# INLINE_NORMAL mapM_ #-}
mapM_ :: Monad m => (a -> m b) -> Stream m a -> m ()
mapM_ m = drain . mapM m

-------------------------------------------------------------------------------
-- Stream transformations using Unfolds
-------------------------------------------------------------------------------

-- Define a unique structure to use in inspection testing
data ConcatMapUState o i =
      ConcatMapUOuter o
    | ConcatMapUInner o i

-- | @concatMapU unfold stream@ uses @unfold@ to map the input stream elements
-- to streams and then flattens the generated streams into a single output
-- stream.

-- This is like 'concatMap' but uses an unfold with an explicit state to
-- generate the stream instead of a 'Stream' type generator. This allows better
-- optimization via fusion.  This can be many times more efficient than
-- 'concatMap'.

{-# INLINE_NORMAL concatMapU #-}
concatMapU :: Monad m => Unfold m a b -> Stream m a -> Stream m b
concatMapU (Unfold istep inject) (Stream ostep ost) =
    Stream step (ConcatMapUOuter ost)
  where
    {-# INLINE_LATE step #-}
    step gst (ConcatMapUOuter o) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- inject a
                i `seq` return (Skip (ConcatMapUInner o' i))
            Skip o' -> return $ Skip (ConcatMapUOuter o')
            Stop -> return $ Stop

    step _ (ConcatMapUInner o i) = do
        r <- istep i
        return $ case r of
            Yield x i' -> Yield x (ConcatMapUInner o i')
            Skip i'    -> Skip (ConcatMapUInner o i')
            Stop       -> Skip (ConcatMapUOuter o)

data ConcatUnfoldInterleaveState o i =
      ConcatUnfoldInterleaveOuter o [i]
    | ConcatUnfoldInterleaveInner o [i]
    | ConcatUnfoldInterleaveInnerL [i] [i]
    | ConcatUnfoldInterleaveInnerR [i] [i]

-- XXX use arrays to store state instead of lists.
-- XXX In general we can use different scheduling strategies e.g. how to
-- schedule the outer vs inner loop or assigning weights to different streams
-- or outer and inner loops.

-- After a yield, switch to the next stream. Do not switch streams on Skip.
-- Yield from outer stream switches to the inner stream.
--
-- There are two choices here, (1) exhaust the outer stream first and then
-- start yielding from the inner streams, this is much simpler to implement,
-- (2) yield at least one element from an inner stream before going back to
-- outer stream and opening the next stream from it.
--
-- Ideally, we need some scheduling bias to inner streams vs outer stream.
-- Maybe we can configure the behavior.
--
{-# INLINE_NORMAL concatUnfoldInterleave #-}
concatUnfoldInterleave :: Monad m => Unfold m a b -> Stream m a -> Stream m b
concatUnfoldInterleave (Unfold istep inject) (Stream ostep ost) =
    Stream step (ConcatUnfoldInterleaveOuter ost [])
  where
    {-# INLINE_LATE step #-}
    step gst (ConcatUnfoldInterleaveOuter o ls) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- inject a
                i `seq` return (Skip (ConcatUnfoldInterleaveInner o' (i : ls)))
            Skip o' -> return $ Skip (ConcatUnfoldInterleaveOuter o' ls)
            Stop -> return $ Skip (ConcatUnfoldInterleaveInnerL ls [])

    step _ (ConcatUnfoldInterleaveInner _ []) = undefined
    step _ (ConcatUnfoldInterleaveInner o (st:ls)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveOuter o (s:ls))
            Skip s    -> Skip (ConcatUnfoldInterleaveInner o (s:ls))
            Stop      -> Skip (ConcatUnfoldInterleaveOuter o ls)

    step _ (ConcatUnfoldInterleaveInnerL [] []) = return Stop
    step _ (ConcatUnfoldInterleaveInnerL [] rs) =
        return $ Skip (ConcatUnfoldInterleaveInnerR [] rs)

    step _ (ConcatUnfoldInterleaveInnerL (st:ls) rs) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveInnerL ls (s:rs))
            Skip s    -> Skip (ConcatUnfoldInterleaveInnerL (s:ls) rs)
            Stop      -> Skip (ConcatUnfoldInterleaveInnerL ls rs)

    step _ (ConcatUnfoldInterleaveInnerR [] []) = return Stop
    step _ (ConcatUnfoldInterleaveInnerR ls []) =
        return $ Skip (ConcatUnfoldInterleaveInnerL ls [])

    step _ (ConcatUnfoldInterleaveInnerR ls (st:rs)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveInnerR (s:ls) rs)
            Skip s    -> Skip (ConcatUnfoldInterleaveInnerR ls (s:rs))
            Stop      -> Skip (ConcatUnfoldInterleaveInnerR ls rs)

-- XXX In general we can use different scheduling strategies e.g. how to
-- schedule the outer vs inner loop or assigning weights to different streams
-- or outer and inner loops.
--
-- This could be inefficient if the tasks are too small.
--
-- Compared to concatUnfoldInterleave this one switches streams on Skips.
--
{-# INLINE_NORMAL concatUnfoldRoundrobin #-}
concatUnfoldRoundrobin :: Monad m => Unfold m a b -> Stream m a -> Stream m b
concatUnfoldRoundrobin (Unfold istep inject) (Stream ostep ost) =
    Stream step (ConcatUnfoldInterleaveOuter ost [])
  where
    {-# INLINE_LATE step #-}
    step gst (ConcatUnfoldInterleaveOuter o ls) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- inject a
                i `seq` return (Skip (ConcatUnfoldInterleaveInner o' (i : ls)))
            Skip o' -> return $ Skip (ConcatUnfoldInterleaveInner o' ls)
            Stop -> return $ Skip (ConcatUnfoldInterleaveInnerL ls [])

    step _ (ConcatUnfoldInterleaveInner o []) =
            return $ Skip (ConcatUnfoldInterleaveOuter o [])

    step _ (ConcatUnfoldInterleaveInner o (st:ls)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveOuter o (s:ls))
            Skip s    -> Skip (ConcatUnfoldInterleaveOuter o (s:ls))
            Stop      -> Skip (ConcatUnfoldInterleaveOuter o ls)

    step _ (ConcatUnfoldInterleaveInnerL [] []) = return Stop
    step _ (ConcatUnfoldInterleaveInnerL [] rs) =
        return $ Skip (ConcatUnfoldInterleaveInnerR [] rs)

    step _ (ConcatUnfoldInterleaveInnerL (st:ls) rs) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveInnerL ls (s:rs))
            Skip s    -> Skip (ConcatUnfoldInterleaveInnerL ls (s:rs))
            Stop      -> Skip (ConcatUnfoldInterleaveInnerL ls rs)

    step _ (ConcatUnfoldInterleaveInnerR [] []) = return Stop
    step _ (ConcatUnfoldInterleaveInnerR ls []) =
        return $ Skip (ConcatUnfoldInterleaveInnerL ls [])

    step _ (ConcatUnfoldInterleaveInnerR ls (st:rs)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveInnerR (s:ls) rs)
            Skip s    -> Skip (ConcatUnfoldInterleaveInnerR (s:ls) rs)
            Stop      -> Skip (ConcatUnfoldInterleaveInnerR ls rs)

data AppendState s1 s2 = AppendFirst s1 | AppendSecond s2

-- Note that this could be much faster compared to the CPS stream. However, as
-- the number of streams being composed increases this may become expensive.
-- Need to see where the breaking point is between the two.
--
{-# INLINE_NORMAL append #-}
append :: Monad m => Stream m a -> Stream m a -> Stream m a
append (Stream step1 state1) (Stream step2 state2) =
    Stream step (AppendFirst state1)

    where

    {-# INLINE_LATE step #-}
    step gst (AppendFirst st) = do
        r <- step1 gst st
        return $ case r of
            Yield a s -> Yield a (AppendFirst s)
            Skip s -> Skip (AppendFirst s)
            Stop -> Skip (AppendSecond state2)

    step gst (AppendSecond st) = do
        r <- step2 gst st
        return $ case r of
            Yield a s -> Yield a (AppendSecond s)
            Skip s -> Skip (AppendSecond s)
            Stop -> Stop

data InterleaveState s1 s2 = InterleaveFirst s1 s2 | InterleaveSecond s1 s2
    | InterleaveSecondOnly s2 | InterleaveFirstOnly s1

{-# INLINE_NORMAL interleave #-}
interleave :: Monad m => Stream m a -> Stream m a -> Stream m a
interleave (Stream step1 state1) (Stream step2 state2) =
    Stream step (InterleaveFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (InterleaveFirst st1 st2) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveSecond s st2)
            Skip s -> Skip (InterleaveFirst s st2)
            Stop -> Skip (InterleaveSecondOnly st2)

    step gst (InterleaveSecond st1 st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveFirst st1 s)
            Skip s -> Skip (InterleaveSecond st1 s)
            Stop -> Skip (InterleaveFirstOnly st1)

    step gst (InterleaveFirstOnly st1) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveFirstOnly s)
            Skip s -> Skip (InterleaveFirstOnly s)
            Stop -> Stop

    step gst (InterleaveSecondOnly st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveSecondOnly s)
            Skip s -> Skip (InterleaveSecondOnly s)
            Stop -> Stop

{-# INLINE_NORMAL interleaveMin #-}
interleaveMin :: Monad m => Stream m a -> Stream m a -> Stream m a
interleaveMin (Stream step1 state1) (Stream step2 state2) =
    Stream step (InterleaveFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (InterleaveFirst st1 st2) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveSecond s st2)
            Skip s -> Skip (InterleaveFirst s st2)
            Stop -> Stop

    step gst (InterleaveSecond st1 st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveFirst st1 s)
            Skip s -> Skip (InterleaveSecond st1 s)
            Stop -> Stop

    step _ (InterleaveFirstOnly _) =  undefined
    step _ (InterleaveSecondOnly _) =  undefined

{-# INLINE_NORMAL interleaveFst #-}
interleaveFst :: Monad m => Stream m a -> Stream m a -> Stream m a
interleaveFst (Stream step1 state1) (Stream step2 state2) =
    Stream step (InterleaveFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (InterleaveFirst st1 st2) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveSecond s st2)
            Skip s -> Skip (InterleaveFirst s st2)
            Stop -> Stop

    step gst (InterleaveSecond st1 st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveFirst st1 s)
            Skip s -> Skip (InterleaveSecond st1 s)
            Stop -> Skip (InterleaveFirstOnly st1)

    step gst (InterleaveFirstOnly st1) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveFirstOnly s)
            Skip s -> Skip (InterleaveFirstOnly s)
            Stop -> Stop

    step _ (InterleaveSecondOnly _) =  undefined

{-# INLINE_NORMAL roundRobin #-}
roundRobin :: Monad m => Stream m a -> Stream m a -> Stream m a
roundRobin (Stream step1 state1) (Stream step2 state2) =
    Stream step (InterleaveFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (InterleaveFirst st1 st2) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveSecond s st2)
            Skip s -> Skip (InterleaveSecond s st2)
            Stop -> Skip (InterleaveSecondOnly st2)

    step gst (InterleaveSecond st1 st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveFirst st1 s)
            Skip s -> Skip (InterleaveFirst st1 s)
            Stop -> Skip (InterleaveFirstOnly st1)

    step gst (InterleaveSecondOnly st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveSecondOnly s)
            Skip s -> Skip (InterleaveSecondOnly s)
            Stop -> Stop

    step gst (InterleaveFirstOnly st1) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveFirstOnly s)
            Skip s -> Skip (InterleaveFirstOnly s)
            Stop -> Stop

data ICUState s1 s2 i1 i2 =
      ICUFirst s1 s2
    | ICUSecond s1 s2
    | ICUSecondOnly s2
    | ICUFirstOnly s1
    | ICUFirstInner s1 s2 i1
    | ICUSecondInner s1 s2 i2
    | ICUFirstOnlyInner s1 i1
    | ICUSecondOnlyInner s2 i2

-- | Interleave streams (full streams, not the elements) unfolded from two
-- input streams and concat. Stop when the first stream stops. If the second
-- stream ends before the first one then first stream still keeps running alone
-- without any interleaving with the second stream.
--
--    [a1, a2, ... an]                   [b1, b2 ...]
-- => [streamA1, streamA2, ... streamAn] [streamB1, streamB2, ...]
-- => [streamA1, streamB1, streamA2...StreamAn, streamBn]
-- => [a11, a12, ...a1j, b11, b12, ...b1k, a21, a22, ...]
--
{-# INLINE_NORMAL interleaveFstThenConcat #-}
interleaveFstThenConcat
    :: Monad m
    => Unfold m a c -> Stream m a -> Unfold m b c -> Stream m b -> Stream m c
interleaveFstThenConcat
    (Unfold istep1 inject1) (Stream step1 state1)
    (Unfold istep2 inject2) (Stream step2 state2) =
    Stream step (ICUFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (ICUFirst s1 s2) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (ICUFirstInner s s2 i))
            Skip s -> return $ Skip (ICUFirst s s2)
            Stop -> return Stop

    step gst (ICUFirstOnly s1) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (ICUFirstOnlyInner s i))
            Skip s -> return $ Skip (ICUFirstOnly s)
            Stop -> return Stop

    step _ (ICUFirstInner s1 s2 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (ICUFirstInner s1 s2 i')
            Skip i'    -> Skip (ICUFirstInner s1 s2 i')
            Stop       -> Skip (ICUSecond s1 s2)

    step _ (ICUFirstOnlyInner s1 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (ICUFirstOnlyInner s1 i')
            Skip i'    -> Skip (ICUFirstOnlyInner s1 i')
            Stop       -> Skip (ICUFirstOnly s1)

    step gst (ICUSecond s1 s2) = do
        r <- step2 (adaptState gst) s2
        case r of
            Yield a s -> do
                i <- inject2 a
                i `seq` return (Skip (ICUSecondInner s1 s i))
            Skip s -> return $ Skip (ICUSecond s1 s)
            Stop -> return $ Skip (ICUFirstOnly s1)

    step _ (ICUSecondInner s1 s2 i2) = do
        r <- istep2 i2
        return $ case r of
            Yield x i' -> Yield x (ICUSecondInner s1 s2 i')
            Skip i'    -> Skip (ICUSecondInner s1 s2 i')
            Stop       -> Skip (ICUFirst s1 s2)

    step _ (ICUSecondOnly _s2) = undefined
    step _ (ICUSecondOnlyInner _s2 _i2) = undefined

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

-- | Run a side effect before the stream yields its first element.
{-# INLINE_NORMAL before #-}
before :: Monad m => m b -> Stream m a -> Stream m a
before action (Stream step state) = Stream step' Nothing

    where

    {-# INLINE_LATE step' #-}
    step' _ Nothing = action >> return (Skip (Just state))

    step' gst (Just st) = do
        res <- step gst st
        case res of
            Yield x s -> return $ Yield x (Just s)
            Skip s    -> return $ Skip (Just s)
            Stop      -> return Stop

-- | Run a side effect whenever the stream stops normally.
{-# INLINE_NORMAL after #-}
after :: Monad m => m b -> Stream m a -> Stream m a
after action (Stream step state) = Stream step' state

    where

    {-# INLINE_LATE step' #-}
    step' gst st = do
        res <- step gst st
        case res of
            Yield x s -> return $ Yield x s
            Skip s    -> return $ Skip s
            Stop      -> action >> return Stop

-- XXX These combinators are expensive due to the call to
-- onException/handle/try on each step. Therefore, when possible, they should
-- be called in an outer loop where we perform less iterations. For example, we
-- cannot call them on each iteration in a char stream, instead we can call
-- them when doing an IO on an array.
--
-- XXX For high performance error checks in busy streams we may need another
-- Error constructor in step.
--
-- | Run a side effect whenever the stream aborts due to an exception. The
-- exception is not caught simply rethrown.
{-# INLINE_NORMAL onException #-}
onException :: MonadCatch m => m b -> Stream m a -> Stream m a
onException action (Stream step state) = Stream step' state

    where

    {-# INLINE_LATE step' #-}
    step' gst st = do
        res <- step gst st `MC.onException` action
        case res of
            Yield x s -> return $ Yield x s
            Skip s    -> return $ Skip s
            Stop      -> return Stop

-- | Run a side effect whenever the stream stops normally or aborts due to an
-- exception.
{-# INLINE finally #-}
finally :: MonadCatch m => m b -> Stream m a -> Stream m a
finally action xs = after action $ onException action xs

-- XXX bracket is like concatMap, it generates a stream and then flattens it.
-- Like concatMap it has 10x worse performance compared to linear fused
-- compositions.
--
-- | Run the first action before the stream starts and remember its output,
-- generate a stream using the output, run the second action using the
-- remembered value as an argument whenever the stream ends normally or due to
-- an exception.
{-# INLINE_NORMAL bracket #-}
bracket :: MonadCatch m => m b -> (b -> m c) -> (b -> Stream m a) -> Stream m a
bracket bef aft bet = Stream step' Nothing

    where

    {-# INLINE_LATE step' #-}
    step' _ Nothing = bef >>= \x -> return (Skip (Just (bet x, x)))

    -- NOTE: It is important to use UnStream instead of the Stream pattern
    -- here, otherwise we get huge perf degradation, see note in concatMap.
    step' gst (Just (UnStream step state, v)) = do
        res <- step gst state `MC.onException` aft v
        case res of
            Yield x s -> return $ Yield x (Just (Stream step s, v))
            Skip s    -> return $ Skip (Just (Stream step s, v))
            Stop      -> aft v >> return Stop

-- | When evaluating a stream if an exception occurs, stream evaluation aborts
-- and the specified exception handler is run with the exception as argument.
{-# INLINE_NORMAL handle #-}
handle :: (MonadCatch m, Exception e) => (e -> m a) -> Stream m a -> Stream m a
handle f (Stream step state) = Stream step' (Just state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (Just st) = do
        res <- MC.try $ step gst st
        case res of
            Left e -> f e >>= \x -> return (Yield x Nothing)
            Right r -> case r of
                Yield x s -> return $ Yield x (Just s)
                Skip s    -> return $ Skip (Just s)
                Stop      -> return Stop

    step' _ Nothing = return Stop

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

{-# INLINE scanlx' #-}
scanlx' :: Monad m
    => (x -> a -> x) -> x -> (x -> b) -> Stream m a -> Stream m b
scanlx' fstep begin done s =
    scanlMx' (\b a -> return (fstep b a)) (return begin) (return . done) s

------------------------------------------------------------------------------
-- postscans
------------------------------------------------------------------------------

{-# INLINE_NORMAL postscanlM' #-}
postscanlM' :: Monad m => (b -> a -> m b) -> b -> Stream m a -> Stream m b
postscanlM' fstep begin (Stream step state) =
    begin `seq` Stream step' (state, begin)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, acc) = acc `seq` do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                y <- fstep acc x
                y `seq` return (Yield y (s, y))
            Skip s -> return $ Skip (s, acc)
            Stop   -> return Stop

{-# INLINE_NORMAL postscanl' #-}
postscanl' :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
postscanl' f = postscanlM' (\a b -> return (f a b))

{-# INLINE_NORMAL postscanlM #-}
postscanlM :: Monad m => (b -> a -> m b) -> b -> Stream m a -> Stream m b
postscanlM fstep begin (Stream step state) = Stream step' (state, begin)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, acc) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                y <- fstep acc x
                return (Yield y (s, y))
            Skip s -> return $ Skip (s, acc)
            Stop   -> return Stop

{-# INLINE_NORMAL postscanl #-}
postscanl :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
postscanl f = postscanlM (\a b -> return (f a b))

{-# INLINE_NORMAL scanlM' #-}
scanlM' :: Monad m => (b -> a -> m b) -> b -> Stream m a -> Stream m b
scanlM' fstep begin s = begin `seq` (begin `cons` postscanlM' fstep begin s)

{-# INLINE scanl' #-}
scanl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> Stream m b
scanl' f = scanlM' (\a b -> return (f a b))

{-# INLINE_NORMAL scanlM #-}
scanlM :: Monad m => (b -> a -> m b) -> b -> Stream m a -> Stream m b
scanlM fstep begin s = begin `cons` postscanlM fstep begin s

{-# INLINE scanl #-}
scanl :: Monad m => (b -> a -> b) -> b -> Stream m a -> Stream m b
scanl f = scanlM (\a b -> return (f a b))

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

{-# INLINE tap #-}
tap :: Monad m => Fold m a b -> Stream m a -> Stream m a
tap (Fold fstep initial extract) (Stream step state) = Stream step' Nothing

    where

    step' _ Nothing = do
        r <- initial
        return $ Skip (Just (r, state))

    step' gst (Just (acc, st)) = do
        r <- step gst st
        case r of
            Yield x s -> do
                acc' <- fstep acc x
                return $ Yield x (Just (acc', s))
            Skip s    -> return $ Skip (Just (acc, s))
            Stop      -> do
                void $ extract acc
                return $ Stop

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE_NORMAL takeWhileM #-}
takeWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
takeWhileM f (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- step gst st
        case r of
            Yield x s -> do
                b <- f x
                return $ if b then Yield x s else Stop
            Skip s -> return $ Skip s
            Stop   -> return Stop

{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
takeWhile f = takeWhileM (return . f)

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

data SuffixState s a
    = SuffixElem s
    | SuffixSuffix s
    | SuffixYield a (SuffixState s a)

{-# INLINE_NORMAL insertAfterEach #-}
insertAfterEach :: forall m a. Monad m => m a -> Stream m a -> Stream m a
insertAfterEach action (Stream step state) = Stream step' (SuffixElem state)
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

--------------------------------------------------------------------------------
-- UTF8 Encoding / Decoding
--------------------------------------------------------------------------------

-- UTF-8 primitives, Lifted from GHC.IO.Encoding.UTF8.

{-# INLINE ord2 #-}
ord2 :: Char -> WList
ord2 c = assert (n >= 0x80 && n <= 0x07ff) (WCons x1 (WCons x2 WNil))
  where
    n = ord c
    x1 = fromIntegral $ (n `shiftR` 6) + 0xC0
    x2 = fromIntegral $ (n .&. 0x3F) + 0x80

{-# INLINE ord3 #-}
ord3 :: Char -> WList
ord3 c = assert (n >= 0x0800 && n <= 0xffff) (WCons x1 (WCons x2 (WCons x3 WNil)))
  where
    n = ord c
    x1 = fromIntegral $ (n `shiftR` 12) + 0xE0
    x2 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
    x3 = fromIntegral $ (n .&. 0x3F) + 0x80

{-# INLINE ord4 #-}
ord4 :: Char -> WList
ord4 c = assert (n >= 0x10000)  (WCons x1 (WCons x2 (WCons x3 (WCons x4 WNil))))
  where
    n = ord c
    x1 = fromIntegral $ (n `shiftR` 18) + 0xF0
    x2 = fromIntegral $ ((n `shiftR` 12) .&. 0x3F) + 0x80
    x3 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
    x4 = fromIntegral $ (n .&. 0x3F) + 0x80

data CodingFailureMode
    = TransliterateCodingFailure
    | ErrorOnCodingFailure
    deriving (Show)

{-# INLINE replacementChar #-}
replacementChar :: Char
replacementChar = '\xFFFD'

type CodePoint = Word32
type DecoderState = Word32

-- See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.

utf8d :: A.Array Word32
utf8d = A.fromList [
   -- The first part of the table maps bytes to character classes that
   -- to reduce the size of the transition table and create bitmasks.
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

   -- The second part is a transition table that maps a combination
   -- of a state of the automaton and a character class to a state.
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12
  ]

{-# INLINE decode #-}
decode :: DecoderState -> CodePoint -> Word32 -> Tuple' DecoderState CodePoint
decode state codep byte =
    let t = utf8d `A.unsafeIndex` (fromIntegral byte)
        codep' =
            if state /= 0
                then (byte .&. 0x3f) .|. (codep `shiftL` 6)
                else (0xff `shiftR` (fromIntegral t)) .&. byte
        state' = utf8d `A.unsafeIndex` (fromIntegral $ 256 + state + t)
     in (Tuple' state' codep')

data FreshPoint s
    = FreshPoint !CodePoint !DecoderState s
    | YieldAndContinue !Char (FreshPoint s)
    | Done

-- XXX Add proper error messages
{-# INLINE_NORMAL decodeUtf8With #-}
decodeUtf8With :: Monad m => CodingFailureMode -> Stream m Word8 -> Stream m Char
decodeUtf8With cfm (Stream step state) = Stream step' (FreshPoint 0 0 state)
  where
    {-# INLINE transliterateOrError #-}
    transliterateOrError e s =
        case cfm of
            ErrorOnCodingFailure -> error e
            TransliterateCodingFailure -> YieldAndContinue replacementChar s
    {-# INLINE inputUnderflow #-}
    inputUnderflow =
        case cfm of
            ErrorOnCodingFailure ->
                error "Streamly.Streams.StreamD.decodeUtf8With: Input Underflow"
            TransliterateCodingFailure -> YieldAndContinue replacementChar Done
    {-# INLINE_LATE step' #-}
    step' gst (FreshPoint codepointPtr statePtr st) = do
        r <- step (adaptState gst) st
        return $
            case r of
                Yield x s ->
                    let (Tuple' sv cp) =
                            decode statePtr codepointPtr (fromIntegral x)
                     in case sv of
                            12 ->
                                Skip $
                                transliterateOrError
                                    "Streamly.Streams.StreamD.decodeUtf8With: Invalid UTF8 codepoint encountered"
                                    (FreshPoint 0 0 st)
                            0 ->
                                Skip $
                                YieldAndContinue
                                    (unsafeChr (fromIntegral cp))
                                    (FreshPoint cp sv s)
                            _ -> Skip (FreshPoint cp sv s)
                Skip s -> Skip (FreshPoint codepointPtr statePtr s)
                Stop ->
                    if statePtr /= 0
                        then Skip inputUnderflow
                        else Skip Done
    step' _ (YieldAndContinue c s) = return $ Yield c s
    step' _ Done = return Stop

{-# INLINE decodeUtf8 #-}
decodeUtf8 :: Monad m => Stream m Word8 -> Stream m Char
decodeUtf8 = decodeUtf8With ErrorOnCodingFailure

{-# INLINE decodeUtf8Lenient #-}
decodeUtf8Lenient :: Monad m => Stream m Word8 -> Stream m Char
decodeUtf8Lenient = decodeUtf8With TransliterateCodingFailure

data FlattenState s a
    = OuterLoop !CodePoint !DecoderState s
    | InnerLoop !CodePoint !DecoderState s (ForeignPtr a) (Ptr a) (Ptr a)
    | YAndC !Char (FlattenState s a) -- These constructors can be
                                     -- encoded in the FreshPoint
                                     -- type, I prefer to keep these
                                     -- flat even though that means
                                     -- coming up with new names
    | D

-- The normal decodeUtf8 above should fuse with flattenArrays
-- to create this exact code but it doesn't for some reason, as of now this
-- remains the fastest way I could figure out to decodeUtf8.
--
-- XXX Add Proper error messages
{-# INLINE_NORMAL decodeUtf8ArraysWith #-}
decodeUtf8ArraysWith ::
       MonadIO m
    => CodingFailureMode
    -> Stream m (A.Array Word8)
    -> Stream m Char
decodeUtf8ArraysWith cfm (Stream step state) =
    Stream step' (OuterLoop 0 0 state)
  where
    {-# INLINE transliterateOrError #-}
    transliterateOrError e s =
        case cfm of
            ErrorOnCodingFailure -> error e
            TransliterateCodingFailure -> YAndC replacementChar s
    {-# INLINE inputUnderflow #-}
    inputUnderflow =
        case cfm of
            ErrorOnCodingFailure ->
                error
                    "Streamly.Streams.StreamD.decodeUtf8ArraysWith: Input Underflow"
            TransliterateCodingFailure -> YAndC replacementChar D
    {-# INLINE_LATE step' #-}
    step' gst (OuterLoop cp ds st) = do
        r <- step (adaptState gst) st
        return $
            case r of
                Yield A.Array {..} s ->
                    let p = unsafeForeignPtrToPtr aStart
                     in Skip (InnerLoop cp ds s aStart p aEnd)
                Skip s -> Skip (OuterLoop cp ds s)
                Stop ->
                    if ds /= 0
                        then Skip inputUnderflow
                        else Skip D
    step' _ (InnerLoop cp ds st _ p end)
        | p == end = return $ Skip $ OuterLoop cp ds st
    step' _ (InnerLoop codepointPtr statePtr st startf p end) = do
        x <-
            liftIO $ do
                r <- peek p
                touchForeignPtr startf
                return r
        let (Tuple' sv cp) = decode statePtr codepointPtr (fromIntegral x)
        return $
            case sv of
                12 ->
                    Skip $
                    transliterateOrError
                        "Streamly.Streams.StreamD.decodeUtf8ArraysWith: Invalid UTF8 codepoint encountered"
                        (InnerLoop 0 0 st startf (p `plusPtr` 1) end)
                0 ->
                    Skip $
                    YAndC
                        (unsafeChr (fromIntegral cp))
                        (InnerLoop cp sv st startf (p `plusPtr` 1) end)
                _ -> Skip (InnerLoop cp sv st startf (p `plusPtr` 1) end)
    step' _ (YAndC c s) = return $ Yield c s
    step' _ D = return Stop

{-# INLINE decodeUtf8Arrays #-}
decodeUtf8Arrays ::
       MonadIO m
    => Stream m (A.Array Word8)
    -> Stream m Char
decodeUtf8Arrays = decodeUtf8ArraysWith ErrorOnCodingFailure

{-# INLINE decodeUtf8ArraysLenient #-}
decodeUtf8ArraysLenient ::
       MonadIO m
    => Stream m (A.Array Word8)
    -> Stream m Char
decodeUtf8ArraysLenient = decodeUtf8ArraysWith TransliterateCodingFailure

data WList = WCons !Word8 !WList | WNil

data EncodeState s = EncodeState s !WList

{-# INLINE_NORMAL encodeUtf8 #-}
encodeUtf8 :: Monad m => Stream m Char -> Stream m Word8
encodeUtf8 (Stream step state) = Stream step' (EncodeState state WNil)
  where
    {-# INLINE_LATE step' #-}
    step' gst (EncodeState st WNil) = do
        r <- step (adaptState gst) st
        return $
            case r of
                Yield c s ->
                    case ord c of
                        x
                            | x <= 0x7F ->
                                Skip
                                    (EncodeState s (WCons (fromIntegral x) WNil))
                            | x <= 0x7FF -> Skip (EncodeState s (ord2 c))
                            | x <= 0xFFFF ->
                                if isSurrogate c
                                    then error
                                             "Streamly.Streams.StreamD.encodeUtf8: Encountered a surrogate"
                                    else Skip (EncodeState s (ord3 c))
                            | otherwise -> Skip (EncodeState s (ord4 c))
                Skip s -> Skip (EncodeState s WNil)
                Stop -> Stop
    step' _ (EncodeState s (WCons x xs)) = return $ Yield x (EncodeState s xs)
