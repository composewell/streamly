{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Fold.Combinators
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- See "Streamly.Data.Fold" for an overview and
-- "Streamly.Internal.Data.Fold.Type" for design notes.

module Streamly.Internal.Data.Fold.Combinators
    (
    -- * Mappers
    -- | Monadic functions useful with mapM/lmapM on folds or streams.
      tracing
    , trace

    -- * Folds

    -- ** Accumulators
    -- *** Semigroups and Monoids
    , sconcat
    , mconcat
    , foldMap
    , foldMapM

    -- *** Reducers
    , drainMapM
    , the
    , mean
    , rollingHash
    , Scanl.defaultSalt
    , rollingHashWithSalt
    , rollingHashFirstN
    -- , rollingHashLastN

    -- *** Saturating Reducers
    -- | 'product' terminates if it becomes 0. Other folds can theoretically
    -- saturate on bounded types, and therefore terminate, however, they will
    -- run forever on unbounded types like Integer/Double.
    , sum
    , product
    , maximumBy
    , maximum
    , minimumBy
    , minimum
    , rangeBy
    , range

    -- *** Collectors
    -- | Avoid using these folds in scalable or performance critical
    -- applications, they buffer all the input in GC memory which can be
    -- detrimental to performance if the input is large.
    , toStream
    , toStreamRev
    , topBy
    , top
    , bottomBy
    , bottom

    -- *** Scanners
    -- | Stateful transformation of the elements. Useful in combination with
    -- the 'scanMaybe' combinator. For scanners the result of the fold is
    -- usually a transformation of the current element rather than an
    -- aggregation of all elements till now.
 -- , nthLast -- using RingArray array
    , rollingMap
    , rollingMapM

    -- *** Filters

    -- XXX deprecate these in favor of corresponding scans

    -- | Useful in combination with the 'scanMaybe' combinator.
    , deleteBy
    , uniqBy
    , uniq
    , repeated
    , findIndices
    , elemIndices

    -- *** Singleton folds
    -- | Folds that terminate after consuming exactly one input element. All
    -- these can be implemented in terms of the 'maybe' fold.
    , one
    , null -- XXX not very useful and could be problematic, remove it?
    , satisfy
    , maybe

    -- *** Multi folds
    -- | Terminate after consuming one or more elements.
    , drainN
    -- , lastN
    -- , (!!)
    , genericIndex
    , index
    , findM
    , find
    , lookup
    , findIndex
    , elemIndex
    , elem
    , notElem
    , all
    , any
    , and
    , or

    -- ** Trimmers
    -- | Useful in combination with the 'scanMaybe' combinator.
    , takingEndByM
    , takingEndBy
    , takingEndByM_
    , takingEndBy_
    , droppingWhileM
    , droppingWhile
    , prune

    -- * Running A Fold
    , drive
    -- , breakStream

    -- * Building Incrementally
    , addStream

    -- * Combinators
    -- ** Utilities
    , with

    -- ** Sliding Window
    , slide2

    -- ** Scanning Input
    , pipe
    , indexed

    -- ** Zipping Input
    , zipStreamWithM
    , zipStream

    -- ** Filtering Input
    , mapMaybeM
    , mapMaybe
    , sampleFromthen

    {-
    -- ** Insertion
    -- | Insertion adds more elements to the stream.

    , insertBy
    , intersperseM

    -- ** Reordering
    , reverse
    -}

    -- ** Trimming

    -- By elements
    , takeEndBySeq
    , takeEndBySeq_
    {-
    , drop
    , dropWhile
    , dropWhileM
    -}

    -- ** Serial Append
    -- , tail
    -- , init
    , splitAt -- spanN
    -- , splitIn -- sessionN

    -- ** Parallel Distribution
    , tee
    , distribute
    , distributeScan
    -- , distributeFst
    -- , distributeMin

    -- ** Unzipping
    , unzip
    -- These two can be expressed using lmap/lmapM and unzip
    , unzipWith
    , unzipWithM
    , unzipWithFstM
    , unzipWithMinM

    -- ** Partitioning
    , partitionByM
    , partitionByFstM
    , partitionByMinM
    , partitionBy
    , partition

    -- ** Splitting
    , chunksBetween
    , intersperseWithQuotes

    -- ** Nesting
    , unfoldMany
    , concatSequence

    -- * Deprecated
    , drainBy
    , head
    , sequence
    , mapM
    , variance
    , stdDev
    , indexingWith
    , indexing
    , indexingRev
    )
where

#include "inline.hs"
#include "ArrayMacros.h"

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.Either (isLeft, isRight, fromLeft, fromRight)
import Data.Int (Int64)
import Data.Proxy (Proxy(..))
import Data.Word (Word32)
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Scanl.Type (Scanl(..))
import Streamly.Internal.Data.Unbox (Unbox(..))
import Streamly.Internal.Data.MutArray.Type (MutArray(..))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.Pipe.Type (Pipe (..))
import Streamly.Internal.Data.RingArray (RingArray(..))
-- import Streamly.Internal.Data.Scan (Scan (..))
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import qualified Prelude
import qualified Streamly.Internal.Data.MutArray.Type as MA
import qualified Streamly.Internal.Data.Array.Type as Array
import qualified Streamly.Internal.Data.Pipe.Type as Pipe
import qualified Streamly.Internal.Data.RingArray as RingArray
import qualified Streamly.Internal.Data.Scanl.Combinators as Scanl
import qualified Streamly.Internal.Data.Scanl.Type as Scanl
import qualified Streamly.Internal.Data.Stream.Type as StreamD

import Prelude hiding
       ( Foldable(..), filter, drop, dropWhile, take, takeWhile, zipWith
       , map, mapM_, sequence, all, any
       , notElem, head, last, tail
       , reverse, iterate, init, and, or, lookup, (!!)
       , scanl, scanl1, replicate, concatMap, mconcat, unzip
       , span, splitAt, break, mapM, zip, maybe)
import Streamly.Internal.Data.Fold.Type

#include "DocTestDataFold.hs"

------------------------------------------------------------------------------
-- Running
------------------------------------------------------------------------------

-- | Drive a fold using the supplied 'Stream', reducing the resulting
-- expression strictly at each step.
--
-- Definition:
--
-- >>> drive = flip Stream.fold
--
-- Example:
--
-- >>> Fold.drive (Stream.enumerateFromTo 1 100) Fold.sum
-- 5050
--
{-# INLINE drive #-}
drive :: Monad m => Stream m a -> Fold m a b -> m b
drive = flip StreamD.fold

{-
-- | Like 'drive' but also returns the remaining stream. The resulting stream
-- would be 'Stream.nil' if the stream finished before the fold.
--
-- Definition:
--
-- >>> breakStream = flip Stream.foldBreak
--
-- /CPS/
--
{-# INLINE breakStreamK #-}
breakStreamK :: Monad m => StreamK m a -> Fold m a b -> m (b, StreamK m a)
breakStreamK strm fl = fmap f $ K.foldBreak fl (Stream.toStreamK strm)

    where

    f (b, str) = (b, Stream.fromStreamK str)
-}

-- | Append a stream to a fold to build the fold accumulator incrementally. We
-- can repeatedly call 'addStream' on the same fold to continue building the
-- fold and finally use 'drive' to finish the fold and extract the result. Also
-- see the 'Streamly.Data.Fold.addOne' operation which is a singleton version
-- of 'addStream'.
--
-- Definitions:
--
-- >>> addStream stream = Fold.drive stream . Fold.duplicate
--
-- Example, build a list incrementally:
--
-- >>> :{
-- pure (Fold.toList :: Fold IO Int [Int])
--     >>= Fold.addOne 1
--     >>= Fold.addStream (Stream.enumerateFromTo 2 4)
--     >>= Fold.drive Stream.nil
--     >>= print
-- :}
-- [1,2,3,4]
--
-- This can be used as an O(n) list append compared to the O(n^2) @++@ when
-- used for incrementally building a list.
--
-- Example, build a stream incrementally:
--
-- >>> :{
-- pure (Fold.toStream :: Fold IO Int (Stream Identity Int))
--     >>= Fold.addOne 1
--     >>= Fold.addStream (Stream.enumerateFromTo 2 4)
--     >>= Fold.drive Stream.nil
--     >>= print
-- :}
-- fromList [1,2,3,4]
--
-- This can be used as an O(n) stream append compared to the O(n^2) @<>@ when
-- used for incrementally building a stream.
--
-- Example, build an array incrementally:
--
-- >>> :{
-- pure (Array.create :: Fold IO Int (Array Int))
--     >>= Fold.addOne 1
--     >>= Fold.addStream (Stream.enumerateFromTo 2 4)
--     >>= Fold.drive Stream.nil
--     >>= print
-- :}
-- fromList [1,2,3,4]
--
-- Example, build an array stream incrementally:
--
-- >>> :{
-- let f :: Fold IO Int (Stream Identity (Array Int))
--     f = Fold.groupsOf 2 (Array.createOf 3) Fold.toStream
-- in pure f
--     >>= Fold.addOne 1
--     >>= Fold.addStream (Stream.enumerateFromTo 2 4)
--     >>= Fold.drive Stream.nil
--     >>= print
-- :}
-- fromList [fromList [1,2],fromList [3,4]]
--
addStream :: Monad m => Stream m a -> Fold m a b -> m (Fold m a b)
addStream stream = drive stream . duplicate

------------------------------------------------------------------------------
-- Transformations on fold inputs
------------------------------------------------------------------------------

-- | Flatten the monadic output of a fold to pure output.
--
{-# DEPRECATED sequence "Use \"rmapM id\" instead" #-}
{-# INLINE sequence #-}
sequence :: Monad m => Fold m a (m b) -> Fold m a b
sequence = rmapM id

-- | Map a monadic function on the output of a fold.
--
{-# DEPRECATED mapM "Use rmapM instead" #-}
{-# INLINE mapM #-}
mapM :: Monad m => (b -> m c) -> Fold m a b -> Fold m a c
mapM = rmapM

-- |
-- >>> mapMaybeM f = Fold.lmapM f . Fold.catMaybes
--
{-# INLINE mapMaybeM #-}
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Fold m b r -> Fold m a r
mapMaybeM f = lmapM f . catMaybes

-- | @mapMaybe f fold@ maps a 'Maybe' returning function @f@ on the input of
-- the fold, filters out 'Nothing' elements, and return the values extracted
-- from 'Just'.
--
-- >>> mapMaybe f = Fold.lmap f . Fold.catMaybes
-- >>> mapMaybe f = Fold.mapMaybeM (return . f)
--
-- >>> f x = if even x then Just x else Nothing
-- >>> fld = Fold.mapMaybe f Fold.toList
-- >>> Stream.fold fld (Stream.enumerateFromTo 1 10)
-- [2,4,6,8,10]
--
{-# INLINE mapMaybe #-}
mapMaybe :: Monad m => (a -> Maybe b) -> Fold m b r -> Fold m a r
mapMaybe f = lmap f . catMaybes

------------------------------------------------------------------------------
-- Transformations on fold inputs
------------------------------------------------------------------------------

-- | Apply a monadic function on the input and return the input.
--
-- >>> Stream.fold (Fold.lmapM (Fold.tracing print) Fold.drain) $ (Stream.enumerateFromTo (1 :: Int) 2)
-- 1
-- 2
--
-- /Pre-release/
--
{-# INLINE tracing #-}
tracing :: Monad m => (a -> m b) -> (a -> m a)
tracing f x = void (f x) >> return x

-- | Apply a monadic function to each element flowing through and discard the
-- results.
--
-- >>> Stream.fold (Fold.trace print Fold.drain) $ (Stream.enumerateFromTo (1 :: Int) 2)
-- 1
-- 2
--
-- >>> trace f = Fold.lmapM (Fold.tracing f)
--
-- /Pre-release/
{-# INLINE trace #-}
trace :: Monad m => (a -> m b) -> Fold m a r -> Fold m a r
trace f = lmapM (tracing f)

-- | Attach a 'Pipe' on the input of a 'Fold'.
--
-- /Pre-release/
{-# INLINE pipe #-}
pipe :: Monad m => Pipe m a b -> Fold m b c -> Fold m a c
pipe (Pipe consume produce pinitial) (Fold fstep finitial fextract ffinal) =
    Fold step initial extract final

    where

    initial = first (Tuple' pinitial) <$> finitial

    step (Tuple' cs fs) x = do
        r <- consume cs x
        go fs r

        where

        -- XXX use SPEC?
        go acc (Pipe.YieldC cs1 b) = do
            acc1 <- fstep acc b
            return
                $ case acc1 of
                      Partial s -> Partial $ Tuple' cs1 s
                      Done b1 -> Done b1
        -- XXX this case is recursive may cause fusion issues.
        -- To remove recursion we will need a produce mode in folds which makes
        -- it similar to pipes except that it does not yield intermediate
        -- values..
        go acc (Pipe.YieldP ps1 b) = do
            acc1 <- fstep acc b
            r <- produce ps1
            case acc1 of
                Partial s -> go s r
                Done b1 -> return $ Done b1
        go acc (Pipe.SkipC cs1) =
            return $ Partial $ Tuple' cs1 acc
        -- XXX this case is recursive may cause fusion issues.
        go acc (Pipe.SkipP ps1) = do
            r <- produce ps1
            go acc r
        -- XXX a Stop in consumer means we dropped the input.
        go acc Pipe.Stop = Done <$> ffinal acc

    extract (Tuple' _ fs) = fextract fs

    final (Tuple' _ fs) = ffinal fs

------------------------------------------------------------------------------
-- Filters
------------------------------------------------------------------------------

-- | Returns the latest element omitting the first occurrence that satisfies
-- the given equality predicate.
--
-- Example:
--
-- >>> input = Stream.fromList [1,3,3,5]
--
-- >> Stream.toList $ Stream.scanMaybe (Fold.deleteBy (==) 3) input
-- [1,3,5]
--
{-# INLINE_NORMAL deleteBy #-}
deleteBy :: Monad m => (a -> a -> Bool) -> a -> Fold m a (Maybe a)
deleteBy eq = fromScanl . Scanl.deleteBy eq

-- | Provide a sliding window of length 2 elements.
--
-- See "Streamly.Internal.Data.Fold.Window".
--
{-# INLINE slide2 #-}
slide2 :: Monad m => Fold m (a, Maybe a) b -> Fold m a b
slide2 (Fold step1 initial1 extract1 final1) = Fold step initial extract final

    where

    initial =
        first (Tuple' Nothing) <$> initial1

    step (Tuple' prev s) cur =
        first (Tuple' (Just cur)) <$> step1 s (cur, prev)

    extract (Tuple' _ s) = extract1 s

    final (Tuple' _ s) = final1 s

-- | Return the latest unique element using the supplied comparison function.
-- Returns 'Nothing' if the current element is same as the last element
-- otherwise returns 'Just'.
--
-- Example, strip duplicate path separators:
--
-- >>> input = Stream.fromList "//a//b"
-- >>> f x y = x == '/' && y == '/'
--
-- >> Stream.toList $ Stream.scanMaybe (Fold.uniqBy f) input
-- "/a/b"
--
-- Space: @O(1)@
--
-- /Pre-release/
--
{-# INLINE uniqBy #-}
uniqBy :: Monad m => (a -> a -> Bool) -> Fold m a (Maybe a)
uniqBy = fromScanl . Scanl.uniqBy

-- | See 'uniqBy'.
--
-- Definition:
--
-- >>> uniq = Fold.uniqBy (==)
--
{-# INLINE uniq #-}
uniq :: (Monad m, Eq a) => Fold m a (Maybe a)
uniq = fromScanl Scanl.uniq

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
    (a -> Bool) -> Fold m a (Maybe a)
prune = error "Not implemented yet!"

-- | Emit only repeated elements, once.
--
-- /Unimplemented/
repeated :: -- (Monad m, Eq a) =>
    Fold m a (Maybe a)
repeated = error "Not implemented yet!"

------------------------------------------------------------------------------
-- Left folds
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Run Effects
------------------------------------------------------------------------------

-- |
-- Definitions:
--
-- >>> drainMapM f = Fold.lmapM f Fold.drain
-- >>> drainMapM f = Fold.foldMapM (void . f)
--
-- Drain all input after passing it through a monadic function. This is the
-- dual of mapM_ on stream producers.
--
{-# INLINE drainMapM #-}
drainMapM ::  Monad m => (a -> m b) -> Fold m a ()
drainMapM f = lmapM f drain

{-# DEPRECATED drainBy "Please use 'drainMapM' instead." #-}
{-# INLINE drainBy #-}
drainBy ::  Monad m => (a -> m b) -> Fold m a ()
drainBy = drainMapM

-- | Terminates with 'Nothing' as soon as it finds an element different than
-- the previous one, returns 'the' element if the entire input consists of the
-- same element.
--
{-# INLINE the #-}
the :: (Monad m, Eq a) => Fold m a (Maybe a)
the = fromScanl Scanl.the

------------------------------------------------------------------------------
-- To Summary
------------------------------------------------------------------------------

-- | Determine the sum of all elements of a stream of numbers. Returns additive
-- identity (@0@) when the stream is empty. Note that this is not numerically
-- stable for floating point numbers.
--
-- >>> sum = Fold.fromScanl (Scanl.cumulativeScan Scanl.incrSum)
--
-- Same as following but numerically stable:
--
-- >>> sum = Fold.foldl' (+) 0
-- >>> sum = fmap Data.Monoid.getSum $ Fold.foldMap Data.Monoid.Sum
--
{-# INLINE sum #-}
sum :: (Monad m, Num a) => Fold m a a
sum = fromScanl Scanl.sum

-- | Determine the product of all elements of a stream of numbers. Returns
-- multiplicative identity (@1@) when the stream is empty. The fold terminates
-- when it encounters (@0@) in its input.
--
-- Same as the following but terminates on multiplication by @0@:
--
-- >>> product = fmap Data.Monoid.getProduct $ Fold.foldMap Data.Monoid.Product
--
{-# INLINE product #-}
product :: (Monad m, Num a, Eq a) => Fold m a a
product = fromScanl Scanl.product

------------------------------------------------------------------------------
-- To Summary (Maybe)
------------------------------------------------------------------------------

-- | Determine the maximum element in a stream using the supplied comparison
-- function.
--
{-# INLINE maximumBy #-}
maximumBy :: Monad m => (a -> a -> Ordering) -> Fold m a (Maybe a)
maximumBy cmp = foldl1' max'

    where

    max' x y =
        case cmp x y of
            GT -> x
            _ -> y

-- | Determine the maximum element in a stream.
--
-- Definitions:
--
-- >>> maximum = Fold.maximumBy compare
-- >>> maximum = Fold.foldl1' max
--
-- Same as the following but without a default maximum. The 'Max' Monoid uses
-- the 'minBound' as the default maximum:
--
-- >>> maximum = fmap Data.Semigroup.getMax $ Fold.foldMap Data.Semigroup.Max
--
{-# INLINE maximum #-}
maximum :: (Monad m, Ord a) => Fold m a (Maybe a)
maximum = foldl1' max

-- | Computes the minimum element with respect to the given comparison function
--
{-# INLINE minimumBy #-}
minimumBy :: Monad m => (a -> a -> Ordering) -> Fold m a (Maybe a)
minimumBy cmp = foldl1' min'

    where

    min' x y =
        case cmp x y of
            GT -> y
            _ -> x

-- | Determine the minimum element in a stream using the supplied comparison
-- function.
--
-- Definitions:
--
-- >>> minimum = Fold.minimumBy compare
-- >>> minimum = Fold.foldl1' min
--
-- Same as the following but without a default minimum. The 'Min' Monoid uses the
-- 'maxBound' as the default maximum:
--
-- >>> maximum = fmap Data.Semigroup.getMin $ Fold.foldMap Data.Semigroup.Min
--
{-# INLINE minimum #-}
minimum :: (Monad m, Ord a) => Fold m a (Maybe a)
minimum = foldl1' min

{-# INLINE rangeBy #-}
rangeBy :: Monad m => (a -> a -> Ordering) -> Fold m a (Maybe (a, a))
rangeBy cmp = fromScanl (Scanl.rangeBy cmp)

-- | Find minimum and maximum elements i.e. (min, max).
--
{-# INLINE range #-}
range :: (Monad m, Ord a) => Fold m a (Maybe (a, a))
range = fromScanl Scanl.range

------------------------------------------------------------------------------
-- To Summary (Statistical)
------------------------------------------------------------------------------

-- | Compute a numerically stable arithmetic mean of all elements in the input
-- stream.
--
{-# INLINE mean #-}
mean :: (Monad m, Fractional a) => Fold m a a
mean = fromScanl Scanl.mean

-- | Compute a numerically stable (population) variance over all elements in
-- the input stream.
--
{-# DEPRECATED variance "Use the streamly-statistics package instead" #-}
{-# INLINE variance #-}
variance :: (Monad m, Fractional a) => Fold m a a
variance = fmap done $ foldl' step begin

    where

    begin = Tuple3' 0 0 0

    step (Tuple3' n mean_ m2) x = Tuple3' n' mean' m2'

        where

        n' = n + 1
        mean' = (n * mean_ + x) / (n + 1)
        delta = x - mean_
        m2' = m2 + delta * delta * n / (n + 1)

    done (Tuple3' n _ m2) = m2 / n

-- | Compute a numerically stable (population) standard deviation over all
-- elements in the input stream.
--
{-# DEPRECATED stdDev "Use the streamly-statistics package instead" #-}
{-# INLINE stdDev #-}
stdDev :: (Monad m, Floating a) => Fold m a a
stdDev = sqrt <$> variance

-- | Compute an 'Int' sized polynomial rolling hash
--
-- > H = salt * k ^ n + c1 * k ^ (n - 1) + c2 * k ^ (n - 2) + ... + cn * k ^ 0
--
-- Where @c1@, @c2@, @cn@ are the elements in the input stream and @k@ is a
-- constant.
--
-- This hash is often used in Rabin-Karp string search algorithm.
--
-- See https://en.wikipedia.org/wiki/Rolling_hash
--
{-# INLINE rollingHashWithSalt #-}
rollingHashWithSalt :: (Monad m, Enum a) => Int64 -> Fold m a Int64
rollingHashWithSalt = fromScanl . Scanl.rollingHashWithSalt

-- | Compute an 'Int' sized polynomial rolling hash of a stream.
--
-- >>> rollingHash = Fold.rollingHashWithSalt Fold.defaultSalt
--
{-# INLINE rollingHash #-}
rollingHash :: (Monad m, Enum a) => Fold m a Int64
rollingHash = fromScanl Scanl.rollingHash

-- | Compute an 'Int' sized polynomial rolling hash of the first n elements of
-- a stream.
--
-- >>> rollingHashFirstN n = Fold.take n Fold.rollingHash
--
-- /Pre-release/
{-# INLINE rollingHashFirstN #-}
rollingHashFirstN :: (Monad m, Enum a) => Int -> Fold m a Int64
rollingHashFirstN = fromScanl . Scanl.rollingHashFirstN

-- XXX Compare this with the implementation in Fold.Window, preferrably use the
-- latter if performance is good.

-- | Apply a function on every two successive elements of a stream. The first
-- argument of the map function is the previous element and the second argument
-- is the current element. When processing the very first element in the
-- stream, the previous element is 'Nothing'.
--
-- /Pre-release/
--
{-# INLINE rollingMapM #-}
rollingMapM :: Monad m => (Maybe a -> a -> m b) -> Fold m a b
rollingMapM = fromScanl . Scanl.rollingMapM

-- |
-- >>> rollingMap f = Fold.rollingMapM (\x y -> return $ f x y)
--
{-# INLINE rollingMap #-}
rollingMap :: Monad m => (Maybe a -> a -> b) -> Fold m a b
rollingMap = fromScanl . Scanl.rollingMap

------------------------------------------------------------------------------
-- Monoidal left folds
------------------------------------------------------------------------------

-- | Semigroup concat. Append the elements of an input stream to a provided
-- starting value.
--
-- Definition:
--
-- >>> sconcat = Fold.foldl' (<>)
--
-- >>> semigroups = fmap Data.Monoid.Sum $ Stream.enumerateFromTo 1 10
-- >>> Stream.fold (Fold.sconcat 10) semigroups
-- Sum {getSum = 65}
--
{-# INLINE sconcat #-}
sconcat :: (Monad m, Semigroup a) => a -> Fold m a a
sconcat = fromScanl . Scanl.sconcat

-- | Monoid concat. Fold an input stream consisting of monoidal elements using
-- 'mappend' and 'mempty'.
--
-- Definition:
--
-- >>> mconcat = Fold.sconcat mempty
--
-- >>> monoids = fmap Data.Monoid.Sum $ Stream.enumerateFromTo 1 10
-- >>> Stream.fold Fold.mconcat monoids
-- Sum {getSum = 55}
--
{-# INLINE mconcat #-}
mconcat ::
    ( Monad m
    , Monoid a) => Fold m a a
mconcat = fromScanl Scanl.mconcat

-- |
-- Definition:
--
-- >>> foldMap f = Fold.lmap f Fold.mconcat
--
-- Make a fold from a pure function that folds the output of the function
-- using 'mappend' and 'mempty'.
--
-- >>> sum = Fold.foldMap Data.Monoid.Sum
-- >>> Stream.fold sum $ Stream.enumerateFromTo 1 10
-- Sum {getSum = 55}
--
{-# INLINE foldMap #-}
foldMap :: (Monad m, Monoid b) => (a -> b) -> Fold m a b
foldMap = fromScanl . Scanl.foldMap

-- |
-- Definition:
--
-- >>> foldMapM f = Fold.lmapM f Fold.mconcat
--
-- Make a fold from a monadic function that folds the output of the function
-- using 'mappend' and 'mempty'.
--
-- >>> sum = Fold.foldMapM (return . Data.Monoid.Sum)
-- >>> Stream.fold sum $ Stream.enumerateFromTo 1 10
-- Sum {getSum = 55}
--
{-# INLINE foldMapM #-}
foldMapM ::  (Monad m, Monoid b) => (a -> m b) -> Fold m a b
foldMapM = fromScanl . Scanl.foldMapM

------------------------------------------------------------------------------
-- Partial Folds
------------------------------------------------------------------------------

-- | A fold that drains the first n elements of its input, running the effects
-- and discarding the results.
--
-- Definition:
--
-- >>> drainN n = Fold.take n Fold.drain
--
-- /Pre-release/
{-# INLINE drainN #-}
drainN :: Monad m => Int -> Fold m a ()
drainN = fromScanl . Scanl.drainN

------------------------------------------------------------------------------
-- To Elements
------------------------------------------------------------------------------

-- | Like 'index', except with a more general 'Integral' argument
--
-- /Pre-release/
{-# INLINE genericIndex #-}
genericIndex :: (Integral i, Monad m) => i -> Fold m a (Maybe a)
genericIndex i = foldt' step (Partial 0) (const Nothing)

    where

    step j a =
        if i == j
        then Done $ Just a
        else Partial (j + 1)

-- | Return the element at the given index.
--
-- Definition:
--
-- >>> index = Fold.genericIndex
--
{-# INLINE index #-}
index :: Monad m => Int -> Fold m a (Maybe a)
index = genericIndex

-- | Consume a single input and transform it using the supplied 'Maybe'
-- returning function.
--
-- /Pre-release/
--
{-# INLINE maybe #-}
maybe :: Monad m => (a -> Maybe b) -> Fold m a (Maybe b)
maybe f = foldt' (const (Done . f)) (Partial Nothing) id

-- | Consume a single element and return it if it passes the predicate else
-- return 'Nothing'.
--
-- Definition:
--
-- >>> satisfy f = Fold.maybe (\a -> if f a then Just a else Nothing)
--
-- /Pre-release/
{-# INLINE satisfy #-}
satisfy :: Monad m => (a -> Bool) -> Fold m a (Maybe a)
satisfy f = maybe (\a -> if f a then Just a else Nothing)
{-
satisfy f = Fold step (return $ Partial ()) (const (return Nothing))

    where

    step () a = return $ Done $ if f a then Just a else Nothing
-}

-- Naming notes:
--
-- "head" and "next" are two alternative names for the same API. head sounds
-- apt in the context of lists but next sounds more apt in the context of
-- streams where we think in terms of generating and consuming the next element
-- rather than taking the head of some static/persistent structure.
--
-- We also want to keep the nomenclature consistent across folds and parsers,
-- "head" becomes even more unintuitive for parsers because there are two
-- possible variants viz. peek and next.
--
-- Also, the "head" fold creates confusion in situations like
-- https://github.com/composewell/streamly/issues/1404 where intuitive
-- expectation from head is to consume the entire stream and just give us the
-- head. There we want to convey the notion that we consume one element from
-- the stream and stop. The name "one" already being used in parsers for this
-- purpose sounds more apt from this perspective.
--
-- The source of confusion is perhaps due to the fact that some folds consume
-- the entire stream and others terminate early. It may have been clearer if we
-- had separate abstractions for the two use cases.

-- XXX We can possibly use "head" for the purposes of reducing the entire
-- stream to the head element i.e. take the head and drain the rest.

-- | Take one element from the stream and stop.
--
-- Definition:
--
-- >>> one = Fold.maybe Just
--
-- This is similar to the stream 'Stream.uncons' operation.
--
{-# INLINE one #-}
one :: Monad m => Fold m a (Maybe a)
one = maybe Just

-- | Extract the first element of the stream, if any.
--
-- >>> head = Fold.one
--
{-# DEPRECATED head "Please use \"one\" instead" #-}
{-# INLINE head #-}
head :: Monad m => Fold m a (Maybe a)
head = one

-- | Returns the first element that satisfies the given predicate.
--
-- /Pre-release/
{-# INLINE findM #-}
findM :: Monad m => (a -> m Bool) -> Fold m a (Maybe a)
findM predicate =
    Fold step (return $ Partial ()) extract extract

    where

    step () a =
        let f r =
                if r
                then Done (Just a)
                else Partial ()
         in f <$> predicate a

    extract = const $ return Nothing

-- | Returns the first element that satisfies the given predicate.
--
{-# INLINE find #-}
find :: Monad m => (a -> Bool) -> Fold m a (Maybe a)
find p = findM (return . p)

-- | In a stream of (key-value) pairs @(a, b)@, return the value @b@ of the
-- first pair where the key equals the given value @a@.
--
-- Definition:
--
-- >>> lookup x = fmap snd <$> Fold.find ((== x) . fst)
--
{-# INLINE lookup #-}
lookup :: (Eq a, Monad m) => a -> Fold m (a,b) (Maybe b)
lookup a0 = foldt' step (Partial ()) (const Nothing)

    where

    step () (a, b) =
        if a == a0
        then Done $ Just b
        else Partial ()

-- | Returns the first index that satisfies the given predicate.
--
{-# INLINE findIndex #-}
findIndex :: Monad m => (a -> Bool) -> Fold m a (Maybe Int)
findIndex predicate = foldt' step (Partial 0) (const Nothing)

    where

    step i a =
        if predicate a
        then Done $ Just i
        else Partial (i + 1)

-- | Returns the index of the latest element if the element satisfies the given
-- predicate.
--
{-# INLINE findIndices #-}
findIndices :: Monad m => (a -> Bool) -> Fold m a (Maybe Int)
findIndices = fromScanl . Scanl.findIndices

-- | Returns the index of the latest element if the element matches the given
-- value.
--
-- Definition:
--
-- >>> elemIndices a = Fold.findIndices (== a)
--
{-# INLINE elemIndices #-}
elemIndices :: (Monad m, Eq a) => a -> Fold m a (Maybe Int)
elemIndices = fromScanl . Scanl.elemIndices

-- | Returns the first index where a given value is found in the stream.
--
-- Definition:
--
-- >>> elemIndex a = Fold.findIndex (== a)
--
{-# INLINE elemIndex #-}
elemIndex :: (Eq a, Monad m) => a -> Fold m a (Maybe Int)
elemIndex a = findIndex (== a)

------------------------------------------------------------------------------
-- To Boolean
------------------------------------------------------------------------------

-- Similar to 'eof' parser, but the fold consumes and discards an input element
-- when not at eof. XXX Remove or Rename to "eof"?

-- | Consume one element, return 'True' if successful else return 'False'. In
-- other words, test if the input is empty or not.
--
-- WARNING! It consumes one element if the stream is not empty. If that is not
-- what you want please use the eof parser instead.
--
-- Definition:
--
-- >>> null = fmap isJust Fold.one
--
{-# INLINE null #-}
null :: Monad m => Fold m a Bool
null = foldt' (\() _ -> Done False) (Partial ()) (const True)

-- | Returns 'True' if any element of the input satisfies the predicate.
--
-- Definition:
--
-- >>> any p = Fold.lmap p Fold.or
--
-- Example:
--
-- >>> Stream.fold (Fold.any (== 0)) $ Stream.fromList [1,0,1]
-- True
--
{-# INLINE any #-}
any :: Monad m => (a -> Bool) -> Fold m a Bool
any predicate = foldt' step initial id

    where

    initial = Partial False

    step _ a =
        if predicate a
        then Done True
        else Partial False

-- | Return 'True' if the given element is present in the stream.
--
-- Definition:
--
-- >>> elem a = Fold.any (== a)
--
{-# INLINE elem #-}
elem :: (Eq a, Monad m) => a -> Fold m a Bool
elem a = any (== a)

-- | Returns 'True' if all elements of the input satisfy the predicate.
--
-- Definition:
--
-- >>> all p = Fold.lmap p Fold.and
--
-- Example:
--
-- >>> Stream.fold (Fold.all (== 0)) $ Stream.fromList [1,0,1]
-- False
--
{-# INLINE all #-}
all :: Monad m => (a -> Bool) -> Fold m a Bool
all predicate = foldt' step initial id

    where

    initial = Partial True

    step _ a =
        if predicate a
        then Partial True
        else Done False

-- | Returns 'True' if the given element is not present in the stream.
--
-- Definition:
--
-- >>> notElem a = Fold.all (/= a)
--
{-# INLINE notElem #-}
notElem :: (Eq a, Monad m) => a -> Fold m a Bool
notElem a = all (/= a)

-- | Returns 'True' if all elements are 'True', 'False' otherwise
--
-- Definition:
--
-- >>> and = Fold.all (== True)
--
{-# INLINE and #-}
and :: Monad m => Fold m Bool Bool
and = all id

-- | Returns 'True' if any element is 'True', 'False' otherwise
--
-- Definition:
--
-- >>> or = Fold.any (== True)
--
{-# INLINE or #-}
or :: Monad m => Fold m Bool Bool
or = any id

------------------------------------------------------------------------------
-- Grouping/Splitting
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Grouping without looking at elements
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------

-- | @splitAt n f1 f2@ composes folds @f1@ and @f2@ such that first @n@
-- elements of its input are consumed by fold @f1@ and the rest of the stream
-- is consumed by fold @f2@.
--
-- >>> let splitAt_ n xs = Stream.fold (Fold.splitAt n Fold.toList Fold.toList) $ Stream.fromList xs
--
-- >>> splitAt_ 6 "Hello World!"
-- ("Hello ","World!")
--
-- >>> splitAt_ (-1) [1,2,3]
-- ([],[1,2,3])
--
-- >>> splitAt_ 0 [1,2,3]
-- ([],[1,2,3])
--
-- >>> splitAt_ 1 [1,2,3]
-- ([1],[2,3])
--
-- >>> splitAt_ 3 [1,2,3]
-- ([1,2,3],[])
--
-- >>> splitAt_ 4 [1,2,3]
-- ([1,2,3],[])
--
-- > splitAt n f1 f2 = Fold.splitWith (,) (Fold.take n f1) f2
--
-- /Internal/

{-# INLINE splitAt #-}
splitAt
    :: Monad m
    => Int
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
splitAt n fld = splitWith (,) (take n fld)

------------------------------------------------------------------------------
-- Element Aware APIs
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------

{-# INLINE takingEndByM #-}
takingEndByM :: Monad m => (a -> m Bool) -> Fold m a (Maybe a)
takingEndByM p = Fold step initial extract extract

    where

    initial = return $ Partial Nothing'

    step _ a = do
        r <- p a
        return
            $ if r
              then Done $ Just a
              else Partial $ Just' a

    extract = return . toMaybe

-- |
--
-- >>> takingEndBy p = Fold.takingEndByM (return . p)
--
{-# INLINE takingEndBy #-}
takingEndBy :: Monad m => (a -> Bool) -> Fold m a (Maybe a)
takingEndBy p = takingEndByM (return . p)

{-# INLINE takingEndByM_ #-}
takingEndByM_ :: Monad m => (a -> m Bool) -> Fold m a (Maybe a)
takingEndByM_ p = Fold step initial extract extract

    where

    initial = return $ Partial Nothing'

    step _ a = do
        r <- p a
        return
            $ if r
              then Done Nothing
              else Partial $ Just' a

    extract = return . toMaybe

-- |
--
-- >>> takingEndBy_ p = Fold.takingEndByM_ (return . p)
--
{-# INLINE takingEndBy_ #-}
takingEndBy_ :: Monad m => (a -> Bool) -> Fold m a (Maybe a)
takingEndBy_ p = takingEndByM_ (return . p)

{-# INLINE droppingWhileM #-}
droppingWhileM :: Monad m => (a -> m Bool) -> Fold m a (Maybe a)
droppingWhileM p = Fold step initial extract extract

    where

    initial = return $ Partial Nothing'

    step Nothing' a = do
        r <- p a
        return
            $ Partial
            $ if r
              then Nothing'
              else Just' a
    step _ a = return $ Partial $ Just' a

    extract = return . toMaybe

-- |
-- >>> droppingWhile p = Fold.droppingWhileM (return . p)
--
{-# INLINE droppingWhile #-}
droppingWhile :: Monad m => (a -> Bool) -> Fold m a (Maybe a)
droppingWhile p = droppingWhileM (return . p)

------------------------------------------------------------------------------
-- Binary splitting on a separator
------------------------------------------------------------------------------

data SplitOnSeqState mba acc a rh w ck =
      SplitOnSeqEmpty !acc
    | SplitOnSeqSingle !acc !a
    | SplitOnSeqWord !acc !Int !w
    | SplitOnSeqWordLoop !acc !w
    | SplitOnSeqKR !acc !Int !mba
    | SplitOnSeqKRLoop !acc !ck !mba !rh

-- XXX Need to add tests for takeEndBySeq, we have tests for takeEndBySeq_ .

-- | Continue taking the input until the input sequence matches the supplied
-- sequence, taking the supplied sequence as well. If the pattern is empty this
-- acts as an identity fold.
--
-- >>> s = Stream.fromList "Gauss---Euler---Noether"
-- >>> f = Fold.takeEndBySeq (Array.fromList "---") Fold.toList
-- >>> Stream.fold f s
-- "Gauss---"
--
-- >>> Stream.fold Fold.toList $ Stream.foldMany f s
-- ["Gauss---","Euler---","Noether"]
--
-- Uses Rabin-Karp algorithm for substring search.
--
-- See also: 'Streamly.Data.Stream.splitOnSeq' and
-- 'Streamly.Data.Stream.splitEndBySeq'.
--
-- /Pre-release/
{-# INLINE takeEndBySeq #-}
takeEndBySeq :: forall m a b. (MonadIO m, Unbox a, Enum a, Eq a) =>
       Array.Array a
    -> Fold m a b
    -> Fold m a b
takeEndBySeq patArr (Fold fstep finitial fextract ffinal) =
    Fold step initial extract final

    where

    patLen = Array.length patArr
    patBytes = Array.byteLength patArr
    maxIndex = patLen - 1
    maxOffset = patBytes - SIZE_OF(a)

    initial = do
        res <- finitial
        case res of
            Partial acc
                | patLen == 0 ->
                    -- XXX Should we match nothing or everything on empty
                    -- pattern?
                    -- Done <$> ffinal acc
                    return $ Partial $ SplitOnSeqEmpty acc
                | patLen == 1 -> do
                    pat <- liftIO $ Array.unsafeGetIndexIO 0 patArr
                    return $ Partial $ SplitOnSeqSingle acc pat
                | SIZE_OF(a) * patLen <= sizeOf (Proxy :: Proxy Word) ->
                    return $ Partial $ SplitOnSeqWord acc 0 0
                | otherwise -> do
                    (MutArray mba _ _ _) :: MutArray a <-
                        liftIO $ MA.emptyOf patLen
                    return $ Partial $ SplitOnSeqKR acc 0 mba
            Done b -> return $ Done b

    -- Word pattern related
    elemBits = SIZE_OF(a) * 8

    wordMask :: Word
    wordMask = (1 `shiftL` (elemBits * patLen)) - 1

    wordPat :: Word
    wordPat = wordMask .&. Array.foldl' addToWord 0 patArr

    addToWord wd a = (wd `shiftL` elemBits) .|. fromIntegral (fromEnum a)

    -- For Rabin-Karp search
    k = 2891336453 :: Word32
    coeff = k ^ patLen

    addCksum cksum a = cksum * k + fromIntegral (fromEnum a)

    deltaCksum cksum old new =
        addCksum cksum new - coeff * fromIntegral (fromEnum old)

    -- XXX shall we use a random starting hash or 1 instead of 0?
    -- XXX Need to keep this cached across fold calls in foldmany
    -- XXX We may need refold to inject the cached state instead of
    -- initializing the state every time.
    -- XXX Allocation of ring buffer should also be done once
    patHash = Array.foldl' addCksum 0 patArr

    step (SplitOnSeqEmpty s) x = do
        res <- fstep s x
        case res of
            Partial s1 -> return $ Partial $ SplitOnSeqEmpty s1
            Done b -> return $ Done b
    step (SplitOnSeqSingle s pat) x = do
        res <- fstep s x
        case res of
            Partial s1
                | pat /= x -> return $ Partial $ SplitOnSeqSingle s1 pat
                | otherwise -> Done <$> ffinal s1
            Done b -> return $ Done b
    step (SplitOnSeqWord s idx wrd) x = do
        res <- fstep s x
        let wrd1 = addToWord wrd x
        case res of
            Partial s1
                | idx == maxIndex -> do
                    if wrd1 .&. wordMask == wordPat
                    then Done <$> ffinal s1
                    else return $ Partial $ SplitOnSeqWordLoop s1 wrd1
                | otherwise ->
                    return $ Partial $ SplitOnSeqWord s1 (idx + 1) wrd1
            Done b -> return $ Done b
    step (SplitOnSeqWordLoop s wrd) x = do
        res <- fstep s x
        let wrd1 = addToWord wrd x
        case res of
            Partial s1
                | wrd1 .&. wordMask == wordPat ->
                    Done <$> ffinal s1
                | otherwise ->
                    return $ Partial $ SplitOnSeqWordLoop s1 wrd1
            Done b -> return $ Done b
    step (SplitOnSeqKR s offset mba) x = do
        res <- fstep s x
        case res of
            Partial s1 -> do
                liftIO $ pokeAt offset mba x
                if offset == maxOffset
                then do
                    let arr :: Array a = Array
                                { arrContents = mba
                                , arrStart = 0
                                , arrEnd = patBytes
                                }
                    let ringHash = Array.foldl' addCksum 0 arr
                    if ringHash == patHash && Array.byteEq arr patArr
                    then Done <$> ffinal s1
                    else return $ Partial $ SplitOnSeqKRLoop s1 ringHash mba 0
                else
                    return $ Partial $ SplitOnSeqKR s1 (offset + SIZE_OF(a)) mba
            Done b -> return $ Done b
    step (SplitOnSeqKRLoop s cksum mba offset) x = do
        res <- fstep s x
        case res of
            Partial s1 -> do
                let rb = RingArray
                        { ringContents = mba
                        , ringSize = patBytes
                        , ringHead = offset
                        }
                (rb1, old :: a) <- liftIO (RingArray.replace rb x)
                let ringHash = deltaCksum cksum old x
                let rh1 = ringHead rb1
                matches <-
                    if ringHash == patHash
                    then liftIO $ RingArray.eqArray rb1 patArr
                    else return False
                if matches
                then Done <$> ffinal s1
                else return $ Partial $ SplitOnSeqKRLoop s1 ringHash mba rh1
            Done b -> return $ Done b

    extractFunc fex state =
        let st =
                case state of
                    SplitOnSeqEmpty s -> s
                    SplitOnSeqSingle s _ -> s
                    SplitOnSeqWord s _ _ -> s
                    SplitOnSeqWordLoop s _ -> s
                    SplitOnSeqKR s _ _ -> s
                    SplitOnSeqKRLoop s _ _ _ -> s
        in fex st

    extract = extractFunc fextract

    final = extractFunc ffinal

-- | Like 'takeEndBySeq' but discards the matched sequence.
--
-- >>> s = Stream.fromList "Gauss---Euler---Noether"
-- >>> f = Fold.takeEndBySeq_ (Array.fromList "---") Fold.toList
-- >>> Stream.fold f s
-- "Gauss"
--
-- >>> Stream.fold Fold.toList $ Stream.foldMany f s
-- ["Gauss","Euler","Noether"]
--
-- See also: 'Streamly.Data.Stream.splitOnSeq' and
-- 'Streamly.Data.Stream.splitEndBySeq_'.
--
-- /Pre-release/
--
{-# INLINE takeEndBySeq_ #-}
takeEndBySeq_ :: forall m a b. (MonadIO m, Unbox a, Enum a, Eq a) =>
       Array.Array a
    -> Fold m a b
    -> Fold m a b
takeEndBySeq_ patArr (Fold fstep finitial fextract ffinal) =
    Fold step initial extract final

    where

    patLen = Array.length patArr
    patBytes = Array.byteLength patArr
    maxIndex = patLen - 1
    maxOffset = patBytes - SIZE_OF(a)

    initial = do
        res <- finitial
        case res of
            Partial acc
                | patLen == 0 ->
                    -- XXX Should we match nothing or everything on empty
                    -- pattern?
                    -- Done <$> ffinal acc
                    return $ Partial $ SplitOnSeqEmpty acc
                | patLen == 1 -> do
                    pat <- liftIO $ Array.unsafeGetIndexIO 0 patArr
                    return $ Partial $ SplitOnSeqSingle acc pat
                -- XXX Need to add tests for this case
                | SIZE_OF(a) * patLen <= sizeOf (Proxy :: Proxy Word) ->
                    return $ Partial $ SplitOnSeqWord acc 0 0
                | otherwise -> do
                    (MutArray mba _ _ _) :: MutArray a <-
                        liftIO $ MA.emptyOf patLen
                    return $ Partial $ SplitOnSeqKR acc 0 mba
            Done b -> return $ Done b

    -- Word pattern related
    elemBits = SIZE_OF(a) * 8

    wordMask :: Word
    wordMask = (1 `shiftL` (elemBits * patLen)) - 1

    elemMask :: Word
    elemMask = (1 `shiftL` elemBits) - 1

    wordPat :: Word
    wordPat = wordMask .&. Array.foldl' addToWord 0 patArr

    addToWord wd a = (wd `shiftL` elemBits) .|. fromIntegral (fromEnum a)

    -- For Rabin-Karp search
    k = 2891336453 :: Word32
    coeff = k ^ patLen

    addCksum cksum a = cksum * k + fromIntegral (fromEnum a)

    deltaCksum cksum old new =
        addCksum cksum new - coeff * fromIntegral (fromEnum old)

    -- XXX shall we use a random starting hash or 1 instead of 0?
    -- XXX Need to keep this cached across fold calls in foldMany
    -- XXX We may need refold to inject the cached state instead of
    -- initializing the state every time.
    -- XXX Allocation of ring buffer should also be done once
    patHash = Array.foldl' addCksum 0 patArr

    step (SplitOnSeqEmpty s) x = do
        res <- fstep s x
        case res of
            Partial s1 -> return $ Partial $ SplitOnSeqEmpty s1
            Done b -> return $ Done b
    step (SplitOnSeqSingle s pat) x = do
        if pat /= x
        then do
            res <- fstep s x
            case res of
                Partial s1 -> return $ Partial $ SplitOnSeqSingle s1 pat
                Done b -> return $ Done b
        else Done <$> ffinal s
    step (SplitOnSeqWord s idx wrd) x = do
        let wrd1 = addToWord wrd x
        if idx == maxIndex
        then do
            if wrd1 .&. wordMask == wordPat
            then Done <$> ffinal s
            else return $ Partial $ SplitOnSeqWordLoop s wrd1
        else return $ Partial $ SplitOnSeqWord s (idx + 1) wrd1
    step (SplitOnSeqWordLoop s wrd) x = do
        let wrd1 = addToWord wrd x
            old = (wordMask .&. wrd)
                    `shiftR` (elemBits * (patLen - 1))
        res <- fstep s (toEnum $ fromIntegral old)
        case res of
            Partial s1
                | wrd1 .&. wordMask == wordPat ->
                    Done <$> ffinal s1
                | otherwise ->
                    return $ Partial $ SplitOnSeqWordLoop s1 wrd1
            Done b -> return $ Done b
    step (SplitOnSeqKR s offset mba) x = do
        liftIO $ pokeAt offset mba x
        if offset == maxOffset
        then do
            let arr :: Array a = Array
                        { arrContents = mba
                        , arrStart = 0
                        , arrEnd = patBytes
                        }
            let ringHash = Array.foldl' addCksum 0 arr
            if ringHash == patHash && Array.byteEq arr patArr
            then Done <$> ffinal s
            else return $ Partial $ SplitOnSeqKRLoop s ringHash mba 0
        else return $ Partial $ SplitOnSeqKR s (offset + SIZE_OF(a)) mba
    step (SplitOnSeqKRLoop s cksum mba offset) x = do
        let rb = RingArray
                { ringContents = mba
                , ringSize = patBytes
                , ringHead = offset
                }
        (rb1, old :: a) <- liftIO (RingArray.replace rb x)
        res <- fstep s old
        case res of
            Partial s1 -> do
                let ringHash = deltaCksum cksum old x
                let rh1 = ringHead rb1
                matches <-
                    if ringHash == patHash
                    then liftIO $ RingArray.eqArray rb1 patArr
                    else return False
                if matches
                then Done <$> ffinal s1
                else return $ Partial $ SplitOnSeqKRLoop s1 ringHash mba rh1
            Done b -> return $ Done b

    -- XXX extract should return backtrack count as well. If the fold
    -- terminates early inside extract, we may still have buffered data
    -- remaining which will be lost if we do not communicate that to the
    -- driver.
    extractFunc fex state = do
        let consumeWord s n wrd = do
                if n == 0
                then fex s
                else do
                    let old = elemMask .&. (wrd `shiftR` (elemBits * (n - 1)))
                    r <- fstep s (toEnum $ fromIntegral old)
                    case r of
                        Partial s1 -> consumeWord s1 (n - 1) wrd
                        Done b -> return b

        let consumeArray s end mba offset =
                if offset == end
                then fex s
                else do
                    old <- liftIO $ peekAt offset mba
                    r <- fstep s old
                    case r of
                        Partial s1 ->
                            consumeArray s1 end mba (offset + SIZE_OF(a))
                        Done b -> return b

        let consumeRing s orig mba offset = do
                let rb :: RingArray a = RingArray
                            { ringContents = mba
                            , ringSize = patBytes
                            , ringHead = offset
                            }
                old <- RingArray.unsafeGetHead rb
                let rb1 = RingArray.moveForward rb
                r <- fstep s old
                case r of
                    Partial s1 ->
                        let rh = ringHead rb1
                         in if rh == orig
                            then fex s1
                            else consumeRing s1 orig mba rh
                    Done b -> return b

        case state of
            SplitOnSeqEmpty s -> fex s
            SplitOnSeqSingle s _ -> fex s
            SplitOnSeqWord s idx wrd -> consumeWord s idx wrd
            SplitOnSeqWordLoop s wrd -> consumeWord s patLen wrd
            SplitOnSeqKR s end mba -> consumeArray s end mba 0
            SplitOnSeqKRLoop s _ mba rh -> consumeRing s rh mba rh

    extract = extractFunc fextract

    final = extractFunc ffinal

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
--
--  Definition:
--
-- >>> tee = Fold.teeWith (,)
--
-- Example:
--
-- >>> t = Fold.tee Fold.sum Fold.length
-- >>> Stream.fold t (Stream.enumerateFromTo 1.0 100.0)
-- (5050.0,100)
--
{-# INLINE tee #-}
tee :: Monad m => Fold m a b -> Fold m a c -> Fold m a (b,c)
tee = teeWith (,)

-- XXX use "List" instead of "[]"?, use Array for output to scale it to a large
-- number of consumers? For polymorphic case a vector could be helpful. For
-- Unboxs we can use arrays. Will need separate APIs for those.
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
-- >>> Stream.fold (Fold.distribute [Fold.sum, Fold.length]) (Stream.enumerateFromTo 1 5)
-- [15,5]
--
-- >>> distribute = Prelude.foldr (Fold.teeWith (:)) (Fold.fromPure [])
--
-- This is the consumer side dual of the producer side 'sequence' operation.
--
-- Stops when all the folds stop.
--
{-# INLINE distribute #-}
distribute :: Monad m => [Fold m a b] -> Fold m a [b]
distribute = Prelude.foldr (teeWith (:)) (fromPure [])

-- XXX use mutable cells for better performance.

-- | Distribute the input to the folds returned by an effect. The effect is
-- executed every time an input is processed, and the folds returned by it are
-- added to the distribution list. The scan returns the results of the folds as
-- they complete. To avoid adding the same folds repeatedly, the action must
-- return the folds only once e.g. it can be implemented using modifyIORef
-- replacing the original value by an empty list before returning it.
--
-- >>> import Data.IORef
-- >>> ref <- newIORef [Fold.take 2 Fold.sum, Fold.take 2 Fold.length :: Fold IO Int Int]
-- >>> gen = atomicModifyIORef ref (\xs -> ([], xs))
-- >>> Stream.toList $ Stream.scanl (Fold.distributeScan gen) (Stream.enumerateFromTo 1 10)
-- [[],[],[],[2,3],[],[],[],[],[],[],[]]
--
{-# INLINE distributeScan #-}
distributeScan :: Monad m => m [Fold m a b] -> Scanl m a [b]
distributeScan getFolds = Scanl consume initial extract final

    where

    initial = return $ Partial (Tuple' [] [])

    run st [] _ = return $ Partial st
    run (Tuple' ys zs) (Fold step init extr fin : xs) a = do
        res <- init
        case res of
            Partial fs -> do
              r <- step fs a
              run (Tuple' (Fold step (return r) extr fin : ys) zs) xs a
            Done b -> do
              run (Tuple' ys (b : zs)) xs a

    consume (Tuple' st _) x = do
        xs <- getFolds
        xs1 <- Prelude.mapM reduce xs
        let st1 = st ++ xs1
        run (Tuple' [] []) st1 x

    extract (Tuple' _ done) = return done

    final (Tuple' st done) = do
        Prelude.mapM_ finalM st
        return done

------------------------------------------------------------------------------
-- Partitioning
------------------------------------------------------------------------------

{-# INLINE partitionByMUsing #-}
partitionByMUsing :: Monad m =>
       (  (x -> y -> (x, y))
       -> Fold m (Either b c) x
       -> Fold m (Either b c) y
       -> Fold m (Either b c) (x, y)
       )
    -> (a -> m (Either b c))
    -> Fold m b x
    -> Fold m c y
    -> Fold m a (x, y)
partitionByMUsing t f fld1 fld2 =
    let l = lmap (fromLeft undefined) fld1  -- :: Fold m (Either b c) x
        r = lmap (fromRight undefined) fld2 -- :: Fold m (Either b c) y
     in lmapM f (t (,) (filter isLeft l) (filter isRight r))

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
-- Example, send input to either fold randomly:
--
-- >>> :set -package random
-- >>> import System.Random (randomIO)
-- >>> randomly a = randomIO >>= \x -> return $ if x then Left a else Right a
-- >>> f = Fold.partitionByM randomly Fold.length Fold.length
-- >>> Stream.fold f (Stream.enumerateFromTo 1 100)
-- ...
--
-- Example, send input to the two folds in a proportion of 2:1:
--
-- >>> :set -fno-warn-unrecognised-warning-flags
-- >>> :set -fno-warn-x-partial
-- >>> :{
-- proportionately m n = do
--  ref <- newIORef $ cycle $ concat [replicate m Left, replicate n Right]
--  return $ \a -> do
--      r <- readIORef ref
--      writeIORef ref $ tail r
--      return $ Prelude.head r a
-- :}
--
-- >>> :{
-- main = do
--  g <- proportionately 2 1
--  let f = Fold.partitionByM g Fold.length Fold.length
--  r <- Stream.fold f (Stream.enumerateFromTo (1 :: Int) 100)
--  print r
-- :}
--
-- >>> main
-- (67,33)
--
--
-- This is the consumer side dual of the producer side 'mergeBy' operation.
--
-- When one fold is done, any input meant for it is ignored until the other
-- fold is also done.
--
-- Stops when both the folds stop.
--
-- /See also: 'partitionByFstM' and 'partitionByMinM'./
--
-- /Pre-release/
{-# INLINE partitionByM #-}
partitionByM :: Monad m
    => (a -> m (Either b c)) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionByM = partitionByMUsing teeWith

-- | Similar to 'partitionByM' but terminates when the first fold terminates.
--
{-# INLINE partitionByFstM #-}
partitionByFstM :: Monad m
    => (a -> m (Either b c)) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionByFstM = partitionByMUsing teeWithFst

-- | Similar to 'partitionByM' but terminates when any fold terminates.
--
{-# INLINE partitionByMinM #-}
partitionByMinM :: Monad m =>
    (a -> m (Either b c)) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionByMinM = partitionByMUsing teeWithMin

-- Note: we could use (a -> Bool) instead of (a -> Either b c), but the latter
-- makes the signature clearer as to which case belongs to which fold.
-- XXX need to check the performance in both cases.

-- | Same as 'partitionByM' but with a pure partition function.
--
-- Example, count even and odd numbers in a stream:
--
-- >>> :{
--  let f = Fold.partitionBy (\n -> if even n then Left n else Right n)
--                      (fmap (("Even " ++) . show) Fold.length)
--                      (fmap (("Odd "  ++) . show) Fold.length)
--   in Stream.fold f (Stream.enumerateFromTo 1 100)
-- :}
-- ("Even 50","Odd 50")
--
-- /Pre-release/
{-# INLINE partitionBy #-}
partitionBy :: Monad m
    => (a -> Either b c) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionBy f = partitionByM (return . f)

-- | Compose two folds such that the combined fold accepts a stream of 'Either'
-- and routes the 'Left' values to the first fold and 'Right' values to the
-- second fold.
--
-- Definition:
--
-- >>> partition = Fold.partitionBy id
--
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

------------------------------------------------------------------------------
-- Unzipping
------------------------------------------------------------------------------

{-# INLINE unzipWithMUsing #-}
unzipWithMUsing :: Monad m =>
       (  (x -> y -> (x, y))
       -> Fold m (b, c) x
       -> Fold m (b, c) y
       -> Fold m (b, c) (x, y)
       )
    -> (a -> m (b, c))
    -> Fold m b x
    -> Fold m c y
    -> Fold m a (x, y)
unzipWithMUsing t f fld1 fld2 =
    let f1 = lmap fst fld1  -- :: Fold m (b, c) b
        f2 = lmap snd fld2  -- :: Fold m (b, c) c
     in lmapM f (t (,) f1 f2)

-- | Like 'unzipWith' but with a monadic splitter function.
--
-- Definition:
--
-- >>> unzipWithM k f1 f2 = Fold.lmapM k (Fold.unzip f1 f2)
--
-- /Pre-release/
{-# INLINE unzipWithM #-}
unzipWithM :: Monad m
    => (a -> m (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWithM = unzipWithMUsing teeWith

-- | Similar to 'unzipWithM' but terminates when the first fold terminates.
--
{-# INLINE unzipWithFstM #-}
unzipWithFstM :: Monad m =>
    (a -> m (b, c)) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
unzipWithFstM = unzipWithMUsing teeWithFst

-- | Similar to 'unzipWithM' but terminates when any fold terminates.
--
{-# INLINE unzipWithMinM #-}
unzipWithMinM :: Monad m =>
    (a -> m (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWithMinM = unzipWithMUsing teeWithMin

-- | Split elements in the input stream into two parts using a pure splitter
-- function, direct each part to a different fold and zip the results.
--
-- Definitions:
--
-- >>> unzipWith f = Fold.unzipWithM (return . f)
-- >>> unzipWith f fld1 fld2 = Fold.lmap f (Fold.unzip fld1 fld2)
--
-- This fold terminates when both the input folds terminate.
--
-- /Pre-release/
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
-- Definition:
--
-- >>> unzip = Fold.unzipWith id
--
-- This is the consumer side dual of the producer side 'zip' operation.
--
{-# INLINE unzip #-}
unzip :: Monad m => Fold m a x -> Fold m b y -> Fold m (a,b) (x,y)
unzip = unzipWith id

------------------------------------------------------------------------------
-- Combining streams and folds - Zipping
------------------------------------------------------------------------------

-- XXX These can be implemented using the fold scan, using the stream as a
-- state.
-- XXX Stream Skip state cannot be efficiently handled in folds but can be
-- handled in parsers using the Continue facility. See zipWithM in the Parser
-- module.
--
-- cmpBy, eqBy, isPrefixOf, isSubsequenceOf etc can be implemented using
-- zipStream.

-- | Zip a stream with the input of a fold using the supplied function.
--
-- /Unimplemented/
--
{-# INLINE zipStreamWithM #-}
zipStreamWithM :: -- Monad m =>
    (a -> b -> m c) -> Stream m a -> Fold m c x -> Fold m b x
zipStreamWithM = undefined

-- | Zip a stream with the input of a fold.
--
-- >>> zip = Fold.zipStreamWithM (curry return)
--
-- /Unimplemented/
--
{-# INLINE zipStream #-}
zipStream :: Monad m => Stream m a -> Fold m (a, b) x -> Fold m b x
zipStream = zipStreamWithM (curry return)

-- | Pair each element of a fold input with its index, starting from index 0.
--
{-# DEPRECATED indexingWith "Use Scanl.indexingWith instead" #-}
{-# INLINE indexingWith #-}
indexingWith :: Monad m => Int -> (Int -> Int) -> Fold m a (Maybe (Int, a))
indexingWith i f = fmap toMaybe $ foldl' step initial

    where

    initial = Nothing'

    step Nothing' a = Just' (i, a)
    step (Just' (n, _)) a = Just' (f n, a)

-- |
-- >> indexing = Fold.indexingWith 0 (+ 1)
--
{-# DEPRECATED indexing "Use Scanl.indexing instead" #-}
{-# INLINE indexing #-}
indexing :: Monad m => Fold m a (Maybe (Int, a))
indexing = indexingWith 0 (+ 1)

-- |
-- >> indexingRev n = Fold.indexingWith n (subtract 1)
--
{-# DEPRECATED indexingRev "Use Scanl.indexingRev instead" #-}
{-# INLINE indexingRev #-}
indexingRev :: Monad m => Int -> Fold m a (Maybe (Int, a))
indexingRev n = indexingWith n (subtract 1)

-- | Pair each element of a fold input with its index, starting from index 0.
--
-- >>> indexed = Fold.postscanlMaybe Scanl.indexing
--
{-# INLINE indexed #-}
indexed :: Monad m => Fold m (Int, a) b -> Fold m a b
indexed = postscanlMaybe Scanl.indexing

-- | Change the predicate function of a Fold from @a -> b@ to accept an
-- additional state input @(s, a) -> b@. Convenient to filter with an
-- addiitonal index or time input.
--
-- >>> filterWithIndex = Fold.with Fold.indexed Fold.filter
--
-- @
-- filterWithAbsTime = with timestamped filter
-- filterWithRelTime = with timeIndexed filter
-- @
--
-- /Pre-release/
{-# INLINE with #-}
with ::
       (Fold m (s, a) b -> Fold m a b)
    -> (((s, a) -> c) -> Fold m (s, a) b -> Fold m (s, a) b)
    -> (((s, a) -> c) -> Fold m a b -> Fold m a b)
with f comb g = f . comb g . lmap snd

-- XXX Implement as a filter
-- sampleFromthen :: Monad m => Int -> Int -> Fold m a (Maybe a)

-- | @sampleFromthen offset stride@ samples the element at @offset@ index and
-- then every element at strides of @stride@.
--
{-# INLINE sampleFromthen #-}
sampleFromthen :: Monad m => Int -> Int -> Fold m a b -> Fold m a b
sampleFromthen offset size =
    with indexed filter (\(i, _) -> (i + offset) `mod` size == 0)

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------

-- | @concatSequence f t@ applies folds from stream @t@ sequentially and
-- collects the results using the fold @f@.
--
-- /Unimplemented/
--
{-# INLINE concatSequence #-}
concatSequence ::
    -- IsStream t =>
    Fold m b c -> t (Fold m a b) -> Fold m a c
concatSequence _f _p = undefined

-- | Group the input stream into groups of elements between @low@ and @high@.
-- Collection starts in chunks of @low@ and then keeps doubling until we reach
-- @high@. Each chunk is folded using the provided fold function.
--
-- This could be useful, for example, when we are folding a stream of unknown
-- size to a stream of arrays and we want to minimize the number of
-- allocations.
--
-- NOTE: this would be an application of "many" using a terminating fold.
--
-- /Unimplemented/
--
{-# INLINE chunksBetween #-}
chunksBetween :: -- Monad m =>
       Int -> Int -> Fold m a b -> Fold m b c -> Fold m a c
chunksBetween _low _high _f1 _f2 = undefined

-- | A fold that buffers its input to a pure stream.
--
-- /Warning!/ working on large streams accumulated as buffers in memory could
-- be very inefficient, consider using "Streamly.Data.Array" instead.
--
-- >>> toStream = fmap Stream.fromList Fold.toList
--
-- /Pre-release/
{-# INLINE toStream #-}
toStream :: (Monad m, Monad n) => Fold m a (Stream n a)
toStream = fromScanl Scanl.toStream

-- This is more efficient than 'toStream'. toStream is exactly the same as
-- reversing the stream after toStreamRev.
--
-- | Buffers the input stream to a pure stream in the reverse order of the
-- input.
--
-- >>> toStreamRev = fmap Stream.fromList Fold.toListRev
--
-- /Warning!/ working on large streams accumulated as buffers in memory could
-- be very inefficient, consider using "Streamly.Data.Array" instead.
--
-- /Pre-release/

--  xn : ... : x2 : x1 : []
{-# INLINE toStreamRev #-}
toStreamRev :: (Monad m, Monad n) => Fold m a (Stream n a)
toStreamRev = fromScanl Scanl.toStreamRev

-- XXX This does not fuse. It contains a recursive step function. We will need
-- a Skip input constructor in the fold type to make it fuse.
--
-- | Unfold and flatten the input stream of a fold.
--
-- @
-- Stream.fold (unfoldMany u f) = Stream.fold f . Stream.unfoldMany u
-- @
--
-- /Pre-release/
{-# INLINE unfoldMany #-}
unfoldMany :: Monad m => Unfold m a b -> Fold m b c -> Fold m a c
unfoldMany (Unfold ustep inject) (Fold fstep initial extract final) =
    Fold consume initial extract final

    where

    {-# INLINE produce #-}
    produce fs us = do
        ures <- ustep us
        case ures of
            StreamD.Yield b us1 -> do
                fres <- fstep fs b
                case fres of
                    Partial fs1 -> produce fs1 us1
                    -- XXX What to do with the remaining stream?
                    Done c -> return $ Done c
            StreamD.Skip us1 -> produce fs us1
            StreamD.Stop -> return $ Partial fs

    {-# INLINE_LATE consume #-}
    consume s a = inject a >>= produce s

-- | Get the bottom most @n@ elements using the supplied comparison function.
--
{-# INLINE bottomBy #-}
bottomBy :: (MonadIO m, Unbox a) =>
       (a -> a -> Ordering)
    -> Int
    -> Fold m a (MutArray a)
bottomBy cmp = fromScanl . Scanl.bottomBy cmp

-- | Get the top @n@ elements using the supplied comparison function.
--
-- To get bottom n elements instead:
--
-- >>> bottomBy cmp = Fold.topBy (flip cmp)
--
-- Example:
--
-- >>> stream = Stream.fromList [2::Int,7,9,3,1,5,6,11,17]
-- >>> Stream.fold (Fold.topBy compare 3) stream >>= MutArray.toList
-- [17,11,9]
--
-- /Pre-release/
--
{-# INLINE topBy #-}
topBy :: (MonadIO m, Unbox a) =>
       (a -> a -> Ordering)
    -> Int
    -> Fold m a (MutArray a)
topBy cmp = bottomBy (flip cmp)

-- | Fold the input stream to top n elements.
--
-- Definition:
--
-- >>> top = Fold.topBy compare
--
-- >>> stream = Stream.fromList [2::Int,7,9,3,1,5,6,11,17]
-- >>> Stream.fold (Fold.top 3) stream >>= MutArray.toList
-- [17,11,9]
--
-- /Pre-release/
{-# INLINE top #-}
top :: (MonadIO m, Unbox a, Ord a) => Int -> Fold m a (MutArray a)
top = fromScanl . Scanl.top

-- | Fold the input stream to bottom n elements.
--
-- Definition:
--
-- >>> bottom = Fold.bottomBy compare
--
-- >>> stream = Stream.fromList [2::Int,7,9,3,1,5,6,11,17]
-- >>> Stream.fold (Fold.bottom 3) stream >>= MutArray.toList
-- [1,2,3]
--
-- /Pre-release/
{-# INLINE bottom #-}
bottom :: (MonadIO m, Unbox a, Ord a) => Int -> Fold m a (MutArray a)
bottom = fromScanl . Scanl.bottom

------------------------------------------------------------------------------
-- Interspersed parsing
------------------------------------------------------------------------------

data IntersperseQState fs ps =
      IntersperseQUnquoted !fs !ps
    | IntersperseQQuoted !fs !ps
    | IntersperseQQuotedEsc !fs !ps

-- Useful for parsing CSV with quoting and escaping
{-# INLINE intersperseWithQuotes #-}
intersperseWithQuotes :: (Monad m, Eq a) =>
    a -> a -> a -> Fold m a b -> Fold m b c -> Fold m a c
intersperseWithQuotes
    quote
    esc
    separator
    (Fold stepL initialL _ finalL)
    (Fold stepR initialR extractR finalR) = Fold step initial extract final

    where

    errMsg p status =
        error $ "intersperseWithQuotes: " ++ p ++ " parsing fold cannot "
                ++ status ++ " without input"

    {-# INLINE initL #-}
    initL mkState = do
        resL <- initialL
        case resL of
            Partial sL ->
                return $ Partial $ mkState sL
            Done _ ->
                errMsg "content" "succeed"

    initial = do
        res <- initialR
        case res of
            Partial sR -> initL (IntersperseQUnquoted sR)
            Done b -> return $ Done b

    {-# INLINE collect #-}
    collect nextS sR b = do
        res <- stepR sR b
        case res of
            Partial s ->
                initL (nextS s)
            Done c -> return (Done c)

    {-# INLINE process #-}
    process a sL sR nextState = do
        r <- stepL sL a
        case r of
            Partial s -> return $ Partial (nextState sR s)
            Done b -> collect nextState sR b

    {-# INLINE processQuoted #-}
    processQuoted a sL sR nextState = do
        r <- stepL sL a
        case r of
            Partial s -> return $ Partial (nextState sR s)
            Done _ -> do
                _ <- finalR sR
                error "Collecting fold finished inside quote"

    step (IntersperseQUnquoted sR sL) a
        | a == separator = do
            b <- finalL sL
            collect IntersperseQUnquoted sR b
        | a == quote = processQuoted a sL sR IntersperseQQuoted
        | otherwise = process a sL sR IntersperseQUnquoted

    step (IntersperseQQuoted sR sL) a
        | a == esc = processQuoted a sL sR IntersperseQQuotedEsc
        | a == quote = process a sL sR IntersperseQUnquoted
        | otherwise = processQuoted a sL sR IntersperseQQuoted

    step (IntersperseQQuotedEsc sR sL) a =
        processQuoted a sL sR IntersperseQQuoted

    extract (IntersperseQUnquoted sR _) = extractR sR
    extract (IntersperseQQuoted _ _) =
        error "intersperseWithQuotes: finished inside quote"
    extract (IntersperseQQuotedEsc _ _) =
        error "intersperseWithQuotes: finished inside quote, at escape char"

    final (IntersperseQUnquoted sR sL) = finalL sL *> finalR sR
    final (IntersperseQQuoted sR sL) = do
        _ <- finalR sR
        _ <- finalL sL
        error "intersperseWithQuotes: finished inside quote"
    final (IntersperseQQuotedEsc sR sL) = do
        _ <- finalR sR
        _ <- finalL sL
        error "intersperseWithQuotes: finished inside quote, at escape char"
