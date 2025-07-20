{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Scanl.Combinators
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Internal.Data.Scanl.Combinators
    (
    -- * Scans

    -- ** Accumulators
    -- *** Semigroups and Monoids
      sconcat
    , mconcat
    , foldMap
    , foldMapM

    -- *** Reducers
    , drainMapM
    , the
    , mean
    , rollingHash
    , defaultSalt
    , rollingHashWithSalt
    , rollingHashFirstN
    -- , rollingHashLastN

    -- *** Saturating Reducers
    -- | 'product' terminates if it becomes 0. Other scans can theoretically
    -- saturate on bounded types, and therefore terminate, however, they will
    -- run forever on unbounded types like Integer/Double.
    , sum
    , product

    -- *** Collectors
    -- | Avoid using these scans in scalable or performance critical
    -- applications, they buffer all the input in GC memory which can be
    -- detrimental to performance if the input is large.
    , toListRev
    -- $toListRev
    , toStream
    , toStreamRev
    , topBy
    , top
    , bottomBy
    , bottom

    -- *** Scanners
    -- | Stateful transformation of the elements. Useful in combination with
    -- the 'postscanlMaybe' combinator. For scanners the result of the scan is
    -- usually a transformation of the current element rather than an
    -- aggregation of all elements till now.
 -- , nthLast -- using RingArray array
    , indexingWith
    , indexing
    , indexingRev
    , rollingMap
    , rollingMapM

    -- *** Filters
    -- | Useful in combination with the 'postscanlMaybe' combinator.
    , deleteBy
    , uniqBy
    , uniq
    , repeated
    , findIndices
    , elemIndices

    {-
    -- *** Singleton scans
    -- | Scans that terminate after consuming exactly one input element. All
    -- these can be implemented in terms of the 'maybe' scan.
    , one
    , null -- XXX not very useful and could be problematic, remove it?
    , satisfy
    , maybe
    -}

    -- *** Multi scans
    -- | Terminate after consuming one or more elements.
    , drainN
    {-
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
    -}

    -- ** Trimmers
    -- | Useful in combination with the 'postscanlMaybe' combinator.
    , takingEndByM
    , takingEndBy
    , takingEndByM_
    , takingEndBy_
    , droppingWhileM
    , droppingWhile
    , prune

    -- -- * Running A Scanl
    -- , drive
    -- , breakStream

    -- -- * Building Incrementally
    -- , addStream

    -- * Combinators
    -- ** Utilities
    , with

    -- -- ** Sliding Window
    -- , slide2

    -- ** Scanning Input
    , scanl
    , scanlMany
    -- , runScan
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

    -- -- ** Trimming

    -- By elements
    -- , takeEndBySeq
    -- , takeEndBySeq_
    {-
    , drop
    , dropWhile
    , dropWhileM
    -}

    -- -- ** Serial Append
    -- , tail
    -- , init
    -- , splitAt -- spanN
    -- , splitIn -- sessionN

    -- ** Parallel Distribution
    , tee
    , distribute
    -- , distributeFst
    -- , distributeMin

    -- ** Unzipping
    , unzip
    -- These two can be expressed using lmap/lmapM and unzip
    , unzipWith
    , unzipWithM
    -- , unzipWithFstM
    -- , unzipWithMaxM

    -- ** Partitioning
    , partitionByM
    -- , partitionByFstM
    -- , partitionByMinM
    , partitionBy
    , partition

    -- -- ** Splitting
    -- , chunksBetween
    -- , intersperseWithQuotes

    -- ** Nesting
    , unfoldMany
    -- , concatSequence
    )
where

#include "inline.hs"
#include "ArrayMacros.h"

-- import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (first)
-- import Data.Bits (shiftL, shiftR, (.|.), (.&.))
-- import Data.Either (isLeft, isRight, fromLeft, fromRight)
import Data.Int (Int64)
-- import Data.Proxy (Proxy(..))
-- import Data.Word (Word32)
import Streamly.Internal.Data.Unbox (Unbox(..))
import Streamly.Internal.Data.MutArray.Type (MutArray(..))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.Pipe.Type (Pipe (..))
-- import Streamly.Internal.Data.Scan (Scan (..))
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import qualified Prelude
import qualified Streamly.Internal.Data.MutArray.Type as MA
-- import qualified Streamly.Internal.Data.Array.Type as Array
import qualified Streamly.Internal.Data.Scanl.Window as Scanl
import qualified Streamly.Internal.Data.Pipe.Type as Pipe
-- import qualified Streamly.Internal.Data.RingArray as RingArray
import qualified Streamly.Internal.Data.Stream.Type as StreamD

import Streamly.Internal.Data.Scanl.Type
import Prelude hiding
       ( Foldable(..), filter, drop, dropWhile, take, takeWhile, zipWith
       , map, mapM_, sequence, all, any
       , notElem, head, last, tail
       , reverse, iterate, init, and, or, lookup, (!!)
       , scanl, scanl1, replicate, concatMap, mconcat, unzip
       , span, splitAt, break, mapM, zip, maybe, const)

#include "DocTestDataScanl.hs"

------------------------------------------------------------------------------
-- Running
------------------------------------------------------------------------------

{-
-- | Drive a fold using the supplied 'Stream', reducing the resulting
-- expression strictly at each step.
--
-- Definition:
--
-- >>> drive = flip Stream.toList $ Stream.scanl
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
-- >>> breakStream = flip Stream.toList $ Stream.scanlBreak
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
-- pure (Array.write :: Fold IO Int (Array Int))
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
--     f = Fold.groupsOf 2 (Array.writeN 3) Fold.toStream
-- in pure f
--     >>= Fold.addOne 1
--     >>= Fold.addStream (Stream.enumerateFromTo 2 4)
--     >>= Fold.drive Stream.nil
--     >>= print
-- :}
-- fromList [fromList [1,2],fromList [3,4]]
--
addStream :: Monad m => Stream m a -> Scanl m a b -> m (Scanl m a b)
addStream stream = drive stream . duplicate
-}

------------------------------------------------------------------------------
-- Transformations on fold inputs
------------------------------------------------------------------------------

-- |
-- >>> mapMaybeM f = Scanl.lmapM f . Scanl.catMaybes
--
{-# INLINE mapMaybeM #-}
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Scanl m b r -> Scanl m a r
mapMaybeM f = lmapM f . catMaybes

-- | @mapMaybe f scan@ maps a 'Maybe' returning function @f@ on the input of
-- the scan, filters out 'Nothing' elements, and return the values extracted
-- from 'Just'.
--
-- >>> mapMaybe f = Scanl.lmap f . Scanl.catMaybes
-- >>> mapMaybe f = Scanl.mapMaybeM (return . f)
--
-- >>> f x = if even x then Just x else Nothing
-- >>> scn = Scanl.mapMaybe f Scanl.toList
-- >>> Stream.toList $ Stream.scanl scn (Stream.enumerateFromTo 1 10)
-- [[],[],[2],[2],[2,4],[2,4],[2,4,6],[2,4,6],[2,4,6,8],[2,4,6,8],[2,4,6,8,10]]
--
{-# INLINE mapMaybe #-}
mapMaybe :: Monad m => (a -> Maybe b) -> Scanl m b r -> Scanl m a r
mapMaybe f = lmap f . catMaybes

------------------------------------------------------------------------------
-- Transformations on scan inputs
------------------------------------------------------------------------------

-- XXX rather scanl the input of a pipe? And scanr the output?
-- pipe :: Monad m => Scanl m a b -> Pipe m b c -> Scanl m a c
-- Can we do this too (in the pipe module):
-- pipe :: Monad m => Scanl m a b -> Pipe m b c -> Pipe m a c

-- | Attach a 'Pipe' on the input of a 'Scanl'.
--
-- /Pre-release/
{-# INLINE pipe #-}
pipe :: Monad m => Pipe m a b -> Scanl m b c -> Scanl m a c
pipe (Pipe consume produce pinitial) (Scanl fstep finitial fextract ffinal) =
    Scanl step initial extract final

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
        -- To remove recursion we will need a produce mode in scans which makes
        -- scans similar to pipes except that they do not yield intermediate
        -- values.
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
        -- XXX Need to use a "Done b" in pipes as well to represent the same
        -- behavior as scans.
        go acc Pipe.Stop = Done <$> ffinal acc

    extract (Tuple' _ fs) = fextract fs

    final (Tuple' _ fs) = ffinal fs

{-
{-# INLINE runScanWith #-}
runScanWith :: Monad m => Bool -> Scan m a b -> Fold m b c -> Scanl m a c
runScanWith isMany
    (Scan stepL initialL)
    (Fold stepR initialR extractR finalR) =
    Fold step initial extract final

    where

    step (sL, sR) x = do
        rL <- stepL sL x
        case rL of
            StreamD.Yield b sL1 -> do
                rR <- stepR sR b
                case rR of
                    Partial sR1 -> return $ Partial (sL1, sR1)
                    Done bR -> return (Done bR)
            StreamD.Skip sL1 -> return $ Partial (sL1, sR)
            -- XXX We have dropped the input.
            -- XXX Need same behavior for Stop in Fold so that the driver can
            -- consistently assume it is dropped.
            StreamD.Stop ->
                if isMany
                then return $ Partial (initialL, sR)
                else Done <$> finalR sR

    initial = do
        r <- initialR
        case r of
            Partial sR -> return $ Partial (initialL, sR)
            Done b -> return $ Done b

    extract = extractR . snd

    final = finalR . snd

-- | Scan the input of a 'Fold' to change it in a stateful manner using a
-- 'Scan'. The scan stops as soon as the fold terminates.
--
-- /Pre-release/
{-# INLINE runScan #-}
runScan :: Monad m => Scan m a b -> Fold m b c -> Scanl m a c
runScan = runScanWith False
-}

{-# INLINE scanWith #-}
scanWith :: Monad m => Bool -> Scanl m a b -> Scanl m b c -> Scanl m a c
scanWith isMany
    (Scanl stepL initialL extractL finalL)
    (Scanl stepR initialR extractR finalR) =
    Scanl step initial extract final

    where

    {-# INLINE runStep #-}
    runStep actionL sR = do
        rL <- actionL
        case rL of
            Done bL -> do
                rR <- stepR sR bL
                case rR of
                    Partial sR1 ->
                        if isMany
                        -- XXX recursive call. If initialL returns Done then it
                        -- will not terminate. In that case we should return
                        -- error in the beginning itself. And we should remove
                        -- this recursion, assuming it won't return Done.
                        then runStep initialL sR1
                        else Done <$> finalR sR1
                    Done bR -> return $ Done bR
            Partial sL -> do
                !b <- extractL sL
                rR <- stepR sR b
                case rR of
                    Partial sR1 -> return $ Partial (sL, sR1)
                    Done bR -> finalL sL >> return (Done bR)

    initial = do
        r <- initialR
        case r of
            Partial sR -> runStep initialL sR
            Done b -> return $ Done b

    step (sL, sR) x = runStep (stepL sL x) sR

    extract = extractR . snd

    final (sL, sR) = finalL sL *> finalR sR

-- | Scan the input of a 'Scanl' to change it in a stateful manner using
-- another 'Scanl'. The scan stops as soon as any of the scans terminates.
--
-- This is basically an append operation.
--
-- /Pre-release/
{-# INLINE scanl #-}
scanl :: Monad m => Scanl m a b -> Scanl m b c -> Scanl m a c
scanl = scanWith False

-- XXX This does not fuse beacuse of the recursive step. Need to investigate.

-- | Scan the input of a 'Scanl' to change it in a stateful manner using
-- another 'Scanl'. The scan restarts with a fresh state if it terminates.
--
-- /Pre-release/
{-# INLINE scanlMany #-}
scanlMany :: Monad m => Scanl m a b -> Scanl m b c -> Scanl m a c
scanlMany = scanWith True

------------------------------------------------------------------------------
-- Filters
------------------------------------------------------------------------------

-- | Returns the latest element omitting the first occurrence that satisfies
-- the given equality predicate.
--
-- Example:
--
-- >>> input = Stream.fromList [1,3,3,5]
-- >>> Stream.toList $ Stream.postscanlMaybe (Scanl.deleteBy (==) 3) input
-- [1,3,5]
--
{-# INLINE_NORMAL deleteBy #-}
deleteBy :: Monad m => (a -> a -> Bool) -> a -> Scanl m a (Maybe a)
deleteBy eq x0 = fmap extract $ mkScanl step (Tuple' False Nothing)

    where

    step (Tuple' False _) x =
        if eq x x0
        then Tuple' True Nothing
        else Tuple' False (Just x)
    step (Tuple' True _) x = Tuple' True (Just x)

    extract (Tuple' _ x) = x

{-
-- | Provide a sliding window of length 2 elements.
--
-- See "Streamly.Internal.Data.Scanl.Window".
--
{-# INLINE slide2 #-}
slide2 :: Monad m => Fold m (a, Maybe a) b -> Scanl m a b
slide2 (Fold step1 initial1 extract1 final1) = Fold step initial extract final

    where

    initial =
        first (Tuple' Nothing) <$> initial1

    step (Tuple' prev s) cur =
        first (Tuple' (Just cur)) <$> step1 s (cur, prev)

    extract (Tuple' _ s) = extract1 s

    final (Tuple' _ s) = final1 s
-}

-- XXX Compare this with the implementation in Scanl.Window, preferrably use the
-- latter if performance is good.

-- | Apply a function on every two successive elements of a stream. The first
-- argument of the map function is the previous element and the second argument
-- is the current element. When processing the very first element in the
-- stream, the previous element is 'Nothing'.
--
-- /Pre-release/
--
{-# INLINE rollingMapM #-}
rollingMapM :: Monad m => (Maybe a -> a -> m b) -> Scanl m a b
rollingMapM f = Scanl step initial extract extract

    where

    -- XXX We need just a postscan. We do not need an initial result here.
    -- Or we can supply a default initial result as an argument to rollingMapM.
    initial = return $ Partial (Nothing, error "Empty stream")

    step (prev, _) cur = do
        x <- f prev cur
        return $ Partial (Just cur, x)

    extract = return . snd

-- |
-- >>> rollingMap f = Scanl.rollingMapM (\x y -> return $ f x y)
--
{-# INLINE rollingMap #-}
rollingMap :: Monad m => (Maybe a -> a -> b) -> Scanl m a b
rollingMap f = rollingMapM (\x y -> return $ f x y)

-- | Return the latest unique element using the supplied comparison function.
-- Returns 'Nothing' if the current element is same as the last element
-- otherwise returns 'Just'.
--
-- Example, strip duplicate path separators:
--
-- >>> input = Stream.fromList "//a//b"
-- >>> f x y = x == '/' && y == '/'
-- >>> Stream.toList $ Stream.postscanlMaybe (Scanl.uniqBy f) input
-- "/a/b"
--
-- Space: @O(1)@
--
-- /Pre-release/
--
{-# INLINE uniqBy #-}
uniqBy :: Monad m => (a -> a -> Bool) -> Scanl m a (Maybe a)
uniqBy eq = rollingMap f

    where

    f pre curr =
        case pre of
            Nothing -> Just curr
            Just x -> if x `eq` curr then Nothing else Just curr

-- | See 'uniqBy'.
--
-- Definition:
--
-- >>> uniq = Scanl.uniqBy (==)
--
{-# INLINE uniq #-}
uniq :: (Monad m, Eq a) => Scanl m a (Maybe a)
uniq = uniqBy (==)

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
    (a -> Bool) -> Scanl m a (Maybe a)
prune = error "Not implemented yet!"

-- | Emit only repeated elements, once.
--
-- /Unimplemented/
repeated :: -- (Monad m, Eq a) =>
    Scanl m a (Maybe a)
repeated = error "Not implemented yet!"

------------------------------------------------------------------------------
-- Left scans
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Run Effects
------------------------------------------------------------------------------

-- |
-- Definitions:
--
-- >>> drainMapM f = Scanl.lmapM f Scanl.drain
-- >>> drainMapM f = Scanl.foldMapM (void . f)
--
-- Drain all input after passing it through a monadic function. This is the
-- dual of mapM_ on stream producers.
--
{-# INLINE drainMapM #-}
drainMapM ::  Monad m => (a -> m b) -> Scanl m a ()
drainMapM f = lmapM f drain

-- | Terminates with 'Nothing' as soon as it finds an element different than
-- the previous one, returns 'the' element if the entire input consists of the
-- same element.
--
{-# INLINE the #-}
the :: (Monad m, Eq a) => Scanl m a (Maybe a)
the = mkScant step initial id

    where

    initial = Partial Nothing

    step Nothing x = Partial (Just x)
    step old@(Just x0) x =
            if x0 == x
            then Partial old
            else Done Nothing

------------------------------------------------------------------------------
-- To Summary
------------------------------------------------------------------------------

-- | Determine the sum of all elements of a stream of numbers. Returns additive
-- identity (@0@) when the stream is empty. Note that this is not numerically
-- stable for floating point numbers.
--
-- >>> sum = Scanl.cumulativeScan Scanl.incrSum
--
-- Same as following but numerically stable:
--
-- >>> sum = Scanl.mkScanl (+) 0
-- >>> sum = fmap Data.Monoid.getSum $ Scanl.foldMap Data.Monoid.Sum
--
{-# INLINE sum #-}
sum :: (Monad m, Num a) => Scanl m a a
sum = Scanl.cumulativeScan Scanl.incrSum

-- | Determine the product of all elements of a stream of numbers. Returns
-- multiplicative identity (@1@) when the stream is empty. The scan terminates
-- when it encounters (@0@) in its input.
--
-- Same as the following but terminates on multiplication by @0@:
--
-- >>> product = fmap Data.Monoid.getProduct $ Scanl.foldMap Data.Monoid.Product
--
{-# INLINE product #-}
product :: (Monad m, Num a, Eq a) => Scanl m a a
product =  mkScant step (Partial 1) id

    where

    step x a =
        if a == 0
        then Done 0
        else Partial $ x * a

------------------------------------------------------------------------------
-- To Summary (Statistical)
------------------------------------------------------------------------------

-- | Compute a numerically stable arithmetic mean of all elements in the input
-- stream.
--
{-# INLINE mean #-}
mean :: (Monad m, Fractional a) => Scanl m a a
mean = fmap done $ mkScanl step begin

    where

    begin = Tuple' 0 0

    step (Tuple' x n) y =
        let n1 = n + 1
         in Tuple' (x + (y - x) / n1) n1

    done (Tuple' x _) = x

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
rollingHashWithSalt :: (Monad m, Enum a) => Int64 -> Scanl m a Int64
rollingHashWithSalt = mkScanl step

    where

    k = 2891336453 :: Int64

    step cksum a = cksum * k + fromIntegral (fromEnum a)

-- | A default salt used in the implementation of 'rollingHash'.
{-# INLINE defaultSalt #-}
defaultSalt :: Int64
defaultSalt = -2578643520546668380

-- | Compute an 'Int' sized polynomial rolling hash of a stream.
--
-- >>> rollingHash = Scanl.rollingHashWithSalt Scanl.defaultSalt
--
{-# INLINE rollingHash #-}
rollingHash :: (Monad m, Enum a) => Scanl m a Int64
rollingHash = rollingHashWithSalt defaultSalt

-- | Compute an 'Int' sized polynomial rolling hash of the first n elements of
-- a stream.
--
-- >>> rollingHashFirstN n = Scanl.take n Scanl.rollingHash
--
-- /Pre-release/
{-# INLINE rollingHashFirstN #-}
rollingHashFirstN :: (Monad m, Enum a) => Int -> Scanl m a Int64
rollingHashFirstN n = take n rollingHash

------------------------------------------------------------------------------
-- Monoidal left scans
------------------------------------------------------------------------------

-- | Semigroup concat. Append the elements of an input stream to a provided
-- starting value.
--
-- Definition:
--
-- >>> sconcat = Scanl.mkScanl (<>)
--
-- >>> semigroups = fmap Data.Monoid.Sum $ Stream.enumerateFromTo 1 3
-- >>> Stream.toList $ Stream.scanl (Scanl.sconcat 3) semigroups
-- [Sum {getSum = 3},Sum {getSum = 4},Sum {getSum = 6},Sum {getSum = 9}]
--
{-# INLINE sconcat #-}
sconcat :: (Monad m, Semigroup a) => a -> Scanl m a a
sconcat = mkScanl (<>)

-- | Monoid concat. Scan an input stream consisting of monoidal elements using
-- 'mappend' and 'mempty'.
--
-- Definition:
--
-- >>> mconcat = Scanl.sconcat mempty
--
-- >>> monoids = fmap Data.Monoid.Sum $ Stream.enumerateFromTo 1 3
-- >>> Stream.toList $ Stream.scanl Scanl.mconcat monoids
-- [Sum {getSum = 0},Sum {getSum = 1},Sum {getSum = 3},Sum {getSum = 6}]
--
{-# INLINE mconcat #-}
mconcat ::
    ( Monad m
    , Monoid a) => Scanl m a a
mconcat = sconcat mempty

-- |
-- Definition:
--
-- >>> foldMap f = Scanl.lmap f Scanl.mconcat
--
-- Make a scan from a pure function that scans the output of the function
-- using 'mappend' and 'mempty'.
--
-- >>> sum = Scanl.foldMap Data.Monoid.Sum
-- >>> Stream.toList $ Stream.scanl sum $ Stream.enumerateFromTo 1 3
-- [Sum {getSum = 0},Sum {getSum = 1},Sum {getSum = 3},Sum {getSum = 6}]
--
{-# INLINE foldMap #-}
foldMap :: (Monad m, Monoid b) => (a -> b) -> Scanl m a b
foldMap f = lmap f mconcat

-- |
-- Definition:
--
-- >>> foldMapM f = Scanl.lmapM f Scanl.mconcat
--
-- Make a scan from a monadic function that scans the output of the function
-- using 'mappend' and 'mempty'.
--
-- >>> sum = Scanl.foldMapM (return . Data.Monoid.Sum)
-- >>> Stream.toList $ Stream.scanl sum $ Stream.enumerateFromTo 1 3
-- [Sum {getSum = 0},Sum {getSum = 1},Sum {getSum = 3},Sum {getSum = 6}]
--
{-# INLINE foldMapM #-}
foldMapM ::  (Monad m, Monoid b) => (a -> m b) -> Scanl m a b
foldMapM act = mkScanlM step (pure mempty)

    where

    step m a = do
        m' <- act a
        return $! mappend m m'

------------------------------------------------------------------------------
-- To Containers
------------------------------------------------------------------------------

-- $toListRev
-- This is more efficient than 'Streamly.Internal.Data.Scanl.toList'. toList is
-- exactly the same as reversing the list after 'toListRev'.

-- | Buffers the input stream to a list in the reverse order of the input.
--
-- Definition:
--
-- >>> toListRev = Scanl.mkScanl (flip (:)) []
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Array" instead.
--

--  xn : ... : x2 : x1 : []
{-# INLINE toListRev #-}
toListRev :: Monad m => Scanl m a [a]
toListRev = mkScanl (flip (:)) []

------------------------------------------------------------------------------
-- Partial Scans
------------------------------------------------------------------------------

-- | A scan that drains the first n elements of its input, running the effects
-- and discarding the results.
--
-- Definition:
--
-- >>> drainN n = Scanl.take n Scanl.drain
--
-- /Pre-release/
{-# INLINE drainN #-}
drainN :: Monad m => Int -> Scanl m a ()
drainN n = take n drain

{-
------------------------------------------------------------------------------
-- To Elements
------------------------------------------------------------------------------

-- | Like 'index', except with a more general 'Integral' argument
--
-- /Pre-release/
{-# INLINE genericIndex #-}
genericIndex :: (Integral i, Monad m) => i -> Scanl m a (Maybe a)
genericIndex i = mkScant step (Partial 0) (const Nothing)

    where

    step j a =
        if i == j
        then Done $ Just a
        else Partial (j + 1)

-- | Return the element at the given index.
--
-- Definition:
--
-- >>> index = Scanl.genericIndex
--
{-# INLINE index #-}
index :: Monad m => Int -> Scanl m a (Maybe a)
index = genericIndex

-- | Consume a single input and transform it using the supplied 'Maybe'
-- returning function.
--
-- /Pre-release/
--
{-# INLINE maybe #-}
maybe :: Monad m => (a -> Maybe b) -> Scanl m a (Maybe b)
maybe f = mkScant (const (Done . f)) (Partial Nothing) id

-- | Consume a single element and return it if it passes the predicate else
-- return 'Nothing'.
--
-- Definition:
--
-- >>> satisfy f = Scanl.maybe (\a -> if f a then Just a else Nothing)
--
-- /Pre-release/
{-# INLINE satisfy #-}
satisfy :: Monad m => (a -> Bool) -> Scanl m a (Maybe a)
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
-- >>> one = Scanl.maybe Just
--
-- This is similar to the stream 'Stream.uncons' operation.
--
{-# INLINE one #-}
one :: Monad m => Scanl m a (Maybe a)
one = maybe Just

-- | Returns the first element that satisfies the given predicate.
--
-- /Pre-release/
{-# INLINE findM #-}
findM :: Monad m => (a -> m Bool) -> Scanl m a (Maybe a)
findM predicate =
    Scanl step (return $ Partial ()) extract extract

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
find :: Monad m => (a -> Bool) -> Scanl m a (Maybe a)
find p = findM (return . p)

-- | In a stream of (key-value) pairs @(a, b)@, return the value @b@ of the
-- first pair where the key equals the given value @a@.
--
-- Definition:
--
-- >>> lookup x = fmap snd <$> Scanl.find ((== x) . fst)
--
{-# INLINE lookup #-}
lookup :: (Eq a, Monad m) => a -> Scanl m (a,b) (Maybe b)
lookup a0 = mkScant step (Partial ()) (const Nothing)

    where

    step () (a, b) =
        if a == a0
        then Done $ Just b
        else Partial ()

-- | Returns the first index that satisfies the given predicate.
--
{-# INLINE findIndex #-}
findIndex :: Monad m => (a -> Bool) -> Scanl m a (Maybe Int)
findIndex predicate = mkScant step (Partial 0) (const Nothing)

    where

    step i a =
        if predicate a
        then Done $ Just i
        else Partial (i + 1)
-}

-- | Returns the index of the latest element if the element satisfies the given
-- predicate.
--
{-# INLINE findIndices #-}
findIndices :: Monad m => (a -> Bool) -> Scanl m a (Maybe Int)
findIndices predicate =
    -- XXX implement by combining indexing and filtering scans
    fmap (either (Prelude.const Nothing) Just) $ mkScanl step (Left (-1))

    where

    step i a =
        if predicate a
        then Right (either id id i + 1)
        else Left (either id id i + 1)

-- | Returns the index of the latest element if the element matches the given
-- value.
--
-- Definition:
--
-- >>> elemIndices a = Scanl.findIndices (== a)
--
{-# INLINE elemIndices #-}
elemIndices :: (Monad m, Eq a) => a -> Scanl m a (Maybe Int)
elemIndices a = findIndices (== a)

{-
-- | Returns the first index where a given value is found in the stream.
--
-- Definition:
--
-- >>> elemIndex a = Scanl.findIndex (== a)
--
{-# INLINE elemIndex #-}
elemIndex :: (Eq a, Monad m) => a -> Scanl m a (Maybe Int)
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
-- >>> null = fmap isJust Scanl.one
--
{-# INLINE null #-}
null :: Monad m => Scanl m a Bool
null = mkScant (\() _ -> Done False) (Partial ()) (const True)

-- | Returns 'True' if any element of the input satisfies the predicate.
--
-- Definition:
--
-- >>> any p = Scanl.lmap p Scanl.or
--
-- Example:
--
-- >>> Stream.toList $ Stream.scanl (Scanl.any (== 0)) $ Stream.fromList [1,0,1]
-- True
--
{-# INLINE any #-}
any :: Monad m => (a -> Bool) -> Scanl m a Bool
any predicate = mkScant step initial id

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
-- >>> elem a = Scanl.any (== a)
--
{-# INLINE elem #-}
elem :: (Eq a, Monad m) => a -> Scanl m a Bool
elem a = any (== a)

-- | Returns 'True' if all elements of the input satisfy the predicate.
--
-- Definition:
--
-- >>> all p = Scanl.lmap p Scanl.and
--
-- Example:
--
-- >>> Stream.toList $ Stream.scanl (Scanl.all (== 0)) $ Stream.fromList [1,0,1]
-- False
--
{-# INLINE all #-}
all :: Monad m => (a -> Bool) -> Scanl m a Bool
all predicate = mkScant step initial id

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
-- >>> notElem a = Scanl.all (/= a)
--
{-# INLINE notElem #-}
notElem :: (Eq a, Monad m) => a -> Scanl m a Bool
notElem a = all (/= a)

-- | Returns 'True' if all elements are 'True', 'False' otherwise
--
-- Definition:
--
-- >>> and = Scanl.all (== True)
--
{-# INLINE and #-}
and :: Monad m => Scanl m Bool Bool
and = all id

-- | Returns 'True' if any element is 'True', 'False' otherwise
--
-- Definition:
--
-- >>> or = Scanl.any (== True)
--
{-# INLINE or #-}
or :: Monad m => Scanl m Bool Bool
or = any id
-}

------------------------------------------------------------------------------
-- Grouping/Splitting
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Grouping without looking at elements
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------

{-
-- | @splitAt n f1 f2@ composes folds @f1@ and @f2@ such that first @n@
-- elements of its input are consumed by fold @f1@ and the rest of the stream
-- is consumed by fold @f2@.
--
-- >>> let splitAt_ n xs = Stream.toList $ Stream.scanl (Fold.splitAt n Fold.toList Fold.toList) $ Stream.fromList xs
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
    -> Scanl m a b
    -> Scanl m a c
    -> Scanl m a (b, c)
splitAt n fld = splitWith (,) (take n fld)
-}

------------------------------------------------------------------------------
-- Element Aware APIs
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------

{-# INLINE takingEndByM #-}
takingEndByM :: Monad m => (a -> m Bool) -> Scanl m a (Maybe a)
takingEndByM p = Scanl step initial extract extract

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
-- >>> takingEndBy p = Scanl.takingEndByM (return . p)
--
{-# INLINE takingEndBy #-}
takingEndBy :: Monad m => (a -> Bool) -> Scanl m a (Maybe a)
takingEndBy p = takingEndByM (return . p)

{-# INLINE takingEndByM_ #-}
takingEndByM_ :: Monad m => (a -> m Bool) -> Scanl m a (Maybe a)
takingEndByM_ p = Scanl step initial extract extract

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
-- >>> takingEndBy_ p = Scanl.takingEndByM_ (return . p)
--
{-# INLINE takingEndBy_ #-}
takingEndBy_ :: Monad m => (a -> Bool) -> Scanl m a (Maybe a)
takingEndBy_ p = takingEndByM_ (return . p)

{-# INLINE droppingWhileM #-}
droppingWhileM :: Monad m => (a -> m Bool) -> Scanl m a (Maybe a)
droppingWhileM p = Scanl step initial extract extract

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
-- >>> droppingWhile p = Scanl.droppingWhileM (return . p)
--
{-# INLINE droppingWhile #-}
droppingWhile :: Monad m => (a -> Bool) -> Scanl m a (Maybe a)
droppingWhile p = droppingWhileM (return . p)

------------------------------------------------------------------------------
-- Binary splitting on a separator
------------------------------------------------------------------------------

{-
data SplitOnSeqState acc a rb rh w ck =
      SplitOnSeqEmpty !acc
    | SplitOnSeqSingle !acc !a
    | SplitOnSeqWord !acc !Int !w
    | SplitOnSeqWordLoop !acc !w
    | SplitOnSeqKR !acc !Int !rb !rh
    | SplitOnSeqKRLoop !acc !ck !rb !rh

-- XXX Need to add tests for takeEndBySeq, we have tests for takeEndBySeq_ .

-- | Continue taking the input until the input sequence matches the supplied
-- sequence, taking the supplied sequence as well. If the pattern is empty this
-- acts as an identity fold.
--
-- >>> s = Stream.fromList "hello there. How are you?"
-- >>> f = Fold.takeEndBySeq (Array.fromList "re") Fold.toList
-- >>> Stream.toList $ Stream.scanl f s
-- "hello there"
--
-- >>> Stream.toList $ Stream.scanl Fold.toList $ Stream.toList $ Stream.scanlMany f s
-- ["hello there",". How are"," you?"]
--
-- /Pre-release/
{-# INLINE takeEndBySeq #-}
takeEndBySeq :: forall m a b. (MonadIO m, Unbox a, Enum a, Eq a) =>
       Array.Array a
    -> Scanl m a b
    -> Scanl m a b
takeEndBySeq patArr (Fold fstep finitial fextract ffinal) =
    Fold step initial extract final

    where

    patLen = Array.length patArr

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
                    rb <- liftIO $ RingArray.emptyOf patLen
                    return $ Partial $ SplitOnSeqKR acc 0 rb 0
            Done b -> return $ Done b

    -- Word pattern related
    maxIndex = patLen - 1

    elemBits = SIZE_OF(a) * 8

    wordMask :: Word
    wordMask = (1 `shiftL` (elemBits * patLen)) - 1

    wordPat :: Word
    wordPat = wordMask .&. Array.scanl' addToWord 0 patArr

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
    patHash = Array.scanl' addCksum 0 patArr

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
    step (SplitOnSeqKR s idx rb rh) x = do
        res <- fstep s x
        case res of
            Partial s1 -> do
                rh1 <- liftIO $ RingArray.unsafeInsert rb rh x
                if idx == maxIndex
                then do
                    let fld = RingArray.unsafeFoldRing (RingArray.ringCapacity rb)
                    let !ringHash = fld addCksum 0 rb
                    if ringHash == patHash && RingArray.unsafeEqArray rb rh1 patArr
                    then Done <$> ffinal s1
                    else return $ Partial $ SplitOnSeqKRLoop s1 ringHash rb rh1
                else
                    return $ Partial $ SplitOnSeqKR s1 (idx + 1) rb rh1
            Done b -> return $ Done b
    step (SplitOnSeqKRLoop s cksum rb rh) x = do
        res <- fstep s x
        case res of
            Partial s1 -> do
                (old :: a) <- RingArray.unsafeGetIndex rh rb
                rh1 <- liftIO $ RingArray.unsafeInsert rb rh x
                let ringHash = deltaCksum cksum old x
                if ringHash == patHash && RingArray.unsafeEqArray rb rh1 patArr
                then Done <$> ffinal s1
                else return $ Partial $ SplitOnSeqKRLoop s1 ringHash rb rh1
            Done b -> return $ Done b

    extractFunc fex state =
        let st =
                case state of
                    SplitOnSeqEmpty s -> s
                    SplitOnSeqSingle s _ -> s
                    SplitOnSeqWord s _ _ -> s
                    SplitOnSeqWordLoop s _ -> s
                    SplitOnSeqKR s _ _ _ -> s
                    SplitOnSeqKRLoop s _ _ _ -> s
        in fex st

    extract = extractFunc fextract

    final = extractFunc ffinal

-- | Like 'takeEndBySeq' but discards the matched sequence.
--
-- /Pre-release/
--
{-# INLINE takeEndBySeq_ #-}
takeEndBySeq_ :: forall m a b. (MonadIO m, Unbox a, Enum a, Eq a) =>
       Array.Array a
    -> Scanl m a b
    -> Scanl m a b
takeEndBySeq_ patArr (Fold fstep finitial fextract ffinal) =
    Fold step initial extract final

    where

    patLen = Array.length patArr

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
                    rb <- liftIO $ RingArray.emptyOf patLen
                    return $ Partial $ SplitOnSeqKR acc 0 rb 0
            Done b -> return $ Done b

    -- Word pattern related
    maxIndex = patLen - 1

    elemBits = SIZE_OF(a) * 8

    wordMask :: Word
    wordMask = (1 `shiftL` (elemBits * patLen)) - 1

    elemMask :: Word
    elemMask = (1 `shiftL` elemBits) - 1

    wordPat :: Word
    wordPat = wordMask .&. Array.scanl' addToWord 0 patArr

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
    patHash = Array.scanl' addCksum 0 patArr

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
    step (SplitOnSeqKR s idx rb rh) x = do
        rh1 <- liftIO $ RingArray.unsafeInsert rb rh x
        if idx == maxIndex
        then do
            let fld = RingArray.unsafeFoldRing (RingArray.ringCapacity rb)
            let !ringHash = fld addCksum 0 rb
            if ringHash == patHash && RingArray.unsafeEqArray rb rh1 patArr
            then Done <$> ffinal s
            else return $ Partial $ SplitOnSeqKRLoop s ringHash rb rh1
        else return $ Partial $ SplitOnSeqKR s (idx + 1) rb rh1
    step (SplitOnSeqKRLoop s cksum rb rh) x = do
        old <- RingArray.unsafeGetIndex rh rb
        res <- fstep s old
        case res of
            Partial s1 -> do
                rh1 <- liftIO $ RingArray.unsafeInsert rb rh x
                let ringHash = deltaCksum cksum old x
                if ringHash == patHash && RingArray.unsafeEqArray rb rh1 patArr
                then Done <$> ffinal s1
                else return $ Partial $ SplitOnSeqKRLoop s1 ringHash rb rh1
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

        let consumeRing s n rb rh =
                if n == 0
                then fex s
                else do
                    old <- RingArray.unsafeGetIndex rh rb
                    let rh1 = RingArray.advance rb rh
                    r <- fstep s old
                    case r of
                        Partial s1 -> consumeRing s1 (n - 1) rb rh1
                        Done b -> return b

        case state of
            SplitOnSeqEmpty s -> fex s
            SplitOnSeqSingle s _ -> fex s
            SplitOnSeqWord s idx wrd -> consumeWord s idx wrd
            SplitOnSeqWordLoop s wrd -> consumeWord s patLen wrd
            SplitOnSeqKR s idx rb _ -> consumeRing s idx rb 0
            SplitOnSeqKRLoop s _ rb rh -> consumeRing s patLen rb rh

    extract = extractFunc fextract

    final = extractFunc ffinal
    -}

------------------------------------------------------------------------------
-- Distributing
------------------------------------------------------------------------------
--
-- | Distribute one copy of the stream to each scan and zip the results.
--
-- @
--                 |-------Scanl m a b--------|
-- ---stream m a---|                          |---m (b,c)
--                 |-------Scanl m a c--------|
-- @
--
--  Definition:
--
-- >>> tee = Scanl.teeWith (,)
--
-- Example:
--
-- >>> t = Scanl.tee Scanl.sum Scanl.length
-- >>> Stream.toList $ Stream.scanl t (Stream.enumerateFromTo 1.0 10.0)
-- [(0.0,0),(1.0,1),(3.0,2),(6.0,3),(10.0,4),(15.0,5),(21.0,6),(28.0,7),(36.0,8),(45.0,9),(55.0,10)]
--
{-# INLINE tee #-}
tee :: Monad m => Scanl m a b -> Scanl m a c -> Scanl m a (b,c)
tee = teeWith (,)

-- XXX use unboxed Array for output to scale it to a large number of consumers?

-- | Distribute one copy of the stream to each scan and collect the results in
-- a container.
--
-- @
--
--                 |-------Scanl m a b--------|
-- ---stream m a---|                          |---m [b]
--                 |-------Scanl m a b--------|
--                 |                          |
--                            ...
-- @
--
-- >>> Stream.toList $ Stream.scanl (Scanl.distribute [Scanl.sum, Scanl.length]) (Stream.enumerateFromTo 1 5)
-- [[0,0],[1,1],[3,2],[6,3],[10,4],[15,5]]
--
-- >>> distribute = Prelude.foldr (Scanl.teeWith (:)) (Scanl.const [])
--
-- This is the consumer side dual of the producer side 'sequence' operation.
--
-- Stops as soon as any of the scans stop.
--
{-# INLINE distribute #-}
distribute :: Monad m => [Scanl m a b] -> Scanl m a [b]
distribute = Prelude.foldr (teeWith (:)) (const [])

------------------------------------------------------------------------------
-- Partitioning
------------------------------------------------------------------------------

{-
{-# INLINE partitionByMUsing #-}
partitionByMUsing :: Monad m =>
       (  (x -> y -> (x, y))
       -> Scanl m (Either b c) x
       -> Scanl m (Either b c) y
       -> Scanl m (Either b c) (x, y)
       )
    -> (a -> m (Either b c))
    -> Scanl m b x
    -> Scanl m c y
    -> Scanl m a (x, y)
partitionByMUsing t f fld1 fld2 =
    let l = lmap (fromLeft undefined) fld1  -- :: Fold m (Either b c) x
        r = lmap (fromRight undefined) fld2 -- :: Fold m (Either b c) y
     in lmapM f (t (,) (filter isLeft l) (filter isRight r))
 -}

data PartState sL sR = PartLeft !sL !sR | PartRight !sL !sR

-- | Partition the input over two scans using an 'Either' partitioning
-- predicate.
--
-- @
--
--                                     |-------Scanl b x--------|
-- -----stream m a --> (Either b c)----|                        |----(x,y)
--                                     |-------Scanl c y--------|
-- @
--
-- Example, send input to either scan randomly:
--
-- >>> :set -package random
-- >>> import System.Random (randomIO)
-- >>> randomly a = randomIO >>= \x -> return $ if x then Left a else Right a
-- >>> f = Scanl.partitionByM randomly Scanl.length Scanl.length
-- >>> Stream.toList $ Stream.scanl f (Stream.enumerateFromTo 1 10)
-- ...
--
-- Example, send input to the two scans in a proportion of 2:1:
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
--  let f = Scanl.partitionByM g Scanl.length Scanl.length
--  r <- Stream.toList $ Stream.scanl f (Stream.enumerateFromTo (1 :: Int) 10)
--  print r
-- :}
--
-- >>> main
-- ...
--
--
-- This is the consumer side dual of the producer side 'mergeBy' operation.
--
-- Terminates as soon as any of the scans terminate.
--
-- /Pre-release/
{-# INLINE partitionByM #-}
partitionByM :: Monad m
    => (a -> m (Either b c)) -> Scanl m b x -> Scanl m c x -> Scanl m a x
partitionByM f
    (Scanl stepL initialL extractL finalL)
    (Scanl stepR initialR extractR finalR) =
    Scanl step initial extract final

    where

    initial = do
        resL <- initialL
        resR <- initialR
        return
            $ case resL of
                  Done bl -> Done bl
                  Partial sl ->
                      case resR of
                            Partial sr -> Partial $ PartLeft sl sr
                            Done br -> Done br

    runBoth sL sR a = do
        pRes <- f a
        case pRes of
            Left b -> do
                resL <- stepL sL b
                case resL of
                    Partial s -> return $ Partial $ PartLeft s sR
                    Done x -> return $ Done x
            Right c -> do
                resR <- stepR sR c
                case resR of
                    Partial s -> return $ Partial $ PartRight sL s
                    Done x -> return $ Done x

    step (PartLeft sL sR) = runBoth sL sR
    step (PartRight sL sR) = runBoth sL sR

    extract (PartLeft sL _) = extractL sL
    extract (PartRight _ sR) = extractR sR

    final (PartLeft sL sR) = finalR sR *> finalL sL
    final (PartRight sL sR) = finalL sL *> finalR sR

{-
-- | Similar to 'partitionByM' but terminates when the first fold terminates.
--
{-# INLINE partitionByFstM #-}
partitionByFstM :: Monad m
    => (a -> m (Either b c)) -> Scanl m b x -> Scanl m c y -> Scanl m a (x, y)
partitionByFstM = partitionByMUsing teeWithFst

-- | Similar to 'partitionByM' but terminates when any fold terminates.
--
{-# INLINE partitionByMinM #-}
partitionByMinM :: Monad m =>
    (a -> m (Either b c)) -> Scanl m b x -> Scanl m c y -> Scanl m a (x, y)
partitionByMinM = partitionByMUsing teeWithMin
-}

-- Note: we could use (a -> Bool) instead of (a -> Either b c), but the latter
-- makes the signature clearer as to which case belongs to which scan.
-- XXX need to check the performance in both cases.

-- | Same as 'partitionByM' but with a pure partition function.
--
-- Example, count even and odd numbers in a stream:
--
-- >>> :{
--  let f = Scanl.partitionBy (\n -> if even n then Left n else Right n)
--                      (fmap (("Even " ++) . show) Scanl.length)
--                      (fmap (("Odd "  ++) . show) Scanl.length)
--   in Stream.toList $ Stream.postscanl f (Stream.enumerateFromTo 1 10)
-- :}
-- ["Odd 1","Even 1","Odd 2","Even 2","Odd 3","Even 3","Odd 4","Even 4","Odd 5","Even 5"]
--
-- /Pre-release/
{-# INLINE partitionBy #-}
partitionBy :: Monad m
    => (a -> Either b c) -> Scanl m b x -> Scanl m c x -> Scanl m a x
partitionBy f = partitionByM (return . f)

-- | Compose two scans such that the combined scan accepts a stream of 'Either'
-- and routes the 'Left' values to the first scan and 'Right' values to the
-- second scan.
--
-- Definition:
--
-- >>> partition = Scanl.partitionBy id
--
{-# INLINE partition #-}
partition :: Monad m
    => Scanl m b x -> Scanl m c x -> Scanl m (Either b c) x
partition = partitionBy id

{-
-- | Send one item to each fold in a round-robin fashion. This is the consumer
-- side dual of producer side 'mergeN' operation.
--
-- partitionN :: Monad m => [Scanl m a b] -> Scanl m a [b]
-- partitionN fs = Fold step begin done
-}

------------------------------------------------------------------------------
-- Unzipping
------------------------------------------------------------------------------

{-# INLINE unzipWithMUsing #-}
unzipWithMUsing :: Monad m =>
       (  (x -> y -> (x, y))
       -> Scanl m (b, c) x
       -> Scanl m (b, c) y
       -> Scanl m (b, c) (x, y)
       )
    -> (a -> m (b, c))
    -> Scanl m b x
    -> Scanl m c y
    -> Scanl m a (x, y)
unzipWithMUsing t f fld1 fld2 =
    let f1 = lmap fst fld1  -- :: Scanl m (b, c) b
        f2 = lmap snd fld2  -- :: Scanl m (b, c) c
     in lmapM f (t (,) f1 f2)

-- | Like 'unzipWith' but with a monadic splitter function.
--
-- Definition:
--
-- >>> unzipWithM k f1 f2 = Scanl.lmapM k (Scanl.unzip f1 f2)
--
-- /Pre-release/
{-# INLINE unzipWithM #-}
unzipWithM :: Monad m
    => (a -> m (b,c)) -> Scanl m b x -> Scanl m c y -> Scanl m a (x,y)
unzipWithM = unzipWithMUsing teeWith

{-
-- | Similar to 'unzipWithM' but terminates when the first fold terminates.
--
{-# INLINE unzipWithFstM #-}
unzipWithFstM :: Monad m =>
    (a -> m (b, c)) -> Scanl m b x -> Scanl m c y -> Scanl m a (x, y)
unzipWithFstM = unzipWithMUsing teeWithFst

-- | Similar to 'unzipWithM' but terminates when any fold terminates.
--
{-# INLINE unzipWithMaxM #-}
unzipWithMaxM :: Monad m =>
    (a -> m (b,c)) -> Scanl m b x -> Scanl m c y -> Scanl m a (x,y)
unzipWithMaxM = unzipWithMUsing teeWithMax
-}

-- | Split elements in the input stream into two parts using a pure splitter
-- function, direct each part to a different scan and zip the results.
--
-- Definitions:
--
-- >>> unzipWith f = Scanl.unzipWithM (return . f)
-- >>> unzipWith f fld1 fld2 = Scanl.lmap f (Scanl.unzip fld1 fld2)
--
-- This scan terminates as soon as any of the input scans terminate.
--
-- /Pre-release/
{-# INLINE unzipWith #-}
unzipWith :: Monad m
    => (a -> (b,c)) -> Scanl m b x -> Scanl m c y -> Scanl m a (x,y)
unzipWith f = unzipWithM (return . f)

-- | Send the elements of tuples in a stream of tuples through two different
-- scans.
--
-- @
--
--                           |-------Scanl m a x--------|
-- ---------stream of (a,b)--|                          |----m (x,y)
--                           |-------Scanl m b y--------|
--
-- @
--
-- Definition:
--
-- >>> unzip = Scanl.unzipWith id
--
-- This is the consumer side dual of the producer side 'zip' operation.
--
{-# INLINE unzip #-}
unzip :: Monad m => Scanl m a x -> Scanl m b y -> Scanl m (a,b) (x,y)
unzip = unzipWith id

------------------------------------------------------------------------------
-- Combining streams and scans - Zipping
------------------------------------------------------------------------------

-- XXX These can be implemented using the fold scan, using the stream as a
-- state.
-- XXX Stream Skip state cannot be efficiently handled in folds but can be
-- handled in parsers using the Continue facility. See zipWithM in the Parser
-- module.
--
-- cmpBy, eqBy, isPrefixOf, isSubsequenceOf etc can be implemented using
-- zipStream.

-- | Zip a stream with the input of a scan using the supplied function.
--
-- /Unimplemented/
--
{-# INLINE zipStreamWithM #-}
zipStreamWithM :: -- Monad m =>
    (a -> b -> m c) -> Stream m a -> Scanl m c x -> Scanl m b x
zipStreamWithM = undefined

-- | Zip a stream with the input of a scan.
--
-- >>> zip = Scanl.zipStreamWithM (curry return)
--
-- /Unimplemented/
--
{-# INLINE zipStream #-}
zipStream :: Monad m => Stream m a -> Scanl m (a, b) x -> Scanl m b x
zipStream = zipStreamWithM (curry return)

-- | Pair each element of a scan input with its index, starting from index 0.
--
{-# INLINE indexingWith #-}
indexingWith :: Monad m => Int -> (Int -> Int) -> Scanl m a (Maybe (Int, a))
indexingWith i f = fmap toMaybe $ mkScanl step initial

    where

    initial = Nothing'

    step Nothing' a = Just' (i, a)
    step (Just' (n, _)) a = Just' (f n, a)

-- |
-- >>> indexing = Scanl.indexingWith 0 (+ 1)
--
{-# INLINE indexing #-}
indexing :: Monad m => Scanl m a (Maybe (Int, a))
indexing = indexingWith 0 (+ 1)

-- |
-- >>> indexingRev n = Scanl.indexingWith n (subtract 1)
--
{-# INLINE indexingRev #-}
indexingRev :: Monad m => Int -> Scanl m a (Maybe (Int, a))
indexingRev n = indexingWith n (subtract 1)

-- | Pair each element of a scan input with its index, starting from index 0.
--
-- >>> indexed = Scanl.postscanlMaybe Scanl.indexing
--
{-# INLINE indexed #-}
indexed :: Monad m => Scanl m (Int, a) b -> Scanl m a b
indexed = postscanlMaybe indexing

-- | Change the predicate function of a Scanl from @a -> b@ to accept an
-- additional state input @(s, a) -> b@. Convenient to filter with an
-- addiitonal index or time input.
--
-- >>> filterWithIndex = Scanl.with Scanl.indexed Scanl.filter
--
-- @
-- filterWithAbsTime = with timestamped filter
-- filterWithRelTime = with timeIndexed filter
-- @
--
-- /Pre-release/
{-# INLINE with #-}
with ::
       (Scanl m (s, a) b -> Scanl m a b)
    -> (((s, a) -> c) -> Scanl m (s, a) b -> Scanl m (s, a) b)
    -> (((s, a) -> c) -> Scanl m a b -> Scanl m a b)
with f comb g = f . comb g . lmap snd

-- XXX Implement as a filter
-- sampleFromthen :: Monad m => Int -> Int -> Scanl m a (Maybe a)

-- | @sampleFromthen offset stride@ samples the element at @offset@ index and
-- then every element at strides of @stride@.
--
{-# INLINE sampleFromthen #-}
sampleFromthen :: Monad m => Int -> Int -> Scanl m a b -> Scanl m a b
sampleFromthen offset size =
    with indexed filter (\(i, _) -> (i + offset) `mod` size == 0)

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------

{-
-- | @concatSequence f t@ applies folds from stream @t@ sequentially and
-- collects the results using the fold @f@.
--
-- /Unimplemented/
--
{-# INLINE concatSequence #-}
concatSequence ::
    -- IsStream t =>
    Fold m b c -> t (Scanl m a b) -> Scanl m a c
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
       Int -> Int -> Scanl m a b -> Scanl m b c -> Scanl m a c
chunksBetween _low _high _f1 _f2 = undefined
-}

-- | A scan that buffers its input to a pure stream.
--
-- /Warning!/ working on large streams accumulated as buffers in memory could
-- be very inefficient, consider using "Streamly.Data.Array" instead.
--
-- >>> toStream = fmap Stream.fromList Scanl.toList
--
-- /Pre-release/
{-# INLINE toStream #-}
toStream :: (Monad m, Monad n) => Scanl m a (Stream n a)
toStream = fmap StreamD.fromList toList

-- This is more efficient than 'toStream'. toStream is exactly the same as
-- reversing the stream after toStreamRev.
--
-- | Buffers the input stream to a pure stream in the reverse order of the
-- input.
--
-- >>> toStreamRev = fmap Stream.fromList Scanl.toListRev
--
-- /Warning!/ working on large streams accumulated as buffers in memory could
-- be very inefficient, consider using "Streamly.Data.Array" instead.
--
-- /Pre-release/

--  xn : ... : x2 : x1 : []
{-# INLINE toStreamRev #-}
toStreamRev :: (Monad m, Monad n) => Scanl m a (Stream n a)
toStreamRev = fmap StreamD.fromList toListRev

-- XXX This does not fuse. It contains a recursive step function. We will need
-- a Skip input constructor in the fold type to make it fuse.

-- | Unfold and flatten the input stream of a scan.
--
-- @
-- Stream.scanl (unfoldMany u f) == Stream.scanl f . Stream.unfoldMany u
-- @
--
-- /Pre-release/
{-# INLINE unfoldMany #-}
unfoldMany :: Monad m => Unfold m a b -> Scanl m b c -> Scanl m a c
unfoldMany (Unfold ustep inject) (Scanl fstep initial extract final) =
    Scanl consume initial extract final

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
    -> Scanl m a (MutArray a)
bottomBy cmp n = Scanl step initial extract extract

    where

    initial = do
        arr <- MA.emptyOf' n
        if n <= 0
        then return $ Done arr
        else return $ Partial (arr, 0)

    step (arr, i) x =
        if i < n
        then do
            arr' <- MA.snoc arr x
            MA.bubble cmp arr'
            return $ Partial (arr', i + 1)
        else do
            x1 <- MA.unsafeGetIndex (i - 1) arr
            case x `cmp` x1 of
                LT -> do
                    MA.unsafePutIndex (i - 1) arr x
                    MA.bubble cmp arr
                    return $ Partial (arr, i)
                _ -> return $ Partial (arr, i)

    extract = return . fst

-- | Get the top @n@ elements using the supplied comparison function.
--
-- To get bottom n elements instead:
--
-- >>> bottomBy cmp = Scanl.topBy (flip cmp)
--
-- Example:
--
-- >>> stream = Stream.fromList [2::Int,7,9,3,1,5,6,11,17]
-- >>> Stream.toList (Stream.scanl (Scanl.topBy compare 3) stream) >>= mapM MutArray.toList
-- [[],[17],[17,11],[17,11,9],[17,11,9],[17,11,9],[17,11,9],[17,11,9],[17,11,9],[17,11,9]]
--
-- /Pre-release/
--
{-# INLINE topBy #-}
topBy :: (MonadIO m, Unbox a) =>
       (a -> a -> Ordering)
    -> Int
    -> Scanl m a (MutArray a)
topBy cmp = bottomBy (flip cmp)

-- | Scan the input stream to top n elements.
--
-- Definition:
--
-- >>> top = Scanl.topBy compare
--
-- >>> stream = Stream.fromList [2::Int,7,9,3,1,5,6,11,17]
-- >>> Stream.toList (Stream.scanl (Scanl.top 3) stream) >>= mapM MutArray.toList
-- [[],[17],[17,11],[17,11,9],[17,11,9],[17,11,9],[17,11,9],[17,11,9],[17,11,9],[17,11,9]]
--
-- /Pre-release/
{-# INLINE top #-}
top :: (MonadIO m, Unbox a, Ord a) => Int -> Scanl m a (MutArray a)
top = bottomBy $ flip compare

-- | Scan the input stream to bottom n elements.
--
-- Definition:
--
-- >>> bottom = Scanl.bottomBy compare
--
-- >>> stream = Stream.fromList [2::Int,7,9,3,1,5,6,11,17]
-- >>> Stream.toList (Stream.scanl (Scanl.bottom 3) stream) >>= mapM MutArray.toList
-- [[],[1],[1,2],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]]
--
-- /Pre-release/
{-# INLINE bottom #-}
bottom :: (MonadIO m, Unbox a, Ord a) => Int -> Scanl m a (MutArray a)
bottom = bottomBy compare

{-
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
    a -> a -> a -> Scanl m a b -> Scanl m b c -> Scanl m a c
intersperseWithQuotes
    quote
    esc
    separator
    (Scanl stepL initialL _ finalL)
    (Scanl stepR initialR extractR finalR) = Scanl step initial extract final

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
-}
