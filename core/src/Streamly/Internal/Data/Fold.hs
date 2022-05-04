-- |
-- Module      : Streamly.Internal.Data.Fold
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- See "Streamly.Data.Fold" for an overview and
-- "Streamly.Internal.Data.Fold.Types" for design notes.
--
-- IMPORTANT: keep the signatures consistent with the folds in Streamly.Prelude

module Streamly.Internal.Data.Fold
    (
    -- * Fold Type
      Step (..)
    , Fold (..)

    -- * Constructors
    -- | Which constructor to use?
    --
    -- * @foldl*@: If the fold never terminates i.e. does not use the 'Done'
    -- constructor otherwise use the @mkFold@ variants.
    -- * @*M@: Use the @M@ suffix variants if any of the step, initial, or
    -- extract function is monadic, otherwise use the pure variants.
    -- * @*_@: Use the @_@ suffix variants if the extract function is 'id' or
    -- 'return'.
    --
    , foldl'
    , foldlM'
    , foldl1'
    , foldr
    , foldrM
    , mkFold
    , mkFold_
    , mkFoldM
    , mkFoldM_

    -- * Folds
    -- ** Identity
    , fromPure
    , fromEffect

    -- ** Accumulators
    -- *** Semigroups and Monoids
    , sconcat
    , mconcat
    , foldMap
    , foldMapM

    -- *** Reducers
    , drain
    , drainBy
    , last
    , length
    , genericLength
    , countDistinct
    , countDistinctInt
    , mean
    , variance
    , stdDev
    , rollingHash
    , rollingHashWithSalt
    , rollingHashFirstN
    -- , rollingHashLastN
    , rollingMapM

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

    -- *** Collectors
    -- | Avoid using these folds in scalable or performance critical
    -- applications, they buffer all the input in GC memory which can be
    -- detrimental to performance if the input is large.
    , toList
    , toListRev
    -- $toListRev
    , toStream
    , toStreamRev
    , toMap

    -- ** Terminating Folds
    , drainN
    -- , lastN
    -- , (!!)
    , genericIndex
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
    , all
    , any
    , and
    , or
    -- , the

    -- * Combinators
    -- ** Utilities
    , with

    -- ** Transforming the Monad
    , hoist
    , generally

    -- ** Mapping on output
    , rmapM

    -- ** Mapping on Input
    , transform
    , lmap
    --, lsequence
    , lmapM
    , scan
    , scanMany
    , postscan
    , indexed

    -- ** Zipping Input
    , zipWithM
    , zip

    -- ** Filtering
    , filter
    , filterM
    , foldFilter
    , satisfy
    , sampleFromthen
    -- , ldeleteBy
    -- , luniq

    -- ** Mapping Filters
    , catMaybes
    , mapMaybe
    -- , mapMaybeM

    -- ** Scanning Filters
    , findIndices
    {-
    , elemIndices

    -- ** Insertion
    -- | Insertion adds more elements to the stream.

    , insertBy
    , intersperseM

    -- ** Reordering
    , reverse
    -}

    -- ** Trimming
    , take

    -- By elements
    , takeEndBy
    , takeEndBy_
    -- , takeEndBySeq
    {-
    , drop
    , dropWhile
    , dropWhileM
    -}

    -- ** Serial Append
    , serialWith
    , serial_
    -- , tail
    -- , init
    , splitAt -- spanN
    -- , splitIn -- sessionN

    -- ** Parallel Distribution
    , teeWith
    , tee
    , teeWithFst
    , teeWithMin
    , distribute
    -- , distributeFst
    -- , distributeMin

    -- ** Unzipping
    , unzip
    -- These two can be expressed using lmap/lmapM and unzip
    , unzipWith
    , unzipWithM
    , unzipWithFstM
    , unzipWithMinM

    -- ** Parallel Alternative
    , shortest
    , longest

    -- ** Partitioning
    , partitionByM
    , partitionByFstM
    , partitionByMinM
    , partitionBy
    , partition

    -- ** Demultiplexing
    -- | Direct values in the input stream to different folds using an n-ary
    -- fold selector. 'demux' is a generalization of 'classify' (and
    -- 'partition') where each key of the classifier can use a different fold.
    , demux
    , demuxWith
    , demuxScanWith
    , demuxScanMutWith
    , demuxMutWith

    -- TODO: These can be implemented using the above operations
    -- , demuxWithSel -- Stop when the fold for the specified key stops
    -- , demuxWithMin -- Stop when any of the folds stop
    -- , demuxWithAll -- Stop when all the folds stop (run once)

    -- ** Classifying
    -- | In an input stream of key value pairs fold values for different keys
    -- in individual output buckets using the given fold. 'classify' is a
    -- special case of 'demux' where all the branches of the demultiplexer use
    -- the same fold.
    --
    -- Different types of maps can be used with these combinators via the IsMap
    -- type class. Hashmap performs better when there are more collisions, trie
    -- Map performs better otherwise. Trie has an advantage of sorting the keys
    -- at the same time.  For example if we want to store a dictionary of words
    -- and their meanings then trie Map would be better if we also want to
    -- display them in sorted order.

    , classify
    , classifyWith
    , classifyMutWith
    , classifyScanWith
    -- , classifyWithSel
    -- , classifyWithMin

    -- ** Splitting
    , many
    , chunksOf
    , chunksBetween

    -- ** Nesting
    , unfoldMany
    , concatSequence
    , concatMap

    -- * Running A Fold
    -- | Normally you would run a fold to completion by supplying it a stream,
    -- e.g. using 'Stream.fold'. However, you could also run a fold partially
    -- by using 'duplicate' on it and then running it with a stream.
    -- Alternatively, 'initialize', 'snoc' and 'finish' can be used to run a
    -- fold incrementally, however, that may not be the most efficient way to
    -- run a fold.
    , initialize
    , snoc
    , duplicate
    , finish
    , top
    , bottom

    -- * Deprecated
    , sequence
    , mapM
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (first)
import Data.Either (isLeft, isRight, fromLeft, fromRight)
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Maybe (isJust, fromJust)
import Foreign.Storable (Storable)
import Streamly.Internal.Data.IsMap (IsMap(..))
import Streamly.Internal.Data.Pipe.Type (Pipe (..), PipeState(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))
import Streamly.Internal.Data.Stream.Serial (SerialT(..))

import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Streamly.Internal.Data.IsMap as IsMap
import qualified Prelude
import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as MA
import qualified Streamly.Internal.Data.Pipe.Type as Pipe
import qualified Streamly.Internal.Data.Stream.StreamD.Type as StreamD

import Prelude hiding
       ( filter, foldl1, drop, dropWhile, take, takeWhile, zipWith
       , foldl, foldr, map, mapM_, sequence, all, any, sum, product, elem
       , notElem, maximum, minimum, head, last, tail, length, null
       , reverse, iterate, init, and, or, lookup, (!!)
       , scanl, scanl1, replicate, concatMap, mconcat, foldMap, unzip
       , span, splitAt, break, mapM, zip)
import Streamly.Internal.Data.Fold.Type

-- $setup
-- >>> :m
-- >>> :set -package streamly
-- >>> import Prelude hiding (break, map, span, splitAt)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream (parse, foldMany)
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold.Type as Fold
-- >>> import qualified Streamly.Internal.Data.Parser as Parser
-- >>> import Streamly.Internal.Data.Stream.Serial (SerialT(..))
-- >>> import Data.IORef (newIORef, readIORef, writeIORef)
-- >>> import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as MA

------------------------------------------------------------------------------
-- hoist
------------------------------------------------------------------------------

-- | Change the underlying monad of a fold
--
-- /Pre-release/
hoist :: (forall x. m x -> n x) -> Fold m a b -> Fold n a b
hoist f (Fold step initial extract) =
    Fold (\x a -> f $ step x a) (f initial) (f . extract)

-- | Adapt a pure fold to any monad
--
-- > generally = Fold.hoist (return . runIdentity)
--
-- /Pre-release/
generally :: Monad m => Fold Identity a b -> Fold m a b
generally = hoist (return . runIdentity)

------------------------------------------------------------------------------
-- Transformations on fold inputs
------------------------------------------------------------------------------

-- | Flatten the monadic output of a fold to pure output.
--
-- @since 0.7.0
{-# DEPRECATED sequence "Use \"rmapM id\" instead" #-}
{-# INLINE sequence #-}
sequence :: Monad m => Fold m a (m b) -> Fold m a b
sequence = rmapM id

-- | Map a monadic function on the output of a fold.
--
-- @since 0.7.0
{-# DEPRECATED mapM "Use rmapM instead" #-}
{-# INLINE mapM #-}
mapM :: Monad m => (b -> m c) -> Fold m a b -> Fold m a c
mapM = rmapM

-- | @mapMaybe f fold@ maps a 'Maybe' returning function @f@ on the input of
-- the fold, filters out 'Nothing' elements, and return the values extracted
-- from 'Just'.
--
-- >>> f x = if even x then Just x else Nothing
-- >>> fld = Fold.mapMaybe f Fold.toList
-- >>> Stream.fold fld (Stream.enumerateFromTo 1 10)
-- [2,4,6,8,10]
--
-- @since 0.8.0
{-# INLINE mapMaybe #-}
mapMaybe :: (Monad m) => (a -> Maybe b) -> Fold m b r -> Fold m a r
mapMaybe f = lmap f . filter isJust . lmap fromJust

------------------------------------------------------------------------------
-- Transformations on fold inputs
------------------------------------------------------------------------------

-- rename to lpipe?
--
-- | Apply a transformation on a 'Fold' using a 'Pipe'.
--
-- /Pre-release/
{-# INLINE transform #-}
transform :: Monad m => Pipe m a b -> Fold m b c -> Fold m a c
transform (Pipe pstep1 pstep2 pinitial) (Fold fstep finitial fextract) =
    Fold step initial extract

    where

    initial = first (Tuple' pinitial) <$> finitial

    step (Tuple' ps fs) x = do
        r <- pstep1 ps x
        go fs r

        where

        -- XXX use SPEC?
        go acc (Pipe.Yield b (Consume ps')) = do
            acc' <- fstep acc b
            return
                $ case acc' of
                      Partial s -> Partial $ Tuple' ps' s
                      Done b2 -> Done b2
        go acc (Pipe.Yield b (Produce ps')) = do
            acc' <- fstep acc b
            r <- pstep2 ps'
            case acc' of
                Partial s -> go s r
                Done b2 -> return $ Done b2
        go acc (Pipe.Continue (Consume ps')) =
            return $ Partial $ Tuple' ps' acc
        go acc (Pipe.Continue (Produce ps')) = do
            r <- pstep2 ps'
            go acc r

    extract (Tuple' _ fs) = fextract fs

{-# INLINE scanWith #-}
scanWith :: Monad m => Bool -> Fold m a b -> Fold m b c -> Fold m a c
scanWith isMany (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

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
                        then runStep initialL sR1
                        else Done <$> extractR sR1
                    Done bR -> return $ Done bR
            Partial sL -> do
                !b <- extractL sL
                rR <- stepR sR b
                return
                    $ case rR of
                        Partial sR1 -> Partial (sL, sR1)
                        Done bR -> Done bR

    initial = do
        r <- initialR
        case r of
            Partial sR -> runStep initialL sR
            Done b -> return $ Done b

    step (sL, sR) x = runStep (stepL sL x) sR

    extract = extractR . snd

-- | Scan the input of a 'Fold' to change it in a stateful manner using another
-- 'Fold'. The scan stops as soon as the fold terminates.
--
-- /Pre-release/
{-# INLINE scan #-}
scan :: Monad m => Fold m a b -> Fold m b c -> Fold m a c
scan = scanWith False

-- XXX This does not fuse beacuse of the recursive step. Need to investigate.
--
-- | Scan the input of a 'Fold' to change it in a stateful manner using another
-- 'Fold'. The scan restarts with a fresh state if the fold terminates.
--
-- /Pre-release/
{-# INLINE scanMany #-}
scanMany :: Monad m => Fold m a b -> Fold m b c -> Fold m a c
scanMany = scanWith True

-- | Postscan the input of a 'Fold' to change it in a stateful manner using
-- another 'Fold'.
-- /Pre-release/
{-# INLINE postscan #-}
postscan :: Monad m => Fold m a b -> Fold m b c -> Fold m a c
postscan (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    {-# INLINE runStep #-}
    runStep actionL sR = do
        rL <- actionL
        case rL of
            Done bL -> do
                rR <- stepR sR bL
                case rR of
                    Partial sR1 -> Done <$> extractR sR1
                    Done bR -> return $ Done bR
            Partial sL -> do
                !b <- extractL sL
                rR <- stepR sR b
                return
                    $ case rR of
                        Partial sR1 -> Partial (sL, sR1)
                        Done bR -> Done bR

    initial = do
        r <- initialR
        rL <- initialL
        case r of
            Partial sR ->
                case rL of
                    Done _ -> Done <$> extractR sR
                    Partial sL -> return $ Partial (sL, sR)
            Done b -> return $ Done b

    step (sL, sR) x = runStep (stepL sL x) sR

    extract = extractR . snd

------------------------------------------------------------------------------
-- Filters
------------------------------------------------------------------------------

-- | Convert a predicate into a filtering fold.
--
-- >>> f = Fold.foldFilter (Fold.satisfy (> 5)) Fold.sum
-- >>> Stream.fold f $ Stream.fromList [1..10]
-- 40
--
-- /Pre-release/
{-# INLINE satisfy #-}
satisfy :: Monad m => (a -> Bool) -> Fold m a (Maybe a)
satisfy f = Fold step (return $ Partial ()) (const (return Nothing))

    where

    step () a = return $ Done $ if f a then Just a else Nothing

-- | Use a 'Maybe' returning fold as a filtering scan.
--
-- >>> f = Fold.foldFilter (Fold.satisfy (> 5)) Fold.sum
-- >>> Stream.fold f $ Stream.fromList [1..10]
-- 40
--
-- The above snippet is equivalent to:
--
-- >>> f = Fold.filter (> 5) Fold.sum
-- >>> Stream.fold f $ Stream.fromList [1..10]
-- 40
--
-- /Pre-release/
{-# INLINE foldFilter #-}
foldFilter :: Monad m => Fold m a (Maybe b) -> Fold m b c -> Fold m a c
foldFilter f1 f2 = many f1 (catMaybes f2)

------------------------------------------------------------------------------
-- Left folds
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Run Effects
------------------------------------------------------------------------------

-- |
-- > drainBy f = lmapM f drain
-- > drainBy = Fold.foldMapM (void . f)
--
-- Drain all input after passing it through a monadic function. This is the
-- dual of mapM_ on stream producers.
--
-- See also: 'Streamly.Prelude.mapM_'
--
-- @since 0.7.0
{-# INLINE drainBy #-}
drainBy ::  Monad m => (a -> m b) -> Fold m a ()
drainBy f = lmapM f drain

-- | Extract the last element of the input stream, if any.
--
-- > last = fmap getLast $ Fold.foldMap (Last . Just)
--
-- @since 0.7.0
{-# INLINE last #-}
last :: Monad m => Fold m a (Maybe a)
last = foldl1' (\_ x -> x)

------------------------------------------------------------------------------
-- To Summary
------------------------------------------------------------------------------

-- | Like 'length', except with a more general 'Num' return value
--
-- > genericLength = fmap getSum $ foldMap (Sum . const  1)
--
-- /Pre-release/
{-# INLINE genericLength #-}
genericLength :: (Monad m, Num b) => Fold m a b
genericLength = foldl' (\n _ -> n + 1) 0

-- | Determine the length of the input stream.
--
-- > length = fmap getSum $ Fold.foldMap (Sum . const  1)
--
-- @since 0.7.0
{-# INLINE length #-}
length :: Monad m => Fold m a Int
length = genericLength

-- XXX Try Hash set
-- XXX Add a countDistinct window fold
-- XXX Add a bloom filter fold

-- | Count non-duplicate elements in the stream.
--
-- Equivalent to using 'nub' followed by 'length' on a stream.
--
-- The memory used is proportional to the number of distinct elements in the
-- stream, to guard against using too much memory use it as a scan and
-- terminate if the count reaches more than a threshold.
--
-- /Space/: \(\mathcal{O}(n)\)
--
-- /Pre-release/
--
{-# INLINE countDistinct #-}
countDistinct :: (Monad m, Ord a) => Fold m a Int
countDistinct = fmap (\(Tuple' _ n) -> n) $ foldl' step initial

    where

    initial = Tuple' Set.empty 0

    step (Tuple' set n) x = do
        if Set.member x set
        then
            Tuple' set n
        else
            let cnt = n + 1
             in Tuple' (Set.insert x set) cnt

-- | Like 'countDistinct' but specialized to a stream of 'Int', for better
-- performance.
--
-- /Pre-release/
{-# INLINE countDistinctInt #-}
countDistinctInt :: Monad m => Fold m Int Int
countDistinctInt = fmap (\(Tuple' _ n) -> n) $ foldl' step initial

    where

    initial = Tuple' IntSet.empty 0

    step (Tuple' set n) x = do
        if IntSet.member x set
        then
            Tuple' set n
        else
            let cnt = n + 1
             in Tuple' (IntSet.insert x set) cnt

-- | Determine the sum of all elements of a stream of numbers. Returns additive
-- identity (@0@) when the stream is empty. Note that this is not numerically
-- stable for floating point numbers.
--
-- > sum = fmap getSum $ Fold.foldMap Sum
--
-- @since 0.7.0
{-# INLINE sum #-}
sum :: (Monad m, Num a) => Fold m a a
sum =  foldl' (+) 0

-- | Determine the product of all elements of a stream of numbers. Returns
-- multiplicative identity (@1@) when the stream is empty. The fold terminates
-- when it encounters (@0@) in its input.
--
-- Compare with @Fold.foldMap Product@.
--
-- @since 0.7.0
-- /Since 0.8.0 (Added 'Eq' constraint)/
{-# INLINE product #-}
product :: (Monad m, Num a, Eq a) => Fold m a a
product =  mkFold_ step (Partial 1)

    where

    step x a =
        if a == 0
        then Done 0
        else Partial $ x * a

------------------------------------------------------------------------------
-- To Summary (Maybe)
------------------------------------------------------------------------------

-- | Determine the maximum element in a stream using the supplied comparison
-- function.
--
-- @since 0.7.0
{-# INLINE maximumBy #-}
maximumBy :: Monad m => (a -> a -> Ordering) -> Fold m a (Maybe a)
maximumBy cmp = foldl1' max'

    where

    max' x y =
        case cmp x y of
            GT -> x
            _ -> y

-- |
-- @
-- maximum = Fold.maximumBy compare
-- @
--
-- Determine the maximum element in a stream.
--
-- Compare with @Fold.foldMap Max@.
--
-- @since 0.7.0
{-# INLINE maximum #-}
maximum :: (Monad m, Ord a) => Fold m a (Maybe a)
maximum = foldl1' max

-- | Computes the minimum element with respect to the given comparison function
--
-- @since 0.7.0
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
-- @
-- minimum = 'minimumBy' compare
-- @
--
-- Compare with @Fold.foldMap Min@.
--
-- @since 0.7.0
{-# INLINE minimum #-}
minimum :: (Monad m, Ord a) => Fold m a (Maybe a)
minimum = foldl1' min

------------------------------------------------------------------------------
-- To Summary (Statistical)
------------------------------------------------------------------------------

-- | Compute a numerically stable arithmetic mean of all elements in the input
-- stream.
--
-- @since 0.7.0
{-# INLINE mean #-}
mean :: (Monad m, Fractional a) => Fold m a a
mean = fmap done $ foldl' step begin

    where

    begin = Tuple' 0 0

    step (Tuple' x n) y =
        let n1 = n + 1
         in Tuple' (x + (y - x) / n1) n1

    done (Tuple' x _) = x

-- | Compute a numerically stable (population) variance over all elements in
-- the input stream.
--
-- @since 0.7.0
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
-- @since 0.7.0
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
-- @since 0.8.0
{-# INLINE rollingHashWithSalt #-}
rollingHashWithSalt :: (Monad m, Enum a) => Int64 -> Fold m a Int64
rollingHashWithSalt = foldl' step

    where

    k = 2891336453 :: Int64

    step cksum a = cksum * k + fromIntegral (fromEnum a)

-- | A default salt used in the implementation of 'rollingHash'.
{-# INLINE defaultSalt #-}
defaultSalt :: Int64
defaultSalt = -2578643520546668380

-- | Compute an 'Int' sized polynomial rolling hash of a stream.
--
-- > rollingHash = Fold.rollingHashWithSalt defaultSalt
--
-- @since 0.8.0
{-# INLINE rollingHash #-}
rollingHash :: (Monad m, Enum a) => Fold m a Int64
rollingHash = rollingHashWithSalt defaultSalt

-- | Compute an 'Int' sized polynomial rolling hash of the first n elements of
-- a stream.
--
-- > rollingHashFirstN = Fold.take n Fold.rollingHash
--
-- /Pre-release/
{-# INLINE rollingHashFirstN #-}
rollingHashFirstN :: (Monad m, Enum a) => Int -> Fold m a Int64
rollingHashFirstN n = take n rollingHash

-- | Apply a function on every two successive elements of a stream. The first
-- argument of the map function is the previous element and the second argument
-- is the current element. When processing the very first element in the
-- stream, the previous element is 'Nothing'.
--
-- /Pre-release/
--
{-# INLINE rollingMapM #-}
rollingMapM :: Monad m => (Maybe a -> a -> m b) -> Fold m a b
rollingMapM f = Fold step initial extract

    where

    -- XXX We need just a postscan. We do not need an initial result here.
    -- Or we can supply a default initial result as an argument to rollingMapM.
    initial = return $ Partial (Nothing, error "Empty stream")

    step (prev, _) cur = do
        x <- f prev cur
        return $ Partial (Just cur, x)

    extract = return . snd

------------------------------------------------------------------------------
-- Monoidal left folds
------------------------------------------------------------------------------

-- | Append the elements of an input stream to a provided starting value.
--
-- >>> Stream.fold (Fold.sconcat 10) (Stream.map Data.Monoid.Sum $ Stream.enumerateFromTo 1 10)
-- Sum {getSum = 65}
--
-- @
-- sconcat = Fold.foldl' (<>)
-- @
--
-- @since 0.8.0
{-# INLINE sconcat #-}
sconcat :: (Monad m, Semigroup a) => a -> Fold m a a
sconcat = foldl' (<>)

-- | Fold an input stream consisting of monoidal elements using 'mappend'
-- and 'mempty'.
--
-- >>> Stream.fold Fold.mconcat (Stream.map Data.Monoid.Sum $ Stream.enumerateFromTo 1 10)
-- Sum {getSum = 55}
--
-- > mconcat = Fold.sconcat mempty
--
-- @since 0.7.0
{-# INLINE mconcat #-}
mconcat ::
    ( Monad m
    , Monoid a) => Fold m a a
mconcat = sconcat mempty

-- |
-- > foldMap f = Fold.lmap f Fold.mconcat
--
-- Make a fold from a pure function that folds the output of the function
-- using 'mappend' and 'mempty'.
--
-- >>> Stream.fold (Fold.foldMap Data.Monoid.Sum) $ Stream.enumerateFromTo 1 10
-- Sum {getSum = 55}
--
-- @since 0.7.0
{-# INLINE foldMap #-}
foldMap :: (Monad m, Monoid b
    ) => (a -> b) -> Fold m a b
foldMap f = lmap f mconcat

-- |
-- > foldMapM f = Fold.lmapM f Fold.mconcat
--
-- Make a fold from a monadic function that folds the output of the function
-- using 'mappend' and 'mempty'.
--
-- >>> Stream.fold (Fold.foldMapM (return . Data.Monoid.Sum)) $ Stream.enumerateFromTo 1 10
-- Sum {getSum = 55}
--
-- @since 0.7.0
{-# INLINE foldMapM #-}
foldMapM ::  (Monad m, Monoid b) => (a -> m b) -> Fold m a b
foldMapM act = foldlM' step (pure mempty)

    where

    step m a = do
        m' <- act a
        return $! mappend m m'

------------------------------------------------------------------------------
-- To Containers
------------------------------------------------------------------------------

-- $toListRev
-- This is more efficient than 'Streamly.Internal.Data.Fold.toList'. toList is
-- exactly the same as reversing the list after 'toListRev'.

-- | Buffers the input stream to a list in the reverse order of the input.
--
-- > toListRev = Fold.foldl' (flip (:)) []
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Array" instead.
--
-- @since 0.8.0

--  xn : ... : x2 : x1 : []
{-# INLINE toListRev #-}
toListRev :: Monad m => Fold m a [a]
toListRev = foldl' (flip (:)) []

------------------------------------------------------------------------------
-- Partial Folds
------------------------------------------------------------------------------

-- | A fold that drains the first n elements of its input, running the effects
-- and discarding the results.
--
-- > drainN n = Fold.take n Fold.drain
--
-- /Pre-release/
{-# INLINE drainN #-}
drainN :: Monad m => Int -> Fold m a ()
drainN n = take n drain

------------------------------------------------------------------------------
-- To Elements
------------------------------------------------------------------------------

-- | Like 'index', except with a more general 'Integral' argument
--
-- /Pre-release/
{-# INLINE genericIndex #-}
genericIndex :: (Integral i, Monad m) => i -> Fold m a (Maybe a)
genericIndex i = mkFold step (Partial 0) (const Nothing)

    where

    step j a =
        if i == j
        then Done $ Just a
        else Partial (j + 1)

-- | Lookup the element at the given index.
--
-- See also: 'Streamly.Prelude.!!'
--
-- @since 0.7.0
{-# INLINE index #-}
index :: Monad m => Int -> Fold m a (Maybe a)
index = genericIndex

-- | Extract the first element of the stream, if any.
--
-- @since 0.7.0
{-# INLINE head #-}
head :: Monad m => Fold m a (Maybe a)
head = mkFold_ (const (Done . Just)) (Partial Nothing)

-- | Returns the first element that satisfies the given predicate.
--
-- @since 0.7.0
{-# INLINE find #-}
find :: Monad m => (a -> Bool) -> Fold m a (Maybe a)
find predicate = mkFold step (Partial ()) (const Nothing)

    where

    step () a =
        if predicate a
        then Done (Just a)
        else Partial ()

-- | In a stream of (key-value) pairs @(a, b)@, return the value @b@ of the
-- first pair where the key equals the given value @a@.
--
-- > lookup = snd <$> Fold.find ((==) . fst)
--
-- @since 0.7.0
{-# INLINE lookup #-}
lookup :: (Eq a, Monad m) => a -> Fold m (a,b) (Maybe b)
lookup a0 = mkFold step (Partial ()) (const Nothing)

    where

    step () (a, b) =
        if a == a0
        then Done $ Just b
        else Partial ()

-- | Returns the first index that satisfies the given predicate.
--
-- @since 0.7.0
{-# INLINE findIndex #-}
findIndex :: Monad m => (a -> Bool) -> Fold m a (Maybe Int)
findIndex predicate = mkFold step (Partial 0) (const Nothing)

    where

    step i a =
        if predicate a
        then Done $ Just i
        else Partial (i + 1)

-- XXX If we have a Continue contructor we won't need a Maybe type for the
-- scan. The fold will have the ability to skip producing output.
--
-- | Returns all indices that satisfy the given predicate.
--
{-# INLINE findIndices #-}
findIndices :: Monad m => (a -> Bool) -> Fold m a (Maybe Int)
findIndices predicate =
    fmap (either (const Nothing) Just) $ foldl' step (Left (-1))

    where

    step i a =
        if predicate a
        then Right (either id id i + 1)
        else Left (either id id i + 1)

-- | Returns the first index where a given value is found in the stream.
--
-- > elemIndex a = Fold.findIndex (== a)
--
-- @since 0.7.0
{-# INLINE elemIndex #-}
elemIndex :: (Eq a, Monad m) => a -> Fold m a (Maybe Int)
elemIndex a = findIndex (a ==)

------------------------------------------------------------------------------
-- To Boolean
------------------------------------------------------------------------------

-- | Return 'True' if the input stream is empty.
--
-- > null = fmap isJust Fold.head
--
-- @since 0.7.0
{-# INLINE null #-}
null :: Monad m => Fold m a Bool
null = mkFold (\() _ -> Done False) (Partial ()) (const True)

-- | Returns 'True' if any of the elements of a stream satisfies a predicate.
--
-- >>> Stream.fold (Fold.any (== 0)) $ Stream.fromList [1,0,1]
-- True
--
-- > any p = Fold.lmap p Fold.or
--
-- @since 0.7.0
{-# INLINE any #-}
any :: Monad m => (a -> Bool) -> Fold m a Bool
any predicate = mkFold_ step initial

    where

    initial = Partial False

    step _ a =
        if predicate a
        then Done True
        else Partial False

-- | Return 'True' if the given element is present in the stream.
--
-- > elem a = Fold.any (== a)
--
-- @since 0.7.0
{-# INLINE elem #-}
elem :: (Eq a, Monad m) => a -> Fold m a Bool
elem a = any (a ==)

-- | Returns 'True' if all elements of a stream satisfy a predicate.
--
-- >>> Stream.fold (Fold.all (== 0)) $ Stream.fromList [1,0,1]
-- False
--
-- > all p = Fold.lmap p Fold.and
--
-- @since 0.7.0
{-# INLINE all #-}
all :: Monad m => (a -> Bool) -> Fold m a Bool
all predicate = mkFold_ step initial

    where

    initial = Partial True

    step _ a =
        if predicate a
        then Partial True
        else Done False

-- | Returns 'True' if the given element is not present in the stream.
--
-- > notElem a = Fold.all (/= a)
--
-- @since 0.7.0
{-# INLINE notElem #-}
notElem :: (Eq a, Monad m) => a -> Fold m a Bool
notElem a = all (a /=)

-- | Returns 'True' if all elements are 'True', 'False' otherwise
--
-- > and = Fold.all (== True)
--
-- @since 0.7.0
{-# INLINE and #-}
and :: Monad m => Fold m Bool Bool
and = all (== True)

-- | Returns 'True' if any element is 'True', 'False' otherwise
--
-- > or = Fold.any (== True)
--
-- @since 0.7.0
{-# INLINE or #-}
or :: Monad m => Fold m Bool Bool
or = any (== True)

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
-- > splitAt n f1 f2 = Fold.serialWith (,) (Fold.take n f1) f2
--
-- /Internal/

{-# INLINE splitAt #-}
splitAt
    :: Monad m
    => Int
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
splitAt n fld = serialWith (,) (take n fld)

------------------------------------------------------------------------------
-- Element Aware APIs
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------

-- Note: Keep this consistent with S.splitOn. In fact we should eliminate
-- S.splitOn in favor of the fold.
--
-- XXX Use Fold.many instead once it is fixed.
--
-- | Like 'takeEndBy' but drops the element on which the predicate succeeds.
--
-- >>> Stream.fold (Fold.takeEndBy_ (== '\n') Fold.toList) $ Stream.fromList "hello\nthere\n"
-- "hello"
--
-- >>> Stream.toList $ Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList) $ Stream.fromList "hello\nthere\n"
-- ["hello","there"]
--
-- > Stream.splitOnSuffix p f = Stream.foldMany (Fold.takeEndBy_ p f)
--
-- See 'Streamly.Prelude.splitOnSuffix' for more details on splitting a
-- stream using 'takeEndBy_'.
--
-- @since 0.8.0
{-# INLINE takeEndBy_ #-}
takeEndBy_ :: Monad m => (a -> Bool) -> Fold m a b -> Fold m a b
takeEndBy_ predicate (Fold fstep finitial fextract) =
    Fold step finitial fextract

    where

    step s a =
        if not (predicate a)
        then fstep s a
        else Done <$> fextract s

-- | Take the input, stop when the predicate succeeds taking the succeeding
-- element as well.
--
-- >>> Stream.fold (Fold.takeEndBy (== '\n') Fold.toList) $ Stream.fromList "hello\nthere\n"
-- "hello\n"
--
-- >>> Stream.toList $ Stream.foldMany (Fold.takeEndBy (== '\n') Fold.toList) $ Stream.fromList "hello\nthere\n"
-- ["hello\n","there\n"]
--
-- > Stream.splitWithSuffix p f = Stream.foldMany (Fold.takeEndBy p f)
--
-- See 'Streamly.Prelude.splitWithSuffix' for more details on splitting a
-- stream using 'takeEndBy'.
--
-- @since 0.8.0
{-# INLINE takeEndBy #-}
takeEndBy :: Monad m => (a -> Bool) -> Fold m a b -> Fold m a b
takeEndBy predicate (Fold fstep finitial fextract) =
    Fold step finitial fextract

    where

    step s a = do
        res <- fstep s a
        if not (predicate a)
        then return res
        else do
            case res of
                Partial s1 -> Done <$> fextract s1
                Done b -> return $ Done b

------------------------------------------------------------------------------
-- Binary splitting on a separator
------------------------------------------------------------------------------

{-
-- | Find the first occurrence of the specified sequence in the input stream
-- and break the input stream into two parts, the first part consisting of the
-- stream before the sequence and the second part consisting of the sequence
-- and the rest of the stream.
--
-- > let breakOn_ pat xs = S.fold (S.breakOn pat FL.toList FL.toList) $ S.fromList xs
--
-- >>> breakOn_ "dear" "Hello dear world!"
-- > ("Hello ","dear world!")
--
{-# INLINE breakOn #-}
breakOn :: Monad m => Array a -> Fold m a b -> Fold m a c -> Fold m a (b,c)
breakOn pat f m = undefined
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
-- >>> Stream.fold (Fold.tee Fold.sum Fold.length) (Stream.enumerateFromTo 1.0 100.0)
-- (5050.0,100)
--
-- > tee = teeWith (,)
--
-- @since 0.7.0
{-# INLINE tee #-}
tee :: Monad m => Fold m a b -> Fold m a c -> Fold m a (b,c)
tee = teeWith (,)

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
-- >>> Stream.fold (Fold.distribute [Fold.sum, Fold.length]) (Stream.enumerateFromTo 1 5)
-- [15,5]
--
-- >>> distribute = Prelude.foldr (Fold.teeWith (:)) (Fold.fromPure [])
--
-- This is the consumer side dual of the producer side 'sequence' operation.
--
-- Stops when all the folds stop.
--
-- @since 0.7.0
{-# INLINE distribute #-}
distribute :: Monad m => [Fold m a b] -> Fold m a [b]
distribute = Prelude.foldr (teeWith (:)) (fromPure [])

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
-- Send input to either fold randomly:
--
-- @
-- > import System.Random (randomIO)
-- > randomly a = randomIO >>= \\x -> return $ if x then Left a else Right a
-- > Stream.fold (Fold.partitionByM randomly Fold.length Fold.length) (Stream.enumerateFromTo 1 100)
-- (59,41)
-- @
--
-- Send input to the two folds in a proportion of 2:1:
--
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
--  f <- proportionately 2 1
--  r <- Stream.fold (Fold.partitionByM f Fold.length Fold.length) (Stream.enumerateFromTo (1 :: Int) 100)
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
--
{-# INLINE partitionByFstM #-}
partitionByFstM :: Monad m
    => (a -> m (Either b c)) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionByFstM = partitionByMUsing teeWithFst

-- | Similar to 'partitionByM' but terminates when any fold terminates.
--
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
-- Count even and odd numbers in a stream:
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

------------------------------------------------------------------------------
-- demux: in a key value stream fold each key sub-stream with a different fold
------------------------------------------------------------------------------

-- TODO Demultiplex an input element into a number of typed variants. We want
-- to statically restrict the target values within a set of predefined types,
-- an enumeration of a GADT.
--
-- This is the consumer side dual of the producer side 'mux' operation (XXX to
-- be implemented).
--
-- XXX If we use Refold in it, it can perhaps fuse/be more efficient. For
-- example we can store just the result rather than storing the whole fold in
-- the Map.
--
-- Note: There are separate functions to determine Key and Fold from the input
-- because key is to be determined on each input whereas fold is to be
-- determined only once for a key.

-- | In a key value stream, fold values corresponding to each key with a key
-- specific fold. The fold returns the fold result as the second component of
-- the output tuple whenever a fold terminates. The first component of the
-- tuple is a container of in-progress folds. If a fold terminates, another
-- instance of the fold is started upon receiving an input with that key.
--
-- This can be used to scan a stream and collect the results from the scan
-- output.
--
-- /Pre-release/
--
{-# INLINE demuxScanWith #-}
demuxScanWith :: (Monad m, IsMap f, Traversable f) =>
       (a -> Key f)
    -> (a -> m (Fold m a b))
    -> Fold m a (m (f b), Maybe (Key f, b))
demuxScanWith getKey getFold = fmap extract $ foldlM' step initial

    where

    initial = return $ Tuple' IsMap.mapEmpty Nothing

    {-# INLINE runFold #-}
    runFold kv (Fold step1 initial1 extract1) (k, a) = do
         res <- initial1
         case res of
            Partial s -> do
                res1 <- step1 s a
                return
                    $ case res1 of
                        Partial _ ->
                            let fld = Fold step1 (return res1) extract1
                             in Tuple' (IsMap.mapInsert k fld kv) Nothing
                        Done b -> Tuple' (IsMap.mapDelete k kv) (Just (k, b))
            Done b -> return $ Tuple' kv (Just (k, b))

    step (Tuple' kv _) a = do
        let k = getKey a
        case IsMap.mapLookup k kv of
            Nothing -> do
                fld <- getFold a
                runFold kv fld (k, a)
            Just f -> runFold kv f (k, a)

    extract (Tuple' kv x) = (Prelude.mapM f kv, x)

        where

        f (Fold _ i e) = do
            r <- i
            case r of
                Partial s -> e s
                Done b -> return b

-- | This is specialized version of 'demuxScanWith' that uses mutable cells as
-- fold accumulators for better performance.
--
-- >>> demuxScanMutWith = Fold.demuxScanWith
--
{-# INLINE demuxScanMutWith #-}
demuxScanMutWith :: (MonadIO m, IsMap f, Traversable f) =>
       (a -> Key f)
    -> (a -> m (Fold m a b))
    -> Fold m a (m (f b), Maybe (Key f, b))
demuxScanMutWith getKey getFold = fmap extract $ foldlM' step initial

    where

    initial = return $ Tuple' IsMap.mapEmpty Nothing

    {-# INLINE initFold #-}
    initFold kv (Fold step1 initial1 extract1) (k, a) = do
         res <- initial1
         case res of
            Partial s -> do
                res1 <- step1 s a
                case res1 of
                    Partial _ -> do
                        let fld = Fold step1 (return res1) extract1
                        ref <- liftIO $ newIORef fld
                        return $ Tuple' (IsMap.mapInsert k ref kv) Nothing
                    Done b -> return $ Tuple' kv (Just (k, b))
            Done b -> return $ Tuple' kv (Just (k, b))

    {-# INLINE runFold #-}
    runFold kv ref (Fold step1 initial1 extract1) (k, a) = do
         res <- initial1
         case res of
            Partial s -> do
                res1 <- step1 s a
                case res1 of
                        Partial _ -> do
                            let fld = Fold step1 (return res1) extract1
                            liftIO $ writeIORef ref fld
                            return $ Tuple' kv Nothing
                        Done b ->
                            let kv1 = IsMap.mapDelete k kv
                             in return $ Tuple' kv1 (Just (k, b))
            Done _ -> error "demuxScanMutWith: unreachable"

    step (Tuple' kv _) a = do
        let k = getKey a
        case IsMap.mapLookup k kv of
            Nothing -> do
                f <- getFold a
                initFold kv f (k, a)
            Just ref -> do
                f <- liftIO $ readIORef ref
                runFold kv ref f (k, a)

    extract (Tuple' kv x) = (Prelude.mapM f kv, x)

        where

        f ref = do
            (Fold _ i e) <- liftIO $ readIORef ref
            r <- i
            case r of
                Partial s -> e s
                Done b -> return b

-- | This collects all the results of 'demuxScanWith' in a container.
--
{-# INLINE demuxWith #-}
demuxWith :: (Monad m, IsMap f, Traversable f) =>
    (a -> Key f) -> (a -> m (Fold m a b)) -> Fold m a (f b)
demuxWith getKey getFold =
    let
        classifier = demuxScanWith getKey getFold
        getMap Nothing = pure IsMap.mapEmpty
        getMap (Just action) = action
        aggregator =
            teeWith IsMap.mapUnion
                (rmapM getMap $ lmap fst last)
                (lmap snd $ catMaybes toMap)
    in postscan classifier aggregator

-- | Same as 'demuxWith' but uses 'demuxScanMutWith' for better performance.
--
-- >>> demuxMutWith = Fold.demuxWith
--
{-# INLINE demuxMutWith #-}
demuxMutWith :: (MonadIO m, IsMap f, Traversable f) =>
    (a -> Key f) -> (a -> m (Fold m a b)) -> Fold m a (f b)
demuxMutWith getKey getFold =
    let
        classifier = demuxScanMutWith getKey getFold
        getMap Nothing = pure IsMap.mapEmpty
        getMap (Just action) = action
        aggregator =
            teeWith IsMap.mapUnion
                (rmapM getMap $ lmap fst last)
                (lmap snd $ catMaybes toMap)
    in postscan classifier aggregator

-- | Fold a stream of key value pairs using a function that maps keys to folds.
--
-- >>> import Data.Map (Map)
-- >>> :{
--  let f "SUM" = return Fold.sum
--      f _ = return Fold.product
--      input = Stream.fromList [("SUM",1),("PRODUCT",2),("SUM",3),("PRODUCT",4)]
--   in Stream.fold (Fold.demux f) input :: IO (Map String Int)
-- :}
-- fromList [("PRODUCT",8),("SUM",4)]
--
-- >>> demux f = Fold.demuxWith fst (Fold.lmap snd . f)
--
-- /Pre-release/
{-# INLINE demux #-}
demux :: (Monad m, IsMap f, Traversable f) =>
    (Key f -> m (Fold m a b)) -> Fold m (Key f, a) (f b)
demux f = demuxWith fst (\(k, _) -> fmap (lmap snd) (f k))

------------------------------------------------------------------------------
-- Classify: Like demux but uses the same fold for all keys.
------------------------------------------------------------------------------

-- XXX Change these to make the behavior similar to demux* variants. We can
-- implement this using classifyScanManyWith. Maintain a set of done folds in
-- the underlying monad, and when initial is called look it up, if the fold is
-- done then initial would set a flag in the state to ignore the input or
-- return an error.

-- | Folds the values for each key using the supplied fold. When scanning, as
-- soon as the fold is complete, its result is available in the second
-- component of the tuple.  The first component of the tuple is a snapshot of
-- the in-progress folds.
--
-- Once the fold for a key is done, any future values of the key are ignored.
--
-- >>> classifyScanWith f fld = Fold.demuxScanWith f (const fld)
--
{-# INLINE classifyScanWith #-}
classifyScanWith :: (Monad m, IsMap f, Traversable f, Ord (Key f)) =>
    -- Note: we need to return the Map itself to display the in-progress values
    -- e.g. to implement top. We could possibly create a separate abstraction
    -- for that use case. We return an action because we want it to be lazy so
    -- that the downstream consumers can choose to process or discard it.
    (a -> Key f) -> Fold m a b -> Fold m a (m (f b), Maybe (Key f, b))
classifyScanWith f (Fold step1 initial1 extract1) =
    fmap extract $ foldlM' step initial

    where

    initial = return $ Tuple3' IsMap.mapEmpty Set.empty Nothing

    {-# INLINE initFold #-}
    initFold kv set k a = do
        x <- initial1
        case x of
              Partial s -> do
                r <- step1 s a
                return
                    $ case r of
                          Partial s1 ->
                            Tuple3' (IsMap.mapInsert k s1 kv) set Nothing
                          Done b ->
                            Tuple3' kv set (Just (k, b))
              Done b -> return (Tuple3' kv (Set.insert k set) (Just (k, b)))

    step (Tuple3' kv set _) a = do
        let k = f a
        case IsMap.mapLookup k kv of
            Nothing -> do
                if Set.member k set
                then return (Tuple3' kv set Nothing)
                else initFold kv set k a
            Just s -> do
                r <- step1 s a
                return
                    $ case r of
                          Partial s1 ->
                            Tuple3' (IsMap.mapInsert k s1 kv) set Nothing
                          Done b ->
                            let kv1 = IsMap.mapDelete k kv
                             in Tuple3' kv1 (Set.insert k set) (Just (k, b))

    extract (Tuple3' kv _ x) = (Prelude.mapM extract1 kv, x)

-- XXX we can use a Prim IORef if we can constrain the state "s" to be Prim
--
-- The code is almost the same as classifyScanWith except the IORef operations.
--
-- | Same as classifyScanWith except that it uses mutable IORef cells in the
-- Map providing better performance. Be aware that if this is used as a scan,
-- the values in the intermediate Maps would be mutable.
--
-- >>> classifyScanMutWith = classifyScanWith
-- >>> classifyScanMutWith f fld = demuxScanMutWith f (const fld)
--
{-# INLINE classifyScanMutWith #-}
classifyScanMutWith :: (MonadIO m, IsMap f, Traversable f, Ord (Key f)) =>
    (a -> Key f) -> Fold m a b -> Fold m a (m (f b), Maybe (Key f, b))
classifyScanMutWith f (Fold step1 initial1 extract1) =
    fmap extract $ foldlM' step initial

    where

    initial = return $ Tuple3' IsMap.mapEmpty Set.empty Nothing

    {-# INLINE initFold #-}
    initFold kv set k a = do
        x <- initial1
        case x of
              Partial s -> do
                r <- step1 s a
                case r of
                      Partial s1 -> do
                        ref <- liftIO $ newIORef s1
                        return $ Tuple3' (IsMap.mapInsert k ref kv) set Nothing
                      Done b ->
                        return $ Tuple3' kv set (Just (k, b))
              Done b -> return (Tuple3' kv (Set.insert k set) (Just (k, b)))

    step (Tuple3' kv set _) a = do
        let k = f a
        case IsMap.mapLookup k kv of
            Nothing -> do
                if Set.member k set
                then return (Tuple3' kv set Nothing)
                else initFold kv set k a
            Just ref -> do
                s <- liftIO $ readIORef ref
                r <- step1 s a
                case r of
                      Partial s1 -> do
                        liftIO $ writeIORef ref s1
                        return $ Tuple3' kv set Nothing
                      Done b ->
                        let kv1 = IsMap.mapDelete k kv
                         in return
                                $ Tuple3' kv1 (Set.insert k set) (Just (k, b))

    extract (Tuple3' kv _ x) =
        (Prelude.mapM (\ref -> liftIO (readIORef ref) >>= extract1) kv, x)

-- | Fold a key value stream to a key-value container. If the same key appears
-- multiple times, only the last value is retained.
{-# INLINE toMap #-}
toMap :: (Monad m, IsMap f) => Fold m (Key f, a) (f a)
toMap = foldl' (\kv (k, v) -> IsMap.mapInsert k v kv) IsMap.mapEmpty

-- | Split the input stream based on a key field and fold each split using the
-- given fold. Useful for map/reduce, bucketizing the input in different bins
-- or for generating histograms.
--
-- >>> import Data.Map.Strict (Map)
-- >>> :{
--  let input = Stream.fromList [("ONE",1),("ONE",1.1),("TWO",2), ("TWO",2.2)]
--      classify = Fold.classifyWith fst (Fold.lmap snd Fold.toList)
--   in Stream.fold classify input :: IO (Map String [Double])
-- :}
-- fromList [("ONE",[1.0,1.1]),("TWO",[2.0,2.2])]
--
-- Once the classifier fold terminates for a particular key any further inputs
-- in that bucket are ignored.
--
-- Space used is proportional to the number of keys seen till now and
-- monotonically increases because it stores whether a key has been seen or
-- not.
--
-- /Stops: never/
--
-- /Pre-release/
--
{-# INLINE classifyWith #-}
classifyWith :: (Monad m, IsMap f, Traversable f, Ord (Key f)) =>
    (a -> Key f) -> Fold m a b -> Fold m a (f b)
classifyWith f fld =
    let
        classifier = classifyScanWith f fld
        getMap Nothing = pure IsMap.mapEmpty
        getMap (Just action) = action
        aggregator =
            teeWith IsMap.mapUnion
                (rmapM getMap $ lmap fst last)
                (lmap snd $ catMaybes toMap)
    in postscan classifier aggregator

-- | Same as 'classifyWith' but maybe faster because it uses mutable cells as
-- fold accumulators in the Map.
--
-- >>> classifyMutWith = Fold.classifyWith
--
{-# INLINE classifyMutWith #-}
classifyMutWith :: (MonadIO m, IsMap f, Traversable f, Ord (Key f)) =>
    (a -> Key f) -> Fold m a b -> Fold m a (f b)
classifyMutWith f fld =
    let
        classifier = classifyScanMutWith f fld
        getMap Nothing = pure IsMap.mapEmpty
        getMap (Just action) = action
        aggregator =
            teeWith IsMap.mapUnion
                (rmapM getMap $ lmap fst last)
                (lmap snd $ catMaybes toMap)
    in postscan classifier aggregator

-- | Given an input stream of key value pairs and a fold for values, fold all
-- the values belonging to each key.  Useful for map/reduce, bucketizing the
-- input in different bins or for generating histograms.
--
-- >>> :{
--  let input = Stream.fromList [("ONE",1),("ONE",1.1),("TWO",2), ("TWO",2.2)]
--   in Stream.fold (Fold.classify Fold.toList) input
-- :}
-- fromList [("ONE",[1.0,1.1]),("TWO",[2.0,2.2])]
--
-- Same as:
--
-- >>> classify = Fold.classifyWith fst . Fold.lmap snd
--
-- /Pre-release/
{-# INLINE classify #-}
classify :: (Monad m, Ord k) => Fold m a b -> Fold m (k, a) (Map k b)
classify = classifyWith fst . lmap snd

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
-- @unzipWithM k f1 f2 = lmapM k (unzip f1 f2)@
--
-- /Pre-release/
{-# INLINE unzipWithM #-}
unzipWithM :: Monad m
    => (a -> m (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWithM = unzipWithMUsing teeWith

-- | Similar to 'unzipWithM' but terminates when the first fold terminates.
--
--
{-# INLINE unzipWithFstM #-}
unzipWithFstM :: Monad m =>
    (a -> m (b, c)) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
unzipWithFstM = unzipWithMUsing teeWithFst

-- | Similar to 'unzipWithM' but terminates when any fold terminates.
--
--
{-# INLINE unzipWithMinM #-}
unzipWithMinM :: Monad m =>
    (a -> m (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWithMinM = unzipWithMUsing teeWithMin

-- | Split elements in the input stream into two parts using a pure splitter
-- function, direct each part to a different fold and zip the results.
--
-- @unzipWith f fld1 fld2 = Fold.lmap f (Fold.unzip fld1 fld2)@
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
-- > unzip = Fold.unzipWith id
--
-- This is the consumer side dual of the producer side 'zip' operation.
--
-- @since 0.7.0
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
-- | Zip a stream with the input of a fold using the supplied function.
--
-- /Unimplemented/
--
{-# INLINE zipWithM #-}
zipWithM :: -- Monad m =>
    (a -> b -> m c) -> t m a -> Fold m c x -> Fold m b x
zipWithM = undefined

-- | Zip a stream with the input of a fold.
--
-- /Unimplemented/
--
{-# INLINE zip #-}
zip :: Monad m => t m a -> Fold m (a, b) x -> Fold m b x
zip = zipWithM (curry return)

-- | Pair each element of a fold input with its index, starting from index 0.
--
-- /Unimplemented/
{-# INLINE indexed #-}
indexed :: -- forall m a b. Monad m =>
    Fold m (Int, a) b -> Fold m a b
indexed = undefined -- zip (Stream.enumerateFrom 0 :: SerialT m Int)

-- | Change the predicate function of a Fold from @a -> b@ to accept an
-- additional state input @(s, a) -> b@. Convenient to filter with an
-- addiitonal index or time input.
--
-- @
-- filterWithIndex = with indexed filter
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

-- | @sampleFromthen offset stride@ samples the element at @offset@ index and
-- then every element at strides of @stride@.
--
-- /Unimplemented/
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
-- >>> toStream = fmap SerialT Fold.toStreamK
--
-- /Pre-release/
{-# INLINE toStream #-}
toStream :: Monad m => Fold m a (SerialT n a)
toStream = fmap SerialT toStreamK

-- This is more efficient than 'toStream'. toStream is exactly the same as
-- reversing the stream after toStreamRev.
--
-- | Buffers the input stream to a pure stream in the reverse order of the
-- input.
--
-- >>> toStreamRev = fmap SerialT Fold.toStreamKRev
--
-- /Warning!/ working on large streams accumulated as buffers in memory could
-- be very inefficient, consider using "Streamly.Data.Array" instead.
--
-- /Pre-release/

--  xn : ... : x2 : x1 : []
{-# INLINE toStreamRev #-}
toStreamRev :: Monad m => Fold m a (SerialT n a)
toStreamRev = fmap SerialT toStreamKRev

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
unfoldMany (Unfold ustep inject) (Fold fstep initial extract) =
    Fold consume initial extract

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

{-# INLINE topBy #-}
topBy :: (MonadIO m, Storable a) =>
       (a -> a -> Ordering)
    -> Int
    -> Fold m a (MA.Array a)
topBy cmp n = Fold step initial extract

    where

    initial = do
        arr <- MA.newArray n
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
            x1 <- MA.getIndexUnsafe (i - 1) arr
            case x `cmp` x1 of
                LT -> do
                    MA.putIndexUnsafe (i - 1) x arr
                    MA.bubble cmp arr
                    return $ Partial (arr, i)
                _ -> return $ Partial (arr, i)

    extract = return . fst

-- | Fold the input stream to top n elements.
--
-- >>> stream = Stream.fromList [2::Int,7,9,3,1,5,6,11,17]
-- >>> Stream.fold (Fold.top 3) stream >>= MA.toList
-- [17,11,9]
--
-- /Pre-release/
{-# INLINE top #-}
top :: (MonadIO m, Storable a, Ord a) => Int -> Fold m a (MA.Array a)
top = topBy $ flip compare

-- | Fold the input stream to bottom n elements.
--
-- >>> stream = Stream.fromList [2::Int,7,9,3,1,5,6,11,17]
-- >>> Stream.fold (Fold.bottom 3) stream >>= MA.toList
-- [1,2,3]
--
-- /Pre-release/
{-# INLINE bottom #-}
bottom :: (MonadIO m, Storable a, Ord a) => Int -> Fold m a (MA.Array a)
bottom = topBy compare
