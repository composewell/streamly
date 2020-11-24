-- |
-- Module      : Streamly.Internal.Data.Fold
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

-- Also see the "Streamly.Internal.Data.Sink" module that provides specialized
-- left folds that discard the outputs.
--
-- IMPORTANT: keep the signatures consistent with the folds in Streamly.Prelude

module Streamly.Internal.Data.Fold
    (
    -- * Fold Type
      Step (..)
    , Fold (..)

    , hoist
    , generally

    -- , tail
    -- , init

    -- * Fold Creation Utilities
    , mkAccum
    , mkAccum_
    , mkAccumM
    , mkAccumM_
    , mkFold
    , mkFold_
    , mkFoldM
    , mkFoldM_

    -- ** Full Folds
    , drain
    , drainBy
    , drainBy2
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
    , rollingHash
    , rollingHashWithSalt
    , rollingHashFirstN
    -- , rollingHashLastN

    -- ** Fold Semigroups
    , sconcat

    -- ** Full Folds (Monoidal)
    , mconcat
    , foldMap
    , foldMapM

    -- ** Full Folds (To Containers)

    , toList
    , toListRevF  -- experimental

    -- ** Partial Folds
    , drainN
    , drainSepBy
    -- , lastN
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

    -- ** Covariant Operations
    , sequence
    , mapM

    -- ** Mapping
    , transform
    , lmap
    --, lsequence
    , lmapM
    -- ** Filtering
    , lfilter
    , lfilterM
    -- , ldeleteBy
    -- , luniq
    , lcatMaybes
    , mapMaybe
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
    , takeSepBy
    {-
    , ltakeWhileM
    , ldrop
    , ldropWhile
    , ldropWhileM
    -}

    , lsessionsOf
    , lchunksOf

    -- ** Breaking

    -- Binary
    , splitAt -- spanN
    -- , splitIn -- sessionN

    -- By elements
    , span  -- spanWhile
    , break -- breakBefore
    -- , breakAfter
    -- , breakOn
    -- , breakAround
    , spanBy
    , spanByRolling
    , sliceSepBy
    , sliceSepWith

    -- By sequences
    -- , breakOnSeq
    -- , breakOnStream -- on a stream

    -- * Distributing

    , tee
    , distribute
    , distribute_

    -- * Partitioning

    , partitionByM
    , partitionByFstM
    , partitionByMinM
    , partitionBy
    , partition

    -- * Demultiplexing

    , demux
    , demuxWith
    , demux_
    , demuxDefault_
    , demuxWith_
    , demuxWithDefault_

    -- * Classifying

    , classify
    , classifyWith

    -- * Unzipping
    , unzip
    , unzipWith
    , unzipWithM
    , unzipWithFstM
    , unzipWithMinM

    -- * Nested Folds
    -- , concatMap
    , many
    , duplicate

    -- * Running Folds
    , initialize
    , runStep

    -- * Folding to SVar
    , toParallelSVar
    , toParallelSVarLimited
    )
where

import Control.Monad (void, join)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Maybe (isJust, fromJust)
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup (Semigroup((<>)))
#endif
import Streamly.Internal.Data.Pipe.Types (Pipe (..), PipeState(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.Either.Strict (Either'(..))

import qualified Streamly.Internal.Data.Fold.Types as FL
import qualified Streamly.Internal.Data.Pipe.Types as Pipe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Prelude

import Prelude hiding
       ( filter, drop, dropWhile, take, takeWhile, zipWith
       , foldl, map, mapM_, sequence, all, any, sum, product, elem
       , notElem, maximum, minimum, head, last, tail, length, null
       , reverse, iterate, init, and, or, lookup, (!!)
       , scanl, scanl1, replicate, concatMap, mconcat, foldMap, unzip
       , span, splitAt, break, mapM)
import Streamly.Internal.Data.SVar
import Streamly.Internal.Data.Fold.Types

------------------------------------------------------------------------------
-- Smart constructors
------------------------------------------------------------------------------

-- | Make an accumulating (non-terminating) fold using a pure step function, a
-- pure initial state and a pure state extraction function.
--
-- /Internal/
--
{-# INLINE mkAccum #-}
mkAccum :: Monad m => (s -> a -> s) -> s -> (s -> b) -> Fold m a b
mkAccum step initial extract =
    Fold
        (\s a -> return $ Partial $ step s a)
        (return initial)
        (return . extract)

-- | Similar to 'mkAccum' but the final state extracted is identical to the
-- intermediate state.
--
-- /Internal/
--
{-# INLINE mkAccum_ #-}
mkAccum_ :: Monad m => (b -> a -> b) -> b -> Fold m a b
mkAccum_ step initial = mkAccum step initial id

-- | Make an accumulating (non-terminating) fold with an effectful step
-- function, an initial state, and a state extraction function.
--
-- /Internal/
--
{-# INLINE mkAccumM #-}
mkAccumM :: Functor m => (s -> a -> m s) -> m s -> (s -> m b) -> Fold m a b
mkAccumM step = Fold (\s a -> Partial <$> step s a)

-- | Similar to 'mkAccumM' but the final state extracted is identical to the
-- intermediate state.
--
-- /Internal/
--
{-# INLINE mkAccumM_ #-}
mkAccumM_ :: Monad m => (b -> a -> m b) -> m b -> Fold m a b
mkAccumM_ step initial = mkAccumM step initial return

-- | Make a terminating fold using a pure step function, a pure initial state
-- and a pure state extraction function.
--
-- /Internal/
--
{-# INLINE mkFold #-}
mkFold :: Monad m => (s -> a -> Step s b) -> s -> (s -> b) -> Fold m a b
mkFold step initial extract =
    Fold (\s a -> return $ step s a) (return initial) (return . extract)

-- | Similar to 'mkFold' but the final state extracted is identical to the
-- intermediate state.
--
-- /Internal/
--
{-# INLINE mkFold_ #-}
mkFold_ :: Monad m => (b -> a -> Step b b) -> b -> Fold m a b
mkFold_ step initial = mkFold step initial id

-- | Make a terminating fold with an effectful step function and initial state,
-- and a state extraction function.
--
-- > mkFoldM = Fold
--
--  We can just use 'Fold' but it is provided for completeness.
--
-- /Internal/
--
{-# INLINE mkFoldM #-}
mkFoldM :: (s -> a -> m (Step s b)) -> m s -> (s -> m b) -> Fold m a b
mkFoldM = Fold

-- | Similar to 'mkFoldM' but the final state extracted is identical to the
-- intermediate state.
--
-- /Internal/
--
{-# INLINE mkFoldM_ #-}
mkFoldM_ :: Monad m => (b -> a -> m (Step b b)) -> m b -> Fold m a b
mkFoldM_ step initial = Fold step initial return

------------------------------------------------------------------------------
-- hoist
------------------------------------------------------------------------------

-- | Change the underlying monad of a fold
--
-- /Internal/
hoist :: (forall x. m x -> n x) -> Fold m a b -> Fold n a b
hoist f (Fold step initial extract) =
    Fold (\x a -> f $ step x a) (f initial) (f . extract)

-- | Adapt a pure fold to any monad
--
-- > generally = hoist (return . runIdentity)
--
-- /Internal/
generally :: Monad m => Fold Identity a b -> Fold m a b
generally = hoist (return . runIdentity)

------------------------------------------------------------------------------
-- Transformations on fold inputs
------------------------------------------------------------------------------

-- | Flatten the monadic output of a fold to pure output.
--
-- @since 0.7.0
{-# INLINE sequence #-}
sequence :: Monad m => Fold m a (m b) -> Fold m a b
sequence (Fold step initial extract) = Fold step' initial extract'

    where

    step' s a = do
        res <- step s a
        case res of
            Partial x -> return $ Partial x
            Done b -> Done <$> b

    extract' = join . extract

-- | Map a monadic function on the output of a fold.
--
-- @since 0.7.0
{-# INLINE mapM #-}
mapM :: Monad m => (b -> m c) -> Fold m a b -> Fold m a c
mapM f = sequence . fmap f

-- | @mapMaybe f fold@ maps a 'Maybe' returning function @f@ on the input of
-- the fold, filters out 'Nothing' elements, and return the values extracted
-- from 'Just'.
--
-- >>> f x = if even x then Just x else Nothing
-- >>> fld = Fold.mapMaybe f Fold.toList
-- >>> Stream.fold fld (Stream.enumerateFromTo 1 10)
-- [2,4,6,8]
--
-- /Internal/
{-# INLINE mapMaybe #-}
mapMaybe :: (Monad m) => (a -> Maybe b) -> Fold m b r -> Fold m a r
mapMaybe f = lmap f . lfilter isJust . lmap fromJust

------------------------------------------------------------------------------
-- Transformations on fold inputs
------------------------------------------------------------------------------

-- rename to lpipe?
--
-- | Apply a transformation on a 'Fold' using a 'Pipe'.
--
-- @since 0.7.0
{-# INLINE transform #-}
transform :: Monad m => Pipe m a b -> Fold m b c -> Fold m a c
transform (Pipe pstep1 pstep2 pinitial) (Fold fstep finitial fextract) =
    Fold step initial extract

    where

    initial = Tuple' pinitial <$> finitial

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

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- | @_Fold1 step@ returns a new 'Fold' using just a step function that has the
-- same type for the accumulator and the element. The result type is the
-- accumulator type wrapped in 'Maybe'. The initial accumulator is retrieved
-- from the 'Foldable', the result is 'None' for empty containers.
{-# INLINABLE _Fold1 #-}
_Fold1 :: Monad m => (a -> a -> a) -> Fold m a (Maybe a)
_Fold1 step = Fold step_ (return Nothing') (return . toMaybe)

    where

    step_ Nothing' a = return $ Partial $ Just' a
    step_ (Just' x) a = return $ Partial $ Just' $ step x a

------------------------------------------------------------------------------
-- Left folds
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Run Effects
------------------------------------------------------------------------------

-- | A fold that drains all its input, running the effects and discarding the
-- results.
--
-- > drain = drainBy (const (return ()))
--
-- @since 0.7.0
{-# INLINABLE drain #-}
drain :: Monad m => Fold m a ()
drain = Fold step begin done

    where

    begin = return ()

    step _ _ = return $ FL.Partial ()

    done = return

-- |
-- > drainBy f = lmapM f drain
-- > drainBy = FL.foldMapM (void . f)
--
-- Drain all input after passing it through a monadic function. This is the
-- dual of mapM_ on stream producers.
--
-- @since 0.7.0
{-# INLINABLE drainBy #-}
drainBy ::  Monad m => (a -> m b) -> Fold m a ()
drainBy f = Fold (const (fmap FL.Partial . void . f)) (return ()) return

{-# INLINABLE drainBy2 #-}
drainBy2 ::  Monad m => (a -> m b) -> Fold2 m c a ()
drainBy2 f = Fold2 (const (void . f)) (\_ -> return ()) return

-- | Extract the last element of the input stream, if any.
--
-- > last = fmap getLast $ FL.foldMap (Last . Just)
--
-- @since 0.7.0
{-# INLINABLE last #-}
last :: Monad m => Fold m a (Maybe a)
last = _Fold1 (\_ x -> x)

------------------------------------------------------------------------------
-- To Summary
------------------------------------------------------------------------------

-- | Like 'length', except with a more general 'Num' return value
--
-- > genericLength = fmap getSum $ foldMap (Sum . const  1)
--
-- @since 0.7.0
{-# INLINE genericLength #-}
genericLength :: (Monad m, Num b) => Fold m a b
genericLength = Fold (\n _ -> return $ Partial $ n + 1) (return 0) return

-- | Determine the length of the input stream.
--
-- > length = fmap getSum $ foldMap (Sum . const  1)
--
-- @since 0.7.0
{-# INLINE length #-}
length :: Monad m => Fold m a Int
length = genericLength

-- | Determine the sum of all elements of a stream of numbers. Returns additive
-- identity (@0@) when the stream is empty. Note that this is not numerically
-- stable for floating point numbers.
--
-- > sum = fmap getSum $ FL.foldMap Sum
--
-- @since 0.7.0
{-# INLINE sum #-}
sum :: (Monad m, Num a) => Fold m a a
sum = Fold (\x a -> return $ Partial $ x + a) (return 0) return

-- | Determine the product of all elements of a stream of numbers. Returns
-- multiplicative identity (@1@) when the stream is empty. The fold terminates
-- when it encounters (@0@) in its input.
--
-- > product = fmap getProduct $ FL.foldMap Product
--
-- @since 0.7.0
-- /since 0.8.0 (Added 'Eq' constraint)/
{-# INLINE product #-}
product :: (Monad m, Num a, Eq a) => Fold m a a
product = Fold step (return 1) return

    where

    step x a =
        return
            $ if a == 0
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
maximumBy cmp = _Fold1 max'

    where

    max' x y =
        case cmp x y of
            GT -> x
            _ -> y

-- |
-- @
-- maximum = 'maximumBy' compare
-- @
--
-- Determine the maximum element in a stream.
--
-- Compare with @FL.foldMap Max@.
--
-- @since 0.7.0
{-# INLINE maximum #-}
maximum :: (Monad m, Ord a) => Fold m a (Maybe a)
maximum = _Fold1 max

-- | Computes the minimum element with respect to the given comparison function
--
-- @since 0.7.0
{-# INLINE minimumBy #-}
minimumBy :: Monad m => (a -> a -> Ordering) -> Fold m a (Maybe a)
minimumBy cmp = _Fold1 min'

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
-- Compare with @FL.foldMap Min@.
--
-- @since 0.7.0
{-# INLINE minimum #-}
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

    step (Tuple' x n) y =
        return
            $ let n' = n + 1
               in Partial $ Tuple' (x + (y - x) / n') n'

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

    step (Tuple3' n mean_ m2) x = return $ Partial $ Tuple3' n' mean' m2'

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
{-# INLINABLE stdDev #-}
stdDev :: (Monad m, Floating a) => Fold m a a
stdDev = sqrt variance

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
-- @since 0.7.0
{-# INLINABLE rollingHashWithSalt #-}
rollingHashWithSalt :: (Monad m, Enum a) => Int64 -> Fold m a Int64
rollingHashWithSalt salt = Fold step initial extract

    where

    k = 2891336453 :: Int64

    initial = return salt

    step cksum a = return $ Partial $ cksum * k + fromIntegral (fromEnum a)

    extract = return

-- | A default salt used in the implementation of 'rollingHash'.
{-# INLINE defaultSalt #-}
defaultSalt :: Int64
defaultSalt = -2578643520546668380

-- | Compute an 'Int' sized polynomial rolling hash of a stream.
--
-- > rollingHash = rollingHashWithSalt defaultSalt
--
-- @since 0.7.0
{-# INLINABLE rollingHash #-}
rollingHash :: (Monad m, Enum a) => Fold m a Int64
rollingHash = rollingHashWithSalt defaultSalt

-- | Compute an 'Int' sized polynomial rolling hash of the first n elements of
-- a stream.
--
-- > rollingHashFirstN = ltake n rollingHash
{-# INLINABLE rollingHashFirstN #-}
rollingHashFirstN :: (Monad m, Enum a) => Int -> Fold m a Int64
rollingHashFirstN n = ltake n rollingHash

------------------------------------------------------------------------------
-- Monoidal left folds
------------------------------------------------------------------------------

-- | Append the elements of an input stream to a provided starting value.
--
-- > S.fold (FL.sconcat 10) (S.map Sum $ S.enumerateFromTo 1 10)
--
-- /Internal/
--
{-# INLINE sconcat #-}
sconcat :: (Monad m, Semigroup a) => a -> Fold m a a
sconcat i = Fold (\x a -> return $ Partial $ x <> a) (return i) return

-- | Fold an input stream consisting of monoidal elements using 'mappend'
-- and 'mempty'.
--
-- > S.fold FL.mconcat (S.map Sum $ S.enumerateFromTo 1 10)
--
-- @since 0.7.0
{-# INLINE mconcat #-}
mconcat ::
    ( Monad m
#if !MIN_VERSION_base(4,11,0)
    , Semigroup a
#endif
    , Monoid a) => Fold m a a
mconcat = sconcat mempty

-- |
-- > foldMap f = lmap f mconcat
--
-- Make a fold from a pure function that folds the output of the function
-- using 'mappend' and 'mempty'.
--
-- > S.fold (FL.foldMap Sum) $ S.enumerateFromTo 1 10
--
-- @since 0.7.0
{-# INLINABLE foldMap #-}
foldMap :: (Monad m, Monoid b
#if __GLASGOW_HASKELL__ < 804
    , Semigroup b
#endif
    ) => (a -> b) -> Fold m a b
foldMap f = lmap f mconcat

-- |
-- > foldMapM f = lmapM f mconcat
--
-- Make a fold from a monadic function that folds the output of the function
-- using 'mappend' and 'mempty'.
--
-- > S.fold (FL.foldMapM (return . Sum)) $ S.enumerateFromTo 1 10
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
        return $! Partial $! mappend m m'

------------------------------------------------------------------------------
-- To Containers
------------------------------------------------------------------------------

-- | Folds the input stream to a list.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Data.Array.Storable.Foreign"
-- instead.
--
-- @since 0.7.0

-- id . (x1 :) . (x2 :) . (x3 :) . ... . (xn :) $ []
{-# INLINABLE toList #-}
toList :: Monad m => Fold m a [a]
toList = Fold (\f x -> return $ Partial $ f . (x :))
              (return id)
              (return . ($ []))

------------------------------------------------------------------------------
-- Partial Folds
------------------------------------------------------------------------------

-- | A fold that drains the first n elements of its input, running the effects
-- and discarding the results.
{-# INLINABLE drainN #-}
drainN :: Monad m => Int -> Fold m a ()
drainN n = ltake n drain

-- | A fold that drains elements of its input as long as the predicate succeeds,
-- running the effects and discarding the results.
{-# INLINABLE drainSepBy #-}
drainSepBy :: Monad m => (a -> Bool) -> Fold m a ()
drainSepBy p = takeSepBy p drain

------------------------------------------------------------------------------
-- To Elements
------------------------------------------------------------------------------

-- | Like 'index', except with a more general 'Integral' argument
--
-- @since 0.7.0
{-# INLINABLE genericIndex #-}
genericIndex :: (Integral i, Monad m) => i -> Fold m a (Maybe a)
genericIndex i = Fold step (return 0) (const (return Nothing))

    where

    step j a =
        return
            $ if i == j
              then Done $ Just a
              else Partial (j + 1)

-- | Lookup the element at the given index.
--
-- @since 0.7.0
{-# INLINABLE index #-}
index :: Monad m => Int -> Fold m a (Maybe a)
index = genericIndex

-- | Extract the first element of the stream, if any.
--
-- > head = fmap getFirst $ FL.foldMap (First . Just)
--
-- @since 0.7.0
{-# INLINABLE head #-}
head :: Monad m => Fold m a (Maybe a)
head = _Fold1 const

-- | Returns the first element that satisfies the given predicate.
--
-- @
-- find p = fmap getFirst $
--     FL.foldMap (\x -> First (if p x then Just x else Nothing))
-- @
--
-- @since 0.7.0
{-# INLINABLE find #-}
find :: Monad m => (a -> Bool) -> Fold m a (Maybe a)
find predicate = Fold step (return ()) (const (return Nothing))

    where

    step () a =
        return
            $ if predicate a
              then Done (Just a)
              else Partial ()

-- | In a stream of (key-value) pairs @(a, b)@, return the value @b@ of the
-- first pair where the key equals the given value @a@.
--
-- > lookup = snd <$> find ((==) . fst)
--
-- @since 0.7.0
{-# INLINABLE lookup #-}
lookup :: (Eq a, Monad m) => a -> Fold m (a,b) (Maybe b)
lookup a0 = Fold step (return ()) (const (return Nothing))

    where

    step () (a, b) =
        return
            $ if a == a0
              then Done $ Just b
              else Partial ()

-- | Returns the first index that satisfies the given predicate.
--
-- @since 0.7.0
{-# INLINABLE findIndex #-}
findIndex :: Monad m => (a -> Bool) -> Fold m a (Maybe Int)
findIndex predicate = Fold step (return 0) (const (return Nothing))

    where

    step i a =
        return
            $ if predicate a
              then Done $ Just i
              else Partial (i + 1)

-- | Returns the first index where a given value is found in the stream.
--
-- > elemIndex a = findIndex (== a)
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
-- > null = fmap isJust head
--
-- @since 0.7.0
{-# INLINABLE null #-}
null :: Monad m => Fold m a Bool
null = Fold (\() _ -> return $ Done False) (return ()) (\() -> return True)

-- |
-- > any p = lmap p or
-- > any p = fmap getAny . FL.foldMap (Any . p)
--
-- | Returns 'True' if any of the elements of a stream satisfies a predicate.
--
-- @since 0.7.0
{-# INLINE any #-}
any :: Monad m => (a -> Bool) -> Fold m a Bool
any predicate = Fold step initial return

    where

    initial = return False

    step _ a =
        return
            $ if predicate a
              then Done True
              else Partial False

-- | Return 'True' if the given element is present in the stream.
--
-- > elem a = any (== a)
--
-- @since 0.7.0
{-# INLINABLE elem #-}
elem :: (Eq a, Monad m) => a -> Fold m a Bool
elem a = any (a ==)

-- |
-- > all p = lmap p and
-- > all p = fmap getAll . FL.foldMap (All . p)
--
-- | Returns 'True' if all elements of a stream satisfy a predicate.
--
-- @since 0.7.0
{-# INLINABLE all #-}
all :: Monad m => (a -> Bool) -> Fold m a Bool
all predicate = Fold step initial return

    where

    initial = return True

    step _ a =
        return
            $ if predicate a
              then Partial True
              else Done False

-- | Returns 'True' if the given element is not present in the stream.
--
-- > notElem a = all (/= a)
--
-- @since 0.7.0
{-# INLINABLE notElem #-}
notElem :: (Eq a, Monad m) => a -> Fold m a Bool
notElem a = all (a /=)

-- | Returns 'True' if all elements are 'True', 'False' otherwise
--
-- > and = all (== True)
-- > and = fmap getAll . FL.foldMap All
--
-- @since 0.7.0
{-# INLINE and #-}
and :: Monad m => Fold m Bool Bool
and = all (== True)

-- | Returns 'True' if any element is 'True', 'False' otherwise
--
-- > or = any (== True)
-- > or = fmap getAny . FL.foldMap Any
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

-- XXX These would just be applicative compositions of terminating folds.

-- | @splitAt n f1 f2@ composes folds @f1@ and @f2@ such that first @n@
-- elements of its input are consumed by fold @f1@ and the rest of the stream
-- is consumed by fold @f2@.
--
-- > let splitAt_ n xs = S.fold (FL.splitAt n FL.toList FL.toList) $ S.fromList xs
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
-- /Internal/

-- This can be considered as a two-fold version of 'ltake' where we take both
-- the segments instead of discarding the leftover.
--
{-# INLINE splitAt #-}
splitAt
    :: Monad m
    => Int
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
splitAt n fld1 fld2 = splitWith (,) (ltake n fld1) fld2

------------------------------------------------------------------------------
-- Element Aware APIs
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------

-- | Stops the fold at an infixed separator element, dropping the separator.
--
-- @
-- "--.--" => "--" "--"
-- "--."   => "--" ""
-- ".--"   => ""   "--"
-- @
--
-- * Stops - when the predicate succeeds.
--
-- /Internal/
{-# INLINE sliceSepBy #-}
sliceSepBy :: Monad m => (a -> Bool) -> Fold m a b -> Fold m a b
sliceSepBy predicate (Fold fstep finitial fextract) = Fold step initial fextract

    where

    initial = finitial

    step s a =
        if not (predicate a)
        then fstep s a
        else Done <$> fextract s

-- | Like 'sliceSepBy' but does not drop the seperator element.
--
-- @
-- "--.--" => "--." "--"
-- "--."   => "--." ""
-- ".--"   => "."   "--"
-- @
--
-- * Stops - when the predicate succeeds.
--
-- /Internal/
{-# INLINE sliceSepWith #-}
sliceSepWith :: Monad m => (a -> Bool) -> Fold m a b -> Fold m a b
sliceSepWith predicate (Fold fstep finitial fextract) =
    Fold step initial fextract

    where

    initial = finitial

    step s a =
        if not (predicate a)
        then fstep s a
        else do
            res <- fstep s a
            case res of
                Partial sres -> Done <$> fextract sres
                Done bres -> return $ Done bres

data SpanByState a bl fl fr
    = SpanByLeft0 !fl !fr
    | SpanByLeft !a !fl !fr
    | SpanByRight !bl !fr

-- | Break the input stream into two groups, the first group takes the input as
-- long as the predicate applied to the first element of the stream and next
-- input element holds 'True', the second group takes the rest of the input.
--
-- /Internal/
--
{-# INLINE spanBy #-}
spanBy
    :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
spanBy cmp (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    initial = SpanByLeft0 <$> initialL <*> initialR

    step (SpanByLeft0 fl fr) input = do
        sfl <- stepL fl input
        return
            $ Partial
            $ case sfl of
                  Partial fl1 -> SpanByLeft input fl1 fr
                  Done bl -> SpanByRight bl fr
    step (SpanByLeft frst fl fr) input =
        if cmp frst input
        then do
            sfl <- stepL fl input
            return
                $ Partial
                $ case sfl of
                      Partial fl1 -> SpanByLeft frst fl1 fr
                      Done bl -> SpanByRight bl fr
        else do
            bl <- extractL fl
            sfr <- stepR fr input
            return
                $ case sfr of
                      Partial fr1 -> Partial $ SpanByRight bl fr1
                      Done br -> Done (bl, br)
    step (SpanByRight bl fr) input = do
        sfr <- stepR fr input
        return
            $ case sfr of
                  Partial fr1 -> Partial $ SpanByRight bl fr1
                  Done br -> Done (bl, br)

    extract (SpanByLeft0 fl fr) = (,) <$> extractL fl <*> extractR fr
    extract (SpanByLeft _ fl fr) = (,) <$> extractL fl <*> extractR fr
    extract (SpanByRight bl fr) = (bl, ) <$> extractR fr

data SpanState bl fl fr
    = SpanLeft !fl !fr
    | SpanRight !bl !fr

-- | @span p f1 f2@ composes folds @f1@ and @f2@ such that @f1@ consumes the
-- input as long as the predicate @p@ is 'True'.  @f2@ consumes the rest of the
-- input.
--
-- > let span_ p xs = S.fold (FL.span p FL.toList FL.toList) $ S.fromList xs
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
-- /Internal/

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
    Fold step initial extract

    where

    initial = SpanLeft <$> initialL <*> initialR

    step (SpanLeft fl fr) input =
        if p input
        then do
            sfl <- stepL fl input
            return
                $ Partial
                $ case sfl of
                      Partial fl1 -> SpanLeft fl1 fr
                      Done bl -> SpanRight bl fr
        else do
            bl <- extractL fl
            sfr <- stepR fr input
            return
                $ case sfr of
                      Partial fr1 -> Partial $ SpanRight bl fr1
                      Done br -> Done (bl, br)
    step (SpanRight bl fr) input = do
        sfr <- stepR fr input
        return
            $ case sfr of
                  Partial fr1 -> Partial $ SpanRight bl fr1
                  Done br -> Done (bl, br)

    extract (SpanLeft fl fr) = (,) <$> extractL fl <*> extractR fr
    extract (SpanRight bl fr) = (bl, ) <$> extractR fr

-- |
-- > break p = span (not . p)
--
-- Break as soon as the predicate becomes 'True'. @break p f1 f2@ composes
-- folds @f1@ and @f2@ such that @f1@ stops consuming input as soon as the
-- predicate @p@ becomes 'True'. The rest of the input is consumed @f2@.
--
-- This is the binary version of 'splitBy'.
--
-- > let break_ p xs = S.fold (S.break p FL.toList FL.toList) $ S.fromList xs
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
-- /Internal/
{-# INLINE break #-}
break
    :: Monad m
    => (a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
break p = span (not . p)

-- | Like 'spanBy' but applies the predicate in a rolling fashion i.e.
-- predicate is applied to the previous and the next input elements.
--
-- /Internal/
{-# INLINE spanByRolling #-}
spanByRolling
    :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
spanByRolling cmp (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    initial = SpanByLeft0 <$> initialL <*> initialR

    step (SpanByLeft0 fl fr) input = do
        sfl <- stepL fl input
        return
            $ Partial
            $ case sfl of
                  Partial fl1 -> SpanByLeft input fl1 fr
                  Done bl -> SpanByRight bl fr
    step (SpanByLeft frst fl fr) input =
        if cmp frst input
        then do
            sfl <- stepL fl input
            return
                $ Partial
                $ case sfl of
                      Partial fl1 -> SpanByLeft input fl1 fr
                      Done bl -> SpanByRight bl fr
        else do
            bl <- extractL fl
            sfr <- stepR fr input
            return
                $ case sfr of
                      Partial fr1 -> Partial $ SpanByRight bl fr1
                      Done br -> Done (bl, br)
    step (SpanByRight bl fr) input = do
        sfr <- stepR fr input
        return
            $ case sfr of
                  Partial fr1 -> Partial $ SpanByRight bl fr1
                  Done br -> Done (bl, br)

    extract (SpanByLeft0 fl fr) = (,) <$> extractL fl <*> extractR fr
    extract (SpanByLeft _ fl fr) = (,) <$> extractL fl <*> extractR fr
    extract (SpanByRight bl fr) = (bl, ) <$> extractR fr

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
-- >>> S.fold (FL.tee FL.sum FL.length) (S.enumerateFromTo 1.0 100.0)
-- (5050.0,100)
--
-- @since 0.7.0
{-# INLINE tee #-}
tee :: Monad m => Fold m a b -> Fold m a c -> Fold m a (b,c)
tee f1 f2 = teeWith (,) f1 f2

{-# INLINE foldNil #-}
foldNil :: Monad m => Fold m a [b]
foldNil = Fold step begin done  where
  begin = return []
  step _ _ = return $ Partial []
  done = return

-- XXX How is the performance?
{-# INLINE foldCons #-}
foldCons :: Monad m => Fold m a b -> Fold m a [b] -> Fold m a [b]
foldCons f1 f2 = teeWith (:) f1 f2

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
-- >>> S.fold (FL.distribute [FL.sum, FL.length]) (S.enumerateFromTo 1 5)
-- [15,5]
--
-- This is the consumer side dual of the producer side 'sequence' operation.
--
-- @since 0.7.0
{-# INLINE distribute #-}
distribute :: Monad m => [Fold m a b] -> Fold m a [b]
distribute = foldr foldCons foldNil

-- XXX This is 2x times faster wiithout the terminating condition.
-- | Like 'distribute' but for folds that return (), this can be more efficient
-- than 'distribute' as it does not need to maintain state. This fold terminates
-- when all the folds terminate.
--
{-# INLINE distribute_ #-}
distribute_ :: Monad m => [Fold m a ()] -> Fold m a ()
distribute_ fs = Fold step initial extract

    where

    initial = Prelude.mapM initialize fs

    -- XXX This itself can be a recursive function but we might face inlining
    -- problems.
    step [] _ = return $ Done ()
    step ss a = do
        !ss1 <- go ss a
        return
          $ if Prelude.null ss1
            then Done ()
            else Partial ss1

    go [] _ = return []
    go ((Fold s i d):ss) a = do
        i1 <- i
        res <- s i1 a
        case res of
            Partial i2 -> do
                rst <- go ss a
                let fld = Fold s (return i2) d
                return $ fld : rst
            Done () -> go ss a

    extract ss = go1 ss

    go1 [] = return ()
    go1 ((Fold _ i d):ss) = (i >>= d) >> go1 ss

------------------------------------------------------------------------------
-- Partitioning
------------------------------------------------------------------------------

-- | Partition the input over two folds using an 'Either' partitioning
-- predicate. This fold terminates when both the folds terminate.
--
-- See also: 'partitionByFstM' and 'partitionByMinM'.
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
-- >>> S.fold (FL.partitionByM randomly FL.length FL.length) (S.enumerateFromTo 1 100)
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
--  r <- S.fold (FL.partitionByM f FL.length FL.length) (S.enumerateFromTo (1 :: Int) 100)
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

    begin = do
        sL <- beginL
        sR <- beginR
        return $ RunBoth sL sR

    step (RunBoth sL sR) a = do
        r <- f a
        case r of
            Left b -> do
                res <- stepL sL b
                return
                  $ Partial
                  $ case res of
                        Partial sres -> RunBoth sres sR
                        Done bres -> RunRight bres sR
            Right c -> do
                res <- stepR sR c
                return
                  $ Partial
                  $ case res of
                        Partial sres -> RunBoth sL sres
                        Done bres -> RunLeft sL bres
    step (RunLeft sL bR) a = do
        r <- f a
        case r of
            Left b -> do
                res <- stepL sL b
                return
                  $ case res of
                        Partial sres -> Partial $ RunLeft sres bR
                        Done bres -> Done (bres, bR)
            Right _ -> return $ Partial $ RunLeft sL bR
    step (RunRight bL sR) a = do
        r <- f a
        case r of
            Left _ -> return $ Partial $ RunRight bL sR
            Right c -> do
                res <- stepR sR c
                return
                  $ case res of
                        Partial sres -> Partial $ RunRight bL sres
                        Done bres -> Done (bL, bres)

    done (RunBoth sL sR) = do
        bL <- doneL sL
        bR <- doneR sR
        return (bL, bR)
    done (RunLeft sL bR) = do
        bL <- doneL sL
        return (bL, bR)
    done (RunRight bL sR) = do
        bR <- doneR sR
        return (bL, bR)

-- | Similar to 'partitionByM' but terminates when the first fold terminates.
--
-- /Unimplemented/
--
{-# INLINE partitionByFstM #-}
partitionByFstM :: -- Monad m =>
       (a -> m (Either b c)) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionByFstM = undefined

-- | Similar to 'partitionByM' but terminates when any fold terminates.
--
-- /Unimplemented/
--
{-# INLINE partitionByMinM #-}
partitionByMinM :: -- Monad m =>
       (a -> m (Either b c)) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionByMinM = undefined

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
--   in S.fold f (S.enumerateFromTo 1 100)
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
    => (a -> (k, a')) -> Map k (Fold m a' b) -> Fold m a (Map k b)
demuxWith f kv = Fold step initial extract

    where

    initial = Tuple' Map.empty <$> Prelude.mapM initialize kv

    step (Tuple' bmp mp) a =
        if Map.size mp > 0
        then do
            let (k, a') = f a
            case Map.lookup k mp of
                Nothing -> return $ Partial $ Tuple' bmp mp
                Just (Fold stp ini dn) -> do
                    !st <- ini
                    !res <- stp st a'
                    return
                        $ case res of
                              Partial sres ->
                                  let fld = Fold stp (return sres) dn
                                      mp1 = Map.insert k fld mp
                                   in Partial $ Tuple' bmp mp1
                              Done bres ->
                                  let mp1 = Map.delete k mp
                                      bmp1 = Map.insert k bres bmp
                                   in if Map.size mp1 == 0
                                      then Done bmp1
                                      else Partial $ Tuple' bmp1 mp1
        -- XXX This state is reached even for an empty Map. That will change
        -- once we have Step in the initial value
        else error "Illegal state"

    extract (Tuple' bmp mp) = do
        mpe <- Prelude.mapM (\(Fold _ i d) -> i >>= d) mp
        return $ bmp `Map.union` mpe

-- | Fold a stream of key value pairs using a map of specific folds for each
-- key into a map from keys to the results of fold outputs of the corresponding
-- values.
--
-- @
-- > let table = Data.Map.fromList [(\"SUM", FL.sum), (\"PRODUCT", FL.product)]
--       input = S.fromList [(\"SUM",1),(\"PRODUCT",2),(\"SUM",3),(\"PRODUCT",4)]
--   in S.fold (FL.demux table) input
-- fromList [("PRODUCT",8),("SUM",4)]
-- @
--
-- @since 0.7.0
{-# INLINE demux #-}
demux :: (Monad m, Ord k)
    => Map k (Fold m a b) -> Fold m (k, a) (Map k b)
demux = demuxWith id

-- data DemuxState m s = DemuxP !m !s | DemuxingOnlyMap !m
data DemuxState s m1 m2 = DemuxingWithDefault !s !m1 !m2 | DemuxingOnlyMap !m2

{-# INLINE demuxWithDefault_ #-}
demuxWithDefault_ :: (Monad m, Ord k)
    => (a -> (k, a')) -> Map k (Fold m a' b) -> Fold m (k, a') b -> Fold m a ()
demuxWithDefault_ f kv (Fold dstep dinitial dextract) =
    Fold step initial extract

    where

    initial = do
        ini <- dinitial
        mp <- Prelude.mapM initialize kv
        return $ DemuxingWithDefault ini Set.empty mp

    -- XXX Some code can probably be abstracted
    step (DemuxingWithDefault dacc bst mp) a =
        if Map.size mp > 0
        then do
            let (k, a1) = f a
            case Map.lookup k mp of
                Nothing ->
                    if k `Set.member` bst
                    then return $ Partial $ DemuxingWithDefault dacc bst mp
                    else do
                        res <- dstep dacc (k, a1)
                        return
                            $ Partial
                            $ case res of
                                  Partial sres ->
                                      DemuxingWithDefault sres bst mp
                                  Done _ -> DemuxingOnlyMap mp
                Just (Fold stp ini dn) -> do
                    !st <- ini
                    !res <- stp st a1
                    return
                        $ case res of
                              Partial sres ->
                                  let fld = (Fold stp (return sres) dn)
                                      mp1 = Map.insert k fld mp
                                   in Partial $ DemuxingWithDefault dacc bst mp1
                              Done _ ->
                                  -- XXX We can skip the check here
                                  if Map.size mp == 1
                                  then Done ()
                                  else Partial
                                           $ DemuxingWithDefault
                                                 dacc
                                                 (Set.insert k bst)
                                                 (Map.delete k mp)
        -- XXX This state is reached even for an empty Map. That will change
        -- once we have Step in the initial value
        else error "Illegal state"
    -- XXX Reduce code duplication?
    step (DemuxingOnlyMap mp) a =
        if Map.size mp > 0
        then do
            let (k, a1) = f a
            case Map.lookup k mp of
                Nothing -> return $ Partial $ DemuxingOnlyMap mp
                Just (Fold stp ini dn) -> do
                    !st <- ini
                    !res <- stp st a1
                    return
                        $ case res of
                              Partial sres ->
                                  Partial
                                      $ DemuxingOnlyMap
                                      $ Map.insert
                                            k
                                            (Fold stp (return sres) dn)
                                            mp
                              Done _ ->
                                  if Map.size mp == 1
                                  then Done ()
                                  else Partial
                                           $ DemuxingOnlyMap $ Map.delete k mp
        -- XXX This state is reached even for an empty Map. That will change
        -- once we have Step in the initial value
        else error "Illegal state"

    extract (DemuxingWithDefault dacc _ mp) = do
        void $ dextract dacc
        Prelude.mapM_ (\(Fold _ i d) -> i >>= d) mp
    extract (DemuxingOnlyMap mp) = do
        Prelude.mapM_ (\(Fold _ i d) -> i >>= d) mp

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
    => (a -> (k, a')) -> Map k (Fold m a' b) -> Fold m a ()
demuxWith_ f kv = Fold step initial extract

    where

    initial = Tuple' (Map.size kv) <$> Prelude.mapM initialize kv

    step (Tuple' n mp) a
        | (k, a') <- f a =
            case Map.lookup k mp of
                Nothing -> return $ Partial $ Tuple' n mp
                Just (Fold stp ini dn) -> do
                    !st <- ini
                    !res <- stp st a'
                    let n1 = n - 1
                    return
                        $ case res of
                              Partial sres ->
                                  let fld = Fold stp (return sres) dn
                                   in Partial $ Tuple' n $ Map.insert k fld mp
                              Done _ ->
                                  if n1 == 0
                                  then Done ()
                                  else Partial $ Tuple' n1 $ Map.delete k mp

    extract (Tuple' _ mp) = do
        Prelude.mapM_ (\(Fold _ i d) -> i >>= d) mp

-- | Given a stream of key value pairs and a map from keys to folds, fold the
-- values for each key using the corresponding folds, discarding the outputs.
--
-- @
-- > let prn = FL.drainBy print
-- > let table = Data.Map.fromList [(\"ONE", prn), (\"TWO", prn)]
--       input = S.fromList [(\"ONE",1),(\"TWO",2)]
--   in S.fold (FL.demux_ table) input
-- One 1
-- Two 2
-- @
--
-- @since 0.7.0
{-# INLINE demux_ #-}
demux_ :: (Monad m, Ord k) => Map k (Fold m a ()) -> Fold m (k, a) ()
demux_ = demuxWith_ id

{-# INLINE demuxDefault_ #-}
demuxDefault_ :: (Monad m, Ord k)
    => Map k (Fold m a ()) -> Fold m (k, a) () -> Fold m (k, a) ()
demuxDefault_ = demuxWithDefault_ id

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
--   in S.fold (FL.classify FL.toList) input
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
        case Map.lookup k kv of
            Nothing -> do
                x <- initial
                r <- step x a
                return
                    $ Partial
                    $ flip (Map.insert k) kv
                    $ case r of
                          Partial sr -> Left' sr
                          Done br -> Right' br
            Just x -> do
                case x of
                    Left' s -> do
                        r <- step s a
                        return
                            $ Partial
                            $ flip (Map.insert k) kv
                            $ case r of
                                  Partial sr -> Left' sr
                                  Done br -> Right' br
                    Right' _ -> return $ Partial kv

        where

        k = f a

    extract' =
        Prelude.mapM
            (\case
                 Left' s -> extract s
                 Right' b -> return b)

-- | Given an input stream of key value pairs and a fold for values, fold all
-- the values belonging to each key.  Useful for map/reduce, bucketizing the
-- input in different bins or for generating histograms.
--
-- @
-- > let input = S.fromList [(\"ONE",1),(\"ONE",1.1),(\"TWO",2), (\"TWO",2.2)]
--   in S.fold (FL.classify FL.toList) input
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

-- | Like 'unzipWith' but with a monadic splitter function.
--
-- -- @unzipWithM f fld1 fld2 = lmapM f (unzip fld1 fld2)@
--
-- @since 0.7.0
{-# INLINE unzipWithM #-}
unzipWithM :: Monad m
    => (a -> m (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWithM f (Fold stepL beginL doneL) (Fold stepR beginR doneR) =
    Fold step begin done

    where

    begin = RunBoth <$> beginL <*> beginR

    step (RunBoth sL sR) a = do
        (b, c) <- f a
        resL <- stepL sL b
        resR <- stepR sR c
        case resL of
            Partial sresL ->
                return
                    $ Partial
                    $ case resR of
                          Partial sresR -> RunBoth sresL sresR
                          Done bresR -> RunLeft sresL bresR
            Done bresL ->
                return
                    $ case resR of
                          Partial sresR -> Partial $ RunRight bresL sresR
                          Done bresR -> Done (bresL, bresR)
    step (RunLeft sL bR) a = do
        (b, _) <- f a
        resL <- stepL sL b
        return
            $ case resL of
                  Partial sresL -> Partial $ RunLeft sresL bR
                  Done bresL -> Done (bresL, bR)
    step (RunRight bL sR) a = do
        (_, c) <- f a
        resR <- stepR sR c
        return
            $ case resR of
                  Partial sresR -> Partial $ RunRight bL sresR
                  Done bresR -> Done (bL, bresR)

    done (RunBoth sL sR) = (,) <$> doneL sL <*> doneR sR
    done (RunLeft sL bR) = (,) <$> doneL sL <*> return bR
    done (RunRight bL sR) = (,) bL <$> doneR sR

-- | Similar to 'unzipWithM' but terminates when the first fold terminates.
--
-- /Unimplemented/
--
{-# INLINE unzipWithFstM #-}
unzipWithFstM :: -- Monad m =>
     (a -> m (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWithFstM = undefined

-- | Similar to 'unzipWithM' but terminates when any fold terminates.
--
-- /Unimplemented/
--
{-# INLINE unzipWithMinM #-}
unzipWithMinM :: -- Monad m =>
     (a -> m (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWithMinM = undefined

-- | Split elements in the input stream into two parts using a pure splitter
-- function, direct each part to a different fold and zip the results.
--
-- @unzipWith f fld1 fld2 = lmap f (unzip fld1 fld2)@
--
-- This fold terminates when both the input folds terminate.
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


{-
-- XXX this would be an application of "many" using a terminating fold.
--
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

------------------------------------------------------------------------------
-- Fold to a Parallel SVar
------------------------------------------------------------------------------

{-# INLINE toParallelSVar #-}
toParallelSVar :: MonadIO m => SVar t m a -> Maybe WorkerInfo -> Fold m a ()
toParallelSVar svar winfo = Fold step initial extract

    where

    initial = return ()

    -- XXX we can have a separate fold for unlimited buffer case to avoid a
    -- branch in the step here.
    step () x =
        liftIO $ do
            decrementBufferLimit svar
            void $ send svar (ChildYield x)
            return $ FL.Partial ()

    extract () = liftIO $ sendStop svar winfo

{-# INLINE toParallelSVarLimited #-}
toParallelSVarLimited :: MonadIO m
    => SVar t m a -> Maybe WorkerInfo -> Fold m a ()
toParallelSVarLimited svar winfo = Fold step initial extract

    where

    initial = return True

    step True x =
        liftIO $ do
            yieldLimitOk <- decrementYieldLimit svar
            if yieldLimitOk
            then do
                decrementBufferLimit svar
                void $ send svar (ChildYield x)
                return $ FL.Partial True
            else do
                cleanupSVarFromWorker svar
                sendStop svar winfo
                return $ FL.Done ()
    step False _ = return $ FL.Done ()

    extract True = liftIO $ sendStop svar winfo
    extract False = return ()
