{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
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
-- This module is the consumer side dual of the "Streamly.Prelude" module. The
-- "Streamly.Prelude" module contains producer or stream generation side
-- compositions while this module contains consumer side or stream fold
-- composition. Both the modules are organized in the same way so that you can
-- easily find the corresponding operations.
--
-- Stream consumers are essentially left folds that can be composed in many
-- interesting ways.  For example, a parsing applicative composition builds an
-- applicative parser, a distributing applicative fold composition distributes
-- the input to the constituent folds and then combines the outputs.
-- Similarly, a partitioning applicative can partition the input among the
-- constituent folds. There are many other ways to combine folds.
--
-- > import qualified Streamly.Fold as FL
--
--
-- A left fold is represented by the type 'Fold'. @Fold m a b@ folds an
-- input stream consisting of values of type @a@ to a structure of type
-- @b@. The fold can be run using 'foldl'' and an input stream.
--
-- >>> FL.foldl' FL.sum (S.enumerateFromTo 1 100)
-- 5050

-- Also see the "Streamly.Sink" module that provides specialized left folds
-- that discard the outputs.
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
    -- applications, they buffer all the input in memory.

    , toStream  -- experimental
    , toStreamRev  -- experimental

    , toList
    , toListRev  -- experimental

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

    -- ** Mapping
    , lmap
    --, sequence
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

    -- ** Filtering
    , lfilter
    , lfilterM
    , ltake
    , ltakeWhile
    {-
    , ltakeWhileM
    , ldrop
    , ldropWhile
    , ldropWhileM
    , ldeleteBy
    , luniq

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

    -- * Stream Splitting Operations
    -- ** Spanning
    -- | Spanning splits the input into two groups and applies two different
    -- folds on each group.

    -- Element unaware spanning
    , splitAt

    -- Element aware spanning
    , span
    , break
    -- , spanBy
    -- , spanRollingBy

    -- ** Grouping
    -- | Grouping splits the stream into groups and applies the supplied fold
    -- on each group.

    -- In imperative terms grouped folding can be considered as a nested loop
    -- where we loop over the stream to group elements and then loop over
    -- individual groups to fold them to a single value that is yielded in the
    -- output stream.
    --
    -- Note that these grouping folds are true streaming folds that never
    -- accumulate the group in memory before folding, i.e. the group elements
    -- are consumed by the folds as they are yielded by the stream. Therefore,
    -- the whole computation runs in constant space.
    -- In contrast, we can simply use a scan on the stream to buffer the whole
    -- groups in memory and then map a fold on it to fold the groups. This kind
    -- of grouping and folding would not work well when the group size is big.

    -- Element unaware grouping
    , groupsOf
    , intervalsOf

    -- Element aware grouping
    , groups
    , groupsBy
    , groupsRollingBy
    , grouped -- XXX experimental

    -- ** Splitting by an Element
    , splitBy
    , splitSuffixBy
    -- , splitPrefixBy
    , wordsBy

    -- ** Splitting on a Sequence
    , splitOn
    , splitSuffixOn
    -- , splitPrefixOn
    -- , wordsOn

    -- Keeping the delimiters
    , splitOn'
    , splitSuffixOn'
    -- , splitPrefixOn'

    -- Splitting by multiple sequences
    -- , splitOnAny
    -- , splitSuffixOnAny
    -- , splitPrefixOnAny

    -- ** Distributing
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

    -- ** Partitioning
    -- |
    -- Direct items in the input stream to different folds using a function to
    -- select the fold. This is useful to demultiplex the input stream.
    , partitionByM
    , partitionBy
    , demux_
    , classify

    -- ** Unzipping
    , unzip
    , unzipWith
    , unzipWithM

    -- ** Nested Folds
    -- , concatMap
    -- , groupsOf
    , duplicate  -- experimental

    {-
    -- * Splitter scans
    -- | Scans that can be used to split and fold a stream using 'grouped'.
    , _newline
    -}
    )
where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust, isJust)

import Foreign.Storable (Storable(..))
import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, replicate, concatMap, mconcat, foldMap, unzip,
               span, splitAt, break)

import qualified Data.Map.Strict as Map

import Streamly (MonadAsync, parallel)
import Streamly.Fold.Types (Fold(..))
import Streamly.Mem.Array (Array)
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK (IsStream())

import Streamly.Strict

import qualified Streamly.Mem.Array as A
import qualified Streamly.Prelude as S
import qualified Streamly.Streams.StreamD as D
import qualified Streamly.Streams.StreamK as K
import qualified Streamly.Streams.Prelude as P

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
{-# INLINABLE toListRev #-}
toListRev :: Monad m => Fold m a [a]
toListRev = Fold (\xs x -> return $ x:xs) (return []) return

-- | A fold that buffers its input to a pure stream.
--
-- /Warning!/ working on large streams accumulated as buffers in memory could
-- be very inefficient, consider using "Streamly.Array" instead.
--
-- @since 0.7.0
{-# INLINABLE toStream #-}
toStream :: Monad m => Fold m a (SerialT Identity a)
toStream = Fold (\f x -> return $ f . (x `K.cons`))
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
{-# INLINABLE toStreamRev #-}
toStreamRev :: Monad m => Fold m a (SerialT Identity a)
toStreamRev = Fold (\xs x -> return $ x `S.cons` xs) (return S.nil) return

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
{-# INLINABLE lfilterM #-}
lfilterM :: Monad m => (a -> m Bool) -> Fold m a r -> Fold m a r
lfilterM f (Fold step begin done) = Fold step' begin done
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
-- elements of its input are sent to fold @f1@ and the rest of the stream is
-- sent to fold @f2@.
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
-- @since 0.7.0
{-# INLINE splitAt #-}
splitAt
    :: Monad m
    => Int
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
splitAt n (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
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

-- | Group the input stream into groups of @n@ elements each and then fold each
-- group using the provided fold function.
--
-- >> S.toList $ FL.groupsOf 2 FL.sum (S.enumerateFromTo 1 10)
-- > [3,7,11,15,19]
--
-- @since 0.7.0
{-# INLINE groupsOf #-}
groupsOf
    :: (IsStream t, Monad m)
    => Int -> Fold m a b -> t m a -> t m b
groupsOf n f m = D.fromStreamD $ D.groupsOf n f (D.toStreamD m)

-- | Transform a fold from a pure input to a 'Maybe' input, consuming only
-- 'Just' values.
lcatMaybes :: Monad m => Fold m a b -> Fold m (Maybe a) b
lcatMaybes = lfilter isJust . lmap fromJust

-- XXX add this example after fixing the serial stream rate control
-- >>> S.toList $ S.take 5 $ intervalsOf 1 FL.sum $ constRate 2 $ S.enumerateFrom 1
-- > [3,7,11,15,19]
--
-- | Group the input stream into windows of @n@ second each and then fold each
-- group using the provided fold function.
--
-- @since 0.7.0
{-# INLINE intervalsOf #-}
intervalsOf
    :: (IsStream t, MonadAsync m)
    => Double -> Fold m a b -> t m a -> t m b
intervalsOf n f m = grouped (lcatMaybes f) s
    where
    s = S.map (\x -> (Just x, False)) m `parallel` S.repeatM timeout
    timeout = do
        liftIO $ threadDelay (round $ n * 1000000)
        return (Nothing, True)

------------------------------------------------------------------------------
-- Element Aware APIs
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------

{-
-- | Break the input stream into two groups, the first group takes the input as
-- long as the predicate applied to the first element of the stream and next
-- input element holds 'True', the second group takes the rest of the input.
--
spanBy
    :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
spanBy cmp (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
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
-}

-- | Span as long as the predicate is 'True'. @span p f1 f2@ composes folds
-- @f1@ and @f2@ such that the composed fold continues sending the input to
-- @f1@ as long as the predicate @p@ is 'True'.  The rest of the input is sent
-- to @f2@.
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
-- folds @f1@ and @f2@ such that @f1@ stops receiving input as soon as the
-- predicate @p@ becomes 'True'. The rest of the input is sent to @f2@.
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
{-# INLINE break #-}
break
    :: Monad m
    => (a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
break p = span (not . p)

{-
-- | Like 'spanBy' but applies the predicate in a rolling fashion i.e.
-- predicate is applied to the previous and the next input elements.
{-# INLINE spanRollingBy #-}
spanRollingBy
    :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
spanRollingBy cmp (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
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
-}

------------------------------------------------------------------------------
-- N-ary APIs
------------------------------------------------------------------------------
--
-- XXX should we use a strict pair?
--
-- | The splitter returns True if the current element is the last element of
-- the group, otherwise returns false.
--
-- @since 0.7.0
{-# INLINE grouped #-}
grouped
    :: (IsStream t, Monad m)
    => Fold m a b
    -> t m (a, Bool)
    -> t m b
grouped f m = D.fromStreamD $ D.grouped f (D.toStreamD m)

-- An example of a splitter to be used with grouped.
_newline :: IsStream t => t m Char -> t m (Char,Bool)
_newline m = S.foldrS (\x xs ->
    if x == '\n'
    then (x,True) `K.cons` xs
    else (x,False) `K.cons` xs) K.nil m

-- | @groupsBy cmp f $ S.fromList [a,b,c,...]@ assigns the element @a@ to the
-- first group, if @a \`cmp` b@ is 'True' then @b@ is also assigned to the same
-- group.  If @a \`cmp` c@ is 'True' then @c@ is also assigned to the same
-- group and so on. When the comparison fails a new group is started. Each
-- group is folded using the fold @f@.
--
-- >>> S.toList $ FL.groupsBy (>) FL.toList $ S.fromList [1,3,7,0,2,5]
-- > [[1,3,7],[0,2,5]]
--
-- @since 0.7.0
groupsBy
    :: (IsStream t, Monad m)
    => (a -> a -> Bool)
    -> Fold m a b
    -> t m a
    -> t m b
groupsBy cmp f m = D.fromStreamD $ D.groupsBy cmp f (D.toStreamD m)

-- | Unlike @groupsBy@ this function performs a rolling comparison of two
-- successive elements in the input stream. @groupsRollingBy cmp f $ S.fromList
-- [a,b,c,...]@ assigns the element @a@ to the first group, if @a \`cmp` b@ is
-- 'True' then @b@ is also assigned to the same group.  If @b \`cmp` c@ is
-- 'True' then @c@ is also assigned to the same group and so on. When the
-- comparison fails a new group is started. Each group is folded using the fold
-- @f@.
--
-- >>> S.toList $ FL.groupsRollingBy (\a b -> a + 1 == b) FL.toList $ S.fromList [1,2,3,7,8,9]
-- > [[1,2,3],[7,8,9]]
--
-- @since 0.7.0
groupsRollingBy
    :: (IsStream t, Monad m)
    => (a -> a -> Bool)
    -> Fold m a b
    -> t m a
    -> t m b
groupsRollingBy cmp f m =  D.fromStreamD $ D.groupsRollingBy cmp f (D.toStreamD m)

-- |
-- > groups = groupsBy (==)
-- > groups = groupsRollingBy (==)
--
-- Groups a contiguous span of equal elements together in one group.
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
-- the separator.  Separators are not considered part of stream segments on
-- either side of it instead they are treated as infixed between two stream
-- segments. For example, with @.@ as separator, @"a.b.c"@ would be parsed as
-- @["a","b","c"]@. When @.@ is in leading or trailing position it is still
-- considered as infixed, treating the first or the last segment as empty.  For
-- example, @".a."@ would be parsed as @["","a",""]@.  This operation is
-- opposite of 'intercalate'.
--
-- Let's use the following definition for illustration:
--
-- > splitBy_ p xs = S.toList $ FL.splitBy p (FL.toList) (S.fromList xs)
--
-- >>> splitBy_ (== '.') ""
-- [""]
--
-- >>> splitBy_ (== '.') "."
-- ["",""]
--
-- >>> splitBy_ (== '.') ".a"
-- > ["","a"]
--
-- >>> splitBy_ (== '.') "a."
-- > ["a",""]
--
-- >>> splitBy_ (== '.') "a.b"
-- > ["a","b"]
--
-- >>> splitBy_ (== '.') "a..b"
-- > ["a","","b"]
--
-- @since 0.7.0
{-# INLINE splitBy #-}
splitBy
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitBy predicate f m =
    D.fromStreamD $ D.splitBy predicate f (D.toStreamD m)

-- | Like 'splitBy' but the separator is treated as part of the previous
-- stream segment (suffix).  Therefore, when the separator is in trailing
-- position, no empty segment is considered to follow it. For example, @"a.b."@
-- would be parsed as @["a","b"]@ instead of @["a","b",""]@ as in the case of
-- 'splitBy'.
--
-- > splitSuffixBy_ p xs = S.toList $ FL.splitSuffixBy p (FL.toList) (S.fromList xs)
--
-- >>> splitSuffixBy_ (== '.') ""
-- []
--
-- >>> splitSuffixBy_ (== '.') "."
-- [""]
--
-- >>> splitSuffixBy_ (== '.') "a"
-- ["a"]
--
-- >>> splitSuffixBy_ (== '.') ".a"
-- > ["","a"]
--
-- >>> splitSuffixBy_ (== '.') "a."
-- > ["a"]
--
-- >>> splitSuffixBy_ (== '.') "a.b"
-- > ["a","b"]
--
-- >>> splitSuffixBy_ (== '.') "a.b."
-- > ["a","b"]
--
-- >>> splitSuffixBy_ (== '.') "a..b.."
-- > ["a","","b",""]
--
-- > lines = splitSuffixBy (== '\n')
--
-- @since 0.7.0
{-# INLINE splitSuffixBy #-}
splitSuffixBy
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitSuffixBy predicate f m =
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

-- XXX we should express this using the Splitter config.
--
-- We can get splitSuffixBy' by appending the suffix to the output segments
-- produced by splitSuffixBy. However, it may add an additional suffix if the last
-- fragment did not have a suffix in the first place.

-- | Like 'splitSuffixBy' but keeps the suffix in the splits.
--
-- > splitSuffixBy'_ p xs = S.toList $ FL.splitSuffixBy' p (FL.toList) (S.fromList xs)
--
-- >>> splitSuffixBy'_ (== '.') ""
-- []
--
-- >>> splitSuffixBy'_ (== '.') "."
-- ["."]
--
-- >>> splitSuffixBy'_ (== '.') "a"
-- ["a"]
--
-- >>> splitSuffixBy'_ (== '.') ".a"
-- > [".","a"]
--
-- >>> splitSuffixBy'_ (== '.') "a."
-- > ["a."]
--
-- >>> splitSuffixBy'_ (== '.') "a.b"
-- > ["a.","b"]
--
-- >>> splitSuffixBy'_ (== '.') "a.b."
-- > ["a.","b."]
--
-- >>> splitSuffixBy'_ (== '.') "a..b.."
-- > ["a.",".","b.","."]
--
-- @since 0.7.0
{-# INLINE _splitSuffixBy' #-}
_splitSuffixBy'
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
_splitSuffixBy' predicate f m = grouped f (S.map (\a -> (a, predicate a)) m)

------------------------------------------------------------------------------
-- Split on a delimiter
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

-- | Split the stream on both sides of a separator sequence, dropping the
-- separator.
--
-- For illustration, let's define a function that operates on pure lists:
--
-- @
-- splitOn_ pat xs = S.toList $ FL.splitOn (A.fromList pat) (FL.toList) (S.fromList xs)
-- @
--
-- >>> splitOn_ "" "hello"
-- > ["h","e","l","l","o"]
--
-- >>> splitOn_ "hello" ""
-- > [""]
--
-- >>> splitOn_ "hello" "hello"
-- > ["",""]
--
-- >>> splitOn_ "x" "hello"
-- > ["hello"]
--
-- >>> splitOn_ "h" "hello"
-- > ["","ello"]
--
-- >>> splitOn_ "o" "hello"
-- > ["hell",""]
--
-- >>> splitOn_ "e" "hello"
-- > ["h","llo"]
--
-- >>> splitOn_ "l" "hello"
-- > ["he","","o"]
--
-- >>> splitOn_ "ll" "hello"
-- > ["he","o"]
--
-- 'splitOn' is an inverse of 'intercalate'. The following law always holds:
--
-- > intercalate . splitOn == id
--
-- The following law holds when the separator is non-empty and contains none of
-- the elements present in the input lists:
--
-- > splitOn . intercalate == id
--
-- The following law always holds:
--
-- > concat . splitOn . intercalate == concat
--
-- @since 0.7.0
{-# INLINE splitOn #-}
splitOn
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOn patt f m = D.fromStreamD $ D.splitOn patt f (D.toStreamD m)

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
{-# INLINE splitSuffixOn #-}
splitSuffixOn
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitSuffixOn patt f m =
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
-- | Like 'splitOn' but splits the separator as well, as an infix token.
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
{-# INLINE splitOn' #-}
splitOn'
    :: (IsStream t, MonadAsync m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOn' patt f m = S.intersperseM (foldl' f (A.read patt)) $ splitOn patt f m

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
{-# INLINE splitSuffixOn' #-}
splitSuffixOn'
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitSuffixOn' patt f m =
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
-- Grouped by order
------------------------------------------------------------------------------

{-
-- Buffer until the next element in sequence arrives. The function argument
-- determines the difference in sequence numbers. This could be useful in
-- implementing sequenced streams, for example, TCP reassembly.
{-# INLINE foldOrderedBy #-}
foldOrderedBy
    :: (IsStream t, Monad m)
    => (forall n. Monad n => Fold n a b)
    -> (a -> a -> Int)
    -> t m a
    -> t m b
foldOrderedBy = undefined
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
-- number of consumers?
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

{-
-- | Send one item to each fold in a round-robin fashion. This is the consumer
-- side dual of producer side 'mergeN' operation.
--
-- partitionN :: Monad m => [Fold m a b] -> Fold m a [b]
-- partitionN fs = Fold step begin done

-- | Demultiplex an input element into a number of typed variants. We want to
-- statically restrict the target values within a set of predefined types, an
-- enumeration of a GADT. We also want to make sure that the Map contains only
-- those types and the full set of those types.  Instead of Map it should
-- probably be a lookup-table using an array and not in GC memory.
--
-- This is the consumer side dual of the producer side 'mux' operation.
--
-- demux :: (Monad m, Ord k)
--     => (a -> k) -> Map k (Fold m a b) -> Fold m a (Map k b)
-- demux f kv = Fold step begin done
-}

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
demux_ :: (Monad m, Ord k) => Map k (Fold m a ()) -> Fold m (k, a) ()
demux_ kv = Fold step initial extract

    where

    initial = return ()
    step () (k, a) =
        -- XXX should we raise an exception in Nothing case?
        -- Ideally we should enforce that it is a total map over k so that look
        -- up never fails
        case Map.lookup k kv of
            Nothing -> return ()
            Just (Fold step' initial' extract') ->
                initial' >>= \x -> step' x a >>= extract'
    extract = return

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
classify :: (Monad m, Ord k) => Fold m a b -> Fold m (k, a) (Map k b)
classify (Fold step initial extract) = Fold step' initial' extract'

    where

    initial' = return Map.empty
    step' kv (k, a) =
        case Map.lookup k kv of
            Nothing -> do
                x <- initial
                r <- step x a
                return $ Map.insert k r kv
            Just x -> do
                r <- step x a
                return $ Map.insert k r kv
    extract' = mapM extract

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
--                           |-------Fold a x--------|
-- -----Stream m x----(a,b)--|                       |----m (x,y)
--                           |-------Fold b y--------|
--
-- @
--
-- This is the consumer side dual of the producer side 'zip' operation.
--
-- @since 0.7.0
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
{-# INLINABLE duplicate #-}
duplicate :: Applicative m => Fold m a b -> Fold m a (Fold m a b)
duplicate (Fold step begin done) =
    Fold step begin (\x -> pure (Fold step (pure x) done))

{-
-- All the stream flattening transformations can also be applied to a fold
-- input stream.

-- | This can be used to apply all the stream generation operations on folds.
lconcatMap ::(IsStream t, Monad m) => (a -> t m b)
    -> Fold m b c
    -> Fold m a c
lconcatMap s f1 f2 = undefined

-- All the grouping transformation that we apply to a stream can also be
-- applied to a fold input stream.

-- | Group the input elements of a fold input stream by some criterion and fold
-- each group using a given fold before its fed to the fold.
--
-- For example, we can copy and distribute a stream to multiple folds and then
-- in each fold we can group the input differently e.g. by one second, one
-- minute and one hour windows respectively and fold each resulting stream of
-- folds.
--
-- @
--
-- -----Fold m a b----|-Fold n a c-|-Fold n a c-|-...-|----Fold m a c
--
-- @
lgroupsOf :: Int -> Fold m a b -> Fold m b c -> Fold m a c
lgroupsOf n f1 f2 = undefined
-}
