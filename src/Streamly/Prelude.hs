{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Streamly.Prelude
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Prelude
    (
    -- * Construction
      nil
    , cons
    , (.:)
    , unfoldr
    , unfoldrM
    , each

    -- * Elimination
    -- ** General Folds
    , foldr
    , foldrM
    , scan
    , foldl
    , foldlM
    , uncons

    -- ** Special Folds
    , toList
    , all
    , any
    , head
    , last
    , length
    , elem
    , notElem
    , maximum
    , minimum
    , sum
    , product

    -- * Filtering
    , filter
    , take
    , takeWhile
    , drop
    , dropWhile

    -- * Transformation
    , mapM
    , mapM_
    , sequence
    , replicateM

    -- * Zipping
    , zipWith
    , zipWithM
    , zipAsyncWith
    , zipAsyncWithM

    -- * IO
    , fromHandle
    , toHandle

    )
where

import           Control.Monad (void)
import           Control.Monad.IO.Class      (MonadIO(..))
import           Data.Semigroup              (Semigroup(..))
import           Prelude hiding              (filter, drop, dropWhile, take,
                                              takeWhile, zipWith, foldr, foldl,
                                              mapM, mapM_, sequence, all, any,
                                              sum, product, elem, notElem,
                                              maximum, minimum, head, last,
                                              length)
import qualified Prelude
import qualified System.IO as IO

import           Streamly.Core
import           Streamly.Streams
------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

-- | Build a Stream by unfolding pure steps starting from a seed.
unfoldr :: Streaming t => (b -> Maybe (a, b)) -> b -> t m a
unfoldr step = fromStream . go
    where
    go s = Stream $ \_ stp yld ->
        case step s of
            Nothing -> stp
            Just (a, b) -> yld a (Just (go b))

-- | Build a Stream by unfolding monadic steps starting from a seed.
unfoldrM :: (Streaming t, Monad m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM step = fromStream . go
    where
    go s = Stream $ \_ stp yld -> do
        mayb <- step s
        case mayb of
            Nothing -> stp
            Just (a, b) -> yld a (Just (go b))

-- XXX need eachInterleaved, eachAsync, eachParallel
-- | Same as @foldWith (<>)@ but more efficient.
{-# INLINE each #-}
each :: (Streaming t, Foldable f) => f a -> t m a
each = Prelude.foldr cons nil

-- | Read lines from an IO Handle into a stream of Strings.
fromHandle :: (Streaming t, MonadIO m) => IO.Handle -> t m String
fromHandle h = fromStream go
  where
  go = Stream $ \_ stp yld -> do
        eof <- liftIO $ IO.hIsEOF h
        if eof
        then stp
        else do
            str <- liftIO $ IO.hGetLine h
            yld str (Just go)

------------------------------------------------------------------------------
-- Elimination
------------------------------------------------------------------------------

-- Parallel variants of folds?

-- | Right fold.
foldr :: (Streaming t, Monad m) => (a -> b -> b) -> b -> t m a -> m b
foldr step acc m = go (toStream m)
    where
    go m1 =
        let stop = return acc
            yield a Nothing  = return (step a acc)
            yield a (Just x) = go x >>= \b -> return (step a b)
        in (runStream m1) Nothing stop yield

-- | Right fold with a monadic step function.  See 'toList' for an example use.
{-# INLINE foldrM #-}
foldrM :: Streaming t => (a -> m b -> m b) -> m b -> t m a -> m b
foldrM step acc m = go (toStream m)
    where
    go m1 =
        let stop = acc
            yield a Nothing  = step a acc
            yield a (Just x) = step a (go x)
        in (runStream m1) Nothing stop yield

-- | Scan left. A strict left fold which accumulates the result of its reduction steps inside a stream, from left.
{-# INLINE scan #-}
scan :: Streaming t => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scan step begin done m = cons (done begin) $ fromStream $ go (toStream m) begin
    where
    go m1 !acc = Stream $ \_ stp yld ->
        let stop = stp
            yield a Nothing = yld (done $ step acc a) Nothing
            yield a (Just x) =
                let s = step acc a
                in yld (done s) (Just (go x s))
        in runStream m1 Nothing stop yield

-- | Strict left fold. This is typed to work with the foldl package. To use
-- it normally just pass 'id' as the third argument.
{-# INLINE foldl #-}
foldl :: (Streaming t, Monad m)
    => (x -> a -> x) -> x -> (x -> b) -> t m a -> m b
foldl step begin done m = get $ go (toStream m) begin
    where
    {-# NOINLINE get #-}
    get m1 =
        let yield a Nothing  = return $ done a
            yield _ _ = undefined
         in (runStream m1) Nothing undefined yield

    -- Note, this can be implemented by making a recursive call to "go",
    -- however that is more expensive because of unnecessary recursion
    -- that cannot be tail call optimized. Unfolding recursion explicitly via
    -- continuations is much more efficient.
    go m1 !acc = Stream $ \_ _ yld ->
        let stop = yld acc Nothing
            yield a r =
                let s = step acc a
                in case r of
                    Nothing -> yld s Nothing
                    Just x -> (runStream (go x s)) Nothing undefined yld
        in (runStream m1) Nothing stop yield

-- XXX replace the recursive "go" with explicit continuations.
-- | Strict left fold, with monadic step function. This is typed to work
-- with the foldl package. To use directly pass 'id' as the third argument.
foldlM :: (Streaming t, Monad m)
    => (x -> a -> m x) -> m x -> (x -> m b) -> t m a -> m b
foldlM step begin done m = go begin (toStream m)
    where
    go !acc m1 =
        let stop = acc >>= done
            yield a Nothing  = acc >>= \b -> step b a >>= done
            yield a (Just x) = acc >>= \b -> go (step b a) x
         in (runStream m1) Nothing stop yield

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns 'Just (a, ma)', where 'a' is
-- the head of the stream and 'ma' its tail.
uncons :: (Streaming t, Monad m) => t m a -> m (Maybe (a, t m a))
uncons m =
    let stop = return Nothing
        yield a Nothing  = return (Just (a, nil))
        yield a (Just x) = return (Just (a, fromStream x))
    in (runStream (toStream m)) Nothing stop yield

-- | Write a stream of Strings to an IO Handle.
toHandle :: (Streaming t, MonadIO m) => IO.Handle -> t m String -> m ()
toHandle h m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            yield a Nothing  = liftIO (IO.hPutStrLn h a)
            yield a (Just x) = liftIO (IO.hPutStrLn h a) >> go x
        in (runStream m1) Nothing stop yield

------------------------------------------------------------------------------
-- Special folds
------------------------------------------------------------------------------

-- | Convert a stream into a list in the underlying monad.
{-# INLINABLE toList #-}
toList :: (Streaming t, Monad m) => t m a -> m [a]
toList = foldrM (\a xs -> fmap (a :) xs) (return [])

-- | Take first 'n' elements from the stream and discard the rest.
{-# INLINE take #-}
take :: Streaming t => Int -> t m a -> t m a
take n m = fromStream $ go n (toStream m)
    where
    go n1 m1 = Stream $ \ctx stp yld ->
        let yield a Nothing  = yld a Nothing
            yield a (Just x) = yld a (Just (go (n1 - 1) x))
        in if n1 <= 0 then stp else (runStream m1) ctx stp yield

-- | Include only those elements that pass a predicate.
{-# INLINE filter #-}
filter :: Streaming t => (a -> Bool) -> t m a -> t m a
filter p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \ctx stp yld ->
        let yield a Nothing  | p a       = yld a Nothing
                             | otherwise = stp
            yield a (Just x) | p a       = yld a (Just (go x))
                             | otherwise = (runStream x) ctx stp yield
         in (runStream m1) ctx stp yield

-- | End the stream as soon as the predicate fails on an element.
{-# INLINE takeWhile #-}
takeWhile :: Streaming t => (a -> Bool) -> t m a -> t m a
takeWhile p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \ctx stp yld ->
        let yield a Nothing  | p a       = yld a Nothing
                             | otherwise = stp
            yield a (Just x) | p a       = yld a (Just (go x))
                             | otherwise = stp
         in (runStream m1) ctx stp yield

-- | Discard first 'n' elements from the stream and take the rest.
drop :: Streaming t => Int -> t m a -> t m a
drop n m = fromStream $ go n (toStream m)
    where
    go n1 m1 = Stream $ \ctx stp yld ->
        let yield _ Nothing  = stp
            yield _ (Just x) = (runStream $ go (n1 - 1) x) ctx stp yld
        -- Somehow "<=" check performs better than a ">"
        in if n1 <= 0
           then (runStream m1) ctx stp yld
           else (runStream m1) ctx stp yield

-- | Drop elements in the stream as long as the predicate succeeds and then
-- take the rest of the stream.
{-# INLINE dropWhile #-}
dropWhile :: Streaming t => (a -> Bool) -> t m a -> t m a
dropWhile p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \ctx stp yld ->
        let yield a Nothing  | p a       = stp
                             | otherwise = yld a Nothing
            yield a (Just x) | p a       = (runStream x) ctx stp yield
                             | otherwise = yld a (Just x)
         in (runStream m1) ctx stp yield

-- | Determine whether all elements of a stream satisfy a predicate.
all :: (Streaming t, Monad m) => (a -> Bool) -> t m a -> m Bool
all p m = go (toStream m)
    where
    go m1 =
        let yield a Nothing  | p a       = return True
                             | otherwise = return False
            yield a (Just x) | p a       = go x
                             | otherwise = return False
         in (runStream m1) Nothing (return True) yield

-- | Determine whether any of the elements of a stream satisfy a predicate.
any :: (Streaming t, Monad m) => (a -> Bool) -> t m a -> m Bool
any p m = go (toStream m)
    where
    go m1 =
        let yield a Nothing  | p a       = return True
                             | otherwise = return False
            yield a (Just x) | p a       = return True
                             | otherwise = go x
         in (runStream m1) Nothing (return False) yield

-- | Determine the sum of all elements of a stream of numbers
sum :: (Streaming t, Monad m, Num a) => t m a -> m a
sum = foldl (+) 0 id

-- | Determine the product of all elements of a stream of numbers
product :: (Streaming t, Monad m, Num a) => t m a -> m a
product = foldl (*) 1 id

-- | Extract the first element of the stream, if any.
head :: (Streaming t, Monad m) => t m a -> m (Maybe a)
head m =
    let stop      = return Nothing
        yield a _ = return (Just a)
    in (runStream (toStream m)) Nothing stop yield

-- | Extract the last element of the stream, if any.
{-# INLINE last #-}
last :: (Streaming t, Monad m) => t m a -> m (Maybe a)
last = foldl (\_ y -> Just y) Nothing id

-- | Determine whether an element is present in the stream.
elem :: (Streaming t, Monad m, Eq a) => a -> t m a -> m Bool
elem e m = go (toStream m)
    where
    go m1 =
        let stop            = return False
            yield a Nothing = return (a == e)
            yield a (Just x) = if a == e then return True else go x
        in (runStream m1) Nothing stop yield

-- | Determine whether an element is not present in the stream.
notElem :: (Streaming t, Monad m, Eq a) => a -> t m a -> m Bool
notElem e m = go (toStream m)
    where
    go m1 =
        let stop            = return True
            yield a Nothing = return (a /= e)
            yield a (Just x) = if a == e then return False else go x
        in (runStream m1) Nothing stop yield

-- | Determine the length of the stream.
length :: (Streaming t, Monad m) => t m a -> m Int
length = foldl (\n _ -> n + 1) 0 id

-- XXX replace the recursive "go" with continuation
-- | Determine the minimum element in a stream.
minimum :: (Streaming t, Monad m, Ord a) => t m a -> m (Maybe a)
minimum m = go Nothing (toStream m)
    where
    go r m1 =
        let stop            = return r
            yield a Nothing = return $ min_ a r
            yield a (Just x) = go (min_ a r) x
        in (runStream m1) Nothing stop yield

    min_ a r = case r of
        Nothing -> Just a
        Just e  -> Just $ min a e

-- XXX replace the recursive "go" with continuation
-- | Determine the maximum element in a stream.
maximum :: (Streaming t, Monad m, Ord a) => t m a -> m (Maybe a)
maximum m = go Nothing (toStream m)
    where
    go r m1 =
        let stop            = return r
            yield a Nothing = return $ max_ a r
            yield a (Just x) = go (max_ a r) x
        in (runStream m1) Nothing stop yield

    max_ a r = case r of
        Nothing -> Just a
        Just e  -> Just $ max a e

------------------------------------------------------------------------------
-- Transformation
------------------------------------------------------------------------------

-- XXX Parallel variants of these? mapMWith et al. sequenceWith.

-- | Replace each element of the stream with the result of a monadic action
-- applied on the element.
{-# INLINE mapM #-}
mapM :: (Streaming t, Monad m) => (a -> m b) -> t m a -> t m b
mapM f m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \_ stp yld ->
        let stop = stp
            yield a Nothing  = f a >>= \b -> yld b Nothing
            yield a (Just x) = f a >>= \b -> yld b (Just (go x))
         in (runStream m1) Nothing stop yield

-- | Apply a monadic action to each element of the stream and discard the
-- output of the action.
mapM_ :: (Streaming t, Monad m) => (a -> m b) -> t m a -> m ()
mapM_ f m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            yield a Nothing  = void (f a)
            yield a (Just x) = f a >> go x
         in (runStream m1) Nothing stop yield

-- | Reduce a stream of monadic actions to a stream of the output of those
-- actions.
sequence :: (Streaming t, Monad m) => t m (m a) -> t m a
sequence m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \_ stp yld ->
        let stop = stp
            yield a Nothing  = a >>= \b -> yld b Nothing
            yield a (Just x) = a >>= \b -> yld b (Just (go x))
         in (runStream m1) Nothing stop yield

replicateM :: (Streaming t, Monad m) => Int -> m a -> t m a
replicateM n m = fromStream $ go n
    where
    go cnt = Stream $ \_ stp yld ->
        if cnt <= 0
        then stp
        else m >>= \a -> yld a (Just $ go (cnt - 1))

------------------------------------------------------------------------------
-- Serially Zipping Streams
------------------------------------------------------------------------------

-- | Zip two streams serially using a monadic zipping function.
zipWithM :: Streaming t => (a -> b -> t m c) -> t m a -> t m b -> t m c
zipWithM f m1 m2 = fromStream $ go (toStream m1) (toStream m2)
    where
    go mx my = Stream $ \_ stp yld -> do
        let merge a ra =
                let yield2 b Nothing   = (runStream (g a b)) Nothing stp yld
                    yield2 b (Just rb) =
                        (runStream (g a b <> go ra rb)) Nothing stp yld
                 in (runStream my) Nothing stp yield2
        let yield1 a Nothing   = merge a snil
            yield1 a (Just ra) = merge a ra
        (runStream mx) Nothing stp yield1
    g a b = toStream $ f a b

------------------------------------------------------------------------------
-- Parallely Zipping Streams
------------------------------------------------------------------------------

-- | Zip two streams asyncly (i.e. both the elements being zipped are generated
-- concurrently) using a monadic zipping function.
zipAsyncWithM :: (Streaming t, MonadAsync m)
    => (a -> b -> t m c) -> t m a -> t m b -> t m c
zipAsyncWithM f m1 m2 = fromStream $ Stream $ \_ stp yld -> do
    ma <- async m1
    mb <- async m2
    (runStream (toStream (zipWithM f ma mb))) Nothing stp yld
