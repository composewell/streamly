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
    , fromFoldable
    , iterate
    , iterateM

    -- * Elimination
    -- ** General Folds
    , foldr
    , foldrM
    , foldl'
    , foldlM'
    , scanl'
    , scanx
    , foldx
    , foldxM
    , uncons

    -- ** Special Folds
    , toList
    , all
    , any
    , head
    , tail
    , last
    , null
    , length
    , elem
    , notElem
    , reverse
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

    -- * Deprecated
    , each
    , scan
    , foldl
    , foldlM
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
                                              tail, length, null, reverse,
                                              iterate)
import qualified Prelude
import qualified System.IO as IO

import qualified Streamly.Core as S
import           Streamly.Core (Stream(Stream))
import           Streamly.Streams

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

-- | Build a Stream by unfolding pure steps starting from a seed.
unfoldr :: IsStream t => (b -> Maybe (a, b)) -> b -> t m a
unfoldr step = fromStream . go
    where
    go s = Stream $ \_ stp yld ->
        case step s of
            Nothing -> stp
            Just (a, b) -> yld a (Just (go b))

-- | Build a Stream by unfolding monadic steps starting from a seed.
unfoldrM :: (IsStream t, Monad m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM step = fromStream . go
    where
    go s = Stream $ \_ stp yld -> do
        mayb <- step s
        case mayb of
            Nothing -> stp
            Just (a, b) -> yld a (Just (go b))

-- | Construct a stream from a 'Foldable' container.
{-# INLINE fromFoldable #-}
fromFoldable :: (IsStream t, Foldable f) => f a -> t m a
fromFoldable = Prelude.foldr cons nil

-- | Same as 'fromFoldable'.
{-# DEPRECATED each "Please use fromFoldable instead." #-}
{-# INLINE each #-}
each :: (IsStream t, Foldable f) => f a -> t m a
each = fromFoldable

-- | Iterate a pure function from a seed value, streaming the results forever
iterate :: IsStream t => (a -> a) -> a -> t m a
iterate step = fromStream . go
    where
    go s = S.cons s (Just (go (step s)))

-- | Iterate a monadic function from a seed value, streaming the results forever
iterateM :: (IsStream t, Monad m) => (a -> m a) -> a -> t m a
iterateM step = fromStream . go
    where
    go s = Stream $ \_ _ yld -> do
       a <- step s
       yld s (Just (go a))

-- | Read lines from an IO Handle into a stream of Strings.
fromHandle :: (IsStream t, MonadIO m) => IO.Handle -> t m String
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

-- | Lazy right associative fold. For example, to fold a stream into a list:
--
-- @
-- >> runIdentity $ foldr (:) [] (serially $ fromFoldable [1,2,3])
-- [1,2,3]
-- @
foldr :: Monad m => (a -> b -> b) -> b -> SerialT m a -> m b
foldr step acc m = go (toStream m)
    where
    go m1 =
        let stop = return acc
            yield a Nothing  = return (step a acc)
            yield a (Just x) = go x >>= \b -> return (step a b)
        in (S.runStream m1) Nothing stop yield

-- | Lazy right fold with a monadic step function. For example, to fold a
-- stream into a list:
--
-- @
-- >> runIdentity $ foldrM (\\x xs -> return (x : xs)) [] (serially $ fromFoldable [1,2,3])
-- [1,2,3]
-- @
{-# INLINE foldrM #-}
foldrM :: Monad m => (a -> b -> m b) -> b -> SerialT m a -> m b
foldrM step acc m = go (toStream m)
    where
    go m1 =
        let stop = return acc
            yield a Nothing  = step a acc
            yield a (Just x) = (go x) >>= (step a)
        in (S.runStream m1) Nothing stop yield

-- | Strict left scan with an extraction function. Like 'scanl'', but applies a
-- user supplied extraction function (the third argument) at each step. This is
-- designed to work with the @foldl@ library. The suffix @x@ is a mnemonic for
-- extraction.
{-# INLINE scanx #-}
scanx :: IsStream t => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scanx step begin done m = cons (done begin) $ fromStream $ go (toStream m) begin
    where
    go m1 !acc = Stream $ \_ stp yld ->
        let stop = stp
            yield a Nothing = yld (done $ step acc a) Nothing
            yield a (Just x) =
                let s = step acc a
                in yld (done s) (Just (go x s))
        in S.runStream m1 Nothing stop yield

{-# DEPRECATED scan "Please use scanx instead." #-}
scan :: IsStream t => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scan = scanx

-- | Strict left scan. Like 'foldl'', but returns the folded value at each
-- step, generating a stream of all intermediate fold results. The first
-- element of the stream is the user supplied initial value, and the last
-- element of the stream is the same as the result of 'foldl''.
{-# INLINE scanl' #-}
scanl' :: IsStream t => (b -> a -> b) -> b -> t m a -> t m b
scanl' step begin m = scanx step begin id m

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
{-# INLINE foldx #-}
foldx :: Monad m => (x -> a -> x) -> x -> (x -> b) -> SerialT m a -> m b
foldx step begin done m = get $ go (toStream m) begin
    where
    {-# NOINLINE get #-}
    get m1 =
        let yield a Nothing  = return $ done a
            yield _ _ = undefined
         in (S.runStream m1) Nothing undefined yield

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
                    Just x -> (S.runStream (go x s)) Nothing undefined yld
        in (S.runStream m1) Nothing stop yield

{-# DEPRECATED foldl "Please use foldx instead." #-}
foldl :: Monad m => (x -> a -> x) -> x -> (x -> b) -> SerialT m a -> m b
foldl = foldx

-- | Strict left associative fold.
{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> SerialT m a -> m b
foldl' step begin m = foldx step begin id m

-- XXX replace the recursive "go" with explicit continuations.
-- | Like 'foldx', but with a monadic step function.
foldxM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> SerialT m a -> m b
foldxM step begin done m = go begin (toStream m)
    where
    go !acc m1 =
        let stop = acc >>= done
            yield a Nothing  = acc >>= \b -> step b a >>= done
            yield a (Just x) = acc >>= \b -> go (step b a) x
         in (S.runStream m1) Nothing stop yield

{-# DEPRECATED foldlM "Please use foldxM instead." #-}
foldlM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> SerialT m a -> m b
foldlM = foldxM

-- | Like 'foldl'' but with a monadic step function.
foldlM' :: Monad m => (b -> a -> m b) -> b -> SerialT m a -> m b
foldlM' step begin m = foldxM step (return begin) return m

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns 'Just (a, ma)', where 'a' is
-- the head of the stream and 'ma' its tail.
uncons :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (a, t m a))
uncons m =
    let stop = return Nothing
        yield a Nothing  = return (Just (a, nil))
        yield a (Just x) = return (Just (a, fromStream x))
    in (S.runStream (toStream m)) Nothing stop yield

-- | Write a stream of Strings to an IO Handle.
toHandle :: MonadIO m => IO.Handle -> SerialT m String -> m ()
toHandle h m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            yield a Nothing  = liftIO (IO.hPutStrLn h a)
            yield a (Just x) = liftIO (IO.hPutStrLn h a) >> go x
        in (S.runStream m1) Nothing stop yield

------------------------------------------------------------------------------
-- Special folds
------------------------------------------------------------------------------

-- | Convert a stream into a list in the underlying monad.
{-# INLINABLE toList #-}
toList :: Monad m => SerialT m a -> m [a]
toList = foldrM (\a xs -> return (a : xs)) []

-- | Take first 'n' elements from the stream and discard the rest.
{-# INLINE take #-}
take :: IsStream t => Int -> t m a -> t m a
take n m = fromStream $ go n (toStream m)
    where
    go n1 m1 = Stream $ \ctx stp yld ->
        let yield a Nothing  = yld a Nothing
            yield a (Just x) = yld a (Just (go (n1 - 1) x))
        in if n1 <= 0 then stp else (S.runStream m1) ctx stp yield

-- | Include only those elements that pass a predicate.
{-# INLINE filter #-}
filter :: IsStream t => (a -> Bool) -> t m a -> t m a
filter p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \ctx stp yld ->
        let yield a Nothing  | p a       = yld a Nothing
                             | otherwise = stp
            yield a (Just x) | p a       = yld a (Just (go x))
                             | otherwise = (S.runStream x) ctx stp yield
         in (S.runStream m1) ctx stp yield

-- | End the stream as soon as the predicate fails on an element.
{-# INLINE takeWhile #-}
takeWhile :: IsStream t => (a -> Bool) -> t m a -> t m a
takeWhile p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \ctx stp yld ->
        let yield a Nothing  | p a       = yld a Nothing
                             | otherwise = stp
            yield a (Just x) | p a       = yld a (Just (go x))
                             | otherwise = stp
         in (S.runStream m1) ctx stp yield

-- | Discard first 'n' elements from the stream and take the rest.
drop :: IsStream t => Int -> t m a -> t m a
drop n m = fromStream $ go n (toStream m)
    where
    go n1 m1 = Stream $ \ctx stp yld ->
        let yield _ Nothing  = stp
            yield _ (Just x) = (S.runStream $ go (n1 - 1) x) ctx stp yld
        -- Somehow "<=" check performs better than a ">"
        in if n1 <= 0
           then (S.runStream m1) ctx stp yld
           else (S.runStream m1) ctx stp yield

-- | Drop elements in the stream as long as the predicate succeeds and then
-- take the rest of the stream.
{-# INLINE dropWhile #-}
dropWhile :: IsStream t => (a -> Bool) -> t m a -> t m a
dropWhile p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \ctx stp yld ->
        let yield a Nothing  | p a       = stp
                             | otherwise = yld a Nothing
            yield a (Just x) | p a       = (S.runStream x) ctx stp yield
                             | otherwise = yld a (Just x)
         in (S.runStream m1) ctx stp yield

-- | Determine whether all elements of a stream satisfy a predicate.
all :: Monad m => (a -> Bool) -> SerialT m a -> m Bool
all p m = go (toStream m)
    where
    go m1 =
        let yield a Nothing  | p a       = return True
                             | otherwise = return False
            yield a (Just x) | p a       = go x
                             | otherwise = return False
         in (S.runStream m1) Nothing (return True) yield

-- | Determine whether any of the elements of a stream satisfy a predicate.
any :: Monad m => (a -> Bool) -> SerialT m a -> m Bool
any p m = go (toStream m)
    where
    go m1 =
        let yield a Nothing  | p a       = return True
                             | otherwise = return False
            yield a (Just x) | p a       = return True
                             | otherwise = go x
         in (S.runStream m1) Nothing (return False) yield

-- | Determine the sum of all elements of a stream of numbers
sum :: (Monad m, Num a) => SerialT m a -> m a
sum = foldl (+) 0 id

-- | Determine the product of all elements of a stream of numbers
product :: (Monad m, Num a) => SerialT m a -> m a
product = foldl (*) 1 id

-- | Extract the first element of the stream, if any.
head :: Monad m => SerialT m a -> m (Maybe a)
head m =
    let stop      = return Nothing
        yield a _ = return (Just a)
    in (S.runStream (toStream m)) Nothing stop yield

-- | Extract all but the first element of the stream, if any.
tail :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (t m a))
tail m =
    let stop             = return Nothing
        yield _ Nothing  = return $ Just nil
        yield _ (Just t) = return $ Just $ fromStream t
    in (S.runStream (toStream m)) Nothing stop yield

-- | Extract the last element of the stream, if any.
{-# INLINE last #-}
last :: Monad m => SerialT m a -> m (Maybe a)
last = foldl (\_ y -> Just y) Nothing id

-- | Determine whether the stream is empty.
null :: Monad m => SerialT m a -> m Bool
null m =
    let stop      = return True
        yield _ _ = return False
    in (S.runStream (toStream m)) Nothing stop yield

-- | Determine whether an element is present in the stream.
elem :: (Monad m, Eq a) => a -> SerialT m a -> m Bool
elem e m = go (toStream m)
    where
    go m1 =
        let stop            = return False
            yield a Nothing = return (a == e)
            yield a (Just x) = if a == e then return True else go x
        in (S.runStream m1) Nothing stop yield

-- | Determine whether an element is not present in the stream.
notElem :: (Monad m, Eq a) => a -> SerialT m a -> m Bool
notElem e m = go (toStream m)
    where
    go m1 =
        let stop            = return True
            yield a Nothing = return (a /= e)
            yield a (Just x) = if a == e then return False else go x
        in (S.runStream m1) Nothing stop yield

-- | Determine the length of the stream.
length :: Monad m => SerialT m a -> m Int
length = foldl (\n _ -> n + 1) 0 id

-- | Returns the elements of the stream in reverse order.
-- The stream must be finite.
reverse :: (IsStream t) => t m a -> t m a
reverse m = fromStream $ go Nothing (toStream m)
    where
    go rev rest = Stream $ \svr stp yld ->
        let stop = case rev of
                Nothing ->  stp
                Just str -> S.runStream str svr stp yld
            yield a Nothing  = S.runStream (a `S.cons` rev) svr stp yld
            yield a (Just x) = S.runStream (go (Just $ a `S.cons` rev) x) svr stp yld
         in S.runStream rest svr stop yield

-- XXX replace the recursive "go" with continuation
-- | Determine the minimum element in a stream.
minimum :: (Monad m, Ord a) => SerialT m a -> m (Maybe a)
minimum m = go Nothing (toStream m)
    where
    go r m1 =
        let stop            = return r
            yield a Nothing = return $ min_ a r
            yield a (Just x) = go (min_ a r) x
        in (S.runStream m1) Nothing stop yield

    min_ a r = case r of
        Nothing -> Just a
        Just e  -> Just $ min a e

-- XXX replace the recursive "go" with continuation
-- | Determine the maximum element in a stream.
maximum :: (Monad m, Ord a) => SerialT m a -> m (Maybe a)
maximum m = go Nothing (toStream m)
    where
    go r m1 =
        let stop            = return r
            yield a Nothing = return $ max_ a r
            yield a (Just x) = go (max_ a r) x
        in (S.runStream m1) Nothing stop yield

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
mapM :: (IsStream t, Monad m) => (a -> m b) -> t m a -> t m b
mapM f m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \_ stp yld ->
        let stop = stp
            yield a Nothing  = f a >>= \b -> yld b Nothing
            yield a (Just x) = f a >>= \b -> yld b (Just (go x))
         in (S.runStream m1) Nothing stop yield

-- | Apply a monadic action to each element of the stream and discard the
-- output of the action.
mapM_ :: Monad m => (a -> m b) -> SerialT m a -> m ()
mapM_ f m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            yield a Nothing  = void (f a)
            yield a (Just x) = f a >> go x
         in (S.runStream m1) Nothing stop yield

-- | Reduce a stream of monadic actions to a stream of the output of those
-- actions.
sequence :: (IsStream t, Monad m) => t m (m a) -> t m a
sequence m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \_ stp yld ->
        let stop = stp
            yield a Nothing  = a >>= \b -> yld b Nothing
            yield a (Just x) = a >>= \b -> yld b (Just (go x))
         in (S.runStream m1) Nothing stop yield

-- | Generate a stream by performing an action @n@ times.
replicateM :: (IsStream t, Monad m) => Int -> m a -> t m a
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
zipWithM :: IsStream t => (a -> b -> t m c) -> t m a -> t m b -> t m c
zipWithM f m1 m2 = fromStream $ go (toStream m1) (toStream m2)
    where
    go mx my = Stream $ \_ stp yld -> do
        let merge a ra =
                let yield2 b Nothing   = (S.runStream (g a b)) Nothing stp yld
                    yield2 b (Just rb) =
                        (S.runStream (g a b <> go ra rb)) Nothing stp yld
                 in (S.runStream my) Nothing stp yield2
        let yield1 a Nothing   = merge a S.nil
            yield1 a (Just ra) = merge a ra
        (S.runStream mx) Nothing stp yield1
    g a b = toStream $ f a b

------------------------------------------------------------------------------
-- Parallely Zipping Streams
------------------------------------------------------------------------------

-- | Zip two streams asyncly (i.e. both the elements being zipped are generated
-- concurrently) using a monadic zipping function.
zipAsyncWithM :: (IsStream t, MonadAsync m)
    => (a -> b -> t m c) -> t m a -> t m b -> t m c
zipAsyncWithM f m1 m2 = fromStream $ Stream $ \_ stp yld -> do
    ma <- async m1
    mb <- async m2
    (S.runStream (toStream (zipWithM f ma mb))) Nothing stp yld
