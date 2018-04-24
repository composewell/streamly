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
    , zipParallelWith
    , zipParallelWithM

    -- * IO
    , fromHandle
    , toHandle

    -- * Deprecated
    , each
    , scan
    , foldl
    , foldlM
    , zipAsyncWith
    , zipAsyncWithM
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
    go s = Stream $ \_ stp _ yld ->
        case step s of
            Nothing -> stp
            Just (a, b) -> yld a (go b)

-- | Build a Stream by unfolding monadic steps starting from a seed.
unfoldrM :: (IsStream t, Monad m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM step = fromStream . go
    where
    go s = Stream $ \_ stp _ yld -> do
        mayb <- step s
        case mayb of
            Nothing -> stp
            Just (a, b) -> yld a (go b)

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
    go s = S.cons s (go (step s))

-- | Iterate a monadic function from a seed value, streaming the results forever
iterateM :: (IsStream t, Monad m) => (a -> m a) -> a -> t m a
iterateM step = fromStream . go
    where
    go s = Stream $ \_ _ _ yld -> do
       a <- step s
       yld s (go a)

-- | Read lines from an IO Handle into a stream of Strings.
fromHandle :: (IsStream t, MonadIO m) => IO.Handle -> t m String
fromHandle h = fromStream go
  where
  go = Stream $ \_ stp _ yld -> do
        eof <- liftIO $ IO.hIsEOF h
        if eof
        then stp
        else do
            str <- liftIO $ IO.hGetLine h
            yld str go

------------------------------------------------------------------------------
-- Elimination
------------------------------------------------------------------------------

-- | Lazy right associative fold. For example, to fold a stream into a list:
--
-- @
-- >> runIdentity $ foldr (:) [] (serially $ fromFoldable [1,2,3])
-- [1,2,3]
-- @
foldr :: Monad m => (a -> b -> b) -> b -> StreamT m a -> m b
foldr step acc m = go (toStream m)
    where
    go m1 =
        let stop = return acc
            single a = return (step a acc)
            yield a r = go r >>= \b -> return (step a b)
        in (S.runStream m1) Nothing stop single yield

-- | Lazy right fold with a monadic step function. For example, to fold a
-- stream into a list:
--
-- @
-- >> runIdentity $ foldrM (\\x xs -> return (x : xs)) [] (serially $ fromFoldable [1,2,3])
-- [1,2,3]
-- @
{-# INLINE foldrM #-}
foldrM :: Monad m => (a -> b -> m b) -> b -> StreamT m a -> m b
foldrM step acc m = go (toStream m)
    where
    go m1 =
        let stop = return acc
            single a = step a acc
            yield a r = go r >>= step a
        in (S.runStream m1) Nothing stop single yield

-- | Strict left scan with an extraction function. Like 'scanl'', but applies a
-- user supplied extraction function (the third argument) at each step. This is
-- designed to work with the @foldl@ library. The suffix @x@ is a mnemonic for
-- extraction.
{-# INLINE scanx #-}
scanx :: IsStream t => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scanx step begin done m = cons (done begin) $ fromStream $ go (toStream m) begin
    where
    go m1 !acc = Stream $ \_ stp sng yld ->
        let single a = sng (done $ step acc a)
            yield a r =
                let s = step acc a
                in yld (done s) (go r s)
        in S.runStream m1 Nothing stp single yield

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
foldx :: Monad m => (x -> a -> x) -> x -> (x -> b) -> StreamT m a -> m b
foldx step begin done m = get $ go (toStream m) begin
    where
    {-# NOINLINE get #-}
    get m1 =
        let single = return . done
         in (S.runStream m1) Nothing undefined single undefined

    -- Note, this can be implemented by making a recursive call to "go",
    -- however that is more expensive because of unnecessary recursion
    -- that cannot be tail call optimized. Unfolding recursion explicitly via
    -- continuations is much more efficient.
    go m1 !acc = Stream $ \_ _ sng yld ->
        let stop = sng acc
            single a = sng $ step acc a
            yield a r =
                let stream = go r (step acc a)
                in (S.runStream stream) Nothing undefined sng yld
        in (S.runStream m1) Nothing stop single yield

{-# DEPRECATED foldl "Please use foldx instead." #-}
foldl :: Monad m => (x -> a -> x) -> x -> (x -> b) -> StreamT m a -> m b
foldl = foldx

-- | Strict left associative fold.
{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> StreamT m a -> m b
foldl' step begin m = foldx step begin id m

-- XXX replace the recursive "go" with explicit continuations.
-- | Like 'foldx', but with a monadic step function.
foldxM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> StreamT m a -> m b
foldxM step begin done m = go begin (toStream m)
    where
    go !acc m1 =
        let stop = acc >>= done
            single a = acc >>= \b -> step b a >>= done
            yield a r = acc >>= \b -> go (step b a) r
         in (S.runStream m1) Nothing stop single yield

{-# DEPRECATED foldlM "Please use foldxM instead." #-}
foldlM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> StreamT m a -> m b
foldlM = foldxM

-- | Like 'foldl'' but with a monadic step function.
foldlM' :: Monad m => (b -> a -> m b) -> b -> StreamT m a -> m b
foldlM' step begin m = foldxM step (return begin) return m

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns 'Just (a, ma)', where 'a' is
-- the head of the stream and 'ma' its tail.
uncons :: (IsStream t, Monad m) => StreamT m a -> m (Maybe (a, t m a))
uncons m =
    let stop = return Nothing
        single a = return (Just (a, nil))
        yield a r = return (Just (a, fromStream r))
    in (S.runStream (toStream m)) Nothing stop single yield

-- | Write a stream of Strings to an IO Handle.
toHandle :: MonadIO m => IO.Handle -> StreamT m String -> m ()
toHandle h m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            single a = liftIO (IO.hPutStrLn h a)
            yield a r = liftIO (IO.hPutStrLn h a) >> go r
        in (S.runStream m1) Nothing stop single yield

------------------------------------------------------------------------------
-- Special folds
------------------------------------------------------------------------------

-- | Convert a stream into a list in the underlying monad.
{-# INLINABLE toList #-}
toList :: Monad m => StreamT m a -> m [a]
toList = foldrM (\a xs -> return (a : xs)) []

-- | Take first 'n' elements from the stream and discard the rest.
{-# INLINE take #-}
take :: IsStream t => Int -> t m a -> t m a
take n m = fromStream $ go n (toStream m)
    where
    go n1 m1 = Stream $ \ctx stp sng yld ->
        let yield a r = yld a (go (n1 - 1) r)
        in if n1 <= 0 then stp else (S.runStream m1) ctx stp sng yield

-- | Include only those elements that pass a predicate.
{-# INLINE filter #-}
filter :: IsStream t => (a -> Bool) -> t m a -> t m a
filter p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \ctx stp sng yld ->
        let single a  | p a       = sng a
                      | otherwise = stp
            yield a r | p a       = yld a (go r)
                      | otherwise = (S.runStream r) ctx stp single yield
         in (S.runStream m1) ctx stp single yield

-- | End the stream as soon as the predicate fails on an element.
{-# INLINE takeWhile #-}
takeWhile :: IsStream t => (a -> Bool) -> t m a -> t m a
takeWhile p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \ctx stp sng yld ->
        let single a  | p a       = sng a
                      | otherwise = stp
            yield a r | p a       = yld a (go r)
                      | otherwise = stp
         in (S.runStream m1) ctx stp single yield

-- | Discard first 'n' elements from the stream and take the rest.
drop :: IsStream t => Int -> t m a -> t m a
drop n m = fromStream $ go n (toStream m)
    where
    go n1 m1 = Stream $ \ctx stp sng yld ->
        let single _ = stp
            yield _ r = (S.runStream $ go (n1 - 1) r) ctx stp sng yld
        -- Somehow "<=" check performs better than a ">"
        in if n1 <= 0
           then (S.runStream m1) ctx stp sng yld
           else (S.runStream m1) ctx stp single yield

-- | Drop elements in the stream as long as the predicate succeeds and then
-- take the rest of the stream.
{-# INLINE dropWhile #-}
dropWhile :: IsStream t => (a -> Bool) -> t m a -> t m a
dropWhile p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \ctx stp sng yld ->
        let single a  | p a       = stp
                      | otherwise = sng a
            yield a r | p a       = (S.runStream r) ctx stp single yield
                      | otherwise = yld a r
         in (S.runStream m1) ctx stp single yield

-- | Determine whether all elements of a stream satisfy a predicate.
all :: Monad m => (a -> Bool) -> StreamT m a -> m Bool
all p m = go (toStream m)
    where
    go m1 =
        let single a  | p a       = return True
                      | otherwise = return False
            yield a r | p a       = go r
                      | otherwise = return False
         in (S.runStream m1) Nothing (return True) single yield

-- | Determine whether any of the elements of a stream satisfy a predicate.
any :: Monad m => (a -> Bool) -> StreamT m a -> m Bool
any p m = go (toStream m)
    where
    go m1 =
        let single a  | p a       = return True
                      | otherwise = return False
            yield a r | p a       = return True
                      | otherwise = go r
         in (S.runStream m1) Nothing (return False) single yield

-- | Determine the sum of all elements of a stream of numbers
sum :: (Monad m, Num a) => StreamT m a -> m a
sum = foldl (+) 0 id

-- | Determine the product of all elements of a stream of numbers
product :: (Monad m, Num a) => StreamT m a -> m a
product = foldl (*) 1 id

-- | Extract the first element of the stream, if any.
head :: Monad m => StreamT m a -> m (Maybe a)
head m =
    let stop      = return Nothing
        single a  = return (Just a)
        yield a _ = return (Just a)
    in (S.runStream (toStream m)) Nothing stop single yield

-- | Extract all but the first element of the stream, if any.
tail :: (IsStream t, Monad m) => StreamT m a -> m (Maybe (t m a))
tail m =
    let stop      = return Nothing
        single _  = return $ Just nil
        yield _ r = return $ Just $ fromStream r
    in (S.runStream (toStream m)) Nothing stop single yield

-- | Extract the last element of the stream, if any.
{-# INLINE last #-}
last :: Monad m => StreamT m a -> m (Maybe a)
last = foldl (\_ y -> Just y) Nothing id

-- | Determine whether the stream is empty.
null :: Monad m => StreamT m a -> m Bool
null m =
    let stop      = return True
        single _  = return False
        yield _ _ = return False
    in (S.runStream (toStream m)) Nothing stop single yield

-- | Determine whether an element is present in the stream.
elem :: (Monad m, Eq a) => a -> StreamT m a -> m Bool
elem e m = go (toStream m)
    where
    go m1 =
        let stop      = return False
            single a  = return (a == e)
            yield a r = if a == e then return True else go r
        in (S.runStream m1) Nothing stop single yield

-- | Determine whether an element is not present in the stream.
notElem :: (Monad m, Eq a) => a -> StreamT m a -> m Bool
notElem e m = go (toStream m)
    where
    go m1 =
        let stop      = return True
            single a  = return (a /= e)
            yield a r = if a == e then return False else go r
        in (S.runStream m1) Nothing stop single yield

-- | Determine the length of the stream.
length :: Monad m => StreamT m a -> m Int
length = foldl (\n _ -> n + 1) 0 id

-- | Returns the elements of the stream in reverse order.
-- The stream must be finite.
reverse :: (IsStream t) => t m a -> t m a
reverse m = fromStream $ go S.nil (toStream m)
    where
    go rev rest = Stream $ \svr stp sng yld ->
        let run x = S.runStream x svr stp sng yld
            stop = run rev
            single a = run $ a `S.cons` rev
            yield a r = run $ go (a `S.cons` rev) r
         in S.runStream rest svr stop single yield

-- XXX replace the recursive "go" with continuation
-- | Determine the minimum element in a stream.
minimum :: (Monad m, Ord a) => StreamT m a -> m (Maybe a)
minimum m = go Nothing (toStream m)
    where
    go res m1 =
        let stop      = return res
            single a  = return $ min_ a res
            yield a r = go (min_ a res) r
        in (S.runStream m1) Nothing stop single yield

    min_ a res = case res of
        Nothing -> Just a
        Just e  -> Just $ min a e

-- XXX replace the recursive "go" with continuation
-- | Determine the maximum element in a stream.
maximum :: (Monad m, Ord a) => StreamT m a -> m (Maybe a)
maximum m = go Nothing (toStream m)
    where
    go res m1 =
        let stop      = return res
            single a  = return $ max_ a res
            yield a r = go (max_ a res) r
        in (S.runStream m1) Nothing stop single yield

    max_ a res = case res of
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
    go m1 = Stream $ \_ stp sng yld ->
        let single a  = f a >>= sng
            yield a r = f a >>= \b -> yld b (go r)
         in (S.runStream m1) Nothing stp single yield

-- | Apply a monadic action to each element of the stream and discard the
-- output of the action.
mapM_ :: Monad m => (a -> m b) -> StreamT m a -> m ()
mapM_ f m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            single a = void (f a)
            yield a r = f a >> go r
         in (S.runStream m1) Nothing stop single yield

-- | Reduce a stream of monadic actions to a stream of the output of those
-- actions.
sequence :: (IsStream t, Monad m) => t m (m a) -> t m a
sequence m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \_ stp sng yld ->
        let single ma = ma >>= sng
            yield ma r = ma >>= \b -> yld b (go r)
         in (S.runStream m1) Nothing stp single yield

-- | Generate a stream by performing an action @n@ times.
replicateM :: (IsStream t, Monad m) => Int -> m a -> t m a
replicateM n m = fromStream $ go n
    where
    go cnt = Stream $ \_ stp _ yld ->
        if cnt <= 0
        then stp
        else m >>= \a -> yld a (go (cnt - 1))

------------------------------------------------------------------------------
-- Serially Zipping Streams
------------------------------------------------------------------------------

-- | Zip two streams serially using a monadic zipping function.
zipWithM :: IsStream t => (a -> b -> t m c) -> t m a -> t m b -> t m c
zipWithM f m1 m2 = fromStream $ go (toStream m1) (toStream m2)
    where
    go mx my = Stream $ \_ stp sng yld -> do
        let merge a ra =
                let run x = S.runStream x Nothing stp sng yld
                    single2 b   = run $ toStream (f a b)
                    yield2 b rb = run $ toStream (f a b) <> go ra rb
                 in (S.runStream my) Nothing stp single2 yield2
        let single1 a  = merge a S.nil
            yield1 a ra = merge a ra
        (S.runStream mx) Nothing stp single1 yield1

------------------------------------------------------------------------------
-- Parallely Zipping Streams
------------------------------------------------------------------------------

-- | Zip two streams asyncly (i.e. both the elements being zipped are generated
-- concurrently) using a monadic zipping function.
zipParallelWithM :: (IsStream t, MonadParallel m)
    => (a -> b -> t m c) -> t m a -> t m b -> t m c
zipParallelWithM f m1 m2 = fromStream $ Stream $ \_ stp sng yld -> do
    ma <- async m1
    mb <- async m2
    (S.runStream (toStream (zipWithM f ma mb))) Nothing stp sng yld

{-# DEPRECATED zipAsyncWithM "Please use zipParallelWithM instead." #-}
zipAsyncWithM :: (IsStream t, MonadParallel m)
    => (a -> b -> t m c) -> t m a -> t m b -> t m c
zipAsyncWithM = zipParallelWithM
