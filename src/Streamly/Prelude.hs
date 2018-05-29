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
-- This module is designed to be imported qualified:
--
-- @
-- import qualified Streamly.Prelude as S
-- @
--
-- Functions with the suffix @M@ are general functions that work on monadic
-- arguments. The corresponding functions without the suffix @M@ work on pure
-- arguments and can in general be derived from their monadic versions but are
-- provided for convenience and for consistency with other pure APIs in the
-- @base@ package.
--
-- Functions having a 'MonadAsync' constraint work concurrently when used with
-- appropriate stream type combinator. Please be careful to not use 'parallely'
-- with infinite streams.
--
-- Deconstruction and folds accept a 'SerialT' type instead of a polymorphic
-- type to ensure that streams always have a concrete monomorphic type by
-- default, reducing type errors. In case you want to use any other type of
-- stream you can use one of the type combinators provided in the "Streamly"
-- module to convert the stream type.

module Streamly.Prelude
    (
    -- * Construction
    -- | Primitives to construct or inspect a stream.
      nil
    , consM
    , (|:)
    , cons
    , (.:)

    -- * Generation by Unfolding
    , unfoldr
    , unfoldrM

    -- * Special Generation
    -- | Generate a monadic stream from an input structure, a seed or a
    -- generation function.
    , once
    , replicateM
    , repeatM
    , iterate
    , iterateM
    , fromFoldable
    , fromFoldableM

    -- * Deconstruction
    , uncons

    -- * Elimination by Folding
    -- ** General Folds
    , foldr
    , foldrM
    , foldl'
    , foldlM'
    , foldx
    , foldxM

    -- ** Special Folds
    , mapM_
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
    , maximum
    , minimum
    , sum
    , product

    -- * Scans
    , scanl'
    , scanx

    -- * Filtering
    , filter
    , take
    , takeWhile
    , drop
    , dropWhile
    , mapMaybe

    -- * Reordering
    , reverse

    -- * Mapping
    , mapM
    , mapMaybeM
    , sequence

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
import           Data.Maybe                  (isJust, fromJust)
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

-- | Build a stream by unfolding a /pure/ step function starting from a seed.
-- The step function returns the next element in the stream and the next seed
-- value. When it is done it returns 'Nothing' and the stream ends. For
-- example,
--
-- @
-- let f b =
--         if b > 3
--         then Nothing
--         else Just (b, b + 1)
-- in toList $ unfoldr f 0
-- @
-- @
-- [0,1,2,3]
-- @
--
-- @since 0.1.0
{-# INLINE unfoldr #-}
unfoldr :: IsStream t => (b -> Maybe (a, b)) -> b -> t m a
unfoldr step = fromStream . go
    where
    go s = Stream $ \_ stp _ yld ->
        case step s of
            Nothing -> stp
            Just (a, b) -> yld a (go b)

-- | Build a stream by unfolding a /monadic/ step function starting from a
-- seed.  The step function returns the next element in the stream and the next
-- seed value. When it is done it returns 'Nothing' and the stream ends. For
-- example,
--
-- @
-- let f b =
--         if b > 3
--         then return Nothing
--         else print b >> return (Just (b, b + 1))
-- in runStream $ unfoldrM f 0
-- @
-- @
--  0
--  1
--  2
--  3
-- @
-- When run concurrently, the next unfold step can run concurrently with the
-- processing of the output of the previous step.  Note that more than one step
-- cannot run concurrently as the next step depends on the output of the
-- previous step.
--
-- @
-- (asyncly $ S.unfoldrM (\\n -> liftIO (threadDelay 1000000) >> return (Just (n, n + 1))) 0)
--     & S.foldlM' (\\_ a -> threadDelay 1000000 >> print a) ()
-- @
--
-- /Concurrent/
--
-- /Since: 0.1.0/
{-# INLINE unfoldrM #-}
unfoldrM :: (IsStream t, MonadAsync m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM step = go
    where
    go s = fromStream $ Stream $ \svr stp sng yld -> do
        mayb <- step s
        case mayb of
            Nothing -> stp
            Just (a, b) ->
                S.runStream (toStream (return a |: go b)) svr stp sng yld

-- | Construct a stream from a 'Foldable' containing pure values.
--
-- @since 0.2.0
{-# INLINE fromFoldable #-}
fromFoldable :: (IsStream t, Foldable f) => f a -> t m a
fromFoldable = Prelude.foldr cons nil

-- | Construct a stream from a 'Foldable' containing monadic actions.
--
-- @
-- runStream $ serially $ S.fromFoldableM $ replicate 10 (threadDelay 1000000 >> print 1)
-- runStream $ asyncly  $ S.fromFoldableM $ replicate 10 (threadDelay 1000000 >> print 1)
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite containers)/
--
-- @since 0.3.0
{-# INLINE fromFoldableM #-}
fromFoldableM :: (IsStream t, MonadAsync m, Foldable f) => f (m a) -> t m a
fromFoldableM = Prelude.foldr consM nil

-- | Same as 'fromFoldable'.
--
-- @since 0.1.0
{-# DEPRECATED each "Please use fromFoldable instead." #-}
{-# INLINE each #-}
each :: (IsStream t, Foldable f) => f a -> t m a
each = fromFoldable

-- | Create a singleton stream by executing a monadic action once. Same as
-- @m \`consM` nil@ but more efficient.
--
-- @
-- > toList $ once getLine
-- hello
-- ["hello"]
-- @
--
-- @since 0.2.0
once :: (IsStream t, Monad m) => m a -> t m a
once = fromStream . S.once

-- | Generate a stream by performing a monadic action @n@ times.
--
--
-- @
-- runStream $ serially $ S.replicateM 10 $ (threadDelay 1000000 >> print 1)
-- runStream $ asyncly  $ S.replicateM 10 $ (threadDelay 1000000 >> print 1)
-- @
--
-- /Concurrent/
--
-- @since 0.1.1
replicateM :: (IsStream t, MonadAsync m) => Int -> m a -> t m a
replicateM n m = go n
    where
    go cnt = if cnt <= 0 then nil else m |: go (cnt - 1)

-- | Generate a stream by repeatedly executing a monadic action forever.
--
-- @
-- runStream $ serially $ S.take 10 $ S.repeatM $ (threadDelay 1000000 >> print 1)
-- runStream $ asyncly  $ S.take 10 $ S.repeatM $ (threadDelay 1000000 >> print 1)
-- @
--
-- /Concurrent, infinite (do not use with 'parallely')/
--
-- @since 0.2.0
repeatM :: (IsStream t, MonadAsync m) => m a -> t m a
repeatM = go
    where go m = m |: go m

-- | Iterate a pure function from a seed value, streaming the results forever.
--
-- @since 0.1.2
iterate :: IsStream t => (a -> a) -> a -> t m a
iterate step = fromStream . go
    where
    go s = S.cons s (go (step s))

-- | Iterate a monadic function from a seed value, streaming the results
-- forever.
--
-- When run concurrently, the next iteration can run concurrently with the
-- processing of the previous iteration. Note that more than one iteration
-- cannot run concurrently as the next iteration depends on the output of the
-- previous iteration.
--
-- @
-- runStream $ serially $ S.take 10 $ S.iterateM
--      (\\x -> threadDelay 1000000 >> print x >> return (x + 1)) 0
--
-- runStream $ asyncly  $ S.take 10 $ S.iterateM
--      (\\x -> threadDelay 1000000 >> print x >> return (x + 1)) 0
-- @
--
-- /Concurrent/
--
-- @since 0.1.2
iterateM :: (IsStream t, MonadAsync m) => (a -> m a) -> a -> t m a
iterateM step = go
    where
    go s = fromStream $ Stream $ \svr stp sng yld -> do
       next <- step s
       S.runStream (toStream (return s |: go next)) svr stp sng yld

-- | Read lines from an IO Handle into a stream of Strings.
--
-- @since 0.1.0
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
--
-- @since 0.1.0
foldr :: Monad m => (a -> b -> b) -> b -> SerialT m a -> m b
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
--
-- @since 0.2.0
{-# INLINE foldrM #-}
foldrM :: Monad m => (a -> b -> m b) -> b -> SerialT m a -> m b
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
--
-- @since 0.2.0
{-# INLINE scanx #-}
scanx :: IsStream t => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scanx step begin done m =
    cons (done begin) $ fromStream $ go (toStream m) begin
    where
    go m1 !acc = Stream $ \_ stp sng yld ->
        let single a = sng (done $ step acc a)
            yield a r =
                let s = step acc a
                in yld (done s) (go r s)
        in S.runStream m1 Nothing stp single yield

-- |
-- @since 0.1.1
{-# DEPRECATED scan "Please use scanx instead." #-}
scan :: IsStream t => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scan = scanx

-- | Strict left scan. Like 'foldl'', but returns the folded value at each
-- step, generating a stream of all intermediate fold results. The first
-- element of the stream is the user supplied initial value, and the last
-- element of the stream is the same as the result of 'foldl''.
--
-- @since 0.2.0
{-# INLINE scanl' #-}
scanl' :: IsStream t => (b -> a -> b) -> b -> t m a -> t m b
scanl' step begin m = scanx step begin id m

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
--
-- @since 0.2.0
{-# INLINE foldx #-}
foldx :: Monad m => (x -> a -> x) -> x -> (x -> b) -> SerialT m a -> m b
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

-- |
-- @since 0.1.0
{-# DEPRECATED foldl "Please use foldx instead." #-}
foldl :: Monad m => (x -> a -> x) -> x -> (x -> b) -> SerialT m a -> m b
foldl = foldx

-- | Strict left associative fold.
--
-- @since 0.2.0
{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> SerialT m a -> m b
foldl' step begin m = foldx step begin id m

-- XXX replace the recursive "go" with explicit continuations.
-- | Like 'foldx', but with a monadic step function.
--
-- @since 0.2.0
foldxM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> SerialT m a -> m b
foldxM step begin done m = go begin (toStream m)
    where
    go !acc m1 =
        let stop = acc >>= done
            single a = acc >>= \b -> step b a >>= done
            yield a r = acc >>= \b -> go (step b a) r
         in (S.runStream m1) Nothing stop single yield

-- |
-- @since 0.1.0
{-# DEPRECATED foldlM "Please use foldxM instead." #-}
foldlM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> SerialT m a -> m b
foldlM = foldxM

-- | Like 'foldl'' but with a monadic step function.
--
-- @since 0.2.0
foldlM' :: Monad m => (b -> a -> m b) -> b -> SerialT m a -> m b
foldlM' step begin m = foldxM step (return begin) return m

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns @Just (a, ma)@, where @a@ is
-- the head of the stream and @ma@ its tail.
--
-- @since 0.1.0
uncons :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (a, t m a))
uncons m =
    let stop = return Nothing
        single a = return (Just (a, nil))
        yield a r = return (Just (a, fromStream r))
    in (S.runStream (toStream m)) Nothing stop single yield

-- | Write a stream of Strings to an IO Handle.
--
-- @since 0.1.0
toHandle :: MonadIO m => IO.Handle -> SerialT m String -> m ()
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
--
-- @since 0.1.0
{-# INLINABLE toList #-}
toList :: Monad m => SerialT m a -> m [a]
toList = foldrM (\a xs -> return (a : xs)) []

-- | Take first 'n' elements from the stream and discard the rest.
--
-- @since 0.1.0
{-# INLINE take #-}
take :: IsStream t => Int -> t m a -> t m a
take n m = fromStream $ go n (toStream m)
    where
    go n1 m1 = Stream $ \ctx stp sng yld ->
        let yield a r = yld a (go (n1 - 1) r)
        in if n1 <= 0 then stp else (S.runStream m1) ctx stp sng yield

-- | Include only those elements that pass a predicate.
--
-- @since 0.1.0
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
--
-- @since 0.1.0
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
--
-- @since 0.1.0
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
--
-- @since 0.1.0
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

-- | Map + filter: compute a stream of b from a stream of a and filter out
-- elements that are mapped to Nothing. It is similar to 'mapMaybe' in
-- 'Data.Maybe' but applies to streams instead of list.
--
-- @since 0.2.1
{-# INLINE mapMaybe #-}
mapMaybe :: (IsStream t) => (a -> Maybe b) -> t m a -> t m b
mapMaybe f m = go (toStream m)
  where
    go m1 = fromStream $ Stream $ \_ stp sng yld ->
        let single a = case f a of
                Just b  -> sng b
                Nothing -> stp
            yield a r = case f a of
                Just b  -> yld b (toStream $ go r)
                Nothing -> (S.runStream r) Nothing stp single yield
        in (S.runStream m1) Nothing stp single yield

-- | Determine whether all elements of a stream satisfy a predicate.
--
-- @since 0.1.0
all :: Monad m => (a -> Bool) -> SerialT m a -> m Bool
all p m = go (toStream m)
    where
    go m1 =
        let single a  | p a       = return True
                      | otherwise = return False
            yield a r | p a       = go r
                      | otherwise = return False
         in (S.runStream m1) Nothing (return True) single yield

-- | Determine whether any of the elements of a stream satisfy a predicate.
--
-- @since 0.1.0
any :: Monad m => (a -> Bool) -> SerialT m a -> m Bool
any p m = go (toStream m)
    where
    go m1 =
        let single a  | p a       = return True
                      | otherwise = return False
            yield a r | p a       = return True
                      | otherwise = go r
         in (S.runStream m1) Nothing (return False) single yield

-- | Determine the sum of all elements of a stream of numbers
--
-- @since 0.1.0
sum :: (Monad m, Num a) => SerialT m a -> m a
sum = foldl (+) 0 id

-- | Determine the product of all elements of a stream of numbers
--
-- @since 0.1.1
product :: (Monad m, Num a) => SerialT m a -> m a
product = foldl (*) 1 id

-- | Extract the first element of the stream, if any.
--
-- @since 0.1.0
head :: Monad m => SerialT m a -> m (Maybe a)
head m =
    let stop      = return Nothing
        single a  = return (Just a)
        yield a _ = return (Just a)
    in (S.runStream (toStream m)) Nothing stop single yield

-- | Extract all but the first element of the stream, if any.
--
-- @since 0.1.1
tail :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (t m a))
tail m =
    let stop      = return Nothing
        single _  = return $ Just nil
        yield _ r = return $ Just $ fromStream r
    in (S.runStream (toStream m)) Nothing stop single yield

-- | Extract the last element of the stream, if any.
--
-- @since 0.1.1
{-# INLINE last #-}
last :: Monad m => SerialT m a -> m (Maybe a)
last = foldl (\_ y -> Just y) Nothing id

-- | Determine whether the stream is empty.
--
-- @since 0.1.1
null :: Monad m => SerialT m a -> m Bool
null m =
    let stop      = return True
        single _  = return False
        yield _ _ = return False
    in (S.runStream (toStream m)) Nothing stop single yield

-- | Determine whether an element is present in the stream.
--
-- @since 0.1.0
elem :: (Monad m, Eq a) => a -> SerialT m a -> m Bool
elem e m = go (toStream m)
    where
    go m1 =
        let stop      = return False
            single a  = return (a == e)
            yield a r = if a == e then return True else go r
        in (S.runStream m1) Nothing stop single yield

-- | Determine whether an element is not present in the stream.
--
-- @since 0.1.0
notElem :: (Monad m, Eq a) => a -> SerialT m a -> m Bool
notElem e m = go (toStream m)
    where
    go m1 =
        let stop      = return True
            single a  = return (a /= e)
            yield a r = if a == e then return False else go r
        in (S.runStream m1) Nothing stop single yield

-- | Determine the length of the stream.
--
-- @since 0.1.0
length :: Monad m => SerialT m a -> m Int
length = foldl (\n _ -> n + 1) 0 id

-- | Returns the elements of the stream in reverse order.
-- The stream must be finite.
--
-- @since 0.1.1
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
--
-- @since 0.1.0
minimum :: (Monad m, Ord a) => SerialT m a -> m (Maybe a)
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
--
-- @since 0.1.0
maximum :: (Monad m, Ord a) => SerialT m a -> m (Maybe a)
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

-- | Replace each element of the stream with the result of a monadic action
-- applied on the element.
--
-- @
-- runStream $ S.replicateM 10 (return 1)
--           & (serially . S.mapM (\\x -> threadDelay 1000000 >> print x))
--
-- runStream $ S.replicateM 10 (return 1)
--           & (asyncly . S.mapM (\\x -> threadDelay 1000000 >> print x))
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite streams)/
--
-- @since 0.1.0
{-# INLINE mapM #-}
mapM :: (IsStream t, MonadAsync m) => (a -> m b) -> t m a -> t m b
mapM f m = go (toStream m)
    where
    go m1 = fromStream $ Stream $ \svr stp sng yld ->
        let single a  = f a >>= sng
            yield a r = S.runStream (toStream (f a |: (go r))) svr stp sng yld
         in (S.runStream m1) Nothing stp single yield

-- | Monadic version mapMaybe.
--
-- @since 0.2.1
{-# INLINE mapMaybeM #-}
mapMaybeM :: (IsStream t, MonadAsync m, Functor (t m))
          => (a -> m (Maybe b)) -> t m a -> t m b
mapMaybeM f = fmap fromJust . filter isJust . mapM f

-- XXX this can utilize parallel mapping if we implement it as runStream . mapM
-- | Apply a monadic action to each element of the stream and discard the
-- output of the action.
--
-- @since 0.1.0
mapM_ :: Monad m => (a -> m b) -> SerialT m a -> m ()
mapM_ f m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            single a = void (f a)
            yield a r = f a >> go r
         in (S.runStream m1) Nothing stop single yield

-- | Reduce a stream of monadic actions to a stream of the output of those
-- actions.
--
-- @
-- runStream $ S.replicateM 10 (return $ threadDelay 1000000 >> print 1)
--           & (serially . S.sequence)
--
-- runStream $ S.replicateM 10 (return $ threadDelay 1000000 >> print 1)
--           & (asyncly . S.sequence)
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite streams)/
--
-- @since 0.1.0
sequence :: (IsStream t, MonadAsync m) => t m (m a) -> t m a
sequence m = go (toStream m)
    where
    go m1 = fromStream $ Stream $ \svr stp sng yld ->
        let single ma = ma >>= sng
            yield ma r = S.runStream (toStream $ ma |: go r) svr stp sng yld
         in (S.runStream m1) Nothing stp single yield

------------------------------------------------------------------------------
-- Serially Zipping Streams
------------------------------------------------------------------------------

-- | Zip two streams serially using a pure zipping function.
--
-- @since 0.1.0
zipWith :: IsStream t => (a -> b -> c) -> t m a -> t m b -> t m c
zipWith f m1 m2 = fromStream $ S.zipWith f (toStream m1) (toStream m2)

-- | Zip two streams serially using a monadic zipping function.
--
-- @since 0.1.0
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

-- | Zip two streams concurrently (i.e. both the elements being zipped are
-- generated concurrently) using a pure zipping function.
--
-- @since 0.1.0
zipAsyncWith :: (IsStream t, MonadAsync m)
    => (a -> b -> c) -> t m a -> t m b -> t m c
zipAsyncWith f m1 m2 =
    fromStream $ S.zipAsyncWith f (toStream m1) (toStream m2)

-- | Zip two streams asyncly (i.e. both the elements being zipped are generated
-- concurrently) using a monadic zipping function.
--
-- @since 0.1.0
zipAsyncWithM :: (IsStream t, MonadAsync m)
    => (a -> b -> t m c) -> t m a -> t m b -> t m c
zipAsyncWithM f m1 m2 = fromStream $ Stream $ \_ stp sng yld -> do
    ma <- mkAsync m1
    mb <- mkAsync m2
    (S.runStream (toStream (zipWithM f ma mb))) Nothing stp sng yld
