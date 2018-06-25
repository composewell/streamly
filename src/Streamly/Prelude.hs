{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

#include "Streams/inline.h"

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
    -- | Primitives to construct a stream.
      K.nil
    , K.cons
    , (K..:)
    , consM
    , (|:)

    -- * Deconstruction
    , uncons

    -- * Generation by Unfolding
    , unfoldr
    , unfoldrM

    -- * Special Generation
    -- | Generate a monadic stream from an input structure, a seed or a
    -- generation function.
    , K.yield
    , K.yieldM
    , replicateM
    , K.repeat
    , repeatM
    , iterate
    , iterateM
    , fromList
    , fromListM
    , K.fromFoldable
    , fromFoldableM

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
    , scanlM'
    , scanx

    -- * Mapping
    , Serial.map
    , mapM
    , mapMaybe
    , mapMaybeM
    , sequence

    -- * Filtering
    , filter
    , take
    , takeWhile
    , drop
    , dropWhile

    -- * Reordering
    , reverse

    -- * Zipping
    , zipWith
    , zipWithM
    , zipAsyncWith
    , zipAsyncWithM

    -- * IO
    , fromHandle
    , toHandle

    -- * Deprecated
    , K.once
    , each
    , scan
    , foldl
    , foldlM
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (isJust, fromJust)
import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate)
import qualified Prelude
import qualified System.IO as IO

import Streamly.SVar (MonadAsync)
import Streamly.Streams.StreamK (IsStream(..))
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.Zip (zipWith, zipWithM, zipAsyncWith, zipAsyncWithM)

import qualified Streamly.Streams.StreamK as K
import qualified Streamly.Streams.StreamD as D
import qualified Streamly.Streams.Serial as Serial

------------------------------------------------------------------------------
-- Deconstruction
------------------------------------------------------------------------------

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns @Just (a, ma)@, where @a@ is
-- the head of the stream and @ma@ its tail.
--
-- @since 0.1.0
uncons :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (a, t m a))
uncons m =
    let stop = return Nothing
        single a = return (Just (a, K.nil))
        yieldk a r = return (Just (a, fromStream r))
    in (K.unStream (toStream m)) Nothing stop single yieldk

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
{-# INLINE_EARLY unfoldr #-}
unfoldr :: (Monad m, IsStream t) => (b -> Maybe (a, b)) -> b -> t m a
unfoldr step seed = fromStream $ D.toStreamK (D.unfoldr step seed)
{-# RULES "unfoldr fallback to StreamK" [1]
    forall a b. D.toStreamK (D.unfoldr a b) = K.unfoldr a b #-}

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
{-# INLINE_EARLY unfoldrM #-}
unfoldrM :: (IsStream t, MonadAsync m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM = K.unfoldrM

{-# RULES "unfoldrM serial" unfoldrM = unfoldrMSerial #-}
{-# INLINE_EARLY unfoldrMSerial #-}
unfoldrMSerial :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> SerialT m a
unfoldrMSerial step seed = fromStream $ D.toStreamK (D.unfoldrM step seed)

-- | Construct a stream from a list containing pure values. This can be more
-- efficient than 'K.fromFoldable' for lists as it can fuse the list.
--
-- @since 0.4.0
{-# INLINE_EARLY fromList #-}
fromList :: (Monad m, IsStream t) => [a] -> t m a
fromList = fromStream . D.toStreamK . D.fromList
{-# RULES "fromList fallback to StreamK" [1]
    forall a. D.toStreamK (D.fromList a) = K.fromFoldable a #-}

-- | Construct a stream from a list containing monadic actions. This can be
-- more efficient than 'fromFoldableM' especially for serial streams as it can
-- fuse the list.
--
-- @since 0.4.0
{-# INLINE_EARLY fromListM #-}
fromListM :: (MonadAsync m, IsStream t) => [m a] -> t m a
fromListM = fromStream . D.toStreamK . D.fromListM
{-# RULES "fromListM fallback to StreamK" [1]
    forall a. D.toStreamK (D.fromListM a) = fromFoldableM a #-}

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
fromFoldableM = Prelude.foldr consM K.nil

-- | Same as 'fromFoldable'.
--
-- @since 0.1.0
{-# DEPRECATED each "Please use fromFoldable instead." #-}
{-# INLINE each #-}
each :: (IsStream t, Foldable f) => f a -> t m a
each = K.fromFoldable

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
    go cnt = if cnt <= 0 then K.nil else m |: go (cnt - 1)

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
    go s = K.cons s (go (step s))

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
    go s = fromStream $ K.Stream $ \svr stp sng yld -> do
       next <- step s
       K.unStream (toStream (return s |: go next)) svr stp sng yld

-- | Read lines from an IO Handle into a stream of Strings.
--
-- @since 0.1.0
fromHandle :: (IsStream t, MonadIO m) => IO.Handle -> t m String
fromHandle h = fromStream go
  where
  go = K.Stream $ \_ stp _ yld -> do
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
{-# INLINE foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> SerialT m a -> m b
-- XXX somehow this definition does not perform well, need to investigate
-- foldr step acc m = D.foldr step acc $ D.fromStreamK (toStream m)
foldr f = foldrM (\a b -> return (f a b))

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
foldrM step acc m = D.foldrM step acc $ D.fromStreamK (toStream m)

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
--
-- @since 0.2.0
{-# INLINE foldx #-}
foldx :: Monad m => (x -> a -> x) -> x -> (x -> b) -> SerialT m a -> m b
foldx = K.foldx

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
foldl' step begin m = D.foldl' step begin $ D.fromStreamK (toStream m)

-- XXX replace the recursive "go" with explicit continuations.
-- | Like 'foldx', but with a monadic step function.
--
-- @since 0.2.0
foldxM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> SerialT m a -> m b
foldxM = K.foldxM

-- |
-- @since 0.1.0
{-# DEPRECATED foldlM "Please use foldxM instead." #-}
foldlM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> SerialT m a -> m b
foldlM = foldxM

-- | Like 'foldl'' but with a monadic step function.
--
-- @since 0.2.0
foldlM' :: Monad m => (b -> a -> m b) -> b -> SerialT m a -> m b
foldlM' step begin m = D.foldlM' step begin $ D.fromStreamK (toStream m)

-- | Write a stream of Strings to an IO Handle.
--
-- @since 0.1.0
toHandle :: MonadIO m => IO.Handle -> SerialT m String -> m ()
toHandle h m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            single a = liftIO (IO.hPutStrLn h a)
            yieldk a r = liftIO (IO.hPutStrLn h a) >> go r
        in (K.unStream m1) Nothing stop single yieldk

------------------------------------------------------------------------------
-- Special folds
------------------------------------------------------------------------------

-- | Convert a stream into a list in the underlying monad.
--
-- @since 0.1.0
{-# INLINE toList #-}
toList :: Monad m => SerialT m a -> m [a]
toList m = D.toList $ D.fromStreamK (toStream m)

-- | Take first 'n' elements from the stream and discard the rest.
--
-- @since 0.1.0
{-# INLINE take #-}
take :: (IsStream t, Monad m) => Int -> t m a -> t m a
take n m = fromStream $ D.toStreamK $ D.take n (D.fromStreamK $ toStream m)

-- | Include only those elements that pass a predicate.
--
-- @since 0.1.0
{-# INLINE filter #-}
#if __GLASGOW_HASKELL__ != 802
-- GHC 8.2.2 crashes with this code, when used with "stack"
filter :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m a
filter p m = fromStream $ D.toStreamK $ D.filter p (D.fromStreamK $ toStream m)
#else
filter :: IsStream t => (a -> Bool) -> t m a -> t m a
filter = K.filter
#endif

-- | End the stream as soon as the predicate fails on an element.
--
-- @since 0.1.0
{-# INLINE takeWhile #-}
takeWhile :: IsStream t => (a -> Bool) -> t m a -> t m a
takeWhile p m = fromStream $ go (toStream m)
    where
    go m1 = K.Stream $ \_ stp sng yld ->
        let single a  | p a       = sng a
                      | otherwise = stp
            yieldk a r | p a       = yld a (go r)
                      | otherwise = stp
         in (K.unStream m1) Nothing stp single yieldk

-- | Discard first 'n' elements from the stream and take the rest.
--
-- @since 0.1.0
drop :: IsStream t => Int -> t m a -> t m a
drop n m = fromStream $ go n (toStream m)
    where
    go n1 m1 = K.Stream $ \_ stp sng yld ->
        let single _ = stp
            yieldk _ r = (K.unStream $ go (n1 - 1) r) Nothing stp sng yld
        -- Somehow "<=" check performs better than a ">"
        in if n1 <= 0
           then (K.unStream m1) Nothing stp sng yld
           else (K.unStream m1) Nothing stp single yieldk

-- | Drop elements in the stream as long as the predicate succeeds and then
-- take the rest of the stream.
--
-- @since 0.1.0
{-# INLINE dropWhile #-}
dropWhile :: IsStream t => (a -> Bool) -> t m a -> t m a
dropWhile p m = fromStream $ go (toStream m)
    where
    go m1 = K.Stream $ \_ stp sng yld ->
        let single a  | p a       = stp
                      | otherwise = sng a
            yieldk a r | p a       = (K.unStream r) Nothing stp single yieldk
                      | otherwise = yld a r
         in (K.unStream m1) Nothing stp single yieldk

-- | Determine whether all elements of a stream satisfy a predicate.
--
-- @since 0.1.0
all :: Monad m => (a -> Bool) -> SerialT m a -> m Bool
all p m = go (toStream m)
    where
    go m1 =
        let single a  | p a       = return True
                      | otherwise = return False
            yieldk a r | p a       = go r
                      | otherwise = return False
         in (K.unStream m1) Nothing (return True) single yieldk

-- | Determine whether any of the elements of a stream satisfy a predicate.
--
-- @since 0.1.0
any :: Monad m => (a -> Bool) -> SerialT m a -> m Bool
any p m = go (toStream m)
    where
    go m1 =
        let single a  | p a       = return True
                      | otherwise = return False
            yieldk a r | p a       = return True
                      | otherwise = go r
         in (K.unStream m1) Nothing (return False) single yieldk

-- | Determine the sum of all elements of a stream of numbers
--
-- @since 0.1.0
{-# INLINE sum #-}
sum :: (Monad m, Num a) => SerialT m a -> m a
sum = foldl' (+) 0

-- | Determine the product of all elements of a stream of numbers
--
-- @since 0.1.1
{-# INLINE product #-}
product :: (Monad m, Num a) => SerialT m a -> m a
product = foldl' (*) 1

-- | Extract the first element of the stream, if any.
--
-- @since 0.1.0
head :: Monad m => SerialT m a -> m (Maybe a)
head m =
    let stop      = return Nothing
        single a  = return (Just a)
        yieldk a _ = return (Just a)
    in (K.unStream (toStream m)) Nothing stop single yieldk

-- | Extract all but the first element of the stream, if any.
--
-- @since 0.1.1
tail :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (t m a))
tail m =
    let stop      = return Nothing
        single _  = return $ Just K.nil
        yieldk _ r = return $ Just $ fromStream r
    in (K.unStream (toStream m)) Nothing stop single yieldk

-- | Extract the last element of the stream, if any.
--
-- @since 0.1.1
{-# INLINE last #-}
last :: Monad m => SerialT m a -> m (Maybe a)
last m = D.last $ D.fromStreamK (toStream m)

-- | Determine whether the stream is empty.
--
-- @since 0.1.1
null :: Monad m => SerialT m a -> m Bool
null m =
    let stop      = return True
        single _  = return False
        yieldk _ _ = return False
    in (K.unStream (toStream m)) Nothing stop single yieldk

-- | Determine whether an element is present in the stream.
--
-- @since 0.1.0
elem :: (Monad m, Eq a) => a -> SerialT m a -> m Bool
elem e m = go (toStream m)
    where
    go m1 =
        let stop      = return False
            single a  = return (a == e)
            yieldk a r = if a == e then return True else go r
        in (K.unStream m1) Nothing stop single yieldk

-- | Determine whether an element is not present in the stream.
--
-- @since 0.1.0
notElem :: (Monad m, Eq a) => a -> SerialT m a -> m Bool
notElem e m = go (toStream m)
    where
    go m1 =
        let stop      = return True
            single a  = return (a /= e)
            yieldk a r = if a == e then return False else go r
        in (K.unStream m1) Nothing stop single yieldk

-- | Determine the length of the stream.
--
-- @since 0.1.0
{-# INLINE length #-}
length :: Monad m => SerialT m a -> m Int
length = foldl' (\n _ -> n + 1) 0

-- | Returns the elements of the stream in reverse order.
-- The stream must be finite.
--
-- @since 0.1.1
reverse :: (IsStream t) => t m a -> t m a
reverse m = fromStream $ go K.nil (toStream m)
    where
    go rev rest = K.Stream $ \_ stp sng yld ->
        let runIt x = K.unStream x Nothing stp sng yld
            stop = runIt rev
            single a = runIt $ a `K.cons` rev
            yieldk a r = runIt $ go (a `K.cons` rev) r
         in K.unStream rest Nothing stop single yieldk

-- XXX replace the recursive "go" with continuation
-- | Determine the minimum element in a stream.
--
-- @since 0.1.0
{-# INLINE minimum #-}
minimum :: (Monad m, Ord a) => SerialT m a -> m (Maybe a)
minimum m = go Nothing (toStream m)
    where
    go res m1 =
        let stop      = return res
            single a  = return $ min_ a res
            yieldk a r = go (min_ a res) r
        in (K.unStream m1) Nothing stop single yieldk

    min_ a res = case res of
        Nothing -> Just a
        Just e  -> Just $ min a e

-- XXX replace the recursive "go" with continuation
-- | Determine the maximum element in a stream.
--
-- @since 0.1.0
{-# INLINE maximum #-}
maximum :: (Monad m, Ord a) => SerialT m a -> m (Maybe a)
maximum m = go Nothing (toStream m)
    where
    go res m1 =
        let stop      = return res
            single a  = return $ max_ a res
            yieldk a r = go (max_ a res) r
        in (K.unStream m1) Nothing stop single yieldk

    max_ a res = case res of
        Nothing -> Just a
        Just e  -> Just $ max a e

------------------------------------------------------------------------------
-- Scans
------------------------------------------------------------------------------

-- | Strict left scan with an extraction function. Like 'scanl'', but applies a
-- user supplied extraction function (the third argument) at each step. This is
-- designed to work with the @foldl@ library. The suffix @x@ is a mnemonic for
-- extraction.
--
-- @since 0.2.0
{-# INLINE scanx #-}
scanx :: IsStream t => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scanx = K.scanx

-- |
-- @since 0.1.1
{-# DEPRECATED scan "Please use scanx instead." #-}
scan :: IsStream t => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scan = scanx

-- | Like 'scanl'' but with a monadic step function.
--
-- @since 0.4.0
{-# INLINE scanlM' #-}
scanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> b -> t m a -> t m b
scanlM' step begin m = fromStream $ D.toStreamK $
    D.scanlM' step begin $ D.fromStreamK (toStream m)

-- | Strict left scan. Like 'foldl'', but returns the folded value at each
-- step, generating a stream of all intermediate fold results. The first
-- element of the stream is the user supplied initial value, and the last
-- element of the stream is the same as the result of 'foldl''.
--
-- @since 0.2.0
{-# INLINE scanl' #-}
scanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
scanl' step = scanlM' (\a b -> return (step a b))

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
{-# INLINE_EARLY mapM #-}
mapM :: (IsStream t, MonadAsync m) => (a -> m b) -> t m a -> t m b
mapM = K.mapM

{-# RULES "mapM serial" mapM = mapMSerial #-}
{-# INLINE mapMSerial #-}
mapMSerial :: Monad m => (a -> m b) -> SerialT m a -> SerialT m b
mapMSerial = Serial.mapM

-- | Map a 'Maybe' returning function to a stream, filter out the 'Nothing'
-- elements, and return a stream of values extracted from 'Just'.
--
-- @since 0.3.0
{-# INLINE mapMaybe #-}
mapMaybe :: IsStream t => (a -> Maybe b) -> t m a -> t m b
mapMaybe = K.mapMaybe

-- | Like 'mapMaybe' but maps a monadic function.
--
-- /Concurrent (do not use with 'parallely' on infinite streams)/
--
-- @since 0.3.0
{-# INLINE mapMaybeM #-}
mapMaybeM :: (IsStream t, MonadAsync m, Functor (t m))
          => (a -> m (Maybe b)) -> t m a -> t m b
mapMaybeM f = fmap fromJust . filter isJust . mapM f

-- XXX this can utilize parallel mapping if we implement it as runStream . mapM
-- | Apply a monadic action to each element of the stream and discard the
-- output of the action.
--
-- @since 0.1.0
{-# INLINE mapM_ #-}
mapM_ :: Monad m => (a -> m b) -> SerialT m a -> m ()
mapM_ f m = D.mapM_ f $ D.fromStreamK (toStream m)

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
{-# INLINE sequence #-}
sequence :: (IsStream t, MonadAsync m) => t m (m a) -> t m a
sequence = K.sequence
