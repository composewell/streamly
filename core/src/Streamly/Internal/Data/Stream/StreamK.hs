-- |
-- Module      : Streamly.Internal.Data.Stream.StreamK
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Continuation passing style (CPS) stream implementation. The symbol 'K' below
-- denotes a function as well as a Kontinuation.
--
-- @
-- import qualified Streamly.Internal.Data.Stream.StreamK as K
-- @
--
-- Streams can be constructed like lists, except that they use 'nil' instead of
-- '[]' and 'cons' instead of ':'.
--
-- `cons` adds a pure value at the head of the stream:
--
-- >>> import Streamly.Data.Stream (Stream, cons, consM, nil)
-- >>> stream = 1 `cons` 2 `cons` nil :: Stream IO Int
-- >>> Stream.fold Fold.toList stream -- IO [Int]
-- [1,2]
--
-- `consM` adds an effect at the head of the stream:
--
-- >>> stream = effect 1 `consM` effect 2 `consM` nil
-- >>> Stream.fold Fold.toList stream
-- 1
-- 2
-- [1,2]
--
module Streamly.Internal.Data.Stream.StreamK
    (
    -- * The stream type
      Stream(..) -- XXX stop exporting this
    , StreamK
    , fromStream
    , toStream

    , CrossStreamK(..)
    , fromCross
    , toCross

    -- * Construction Primitives
    , mkStream
    , nil
    , nilM
    , cons
    , (.:)

    -- * Elimination Primitives
    , foldStream
    , foldStreamShared

    -- * Transformation Primitives
    , unShare

    -- * Deconstruction
    , uncons

    -- * Generation
    -- ** Unfolds
    , unfoldr
    , unfoldrM

    -- ** Specialized Generation
    , repeat
    , repeatM
    , replicate
    , replicateM
    , fromIndices
    , fromIndicesM
    , iterate
    , iterateM

    -- ** Conversions
    , fromPure
    , fromEffect
    , fromFoldable
    , fromList

    -- * foldr/build
    , foldrS
    , foldrSM
    , buildS
    , augmentS

    -- * Elimination
    -- ** General Folds
    , foldr
    , foldr1
    , foldrM

    , foldl'
    , foldlM'
    , foldlS
    , foldlx'
    , foldlMx'
    , fold
    , foldBreak
    , foldEither
    , parseBreak

    -- ** Specialized Folds
    , drain
    , null
    , head
    , tail
    , init
    , elem
    , notElem
    , all
    , any
    , last
    , minimum
    , minimumBy
    , maximum
    , maximumBy
    , findIndices
    , lookup
    , findM
    , find
    , (!!)

    -- ** Map and Fold
    , mapM_

    -- ** Conversions
    , toList
    , hoist

    -- * Transformation
    -- ** By folding (scans)
    , scanl'
    , scanlx'

    -- ** Filtering
    , filter
    , take
    , takeWhile
    , drop
    , dropWhile

    -- ** Mapping
    , map
    , mapM
    , sequence

    -- ** Inserting
    , intersperseM
    , intersperse
    , insertBy

    -- ** Deleting
    , deleteBy

    -- ** Reordering
    , reverse
    , sortBy

    -- ** Map and Filter
    , mapMaybe

    -- ** Zipping
    , zipWith
    , zipWithM

    -- ** Merging
    , mergeBy
    , mergeByM

    -- ** Nesting
    , crossApplyWith
    , crossApply
    , crossApplySnd
    , crossApplyFst
    , crossWith

    , concatMapWith
    , concatMap
    , concatEffect
    , bindWith
    , mergeMapWith

    -- ** Transformation comprehensions
    , the

    -- * Semigroup Style Composition
    , append
    , interleave

    -- * Utilities
    , consM
    , mfix
    )
where

#include "inline.hs"
#include "assert.hs"

import Control.Monad (void, join)
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.SVar.Type (adaptState, defState)

import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Parser.ParserD.Type as PR
import qualified Streamly.Internal.Data.Stream.StreamD as Stream
import qualified Prelude

import Prelude
       hiding (foldl, foldr, last, map, mapM, mapM_, repeat, sequence,
               take, filter, all, any, takeWhile, drop, dropWhile, minimum,
               maximum, elem, notElem, null, head, tail, init, zipWith, lookup,
               foldr1, (!!), replicate, reverse, concatMap, iterate, splitAt)

import Streamly.Internal.Data.Stream.StreamK.Type
import Streamly.Internal.Data.Parser.ParserD (ParseError(..))

-- $setup
-- >>> :m

{-# INLINE fromStream #-}
fromStream :: Monad m => Stream.Stream m a -> StreamK m a
fromStream = Stream.toStreamK

{-# INLINE toStream #-}
toStream :: Applicative m => StreamK m a -> Stream.Stream m a
toStream = Stream.fromStreamK

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

{-# INLINE unfoldrM #-}
unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> Stream m a
unfoldrM = unfoldrMWith consM
{-
-- Generalization of concurrent streams/SVar via unfoldr.
--
-- Unfold a value into monadic actions and then run the resulting monadic
-- actions to generate a stream. Since the step of generating the monadic
-- action and running them are decoupled we can run the monadic actions
-- cooncurrently. For example, the seed could be a list of monadic actions or a
-- pure stream of monadic actions.
--
-- We can have different flavors of this depending on the stream type t. The
-- concurrent version could be async or ahead etc. Depending on how we queue
-- back the feedback portion b, it could be DFS or BFS style.
--
unfoldrA :: (b -> Maybe (m a, b)) -> b -> Stream m a
unfoldrA = undefined
-}

-------------------------------------------------------------------------------
-- Special generation
-------------------------------------------------------------------------------

repeatM :: Monad m => m a -> Stream m a
repeatM = repeatMWith consM

{-# INLINE replicateM #-}
replicateM :: Monad m => Int -> m a -> Stream m a
replicateM = replicateMWith consM
{-# INLINE replicate #-}
replicate :: Int -> a -> Stream m a
replicate n a = go n
    where
    go cnt = if cnt <= 0 then nil else a `cons` go (cnt - 1)

{-# INLINE fromIndicesM #-}
fromIndicesM :: Monad m => (Int -> m a) -> Stream m a
fromIndicesM = fromIndicesMWith consM
{-# INLINE fromIndices #-}
fromIndices :: (Int -> a) -> Stream m a
fromIndices gen = go 0
  where
    go n = gen n `cons` go (n + 1)

{-# INLINE iterate #-}
iterate :: (a -> a) -> a -> Stream m a
iterate step = go
    where
        go !s = cons s (go (step s))

{-# INLINE iterateM #-}
iterateM :: Monad m => (a -> m a) -> m a -> Stream m a
iterateM = iterateMWith consM

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

{-# INLINE fromList #-}
fromList :: [a] -> Stream m a
fromList = fromFoldable

-------------------------------------------------------------------------------
-- Elimination by Folding
-------------------------------------------------------------------------------

{-# INLINE foldr1 #-}
foldr1 :: Monad m => (a -> a -> a) -> Stream m a -> m (Maybe a)
foldr1 step m = do
    r <- uncons m
    case r of
        Nothing -> return Nothing
        Just (h, t) -> fmap Just (go h t)
    where
    go p m1 =
        let stp = return p
            single a = return $ step a p
            yieldk a r = fmap (step p) (go a r)
         in foldStream defState yieldk single stp m1

-- XXX replace the recursive "go" with explicit continuations.
-- | Like 'foldx', but with a monadic step function.
{-# INLINABLE foldlMx' #-}
foldlMx' :: Monad m
    => (x -> a -> m x) -> m x -> (x -> m b) -> Stream m a -> m b
foldlMx' step begin done = go begin
    where
    go !acc m1 =
        let stop = acc >>= done
            single a = acc >>= \b -> step b a >>= done
            yieldk a r = acc >>= \b -> step b a >>= \x -> go (return x) r
         in foldStream defState yieldk single stop m1

{-# INLINABLE fold #-}
fold :: Monad m => FL.Fold m a b -> Stream m a -> m b
fold (FL.Fold step begin done) m = do
    res <- begin
    case res of
        FL.Partial fs -> go fs m
        FL.Done fb -> return fb

    where
    go !acc m1 =
        let stop = done acc
            single a = step acc a
              >>= \case
                        FL.Partial s -> done s
                        FL.Done b1 -> return b1
            yieldk a r = step acc a
              >>= \case
                        FL.Partial s -> go s r
                        FL.Done b1 -> return b1
         in foldStream defState yieldk single stop m1

{-# INLINE foldEither #-}
foldEither :: Monad m =>
    Fold m a b -> Stream m a -> m (Either (Fold m a b) (b, Stream m a))
foldEither (FL.Fold step begin done) m = do
    res <- begin
    case res of
        FL.Partial fs -> go fs m
        FL.Done fb -> return $ Right (fb, m)

    where

    go !acc m1 =
        let stop = return $ Left (Fold step (return $ FL.Partial acc) done)
            single a =
                step acc a
                  >>= \case
                    FL.Partial s ->
                        return $ Left (Fold step (return $ FL.Partial s) done)
                    FL.Done b1 -> return $ Right (b1, nil)
            yieldk a r =
                step acc a
                  >>= \case
                    FL.Partial s -> go s r
                    FL.Done b1 -> return $ Right (b1, r)
         in foldStream defState yieldk single stop m1

{-# INLINE foldBreak #-}
foldBreak :: Monad m => Fold m a b -> Stream m a -> m (b, Stream m a)
foldBreak fld strm = do
    r <- foldEither fld strm
    case r of
        Right res -> return res
        Left (Fold _ initial extract) -> do
            res <- initial
            case res of
                FL.Done _ -> error "foldBreak: unreachable state"
                FL.Partial s -> do
                    b <- extract s
                    return (b, nil)

-- | Like 'foldl'' but with a monadic step function.
{-# INLINE foldlM' #-}
foldlM' :: Monad m => (b -> a -> m b) -> m b -> Stream m a -> m b
foldlM' step begin = foldlMx' step begin return

------------------------------------------------------------------------------
-- Specialized folds
------------------------------------------------------------------------------

{-# INLINE head #-}
head :: Monad m => Stream m a -> m (Maybe a)
-- head = foldrM (\x _ -> return $ Just x) (return Nothing)
head m =
    let stop      = return Nothing
        single a  = return (Just a)
        yieldk a _ = return (Just a)
    in foldStream defState yieldk single stop m

{-# INLINE elem #-}
elem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
elem e = go
    where
    go m1 =
        let stop      = return False
            single a  = return (a == e)
            yieldk a r = if a == e then return True else go r
        in foldStream defState yieldk single stop m1

{-# INLINE notElem #-}
notElem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
notElem e = go
    where
    go m1 =
        let stop      = return True
            single a  = return (a /= e)
            yieldk a r = if a == e then return False else go r
        in foldStream defState yieldk single stop m1

{-# INLINABLE all #-}
all :: Monad m => (a -> Bool) -> Stream m a -> m Bool
all p = go
    where
    go m1 =
        let single a   | p a       = return True
                       | otherwise = return False
            yieldk a r | p a       = go r
                       | otherwise = return False
         in foldStream defState yieldk single (return True) m1

{-# INLINABLE any #-}
any :: Monad m => (a -> Bool) -> Stream m a -> m Bool
any p = go
    where
    go m1 =
        let single a   | p a       = return True
                       | otherwise = return False
            yieldk a r | p a       = return True
                       | otherwise = go r
         in foldStream defState yieldk single (return False) m1

-- | Extract the last element of the stream, if any.
{-# INLINE last #-}
last :: Monad m => Stream m a -> m (Maybe a)
last = foldlx' (\_ y -> Just y) Nothing id

{-# INLINE minimum #-}
minimum :: (Monad m, Ord a) => Stream m a -> m (Maybe a)
minimum = go Nothing
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in foldStream defState yieldk single stop m1

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  =
                if res <= a
                then return (Just res)
                else return (Just a)
            yieldk a r =
                if res <= a
                then go (Just res) r
                else go (Just a) r
        in foldStream defState yieldk single stop m1

{-# INLINE minimumBy #-}
minimumBy
    :: (Monad m)
    => (a -> a -> Ordering) -> Stream m a -> m (Maybe a)
minimumBy cmp = go Nothing
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in foldStream defState yieldk single stop m1

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  = case cmp res a of
                GT -> return (Just a)
                _  -> return (Just res)
            yieldk a r = case cmp res a of
                GT -> go (Just a) r
                _  -> go (Just res) r
        in foldStream defState yieldk single stop m1

{-# INLINE maximum #-}
maximum :: (Monad m, Ord a) => Stream m a -> m (Maybe a)
maximum = go Nothing
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in foldStream defState yieldk single stop m1

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  =
                if res <= a
                then return (Just a)
                else return (Just res)
            yieldk a r =
                if res <= a
                then go (Just a) r
                else go (Just res) r
        in foldStream defState yieldk single stop m1

{-# INLINE maximumBy #-}
maximumBy :: Monad m => (a -> a -> Ordering) -> Stream m a -> m (Maybe a)
maximumBy cmp = go Nothing
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in foldStream defState yieldk single stop m1

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  = case cmp res a of
                GT -> return (Just res)
                _  -> return (Just a)
            yieldk a r = case cmp res a of
                GT -> go (Just res) r
                _  -> go (Just a) r
        in foldStream defState yieldk single stop m1

{-# INLINE (!!) #-}
(!!) :: Monad m => Stream m a -> Int -> m (Maybe a)
m !! i = go i m
    where
    go n m1 =
      let single a | n == 0 = return $ Just a
                   | otherwise = return Nothing
          yieldk a x | n < 0 = return Nothing
                     | n == 0 = return $ Just a
                     | otherwise = go (n - 1) x
      in foldStream defState yieldk single (return Nothing) m1

{-# INLINE lookup #-}
lookup :: (Monad m, Eq a) => a -> Stream m (a, b) -> m (Maybe b)
lookup e = go
    where
    go m1 =
        let single (a, b) | a == e = return $ Just b
                          | otherwise = return Nothing
            yieldk (a, b) x | a == e = return $ Just b
                            | otherwise = go x
        in foldStream defState yieldk single (return Nothing) m1

{-# INLINE findM #-}
findM :: Monad m => (a -> m Bool) -> Stream m a -> m (Maybe a)
findM p = go
    where
    go m1 =
        let single a = do
                b <- p a
                if b then return $ Just a else return Nothing
            yieldk a x = do
                b <- p a
                if b then return $ Just a else go x
        in foldStream defState yieldk single (return Nothing) m1

{-# INLINE find #-}
find :: Monad m => (a -> Bool) -> Stream m a -> m (Maybe a)
find p = findM (return . p)

{-# INLINE findIndices #-}
findIndices :: (a -> Bool) -> Stream m a -> Stream m Int
findIndices p = go 0
    where
    go offset m1 = mkStream $ \st yld sng stp ->
        let single a | p a = sng offset
                     | otherwise = stp
            yieldk a x | p a = yld offset $ go (offset + 1) x
                       | otherwise = foldStream (adaptState st) yld sng stp $
                            go (offset + 1) x
        in foldStream (adaptState st) yieldk single stp m1

------------------------------------------------------------------------------
-- Map and Fold
------------------------------------------------------------------------------

-- | Apply a monadic action to each element of the stream and discard the
-- output of the action.
{-# INLINE mapM_ #-}
mapM_ :: Monad m => (a -> m b) -> Stream m a -> m ()
mapM_ f = go
    where
    go m1 =
        let stop = return ()
            single a = void (f a)
            yieldk a r = f a >> go r
         in foldStream defState yieldk single stop m1

{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
mapM = mapMWith consM

------------------------------------------------------------------------------
-- Converting folds
------------------------------------------------------------------------------

{-# INLINABLE toList #-}
toList :: Monad m => Stream m a -> m [a]
toList = foldr (:) []

-- Based on suggestions by David Feuer and Pranay Sashank
{-# INLINE hoist #-}
hoist :: (Monad m, Monad n)
    => (forall x. m x -> n x) -> Stream m a -> Stream n a
hoist f str =
    mkStream $ \st yld sng stp ->
            let single = return . sng
                yieldk a s = return $ yld a (hoist f s)
                stop = return stp
                state = adaptState st
             in join . f $ foldStreamShared state yieldk single stop str

-------------------------------------------------------------------------------
-- Transformation by folding (Scans)
-------------------------------------------------------------------------------

{-# INLINE scanlx' #-}
scanlx' :: (x -> a -> x) -> x -> (x -> b) -> Stream m a -> Stream m b
scanlx' step begin done m =
    cons (done begin) $ go m begin
    where
    go m1 !acc = mkStream $ \st yld sng stp ->
        let single a = sng (done $ step acc a)
            yieldk a r =
                let s = step acc a
                in yld (done s) (go r s)
        in foldStream (adaptState st) yieldk single stp m1

{-# INLINE scanl' #-}
scanl' :: (b -> a -> b) -> b -> Stream m a -> Stream m b
scanl' step begin = scanlx' step begin id

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE filter #-}
filter :: (a -> Bool) -> Stream m a -> Stream m a
filter p = go
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single a   | p a       = sng a
                       | otherwise = stp
            yieldk a r | p a       = yld a (go r)
                       | otherwise = foldStream st yieldk single stp r
         in foldStream st yieldk single stp m1

{-# INLINE take #-}
take :: Int -> Stream m a -> Stream m a
take = go
    where
    go n1 m1 = mkStream $ \st yld sng stp ->
        let yieldk a r = yld a (go (n1 - 1) r)
        in if n1 <= 0
           then stp
           else foldStream st yieldk sng stp m1

{-# INLINE takeWhile #-}
takeWhile :: (a -> Bool) -> Stream m a -> Stream m a
takeWhile p = go
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single a   | p a       = sng a
                       | otherwise = stp
            yieldk a r | p a       = yld a (go r)
                       | otherwise = stp
         in foldStream st yieldk single stp m1

{-# INLINE drop #-}
drop :: Int -> Stream m a -> Stream m a
drop n m = unShare (go n m)
    where
    go n1 m1 = mkStream $ \st yld sng stp ->
        let single _ = stp
            yieldk _ r = foldStreamShared st yld sng stp $ go (n1 - 1) r
        -- Somehow "<=" check performs better than a ">"
        in if n1 <= 0
           then foldStreamShared st yld sng stp m1
           else foldStreamShared st yieldk single stp m1

{-# INLINE dropWhile #-}
dropWhile :: (a -> Bool) -> Stream m a -> Stream m a
dropWhile p = go
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single a   | p a       = stp
                       | otherwise = sng a
            yieldk a r | p a = foldStream st yieldk single stp r
                       | otherwise = yld a r
         in foldStream st yieldk single stp m1

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

-- Be careful when modifying this, this uses a consM (|:) deliberately to allow
-- other stream types to overload it.
{-# INLINE sequence #-}
sequence :: Monad m => Stream m (m a) -> Stream m a
sequence = go
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single ma = ma >>= sng
            yieldk ma r = foldStreamShared st yld sng stp $ ma `consM` go r
         in foldStream (adaptState st) yieldk single stp m1

-------------------------------------------------------------------------------
-- Inserting
-------------------------------------------------------------------------------

{-# INLINE intersperseM #-}
intersperseM :: Monad m => m a -> Stream m a -> Stream m a
intersperseM a = prependingStart
    where
    prependingStart m1 = mkStream $ \st yld sng stp ->
        let yieldk i x =
                foldStreamShared st yld sng stp $ return i `consM` go x
         in foldStream st yieldk sng stp m1
    go m2 = mkStream $ \st yld sng stp ->
        let single i = foldStreamShared st yld sng stp $ a `consM` fromPure i
            yieldk i x =
                foldStreamShared
                    st yld sng stp $ a `consM` return i `consM` go x
         in foldStream st yieldk single stp m2

{-# INLINE intersperse #-}
intersperse :: Monad m => a -> Stream m a -> Stream m a
intersperse a = intersperseM (return a)

{-# INLINE insertBy #-}
insertBy :: (a -> a -> Ordering) -> a -> Stream m a -> Stream m a
insertBy cmp x = go
  where
    go m1 = mkStream $ \st yld _ _ ->
        let single a = case cmp x a of
                GT -> yld a (fromPure x)
                _  -> yld x (fromPure a)
            stop = yld x nil
            yieldk a r = case cmp x a of
                GT -> yld a (go r)
                _  -> yld x (a `cons` r)
         in foldStream st yieldk single stop m1

------------------------------------------------------------------------------
-- Deleting
------------------------------------------------------------------------------

{-# INLINE deleteBy #-}
deleteBy :: (a -> a -> Bool) -> a -> Stream m a -> Stream m a
deleteBy eq x = go
  where
    go m1 = mkStream $ \st yld sng stp ->
        let single a = if eq x a then stp else sng a
            yieldk a r = if eq x a
              then foldStream st yld sng stp r
              else yld a (go r)
         in foldStream st yieldk single stp m1

-------------------------------------------------------------------------------
-- Map and Filter
-------------------------------------------------------------------------------

{-# INLINE mapMaybe #-}
mapMaybe :: (a -> Maybe b) -> Stream m a -> Stream m b
mapMaybe f = go
  where
    go m1 = mkStream $ \st yld sng stp ->
        let single a = maybe stp sng (f a)
            yieldk a r = case f a of
                Just b  -> yld b $ go r
                Nothing -> foldStream (adaptState st) yieldk single stp r
        in foldStream (adaptState st) yieldk single stp m1

------------------------------------------------------------------------------
-- Serial Zipping
------------------------------------------------------------------------------

-- | Zip two streams serially using a pure zipping function.
--
-- @since 0.1.0
{-# INLINE zipWith #-}
zipWith :: Monad m => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWith f = zipWithM (\a b -> return (f a b))

-- | Zip two streams serially using a monadic zipping function.
--
-- @since 0.1.0
{-# INLINE zipWithM #-}
zipWithM :: Monad m =>
    (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
zipWithM f = go

    where

    go mx my = mkStream $ \st yld sng stp -> do
        let merge a ra =
                let single2 b   = f a b >>= sng
                    yield2 b rb = f a b >>= \x -> yld x (go ra rb)
                 in foldStream (adaptState st) yield2 single2 stp my
        let single1 a = merge a nil
            yield1 = merge
        foldStream (adaptState st) yield1 single1 stp mx

------------------------------------------------------------------------------
-- Merging
------------------------------------------------------------------------------

{-# INLINE mergeByM #-}
mergeByM :: Monad m =>
    (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeByM cmp = go

    where

    go mx my = mkStream $ \st yld sng stp -> do
        let stop = foldStream st yld sng stp my
            single x = foldStream st yld sng stp (goX0 x my)
            yield x rx = foldStream st yld sng stp (goX x rx my)
        foldStream st yield single stop mx

    goX0 x my = mkStream $ \st yld sng _ -> do
        let stop = sng x
            single y = do
                r <- cmp x y
                case r of
                    GT -> yld y (fromPure x)
                    _  -> yld x (fromPure y)
            yield y ry = do
                r <- cmp x y
                case r of
                    GT -> yld y (goX0 x ry)
                    _  -> yld x (y `cons` ry)
         in foldStream st yield single stop my

    goX x mx my = mkStream $ \st yld _ _ -> do
        let stop = yld x mx
            single y = do
                r <- cmp x y
                case r of
                    GT -> yld y (x `cons` mx)
                    _  -> yld x (goY0 mx y)
            yield y ry = do
                r <- cmp x y
                case r of
                    GT -> yld y (goX x mx ry)
                    _  -> yld x (goY mx y ry)
         in foldStream st yield single stop my

    goY0 mx y = mkStream $ \st yld sng _ -> do
        let stop = sng y
            single x = do
                r <- cmp x y
                case r of
                    GT -> yld y (fromPure x)
                    _  -> yld x (fromPure y)
            yield x rx = do
                r <- cmp x y
                case r of
                    GT -> yld y (x `cons` rx)
                    _  -> yld x (goY0 rx y)
         in foldStream st yield single stop mx

    goY mx y my = mkStream $ \st yld _ _ -> do
        let stop = yld y my
            single x = do
                r <- cmp x y
                case r of
                    GT -> yld y (goX0 x my)
                    _  -> yld x (y `cons` my)
            yield x rx = do
                r <- cmp x y
                case r of
                    GT -> yld y (goX x rx my)
                    _  -> yld x (goY rx y my)
         in foldStream st yield single stop mx

{-# INLINE mergeBy #-}
mergeBy :: (a -> a -> Ordering) -> Stream m a -> Stream m a -> Stream m a
-- XXX GHC: This has slightly worse performance than replacing "r <- cmp x y"
-- with "let r = cmp x y" in the monadic version. The definition below is
-- exactly the same as mergeByM except this change.
-- mergeBy cmp = mergeByM (\a b -> return $ cmp a b)
mergeBy cmp = go

    where

    go mx my = mkStream $ \st yld sng stp -> do
        let stop = foldStream st yld sng stp my
            single x = foldStream st yld sng stp (goX0 x my)
            yield x rx = foldStream st yld sng stp (goX x rx my)
        foldStream st yield single stop mx

    goX0 x my = mkStream $ \st yld sng _ -> do
        let stop = sng x
            single y = do
                case cmp x y of
                    GT -> yld y (fromPure x)
                    _  -> yld x (fromPure y)
            yield y ry = do
                case cmp x y of
                    GT -> yld y (goX0 x ry)
                    _  -> yld x (y `cons` ry)
         in foldStream st yield single stop my

    goX x mx my = mkStream $ \st yld _ _ -> do
        let stop = yld x mx
            single y = do
                case cmp x y of
                    GT -> yld y (x `cons` mx)
                    _  -> yld x (goY0 mx y)
            yield y ry = do
                case cmp x y of
                    GT -> yld y (goX x mx ry)
                    _  -> yld x (goY mx y ry)
         in foldStream st yield single stop my

    goY0 mx y = mkStream $ \st yld sng _ -> do
        let stop = sng y
            single x = do
                case cmp x y of
                    GT -> yld y (fromPure x)
                    _  -> yld x (fromPure y)
            yield x rx = do
                case cmp x y of
                    GT -> yld y (x `cons` rx)
                    _  -> yld x (goY0 rx y)
         in foldStream st yield single stop mx

    goY mx y my = mkStream $ \st yld _ _ -> do
        let stop = yld y my
            single x = do
                case cmp x y of
                    GT -> yld y (goX0 x my)
                    _  -> yld x (y `cons` my)
            yield x rx = do
                case cmp x y of
                    GT -> yld y (goX x rx my)
                    _  -> yld x (goY rx y my)
         in foldStream st yield single stop mx

------------------------------------------------------------------------------
-- Transformation comprehensions
------------------------------------------------------------------------------

{-# INLINE the #-}
the :: (Eq a, Monad m) => Stream m a -> m (Maybe a)
the m = do
    r <- uncons m
    case r of
        Nothing -> return Nothing
        Just (h, t) -> go h t
    where
    go h m1 =
        let single a   | h == a    = return $ Just h
                       | otherwise = return Nothing
            yieldk a r | h == a    = go h r
                       | otherwise = return Nothing
         in foldStream defState yieldk single (return $ Just h) m1

------------------------------------------------------------------------------
-- Alternative & MonadPlus
------------------------------------------------------------------------------

_alt :: Stream m a -> Stream m a -> Stream m a
_alt m1 m2 = mkStream $ \st yld sng stp ->
    let stop  = foldStream st yld sng stp m2
    in foldStream st yld sng stop m1

------------------------------------------------------------------------------
-- MonadError
------------------------------------------------------------------------------

{-
-- XXX handle and test cross thread state transfer
withCatchError
    :: MonadError e m
    => Stream m a -> (e -> Stream m a) -> Stream m a
withCatchError m h =
    mkStream $ \_ stp sng yld ->
        let run x = unStream x Nothing stp sng yieldk
            handle r = r `catchError` \e -> run $ h e
            yieldk a r = yld a (withCatchError r h)
        in handle $ run m
-}

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- Inlined definition.
{-# INLINE splitAt #-}
splitAt :: Int -> [a] -> ([a],[a])
splitAt n ls
  | n <= 0 = ([], ls)
  | otherwise          = splitAt' n ls
    where
        splitAt' :: Int -> [a] -> ([a], [a])
        splitAt' _  []     = ([], [])
        splitAt' 1  (x:xs) = ([x], xs)
        splitAt' m  (x:xs) = (x:xs', xs'')
          where
            (xs', xs'') = splitAt' (m - 1) xs

-- | Run a 'Parser' over a stream and return rest of the Stream.
{-# INLINE_NORMAL parseBreakD #-}
parseBreakD
    :: Monad m
    => PR.Parser a m b
    -> Stream m a
    -> m (Either ParseError b, Stream m a)
parseBreakD (PR.Parser pstep initial extract) stream = do
    res <- initial
    case res of
        PR.IPartial s -> goStream stream [] s
        PR.IDone b -> return (Right b, stream)
        PR.IError err -> return (Left (ParseError err), stream)

    where

    -- "buf" contains last few items in the stream that we may have to
    -- backtrack to.
    --
    -- XXX currently we are using a dumb list based approach for backtracking
    -- buffer. This can be replaced by a sliding/ring buffer using Data.Array.
    -- That will allow us more efficient random back and forth movement.
    goStream st buf !pst =
        let stop = do
                r <- extract pst
                case r of
                    PR.Error err -> return (Left (ParseError err), nil)
                    PR.Done n b -> do
                        assertM(n <= length buf)
                        let src0 = Prelude.take n buf
                            src  = Prelude.reverse src0
                        return (Right b, fromList src)
                    PR.Partial _ _ -> error "Bug: parseBreak: Partial in extract"
                    PR.Continue 0 s -> goStream nil buf s
                    PR.Continue n s -> do
                        assertM(n <= length buf)
                        let (src0, buf1) = splitAt n buf
                            src = Prelude.reverse src0
                        goBuf nil buf1 src s
            single x = yieldk x nil
            yieldk x r = do
                res <- pstep pst x
                case res of
                    PR.Partial 0 s -> goStream r [] s
                    PR.Partial n s -> do
                        assertM(n <= length (x:buf))
                        let src0 = Prelude.take n (x:buf)
                            src  = Prelude.reverse src0
                        goBuf r [] src s
                    PR.Continue 0 s -> goStream r (x:buf) s
                    PR.Continue n s -> do
                        assertM(n <= length (x:buf))
                        let (src0, buf1) = splitAt n (x:buf)
                            src = Prelude.reverse src0
                        goBuf r buf1 src s
                    PR.Done 0 b -> return (Right b, r)
                    PR.Done n b -> do
                        assertM(n <= length (x:buf))
                        let src0 = Prelude.take n (x:buf)
                            src  = Prelude.reverse src0
                        return (Right b, append (fromList src) r)
                    PR.Error err -> return (Left (ParseError err), r)
         in foldStream defState yieldk single stop st

    goBuf st buf [] !pst = goStream st buf pst
    goBuf st buf (x:xs) !pst = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 s -> goBuf st [] xs s
            PR.Partial n s -> do
                assert (n <= length (x:buf)) (return ())
                let src0 = Prelude.take n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                goBuf st [] src s
            PR.Continue 0 s -> goBuf st (x:buf) xs s
            PR.Continue n s -> do
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                goBuf st buf1 src s
            PR.Done n b -> do
                assert (n <= length (x:buf)) (return ())
                let src0 = Prelude.take n (x:buf)
                    src  = Prelude.reverse src0
                return (Right b, append (fromList src) st)
            PR.Error err -> return (Left (ParseError err), nil)

-- | Parse a stream using the supplied 'Parser'.
--
-- /CPS/
--
{-# INLINE parseBreak #-}
parseBreak :: Monad m =>
    Parser.Parser a m b -> Stream m a -> m (Either ParseError b, Stream m a)
parseBreak p strm = fmap f $ parseBreakD (PR.fromParserK p) (toStreamK strm)

    where

    f (b, str) = (b, fromStreamK str)

-- | Sort the input stream using a supplied comparison function.
--
-- Sorting can be achieved by simply:
--
-- >>> sortBy cmp = Stream.mergeMapWith (Stream.mergeBy cmp) Stream.fromPure
--
-- However, this combinator uses a parser to first split the input stream into
-- down and up sorted segments and then merges them to optimize sorting when
-- pre-sorted sequences exist in the input stream..
--
-- /O(n) space/
--
{-# INLINE sortBy #-}
sortBy :: Monad m => (a -> a -> Ordering) -> Stream m a -> Stream m a
-- sortBy f = Stream.concatPairsWith (Stream.mergeBy f) Stream.fromPure
sortBy cmp =
    let p =
            Parser.groupByRollingEither
                (\x -> (< GT) . cmp x)
                FL.toStreamKRev
                FL.toStreamK
     in   mergeMapWith (mergeBy cmp) id
        . Stream.toStreamK
        . Stream.catRights -- its a non-failing backtracking parser
        . Stream.parseMany (fmap (either id id) p)
        . Stream.fromStreamK
