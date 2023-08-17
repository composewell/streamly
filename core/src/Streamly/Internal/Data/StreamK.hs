{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.StreamK
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.StreamK
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * The stream type
      module Streamly.Internal.Data.Stream.StreamK.Type
    , module Streamly.Internal.Data.Stream.StreamK.Transformer

    , StreamK(..)
    , fromList
    , fromStream
    , toStream

    -- ** Specialized Generation
    , repeatM
    , replicate
    , replicateM
    , fromIndices
    , fromIndicesM
    , iterate
    , iterateM

    -- * Elimination
    -- ** General Folds
    , foldr1
    , foldlM'
    , foldlMx'
    , fold
    , foldBreak
    , foldEither
    , foldConcat
    , parseDBreak
    , parseD
    , parseBreakChunks
    , parseChunks
    , parseBreak
    , parse
    , parseBreakChunksGeneric
    , parseChunksGeneric

    -- ** Specialized Folds
    , head
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
    , mapM
    , sequence

    -- ** Inserting
    , intersperseM
    , intersperse
    , insertBy

    -- ** Deleting
    , deleteBy

    -- ** Reordering
    , sortBy

    -- ** Map and Filter
    , mapMaybe

    -- ** Zipping
    , zipWith
    , zipWithM

    -- ** Merging
    , mergeBy
    , mergeByM

    -- ** Transformation comprehensions
    , the

    -- * Exceptions
    , handle

    -- * Resource Management
    , bracketIO
    )
where

#include "ArrayMacros.h"
#include "inline.hs"
#include "assert.hs"

import Control.Exception (mask_, Exception)
import Control.Monad (void, join)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Proxy (Proxy(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.IOFinalizer (newIOFinalizer, runIOFinalizer)
import Streamly.Internal.Data.Parser.ParserK.Type (ParserK)
import Streamly.Internal.Data.Producer.Type (Producer(..))
import Streamly.Internal.Data.SVar.Type (adaptState, defState)
import Streamly.Internal.Data.Unbox (sizeOf, Unbox)

import qualified Control.Monad.Catch as MC
import qualified Streamly.Internal.Data.Array.Type as Array
import qualified Streamly.Internal.Data.Array.Generic as GenArr
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Parser.ParserD.Type as PR
import qualified Streamly.Internal.Data.Parser.ParserK.Type as ParserK
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Prelude

import Prelude
       hiding (Foldable(..), last, map, mapM, mapM_, repeat, sequence,
               take, filter, all, any, takeWhile, drop, dropWhile,
               notElem, head, tail, init, zipWith, lookup,
               (!!), replicate, reverse, concatMap, iterate, splitAt)
import Data.Foldable (sum, length)
import Streamly.Internal.Data.Stream.StreamK.Type
import Streamly.Internal.Data.Stream.StreamK.Transformer
import Streamly.Internal.Data.Parser (ParseError(..))

#include "DocTestDataStreamK.hs"

-- | Convert a fused 'Stream' to 'StreamK'.
--
-- For example:
--
-- >>> s1 = StreamK.fromStream $ Stream.fromList [1,2]
-- >>> s2 = StreamK.fromStream $ Stream.fromList [3,4]
-- >>> Stream.fold Fold.toList $ StreamK.toStream $ s1 `StreamK.append` s2
-- [1,2,3,4]
--
{-# INLINE fromStream #-}
fromStream :: Monad m => Stream.Stream m a -> StreamK m a
fromStream = Stream.toStreamK

-- | Convert a 'StreamK' to a fused 'Stream'.
--
{-# INLINE toStream #-}
toStream :: Applicative m => StreamK m a -> Stream.Stream m a
toStream = Stream.fromStreamK

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

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
unfoldrA :: (b -> Maybe (m a, b)) -> b -> StreamK m a
unfoldrA = undefined
-}

-------------------------------------------------------------------------------
-- Special generation
-------------------------------------------------------------------------------

-- |
-- >>> repeatM = StreamK.sequence . StreamK.repeat
-- >>> repeatM = fix . StreamK.consM
-- >>> repeatM = cycle1 . StreamK.fromEffect
--
-- Generate a stream by repeatedly executing a monadic action forever.
--
-- >>> :{
-- repeatAction =
--        StreamK.repeatM (threadDelay 1000000 >> print 1)
--      & StreamK.take 10
--      & StreamK.fold Fold.drain
-- :}
--
repeatM :: Monad m => m a -> StreamK m a
repeatM = repeatMWith consM

{-# INLINE replicateM #-}
replicateM :: Monad m => Int -> m a -> StreamK m a
replicateM = replicateMWith consM
{-# INLINE replicate #-}
replicate :: Int -> a -> StreamK m a
replicate n a = go n
    where
    go cnt = if cnt <= 0 then nil else a `cons` go (cnt - 1)

{-# INLINE fromIndicesM #-}
fromIndicesM :: Monad m => (Int -> m a) -> StreamK m a
fromIndicesM = fromIndicesMWith consM
{-# INLINE fromIndices #-}
fromIndices :: (Int -> a) -> StreamK m a
fromIndices gen = go 0
  where
    go n = gen n `cons` go (n + 1)

-- |
-- >>> iterate f x = x `StreamK.cons` iterate f x
--
-- Generate an infinite stream with @x@ as the first element and each
-- successive element derived by applying the function @f@ on the previous
-- element.
--
-- >>> StreamK.toList $ StreamK.take 5 $ StreamK.iterate (+1) 1
-- [1,2,3,4,5]
--
{-# INLINE iterate #-}
iterate :: (a -> a) -> a -> StreamK m a
iterate step = go
    where
        go !s = cons s (go (step s))

-- |
-- >>> iterateM f m = m >>= \a -> return a `StreamK.consM` iterateM f (f a)
--
-- Generate an infinite stream with the first element generated by the action
-- @m@ and each successive element derived by applying the monadic function
-- @f@ on the previous element.
--
-- >>> :{
-- StreamK.iterateM (\x -> print x >> return (x + 1)) (return 0)
--     & StreamK.take 3
--     & StreamK.toList
-- :}
-- 0
-- 1
-- [0,1,2]
--
{-# INLINE iterateM #-}
iterateM :: Monad m => (a -> m a) -> m a -> StreamK m a
iterateM = iterateMWith consM

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

{-# INLINE fromList #-}
fromList :: [a] -> StreamK m a
fromList = fromFoldable

-------------------------------------------------------------------------------
-- Elimination by Folding
-------------------------------------------------------------------------------

{-# INLINE foldr1 #-}
foldr1 :: Monad m => (a -> a -> a) -> StreamK m a -> m (Maybe a)
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
    => (x -> a -> m x) -> m x -> (x -> m b) -> StreamK m a -> m b
foldlMx' step begin done = go begin
    where
    go !acc m1 =
        let stop = acc >>= done
            single a = acc >>= \b -> step b a >>= done
            yieldk a r = acc >>= \b -> step b a >>= \x -> go (return x) r
         in foldStream defState yieldk single stop m1

-- | Fold a stream using the supplied left 'Fold' and reducing the resulting
-- expression strictly at each step. The behavior is similar to 'foldl''. A
-- 'Fold' can terminate early without consuming the full stream. See the
-- documentation of individual 'Fold's for termination behavior.
--
-- Definitions:
--
-- >>> fold f = fmap fst . StreamK.foldBreak f
-- >>> fold f = StreamK.parseD (Parser.fromFold f)
--
-- Example:
--
-- >>> StreamK.fold Fold.sum $ StreamK.fromStream $ Stream.enumerateFromTo 1 100
-- 5050
--
{-# INLINABLE fold #-}
fold :: Monad m => FL.Fold m a b -> StreamK m a -> m b
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

-- | Fold resulting in either breaking the stream or continuation of the fold.
-- Instead of supplying the input stream in one go we can run the fold multiple
-- times, each time supplying the next segment of the input stream. If the fold
-- has not yet finished it returns a fold that can be run again otherwise it
-- returns the fold result and the residual stream.
--
-- /Internal/
{-# INLINE foldEither #-}
foldEither :: Monad m =>
    Fold m a b -> StreamK m a -> m (Either (Fold m a b) (b, StreamK m a))
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

-- | Like 'fold' but also returns the remaining stream. The resulting stream
-- would be 'StreamK.nil' if the stream finished before the fold.
--
{-# INLINE foldBreak #-}
foldBreak :: Monad m => Fold m a b -> StreamK m a -> m (b, StreamK m a)
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

-- XXX Array folds can be implemented using this.
-- foldContainers? Specialized to foldArrays.

-- | Generate streams from individual elements of a stream and fold the
-- concatenation of those streams using the supplied fold. Return the result of
-- the fold and residual stream.
--
-- For example, this can be used to efficiently fold an Array Word8 stream
-- using Word8 folds.
--
-- /Internal/
{-# INLINE foldConcat #-}
foldConcat :: Monad m =>
    Producer m a b -> Fold m b c -> StreamK m a -> m (c, StreamK m a)
foldConcat
    (Producer pstep pinject pextract)
    (Fold fstep begin done)
    stream = do

    res <- begin
    case res of
        FL.Partial fs -> go fs stream
        FL.Done fb -> return (fb, stream)

    where

    go !acc m1 = do
        let stop = do
                r <- done acc
                return (r, nil)
            single a = do
                st <- pinject a
                res <- go1 SPEC acc st
                case res of
                    Left fs -> do
                        r <- done fs
                        return (r, nil)
                    Right (b, s) -> do
                        x <- pextract s
                        return (b, fromPure x)
            yieldk a r = do
                st <- pinject a
                res <- go1 SPEC acc st
                case res of
                    Left fs -> go fs r
                    Right (b, s) -> do
                        x <- pextract s
                        return (b, x `cons` r)
         in foldStream defState yieldk single stop m1

    {-# INLINE go1 #-}
    go1 !_ !fs st = do
        r <- pstep st
        case r of
            Stream.Yield x s -> do
                res <- fstep fs x
                case res of
                    FL.Done b -> return $ Right (b, s)
                    FL.Partial fs1 -> go1 SPEC fs1 s
            Stream.Skip s -> go1 SPEC fs s
            Stream.Stop -> return $ Left fs

-- | Like 'foldl'' but with a monadic step function.
{-# INLINE foldlM' #-}
foldlM' :: Monad m => (b -> a -> m b) -> m b -> StreamK m a -> m b
foldlM' step begin = foldlMx' step begin return

------------------------------------------------------------------------------
-- Specialized folds
------------------------------------------------------------------------------

{-# INLINE head #-}
head :: Monad m => StreamK m a -> m (Maybe a)
-- head = foldrM (\x _ -> return $ Just x) (return Nothing)
head m =
    let stop      = return Nothing
        single a  = return (Just a)
        yieldk a _ = return (Just a)
    in foldStream defState yieldk single stop m

{-# INLINE elem #-}
elem :: (Monad m, Eq a) => a -> StreamK m a -> m Bool
elem e = go
    where
    go m1 =
        let stop      = return False
            single a  = return (a == e)
            yieldk a r = if a == e then return True else go r
        in foldStream defState yieldk single stop m1

{-# INLINE notElem #-}
notElem :: (Monad m, Eq a) => a -> StreamK m a -> m Bool
notElem e = go
    where
    go m1 =
        let stop      = return True
            single a  = return (a /= e)
            yieldk a r = if a == e then return False else go r
        in foldStream defState yieldk single stop m1

{-# INLINABLE all #-}
all :: Monad m => (a -> Bool) -> StreamK m a -> m Bool
all p = go
    where
    go m1 =
        let single a   | p a       = return True
                       | otherwise = return False
            yieldk a r | p a       = go r
                       | otherwise = return False
         in foldStream defState yieldk single (return True) m1

{-# INLINABLE any #-}
any :: Monad m => (a -> Bool) -> StreamK m a -> m Bool
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
last :: Monad m => StreamK m a -> m (Maybe a)
last = foldlx' (\_ y -> Just y) Nothing id

{-# INLINE minimum #-}
minimum :: (Monad m, Ord a) => StreamK m a -> m (Maybe a)
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
    => (a -> a -> Ordering) -> StreamK m a -> m (Maybe a)
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
maximum :: (Monad m, Ord a) => StreamK m a -> m (Maybe a)
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
maximumBy :: Monad m => (a -> a -> Ordering) -> StreamK m a -> m (Maybe a)
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
(!!) :: Monad m => StreamK m a -> Int -> m (Maybe a)
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
lookup :: (Monad m, Eq a) => a -> StreamK m (a, b) -> m (Maybe b)
lookup e = go
    where
    go m1 =
        let single (a, b) | a == e = return $ Just b
                          | otherwise = return Nothing
            yieldk (a, b) x | a == e = return $ Just b
                            | otherwise = go x
        in foldStream defState yieldk single (return Nothing) m1

{-# INLINE findM #-}
findM :: Monad m => (a -> m Bool) -> StreamK m a -> m (Maybe a)
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
find :: Monad m => (a -> Bool) -> StreamK m a -> m (Maybe a)
find p = findM (return . p)

{-# INLINE findIndices #-}
findIndices :: (a -> Bool) -> StreamK m a -> StreamK m Int
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
mapM_ :: Monad m => (a -> m b) -> StreamK m a -> m ()
mapM_ f = go
    where
    go m1 =
        let stop = return ()
            single a = void (f a)
            yieldk a r = f a >> go r
         in foldStream defState yieldk single stop m1

{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> StreamK m a -> StreamK m b
mapM = mapMWith consM

------------------------------------------------------------------------------
-- Converting folds
------------------------------------------------------------------------------

{-# INLINABLE toList #-}
toList :: Monad m => StreamK m a -> m [a]
toList = foldr (:) []

-- Based on suggestions by David Feuer and Pranay Sashank
{-# INLINE hoist #-}
hoist :: (Monad m, Monad n)
    => (forall x. m x -> n x) -> StreamK m a -> StreamK n a
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
scanlx' :: (x -> a -> x) -> x -> (x -> b) -> StreamK m a -> StreamK m b
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
scanl' :: (b -> a -> b) -> b -> StreamK m a -> StreamK m b
scanl' step begin = scanlx' step begin id

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE filter #-}
filter :: (a -> Bool) -> StreamK m a -> StreamK m a
filter p = go
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single a   | p a       = sng a
                       | otherwise = stp
            yieldk a r | p a       = yld a (go r)
                       | otherwise = foldStream st yieldk single stp r
         in foldStream st yieldk single stp m1

{-# INLINE take #-}
take :: Int -> StreamK m a -> StreamK m a
take = go
    where
    go n1 m1 = mkStream $ \st yld sng stp ->
        let yieldk a r = yld a (go (n1 - 1) r)
        in if n1 <= 0
           then stp
           else foldStream st yieldk sng stp m1

{-# INLINE takeWhile #-}
takeWhile :: (a -> Bool) -> StreamK m a -> StreamK m a
takeWhile p = go
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single a   | p a       = sng a
                       | otherwise = stp
            yieldk a r | p a       = yld a (go r)
                       | otherwise = stp
         in foldStream st yieldk single stp m1

{-# INLINE drop #-}
drop :: Int -> StreamK m a -> StreamK m a
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
dropWhile :: (a -> Bool) -> StreamK m a -> StreamK m a
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
sequence :: Monad m => StreamK m (m a) -> StreamK m a
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
intersperseM :: Monad m => m a -> StreamK m a -> StreamK m a
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
intersperse :: Monad m => a -> StreamK m a -> StreamK m a
intersperse a = intersperseM (return a)

{-# INLINE insertBy #-}
insertBy :: (a -> a -> Ordering) -> a -> StreamK m a -> StreamK m a
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
deleteBy :: (a -> a -> Bool) -> a -> StreamK m a -> StreamK m a
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
mapMaybe :: (a -> Maybe b) -> StreamK m a -> StreamK m b
mapMaybe f = go
  where
    go m1 = mkStream $ \st yld sng stp ->
        let single a = maybe stp sng (f a)
            yieldk a r = case f a of
                Just b  -> yld b $ go r
                Nothing -> foldStream (adaptState st) yieldk single stp r
        in foldStream (adaptState st) yieldk single stp m1

-------------------------------------------------------------------------------
-- Exception Handling
-------------------------------------------------------------------------------

-- | Like Streamly.Data.Stream.'Streamly.Data.Stream.handle' but with one
-- significant difference, this function observes exceptions from the consumer
-- of the stream as well.
--
-- You can also convert 'StreamK' to 'Stream' and use exception handling from
-- 'Stream' module:
--
-- >>> handle f s = StreamK.fromStream $ Stream.handle (\e -> StreamK.toStream (f e)) (StreamK.toStream s)
--
{-# INLINABLE handle #-}
handle :: (MonadCatch m, Exception e)
    => (e -> m (StreamK m a)) -> StreamK m a -> StreamK m a
handle f stream = go stream

    where

    go m1 = mkStream $ \st yld sng stp ->
        let yieldk a r = yld a $ go r
        in do
            res <- MC.try (foldStream (adaptState st) yieldk sng stp m1)
            case res of
                Right r -> return r
                Left e -> do
                    r <- f e
                    foldStream (adaptState st) yld sng stp r

-------------------------------------------------------------------------------
-- Resource Management
-------------------------------------------------------------------------------

-- If we are folding the stream and we do not drain the entire stream (e.g. if
-- the fold terminates before the stream) then the finalizer will run on GC.
--
-- XXX To implement a prompt cleanup, we will have to yield a cleanup function
-- via the yield continuation. A chain of cleanup functions can be built and
-- the entire chain can be invoked when the stream ends voluntarily or if
-- someone decides to abandon the stream.

-- | Like Streamly.Data.Stream.'Streamly.Data.Stream.bracketIO' but with one
-- significant difference, this function observes exceptions from the consumer
-- of the stream as well. Therefore, it cleans up the resource promptly when
-- the consumer encounters an exception.
--
-- You can also convert 'StreamK' to 'Stream' and use resource handling from
-- 'Stream' module:
--
-- >>> bracketIO bef aft bet = StreamK.fromStream $ Stream.bracketIO bef aft (StreamK.toStream . bet)
--
{-# INLINABLE bracketIO #-}
bracketIO :: (MonadIO m, MonadCatch m)
    => IO b -> (b -> IO c) -> (b -> StreamK m a) -> StreamK m a
bracketIO bef aft bet =
    concatEffect $ do
        (r, ref) <- liftIO $ mask_ $ do
            r <- bef
            ref <- newIOFinalizer (aft r)
            return (r, ref)
        return $ go ref (bet r)

    where

    go ref m1 = mkStream $ \st yld sng stp ->
        let
            -- We can discard exceptions on continuations to make it equivalent
            -- to StreamD, but it seems like a desirable behavior.
            stop = liftIO (runIOFinalizer ref) >> stp
            single a = liftIO (runIOFinalizer ref) >> sng a
            yieldk a r = yld a $ go ref r
        in do
            -- Do not call the finalizer twice if it has already been
            -- called via stop continuation and stop continuation itself
            -- generated an exception. runIOFinalizer takes care of that.
            res <- MC.try (foldStream (adaptState st) yieldk single stop m1)
            case res of
                Right r -> return r
                Left (e :: MC.SomeException) ->
                    liftIO (runIOFinalizer ref) >> MC.throwM e

------------------------------------------------------------------------------
-- Serial Zipping
------------------------------------------------------------------------------

-- | Zipping of @n@ streams can be performed by combining the streams pair
-- wise using 'mergeMapWith' with O(n * log n) time complexity. If used
-- with 'concatMapWith' it will have O(n^2) performance.
{-# INLINE zipWith #-}
zipWith :: Monad m => (a -> b -> c) -> StreamK m a -> StreamK m b -> StreamK m c
zipWith f = zipWithM (\a b -> return (f a b))

{-# INLINE zipWithM #-}
zipWithM :: Monad m =>
    (a -> b -> m c) -> StreamK m a -> StreamK m b -> StreamK m c
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
    (a -> a -> m Ordering) -> StreamK m a -> StreamK m a -> StreamK m a
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

-- | Merging of @n@ streams can be performed by combining the streams pair
-- wise using 'mergeMapWith' to give O(n * log n) time complexity. If used
-- with 'concatMapWith' it will have O(n^2) performance.
--
{-# INLINE mergeBy #-}
mergeBy :: (a -> a -> Ordering) -> StreamK m a -> StreamK m a -> StreamK m a
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
the :: (Eq a, Monad m) => StreamK m a -> m (Maybe a)
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

_alt :: StreamK m a -> StreamK m a -> StreamK m a
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
    => StreamK m a -> (e -> StreamK m a) -> StreamK m a
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
{-# INLINE_NORMAL parseDBreak #-}
parseDBreak
    :: Monad m
    => PR.Parser a m b
    -> StreamK m a
    -> m (Either ParseError b, StreamK m a)
parseDBreak (PR.Parser pstep initial extract) stream = do
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
                    PR.Error err -> do
                        let src = Prelude.reverse buf
                        return (Left (ParseError err), fromList src)
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
                    PR.Error err -> do
                        let src = Prelude.reverse (x:buf)
                        return (Left (ParseError err), append (fromList src) r)
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
            PR.Error err -> do
                let src = Prelude.reverse buf ++ x:xs
                return (Left (ParseError err), append (fromList src) st)

-- Using ParserD or ParserK on StreamK may not make much difference. We should
-- perhaps use only chunked parsing on StreamK. We can always convert a stream
-- to chunks before parsing. Or just have a ParserK element parser for StreamK
-- and convert ParserD to ParserK for element parsing using StreamK.
{-# INLINE parseD #-}
parseD :: Monad m =>
    Parser.Parser a m b -> StreamK m a -> m (Either ParseError b)
parseD f = fmap fst . parseDBreak f

-------------------------------------------------------------------------------
-- ParserK Chunked
-------------------------------------------------------------------------------

-- The backracking buffer consists of arrays in the most-recent-first order. We
-- want to take a total of n array elements from this buffer. Note: when we
-- have to take an array partially, we must take the last part of the array.
{-# INLINE backTrack #-}
backTrack :: forall m a. Unbox a =>
       Int
    -> [Array a]
    -> StreamK m (Array a)
    -> (StreamK m (Array a), [Array a])
backTrack = go

    where

    go _ [] stream = (stream, [])
    go n xs stream | n <= 0 = (stream, xs)
    go n (x:xs) stream =
        let len = Array.length x
        in if n > len
           then go (n - len) xs (cons x stream)
           else if n == len
           then (cons x stream, xs)
           else let !(Array contents start end) = x
                    !start1 = end - (n * SIZE_OF(a))
                    arr1 = Array contents start1 end
                    arr2 = Array contents start start1
                 in (cons arr1 stream, arr2:xs)

-- | A continuation to extract the result when a CPS parser is done.
{-# INLINE parserDone #-}
parserDone :: Applicative m =>
    ParserK.ParseResult b -> Int -> ParserK.Input a -> m (ParserK.Step a m b)
parserDone (ParserK.Success n b) _ _ = pure $ ParserK.Done n b
parserDone (ParserK.Failure n e) _ _ = pure $ ParserK.Error n e

-- XXX parseDBreakChunks may be faster than converting parserD to parserK and
-- using parseBreakChunks. We can also use parseBreak as an alternative to the
-- monad instance of ParserD.

-- | Run a 'ParserK' over a chunked 'StreamK' and return the rest of the Stream.
{-# INLINE_NORMAL parseBreakChunks #-}
parseBreakChunks
    :: (Monad m, Unbox a)
    => ParserK (Array a) m b
    -> StreamK m (Array a)
    -> m (Either ParseError b, StreamK m (Array a))
parseBreakChunks parser input = do
    let parserk = ParserK.runParser parser parserDone 0 0
     in go [] parserk input

    where

    {-# INLINE goStop #-}
    goStop backBuf parserk = do
        pRes <- parserk ParserK.None
        case pRes of
            -- If we stop in an alternative, it will try calling the next
            -- parser, the next parser may call initial returning Partial and
            -- then immediately we have to call extract on it.
            ParserK.Partial 0 cont1 ->
                 go [] cont1 nil
            ParserK.Partial n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map Array.length backBuf))
                let (s1, backBuf1) = backTrack n1 backBuf nil
                 in go backBuf1 cont1 s1
            ParserK.Continue 0 cont1 ->
                go backBuf cont1 nil
            ParserK.Continue n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map Array.length backBuf))
                let (s1, backBuf1) = backTrack n1 backBuf nil
                 in go backBuf1 cont1 s1
            ParserK.Done 0 b ->
                return (Right b, nil)
            ParserK.Done n b -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map Array.length backBuf))
                let (s1, _) = backTrack n1 backBuf nil
                 in return (Right b, s1)
            ParserK.Error _ err -> do
                let (s1, _) = backTrack maxBound backBuf nil
                return (Left (ParseError err), s1)

    seekErr n len =
        error $ "parseBreak: Partial: forward seek not implemented n = "
            ++ show n ++ " len = " ++ show len

    yieldk backBuf parserk arr stream = do
        pRes <- parserk (ParserK.Chunk arr)
        let len = Array.length arr
        case pRes of
            ParserK.Partial n cont1 ->
                case compare n len of
                    EQ -> go [] cont1 stream
                    LT -> do
                        if n >= 0
                        then yieldk [] cont1 arr stream
                        else do
                            let n1 = negate n
                                bufLen = sum (Prelude.map Array.length backBuf)
                                s = cons arr stream
                            assertM(n1 >= 0 && n1 <= bufLen)
                            let (s1, _) = backTrack n1 backBuf s
                            go [] cont1 s1
                    GT -> seekErr n len
            ParserK.Continue n cont1 ->
                case compare n len of
                    EQ -> go (arr:backBuf) cont1 stream
                    LT -> do
                        if n >= 0
                        then yieldk backBuf cont1 arr stream
                        else do
                            let n1 = negate n
                                bufLen = sum (Prelude.map Array.length backBuf)
                                s = cons arr stream
                            assertM(n1 >= 0 && n1 <= bufLen)
                            let (s1, backBuf1) = backTrack n1 backBuf s
                            go backBuf1 cont1 s1
                    GT -> seekErr n len
            ParserK.Done n b -> do
                let n1 = len - n
                assertM(n1 <= sum (Prelude.map Array.length (arr:backBuf)))
                let (s1, _) = backTrack n1 (arr:backBuf) stream
                 in return (Right b, s1)
            ParserK.Error _ err -> do
                let (s1, _) = backTrack maxBound (arr:backBuf) stream
                return (Left (ParseError err), s1)

    go backBuf parserk stream = do
        let stop = goStop backBuf parserk
            single a = yieldk backBuf parserk a nil
         in foldStream
                defState (yieldk backBuf parserk) single stop stream

{-# INLINE parseChunks #-}
parseChunks :: (Monad m, Unbox a) =>
    ParserK (Array a) m b -> StreamK m (Array a) -> m (Either ParseError b)
parseChunks f = fmap fst . parseBreakChunks f

-------------------------------------------------------------------------------
-- ParserK Singular
-------------------------------------------------------------------------------

{-# INLINE backTrackSingular #-}
backTrackSingular :: Int -> [a] -> StreamK m a -> (StreamK m a, [a])
backTrackSingular = go

    where

    go _ [] stream = (stream, [])
    go n xs stream | n <= 0 = (stream, xs)
    go n xs stream =
        let (appendBuf, newBTBuf) = splitAt n xs
         in (append (fromList (Prelude.reverse appendBuf)) stream, newBTBuf)


-- | Similar to 'parseBreak' but works on singular elements.
--
{-# INLINE_NORMAL parseBreak #-}
parseBreak
    :: forall m a b. Monad m
    => ParserK.ParserK a m b
    -> StreamK m a
    -> m (Either ParseError b, StreamK m a)
parseBreak parser input = do
    let parserk = ParserK.runParser parser parserDone 0 0
     in go [] parserk input

    where

    {-# INLINE goStop #-}
    goStop
        :: [a]
        -> (ParserK.Input a -> m (ParserK.Step a m b))
        -> m (Either ParseError b, StreamK m a)
    goStop backBuf parserk = do
        pRes <- parserk ParserK.None
        case pRes of
            -- If we stop in an alternative, it will try calling the next
            -- parser, the next parser may call initial returning Partial and
            -- then immediately we have to call extract on it.
            ParserK.Partial 0 cont1 ->
                 go [] cont1 nil
            ParserK.Partial n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= length backBuf)
                let (s1, backBuf1) = backTrackSingular n1 backBuf nil
                 in go backBuf1 cont1 s1
            ParserK.Continue 0 cont1 ->
                go backBuf cont1 nil
            ParserK.Continue n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= length backBuf)
                let (s1, backBuf1) = backTrackSingular n1 backBuf nil
                 in go backBuf1 cont1 s1
            ParserK.Done 0 b ->
                return (Right b, nil)
            ParserK.Done n b -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= length backBuf)
                let (s1, _) = backTrackSingular n1 backBuf nil
                 in return (Right b, s1)
            ParserK.Error _ err -> return (Left (ParseError err), nil)

    seekErr n =
        error $ "parseBreak: Partial: forward seek not implemented n = "
            ++ show n

    yieldk
        :: [a]
        -> (ParserK.Input a -> m (ParserK.Step a m b))
        -> a
        -> StreamK m a
        -> m (Either ParseError b, StreamK m a)
    yieldk backBuf parserk arr stream = do
        pRes <- parserk (ParserK.Chunk arr)
        case pRes of
            ParserK.Partial 1 cont1 -> go [] cont1 stream
            ParserK.Partial 0 cont1 -> go [] cont1 (cons arr stream)
            ParserK.Partial n _ | n > 1 -> seekErr n
            ParserK.Partial n cont1 -> do
                let n1 = negate n
                    bufLen = length backBuf
                    s = cons arr stream
                assertM(n1 >= 0 && n1 <= bufLen)
                let (s1, _) = backTrackSingular n1 backBuf s
                go [] cont1 s1
            ParserK.Continue 1 cont1 -> go (arr:backBuf) cont1 stream
            ParserK.Continue 0 cont1 ->
                go backBuf cont1 (cons arr stream)
            ParserK.Continue n _ | n > 1 -> seekErr n
            ParserK.Continue n cont1 -> do
                let n1 = negate n
                    bufLen = length backBuf
                    s = cons arr stream
                assertM(n1 >= 0 && n1 <= bufLen)
                let (s1, backBuf1) = backTrackSingular n1 backBuf s
                go backBuf1 cont1 s1
            ParserK.Done 1 b -> pure (Right b, stream)
            ParserK.Done 0 b -> pure (Right b, cons arr stream)
            ParserK.Done n _ | n > 1 -> seekErr n
            ParserK.Done n b -> do
                let n1 = negate n
                    bufLen = length backBuf
                    s = cons arr stream
                assertM(n1 >= 0 && n1 <= bufLen)
                let (s1, _) = backTrackSingular n1 backBuf s
                pure (Right b, s1)
            ParserK.Error _ err -> return (Left (ParseError err), nil)

    go
        :: [a]
        -> (ParserK.Input a -> m (ParserK.Step a m b))
        -> StreamK m a
        -> m (Either ParseError b, StreamK m a)
    go backBuf parserk stream = do
        let stop = goStop backBuf parserk
            single a = yieldk backBuf parserk a nil
         in foldStream
                defState (yieldk backBuf parserk) single stop stream

-- | Run a 'ParserK' over a 'StreamK'. Please use 'parseChunks' where possible,
-- for better performance.
{-# INLINE parse #-}
parse :: Monad m =>
    ParserK.ParserK a m b -> StreamK m a -> m (Either ParseError b)
parse f = fmap fst . parseBreak f

-------------------------------------------------------------------------------
-- ParserK Chunked Generic
-------------------------------------------------------------------------------

{-# INLINE backTrackGenericChunks #-}
backTrackGenericChunks ::
       Int
    -> [GenArr.Array a]
    -> StreamK m (GenArr.Array a)
    -> (StreamK m (GenArr.Array a), [GenArr.Array a])
backTrackGenericChunks = go

    where

    go _ [] stream = (stream, [])
    go n xs stream | n <= 0 = (stream, xs)
    go n (x:xs) stream =
        let len = GenArr.length x
        in if n > len
           then go (n - len) xs (cons x stream)
           else if n == len
           then (cons x stream, xs)
           else let arr1 = GenArr.getSliceUnsafe (len - n) n x
                    arr2 = GenArr.getSliceUnsafe 0 (len - n) x
                 in (cons arr1 stream, arr2:xs)

-- | Similar to 'parseBreak' but works on generic arrays
--
{-# INLINE_NORMAL parseBreakChunksGeneric #-}
parseBreakChunksGeneric
    :: forall m a b. Monad m
    => ParserK.ParserK (GenArr.Array a) m b
    -> StreamK m (GenArr.Array a)
    -> m (Either ParseError b, StreamK m (GenArr.Array a))
parseBreakChunksGeneric parser input = do
    let parserk = ParserK.runParser parser parserDone 0 0
     in go [] parserk input

    where

    {-# INLINE goStop #-}
    goStop
        :: [GenArr.Array a]
        -> (ParserK.Input (GenArr.Array a)
                -> m (ParserK.Step (GenArr.Array a) m b))
        -> m (Either ParseError b, StreamK m (GenArr.Array a))
    goStop backBuf parserk = do
        pRes <- parserk ParserK.None
        case pRes of
            -- If we stop in an alternative, it will try calling the next
            -- parser, the next parser may call initial returning Partial and
            -- then immediately we have to call extract on it.
            ParserK.Partial 0 cont1 ->
                 go [] cont1 nil
            ParserK.Partial n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map GenArr.length backBuf))
                let (s1, backBuf1) = backTrackGenericChunks n1 backBuf nil
                 in go backBuf1 cont1 s1
            ParserK.Continue 0 cont1 ->
                go backBuf cont1 nil
            ParserK.Continue n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map GenArr.length backBuf))
                let (s1, backBuf1) = backTrackGenericChunks n1 backBuf nil
                 in go backBuf1 cont1 s1
            ParserK.Done 0 b ->
                return (Right b, nil)
            ParserK.Done n b -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map GenArr.length backBuf))
                let (s1, _) = backTrackGenericChunks n1 backBuf nil
                 in return (Right b, s1)
            ParserK.Error _ err -> return (Left (ParseError err), nil)

    seekErr n len =
        error $ "parseBreak: Partial: forward seek not implemented n = "
            ++ show n ++ " len = " ++ show len

    yieldk
        :: [GenArr.Array a]
        -> (ParserK.Input (GenArr.Array a)
                -> m (ParserK.Step (GenArr.Array a) m b))
        -> GenArr.Array a
        -> StreamK m (GenArr.Array a)
        -> m (Either ParseError b, StreamK m (GenArr.Array a))
    yieldk backBuf parserk arr stream = do
        pRes <- parserk (ParserK.Chunk arr)
        let len = GenArr.length arr
        case pRes of
            ParserK.Partial n cont1 ->
                case compare n len of
                    EQ -> go [] cont1 stream
                    LT -> do
                        if n >= 0
                        then yieldk [] cont1 arr stream
                        else do
                            let n1 = negate n
                                bufLen = sum (Prelude.map GenArr.length backBuf)
                                s = cons arr stream
                            assertM(n1 >= 0 && n1 <= bufLen)
                            let (s1, _) = backTrackGenericChunks n1 backBuf s
                            go [] cont1 s1
                    GT -> seekErr n len
            ParserK.Continue n cont1 ->
                case compare n len of
                    EQ -> go (arr:backBuf) cont1 stream
                    LT -> do
                        if n >= 0
                        then yieldk backBuf cont1 arr stream
                        else do
                            let n1 = negate n
                                bufLen = sum (Prelude.map GenArr.length backBuf)
                                s = cons arr stream
                            assertM(n1 >= 0 && n1 <= bufLen)
                            let (s1, backBuf1) = backTrackGenericChunks n1 backBuf s
                            go backBuf1 cont1 s1
                    GT -> seekErr n len
            ParserK.Done n b -> do
                let n1 = len - n
                assertM(n1 <= sum (Prelude.map GenArr.length (arr:backBuf)))
                let (s1, _) = backTrackGenericChunks n1 (arr:backBuf) stream
                 in return (Right b, s1)
            ParserK.Error _ err -> return (Left (ParseError err), nil)

    go
        :: [GenArr.Array a]
        -> (ParserK.Input (GenArr.Array a)
                -> m (ParserK.Step (GenArr.Array a) m b))
        -> StreamK m (GenArr.Array a)
        -> m (Either ParseError b, StreamK m (GenArr.Array a))
    go backBuf parserk stream = do
        let stop = goStop backBuf parserk
            single a = yieldk backBuf parserk a nil
         in foldStream
                defState (yieldk backBuf parserk) single stop stream

{-# INLINE parseChunksGeneric #-}
parseChunksGeneric ::
       (Monad m)
    => ParserK.ParserK (GenArr.Array a) m b
    -> StreamK m (GenArr.Array a)
    -> m (Either ParseError b)
parseChunksGeneric f = fmap fst . parseBreakChunksGeneric f

-------------------------------------------------------------------------------
-- Sorting
-------------------------------------------------------------------------------

-- | Sort the input stream using a supplied comparison function.
--
-- Sorting can be achieved by simply:
--
-- >>> sortBy cmp = StreamK.mergeMapWith (StreamK.mergeBy cmp) StreamK.fromPure
--
-- However, this combinator uses a parser to first split the input stream into
-- down and up sorted segments and then merges them to optimize sorting when
-- pre-sorted sequences exist in the input stream.
--
-- /O(n) space/
--
{-# INLINE sortBy #-}
sortBy :: Monad m => (a -> a -> Ordering) -> StreamK m a -> StreamK m a
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
