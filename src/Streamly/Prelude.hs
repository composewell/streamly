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
    , iterate
    , iterateM
    , repeat
    , replicate

    -- * Elimination
    -- ** General Folds
    , foldr
    , foldr1
    , foldrM
    , scan
    , foldl
    , foldl1
    , foldlM
    , uncons

    -- ** Special Folds
    , toList
    , all
    , any
    , head
    , tail
    , last
    , init
    , null
    , length
    , elem
    , notElem
    , reverse
    , maximum
    , minimum
    , sum
    , product
    , lookup
    , find
    , partition
    , and
    , or

    -- * Splitting
    , splitAt
    , span
    , break
    , stripPrefix

    -- * SubStreams
    , isPrefixOf
    , isInfixOf
    , isSuffixOf
    , isSubsequenceOf

    -- * Indices
    , findIndices
    , findIndex
    , elemIndices
    , elemIndex

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
    , intersperse

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
                                              tail, length, null, reverse,
                                              iterate, repeat, replicate,
                                              lookup, splitAt, span, break,
                                              init, foldr1, foldl1, and, or)
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

-- | Iterate a pure function from a seed value, streaming the results forever
iterate :: Streaming t => (a -> a) -> a -> t m a
iterate step = fromStream . go
    where
    go s = scons s (Just (go (step s)))

-- | Iterate a monadic function from a seed value, streaming the results forever
iterateM :: (Streaming t, Monad m) => (a -> m a) -> a -> t m a
iterateM step = fromStream . go
    where
    go s = Stream $ \_ _ yld -> do
       a <- step s
       yld s (Just (go a))

-- | @repeat a@ creates an infinite stream of @a@.
repeat :: (Streaming t) => a -> t m a
repeat a = a .: repeat a

-- | @replicate n a@ creates an @n@ element stream of only @a@.
replicate :: (Streaming t) => Int -> a -> t m a
replicate n a = take n $ repeat a

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

-- | Right fold, with no starting value. Returns Nothing if the list is empty.
foldr1 :: (Streaming t, Monad m) => (a -> a -> a) -> t m a -> m (Maybe a)
foldr1 step m = go (toStream m)
    where
    go m1 =
        let stop = return Nothing
            yield a Nothing = return $ Just a
            yield a (Just x) = go x >>= \b ->
                return $ Just $ maybe_ a (step a) b
        in runStream m1 Nothing stop yield

    maybe_ def _ Nothing = def
    maybe_ _ f (Just v)  = f v

-- | Right fold with a monadic step function.  See 'toList' for an example use.
{-# INLINE foldrM #-}
foldrM :: (Streaming t, Monad m) => (a -> b -> m b) -> b -> t m a -> m b
foldrM step acc m = go (toStream m)
    where
    go m1 =
        let stop = return acc
            yield a Nothing  = step a acc
            yield a (Just x) = (go x) >>= (step a)
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

-- | Strict left fold, which uses the first element as a starting value.
foldl1 :: (Streaming t, Monad m)
    => (a -> a -> a) -> (a -> b) -> t m a -> m (Maybe b)
foldl1 step done m = do
    mbUnconsM <- uncons m
    case mbUnconsM of
        Nothing -> return Nothing
        Just (h, t) -> do
            res <- foldl step h id t
            return $ Just $ done res


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
toList = foldrM (\a xs -> return (a : xs)) []

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

-- | @splitAt n s@ is equivalent to @return (take n s, drop n s)@, but it
-- doesn't traverse the prefix twice.
splitAt :: (Streaming t, Monad m) => Int -> t m a -> m (t m a, t m a)
splitAt total m = go total (toStream m)
    where
    go n m1 | n <= 0 = return (nil, fromStream m1)
            | otherwise =
                let stop             = return (nil, nil)
                    yield a Nothing  = return (a .: nil, nil)
                    yield a (Just x) = go (n - 1) x >>= \(as, bs) ->
                        return (a .: as, bs)
                in runStream m1 Nothing stop yield

-- | @span p s@ returns a tuple, with the first element beeing the prefix of
-- the stream satisfying the predicate. The second tuple element is the
-- remaining stream.
--
-- @span p s == return (takeWhile p s, dropWhile p s)@, but without traversing
-- the prefix twice.
span :: (Streaming t, Monad m) => (a -> Bool) -> t m a -> m (t m a, t m a)
span p m =
    let stop             = return (nil, nil)
        yield a Nothing  | p a = return (a .: nil, nil)
                         | otherwise = return (nil, a .: nil)
        yield a (Just x) | p a = span p (fromStream x) >>=
                                    \(as, bs) -> return (a .: as, bs)
                         | otherwise = return (nil, m)
    in runStream (toStream m) Nothing stop yield

-- | Like span, but returns a prefix not satisfying the given predicate.
--
-- @break p == span (not . p)@
-- @break p m == return (takeWhile (not . p) m, dropWhile (not . p) m)@
break :: (Streaming t, Monad m) => (a -> Bool) -> t m a -> m (t m a, t m a)
break p m =
    let stop             = return (nil, nil)
        yield a Nothing  | p a = return (nil, a .: nil)
                         | otherwise = return (a .: nil, nil)
        yield a (Just x) | p a = return (nil, m)
                         | otherwise = break p (fromStream x) >>=
                                    \(as, bs) -> return (a .: as, bs)
    in runStream (toStream m) Nothing stop yield

-- | Drops the given prefix from the stream. If the stream doesn't start with
-- the given Stream, then it return @Nothing@.
stripPrefix :: (Streaming t, Monad m, Eq a) =>
    t m a -> t m a -> m (Maybe (t m a))
stripPrefix p m = do
    mbUnconsP <- uncons p
    case mbUnconsP of
        Nothing -> return $ Just m
        Just (h, hs) -> do
            mbUnconsM <- uncons m
            case mbUnconsM of
                Nothing -> return Nothing
                Just (x, xs) | x == h -> stripPrefix hs xs
                             | otherwise -> return Nothing

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

-- | Extract all but the first element of the stream, if any.
tail :: (Streaming t, Monad m) => t m a -> m (Maybe (t m a))
tail m =
    let stop             = return Nothing
        yield _ Nothing  = return $ Just nil
        yield _ (Just t) = return $ Just $ fromStream t
    in (runStream (toStream m)) Nothing stop yield

-- | Extract the last element of the stream, if any.
{-# INLINE last #-}
last :: (Streaming t, Monad m) => t m a -> m (Maybe a)
last = foldl (\_ y -> Just y) Nothing id

snull :: Monad m => Stream m a -> m Bool
snull m1 =
    let stop = return True
        yield _ _ = return False
    in runStream m1 Nothing stop yield

-- | Extract all but the last element of a stream, if any.
init :: (Streaming t, Monad m) => t m a -> m (Maybe (t m a))
init m = (fromStream <$>) <$> go (toStream m)
    where
    go m1 =
        let stop = return Nothing
            yield _ Nothing = return $ Just snil
            yield a (Just x) = snull x >>= \n ->
                if n then return $ Just snil
                     else go x >>= \t ->
                         return $ Just (a `scons` t)
        in runStream m1 Nothing stop yield

-- | Determine whether the stream is empty.
null :: (Streaming t, Monad m) => t m a -> m Bool
null m = snull $ toStream m

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

-- | Returns the elements of the stream in reverse order.
-- The stream must be finite.
reverse :: (Streaming t) => t m a -> t m a
reverse m = fromStream $ go Nothing (toStream m)
    where
    go rev rest = Stream $ \svr stp yld ->
        let stop = case rev of
                Nothing ->  stp
                Just str -> runStream str svr stp yld
            yield a Nothing  = runStream (a `scons` rev) svr stp yld
            yield a (Just x) = runStream (go (Just $ a `scons` rev) x) svr stp yld
         in runStream rest svr stop yield

intersperse :: (Streaming t) => a -> t m a -> t m a
intersperse a m = fromStream $ prependingStart (toStream m)
    where
    prependingStart m1 = Stream $ \svr stp yld ->
        let stop             = stp
            yield i Nothing  = yld i Nothing
            yield i (Just x) = yld i $ Just $ go x
         in runStream m1 svr stop yield
    go m2 = Stream $ \svr stp yld ->
        let stop             = stp
            yield i Nothing  = yld a (Just $ i `scons` Nothing)
            yield i (Just x) = yld a (Just $ i `scons` Just (go x))
        in runStream m2 svr stop yield

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

-- | Looks the given key up, treating the given stream as an association list.
lookup :: (Streaming t, Monad m, Eq a) => a -> t m (a, b) -> m (Maybe b)
lookup e m = go (toStream m)
    where
    go m1 =
        let stop                  = return Nothing
            yield (a, b) Nothing  | a == e = return $ Just b
                                  | otherwise = return Nothing
            yield (a, b) (Just x) | a == e = return $ Just b
                                  | otherwise = go x
        in runStream m1 Nothing stop yield

-- | Returns the first element of the stream satisfying the given predicate,
-- if any.
find :: (Streaming t, Monad m) => (a -> Bool) -> t m a -> m (Maybe a)
find p m = go (toStream m)
    where
    go m1 =
        let stop = return Nothing
            yield a Nothing  | p a = return $ Just a
                             | otherwise = return Nothing
            yield a (Just x) | p a = return $ Just a
                             | otherwise = go x
        in runStream m1 Nothing stop yield

-- | Splits the stream into two streams. The first contains all elements
-- satisfying the given predicate. The second contains all other.
--
-- @partition p m == (filter p m, filter (not . p) m)@
partition :: (Streaming t, Monad m) => (a -> Bool) -> t m a -> m (t m a, t m a)
partition p = foldr select (nil, nil)
    where
        select a ~(ps, fs) | p a = (a .: ps, fs)
                           | otherwise = (ps, a .: fs)

-- | Determines if all elements of a boolean stream are True.
and :: (Streaming t, Monad m) => t m Bool -> m Bool
and = go . toStream
    where
    go m1 =
        let stop                = return True
            yield a Nothing     = return a
            yield False _       = return False
            yield True (Just x) = go x
        in runStream m1 Nothing stop yield

-- | Determines wheter at least one element of a boolean stream is True.
or :: (Streaming t, Monad m) => t m Bool -> m Bool
or = go . toStream
    where
    go m1 =
        let stop                 = return False
            yield a Nothing      = return a
            yield True _         = return True
            yield False (Just x) = go x
        in runStream m1 Nothing stop yield

-- | Takes two streams and returns true if and only if the first is a prefix of
-- the second.
isPrefixOf :: (Streaming t, Monad m, Eq a) => t m a -> t m a -> m Bool
isPrefixOf p m = do
    mbUnconsP <- uncons p
    case mbUnconsP of
        Nothing -> return True
        Just (headP, p') -> do
            mbUnconsM <- uncons m
            case mbUnconsM of
                Just (headM, m') | headM == headP -> p' `isPrefixOf` m'
                _ -> return False

-- | Takes two streams and returns true if and only if the second  the second
-- stream ends in the first.
-- Both streams must be finite.
isSuffixOf :: (Streaming t, Monad m, Eq a) => t m a -> t m a -> m Bool
isSuffixOf p m = do
    lengthP <- length p
    lengthM <- length m
    if lengthM >= lengthP
        then equiv p (drop (lengthM - lengthP) m)
        else return False
    where
    equiv m1 m2 = do
        mbUncons1 <- uncons m1
        mbUncons2 <- uncons m2
        case (mbUncons1, mbUncons2) of
            (Nothing, Nothing) -> return True
            (Just (h1, m1'), Just (h2, m2')) | h1 == h2 -> equiv m1' m2'
            _ -> return False

-- | Takes two streams and determines if the first stream is contained
-- continously in the second.
isInfixOf :: (Streaming t, Monad m, Eq a) => t m a -> t m a -> m Bool
isInfixOf p m = do
    mbUnconsP <- uncons p
    case mbUnconsP of
        Nothing -> return True
        Just (headP, p') -> do
            mbUnconsM <- uncons m
            case mbUnconsM of
                Just (headM, m') -> do
                    next <- p `isInfixOf` m'
                    current <- if headM == headP then p' `isInfixOf` m'
                                                 else return False
                    return $ current || next
                Nothing -> return False

-- | Takes two streams and determines if the elements of the first stream
-- occur in the second in the right order, but opposed to @isInfixOf@ they
-- don't have to be continous.
isSubsequenceOf :: (Streaming t, Monad m, Eq a) => t m a -> t m a -> m Bool
isSubsequenceOf p m = do
    mbUnconsP <- uncons p
    case mbUnconsP of
        Nothing -> return True
        Just (headP, p') -> do
            mbUnconsM <- uncons m
            case mbUnconsM of
                Nothing -> return False
                Just (headM, m') ->
                    if headM == headP then p' `isSubsequenceOf` m'
                                      else p `isSubsequenceOf` m'

-- | Finds all the indices of elements satisfying the given predicate.
findIndices :: Streaming t => (a -> Bool) -> t m a -> t m Int
findIndices p = fromStream . go 0 . toStream
    where
    go offset m1 = Stream $ \svr stp yld ->
        let yield a Nothing =
                if p a then yld offset Nothing
                       else stp
            yield a (Just x) =
                if p a then yld offset $ Just $ go (offset + 1) x
                       else runStream (go (offset + 1) x) svr stp yld
        in runStream m1 Nothing stp yield

-- | Gives the index of the first stream element satisfying the given
-- preficate.
findIndex :: (Streaming t, Monad m) => (a -> Bool) -> t m a -> m (Maybe Int)
findIndex p = head . findIndices p

-- | Finds the index of all elements in the stream which are equal to the
-- given.
elemIndices :: (Streaming t, Eq a) => a -> t m a -> t m Int
elemIndices a = findIndices (==a)

-- | Gives the first index of an element in the stream, which equals the given.
elemIndex :: (Streaming t, Monad m, Eq a) => a -> t m a -> m (Maybe Int)
elemIndex a = findIndex (==a)

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

-- | Generate a stream by performing an action @n@ times.
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
