{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Stream.Eliminate
-- Copyright   : (c) 2018 Composewell Technologies
--               (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

-- A few functions in this module have been adapted from the vector package
-- (c) Roman Leshchinskiy.
--
module Streamly.Internal.Data.Stream.Eliminate
    (
    -- * Running a Parser
      parse
    , parsePos
    , parseBreak
    , parseBreakPos

    -- * Deconstruction
    , uncons

    -- * Right Folds
    , foldr1

    -- * Specific Fold Functions
    , mapM_ -- Map and Fold
    , null
    , init
    , tail
    , last
    , elem
    , notElem
    , all
    , any
    , maximum
    , maximumBy
    , minimum
    , minimumBy
    , lookup
    , findM
    , find
    , (!!)
    , the
    , foldMaybes
    , foldEithers

    -- * To containers
    , toListRev

    -- * Multi-Stream Folds
    -- | These should probably be expressed using parsers.
    , isPrefixOf
    , isInfixOf
    , isSuffixOf
    , isSuffixOfUnbox
    , isSubsequenceOf
    , stripPrefix
    , stripSuffix
    , stripSuffixUnbox

    -- * Deprecated
    , parseD
    , parseBreakD
    )
where

#include "inline.hs"
#include "deprecation.h"

import Control.Monad.IO.Class (MonadIO(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Parser (ParseError(..), ParseErrorPos(..))
import Streamly.Internal.Data.SVar.Type (adaptState, defState)
import Streamly.Internal.Data.Unbox (Unbox)

import Streamly.Internal.Data.Maybe.Strict (Maybe'(..))

import qualified Streamly.Internal.Data.Array.Type as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.ParserDrivers as Drivers
import qualified Streamly.Internal.Data.Stream.Parse as Nesting
import qualified Streamly.Internal.Data.Stream.Transform as StreamD

import Prelude hiding
       ( Foldable(..), all, any, head, last, lookup, mapM, mapM_
       , notElem, splitAt, init, tail, (!!))
import Streamly.Internal.Data.Stream.Type hiding (splitAt)

#include "DocTestDataStream.hs"

------------------------------------------------------------------------------
-- Elimination by Folds
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Right Folds
------------------------------------------------------------------------------

{-# INLINE_NORMAL foldr1 #-}
foldr1 :: Monad m => (a -> a -> a) -> Stream m a -> m (Maybe a)
foldr1 f m = do
     r <- uncons m
     case r of
         Nothing   -> return Nothing
         Just (h, t) -> fmap Just (foldr f h t)

-- | Fold a stream of optional values.
--
--   * If any element in the stream is 'Nothing', the fold terminates and the
--     result is 'Nothing'.
--   * If all elements are 'Just x', the supplied fold is applied to the
--     unwrapped @x@ values and the result is wrapped in 'Just'.
--
-- >>> foldMaybes f = Stream.fold (Fold.foldMaybes f)
--
-- === __Examples__
--
-- >>> Stream.foldMaybes Fold.sum (Stream.fromList [Just 1, Just 2, Just 3])
-- Just 6
--
-- >>> Stream.foldMaybes Fold.sum (Stream.fromList [Just 1, Nothing, Just 3])
-- Nothing
--
{-# INLINE foldMaybes #-}
foldMaybes :: Monad m =>
    Fold m a b -> Stream m (Maybe a) -> m (Maybe b)
foldMaybes f = fold (Fold.foldMaybes f)

-- | Fold a stream of 'Either' values.
--
--   * If any element in the stream is @Left e@, the fold terminates immediately
--     and the result is @Left e@.
--   * If all elements are @Right x@, the supplied fold is applied to the
--     unwrapped @x@ values and the result is wrapped in 'Right'.
--
-- >>> foldEithers f = Stream.fold (Fold.foldEithers f)
--
-- === __Examples__
--
-- >>> Stream.foldEithers Fold.sum (Stream.fromList [Right 1, Right 2, Right 3])
-- Right 6
--
-- >>> Stream.foldEithers Fold.sum (Stream.fromList [Right 1, Left "oops", Right 3])
-- Left "oops"
--
{-# INLINE foldEithers #-}
foldEithers :: Monad m =>
    Fold m b c -> Stream m (Either a b) -> m (Either a c)
foldEithers f = fold (Fold.foldEithers f)

------------------------------------------------------------------------------
-- Parsers
------------------------------------------------------------------------------

-- XXX It may be a good idea to use constant sized chunks for backtracking. We
-- can take a byte stream but when we have to backtrack we create constant
-- sized chunks. We maintain one forward list and one backward list of constant
-- sized chunks, and a last backtracking offset. That way we just need lists of
-- contents and no need to maintain start/end pointers for individual arrays,
-- reducing bookkeeping work.

-- | Parse a stream using the supplied 'Parser'.
--
{-# INLINE parseBreak #-}
parseBreak, parseBreakD :: Monad m =>
    PR.Parser a m b -> Stream m a -> m (Either ParseError b, Stream m a)
parseBreak = Drivers.parseBreak

RENAME(parseBreakD,parseBreak)

-- | Like 'parseBreak' but includes stream position information in the error
-- messages.
--
{-# INLINE parseBreakPos #-}
parseBreakPos :: Monad m =>
    PR.Parser a m b -> Stream m a -> m (Either ParseErrorPos b, Stream m a)
parseBreakPos = Drivers.parseBreakPos

-- | Parse a stream using the supplied 'Parser'.
--
-- Parsers (See "Streamly.Internal.Data.Parser") are more powerful folds that
-- add backtracking and error functionality to terminating folds. Unlike folds,
-- parsers may not always result in a valid output, they may result in an
-- error.  For example:
--
-- >>> Stream.parse (Parser.takeEQ 1 Fold.drain) Stream.nil
-- Left (ParseError "takeEQ: Expecting exactly 1 elements, input terminated on 0")
--
-- Note: @parse p@ is not the same as  @head . parseMany p@ on an empty stream.
--
{-# INLINE [3] parse #-}
parse, parseD :: Monad m => PR.Parser a m b -> Stream m a -> m (Either ParseError b)
parse parser strm = do
    (b, _) <- parseBreak parser strm
    return b

RENAME(parseD,parse)

-- | Like 'parse' but includes stream position information in the error
-- messages.
--
-- >>> Stream.parsePos (Parser.takeEQ 2 Fold.drain) (Stream.fromList [1])
-- Left (ParseErrorPos 1 "takeEQ: Expecting exactly 2 elements, input terminated on 1")
--
{-# INLINE [3] parsePos #-}
parsePos :: Monad m => PR.Parser a m b -> Stream m a -> m (Either ParseErrorPos b)
parsePos parser strm = do
    (b, _) <- parseBreakPos parser strm
    return b

------------------------------------------------------------------------------
-- Specialized Folds
------------------------------------------------------------------------------

-- benchmark after dropping 1 item from stream or using unfolds
{-# INLINE_NORMAL null #-}
null :: Monad m => Stream m a -> m Bool
#ifdef USE_FOLDS_EVERYWHERE
null = fold Fold.null
#else
null = foldrM (\_ _ -> return False) (return True)
#endif

{-# INLINE_NORMAL init #-}
init :: Monad m => Stream m a -> m (Maybe (Stream m a))
init stream = do
    r <- uncons stream
    case r of
        Nothing -> return Nothing
        Just (h, Stream step1 state1) ->
            return $ Just $ Stream step (h, state1)

            where

            step gst (a, s1) = do
                res <- step1 (adaptState gst) s1
                return $
                    case res of
                        Yield x s -> Yield a (x, s)
                        Skip s -> Skip (a, s)
                        Stop -> Stop

-- | Same as:
--
-- >>> tail = fmap (fmap snd) . Stream.uncons
--
-- Does not fuse, has the same performance as the StreamK version.
--
{-# INLINE_NORMAL tail #-}
tail :: Monad m => Stream m a -> m (Maybe (Stream m a))
tail = fmap (fmap snd) . uncons
{-
tail (UnStream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield _ s -> return (Just $ Stream step s)
            Skip  s   -> go SPEC s
            Stop      -> return Nothing
-}

-- XXX will it fuse? need custom impl?
{-# INLINE_NORMAL last #-}
last :: Monad m => Stream m a -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
last = fold Fold.last
#else
last = foldl' (\_ y -> Just y) Nothing
#endif

-- XXX Use the foldrM based impl instead
{-# INLINE_NORMAL elem #-}
elem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
#ifdef USE_FOLDS_EVERYWHERE
elem e = fold (Fold.elem e)
#else
-- elem e m = foldrM (\x xs -> if x == e then return True else xs) (return False) m
elem e (Stream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield x s
              | x == e -> return True
              | otherwise -> go SPEC s
            Skip s -> go SPEC s
            Stop   -> return False
#endif

{-# INLINE_NORMAL notElem #-}
notElem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
notElem e s = fmap not (e `elem` s)

{-# INLINE_NORMAL all #-}
all :: Monad m => (a -> Bool) -> Stream m a -> m Bool
#ifdef USE_FOLDS_EVERYWHERE
all p = fold (Fold.all p)
#else
-- all p m = foldrM (\x xs -> if p x then xs else return False) (return True) m
all p (Stream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield x s
              | p x -> go SPEC s
              | otherwise -> return False
            Skip s -> go SPEC s
            Stop   -> return True
#endif

{-# INLINE_NORMAL any #-}
any :: Monad m => (a -> Bool) -> Stream m a -> m Bool
#ifdef USE_FOLDS_EVERYWHERE
any p = fold (Fold.any p)
#else
-- any p m = foldrM (\x xs -> if p x then return True else xs) (return False) m
any p (Stream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield x s
              | p x -> return True
              | otherwise -> go SPEC s
            Skip s -> go SPEC s
            Stop   -> return False
#endif

{-# INLINE_NORMAL maximum #-}
maximum :: (Monad m, Ord a) => Stream m a -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
maximum = fold Fold.maximum
#else
maximum (Stream step state) = go SPEC Nothing' state
  where
    go !_ Nothing' st = do
        r <- step defState st
        case r of
            Yield x s -> go SPEC (Just' x) s
            Skip  s   -> go SPEC Nothing' s
            Stop      -> return Nothing
    go !_ (Just' acc) st = do
        r <- step defState st
        case r of
            Yield x s
              | acc <= x  -> go SPEC (Just' x) s
              | otherwise -> go SPEC (Just' acc) s
            Skip s -> go SPEC (Just' acc) s
            Stop   -> return (Just acc)
#endif

{-# INLINE_NORMAL maximumBy #-}
maximumBy :: Monad m => (a -> a -> Ordering) -> Stream m a -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
maximumBy cmp = fold (Fold.maximumBy cmp)
#else
maximumBy cmp (Stream step state) = go SPEC Nothing' state
  where
    go !_ Nothing' st = do
        r <- step defState st
        case r of
            Yield x s -> go SPEC (Just' x) s
            Skip  s   -> go SPEC Nothing' s
            Stop      -> return Nothing
    go !_ (Just' acc) st = do
        r <- step defState st
        case r of
            Yield x s -> case cmp acc x of
                GT -> go SPEC (Just' acc) s
                _  -> go SPEC (Just' x) s
            Skip s -> go SPEC (Just' acc) s
            Stop   -> return (Just acc)
#endif

{-# INLINE_NORMAL minimum #-}
minimum :: (Monad m, Ord a) => Stream m a -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
minimum = fold Fold.minimum
#else
minimum (Stream step state) = go SPEC Nothing' state

    where

    go !_ Nothing' st = do
        r <- step defState st
        case r of
            Yield x s -> go SPEC (Just' x) s
            Skip  s   -> go SPEC Nothing' s
            Stop      -> return Nothing
    go !_ (Just' acc) st = do
        r <- step defState st
        case r of
            Yield x s
              | acc <= x  -> go SPEC (Just' acc) s
              | otherwise -> go SPEC (Just' x) s
            Skip s -> go SPEC (Just' acc) s
            Stop   -> return (Just acc)
#endif

{-# INLINE_NORMAL minimumBy #-}
minimumBy :: Monad m => (a -> a -> Ordering) -> Stream m a -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
minimumBy cmp = fold (Fold.minimumBy cmp)
#else
minimumBy cmp (Stream step state) = go SPEC Nothing' state

    where

    go !_ Nothing' st = do
        r <- step defState st
        case r of
            Yield x s -> go SPEC (Just' x) s
            Skip  s   -> go SPEC Nothing' s
            Stop      -> return Nothing
    go !_ (Just' acc) st = do
        r <- step defState st
        case r of
            Yield x s -> case cmp acc x of
                GT -> go SPEC (Just' x) s
                _  -> go SPEC (Just' acc) s
            Skip s -> go SPEC (Just' acc) s
            Stop   -> return (Just acc)
#endif

{-# INLINE_NORMAL (!!) #-}
(!!) :: (Monad m) => Stream m a -> Int -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
stream !! i = fold (Fold.index i) stream
#else
(Stream step state) !! i = go SPEC i state

    where

    go !_ !n st = do
        r <- step defState st
        case r of
            Yield x s | n < 0 -> return Nothing
                      | n == 0 -> return $ Just x
                      | otherwise -> go SPEC (n - 1) s
            Skip s -> go SPEC n s
            Stop   -> return Nothing
#endif

{-# INLINE_NORMAL lookup #-}
lookup :: (Monad m, Eq a) => a -> Stream m (a, b) -> m (Maybe b)
#ifdef USE_FOLDS_EVERYWHERE
lookup e = fold (Fold.lookup e)
#else
lookup e = foldrM (\(a, b) xs -> if e == a then return (Just b) else xs)
                   (return Nothing)
#endif

{-# INLINE_NORMAL findM #-}
findM :: Monad m => (a -> m Bool) -> Stream m a -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
findM p = fold (Fold.findM p)
#else
findM p = foldrM (\x xs -> p x >>= \r -> if r then return (Just x) else xs)
                   (return Nothing)
#endif

{-# INLINE find #-}
find :: Monad m => (a -> Bool) -> Stream m a -> m (Maybe a)
find p = findM (return . p)

{-# INLINE toListRev #-}
toListRev :: Monad m => Stream m a -> m [a]
#ifdef USE_FOLDS_EVERYWHERE
toListRev = fold Fold.toListRev
#else
toListRev = foldl' (flip (:)) []
#endif

------------------------------------------------------------------------------
-- Transformation comprehensions
------------------------------------------------------------------------------

{-# INLINE_NORMAL the #-}
the :: (Eq a, Monad m) => Stream m a -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
the = fold Fold.the
#else
the (Stream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield x s -> go' SPEC x s
            Skip s    -> go SPEC s
            Stop      -> return Nothing
    go' !_ n st = do
        r <- step defState st
        case r of
            Yield x s | x == n -> go' SPEC n s
                      | otherwise -> return Nothing
            Skip s -> go' SPEC n s
            Stop   -> return (Just n)
#endif

------------------------------------------------------------------------------
-- Map and Fold
------------------------------------------------------------------------------

-- | Execute a monadic action for each element of the 'Stream'
{-# INLINE_NORMAL mapM_ #-}
mapM_ :: Monad m => (a -> m b) -> Stream m a -> m ()
#ifdef USE_FOLDS_EVERYWHERE
mapM_ f = fold (Fold.drainBy f)
#else
mapM_ m = drain . mapM m
#endif

------------------------------------------------------------------------------
-- Multi-stream folds
------------------------------------------------------------------------------

-- | Returns 'True' if the first stream is the same as or a prefix of the
-- second. A stream is a prefix of itself.
--
-- >>> Stream.isPrefixOf (Stream.fromList "hello") (Stream.fromList "hello" :: Stream IO Char)
-- True
--
{-# INLINE_NORMAL isPrefixOf #-}
isPrefixOf :: (Monad m, Eq a) => Stream m a -> Stream m a -> m Bool
isPrefixOf (Stream stepa ta) (Stream stepb tb) = go SPEC Nothing' ta tb

    where

    go !_ Nothing' sa sb = do
        r <- stepa defState sa
        case r of
            Yield x sa' -> go SPEC (Just' x) sa' sb
            Skip sa'    -> go SPEC Nothing' sa' sb
            Stop        -> return True

    go !_ (Just' x) sa sb = do
        r <- stepb defState sb
        case r of
            Yield y sb' ->
                if x == y
                    then go SPEC Nothing' sa sb'
                    else return False
            Skip sb' -> go SPEC (Just' x) sa sb'
            Stop     -> return False

-- | Returns 'True' if all the elements of the first stream occur, in order, in
-- the second stream. The elements do not have to occur consecutively. A stream
-- is a subsequence of itself.
--
-- >>> Stream.isSubsequenceOf (Stream.fromList "hlo") (Stream.fromList "hello" :: Stream IO Char)
-- True
--
{-# INLINE_NORMAL isSubsequenceOf #-}
isSubsequenceOf :: (Monad m, Eq a) => Stream m a -> Stream m a -> m Bool
isSubsequenceOf (Stream stepa ta) (Stream stepb tb) = go SPEC Nothing' ta tb

    where

    go !_ Nothing' sa sb = do
        r <- stepa defState sa
        case r of
            Yield x sa' -> go SPEC (Just' x) sa' sb
            Skip sa' -> go SPEC Nothing' sa' sb
            Stop -> return True

    go !_ (Just' x) sa sb = do
        r <- stepb defState sb
        case r of
            Yield y sb' ->
                if x == y
                    then go SPEC Nothing' sa sb'
                    else go SPEC (Just' x) sa sb'
            Skip sb' -> go SPEC (Just' x) sa sb'
            Stop -> return False

-- | @stripPrefix prefix input@ strips the @prefix@ stream from the @input@
-- stream if it is a prefix of input. Returns 'Nothing' if the input does not
-- start with the given prefix, stripped input otherwise. Returns @Just nil@
-- when the prefix is the same as the input stream.
--
-- Space: @O(1)@
--
{-# INLINE_NORMAL stripPrefix #-}
stripPrefix
    :: (Monad m, Eq a)
    => Stream m a -> Stream m a -> m (Maybe (Stream m a))
stripPrefix (Stream stepa ta) (Stream stepb tb) = go SPEC Nothing' ta tb

    where

    go !_ Nothing' sa sb = do
        r <- stepa defState sa
        case r of
            Yield x sa' -> go SPEC (Just' x) sa' sb
            Skip sa'    -> go SPEC Nothing' sa' sb
            Stop        -> return $ Just (Stream stepb sb)

    go !_ (Just' x) sa sb = do
        r <- stepb defState sb
        case r of
            Yield y sb' ->
                if x == y
                    then go SPEC Nothing' sa sb'
                    else return Nothing
            Skip sb' -> go SPEC (Just' x) sa sb'
            Stop     -> return Nothing

-- | Returns 'True' if the first stream is an infix of the second. A stream is
-- considered an infix of itself.
--
-- >>> s = Stream.fromList "hello" :: Stream IO Char
-- >>> Stream.isInfixOf s s
-- True
--
-- Space: @O(n)@ worst case where @n@ is the length of the infix.
--
-- /Pre-release/
--
-- /Requires 'Storable' constraint/
--
{-# INLINE isInfixOf #-}
isInfixOf :: (MonadIO m, Eq a, Enum a, Unbox a)
    => Stream m a -> Stream m a -> m Bool
isInfixOf infx stream = do
    arr <- fold Array.create infx
    -- XXX can use breakOnSeq instead (when available)
    r <- null $ StreamD.drop 1 $ Nesting.splitSepBySeq_ arr Fold.drain stream
    return (not r)

-- Note: isPrefixOf uses the prefix stream only once. In contrast, isSuffixOf
-- may use the suffix stream many times. To run in optimal memory we do not
-- want to buffer the suffix stream in memory therefore  we need an ability to
-- clone (or consume it multiple times) the suffix stream without any side
-- effects so that multiple potential suffix matches can proceed in parallel
-- without buffering the suffix stream. For example, we may create the suffix
-- stream from a file handle, however, if we evaluate the stream multiple
-- times, once for each match, we will need a different file handle each time
-- which may exhaust the file descriptors. Instead, we want to share the same
-- underlying file descriptor, use pread on it to generate the stream and clone
-- the stream for each match. Therefore the suffix stream should be built in
-- such a way that it can be consumed multiple times without any problems.

-- XXX Can be implemented with better space/time complexity.
-- Space: @O(n)@ worst case where @n@ is the length of the suffix.

-- | Returns 'True' if the first stream is a suffix of the second. A stream is
-- considered a suffix of itself.
--
-- >>> Stream.isSuffixOf (Stream.fromList "hello") (Stream.fromList "hello" :: Stream IO Char)
-- True
--
-- Space: @O(n)@, buffers entire input stream and the suffix.
--
-- /Pre-release/
--
-- /Suboptimal/ - Help wanted.
--
{-# INLINE isSuffixOf #-}
isSuffixOf :: (Monad m, Eq a) => Stream m a -> Stream m a -> m Bool
isSuffixOf suffix stream =
    StreamD.reverse suffix `isPrefixOf` StreamD.reverse stream

-- | Much faster than 'isSuffixOf'.
{-# INLINE isSuffixOfUnbox #-}
isSuffixOfUnbox :: (MonadIO m, Eq a, Unbox a) =>
    Stream m a -> Stream m a -> m Bool
isSuffixOfUnbox suffix stream =
    StreamD.reverseUnbox suffix `isPrefixOf` StreamD.reverseUnbox stream

-- | Drops the given suffix from a stream. Returns 'Nothing' if the stream does
-- not end with the given suffix. Returns @Just nil@ when the suffix is the
-- same as the stream.
--
-- It may be more efficient to convert the stream to an Array and use
-- stripSuffix on that especially if the elements have a Storable or Prim
-- instance.
--
-- Space: @O(n)@, buffers the entire input stream as well as the suffix
--
-- /Pre-release/
{-# INLINE stripSuffix #-}
stripSuffix
    :: (Monad m, Eq a)
    => Stream m a -> Stream m a -> m (Maybe (Stream m a))
stripSuffix m1 m2 =
    fmap StreamD.reverse
        <$> stripPrefix (StreamD.reverse m1) (StreamD.reverse m2)

-- | Much faster than 'stripSuffix'.
{-# INLINE stripSuffixUnbox #-}
stripSuffixUnbox
    :: (MonadIO m, Eq a, Unbox a)
    => Stream m a -> Stream m a -> m (Maybe (Stream m a))
stripSuffixUnbox m1 m2 =
    fmap StreamD.reverseUnbox
        <$> stripPrefix (StreamD.reverseUnbox m1) (StreamD.reverseUnbox m2)
