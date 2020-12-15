-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Eliminate
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.IsStream.Eliminate
    (
    -- * Elimination
    -- ** Deconstruction
      uncons
    , tail
    , init

    -- ** Folding
    -- ** Right Folds
    , foldrM
    , foldrS
    , foldrT
    , foldr

    -- ** Left Folds
    , foldl'
    , foldl1'
    , foldlM'

    -- ** Composable Left Folds
    -- | See "Streamly.Internal.Data.Fold"
    , fold

    -- ** Parsers
    -- | See "Streamly.Internal.Data.Parser"
    , parse
    , parseK
    , parseD

    -- ** Concurrent Folds
    , foldAsync
    , (|$.)
    , (|&.)

    -- ** Full Folds

    -- -- ** To Summary (Full Folds)
    , drain
    , last
    , length
    , sum
    , product
    , mconcat

    -- -- ** To Summary (Maybe) (Full Folds)
    , maximumBy
    , maximum
    , minimumBy
    , minimum
    , the

    -- ** Lazy Folds
    -- -- ** To Containers (Full Folds)
    , toList
    , toListRev
    , toPure
    , toPureRev

    -- ** Composable Left Folds

    , toStream    -- XXX rename to write?
    , toStreamRev -- XXX rename to writeRev?

    -- ** Partial Folds

    -- -- ** To Elements (Partial Folds)

    -- -- | Folds that extract selected elements of a stream or their properties.
    , (!!)
    , head
    , headElse
    , findM
    , find
    , lookup

    -- -- ** To Boolean (Partial Folds)
    , null
    , elem
    , notElem
    , all
    , any
    , and
    , or

    -- ** Multi-Stream folds
    -- Full equivalence
    , eqBy
    , cmpBy

    -- finding subsequences
    , isPrefixOf
    , isSubsequenceOf

    -- trimming sequences
    , stripPrefix
    -- , stripInfix
    , dropPrefix
    , dropInfix
    , dropSuffix

    -- * Deprecated
    , foldx
    , foldxM
    , foldr1
    , runStream
    , toHandle
    )
where

#include "inline.hs"

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Functor.Identity (Identity (..))
import Streamly.Internal.Data.Fold.Types (Fold (..))
import Streamly.Internal.Data.Parser (Parser (..))
import Streamly.Internal.Data.SVar (MonadAsync, defState)
import Streamly.Internal.Data.Stream.Prelude (toStreamS)
import Streamly.Internal.Data.Stream.StreamD (fromStreamD, toStreamD)
import Streamly.Internal.Data.Stream.StreamK (IsStream)
import Streamly.Internal.Data.Stream.Serial (SerialT)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.Prelude as P
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Parser.ParserK.Types as PRK
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified System.IO as IO
#ifdef USE_STREAMK_ONLY
import qualified Streamly.Internal.Data.Stream.StreamK as S
#else
import qualified Streamly.Internal.Data.Stream.StreamD as S
#endif

import Prelude hiding
       ( filter, drop, dropWhile, take, takeWhile, zipWith, foldr
       , foldl, map, mapM, mapM_, sequence, all, any, sum, product, elem
       , notElem, maximum, minimum, head, last, tail, length, null
       , reverse, iterate, init, and, or, lookup, foldr1, (!!)
       , scanl, scanl1, replicate, concatMap, span, splitAt, break
       , repeat, concat, mconcat)

------------------------------------------------------------------------------
-- Deconstruction
------------------------------------------------------------------------------

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns @Just (a, ma)@, where @a@ is
-- the head of the stream and @ma@ its tail.
--
-- This is a brute force primitive. Avoid using it as long as possible, use it
-- when no other combinator can do the job. This can be used to do pretty much
-- anything in an imperative manner, as it just breaks down the stream into
-- individual elements and we can loop over them as we deem fit. For example,
-- this can be used to convert a streamly stream into other stream types.
--
-- @since 0.1.0
{-# INLINE uncons #-}
uncons :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (a, t m a))
uncons m = K.uncons (K.adapt m)

------------------------------------------------------------------------------
-- Elimination by Folding
------------------------------------------------------------------------------

-- | Right associative/lazy pull fold. @foldrM build final stream@ constructs
-- an output structure using the step function @build@. @build@ is invoked with
-- the next input element and the remaining (lazy) tail of the output
-- structure. It builds a lazy output expression using the two. When the "tail
-- structure" in the output expression is evaluated it calls @build@ again thus
-- lazily consuming the input @stream@ until either the output expression built
-- by @build@ is free of the "tail" or the input is exhausted in which case
-- @final@ is used as the terminating case for the output structure. For more
-- details see the description in the previous section.
--
-- Example, determine if any element is 'odd' in a stream:
--
-- >>> S.foldrM (\x xs -> if odd x then return True else xs) (return False) $ S.fromList (2:4:5:undefined)
-- > True
--
-- /Since: 0.7.0 (signature changed)/
--
-- /Since: 0.2.0 (signature changed)/
--
-- /Since: 0.1.0/
{-# INLINE foldrM #-}
foldrM :: Monad m => (a -> m b -> m b) -> m b -> SerialT m a -> m b
foldrM = P.foldrM

-- | Right fold to a streaming monad.
--
-- > foldrS S.cons S.nil === id
--
-- 'foldrS' can be used to perform stateless stream to stream transformations
-- like map and filter in general. It can be coupled with a scan to perform
-- stateful transformations. However, note that the custom map and filter
-- routines can be much more efficient than this due to better stream fusion.
--
-- >>> S.toList $ S.foldrS S.cons S.nil $ S.fromList [1..5]
-- > [1,2,3,4,5]
--
-- Find if any element in the stream is 'True':
--
-- >>> S.toList $ S.foldrS (\x xs -> if odd x then return True else xs) (return False) $ (S.fromList (2:4:5:undefined) :: SerialT IO Int)
-- > [True]
--
-- Map (+2) on odd elements and filter out the even elements:
--
-- >>> S.toList $ S.foldrS (\x xs -> if odd x then (x + 2) `S.cons` xs else xs) S.nil $ (S.fromList [1..5] :: SerialT IO Int)
-- > [3,5,7]
--
-- 'foldrM' can also be represented in terms of 'foldrS', however, the former
-- is much more efficient:
--
-- > foldrM f z s = runIdentityT $ foldrS (\x xs -> lift $ f x (runIdentityT xs)) (lift z) s
--
-- /Internal/
{-# INLINE foldrS #-}
foldrS :: IsStream t => (a -> t m b -> t m b) -> t m b -> t m a -> t m b
foldrS = K.foldrS

-- | Right fold to a transformer monad.  This is the most general right fold
-- function. 'foldrS' is a special case of 'foldrT', however 'foldrS'
-- implementation can be more efficient:
--
-- > foldrS = foldrT
-- > foldrM f z s = runIdentityT $ foldrT (\x xs -> lift $ f x (runIdentityT xs)) (lift z) s
--
-- 'foldrT' can be used to translate streamly streams to other transformer
-- monads e.g.  to a different streaming type.
--
-- /Internal/
{-# INLINE foldrT #-}
foldrT :: (IsStream t, Monad m, Monad (s m), MonadTrans s)
    => (a -> s m b -> s m b) -> s m b -> t m a -> s m b
foldrT f z s = S.foldrT f z (toStreamS s)

-- | Right fold, lazy for lazy monads and pure streams, and strict for strict
-- monads.
--
-- Please avoid using this routine in strict monads like IO unless you need a
-- strict right fold. This is provided only for use in lazy monads (e.g.
-- Identity) or pure streams. Note that with this signature it is not possible
-- to implement a lazy foldr when the monad @m@ is strict. In that case it
-- would be strict in its accumulator and therefore would necessarily consume
-- all its input.
--
-- @since 0.1.0
{-# INLINE foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> SerialT m a -> m b
foldr = P.foldr

-- XXX This seems to be of limited use as it cannot be used to construct
-- recursive structures and for reduction foldl1' is better.
--
-- | Lazy right fold for non-empty streams, using first element as the starting
-- value. Returns 'Nothing' if the stream is empty.
--
-- @since 0.5.0
{-# INLINE foldr1 #-}
{-# DEPRECATED foldr1 "Use foldrM instead." #-}
foldr1 :: Monad m => (a -> a -> a) -> SerialT m a -> m (Maybe a)
foldr1 f m = S.foldr1 f (toStreamS m)

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
--
-- @since 0.2.0
{-# DEPRECATED foldx "Please use foldl' followed by fmap instead." #-}
{-# INLINE foldx #-}
foldx :: Monad m => (x -> a -> x) -> x -> (x -> b) -> SerialT m a -> m b
foldx = P.foldlx'

-- | Left associative/strict push fold. @foldl' reduce initial stream@ invokes
-- @reduce@ with the accumulator and the next input in the input stream, using
-- @initial@ as the initial value of the current value of the accumulator. When
-- the input is exhausted the current value of the accumulator is returned.
-- Make sure to use a strict data structure for accumulator to not build
-- unnecessary lazy expressions unless that's what you want. See the previous
-- section for more details.
--
-- @since 0.2.0
{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> SerialT m a -> m b
foldl' = P.foldl'

-- | Strict left fold, for non-empty streams, using first element as the
-- starting value. Returns 'Nothing' if the stream is empty.
--
-- @since 0.5.0
{-# INLINE foldl1' #-}
foldl1' :: Monad m => (a -> a -> a) -> SerialT m a -> m (Maybe a)
foldl1' step m = do
    r <- uncons m
    case r of
        Nothing -> return Nothing
        Just (h, t) -> do
            res <- foldl' step h t
            return $ Just res

-- | Like 'foldx', but with a monadic step function.
--
-- @since 0.2.0
{-# DEPRECATED foldxM "Please use foldlM' followed by fmap instead." #-}
{-# INLINE foldxM #-}
foldxM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> SerialT m a -> m b
foldxM = P.foldlMx'

-- | Like 'foldl'' but with a monadic step function.
--
-- /Since: 0.2.0/
--
-- /Since: 0.8.0 (signature change)/
{-# INLINE foldlM' #-}
foldlM' :: Monad m => (b -> a -> m b) -> m b -> SerialT m a -> m b
foldlM' step begin m = S.foldlM' step begin $ toStreamS m

------------------------------------------------------------------------------
-- Running a Fold
------------------------------------------------------------------------------

-- | Fold a stream using the supplied left 'Fold' and reducing the resulting
-- expression strictly at each step. The behavior is similar to 'foldl''. A
-- 'Fold' can terminate early without consuming the full stream. See the
-- documentation of individual 'Fold's for termination behavior.
--
-- >>> S.fold FL.sum (S.enumerateFromTo 1 100)
-- 5050
--
-- @since 0.7.0
{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> SerialT m a -> m b
fold = P.foldOnce

------------------------------------------------------------------------------
-- Running a sink
------------------------------------------------------------------------------

{-
-- | Drain a stream to a 'Sink'.
{-# INLINE runSink #-}
runSink :: Monad m => Sink m a -> SerialT m a -> m ()
runSink = fold . toFold
-}

------------------------------------------------------------------------------
-- Running a Parser
------------------------------------------------------------------------------

-- | Parse a stream using the supplied 'Parser'.
--
-- /Internal/
--
{-# INLINE_NORMAL parseD #-}
parseD :: MonadThrow m => PRD.Parser m a b -> SerialT m a -> m b
parseD (PRD.Parser step initial extract) = P.parselMx' step initial extract

{-# INLINE parseK #-}
parseK :: MonadThrow m => PRK.Parser m a b -> SerialT m a -> m b
parseK = parse

-- | Parse a stream using the supplied 'Parser'.
--
-- /Internal/
--
{-# INLINE [3] parse #-}
parse :: MonadThrow m => Parser m a b -> SerialT m a -> m b
parse = parseD . PRK.fromParserK

------------------------------------------------------------------------------
-- Specialized folds
------------------------------------------------------------------------------

-- |
-- > drain = mapM_ (\_ -> return ())
-- > drain = fold Fold.drain
--
-- Run a stream, discarding the results. By default it interprets the stream
-- as 'SerialT', to run other types of streams use the type adapting
-- combinators for example @drain . 'asyncly'@.
--
-- @since 0.7.0
{-# INLINE drain #-}
drain :: Monad m => SerialT m a -> m ()
drain = P.drain

-- | Run a stream, discarding the results. By default it interprets the stream
-- as 'SerialT', to run other types of streams use the type adapting
-- combinators for example @runStream . 'asyncly'@.
--
-- @since 0.2.0
{-# DEPRECATED runStream "Please use \"drain\" instead" #-}
{-# INLINE runStream #-}
runStream :: Monad m => SerialT m a -> m ()
runStream = drain

-- | Determine whether the stream is empty.
--
-- > null = fold Fold.null
--
-- @since 0.1.1
{-# INLINE null #-}
null :: Monad m => SerialT m a -> m Bool
null = S.null . toStreamS

-- | Extract the first element of the stream, if any.
--
-- > head = (!! 0)
-- > head = fold Fold.head
--
-- @since 0.1.0
{-# INLINE head #-}
head :: Monad m => SerialT m a -> m (Maybe a)
head = S.head . toStreamS

-- | Extract the first element of the stream, if any, otherwise use the
-- supplied default value. It can help avoid one branch in high performance
-- code.
--
-- /Internal/
{-# INLINE headElse #-}
headElse :: Monad m => a -> SerialT m a -> m a
headElse x = D.headElse x . toStreamD

-- |
-- > tail = fmap (fmap snd) . uncons
--
-- Extract all but the first element of the stream, if any.
--
-- @since 0.1.1
{-# INLINE tail #-}
tail :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (t m a))
tail m = K.tail (K.adapt m)

-- | Extract all but the last element of the stream, if any.
--
-- @since 0.5.0
{-# INLINE init #-}
init :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (t m a))
init m = K.init (K.adapt m)

-- | Extract the last element of the stream, if any.
--
-- > last xs = xs !! (length xs - 1)
-- > last = fold Fold.last
--
-- @since 0.1.1
{-# INLINE last #-}
last :: Monad m => SerialT m a -> m (Maybe a)
last m = S.last $ toStreamS m

-- | Determine whether an element is present in the stream.
--
-- > elem = fold Fold.elem
--
-- @since 0.1.0
{-# INLINE elem #-}
elem :: (Monad m, Eq a) => a -> SerialT m a -> m Bool
elem e m = S.elem e (toStreamS m)

-- | Determine whether an element is not present in the stream.
--
-- > notElem = fold Fold.length
--
-- @since 0.1.0
{-# INLINE notElem #-}
notElem :: (Monad m, Eq a) => a -> SerialT m a -> m Bool
notElem e m = S.notElem e (toStreamS m)

-- | Determine the length of the stream.
--
-- @since 0.1.0
{-# INLINE length #-}
length :: Monad m => SerialT m a -> m Int
length = foldl' (\n _ -> n + 1) 0

-- | Determine whether all elements of a stream satisfy a predicate.
--
-- > all = fold Fold.all
--
-- @since 0.1.0
{-# INLINE all #-}
all :: Monad m => (a -> Bool) -> SerialT m a -> m Bool
all p m = S.all p (toStreamS m)

-- | Determine whether any of the elements of a stream satisfy a predicate.
--
-- > any = fold Fold.any
--
-- @since 0.1.0
{-# INLINE any #-}
any :: Monad m => (a -> Bool) -> SerialT m a -> m Bool
any p m = S.any p (toStreamS m)

-- | Determines if all elements of a boolean stream are True.
--
-- > and = fold Fold.and
--
-- @since 0.5.0
{-# INLINE and #-}
and :: Monad m => SerialT m Bool -> m Bool
and = all (==True)

-- | Determines whether at least one element of a boolean stream is True.
--
-- > or = fold Fold.or
--
-- @since 0.5.0
{-# INLINE or #-}
or :: Monad m => SerialT m Bool -> m Bool
or = any (==True)

-- | Determine the sum of all elements of a stream of numbers. Returns @0@ when
-- the stream is empty. Note that this is not numerically stable for floating
-- point numbers.
--
-- > sum = fold Fold.sum
--
-- @since 0.1.0
{-# INLINE sum #-}
sum :: (Monad m, Num a) => SerialT m a -> m a
sum = foldl' (+) 0

-- | Determine the product of all elements of a stream of numbers. Returns @1@
-- when the stream is empty.
--
-- > product = fold Fold.product
--
-- @since 0.1.1
{-# INLINE product #-}
product :: (Monad m, Num a) => SerialT m a -> m a
product = foldl' (*) 1

-- | Fold a stream of monoid elements by appending them.
--
-- > mconcat = fold Fold.mconcat
--
-- /Internal/
{-# INLINE mconcat #-}
mconcat :: (Monad m, Monoid a) => SerialT m a -> m a
mconcat = foldr mappend mempty

-- |
-- @
-- minimum = 'minimumBy' compare
 -- minimum = fold Fold.minimum
-- @
--
-- Determine the minimum element in a stream.
--
-- @since 0.1.0
{-# INLINE minimum #-}
minimum :: (Monad m, Ord a) => SerialT m a -> m (Maybe a)
minimum m = S.minimum (toStreamS m)

-- | Determine the minimum element in a stream using the supplied comparison
-- function.
--
-- > minimumBy = fold Fold.minimumBy
--
-- @since 0.6.0
{-# INLINE minimumBy #-}
minimumBy :: Monad m => (a -> a -> Ordering) -> SerialT m a -> m (Maybe a)
minimumBy cmp m = S.minimumBy cmp (toStreamS m)

-- |
-- @
-- maximum = 'maximumBy' compare
-- maximum = fold Fold.maximum
-- @
--
-- Determine the maximum element in a stream.
--
-- @since 0.1.0
{-# INLINE maximum #-}
maximum :: (Monad m, Ord a) => SerialT m a -> m (Maybe a)
maximum = P.maximum

-- | Determine the maximum element in a stream using the supplied comparison
-- function.
--
-- > maximumBy = fold Fold.maximumBy
--
-- @since 0.6.0
{-# INLINE maximumBy #-}
maximumBy :: Monad m => (a -> a -> Ordering) -> SerialT m a -> m (Maybe a)
maximumBy cmp m = S.maximumBy cmp (toStreamS m)

-- | Ensures that all the elements of the stream are identical and then returns
-- that unique element.
--
-- @since 0.6.0
{-# INLINE the #-}
the :: (Eq a, Monad m) => SerialT m a -> m (Maybe a)
the m = S.the (toStreamS m)

-- | Lookup the element at the given index.
--
-- @since 0.6.0
{-# INLINE (!!) #-}
(!!) :: Monad m => SerialT m a -> Int -> m (Maybe a)
m !! i = toStreamS m S.!! i

-- | In a stream of (key-value) pairs @(a, b)@, return the value @b@ of the
-- first pair where the key equals the given value @a@.
--
-- > lookup = snd <$> find ((==) . fst)
-- > lookup = fold Fold.lookup
--
-- @since 0.5.0
{-# INLINE lookup #-}
lookup :: (Monad m, Eq a) => a -> SerialT m (a, b) -> m (Maybe b)
lookup a m = S.lookup a (toStreamS m)

-- | Like 'findM' but with a non-monadic predicate.
--
-- > find p = findM (return . p)
-- > find = fold Fold.find
--
-- @since 0.5.0
{-# INLINE find #-}
find :: Monad m => (a -> Bool) -> SerialT m a -> m (Maybe a)
find p m = S.find p (toStreamS m)

-- | Returns the first element that satisfies the given predicate.
--
-- > findM = fold Fold.findM
--
-- @since 0.6.0
{-# INLINE findM #-}
findM :: Monad m => (a -> m Bool) -> SerialT m a -> m (Maybe a)
findM p m = S.findM p (toStreamS m)

------------------------------------------------------------------------------
-- Substreams
------------------------------------------------------------------------------

-- | Returns 'True' if the first stream is the same as or a prefix of the
-- second. A stream is a prefix of itself.
--
-- @
-- > S.isPrefixOf (S.fromList "hello") (S.fromList "hello" :: SerialT IO Char)
-- True
-- @
--
-- @since 0.6.0
{-# INLINE isPrefixOf #-}
isPrefixOf :: (Eq a, IsStream t, Monad m) => t m a -> t m a -> m Bool
isPrefixOf m1 m2 = D.isPrefixOf (toStreamD m1) (toStreamD m2)

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

-- | Returns 'True' if all the elements of the first stream occur, in order, in
-- the second stream. The elements do not have to occur consecutively. A stream
-- is a subsequence of itself.
--
-- @
-- > S.isSubsequenceOf (S.fromList "hlo") (S.fromList "hello" :: SerialT IO Char)
-- True
-- @
--
-- @since 0.6.0
{-# INLINE isSubsequenceOf #-}
isSubsequenceOf :: (Eq a, IsStream t, Monad m) => t m a -> t m a -> m Bool
isSubsequenceOf m1 m2 = D.isSubsequenceOf (toStreamD m1) (toStreamD m2)

-- | @stripPrefix prefix stream@ strips @prefix@ from @stream@ if it is a
-- prefix of stream. Returns 'Nothing' if the stream does not start with the
-- given prefix, stripped stream otherwise. Returns @Just nil@ when the prefix
-- is the same as the stream.
--
-- Space: @O(1)@
--
-- @since 0.6.0
{-# INLINE stripPrefix #-}
stripPrefix
    :: (Eq a, IsStream t, Monad m)
    => t m a -> t m a -> m (Maybe (t m a))
stripPrefix m1 m2 = fmap fromStreamD <$>
    D.stripPrefix (toStreamD m1) (toStreamD m2)

-- | Drop prefix from the input stream if present.
--
-- Space: @O(1)@
--
-- /Unimplemented/ - Help wanted.
{-# INLINE dropPrefix #-}
dropPrefix ::
    -- (Eq a, IsStream t, Monad m) =>
    t m a -> t m a -> t m a
dropPrefix = error "Not implemented yet!"

-- | Drop all matching infix from the input stream if present. Infix stream
-- may be consumed multiple times.
--
-- Space: @O(n)@ where n is the length of the infix.
--
-- /Unimplemented/ - Help wanted.
{-# INLINE dropInfix #-}
dropInfix ::
    -- (Eq a, IsStream t, Monad m) =>
    t m a -> t m a -> t m a
dropInfix = error "Not implemented yet!"

-- | Drop suffix from the input stream if present. Suffix stream may be
-- consumed multiple times.
--
-- Space: @O(n)@ where n is the length of the suffix.
--
-- /Unimplemented/ - Help wanted.
{-# INLINE dropSuffix #-}
dropSuffix ::
    -- (Eq a, IsStream t, Monad m) =>
    t m a -> t m a -> t m a
dropSuffix = error "Not implemented yet!"

------------------------------------------------------------------------------
-- Concurrent Application
------------------------------------------------------------------------------

infixr 0 |$.
infixl 1 |&.

-- | Parallel fold application operator; applies a fold function @t m a -> m b@
-- to a stream @t m a@ concurrently; The the input stream is evaluated
-- asynchronously in an independent thread yielding elements to a buffer and
-- the folding action runs in another thread consuming the input from the
-- buffer.
--
-- If you read the signature as @(t m a -> m b) -> (t m a -> m b)@ you can look
-- at it as a transformation that converts a fold function to a buffered
-- concurrent fold function.
--
-- The @.@ at the end of the operator is a mnemonic for termination of the
-- stream.
--
-- @
--    S.foldlM' (\\_ a -> threadDelay 1000000 >> print a) ()
--       |$. S.repeatM (threadDelay 1000000 >> return 1)
-- @
--
-- /Concurrent/
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE (|$.) #-}
(|$.) :: (IsStream t, MonadAsync m) => (t m a -> m b) -> (t m a -> m b)
-- (|$.) f = f . Async.mkAsync
(|$.) f = f . D.mkParallel

-- | Same as '|$.'.
--
--  /Internal/
--
{-# INLINE foldAsync #-}
foldAsync :: (IsStream t, MonadAsync m) => (t m a -> m b) -> (t m a -> m b)
foldAsync = (|$.)

-- | Parallel reverse function application operator for applying a run or fold
-- functions to a stream. Just like '|$.' except that the operands are reversed.
--
-- @
--        S.repeatM (threadDelay 1000000 >> return 1)
--    |&. S.foldlM' (\\_ a -> threadDelay 1000000 >> print a) ()
-- @
--
-- /Concurrent/
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE (|&.) #-}
(|&.) :: (IsStream t, MonadAsync m) => t m a -> (t m a -> m b) -> m b
x |&. f = f |$. x

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- |
-- @
-- toList = S.foldr (:) []
-- @
--
-- Convert a stream into a list in the underlying monad. The list can be
-- consumed lazily in a lazy monad (e.g. 'Identity'). In a strict monad (e.g.
-- IO) the whole list is generated and buffered before it can be consumed.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Array" instead.
--
-- @since 0.1.0
{-# INLINE toList #-}
toList :: Monad m => SerialT m a -> m [a]
toList = P.toList

-- |
-- @
-- toListRev = S.foldl' (flip (:)) []
-- @
--
-- Convert a stream into a list in reverse order in the underlying monad.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Array" instead.
--
-- /Internal/
{-# INLINE toListRev #-}
toListRev :: Monad m => SerialT m a -> m [a]
toListRev = D.toListRev . toStreamD

-- |
-- @
-- toHandle h = S.mapM_ $ hPutStrLn h
-- @
--
-- Write a stream of Strings to an IO Handle.
--
-- @since 0.1.0
{-# DEPRECATED toHandle
   "Please use Streamly.FileSystem.Handle module (see the changelog)" #-}
toHandle :: MonadIO m => IO.Handle -> SerialT m String -> m ()
toHandle h = go
    where
    go m1 =
        let stop = return ()
            single a = liftIO (IO.hPutStrLn h a)
            yieldk a r = liftIO (IO.hPutStrLn h a) >> go r
        in K.foldStream defState yieldk single stop m1

-- XXX rename these to write/writeRev to make the naming consistent with folds
-- in other modules.
--
-- | A fold that buffers its input to a pure stream.
--
-- /Warning!/ working on large streams accumulated as buffers in memory could
-- be very inefficient, consider using "Streamly.Data.Array" instead.
--
-- /Internal/
{-# INLINE toStream #-}
toStream :: Monad m => Fold m a (SerialT Identity a)
toStream = Fold (\f x -> return $ FL.Partial $ f . (x `K.cons`))
                (return id)
                (return . ($ K.nil))

-- This is more efficient than 'toStream'. toStream is exactly the same as
-- reversing the stream after toStreamRev.
--
-- | Buffers the input stream to a pure stream in the reverse order of the
-- input.
--
-- /Warning!/ working on large streams accumulated as buffers in memory could
-- be very inefficient, consider using "Streamly.Data.Array" instead.
--
-- /Internal/

--  xn : ... : x2 : x1 : []
{-# INLINABLE toStreamRev #-}
toStreamRev :: Monad m => Fold m a (SerialT Identity a)
toStreamRev = Fold (\xs x -> return $ FL.Partial $ x `K.cons` xs) (return K.nil) return

-- | Convert a stream to a pure stream.
--
-- @
-- toPure = foldr cons nil
-- @
--
-- /Internal/
--
{-# INLINE toPure #-}
toPure :: Monad m => SerialT m a -> m (SerialT Identity a)
toPure = foldr K.cons K.nil

-- | Convert a stream to a pure stream in reverse order.
--
-- @
-- toPureRev = foldl' (flip cons) nil
-- @
--
-- /Internal/
--
{-# INLINE toPureRev #-}
toPureRev :: Monad m => SerialT m a -> m (SerialT Identity a)
toPureRev = foldl' (flip K.cons) K.nil

------------------------------------------------------------------------------
-- Comparison
------------------------------------------------------------------------------

-- | Compare two streams for equality using an equality function.
--
-- @since 0.6.0
{-# INLINABLE eqBy #-}
eqBy :: (IsStream t, Monad m) => (a -> b -> Bool) -> t m a -> t m b -> m Bool
eqBy = P.eqBy

-- | Compare two streams lexicographically using a comparison function.
--
-- @since 0.6.0
{-# INLINABLE cmpBy #-}
cmpBy
    :: (IsStream t, Monad m)
    => (a -> b -> Ordering) -> t m a -> t m b -> m Ordering
cmpBy = P.cmpBy
