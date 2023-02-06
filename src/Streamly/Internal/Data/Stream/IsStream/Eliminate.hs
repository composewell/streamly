{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Eliminate
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module contains functions ending in the shape:
--
-- @
-- t m a -> m b
-- @
--
-- We call them stream folding functions, they reduce a stream @t m a@ to a
-- monadic value @m b@.

module Streamly.Internal.Data.Stream.IsStream.Eliminate {-# DEPRECATED "Please use \"Streamly.Data.Stream.*\" instead." #-}
    (
    -- * Running Examples
    -- $setup

    -- * Running a 'Fold'
    --  See "Streamly.Internal.Data.Fold".
      fold

    -- * Running a 'Parser'
    -- "Streamly.Internal.Data.Parser".
    , parse
    , parseK
    , parseD

    -- * Stream Deconstruction
    -- | foldr and foldl do not provide the remaining stream.  'uncons' is more
    -- general, as it can be used to implement those as well.  It allows to use
    -- the stream one element at a time, and we have the remaining stream all
    -- the time.
    , uncons

    -- * Right Folds
    , foldrM
    , foldr

    -- * Left Folds
    -- Lazy left folds are useful only for reversing the stream
    , foldlS
    , foldlT

    , foldl'
    , foldl1'
    , foldlM'

    -- * Specific Fold Functions
    -- | Folds as functions of the shape @t m a -> m b@.
    --
    -- These functions are good to run individually but they do not compose
    -- well. Prefer writing folds as the 'Fold' data type. Use folds from
    -- "Streamly.Internal.Data.Fold" instead of using the functions in this
    -- section.
    --
    -- This section can possibly be removed in future.  Are these better in
    -- some case compared to 'Fold'? When the input stream is in CPS style
    -- (StreamK) we may want to rewrite the function call to CPS implementation
    -- of the fold through these definitions. Will that be more efficient for
    -- StreamK?

    -- ** Full Folds

    -- -- ** To Summary (Full Folds)
    , mapM_
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

    -- ** Partial Folds

    -- -- ** To Elements (Partial Folds)
    , drainN
    , drainWhile

    -- -- | Folds that extract selected elements of a stream or their properties.
    , (!!)
    , head
    , headElse
    , tail
    , init
    , findM
    , find
    , findIndex
    , elemIndex
    , lookup

    -- -- ** To Boolean (Partial Folds)
    , null
    , elem
    , notElem
    , all
    , any
    , and
    , or

    -- -- ** Lazy Folds
    -- ** To Containers
    , toList
    , toListRev
    , toStream
    , toStreamRev

    -- * Concurrent Folds
    , foldAsync
    , (|$.)
    , (|&.)

    -- * Multi-Stream folds
    -- Full equivalence
    , eqBy
    , cmpBy

    -- finding subsequences
    , isPrefixOf
    , isInfixOf
    , isSuffixOf
    , isSubsequenceOf

    -- trimming sequences
    , stripPrefix
    -- , stripInfix
    , stripSuffix

    -- * Deprecated
    , foldx
    , foldxM
    , foldr1
    , runStream
    , runN
    , runWhile
    , toHandle
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Foreign.Storable (Storable)
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Parser (Parser (..))
import Streamly.Internal.Data.SVar (defState)
import Streamly.Internal.Data.Stream.IsStream.Common
    ( fold, drop, findIndices, reverse, splitOnSeq
    , take , takeWhile, mkParallel)
import Streamly.Internal.Data.Stream.IsStream.Type
    (IsStream, fromStreamD, toStreamD)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Unboxed (Unbox)

import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.Stream.Serial as Stream
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified Streamly.Internal.Data.Parser.ParserK.Type as PRK
import qualified Streamly.Internal.Data.Stream.IsStream.Type as IsStream
import qualified Streamly.Internal.Data.Stream.StreamD as D
    (foldr1, foldlT, foldlM', mapM_, null, head, headElse, last, elem
    , notElem, all, any, minimum, minimumBy, maximum, maximumBy, the, lookup
    , find, findM, toListRev, isPrefixOf, isSubsequenceOf, stripPrefix,  (!!)
    , fromStreamK, foldrM, parseD)
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
    (uncons, foldlS, tail, init)
import qualified System.IO as IO

import Prelude hiding
       ( drop, take, takeWhile, foldr , foldl, mapM_, sequence, all, any, sum
       , product, elem, notElem, maximum, minimum, head, last, tail, length
       , null , reverse, init, and, or, lookup, foldr1, (!!) , splitAt, break
       , mconcat)

-- $setup
-- >>> :m
-- >>> :set -fno-warn-deprecations
-- >>> import Streamly.Prelude (SerialT)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Internal.Data.Parser as Parser
-- >>> import qualified Streamly.Internal.Data.Fold as Fold

------------------------------------------------------------------------------
-- Deconstruction
------------------------------------------------------------------------------

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns @Just (a, ma)@, where @a@ is
-- the head of the stream and @ma@ its tail.
--
-- This can be used to do pretty much anything in an imperative manner, as it
-- just breaks down the stream into individual elements and we can loop over
-- them as we deem fit. For example, this can be used to convert a streamly
-- stream into other stream types.
--
-- All the folds in this module can be expressed in terms of 'uncons', however,
-- this is generally less efficient than specific folds because it takes apart
-- the stream one element at a time, therefore, does not take adavantage of
-- stream fusion.
--
-- @since 0.1.0
{-# INLINE uncons #-}
uncons :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (a, t m a))
uncons = fmap (fmap (fmap IsStream.fromStream)) . K.uncons . Stream.toStreamK

------------------------------------------------------------------------------
-- Right Folds
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
-- >>> Stream.foldrM (\x xs -> if odd x then return True else xs) (return False) $ Stream.fromList (2:4:5:undefined)
-- True
--
-- /Since: 0.7.0 (signature changed)/
--
-- /Since: 0.2.0 (signature changed)/
--
-- /Since: 0.1.0/
{-# INLINE foldrM #-}
foldrM :: Monad m => (a -> m b -> m b) -> m b -> SerialT m a -> m b
foldrM step acc (Stream.SerialT m) = D.foldrM step acc $ D.fromStreamK m

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
foldr f z = foldrM (\a b -> f a <$> b) (return z)

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
foldr1 f m = D.foldr1 f (toStreamD m)

------------------------------------------------------------------------------
-- Left Folds
------------------------------------------------------------------------------

-- | Lazy left fold to a stream.
{-# INLINE foldlS #-}
foldlS :: IsStream t => (t m b -> a -> t m b) -> t m b -> t m a -> t m b
foldlS f z =
    IsStream.fromStream
        . K.foldlS
            (\xs x -> IsStream.toStream $ f (IsStream.fromStream xs) x)
            (IsStream.toStream z)
        . IsStream.toStream

-- | Lazy left fold to a transformer monad.
--
-- For example, to reverse a stream:
--
-- > D.toList $ D.foldlT (flip D.cons) D.nil $ (D.fromList [1..5] :: SerialT IO Int)
--
{-# INLINE foldlT #-}
foldlT :: (Monad m, IsStream t, Monad (s m), MonadTrans s)
    => (s m b -> a -> s m b) -> s m b -> t m a -> s m b
foldlT f z s = D.foldlT f z (toStreamD s)

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
--
-- @since 0.2.0
{-# DEPRECATED foldx "Please use foldl' followed by fmap instead." #-}
{-# INLINE foldx #-}
foldx :: Monad m => (x -> a -> x) -> x -> (x -> b) -> SerialT m a -> m b
foldx = IsStream.foldlx'

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
foldl' = IsStream.foldl'

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
foldxM = IsStream.foldlMx'

-- | Like 'foldl'' but with a monadic step function.
--
-- /Since: 0.2.0/
--
-- /Since: 0.8.0 (signature change)/
{-# INLINE foldlM' #-}
foldlM' :: Monad m => (b -> a -> m b) -> m b -> SerialT m a -> m b
foldlM' step begin = D.foldlM' step begin . IsStream.toStreamD

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

-- | Parse a stream using the supplied ParserD 'PRD.Parser'.
--
-- /Internal/
--
{-# INLINE_NORMAL parseD #-}
parseD :: Monad m => PRD.Parser a m b -> SerialT m a -> m (Either PRD.ParseError b)
parseD p = D.parseD p . toStreamD

-- | Parse a stream using the supplied ParserK 'PRK.Parser'.
--
-- /Internal/
{-# INLINE parseK #-}
parseK :: Monad m => PRK.Parser a m b -> SerialT m a -> m (Either PRD.ParseError b)
parseK p = parse (PRD.fromParserK p)

-- | Parse a stream using the supplied 'Parser'.
--
-- Unlike folds, parsers may not always result in a valid output, they may
-- result in an error.  For example:
--
-- >>> Stream.parse (Parser.takeEQ 1 Fold.drain) Stream.nil
-- Left (ParseError "takeEQ: Expecting exactly 1 elements, input terminated on 0")
--
-- Note:
--
-- @
-- fold f = Stream.parse (Parser.fromFold f)
-- @
--
-- @parse p@ is not the same as  @head . parseMany p@ on an empty stream.
--
-- /Pre-release/
--
{-# INLINE [3] parse #-}
parse :: Monad m => Parser a m b -> SerialT m a -> m (Either PRD.ParseError b)
parse = parseD

------------------------------------------------------------------------------
-- Specific Fold Functions
------------------------------------------------------------------------------

-- XXX this can utilize parallel mapping if we implement it as drain . mapM
-- |
-- > mapM_ = Stream.drain . Stream.mapM
--
-- Apply a monadic action to each element of the stream and discard the output
-- of the action. This is not really a pure transformation operation but a
-- transformation followed by fold.
--
-- @since 0.1.0
{-# INLINE mapM_ #-}
mapM_ :: Monad m => (a -> m b) -> SerialT m a -> m ()
mapM_ f = D.mapM_ f . IsStream.toStreamD

-- |
-- > drain = mapM_ (\_ -> return ())
-- > drain = Stream.fold Fold.drain
--
-- Run a stream, discarding the results. By default it interprets the stream
-- as 'SerialT', to run other types of streams use the type adapting
-- combinators for example @Stream.drain . 'fromAsync'@.
--
-- @since 0.7.0
{-# INLINE drain #-}
drain :: Monad m => SerialT m a -> m ()
drain = IsStream.drain

-- |
-- > drainN n = Stream.drain . Stream.take n
-- > drainN n = Stream.fold (Fold.take n Fold.drain)
--
-- Run maximum up to @n@ iterations of a stream.
--
-- @since 0.7.0
{-# INLINE drainN #-}
drainN :: Monad m => Int -> SerialT m a -> m ()
drainN n = drain . take n

-- |
-- > runN n = runStream . take n
--
-- Run maximum up to @n@ iterations of a stream.
--
-- @since 0.6.0
{-# DEPRECATED runN "Please use \"drainN\" instead" #-}
{-# INLINE runN #-}
runN :: Monad m => Int -> SerialT m a -> m ()
runN = drainN

-- |
-- > drainWhile p = Stream.drain . Stream.takeWhile p
--
-- Run a stream as long as the predicate holds true.
--
-- @since 0.7.0
{-# INLINE drainWhile #-}
drainWhile :: Monad m => (a -> Bool) -> SerialT m a -> m ()
drainWhile p = drain . takeWhile p

-- |
-- > runWhile p = runStream . takeWhile p
--
-- Run a stream as long as the predicate holds true.
--
-- @since 0.6.0
{-# DEPRECATED runWhile "Please use \"drainWhile\" instead" #-}
{-# INLINE runWhile #-}
runWhile :: Monad m => (a -> Bool) -> SerialT m a -> m ()
runWhile = drainWhile

-- | Run a stream, discarding the results. By default it interprets the stream
-- as 'SerialT', to run other types of streams use the type adapting
-- combinators for example @runStream . 'fromAsync'@.
--
-- @since 0.2.0
{-# DEPRECATED runStream "Please use \"drain\" instead" #-}
{-# INLINE runStream #-}
runStream :: Monad m => SerialT m a -> m ()
runStream = drain

-- | Determine whether the stream is empty.
--
-- > null = Stream.fold Fold.null
--
-- @since 0.1.1
{-# INLINE null #-}
null :: Monad m => SerialT m a -> m Bool
null = D.null . toStreamD

-- | Extract the first element of the stream, if any.
--
-- > head = (!! 0)
-- > head = Stream.fold Fold.one
--
-- @since 0.1.0
{-# INLINE head #-}
head :: Monad m => SerialT m a -> m (Maybe a)
head = D.head . toStreamD

-- | Extract the first element of the stream, if any, otherwise use the
-- supplied default value. It can help avoid one branch in high performance
-- code.
--
-- /Pre-release/
{-# INLINE headElse #-}
headElse :: Monad m => a -> SerialT m a -> m a
headElse x = D.headElse x . toStreamD

-- |
-- > tail = fmap (fmap snd) . Stream.uncons
--
-- Extract all but the first element of the stream, if any.
--
-- @since 0.1.1
{-# INLINE tail #-}
tail :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (t m a))
tail m = fmap (fmap IsStream.fromStream) $ K.tail $ Stream.toStreamK m

-- | Extract all but the last element of the stream, if any.
--
-- @since 0.5.0
{-# INLINE init #-}
init :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (t m a))
init m = fmap (fmap IsStream.fromStream) $ K.init $ Stream.toStreamK m

-- | Extract the last element of the stream, if any.
--
-- > last xs = xs !! (Stream.length xs - 1)
-- > last = Stream.fold Fold.last
--
-- @since 0.1.1
{-# INLINE last #-}
last :: Monad m => SerialT m a -> m (Maybe a)
last m = D.last $ toStreamD m

-- | Determine whether an element is present in the stream.
--
-- > elem = Stream.fold Fold.elem
--
-- @since 0.1.0
{-# INLINE elem #-}
elem :: (Monad m, Eq a) => a -> SerialT m a -> m Bool
elem e m = D.elem e (toStreamD m)

-- | Determine whether an element is not present in the stream.
--
-- > notElem = Stream.fold Fold.length
--
-- @since 0.1.0
{-# INLINE notElem #-}
notElem :: (Monad m, Eq a) => a -> SerialT m a -> m Bool
notElem e m = D.notElem e (toStreamD m)

-- | Determine the length of the stream.
--
-- @since 0.1.0
{-# INLINE length #-}
length :: Monad m => SerialT m a -> m Int
length = foldl' (\n _ -> n + 1) 0

-- | Determine whether all elements of a stream satisfy a predicate.
--
-- > all = Stream.fold Fold.all
--
-- @since 0.1.0
{-# INLINE all #-}
all :: Monad m => (a -> Bool) -> SerialT m a -> m Bool
all p m = D.all p (toStreamD m)

-- | Determine whether any of the elements of a stream satisfy a predicate.
--
-- > any = Stream.fold Fold.any
--
-- @since 0.1.0
{-# INLINE any #-}
any :: Monad m => (a -> Bool) -> SerialT m a -> m Bool
any p m = D.any p (toStreamD m)

-- | Determines if all elements of a boolean stream are True.
--
-- > and = Stream.fold Fold.and
--
-- @since 0.5.0
{-# INLINE and #-}
and :: Monad m => SerialT m Bool -> m Bool
and = all (==True)

-- | Determines whether at least one element of a boolean stream is True.
--
-- > or = Stream.fold Fold.or
--
-- @since 0.5.0
{-# INLINE or #-}
or :: Monad m => SerialT m Bool -> m Bool
or = any (==True)

-- | Determine the sum of all elements of a stream of numbers. Returns @0@ when
-- the stream is empty. Note that this is not numerically stable for floating
-- point numbers.
--
-- > sum = Stream.fold Fold.sum
--
-- @since 0.1.0
{-# INLINE sum #-}
sum :: (Monad m, Num a) => SerialT m a -> m a
sum = foldl' (+) 0

-- | Determine the product of all elements of a stream of numbers. Returns @1@
-- when the stream is empty.
--
-- > product = Stream.fold Fold.product
--
-- @since 0.1.1
{-# INLINE product #-}
product :: (Monad m, Num a) => SerialT m a -> m a
product = foldl' (*) 1

-- | Fold a stream of monoid elements by appending them.
--
-- > mconcat = Stream.fold Fold.mconcat
--
-- /Pre-release/
{-# INLINE mconcat #-}
mconcat :: (Monad m, Monoid a) => SerialT m a -> m a
mconcat = foldr mappend mempty

-- |
-- @
-- minimum = 'minimumBy' compare
 -- minimum = Stream.fold Fold.minimum
-- @
--
-- Determine the minimum element in a stream.
--
-- @since 0.1.0
{-# INLINE minimum #-}
minimum :: (Monad m, Ord a) => SerialT m a -> m (Maybe a)
minimum m = D.minimum (toStreamD m)

-- | Determine the minimum element in a stream using the supplied comparison
-- function.
--
-- > minimumBy = Stream.fold Fold.minimumBy
--
-- @since 0.6.0
{-# INLINE minimumBy #-}
minimumBy :: Monad m => (a -> a -> Ordering) -> SerialT m a -> m (Maybe a)
minimumBy cmp m = D.minimumBy cmp (toStreamD m)

-- |
-- @
-- maximum = 'maximumBy' compare
-- maximum = Stream.fold Fold.maximum
-- @
--
-- Determine the maximum element in a stream.
--
-- @since 0.1.0
{-# INLINE maximum #-}
maximum :: (Monad m, Ord a) => SerialT m a -> m (Maybe a)
maximum m = D.maximum (toStreamD m)

-- | Determine the maximum element in a stream using the supplied comparison
-- function.
--
-- > maximumBy = Stream.fold Fold.maximumBy
--
-- @since 0.6.0
{-# INLINE maximumBy #-}
maximumBy :: Monad m => (a -> a -> Ordering) -> SerialT m a -> m (Maybe a)
maximumBy cmp m = D.maximumBy cmp (toStreamD m)

-- | Ensures that all the elements of the stream are identical and then returns
-- that unique element.
--
-- @since 0.6.0
{-# INLINE the #-}
the :: (Eq a, Monad m) => SerialT m a -> m (Maybe a)
the m = D.the (toStreamD m)

------------------------------------------------------------------------------
-- Searching
------------------------------------------------------------------------------

-- | Lookup the element at the given index.
--
-- @since 0.6.0
{-# INLINE (!!) #-}
(!!) :: Monad m => SerialT m a -> Int -> m (Maybe a)
m !! i = toStreamD m D.!! i

-- | In a stream of (key-value) pairs @(a, b)@, return the value @b@ of the
-- first pair where the key equals the given value @a@.
--
-- > lookup = snd <$> Stream.find ((==) . fst)
-- > lookup = Stream.fold Fold.lookup
--
-- @since 0.5.0
{-# INLINE lookup #-}
lookup :: (Monad m, Eq a) => a -> SerialT m (a, b) -> m (Maybe b)
lookup a m = D.lookup a (toStreamD m)

-- | Like 'findM' but with a non-monadic predicate.
--
-- > find p = findM (return . p)
-- > find = Stream.fold Fold.find
--
-- @since 0.5.0
{-# INLINE find #-}
find :: Monad m => (a -> Bool) -> SerialT m a -> m (Maybe a)
find p m = D.find p (toStreamD m)

-- | Returns the first element that satisfies the given predicate.
--
-- > findM = Stream.fold Fold.findM
--
-- @since 0.6.0
{-# INLINE findM #-}
findM :: Monad m => (a -> m Bool) -> SerialT m a -> m (Maybe a)
findM p m = D.findM p (toStreamD m)

-- | Returns the first index that satisfies the given predicate.
--
-- > findIndex = Stream.fold Fold.findIndex
--
-- @since 0.5.0
{-# INLINE findIndex #-}
findIndex :: Monad m => (a -> Bool) -> SerialT m a -> m (Maybe Int)
findIndex p = head . findIndices p

-- | Returns the first index where a given value is found in the stream.
--
-- > elemIndex a = Stream.findIndex (== a)
--
-- @since 0.5.0
{-# INLINE elemIndex #-}
elemIndex :: (Monad m, Eq a) => a -> SerialT m a -> m (Maybe Int)
elemIndex a = findIndex (== a)

------------------------------------------------------------------------------
-- To containers
------------------------------------------------------------------------------

-- |
-- @
-- toList = Stream.foldr (:) []
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
toList = IsStream.toList

-- |
-- @
-- toListRev = Stream.foldl' (flip (:)) []
-- @
--
-- Convert a stream into a list in reverse order in the underlying monad.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Array" instead.
--
-- /Pre-release/
{-# INLINE toListRev #-}
toListRev :: Monad m => SerialT m a -> m [a]
toListRev = D.toListRev . toStreamD

-- |
-- @
-- toHandle h = D.mapM_ $ hPutStrLn h
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
        in IsStream.foldStream defState yieldk single stop m1

-- | Convert a stream to a pure stream.
--
-- @
-- toStream = Stream.foldr Stream.cons Stream.nil
-- @
--
-- /Pre-release/
--
{-# INLINE toStream #-}
toStream :: Monad m => SerialT m a -> m (SerialT n a)
toStream = foldr IsStream.cons IsStream.nil

-- | Convert a stream to a pure stream in reverse order.
--
-- @
-- toStreamRev = Stream.foldl' (flip Stream.cons) Stream.nil
-- @
--
-- /Pre-release/
--
{-# INLINE toStreamRev #-}
toStreamRev :: Monad m => SerialT m a -> m (SerialT n a)
toStreamRev = foldl' (flip IsStream.cons) IsStream.nil

------------------------------------------------------------------------------
-- Concurrent Application
------------------------------------------------------------------------------

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
-- In the example below, each stage introduces a delay of 1 sec but output is
-- printed every second because both stages are concurrent.
--
-- >>> import Control.Concurrent (threadDelay)
-- >>> import Streamly.Prelude ((|$.))
-- >>> :{
--  Stream.foldlM' (\_ a -> threadDelay 1000000 >> print a) (return ())
--      |$. Stream.replicateM 3 (threadDelay 1000000 >> return 1)
-- :}
-- 1
-- 1
-- 1
--
-- /Concurrent/
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE (|$.) #-}
(|$.) :: (IsStream t, MonadAsync m) => (t m a -> m b) -> (t m a -> m b)
-- (|$.) f = f . Async.mkAsync
(|$.) f = f . mkParallel

infixr 0 |$.

-- | Same as '|$.'.
--
--  /Internal/
--
{-# INLINE foldAsync #-}
foldAsync :: (IsStream t, MonadAsync m) => (t m a -> m b) -> (t m a -> m b)
foldAsync = (|$.)

-- | Same as '|$.' but with arguments reversed.
--
-- > (|&.) = flip (|$.)
--
-- /Concurrent/
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE (|&.) #-}
(|&.) :: (IsStream t, MonadAsync m) => t m a -> (t m a -> m b) -> m b
x |&. f = f |$. x

infixl 1 |&.

------------------------------------------------------------------------------
-- Multi-stream folds
------------------------------------------------------------------------------

-- | Returns 'True' if the first stream is the same as or a prefix of the
-- second. A stream is a prefix of itself.
--
-- >>> Stream.isPrefixOf (Stream.fromList "hello") (Stream.fromList "hello" :: SerialT IO Char)
-- True
--
-- @since 0.6.0
{-# INLINE isPrefixOf #-}
isPrefixOf :: (Eq a, IsStream t, Monad m) => t m a -> t m a -> m Bool
isPrefixOf m1 m2 = D.isPrefixOf (toStreamD m1) (toStreamD m2)

-- | Returns 'True' if the first stream is an infix of the second. A stream is
-- considered an infix of itself.
--
-- > Stream.isInfixOf (Stream.fromList "hello") (Stream.fromList "hello" :: SerialT IO Char)
-- True
--
-- Space: @O(n)@ worst case where @n@ is the length of the infix.
--
-- /Pre-release/
--
-- /Requires 'Storable' constraint/
--
{-# INLINE isInfixOf #-}
isInfixOf :: (MonadIO m, Eq a, Enum a, Storable a, Unbox a)
    => SerialT m a -> SerialT m a -> m Bool
isInfixOf infx stream = do
    arr <- fold A.write infx
    -- XXX can use breakOnSeq instead (when available)
    r <- null $ drop 1 $ splitOnSeq arr FL.drain stream
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
-- >>> Stream.isSuffixOf (Stream.fromList "hello") (Stream.fromList "hello" :: SerialT IO Char)
-- True
--
-- Space: @O(n)@, buffers entire input stream and the suffix.
--
-- /Pre-release/
--
-- /Suboptimal/ - Help wanted.
--
{-# INLINE isSuffixOf #-}
isSuffixOf :: (Monad m, Eq a) => SerialT m a -> SerialT m a -> m Bool
isSuffixOf suffix stream = reverse suffix `isPrefixOf` reverse stream

-- | Returns 'True' if all the elements of the first stream occur, in order, in
-- the second stream. The elements do not have to occur consecutively. A stream
-- is a subsequence of itself.
--
-- >>> Stream.isSubsequenceOf (Stream.fromList "hlo") (Stream.fromList "hello" :: SerialT IO Char)
-- True
--
-- @since 0.6.0
{-# INLINE isSubsequenceOf #-}
isSubsequenceOf :: (Eq a, IsStream t, Monad m) => t m a -> t m a -> m Bool
isSubsequenceOf m1 m2 = D.isSubsequenceOf (toStreamD m1) (toStreamD m2)

-- Note: If we want to return a Maybe value to know whether the
-- suffix/infix was present or not along with the stripped stream then
-- we need to buffer the whole input stream.
--
-- | @stripPrefix prefix stream@ strips @prefix@ from @stream@ if it is a
-- prefix of stream. Returns 'Nothing' if the stream does not start with the
-- given prefix, stripped stream otherwise. Returns @Just nil@ when the prefix
-- is the same as the stream.
--
-- See also "Streamly.Internal.Data.Stream.IsStream.Nesting.dropPrefix".
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

-- | Drops the given suffix from a stream. Returns 'Nothing' if the stream does
-- not end with the given suffix. Returns @Just nil@ when the suffix is the
-- same as the stream.
--
-- It may be more efficient to convert the stream to an Array and use
-- stripSuffix on that especially if the elements have a Storable or Prim
-- instance.
--
-- See also "Streamly.Internal.Data.Stream.IsStream.Nesting.dropSuffix".
--
-- Space: @O(n)@, buffers the entire input stream as well as the suffix
--
-- /Pre-release/
{-# INLINE stripSuffix #-}
stripSuffix
    :: (Monad m, Eq a)
    => SerialT m a -> SerialT m a -> m (Maybe (SerialT m a))
stripSuffix m1 m2 = fmap reverse <$> stripPrefix (reverse m1) (reverse m2)

------------------------------------------------------------------------------
-- Comparison
------------------------------------------------------------------------------

-- | Compare two streams for equality using an equality function.
--
-- @since 0.6.0
{-# INLINABLE eqBy #-}
eqBy :: (IsStream t, Monad m) => (a -> b -> Bool) -> t m a -> t m b -> m Bool
eqBy = IsStream.eqBy

-- | Compare two streams lexicographically using a comparison function.
--
-- @since 0.6.0
{-# INLINABLE cmpBy #-}
cmpBy
    :: (IsStream t, Monad m)
    => (a -> b -> Ordering) -> t m a -> t m b -> m Ordering
cmpBy = IsStream.cmpBy
