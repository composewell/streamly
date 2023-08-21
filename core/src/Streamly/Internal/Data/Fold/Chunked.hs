-- |
-- Module      : Streamly.Internal.Data.Fold.Chunked
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Use "Streamly.Data.Parser.Chunked" instead.
--
-- Fold a stream of foreign arrays.  @Fold m a b@ in this module works
-- on a stream of "Array a" and produces an output of type @b@.
--
-- Though @Fold m a b@ in this module works on a stream of @Array a@ it is
-- different from @Data.Fold m (Array a) b@.  While the latter works on arrays
-- as a whole treating them as atomic elements, the folds in this module can
-- work on the stream of arrays as if it is an element stream with all the
-- arrays coalesced together. This module allows adapting the element stream
-- folds in Data.Fold to correctly work on an array stream as if it is an
-- element stream. For example:
--
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Stream.Chunked as ArrayStream
-- >>> import qualified Streamly.Internal.Data.Fold.Chunked as ChunkFold
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Data.StreamK as StreamK
--
-- >>> f = ChunkFold.fromFold (Fold.take 7 Fold.toList)
-- >>> s = Stream.chunksOf 5 $ Stream.fromList "hello world"
-- >>> ArrayStream.runArrayFold f (StreamK.fromStream s)
-- Right "hello w"
--
module Streamly.Internal.Data.Fold.Chunked
    (
      ChunkFold (..)

    -- * Construction
    , fromFold
    , adaptFold
    , fromParser
    , fromParserD

    -- * Mapping
    , rmapM

    -- * Applicative
    , fromPure
    , fromEffect
    , splitWith

    -- * Monad
    , concatMap

    -- * Combinators
    , take
    )
where

#include "ArrayMacros.h"

import Control.Applicative (liftA2)
import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (first)
import Data.Proxy (Proxy(..))
import Streamly.Internal.Data.Unbox (Unbox(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.MutArray.Type (touch)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.Parser (Initial(..), Step(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Streamly.Internal.Data.Array.Type as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as ParserD
import qualified Streamly.Internal.Data.Parser as Parser

import Prelude hiding (concatMap, take)

-- | Array stream fold.
--
-- An array stream fold is basically an array stream "Parser" that does not
-- fail.  In case of array stream folds the count in 'Partial', 'Continue' and
-- 'Done' is a count of elements that includes the leftover element count in
-- the array that is currently being processed by the parser. If none of the
-- elements is consumed by the parser the count is at least the whole array
-- length. If the whole array is consumed by the parser then the count will be
-- 0.
--
-- /Pre-release/
--
newtype ChunkFold m a b = ChunkFold (ParserD.Parser (Array a) m b)

-------------------------------------------------------------------------------
-- Constructing array stream folds from element folds and parsers
-------------------------------------------------------------------------------

-- | Convert an element 'Fold' into an array stream fold.
--
-- /Pre-release/
{-# INLINE fromFold #-}
fromFold :: forall m a b. (MonadIO m, Unbox a) =>
    Fold.Fold m a b -> ChunkFold m a b
fromFold (Fold.Fold fstep finitial fextract) =
    ChunkFold (ParserD.Parser step initial (fmap (Done 0) . fextract))

    where

    initial = do
        res <- finitial
        return
            $ case res of
                  Fold.Partial s1 -> IPartial s1
                  Fold.Done b -> IDone b

    step s (Array contents start end) = do
        goArray SPEC start s

        where

        goArray !_ !cur !fs | cur >= end = do
            assert (cur == end) (return ())
            return $ Partial 0 fs
        goArray !_ !cur !fs = do
            x <- liftIO $ peekByteIndex cur contents
            res <- fstep fs x
            let elemSize = SIZE_OF(a)
                next = INDEX_NEXT(cur,a)
            case res of
                Fold.Done b ->
                    return $ Done ((end - next) `div` elemSize) b
                Fold.Partial fs1 ->
                    goArray SPEC next fs1

-- | Convert an element 'ParserD.Parser' into an array stream fold. If the
-- parser fails the fold would throw an exception.
--
-- /Pre-release/
{-# INLINE fromParserD #-}
fromParserD :: forall m a b. (MonadIO m, Unbox a) =>
    ParserD.Parser a m b -> ChunkFold m a b
fromParserD (ParserD.Parser step1 initial1 extract1) =
    ChunkFold (ParserD.Parser step initial1 extract1)

    where

    step s (Array contents start end) = do
        if start >= end
        then return $ Continue 0 s
        else goArray SPEC start s

        where

        {-# INLINE partial #-}
        partial arrRem cur next elemSize st n fs1 = do
            let next1 = next - (n * elemSize)
            if next1 >= start && cur < end
            then goArray SPEC next1 fs1
            else return $ st (arrRem + n) fs1

        goArray !_ !cur !fs = do
            x <- liftIO $ peekByteIndex cur contents
            liftIO $ touch contents
            res <- step1 fs x
            let elemSize = SIZE_OF(a)
                next = INDEX_NEXT(cur,a)
                arrRem = (end - next) `div` elemSize
            case res of
                ParserD.Done n b -> do
                    return $ Done (arrRem + n) b
                ParserD.Partial n fs1 ->
                    partial arrRem cur next elemSize Partial n fs1
                ParserD.Continue n fs1 -> do
                    partial arrRem cur next elemSize Continue n fs1
                Error err -> return $ Error err

-- | Convert an element 'Parser.Parser' into an array stream fold. If the
-- parser fails the fold would throw an exception.
--
-- /Pre-release/
{-# INLINE fromParser #-}
fromParser :: forall m a b. (MonadIO m, Unbox a) =>
    Parser.Parser a m b -> ChunkFold m a b
fromParser = fromParserD

-- | Adapt an array stream fold.
--
-- /Pre-release/
{-# INLINE adaptFold #-}
adaptFold :: forall m a b. (MonadIO m) =>
    Fold.Fold m (Array a) b -> ChunkFold m a b
adaptFold f = ChunkFold $ ParserD.fromFold f

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

-- | Maps a function over the result of fold.
--
-- /Pre-release/
instance Functor m => Functor (ChunkFold m a) where
    {-# INLINE fmap #-}
    fmap f (ChunkFold p) = ChunkFold $ fmap f p

-- | Map a monadic function on the output of a fold.
--
-- /Pre-release/
{-# INLINE rmapM #-}
rmapM :: Monad m => (b -> m c) -> ChunkFold m a b -> ChunkFold m a c
rmapM f (ChunkFold p) = ChunkFold $ ParserD.rmapM f p

-------------------------------------------------------------------------------
-- Sequential applicative
-------------------------------------------------------------------------------

-- | A fold that always yields a pure value without consuming any input.
--
-- /Pre-release/
--
{-# INLINE fromPure #-}
fromPure :: Monad m => b -> ChunkFold m a b
fromPure = ChunkFold . ParserD.fromPure

-- | A fold that always yields the result of an effectful action without
-- consuming any input.
--
-- /Pre-release/
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> ChunkFold m a b
fromEffect = ChunkFold . ParserD.fromEffect

-- | Applies two folds sequentially on the input stream and combines their
-- results using the supplied function.
--
-- /Pre-release/
{-# INLINE split_ #-}
split_ :: Monad m =>
    ChunkFold m x a -> ChunkFold m x b -> ChunkFold m x b
split_ (ChunkFold p1) (ChunkFold p2) =
    ChunkFold $ ParserD.noErrorUnsafeSplit_ p1 p2

-- | Applies two folds sequentially on the input stream and combines their
-- results using the supplied function.
--
-- /Pre-release/
{-# INLINE splitWith #-}
splitWith :: Monad m
    => (a -> b -> c) -> ChunkFold m x a -> ChunkFold m x b -> ChunkFold m x c
splitWith f (ChunkFold p1) (ChunkFold p2) =
    ChunkFold $ ParserD.noErrorUnsafeSplitWith f p1 p2

-- | 'Applicative' form of 'splitWith'.
-- > (<*>) = splitWith id
instance Monad m => Applicative (ChunkFold m a) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = splitWith id

    {-# INLINE (*>) #-}
    (*>) = split_

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

-- XXX This should be implemented using CPS
--
-- | Applies a fold on the input stream, generates the next fold from the
-- output of the previously applied fold and then applies that fold.
--
-- /Pre-release/
--
{-# INLINE concatMap #-}
concatMap :: Monad m =>
    (b -> ChunkFold m a c) -> ChunkFold m a b -> ChunkFold m a c
concatMap func (ChunkFold p) =
    let f x = let ChunkFold y = func x in y
     in ChunkFold $ ParserD.noErrorUnsafeConcatMap f p

-- | Monad instance applies folds sequentially. Next fold can depend on the
-- output of the previous fold. See 'concatMap'.
--
-- > (>>=) = flip concatMap
instance Monad m => Monad (ChunkFold m a) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = flip concatMap

    {-# INLINE (>>) #-}
    (>>) = (*>)

-------------------------------------------------------------------------------
-- Array to Array folds
-------------------------------------------------------------------------------

-- | Take @n@ array elements (@a@) from a stream of arrays (@Array a@).
{-# INLINE take #-}
take :: forall m a b. (Monad m, Unbox a) =>
    Int -> ChunkFold m a b -> ChunkFold m a b
take n (ChunkFold (ParserD.Parser step1 initial1 extract1)) =
    ChunkFold $ ParserD.Parser step initial extract

    where

    -- XXX Need to make the Initial type Step to remove this
    iextract s = do
        r <- extract1 s
        return $ case r of
            Done _ b -> IDone b
            Error err -> IError err
            _ -> error "Bug: ChunkFold take invalid state in initial"

    initial = do
        res <- initial1
        case res of
            IPartial s ->
                if n > 0
                then return $ IPartial $ Tuple' n s
                else iextract s
            IDone b -> return $ IDone b
            IError err -> return $ IError err

    {-# INLINE partial #-}
    partial i1 st j s =
        let i2 = i1 + j
         in if i2 > 0
            then return $ st j (Tuple' i2 s)
            else do
                -- i2 == i1 == j == 0
                r <- extract1 s
                return $ case r of
                    Error err -> Error err
                    Done n1 b -> Done n1 b
                    Continue n1 s1 -> Continue n1 (Tuple' i2 s1)
                    Partial _ _ -> error "Partial in extract"

    -- Tuple' (how many more items to take) (fold state)
    step (Tuple' i r) arr = do
        let len = Array.length arr
            i1 = i - len
        if i1 >= 0
        then do
            res <- step1 r arr
            case res of
                Partial j s -> partial i1 Partial j s
                Continue j s -> partial i1 Continue j s
                Done j b -> return $ Done j b
                Error err -> return $ Error err
        else do
            let !(Array contents start _) = arr
                end = INDEX_OF(start,i,a)
                -- Supply only the required slice of array
                arr1 = Array contents start end
                remaining = negate i1 -- i1 is negative here
            res <- step1 r arr1
            case res of
                Partial 0 s ->
                    ParserD.bimapOverrideCount
                        remaining (Tuple' 0) id <$> extract1 s
                Partial j s -> return $ Partial (remaining + j) (Tuple' j s)
                Continue 0 s ->
                    ParserD.bimapOverrideCount
                        remaining (Tuple' 0) id <$> extract1 s
                Continue j s -> return $ Continue (remaining + j) (Tuple' j s)
                Done j b -> return $ Done (remaining + j) b
                Error err -> return $ Error err

    extract (Tuple' i r) = first (Tuple' i) <$> extract1 r
