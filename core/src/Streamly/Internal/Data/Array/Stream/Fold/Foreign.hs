-- |
-- Module      : Streamly.Internal.Data.Array.Stream.Fold.Foreign
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
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
-- >>> import qualified Streamly.Internal.Data.Array.Stream.Foreign as ArrayStream
-- >>> import qualified Streamly.Internal.Data.Array.Stream.Fold.Foreign as ArrayFold
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream (arraysOf)
-- >>> import qualified Streamly.Prelude as Stream
--
-- >>> f = ArrayFold.fromFold (Fold.take 7 Fold.toList)
-- >>> s = Stream.arraysOf 5 $ Stream.fromList "hello world"
-- >>> ArrayStream.runArrayFold f s
-- "hello w"
--
module Streamly.Internal.Data.Array.Stream.Fold.Foreign
    (
      ArrayFold (..)

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
    , serialWith

    -- * Monad
    , concatMap

    -- * Combinators
    , take
    )
where

#include "ArrayMacros.h"

import Control.Applicative (liftA2)
import Control.Exception (assert)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Ptr (minusPtr, plusPtr)
import Foreign.Storable (Storable(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Array.Unboxed.Mut.Type (touch)
import Streamly.Internal.Data.Array.Unboxed.Type (Array(..))
import Streamly.Internal.Data.Parser.ParserD (Initial(..), Step(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Streamly.Internal.Data.Array.Unboxed as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser.ParserD as ParserD
import qualified Streamly.Internal.Data.Parser.ParserD.Type as ParserD
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
newtype ArrayFold m a b = ArrayFold (ParserD.Parser m (Array a) b)

-------------------------------------------------------------------------------
-- Constructing array stream folds from element folds and parsers
-------------------------------------------------------------------------------

-- | Convert an element 'Fold' into an array stream fold.
--
-- /Pre-release/
{-# INLINE fromFold #-}
fromFold :: forall m a b. (MonadIO m, Storable a) =>
    Fold.Fold m a b -> ArrayFold m a b
fromFold (Fold.Fold fstep finitial fextract) =
    ArrayFold (ParserD.Parser step initial fextract)

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
            liftIO $ touch contents
            return $ Partial 0 fs
        goArray !_ !cur !fs = do
            x <- liftIO $ peek cur
            res <- fstep fs x
            let elemSize = SIZE_OF(a)
                next = PTR_NEXT(cur,a)
            case res of
                Fold.Done b ->
                    return $ Done ((end `minusPtr` next) `div` elemSize) b
                Fold.Partial fs1 ->
                    goArray SPEC next fs1

-- | Convert an element 'ParserD.Parser' into an array stream fold. If the
-- parser fails the fold would throw an exception.
--
-- /Pre-release/
{-# INLINE fromParserD #-}
fromParserD :: forall m a b. (MonadIO m, Storable a) =>
    ParserD.Parser m a b -> ArrayFold m a b
fromParserD (ParserD.Parser step1 initial1 extract1) =
    ArrayFold (ParserD.Parser step initial1 extract1)

    where

    step s (Array contents start end) = do
        if start >= end
        then return $ Continue 0 s
        else goArray SPEC start s

        where

        {-# INLINE partial #-}
        partial arrRem cur next elemSize st n fs1 = do
            let next1 = next `plusPtr` negate (n * elemSize)
            if next1 >= start && cur < end
            then goArray SPEC next1 fs1
            else return $ st (arrRem + n) fs1

        goArray !_ !cur !fs = do
            x <- liftIO $ peek cur
            liftIO $ touch contents
            res <- step1 fs x
            let elemSize = SIZE_OF(a)
                next = PTR_NEXT(cur,a)
                arrRem = (end `minusPtr` next) `div` elemSize
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
fromParser :: forall m a b. (MonadThrow m, MonadIO m, Storable a) =>
    Parser.Parser m a b -> ArrayFold m a b
fromParser = fromParserD . ParserD.fromParserK

-- | Adapt an array stream fold.
--
-- /Pre-release/
{-# INLINE adaptFold #-}
adaptFold :: forall m a b. (MonadIO m) =>
    Fold.Fold m (Array a) b -> ArrayFold m a b
adaptFold f = ArrayFold $ ParserD.fromFold f

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

-- | Maps a function over the result of fold.
--
-- /Pre-release/
instance Functor m => Functor (ArrayFold m a) where
    {-# INLINE fmap #-}
    fmap f (ArrayFold p) = ArrayFold $ fmap f p

-- | Map a monadic function on the output of a fold.
--
-- /Pre-release/
{-# INLINE rmapM #-}
rmapM :: Monad m => (b -> m c) -> ArrayFold m a b -> ArrayFold m a c
rmapM f (ArrayFold p) = ArrayFold $ ParserD.rmapM f p

-------------------------------------------------------------------------------
-- Sequential applicative
-------------------------------------------------------------------------------

-- | A fold that always yields a pure value without consuming any input.
--
-- /Pre-release/
--
{-# INLINE fromPure #-}
fromPure :: Monad m => b -> ArrayFold m a b
fromPure = ArrayFold . ParserD.fromPure

-- | A fold that always yields the result of an effectful action without
-- consuming any input.
--
-- /Pre-release/
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> ArrayFold m a b
fromEffect = ArrayFold . ParserD.fromEffect

-- | Applies two folds sequentially on the input stream and combines their
-- results using the supplied function.
--
-- /Pre-release/
{-# INLINE serial_ #-}
serial_ :: MonadThrow m =>
    ArrayFold m x a -> ArrayFold m x b -> ArrayFold m x b
serial_ (ArrayFold p1) (ArrayFold p2) =
    ArrayFold $ ParserD.noErrorUnsafeSplit_ p1 p2

-- | Applies two folds sequentially on the input stream and combines their
-- results using the supplied function.
--
-- /Pre-release/
{-# INLINE serialWith #-}
serialWith :: MonadThrow m
    => (a -> b -> c) -> ArrayFold m x a -> ArrayFold m x b -> ArrayFold m x c
serialWith f (ArrayFold p1) (ArrayFold p2) =
    ArrayFold $ ParserD.noErrorUnsafeSplitWith f p1 p2

-- | 'Applicative' form of 'serialWith'.
-- > (<*>) = serialWith id
instance MonadThrow m => Applicative (ArrayFold m a) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = serialWith id

    {-# INLINE (*>) #-}
    (*>) = serial_

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
concatMap :: MonadThrow m =>
    (b -> ArrayFold m a c) -> ArrayFold m a b -> ArrayFold m a c
concatMap func (ArrayFold p) =
    let f x = let ArrayFold y = func x in y
     in ArrayFold $ ParserD.noErrorUnsafeConcatMap f p

-- | Monad instance applies folds sequentially. Next fold can depend on the
-- output of the previous fold. See 'concatMap'.
--
-- > (>>=) = flip concatMap
instance MonadThrow m => Monad (ArrayFold m a) where
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
take :: forall m a b. (Monad m, Storable a) =>
    Int -> ArrayFold m a b -> ArrayFold m a b
take n (ArrayFold (ParserD.Parser step1 initial1 extract1)) =
    ArrayFold $ ParserD.Parser step initial extract

    where

    initial = do
        res <- initial1
        case res of
            IPartial s ->
                if n > 0
                then return $ IPartial $ Tuple' n s
                else IDone <$> extract1 s
            IDone b -> return $ IDone b
            IError err -> return $ IError err

    {-# INLINE partial #-}
    partial i1 st j s =
        let i2 = i1 + j
         in if i2 > 0
            then return $ st j (Tuple' i2 s)
            else Done 0 <$> extract1 s -- i2 == i1 == j == 0

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
                end = PTR_INDEX(start,i,a)
                arr1 = Array contents start end
                remaining = negate i1
            res <- step1 r arr1
            case res of
                Partial 0 s -> Done remaining <$> extract1 s
                Partial j s -> return $ Partial (remaining + j) (Tuple' j s)
                Continue 0 s -> Done remaining <$> extract1 s
                Continue j s -> return $ Continue (remaining + j) (Tuple' j s)
                Done j b -> return $ Done (remaining + j) b
                Error err -> return $ Error err

    extract (Tuple' _ r) = extract1 r
