-- |
-- Module      : Streamly.Internal.Data.Array.Stream.Mut.Foreign
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators to efficiently manipulate streams of mutable arrays.
--
module Streamly.Internal.Data.Array.Stream.Mut.Foreign
    (
    -- * Generation
      arraysOf

    -- * Compaction
    , packArraysChunksOf
    , SpliceState (..)
    , lpackArraysChunksOf
    , compact
    , compactLE
    , compactEQ
    , compactGE
    )
where

#include "inline.hs"
#include "ArrayMacros.h"

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow)
import Data.Bifunctor (first)
import Streamly.Internal.Data.Unboxed (Storable, sizeOf)
import Streamly.Internal.Data.Array.Foreign.Mut.Type (Array(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as MArray
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Parser.ParserD as ParserD
import qualified Streamly.Internal.Data.Stream.Type as Stream

-- | @arraysOf n stream@ groups the elements in the input stream into arrays of
-- @n@ elements each.
--
-- Same as the following but may be more efficient:
--
-- > arraysOf n = Stream.foldMany (MArray.writeN n)
--
-- /Pre-release/
{-# INLINE arraysOf #-}
arraysOf :: (MonadIO m, Storable a)
    => Int -> Stream m a -> Stream m (Array a)
arraysOf n = Stream.fromStreamD . MArray.arraysOf n . Stream.toStreamD

-------------------------------------------------------------------------------
-- Compact
-------------------------------------------------------------------------------

data SpliceState s arr
    = SpliceInitial s
    | SpliceBuffering s arr
    | SpliceYielding arr (SpliceState s arr)
    | SpliceFinish

-- XXX This can be removed once compactLEFold/compactLE are implemented.
--
-- | This mutates the first array (if it has space) to append values from the
-- second one. This would work for immutable arrays as well because an
-- immutable array never has space so a new array is allocated instead of
-- mutating it.
--
-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size. Note that if a single array is bigger than the
-- specified size we do not split it to fit. When we coalesce multiple arrays
-- if the size would exceed the specified size we do not coalesce therefore the
-- actual array size may be less than the specified chunk size.
--
-- @since 0.7.0
{-# INLINE_NORMAL packArraysChunksOf #-}
packArraysChunksOf :: (MonadIO m, Storable a)
    => Int -> D.Stream m (Array a) -> D.Stream m (Array a)
packArraysChunksOf n (D.Stream step state) =
    D.Stream step' (SpliceInitial state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (SpliceInitial st) = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.Array.Foreign.Mut.Type.packArraysChunksOf: the size of "
                 ++ "arrays [" ++ show n ++ "] must be a natural number"
        r <- step gst st
        case r of
            D.Yield arr s -> return $
                let len = MArray.byteLength arr
                 in if len >= n
                    then D.Skip (SpliceYielding arr (SpliceInitial s))
                    else D.Skip (SpliceBuffering s arr)
            D.Skip s -> return $ D.Skip (SpliceInitial s)
            D.Stop -> return D.Stop

    step' gst (SpliceBuffering st buf) = do
        r <- step gst st
        case r of
            D.Yield arr s -> do
                let len = MArray.byteLength buf + MArray.byteLength arr
                if len > n
                then return $
                    D.Skip (SpliceYielding buf (SpliceBuffering s arr))
                else do
                    buf' <- if MArray.byteCapacity buf < n
                            then liftIO $ MArray.realloc n buf
                            else return buf
                    buf'' <- MArray.splice buf' arr
                    return $ D.Skip (SpliceBuffering s buf'')
            D.Skip s -> return $ D.Skip (SpliceBuffering s buf)
            D.Stop -> return $ D.Skip (SpliceYielding buf SpliceFinish)

    step' _ SpliceFinish = return D.Stop

    step' _ (SpliceYielding arr next) = return $ D.Yield arr next

-- XXX Remove this once compactLEFold is implemented
-- lpackArraysChunksOf = Fold.many compactLEFold
--
{-# INLINE_NORMAL lpackArraysChunksOf #-}
lpackArraysChunksOf :: (MonadIO m, Storable a)
    => Int -> Fold m (Array a) () -> Fold m (Array a) ()
lpackArraysChunksOf n (Fold step1 initial1 extract1) =
    Fold step initial extract

    where

    initial = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.Array.Foreign.Mut.Type.packArraysChunksOf: the size of "
                 ++ "arrays [" ++ show n ++ "] must be a natural number"

        r <- initial1
        return $ first (Tuple' Nothing) r

    extract (Tuple' Nothing r1) = extract1 r1
    extract (Tuple' (Just buf) r1) = do
        r <- step1 r1 buf
        case r of
            FL.Partial rr -> extract1 rr
            FL.Done _ -> return ()

    step (Tuple' Nothing r1) arr =
            let len = MArray.byteLength arr
             in if len >= n
                then do
                    r <- step1 r1 arr
                    case r of
                        FL.Done _ -> return $ FL.Done ()
                        FL.Partial s -> do
                            extract1 s
                            res <- initial1
                            return $ first (Tuple' Nothing) res
                else return $ FL.Partial $ Tuple' (Just arr) r1

    step (Tuple' (Just buf) r1) arr = do
            let len = MArray.byteLength buf + MArray.byteLength arr
            buf' <- if MArray.byteCapacity buf < len
                    then liftIO $ MArray.realloc (max n len) buf
                    else return buf
            buf'' <- MArray.splice buf' arr

            -- XXX this is common in both the equations of step
            if len >= n
            then do
                r <- step1 r1 buf''
                case r of
                    FL.Done _ -> return $ FL.Done ()
                    FL.Partial s -> do
                        extract1 s
                        res <- initial1
                        return $ first (Tuple' Nothing) res
            else return $ FL.Partial $ Tuple' (Just buf'') r1

-- XXX Same as compactLE, to be removed once that is implemented.
--
-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size in bytes.
--
-- /Internal/
{-# INLINE compact #-}
compact :: (MonadIO m, Storable a)
    => Int -> Stream m (Array a) -> Stream m (Array a)
compact n = Stream.fromStreamD . packArraysChunksOf n . Stream.toStreamD

-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size. Note that if a single array is bigger than the
-- specified size we do not split it to fit. When we coalesce multiple arrays
-- if the size would exceed the specified size we do not coalesce therefore the
-- actual array size may be less than the specified chunk size.
--
-- /Internal/
{-# INLINE_NORMAL compactLEParserD #-}
compactLEParserD ::
       forall m a. (MonadThrow m, MonadIO m, Storable a)
    => Int -> ParserD.Parser m (Array a) (Array a)
compactLEParserD n = ParserD.Parser step initial extract

    where

    nBytes = n * SIZE_OF(a)

    initial =
        return
            $ if n <= 0
              then error
                       $ functionPath
                       ++ ": the size of arrays ["
                       ++ show n ++ "] must be a natural number"
              else ParserD.IPartial Nothing

    step Nothing arr =
        return
            $ let len = MArray.byteLength arr
               in if len >= nBytes
                  then ParserD.Done 0 arr
                  else ParserD.Partial 0 (Just arr)
    step (Just buf) arr =
        let len = MArray.byteLength buf + MArray.byteLength arr
         in if len > nBytes
            then return $ ParserD.Done 1 buf
            else do
                buf1 <-
                    if MArray.byteCapacity buf < nBytes
                    then liftIO $ MArray.realloc nBytes buf
                    else return buf
                buf2 <- MArray.splice buf1 arr
                return $ ParserD.Partial 0 (Just buf2)

    extract Nothing = return MArray.nil
    extract (Just buf) = return buf

    functionPath =
        "Streamly.Internal.Data.Array.Stream.Mut.Foreign.compactLEParserD"

-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- minimum specified size. Note that if all the arrays in the stream together
-- are smaller than the specified size the resulting array will be smaller than
-- the specified size. When we coalesce multiple arrays if the size would exceed
-- the specified size we stop coalescing further.
--
-- /Internal/
{-# INLINE_NORMAL compactGEFold #-}
compactGEFold ::
       forall m a. (MonadIO m, Storable a)
    => Int -> FL.Fold m (Array a) (Array a)
compactGEFold n = Fold step initial extract

    where

    nBytes = n * SIZE_OF(a)

    initial =
        return
            $ if n < 0
              then error
                       $ functionPath
                       ++ ": the size of arrays ["
                       ++ show n ++ "] must be a natural number"
              else FL.Partial Nothing

    step Nothing arr =
        return
            $ let len = MArray.byteLength arr
               in if len >= nBytes
                  then FL.Done arr
                  else FL.Partial (Just arr)
    step (Just buf) arr = do
        let len = MArray.byteLength buf + MArray.byteLength arr
        buf1 <-
            if MArray.byteCapacity buf < len
            then liftIO $ MArray.realloc (max len nBytes) buf
            else return buf
        buf2 <- MArray.splice buf1 arr
        if len >= n
        then return $ FL.Done buf2
        else return $ FL.Partial (Just buf2)

    extract Nothing = return MArray.nil
    extract (Just buf) = return buf

    functionPath =
        "Streamly.Internal.Data.Array.Stream.Mut.Foreign.compactGEFold"

-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size in bytes.
--
-- /Internal/
compactLE :: (MonadThrow m, MonadIO m, Storable a) =>
    Int -> Stream m (Array a) -> Stream m (Array a)
compactLE n =
    Stream.fromStreamD . D.parseMany (compactLEParserD n) . Stream.toStreamD

-- | Like 'compactLE' but generates arrays of exactly equal to the size
-- specified except for the last array in the stream which could be shorter.
--
-- /Unimplemented/
{-# INLINE compactEQ #-}
compactEQ :: -- (MonadIO m, Storable a) =>
    Int -> Stream m (Array a) -> Stream m (Array a)
compactEQ _n _xs = undefined
    -- IsStream.fromStreamD $ D.foldMany (compactEQFold n) (IsStream.toStreamD xs)

-- | Like 'compactLE' but generates arrays of size greater than or equal to the
-- specified except for the last array in the stream which could be shorter.
--
-- /Internal/
{-# INLINE compactGE #-}
compactGE ::
       (MonadIO m, Storable a)
    => Int -> Stream m (Array a) -> Stream m (Array a)
compactGE n =
    Stream.fromStreamD . D.foldMany (compactGEFold n) . Stream.toStreamD
