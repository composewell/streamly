-- |
-- Module      : Streamly.Internal.Data.MutArray.Stream
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators to efficiently manipulate streams of mutable arrays.
--
-- We can either push these in the MutArray module with a "chunks" prefix or
-- keep this as a separate module and release it.
--
module Streamly.Internal.Data.MutArray.Stream
    (
    -- * Generation
      MArray.chunksOf
    , MArray.pinnedChunksOf
    , MArray.writeChunks -- chunksWrite?
    , MArray.splitOn -- chunksSplitOn

    -- * Compaction
    , packArraysChunksOf
    , SpliceState (..)
    , lpackArraysChunksOf
    , compact -- chunksCompact
    , compactLE
    , compactEQ
    , compactGE

    -- * Elimination
    , MArray.flattenArrays -- chunksConcat
    , MArray.flattenArraysRev -- chunksConcatRev
    , MArray.fromArrayStreamK -- chunksCoalesce
    )
where

#include "inline.hs"
#include "ArrayMacros.h"

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Proxy (Proxy(..))
import Streamly.Internal.Data.Unbox (Unbox, sizeOf)
import Streamly.Internal.Data.MutArray.Type (MutArray(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Parser (ParseError)
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Parser as ParserD
import qualified Streamly.Internal.Data.Stream as D

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
packArraysChunksOf :: (MonadIO m, Unbox a)
    => Int -> D.Stream m (MutArray a) -> D.Stream m (MutArray a)
packArraysChunksOf n (D.Stream step state) =
    D.Stream step' (SpliceInitial state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (SpliceInitial st) = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.MutArray.Type.packArraysChunksOf: the size of "
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
                    buf1 <- MArray.splice buf arr
                    return $ D.Skip (SpliceBuffering s buf1)
            D.Skip s -> return $ D.Skip (SpliceBuffering s buf)
            D.Stop -> return $ D.Skip (SpliceYielding buf SpliceFinish)

    step' _ SpliceFinish = return D.Stop

    step' _ (SpliceYielding arr next) = return $ D.Yield arr next

-- XXX Remove this once compactLEFold is implemented
-- lpackArraysChunksOf = Fold.many compactLEFold
--
{-# INLINE_NORMAL lpackArraysChunksOf #-}
lpackArraysChunksOf :: (MonadIO m, Unbox a)
    => Int -> Fold m (MutArray a) () -> Fold m (MutArray a) ()
lpackArraysChunksOf n (Fold step1 initial1 _ final1) =
    Fold step initial extract final

    where

    initial = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.MutArray.Type.packArraysChunksOf: the size of "
                 ++ "arrays [" ++ show n ++ "] must be a natural number"

        r <- initial1
        return $ first (Tuple' Nothing) r

    step (Tuple' Nothing r1) arr =
            let len = MArray.byteLength arr
             in if len >= n
                then do
                    r <- step1 r1 arr
                    case r of
                        FL.Done _ -> return $ FL.Done ()
                        FL.Partial s -> do
                            _ <- final1 s
                            res <- initial1
                            return $ first (Tuple' Nothing) res
                else return $ FL.Partial $ Tuple' (Just arr) r1

    step (Tuple' (Just buf) r1) arr = do
            let len = MArray.byteLength buf + MArray.byteLength arr
            buf1 <- MArray.splice buf arr

            -- XXX this is common in both the equations of step
            if len >= n
            then do
                r <- step1 r1 buf1
                case r of
                    FL.Done _ -> return $ FL.Done ()
                    FL.Partial s -> do
                        _ <- final1 s
                        res <- initial1
                        return $ first (Tuple' Nothing) res
            else return $ FL.Partial $ Tuple' (Just buf1) r1

    -- XXX Several folds do extract >=> final, therefore, we need to make final
    -- return  "m b" rather than using extract post it if we want extract to be
    -- partial.
    --
    -- extract forces the pending buffer to be sent to the fold which is not
    -- what we want.
    extract _ = error "lpackArraysChunksOf: not designed for scanning"

    final (Tuple' Nothing r1) = final1 r1
    final (Tuple' (Just buf) r1) = do
        r <- step1 r1 buf
        case r of
            FL.Partial rr -> final1 rr
            FL.Done _ -> return ()

-- XXX Same as compactLE, to be removed once that is implemented.
--
-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size in bytes.
--
-- /Internal/
{-# INLINE compact #-}
compact :: (MonadIO m, Unbox a)
    => Int -> Stream m (MutArray a) -> Stream m (MutArray a)
compact = packArraysChunksOf

-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size. Note that if a single array is bigger than the
-- specified size we do not split it to fit. When we coalesce multiple arrays
-- if the size would exceed the specified size we do not coalesce therefore the
-- actual array size may be less than the specified chunk size.
--
-- /Internal/
{-# INLINE_NORMAL compactLEParserD #-}
compactLEParserD ::
       forall m a. (MonadIO m, Unbox a)
    => Int -> ParserD.Parser (MutArray a) m (MutArray a)
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
                buf1 <- MArray.splice buf arr
                return $ ParserD.Partial 0 (Just buf1)

    extract Nothing = return $ ParserD.Done 0 MArray.nil
    extract (Just buf) = return $ ParserD.Done 0 buf

    functionPath =
        "Streamly.Internal.Data.Stream.MutChunked.compactLEParserD"

-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- minimum specified size. Note that if all the arrays in the stream together
-- are smaller than the specified size the resulting array will be smaller than
-- the specified size. When we coalesce multiple arrays if the size would exceed
-- the specified size we stop coalescing further.
--
-- /Internal/
{-# INLINE_NORMAL compactGEFold #-}
compactGEFold ::
       forall m a. (MonadIO m, Unbox a)
    => Int -> FL.Fold m (MutArray a) (MutArray a)
compactGEFold n = Fold step initial extract extract

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
        buf2 <- MArray.spliceUnsafe buf1 arr
        if len >= n
        then return $ FL.Done buf2
        else return $ FL.Partial (Just buf2)

    extract Nothing = return MArray.nil
    extract (Just buf) = return buf

    functionPath =
        "Streamly.Internal.Data.Stream.MutChunked.compactGEFold"

-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size in bytes.
--
-- /Internal/
compactLE :: (MonadIO m, Unbox a) =>
    Int -> Stream m (MutArray a) -> Stream m (Either ParseError (MutArray a))
compactLE n = D.parseManyD (compactLEParserD n)

-- | Like 'compactLE' but generates arrays of exactly equal to the size
-- specified except for the last array in the stream which could be shorter.
--
-- /Unimplemented/
{-# INLINE compactEQ #-}
compactEQ :: -- (MonadIO m, Unbox a) =>
    Int -> Stream m (MutArray a) -> Stream m (MutArray a)
compactEQ _n _xs = undefined
    -- IsStream.fromStreamD $ D.foldMany (compactEQFold n) (IsStream.toStreamD xs)

-- | Like 'compactLE' but generates arrays of size greater than or equal to the
-- specified except for the last array in the stream which could be shorter.
--
-- /Internal/
{-# INLINE compactGE #-}
compactGE ::
       (MonadIO m, Unbox a)
    => Int -> Stream m (MutArray a) -> Stream m (MutArray a)
compactGE n = D.foldMany (compactGEFold n)
