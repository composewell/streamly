{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- |
-- Module      : Streamly.Internal.Data.Array.Stream
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Combinators to efficiently manipulate streams of immutable arrays.
--
-- We can either push these in the MutArray module with a "chunks" prefix or
-- keep this as a separate module and release it.
--
module Streamly.Internal.Data.Array.Stream
{-# DEPRECATED "Please use \"Streamly.Internal.Data.Array\" instead." #-}
    (
    -- * Creation
      Array.chunksOf
    , Array.pinnedChunksOf
    , Array.bufferChunks

    -- * Flattening to elements
    , Array.concat
    , Array.flattenArrays
    , Array.concatRev
    , Array.flattenArraysRev
    , Array.interpose
    , Array.interposeSuffix
    , Array.intercalateSuffix
    , unlines

    -- * Elimination
    -- ** Element Folds
    -- The byte level foldBreak can work as efficiently as the chunk level. We
    -- can flatten the stream to byte stream and use that. But if we want the
    -- remaining stream to be a chunk stream then this could be handy. But it
    -- could also be implemented using parseBreak.
    , foldBreak
    , foldBreakD
    -- This is chunked parseBreak. A byte level parseBreak cannot work
    -- efficiently. Because the stream will have to be a StreamK for
    -- backtracking, StreamK at byte level would not be efficient.
    -- parseBreak p = K.parseBreakChunks (ParserK.adaptC p)
    , parseBreak
    -- , foldManyChunks
    -- , parseManyChunks
    , K.parseBreakChunks
    , K.parseChunks

    -- ** Array Folds
    -- XXX Use parseBreakChunks/parseChunks instead
    -- foldBreak can be implemented using parseBreak. Use StreamK.
    , runArrayFold
    , runArrayFoldBreak
    -- , parseArr
    , runArrayParserDBreak -- StreamK.parseBreakChunks
    , runArrayFoldMany

    , toArray

    -- * Compaction
    -- We can use something like foldManyChunks, parseManyChunks with a take
    -- fold.
    , lpackArraysChunksOf -- Fold.compactChunks
    , compact -- rechunk, compactChunks

    -- * Splitting
    -- We can use something like foldManyChunks, parseManyChunks with an
    -- appropriate splitting fold.
    , splitOn       -- Stream.rechunkOn
    , splitOnSuffix -- Stream.rechunkOnSuffix
    )
where

#include "ArrayMacros.h"
#include "inline.hs"

import Data.Bifunctor (second)
import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO(..))
-- import Data.Bifunctor (first)
-- import Data.Either (fromRight)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Streamly.Internal.Data.Unbox (Unbox(..))
import Fusion.Plugin.Types (Fuse(..))
import GHC.Exts (SpecConstrAnnotation(..))
import GHC.Types (SPEC(..))
import Prelude hiding (null, last, (!!), read, concat, unlines)

import Streamly.Data.Fold (Fold)
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Fold.Chunked (ChunkFold(..))
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.StreamK (StreamK, fromStream, toStream)
import Streamly.Internal.Data.SVar.Type (adaptState, defState)

import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser as PRD
    (Parser(..), Initial(..))
-- import qualified Streamly.Internal.Data.ParserK as ParserK
import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.StreamK as K

-- XXX Since these are immutable arrays MonadIO constraint can be removed from
-- most places.

-------------------------------------------------------------------------------
-- Intersperse and append
-------------------------------------------------------------------------------

{-# INLINE_NORMAL unlines #-}
unlines :: forall m a. (MonadIO m, Unbox a)
    => a -> D.Stream m (Array a) -> D.Stream m a
unlines = Array.interposeSuffix

-------------------------------------------------------------------------------
-- Compact
-------------------------------------------------------------------------------

-- XXX instead of writing two different versions of this operation, we should
-- write it as a pipe.
--
-- XXX Confirm that immutable arrays won't be modified.
{-# INLINE_NORMAL lpackArraysChunksOf #-}
lpackArraysChunksOf :: (MonadIO m, Unbox a)
    => Int -> Fold m (Array a) () -> Fold m (Array a) ()
lpackArraysChunksOf = Array.lCompactGE

-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size in bytes.
--
-- @since 0.7.0
{-# INLINE compact #-}
compact :: (MonadIO m, Unbox a)
    => Int -> Stream m (Array a) -> Stream m (Array a)
compact = Array.compactLE

-- | Given a stream of arrays, splice them all together to generate a single
-- array. The stream must be /finite/.
--
-- @since 0.7.0
{-# INLINE toArray #-}
toArray :: (MonadIO m, Unbox a) => Stream m (Array a) -> m (Array a)
toArray = Array.fromChunks

-------------------------------------------------------------------------------
-- Split
-------------------------------------------------------------------------------

-- XXX Remove MonadIO constraint.
-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
-- @since 0.7.0
{-# INLINE splitOn #-}
splitOn
    :: (MonadIO m)
    => Word8
    -> Stream m (Array Word8)
    -> Stream m (Array Word8)
splitOn = Array.compactOnByte

{-# INLINE splitOnSuffix #-}
splitOnSuffix
    :: (MonadIO m)
    => Word8
    -> Stream m (Array Word8)
    -> Stream m (Array Word8)
splitOnSuffix = Array.compactOnByteSuffix

-------------------------------------------------------------------------------
-- Elimination - Running folds
-------------------------------------------------------------------------------

{-# INLINE_NORMAL foldBreakD #-}
foldBreakD :: forall m a b. (MonadIO m, Unbox a) =>
    Fold m a b -> D.Stream m (Array a) -> m (b, D.Stream m (Array a))
foldBreakD = Array.foldBreakChunks

-- | Fold an array stream using the supplied 'Fold'. Returns the fold result
-- and the unconsumed stream.
--
-- > foldBreak f = runArrayFoldBreak (ChunkFold.fromFold f)
--
-- Instead of using this we can adapt the fold to ParserK and use
-- parseBreakChunks instead. ParserK allows composing using Monad as well.
--
-- @
-- foldBreak f s =
--       fmap (first (fromRight undefined))
--     $ K.parseBreakChunks (ParserK.adaptC (PR.fromFold f)) s
-- @
--
-- We can compare perf and remove this one or define it in terms of that.
--
-- /Internal/
--
{-# INLINE_NORMAL foldBreak #-}
foldBreak ::
       (MonadIO m, Unbox a)
    => Fold m a b
    -> StreamK m (A.Array a)
    -> m (b, StreamK m (A.Array a))
foldBreak = Array.foldBreakChunksK
--
-- foldBreak f s = fmap fromStreamD <$> foldBreakD f (toStreamD s)
--
-- foldBreak f s =
--       fmap (first (fromRight undefined))
--     $ K.parseBreakChunks (ParserK.adaptC (PR.fromFold f)) s
--
-- If foldBreak performs better than runArrayFoldBreak we can use a rewrite
-- rule to rewrite runArrayFoldBreak to fold.
-- foldBreak f = runArrayFoldBreak (ChunkFold.fromFold f)

-------------------------------------------------------------------------------
-- Elimination - running element parsers
-------------------------------------------------------------------------------

-- When we have to take an array partially, take the last part of the array.
{-# INLINE takeArrayListRev #-}
takeArrayListRev :: forall a. Unbox a => Int -> [Array a] -> [Array a]
takeArrayListRev = go

    where

    go _ [] = []
    go n _ | n <= 0 = []
    go n (x:xs) =
        let len = Array.length x
        in if n > len
           then x : go (n - len) xs
           else if n == len
           then [x]
           else let !(Array contents _ end) = x
                    !start = end - (n * SIZE_OF(a))
                 in [Array contents start end]

-- When we have to take an array partially, take the last part of the array in
-- the first split.
{-# INLINE splitAtArrayListRev #-}
splitAtArrayListRev ::
    forall a. Unbox a => Int -> [Array a] -> ([Array a],[Array a])
splitAtArrayListRev n ls
  | n <= 0 = ([], ls)
  | otherwise = go n ls
    where
        go :: Int -> [Array a] -> ([Array a], [Array a])
        go _  []     = ([], [])
        go m (x:xs) =
            let len = Array.length x
                (xs', xs'') = go (m - len) xs
             in if m > len
                then (x:xs', xs'')
                else if m == len
                then ([x],xs)
                else let !(Array contents start end) = x
                         end1 = end - (m * SIZE_OF(a))
                         arr2 = Array contents start end1
                         arr1 = Array contents end1 end
                      in ([arr1], arr2:xs)

-- GHC parser does not accept {-# ANN type [] NoSpecConstr #-}, so we need
-- to make a newtype.
{-# ANN type List NoSpecConstr #-}
newtype List a = List {getList :: [a]}

-- | Parse an array stream using the supplied 'Parser'.  Returns the parse
-- result and the unconsumed stream. Throws 'ParseError' if the parse fails.
--
-- >> parseBreak p = K.parseBreakChunks (ParserK.adaptC p)
--
-- This is redundant and we can just use parseBreakChunks, as ParserK can be
-- composed using Monad. The only advantage of this is that we do not need to
-- adapt.
--
-- We can compare perf and remove this one or define it in terms of that.
--
-- /Internal/
--
{-# INLINE_NORMAL parseBreak #-}
parseBreak ::
       (MonadIO m, Unbox a)
    => PR.Parser a m b
    -> StreamK m (A.Array a)
    -> m (Either ParseError b, StreamK m (A.Array a))
{-
parseBreak p s =
    fmap fromStreamD <$> parseBreak (PRD.fromParserK p) (toStreamD s)
-}
parseBreak p = Array.parseBreak (Array.parserK p)

-------------------------------------------------------------------------------
-- Elimination - Running Array Folds and parsers
-------------------------------------------------------------------------------

-- | Note that this is not the same as using a @Parser (Array a) m b@ with the
-- regular "Streamly.Internal.Data.IsStream.parse" function. The regular parse
-- would consume the input arrays as single unit. This parser parses in the way
-- as described in the ChunkFold module. The input arrays are treated as @n@
-- element units and can be consumed partially. The remaining elements are
-- inserted in the source stream as an array.
--
{-# INLINE_NORMAL runArrayParserDBreak #-}
runArrayParserDBreak ::
       forall m a b. (MonadIO m, Unbox a)
    => PRD.Parser (Array a) m b
    -> D.Stream m (Array.Array a)
    -> m (Either ParseError b, D.Stream m (Array.Array a))
runArrayParserDBreak
    (PRD.Parser pstep initial extract)
    stream@(D.Stream step state) = do

    res <- initial
    case res of
        PRD.IPartial s -> go SPEC state (List []) s
        PRD.IDone b -> return (Right b, stream)
        PRD.IError err -> return (Left (ParseError err), stream)

    where

    -- "backBuf" contains last few items in the stream that we may have to
    -- backtrack to.
    --
    -- XXX currently we are using a dumb list based approach for backtracking
    -- buffer. This can be replaced by a sliding/ring buffer using Data.Array.
    -- That will allow us more efficient random back and forth movement.
    go _ st backBuf !pst = do
        r <- step defState st
        case r of
            D.Yield x s -> gobuf SPEC [x] s backBuf pst
            D.Skip s -> go SPEC s backBuf pst
            D.Stop -> goStop backBuf pst

    gobuf !_ [] s backBuf !pst = go SPEC s backBuf pst
    gobuf !_ (x:xs) s backBuf !pst = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                 gobuf SPEC xs s (List []) pst1
            PR.Partial n pst1 -> do
                assert
                    (n <= sum (map Array.length (x:getList backBuf)))
                    (return ())
                let src0 = takeArrayListRev n (x:getList backBuf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC src s (List []) pst1
            PR.Continue 0 pst1 ->
                gobuf SPEC xs s (List (x:getList backBuf)) pst1
            PR.Continue n pst1 -> do
                assert
                    (n <= sum (map Array.length (x:getList backBuf)))
                    (return ())
                let (src0, buf1) = splitAtArrayListRev n (x:getList backBuf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC src s (List buf1) pst1
            PR.Done 0 b -> do
                let str = D.append (D.fromList xs) (D.Stream step s)
                return (Right b, str)
            PR.Done n b -> do
                assert
                    (n <= sum (map Array.length (x:getList backBuf)))
                    (return ())
                let src0 = takeArrayListRev n (x:getList backBuf)
                    src = Prelude.reverse src0 ++ xs
                return (Right b, D.append (D.fromList src) (D.Stream step s))
            PR.SError err -> do
                let src0 = x:getList backBuf
                    src = Prelude.reverse src0 ++ x:xs
                    strm = D.append (D.fromList src) (D.Stream step s)
                return (Left (ParseError err), strm)

    -- This is a simplified gobuf
    goExtract _ [] backBuf !pst = goStop backBuf pst
    goExtract _ (x:xs) backBuf !pst = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                 goExtract SPEC xs (List []) pst1
            PR.Partial n pst1 -> do
                assert
                    (n <= sum (map Array.length (x:getList backBuf)))
                    (return ())
                let src0 = takeArrayListRev n (x:getList backBuf)
                    src  = Prelude.reverse src0 ++ xs
                goExtract SPEC src (List []) pst1
            PR.Continue 0 pst1 ->
                goExtract SPEC xs (List (x:getList backBuf)) pst1
            PR.Continue n pst1 -> do
                assert
                    (n <= sum (map Array.length (x:getList backBuf)))
                    (return ())
                let (src0, buf1) = splitAtArrayListRev n (x:getList backBuf)
                    src  = Prelude.reverse src0 ++ xs
                goExtract SPEC src (List buf1) pst1
            PR.Done 0 b ->
                return (Right b, D.fromList xs)
            PR.Done n b -> do
                assert
                    (n <= sum (map Array.length (x:getList backBuf)))
                    (return ())
                let src0 = takeArrayListRev n (x:getList backBuf)
                    src = Prelude.reverse src0 ++ xs
                return (Right b, D.fromList src)
            PR.SError err -> do
                let src0 = getList backBuf
                    src = Prelude.reverse src0 ++ x:xs
                return (Left (ParseError err), D.fromList src)

    -- This is a simplified goExtract
    {-# INLINE goStop #-}
    goStop backBuf pst = do
        pRes <- extract pst
        case pRes of
            PR.FContinue 0 pst1 ->
                goStop backBuf pst1
            PR.FContinue n pst1 -> do
                assert
                    (n <= sum (map Array.length (getList backBuf)))
                    (return ())
                let (src0, buf1) = splitAtArrayListRev n (getList backBuf)
                    src = Prelude.reverse src0
                goExtract SPEC src (List buf1) pst1
            PR.FDone 0 b -> return (Right b, D.nil)
            PR.FDone n b -> do
                assert
                    (n <= sum (map Array.length (getList backBuf)))
                    (return ())
                let src0 = takeArrayListRev n (getList backBuf)
                    src = Prelude.reverse src0
                return (Right b, D.fromList src)
            PR.FError err -> do
                let src0 = getList backBuf
                    src = Prelude.reverse src0
                return (Left (ParseError err), D.fromList src)

{-
-- | Parse an array stream using the supplied 'Parser'.  Returns the parse
-- result and the unconsumed stream. Throws 'ParseError' if the parse fails.
--
-- /Internal/
--
{-# INLINE parseArr #-}
parseArr ::
       (MonadIO m, MonadThrow m, Unbox a)
    => ASF.Parser a m b
    -> Stream m (A.Array a)
    -> m (b, Stream m (A.Array a))
parseArr p s = fmap fromStreamD <$> parseBreak p (toStreamD s)
-}

-- | Fold an array stream using the supplied array stream 'Fold'.
--
-- /Pre-release/
--
{-# INLINE runArrayFold #-}
runArrayFold :: (MonadIO m, Unbox a) =>
    ChunkFold m a b -> StreamK m (A.Array a) -> m (Either ParseError b)
runArrayFold (ChunkFold p) s = fst <$> runArrayParserDBreak p (toStream s)

-- | Like 'fold' but also returns the remaining stream.
--
-- /Pre-release/
--
{-# INLINE runArrayFoldBreak #-}
runArrayFoldBreak :: (MonadIO m, Unbox a) =>
    ChunkFold m a b -> StreamK m (A.Array a) -> m (Either ParseError b, StreamK m (A.Array a))
runArrayFoldBreak (ChunkFold p) s =
    second fromStream <$> runArrayParserDBreak p (toStream s)

{-# ANN type ParseChunksState Fuse #-}
data ParseChunksState x inpBuf st pst =
      ParseChunksInit inpBuf st
    | ParseChunksInitBuf inpBuf
    | ParseChunksInitLeftOver inpBuf
    | ParseChunksStream st inpBuf !pst
    | ParseChunksStop inpBuf !pst
    | ParseChunksBuf inpBuf st inpBuf !pst
    | ParseChunksExtract inpBuf inpBuf !pst
    | ParseChunksYield x (ParseChunksState x inpBuf st pst)

{-# INLINE_NORMAL runArrayFoldManyD #-}
runArrayFoldManyD
    :: (Monad m, Unbox a)
    => ChunkFold m a b
    -> D.Stream m (Array a)
    -> D.Stream m (Either ParseError b)
runArrayFoldManyD
    (ChunkFold (PRD.Parser pstep initial extract)) (D.Stream step state) =

    D.Stream stepOuter (ParseChunksInit [] state)

    where

    {-# INLINE_LATE stepOuter #-}
    -- Buffer is empty, get the first element from the stream, initialize the
    -- fold and then go to stream processing loop.
    stepOuter gst (ParseChunksInit [] st) = do
        r <- step (adaptState gst) st
        case r of
            D.Yield x s -> do
                res <- initial
                case res of
                    PRD.IPartial ps ->
                        return $ D.Skip $ ParseChunksBuf [x] s [] ps
                    PRD.IDone pb -> do
                        let next = ParseChunksInit [x] s
                        return $ D.Skip $ ParseChunksYield (Right pb) next
                    PRD.IError err -> do
                        let next = ParseChunksInitLeftOver []
                        return
                            $ D.Skip
                            $ ParseChunksYield (Left (ParseError err)) next
            D.Skip s -> return $ D.Skip $ ParseChunksInit [] s
            D.Stop   -> return D.Stop

    -- Buffer is not empty, go to buffered processing loop
    stepOuter _ (ParseChunksInit src st) = do
        res <- initial
        case res of
            PRD.IPartial ps ->
                return $ D.Skip $ ParseChunksBuf src st [] ps
            PRD.IDone pb ->
                let next = ParseChunksInit src st
                 in return $ D.Skip $ ParseChunksYield (Right pb) next
            PRD.IError err -> do
                let next = ParseChunksInitLeftOver []
                return
                    $ D.Skip
                    $ ParseChunksYield (Left (ParseError err)) next

    -- This is a simplified ParseChunksInit
    stepOuter _ (ParseChunksInitBuf src) = do
        res <- initial
        case res of
            PRD.IPartial ps ->
                return $ D.Skip $ ParseChunksExtract src [] ps
            PRD.IDone pb ->
                let next = ParseChunksInitBuf src
                 in return $ D.Skip $ ParseChunksYield (Right pb) next
            PRD.IError err -> do
                let next = ParseChunksInitLeftOver []
                return
                    $ D.Skip
                    $ ParseChunksYield (Left (ParseError err)) next

    -- XXX we just discard any leftover input at the end
    stepOuter _ (ParseChunksInitLeftOver _) = return D.Stop

    -- Buffer is empty, process elements from the stream
    stepOuter gst (ParseChunksStream st backBuf pst) = do
        r <- step (adaptState gst) st
        case r of
            D.Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    PR.Partial 0 pst1 ->
                        return $ D.Skip $ ParseChunksStream s [] pst1
                    PR.Partial n pst1 -> do
                        assert
                            (n <= sum (map Array.length (x:backBuf)))
                            (return ())
                        let src0 = takeArrayListRev n (x:backBuf)
                            src  = Prelude.reverse src0
                        return $ D.Skip $ ParseChunksBuf src s [] pst1
                    PR.Continue 0 pst1 ->
                        return $ D.Skip $ ParseChunksStream s (x:backBuf) pst1
                    PR.Continue n pst1 -> do
                        assert
                            (n <= sum (map Array.length (x:backBuf)))
                            (return ())
                        let (src0, buf1) = splitAtArrayListRev n (x:backBuf)
                            src  = Prelude.reverse src0
                        return $ D.Skip $ ParseChunksBuf src s buf1 pst1
                    PR.Done 0 b -> do
                        return $ D.Skip $
                            ParseChunksYield (Right b) (ParseChunksInit [] s)
                    PR.Done n b -> do
                        assert
                            (n <= sum (map Array.length (x:backBuf)))
                            (return ())
                        let src0 = takeArrayListRev n (x:backBuf)
                            src = Prelude.reverse src0
                            next = ParseChunksInit src s
                        return
                            $ D.Skip
                            $ ParseChunksYield (Right b) next
                    PR.SError err -> do
                        let next = ParseChunksInitLeftOver []
                        return
                            $ D.Skip
                            $ ParseChunksYield (Left (ParseError err)) next

            D.Skip s -> return $ D.Skip $ ParseChunksStream s backBuf pst
            D.Stop -> return $ D.Skip $ ParseChunksStop backBuf pst

    -- go back to stream processing mode
    stepOuter _ (ParseChunksBuf [] s buf pst) =
        return $ D.Skip $ ParseChunksStream s buf pst

    -- buffered processing loop
    stepOuter _ (ParseChunksBuf (x:xs) s backBuf pst) = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                return $ D.Skip $ ParseChunksBuf xs s [] pst1
            PR.Partial n pst1 -> do
                assert (n <= sum (map Array.length (x:backBuf))) (return ())
                let src0 = takeArrayListRev n (x:backBuf)
                    src  = Prelude.reverse src0 ++ xs
                return $ D.Skip $ ParseChunksBuf src s [] pst1
            PR.Continue 0 pst1 ->
                return $ D.Skip $ ParseChunksBuf xs s (x:backBuf) pst1
            PR.Continue n pst1 -> do
                assert (n <= sum (map Array.length (x:backBuf))) (return ())
                let (src0, buf1) = splitAtArrayListRev n (x:backBuf)
                    src  = Prelude.reverse src0 ++ xs
                return $ D.Skip $ ParseChunksBuf src s buf1 pst1
            PR.Done 0 b ->
                return
                    $ D.Skip
                    $ ParseChunksYield (Right b) (ParseChunksInit xs s)
            PR.Done n b -> do
                assert (n <= sum (map Array.length (x:backBuf))) (return ())
                let src0 = takeArrayListRev n (x:backBuf)
                    src = Prelude.reverse src0 ++ xs
                return
                    $ D.Skip
                    $ ParseChunksYield (Right b) (ParseChunksInit src s)
            PR.SError err -> do
                let next = ParseChunksInitLeftOver []
                return
                    $ D.Skip
                    $ ParseChunksYield (Left (ParseError err)) next

    -- This is a simplified ParseChunksBuf
    stepOuter _ (ParseChunksExtract [] buf pst) =
        return $ D.Skip $ ParseChunksStop buf pst

    stepOuter _ (ParseChunksExtract (x:xs) backBuf pst) = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                return $ D.Skip $ ParseChunksExtract xs [] pst1
            PR.Partial n pst1 -> do
                assert (n <= sum (map Array.length (x:backBuf))) (return ())
                let src0 = takeArrayListRev n (x:backBuf)
                    src  = Prelude.reverse src0 ++ xs
                return $ D.Skip $ ParseChunksExtract src [] pst1
            PR.Continue 0 pst1 ->
                return $ D.Skip $ ParseChunksExtract xs (x:backBuf) pst1
            PR.Continue n pst1 -> do
                assert (n <= sum (map Array.length (x:backBuf))) (return ())
                let (src0, buf1) = splitAtArrayListRev n (x:backBuf)
                    src  = Prelude.reverse src0 ++ xs
                return $ D.Skip $ ParseChunksExtract src buf1 pst1
            PR.Done 0 b ->
                return
                    $ D.Skip
                    $ ParseChunksYield (Right b) (ParseChunksInitBuf xs)
            PR.Done n b -> do
                assert (n <= sum (map Array.length (x:backBuf))) (return ())
                let src0 = takeArrayListRev n (x:backBuf)
                    src = Prelude.reverse src0 ++ xs
                return
                    $ D.Skip
                    $ ParseChunksYield (Right b) (ParseChunksInitBuf src)
            PR.SError err -> do
                let next = ParseChunksInitLeftOver []
                return
                    $ D.Skip
                    $ ParseChunksYield (Left (ParseError err)) next


    -- This is a simplified ParseChunksExtract
    stepOuter _ (ParseChunksStop backBuf pst) = do
        pRes <- extract pst
        case pRes of
            PR.FContinue 0 pst1 ->
                return $ D.Skip $ ParseChunksStop backBuf pst1
            PR.FContinue n pst1 -> do
                assert (n <= sum (map Array.length backBuf)) (return ())
                let (src0, buf1) = splitAtArrayListRev n backBuf
                    src  = Prelude.reverse src0
                return $ D.Skip $ ParseChunksExtract src buf1 pst1
            PR.FDone 0 b ->
                return
                    $ D.Skip
                    $ ParseChunksYield (Right b) (ParseChunksInitLeftOver [])
            PR.FDone n b -> do
                assert (n <= sum (map Array.length backBuf)) (return ())
                let src0 = takeArrayListRev n backBuf
                    src = Prelude.reverse src0
                return
                    $ D.Skip
                    $ ParseChunksYield (Right b) (ParseChunksInitBuf src)
            PR.FError err -> do
                let next = ParseChunksInitLeftOver []
                return
                    $ D.Skip
                    $ ParseChunksYield (Left (ParseError err)) next

    stepOuter _ (ParseChunksYield a next) = return $ D.Yield a next

-- | Apply an 'ChunkFold' repeatedly on an array stream and emit the
-- fold outputs in the output stream.
--
-- See "Streamly.Data.Stream.foldMany" for more details.
--
-- /Pre-release/
{-# INLINE runArrayFoldMany #-}
runArrayFoldMany
    :: (Monad m, Unbox a)
    => ChunkFold m a b
    -> StreamK m (Array a)
    -> StreamK m (Either ParseError b)
runArrayFoldMany p m = fromStream $ runArrayFoldManyD p (toStream m)
