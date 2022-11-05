{-# OPTIONS_GHC -Wno-orphans  #-}

-- |
-- Module      : Streamly.Internal.Data.Parser.Chunked
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC

module Streamly.Internal.Data.Parser.Chunked
    (
      ParserChunked (..)
    , fromParserD
    , parseBreak
    , K.fromPure
    , K.fromEffect
    , K.die

    -- We can expose all the parser combinators here, that way we can choose to
    -- use custom or CPS equivalents where it makes sense.
    )
where

#include "ArrayMacros.h"
#include "assert.hs"
#include "inline.hs"

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Parser.Chunked.Type (ParserChunked (..))
import Streamly.Internal.Data.Parser.ParserD.Type (Initial(..), Step(..))
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.SVar.Type (defState)
import Streamly.Internal.Data.Unboxed (peekWith, sizeOf, Unbox)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Parser.Chunked.Type as K
import qualified Streamly.Internal.Data.Parser.ParserD as D
    hiding (fromParserK, toParserK)
import qualified Streamly.Internal.Data.Stream.StreamK as StreamK
import qualified Streamly.Internal.Data.Stream.Type as Stream

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

-- The backracking buffer consists of arrays in the most recent first order. We
-- want to take a total of n array elements from this buffer. Note: when we
-- have to take an array partially, we must take the last part of the array.
{-# INLINE backTrack #-}
backTrack :: forall m a. Unbox a =>
       Int
    -> [Array a]
    -> StreamK.Stream m (Array a)
    -> (StreamK.Stream m (Array a), [Array a])
backTrack = go

    where

    go _ [] stream = (stream, [])
    go n xs stream | n <= 0 = (stream, xs)
    go n (x:xs) stream =
        let len = Array.length x
        in if n > len
           then go (n - len) xs (StreamK.cons x stream)
           else if n == len
           then (StreamK.cons x stream, xs)
           else let !(Array contents start end) = x
                    !start1 = end - (n * SIZE_OF(a))
                    arr1 = Array contents start1 end
                    arr2 = Array contents start start1
                 in (StreamK.cons arr1 stream, arr2:xs)

-- | A continuation to extract the result when a CPS parser is done.
{-# INLINE parserDone #-}
parserDone :: Monad m => (Int, Int) -> K.Parse b -> m (K.Step m a b)
parserDone (0,_) (K.Success n b) = return $ K.Done n b
parserDone st (K.Success _ _) =
    error $ "Bug: fromParserK: inside alternative: " ++ show st
parserDone _ (K.Failure e) = return $ K.Error e

-- | Run a 'Parser' over a stream and return rest of the Stream.
{-# INLINE_NORMAL parseBreak #-}
parseBreak
    :: (MonadThrow m, Unbox a)
    => ParserChunked a m b
    -> Stream m (Array a)
    -> m (b, Stream m (Array a))
parseBreak parser input = do
    pRes <- K.runParser parser 0 (0,0) parserDone
    case pRes of
        K.Done n b -> assert (n == 0) (return (b, input))
        K.Error e -> throwM (D.ParseError e)
        K.Partial n parserk ->
            assert (n == 0) (go [] parserk (Stream.toStreamK input))
        K.Continue n parserk ->
            assert (n == 0) (go [] parserk (Stream.toStreamK input))

    where

    -- This is a simplified yieldk
    extractYieldK backBuf parserk arr stream = do
        pRes <- parserk (Just arr)
        case pRes of
            K.Partial 0 cont1 ->
                goExtract [] cont1 stream
            K.Partial n cont1 -> do
                assertM(n <= sum (map Array.length (arr:backBuf)))
                goExtract [] cont1 (fst $ backTrack n (arr:backBuf) stream)
            K.Continue 0 cont1 ->
                go (arr:backBuf) cont1 stream
            K.Continue n cont1 -> do
                assertM(n <= sum (map Array.length (arr:backBuf)))
                let (s1, backBuf1) = backTrack n (arr:backBuf) stream
                 in go backBuf1 cont1 s1
            K.Done 0 b ->
                return (b, Stream.fromStreamK stream)
            K.Done n b -> do
                assertM(n <= sum (map Array.length (arr:backBuf)))
                let (s1, _) = backTrack n (arr:backBuf) stream
                 in return (b, Stream.fromStreamK s1)
            K.Error err -> throwM $ D.ParseError err

    goExtract backBuf parserk stream = do
        let stop = goStop backBuf parserk
            single a = extractYieldK backBuf parserk a StreamK.nil
         in StreamK.foldStream
                defState (extractYieldK backBuf parserk) single stop stream

    -- This is a simplified goExtract
    {-# INLINE goStop #-}
    goStop backBuf parserk = do
        pRes <- parserk Nothing
        case pRes of
            -- If we stop in an alternative, it will try calling the next
            -- parser, the next parser may call initial returning Partial and
            -- then immediately we have to call extract on it.
            K.Partial 0 cont1 -> do
                 goExtract [] cont1 StreamK.nil
            K.Partial n cont1 -> do
                -- error $ "Bug: parseBreak: Partial in extract, n = " ++ show n
                assertM(n <= sum (map Array.length backBuf))
                let (s1, backBuf1) = backTrack n backBuf StreamK.nil
                 in goExtract backBuf1 cont1 s1
            K.Continue 0 cont1 -> do
                -- error "parseBreak: extract, Continue 0 creates infinite loop"
                go backBuf cont1 StreamK.nil
            K.Continue n cont1 -> do
                assertM(n <= sum (map Array.length backBuf))
                let (s1, backBuf1) = backTrack n backBuf StreamK.nil
                 in goExtract backBuf1 cont1 s1
            K.Done 0 b -> return (b, Stream.nil)
            K.Done n b -> do
                assertM(n <= sum (map Array.length backBuf))
                let (s1, _) = backTrack n backBuf StreamK.nil
                 in return (b, Stream.fromStreamK s1)
            K.Error err -> throwM $ D.ParseError err

    -- SPECIALIZE this on backBuf?
    yieldk backBuf !parserk arr stream = do
        pRes <- parserk (Just arr)
        case pRes of
            K.Partial 0 cont1 ->
                 go [] cont1 stream
            K.Partial n cont1 -> do
                assertM(n <= sum (map Array.length (arr:backBuf)))
                go [] cont1 (fst $ backTrack n (arr:backBuf) stream)
            K.Continue 0 cont1 ->
                 go (arr:backBuf) cont1 stream
            K.Continue n cont1 -> do
                assertM(n <= sum (map Array.length (arr:backBuf)))
                let (s1, backBuf1) = backTrack n (arr:backBuf) stream
                 in go backBuf1 cont1 s1
            K.Done 0 b ->
                return (b, Stream.fromStreamK stream)
            K.Done n b -> do
                assertM(n <= sum (map Array.length (arr:backBuf)))
                let (s1, _) = backTrack n (arr:backBuf) stream
                 in return (b, Stream.fromStreamK s1)
            K.Error err -> throwM $ D.ParseError err

    go backBuf parserk stream = do
        let stop = goStop backBuf parserk
            single a = yieldk backBuf parserk a StreamK.nil
         in StreamK.foldStream
                defState (yieldk backBuf parserk) single stop stream

-- This is very similar to fromParserD in the Array/Unboxed/Fold module.
{-# INLINE parseChunk #-}
parseChunk
    :: forall m a s b. (MonadIO m, Unbox a)
    => (s -> a -> m (Step s b))
    -> s
    -> Array a
    -> m (Step s b)
parseChunk pstep state (Array contents start end) = do
    go SPEC start state

    where

    {-# INLINE partial #-}
    partial arrRem cur next elemSize st n fs1 = do
        let next1 = next - (n * elemSize)
        if next1 >= start && cur < end
        then go SPEC next1 fs1
        else return $ st (arrRem + n) fs1

    go !_ !cur !pst | cur >= end = return $ Continue 0 pst
    go !_ !cur !pst = do
        x <- liftIO $ peekWith contents cur
        pRes <- pstep pst x
        let elemSize = SIZE_OF(a)
            next = INDEX_NEXT(cur,a)
            arrRem = (end - next) `div` elemSize
        case pRes of
            Done n b -> do
                return $ Done (arrRem + n) b
            Partial n pst1 ->
                partial arrRem cur next elemSize Partial n pst1
            Continue n pst1 -> do
                partial arrRem cur next elemSize Continue n pst1
            Error err -> return $ Error err

{-# INLINE_NORMAL parseDToK #-}
parseDToK
    :: (MonadIO m, Unbox a)
    => (s -> a -> m (Step s b))
    -> m (Initial s b)
    -> (s -> m (Step s b))
    -> Int
    -> (Int, Int)
    -> ((Int, Int) -> K.Parse b -> m (K.Step m a r))
    -> m (K.Step m a r)
-- Non 'Alternative' case.
parseDToK pstep initial extract leftover (0, _) cont = do
    res <- initial
    case res of
        IPartial r -> return $ K.Partial leftover (parseCont (return r))
        IDone b -> cont state (K.Success 0 b)
        IError err -> cont state (K.Failure err)

    where

    -- The continuation is called with (0,0) state i.e. Alternative level
    -- is 0 and the used count is 0. Alternative level is 0 because the level
    -- passed in the argument above is 0, the "used" count is 0 because it is
    -- not useful when Alternative level is 0. We should probably use a Maybe
    -- type for the state but that might impact performance, need to measure.
    state = (0,0)

    -- We could pass a stream or array here and drive the ParserD with fusion.
    parseCont pst (Just arr) = do
        r <- pst
        pRes <- parseChunk pstep r arr
        case pRes of
            Done n b -> cont state (K.Success n b)
            Error err -> cont state (K.Failure err)
            Partial n pst1 -> return $ K.Partial n (parseCont (return pst1))
            Continue n pst1 -> return $ K.Continue n (parseCont (return pst1))

    parseCont acc Nothing = do
        pst <- acc
        r <- extract pst
        case r of
            Done n b -> cont state (K.Success n b)
            Error err -> cont state (K.Failure err)
            Partial _ _ -> error "Bug: parseDToK Partial unreachable"
            Continue n pst1 -> return $ K.Continue n (parseCont (return pst1))

-- 'Alternative' case. Used count needs to be maintained when inside an
-- Alternative.
parseDToK pstep initial extract leftover (level, count) cont = do
    res <- initial
    case res of
        IPartial r -> return $ K.Partial leftover (parseCont count (return r))
        IDone b -> cont (level,count) (K.Success 0 b)
        IError err -> cont (level,count) (K.Failure err)

    where

    parseCont !cnt pst (Just arr) = do
        let !cnt1 = cnt + Array.length arr
        r <- pst
        pRes <- parseChunk pstep r arr
        case pRes of
            Done n b -> do
                assertM(n <= cnt1)
                cont (level, cnt1 - n) (K.Success n b)
            Error err ->
                cont (level, cnt1) (K.Failure err)
            Partial n pst1 -> do
                assertM(n <= cnt1)
                return $ K.Partial n (parseCont (cnt1 - n) (return pst1))
            Continue n pst1 -> do
                assertM(n <= cnt1)
                return $ K.Continue n (parseCont (cnt1 - n) (return pst1))
    parseCont cnt acc Nothing = do
        pst <- acc
        r <- extract pst
        let s = (level, cnt)
        case r of
            Done n b -> do
                assertM(n <= cnt)
                cont s (K.Success n b)
            Partial _ _ -> error "Bug: parseDToK Partial unreachable"
            Error e ->
                cont s (K.Failure e)
            Continue n pst1 -> do
                assertM(n <= cnt)
                return $ K.Continue n (parseCont (cnt - n) (return pst1))

-- | Convert a raw byte 'Parser' to a chunked parser.
--
-- /Pre-release/
--
{-# INLINE_LATE fromParserD #-}
fromParserD :: (MonadIO m, Unbox a) => D.Parser a m b -> ParserChunked a m b
fromParserD (D.Parser step initial extract) =
    K.MkParser $ parseDToK step initial extract
