-- |
-- Module      : Streamly.Internal.Data.Parser.Chunked
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Differences from an item parser:
--
-- * Driver drives from a chunk stream
-- * Backtrack buffer is a chunk stream instead of a list
-- * Monad and Alternative can handle chunks
-- * Parser takes stream position and returns new stream position
--
-- We can possibly use the same type for item and chunk parsers, but with
-- different implementations for instances. So it could just be a newtype
-- wrapper rather than a different type. And then we can even have the parser
-- type and chunkedParseBreak in the Parser module itself.

module Streamly.Internal.Data.Parser.Chunked
    (
      ChunkParser (..) -- XXX rename to ChunkParser
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

import Control.Monad.IO.Class (MonadIO(..))
import Data.Proxy (Proxy(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Parser.Chunked.Type (ChunkParser (..))
import Streamly.Internal.Data.Parser.ParserD.Type (Initial(..), Step(..))
import Streamly.Internal.Data.Stream.StreamK.Type (StreamK)
import Streamly.Internal.Data.SVar.Type (defState)
import Streamly.Internal.Data.Unboxed (peekWith, sizeOf, Unbox)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Parser.Chunked.Type as K
import qualified Streamly.Internal.Data.Parser.ParserD as D
import qualified Streamly.Internal.Data.Stream.StreamK as StreamK
import Streamly.Internal.Data.Parser.ParserD (ParseError(..))

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
parserDone :: Monad m =>
    K.ParseResult b -> Int -> Maybe (Array a) -> m (K.Step a m b)
parserDone (K.Success n b) _ _ = return $ K.Done n b
parserDone (K.Failure n e) _ _ = return $ K.Error n e

-- | Run a 'Parser' over a stream and return rest of the Stream.
{-# INLINE_NORMAL parseBreak #-}
parseBreak
    :: (Monad m, Unbox a)
    => ChunkParser a m b
    -> StreamK m (Array a)
    -> m (Either ParseError b, StreamK m (Array a))
parseBreak parser input = do
    let parserk = \arr -> K.runParser parser 0 0 arr parserDone
     in go [] parserk input

    where

    {-# INLINE goStop #-}
    goStop backBuf parserk = do
        pRes <- parserk Nothing
        case pRes of
            -- If we stop in an alternative, it will try calling the next
            -- parser, the next parser may call initial returning Partial and
            -- then immediately we have to call extract on it.
            K.Partial 0 cont1 ->
                 go [] cont1 StreamK.nil
            K.Partial n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (map Array.length backBuf))
                let (s1, backBuf1) = backTrack n1 backBuf StreamK.nil
                 in go backBuf1 cont1 s1
            K.Continue 0 cont1 ->
                go backBuf cont1 StreamK.nil
            K.Continue n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (map Array.length backBuf))
                let (s1, backBuf1) = backTrack n1 backBuf StreamK.nil
                 in go backBuf1 cont1 s1
            K.Done 0 b ->
                return (Right b, StreamK.nil)
            K.Done n b -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (map Array.length backBuf))
                let (s1, _) = backTrack n1 backBuf StreamK.nil
                 in return (Right b, s1)
            K.Error _ err -> return (Left (ParseError err), StreamK.nil)

    seekErr n len =
        error $ "parseBreak: Partial: forward seek not implemented n = "
            ++ show n ++ " len = " ++ show len

    yieldk backBuf parserk arr stream = do
        pRes <- parserk (Just arr)
        let len = Array.length arr
        case pRes of
            K.Partial n cont1 ->
                case compare n len of
                    EQ -> go [] cont1 stream
                    LT -> do
                        if n >= 0
                        then yieldk [] cont1 arr stream
                        else do
                            let n1 = negate n
                                bufLen = sum (map Array.length backBuf)
                                s = StreamK.cons arr stream
                            assertM(n1 >= 0 && n1 <= bufLen)
                            let (s1, _) = backTrack n1 backBuf s
                            go [] cont1 s1
                    GT -> seekErr n len
            K.Continue n cont1 ->
                case compare n len of
                    EQ -> go (arr:backBuf) cont1 stream
                    LT -> do
                        if n >= 0
                        then yieldk backBuf cont1 arr stream
                        else do
                            let n1 = negate n
                                bufLen = sum (map Array.length backBuf)
                                s = StreamK.cons arr stream
                            assertM(n1 >= 0 && n1 <= bufLen)
                            let (s1, backBuf1) = backTrack n1 backBuf s
                            go backBuf1 cont1 s1
                    GT -> seekErr n len
            K.Done n b -> do
                let n1 = len - n
                assertM(n1 <= sum (map Array.length (arr:backBuf)))
                let (s1, _) = backTrack n1 (arr:backBuf) stream
                 in return (Right b, s1)
            K.Error _ err -> return (Left (ParseError err), StreamK.nil)

    go backBuf parserk stream = do
        let stop = goStop backBuf parserk
            single a = yieldk backBuf parserk a StreamK.nil
         in StreamK.foldStream
                defState (yieldk backBuf parserk) single stop stream

data ChunkResult s b =
      ChunkDone !Int !b
    | ChunkPartial !Int !s
    | ChunkContinue !Int !s
    | ChunkError !Int String

-- This is very similar to fromParserD in the Array/Unboxed/Fold module.
{-# INLINE parseChunk #-}
parseChunk
    :: forall m a s b. (MonadIO m, Unbox a)
    => (s -> a -> m (Step s b))
    -> s
    -> Array a
    -> Int
    -> m (ChunkResult s b)
parseChunk pstep state (Array contents start end) offset = do
     if offset >= 0
     then go SPEC (start + offset * SIZE_OF(a)) state
     else return $ ChunkContinue offset state

    where

    {-# INLINE onBack #-}
    onBack offset1 elemSize constr pst = do
        let pos = offset1 - start
         in if pos >= 0
            then go SPEC offset1 pst
            else return $ constr (pos `div` elemSize) pst

    -- Note: div may be expensive but the alternative is to maintain an element
    -- offset in addition to a byte offset or just the element offset and use
    -- multiplication to get the byte offset every time, both these options
    -- turned out to be more expensive than using div.
    go !_ !cur !pst | cur >= end =
        return $ ChunkContinue ((end - start) `div` SIZE_OF(a))  pst
    go !_ !cur !pst = do
        x <- liftIO $ peekWith contents cur
        pRes <- pstep pst x
        let elemSize = SIZE_OF(a)
            next = INDEX_NEXT(cur,a)
            back n = next - n * elemSize
            curOff = (cur - start) `div` elemSize
            nextOff = (next - start) `div` elemSize
        case pRes of
            Done 0 b ->
                return $ ChunkDone nextOff b
            Done 1 b ->
                return $ ChunkDone curOff b
            Done n b ->
                return $ ChunkDone ((back n - start) `div` elemSize) b
            Partial 0 pst1 ->
                go SPEC next pst1
            Partial 1 pst1 ->
                go SPEC cur pst1
            Partial n pst1 ->
                onBack (back n) elemSize ChunkPartial pst1
            Continue 0 pst1 ->
                go SPEC next pst1
            Continue 1 pst1 ->
                go SPEC cur pst1
            Continue n pst1 ->
                onBack (back n) elemSize ChunkContinue pst1
            Error err ->
                return $ ChunkError curOff err

{-# INLINE parseDToK #-}
parseDToK
    :: forall m a s b r. (MonadIO m, Unbox a)
    => (s -> a -> m (Step s b))
    -> m (Initial s b)
    -> (s -> m (Step s b))
    -> Int
    -> Int
    -> Maybe (Array a)
    -> (K.ParseResult b -> Int -> Maybe (Array a) -> m (K.Step a m r))
    -> m (K.Step a m r)
parseDToK pstep initial extract offset usedCount input cont = do
    res <- initial
    case res of
        IPartial pst -> do
            case input of
                Just arr -> parseContJust usedCount offset pst arr
                Nothing -> parseContNothing usedCount pst
        IDone b -> cont (K.Success offset b) usedCount input
        IError err -> cont (K.Failure offset err) usedCount input

    where

    -- XXX We can maintain an absolute position instead of relative that will
    -- help in reporting of error location in the stream.
    {-# NOINLINE parseContJust #-}
    parseContJust count off pst arr = do
        pRes <- parseChunk pstep pst arr off
        -- The "n" here is stream position index wrt the array start, and not
        -- the backtrack count as returned by byte stream parsers.
        case pRes of
            ChunkDone n b ->
                assert (n <= Array.length arr)
                    (cont (K.Success n b) (count + n - off) (Just arr))
            ChunkPartial n pst1 ->
                assert (n < 0 || n >= Array.length arr)
                    (return $ K.Partial n (parseCont (count + n - off) pst1))
            ChunkContinue n pst1 ->
                assert (n < 0 || n >= Array.length arr)
                    (return $ K.Continue n (parseCont (count + n - off) pst1))
            ChunkError n err ->
                cont (K.Failure n err) (count + n - off) (Just arr)

    {-# NOINLINE parseContNothing #-}
    parseContNothing count pst = do
        r <- extract pst
        case r of
            -- IMPORTANT: the n here is from the byte stream parser, that means
            -- it is the backtrack element count and not the stream position
            -- index into the current input array.
            Done n b ->
                assert (n >= 0)
                    (cont (K.Success (- n) b) (count - n) Nothing)
            Continue n pst1 ->
                assert (n >= 0)
                    (return $ K.Continue (- n) (parseCont (count - n) pst1))
            Error err ->
                -- XXX It is called only when there is no input arr. So using 0
                -- as the position is correct?
                cont (K.Failure 0 err) count Nothing
            Partial _ _ -> error "Bug: parseDToK Partial unreachable"

    -- XXX Maybe we can use two separate continuations instead of using
    -- Just/Nothing cases here. That may help in avoiding the parseContJust
    -- function call.
    {-# INLINE parseCont #-}
    parseCont cnt pst (Just arr) = parseContJust cnt 0 pst arr
    parseCont cnt pst Nothing = parseContNothing cnt pst

-- | Convert a raw byte 'Parser' to a chunked parser.
--
-- /Pre-release/
--
{-# INLINE_LATE fromParserD #-}
fromParserD :: (MonadIO m, Unbox a) => D.Parser a m b -> ChunkParser a m b
fromParserD (D.Parser step initial extract) =
    K.MkParser $ parseDToK step initial extract
