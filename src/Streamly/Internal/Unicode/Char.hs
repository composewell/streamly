-- |
-- Module      : Streamly.Internal.Unicode.Char
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

-- XXX We are using head/tail at one place
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif
-- XXX This module should have the reader/writer unfold/refold and read/write
-- stream/fold routines to convert a char to/from stream.
--
module Streamly.Internal.Unicode.Char
    (
    -- * Predicates
      isAsciiAlpha -- XXX Remove or move to unicode-data

    -- XXX move to Unicode.Char.Case?
    -- * Unicode aware operations
    {-
      toCaseFold
    , toLower
    , toUpper
    , toTitle
    -}

    -- XXX Move to Unicode.Stream.Normalize or Unicode.Normalize?
    -- * Unicode normalization
    , NormalizationMode(..)
    , normalize
    )
where

#include "inline.hs"

import Data.Char (isAsciiUpper, isAsciiLower, chr, ord)
import Unicode.Char (DecomposeMode(..))
import Streamly.Internal.Data.Stream (Stream(..), Step (..))

import qualified Unicode.Char as Char

-------------------------------------------------------------------------------
-- Unicode aware operations on strings
-------------------------------------------------------------------------------

-- | Select alphabetic characters in the ascii character set.
--
-- /Pre-release/
--
{-# INLINE isAsciiAlpha #-}
isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiUpper c || isAsciiLower c

-------------------------------------------------------------------------------
-- Unicode aware operations on strings
-------------------------------------------------------------------------------

{-
-- |
-- /undefined/
toCaseFold :: IsStream t => Char -> t m Char
toCaseFold = undefined

-- |
-- /undefined/
toLower :: IsStream t => Char -> t m Char
toLower = undefined

-- |
-- /undefined/
toUpper :: IsStream t => Char -> t m Char
toUpper = undefined

-- |
-- /undefined/
toTitle :: IsStream t => Char -> t m Char
toTitle = undefined
-}

-------------------------------------------------------------------------------
-- Unicode normalization
-------------------------------------------------------------------------------

data NormalizationMode
    = NFD    -- ^ Canonical decomposition.
    | NFKD   -- ^ Compatibility decomposition.
    | NFC    -- ^ Canonical decomposition followed by canonical composition.
    | NFKC   -- ^ Compatibility decomposition followed by canonical composition.
      deriving (Eq, Show, Enum)

-------------------------------------------------------------------------------
-- Normalization combinators
-------------------------------------------------------------------------------

type ReBuf = [Char]

{-# INLINE insertIntoReBuf #-}
insertIntoReBuf :: Char -> ReBuf -> ReBuf
insertIntoReBuf c [] = [c]
insertIntoReBuf c xxs@(x:xs)
    | Char.combiningClass c < Char.combiningClass x = c : xxs
    | otherwise = x : insertIntoReBuf c xs

-- {-# ANN type DecomposeState Fuse #-}
data DecomposeState st
    = YieldCharList [Char] (DecomposeState st)
    | ReadInputChar ReBuf st
    | IsHangul Char st
    | IsDecomposable [Char] ReBuf st
    | DecomposeStop

{-# INLINE_NORMAL decomposeD #-}
decomposeD ::
       Monad m => Bool -> DecomposeMode -> Stream m Char -> Stream m Char
decomposeD decomposeHangul mode (Stream step state) =
    Stream sstep (ReadInputChar [] state)

    where

    {-# INLINE_LATE sstep #-}
    -- XXX Does this cause any problem?
    sstep _ (YieldCharList [] ns) = return $ Skip ns
    sstep _ (YieldCharList (ch:chs) ns) =
        return $ Yield ch (YieldCharList chs ns)
    sstep gst (ReadInputChar rebuf st) = do
        res <- step gst st
        return
          $ Skip
          $ case res of
                Yield ch st1
                    | Char.isHangul ch ->
                        if decomposeHangul
                        then YieldCharList rebuf (IsHangul ch st1)
                        else YieldCharList
                                 (rebuf ++ [ch])
                                 (ReadInputChar [] st1)
                    | Char.isDecomposable mode ch ->
                        IsDecomposable (Char.decompose mode ch) rebuf st1
                    | otherwise ->
                        if Char.isCombining ch
                        then ReadInputChar (insertIntoReBuf ch rebuf) st1
                        else YieldCharList
                                 (rebuf ++ [ch])
                                 (ReadInputChar [] st1)
                Skip st1 -> ReadInputChar rebuf st1
                Stop -> YieldCharList rebuf DecomposeStop
    sstep _ (IsHangul ch st) =
        return
          $ Skip
          $ let (l, v, t) = Char.decomposeHangul ch
             in if t == chr Char.jamoTFirst
                then YieldCharList [l, v] (ReadInputChar [] st)
                else YieldCharList [l, v, t] (ReadInputChar [] st)
    sstep _ (IsDecomposable [] rebuf st) =
        return $ Skip $ ReadInputChar rebuf st
    sstep _ (IsDecomposable (ch:chs) rebuf st)
        | Char.isDecomposable mode ch =
            return
              $ Skip $ IsDecomposable (Char.decompose mode ch ++ chs) rebuf st
        | otherwise =
            return
              $ Skip
              $ if Char.isCombining ch
                then IsDecomposable chs (insertIntoReBuf ch rebuf) st
                else YieldCharList (rebuf ++ [ch]) (IsDecomposable chs [] st)
    sstep _ DecomposeStop = return Stop

-- Hold an L to wait for V, hold an LV to wait for T.
data JamoBuf
    = Jamo !Char -- Jamo L, V or T
    | Hangul !Char -- Hangul Syllable LV or LVT
    | HangulLV !Char

{-# INLINE fromJamoBuf #-}
fromJamoBuf :: JamoBuf -> Char
fromJamoBuf (Jamo ch) = ch
fromJamoBuf (Hangul ch) = ch
fromJamoBuf (HangulLV ch) = ch

-- {-# ANN type ComposeState Fuse #-}
data ComposeState st
    = YieldChar Char (ComposeState st)
    | YieldList [Char] (ComposeState st)
    | ComposeNone st
    | ComposeReg Int [Char] st
    | ComposeJamo JamoBuf st
    | ComposeStop

-- Assumes every character except hangul characters are fully decomposed and the
-- combining characters are reordered. Hangul characters may or may not be
-- decomposed.
{-# INLINE_EARLY partialComposeD #-}
partialComposeD :: Monad m => Stream m Char -> Stream m Char
partialComposeD (Stream step state) = Stream step' (ComposeNone state)

    where

    {-# INLINE_NORMAL step' #-}
    step' _ ComposeStop = return Stop
    step' _ (YieldChar ch ns) = return $ Yield ch ns
    step' _ (YieldList [] ns) = return $ Skip ns
    step' _ (YieldList (x:xs) ns) = return $ Yield x $ YieldList xs ns
    step' gst (ComposeNone st) = do
        r <- step gst st
        return
          $ case r of
                Yield x st1 -> Skip $ composeNone x st1
                Skip st1 -> Skip $ ComposeNone st1
                Stop -> Stop
    step' gst (ComposeJamo jbuf st) = do
        r <- step gst st
        return
          $ case r of
                Yield x st1 -> Skip $ composeJamo jbuf x st1
                Skip st1 -> Skip $ ComposeJamo jbuf st1
                Stop -> Skip $ YieldChar (fromJamoBuf jbuf) ComposeStop
    step' gst (ComposeReg i rbuf st) = do
        r <- step gst st
        return
          $ case r of
                Yield x st1 -> Skip $ composeReg i rbuf x st1
                Skip st1 -> Skip $ ComposeReg i rbuf st1
                Stop -> Skip $ YieldList rbuf ComposeStop

    {-# INLINE initHangul #-}
    initHangul c = ComposeJamo (Hangul c)

    {-# INLINE initJamo #-}
    initJamo c = ComposeJamo (Jamo c)

    {-# INLINE initReg #-}
    initReg !c = ComposeReg 0 [c]

    {-# INLINE composeNone #-}
    composeNone ch st
        | Char.isHangul ch = initHangul ch st
        | Char.isJamo ch = initJamo ch st
        | otherwise = initReg ch st

    {-# INLINE composeCharHangul #-}
    composeCharHangul jbuf ch st =
        YieldChar (fromJamoBuf jbuf) $ ComposeJamo (Hangul ch) st

    {-# INLINE composeCharJamo #-}
    composeCharJamo jbuf ch st
        | ich <= Char.jamoLLast =
            YieldChar (fromJamoBuf jbuf) $ ComposeJamo (Jamo ch) st
        | ich < Char.jamoVFirst = flushAndWrite jbuf ch st
        | ich <= Char.jamoVLast =
            case jbuf of
                Jamo c ->
                    case Char.jamoLIndex c of
                        Just li ->
                            let vi = ich - Char.jamoVFirst
                                lvi = li * Char.jamoNCount + vi * Char.jamoTCount
                                lv = chr (Char.hangulFirst + lvi)
                             in ComposeJamo (HangulLV lv) st
                        Nothing -> writeTwo c ch st
                Hangul c -> writeTwo c ch st
                HangulLV c -> writeTwo c ch st
        | ich <= Char.jamoTFirst = flushAndWrite jbuf ch st
        | otherwise = do
            let ti = ich - Char.jamoTFirst
            case jbuf of
                Jamo c -> writeTwo c ch st
                Hangul c
                    | Char.isHangulLV c -> writeLVT c ti st
                    | otherwise -> writeTwo c ch st
                HangulLV c -> writeLVT c ti st

        where

        flushAndWrite jb c s = YieldList [fromJamoBuf jb, c] $ ComposeNone s

        writeLVT lv ti s =
            let lvt = chr $ ord lv + ti
             in YieldChar lvt $ ComposeNone s

        writeTwo c1 c2 s = YieldList [c1, c2] $ ComposeNone s

        ich = ord ch

    {-# INLINE composeJamo #-}
    composeJamo jbuf ch st
        | Char.isJamo ch = composeCharJamo jbuf ch st
        | Char.isHangul ch = composeCharHangul jbuf ch st
        | otherwise = YieldChar (fromJamoBuf jbuf) (ComposeReg 0 [ch] st)

    -- i ~ Char.combiningClass (last rbuf)
    {-# INLINE composeCharCombining #-}
    composeCharCombining i rbuf ch st =
        if cch > i
        then case Char.compose str ch of
                 Nothing -> ComposeReg cch (rbuf ++ [ch]) st
                 Just x -> ComposeReg i (x : tail rbuf) st
        else ComposeReg i (rbuf ++ [ch]) st

        where

        str = head rbuf
        cch = Char.combiningClass ch

    {-# INLINE composeReg #-}
    composeReg i rbuf !ch !st
        | Char.isHangul ch = YieldList rbuf $ initHangul ch st
        | Char.isJamo ch = YieldList rbuf $ initJamo ch st
        | Char.isCombining ch = composeCharCombining i rbuf ch st
        | [s] <- rbuf
        , Char.isCombiningStarter ch
        , Just x <- Char.composeStarters s ch = ComposeReg 0 [x] st
        | otherwise = YieldList rbuf $ ComposeReg 0 [ch] st

normalizeD :: Monad m => NormalizationMode -> Stream m Char -> Stream m Char
normalizeD NFD = decomposeD True Canonical
normalizeD NFKD = decomposeD True Kompat
normalizeD NFC = partialComposeD . decomposeD False Canonical
normalizeD NFKC = partialComposeD . decomposeD False Kompat

normalize ::
       Monad m
    => NormalizationMode
    -> Stream m Char
    -> Stream m Char
normalize = normalizeD
