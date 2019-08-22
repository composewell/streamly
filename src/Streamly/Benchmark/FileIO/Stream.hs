-- |
-- Module      : Streamly.Benchmark.FileIO.Stream
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}

module Streamly.Benchmark.FileIO.Stream
    (
    -- * FileIO
      last
    , countBytes
    , countLines
    , countLinesU
    , sumBytes
    , cat
    , catStreamWrite
    , copy
    , linesUnlinesCopy
    , wordsUnwordsCopy
    , copyCodecChar8
    , copyCodecUtf8
    , chunksOf
    , splitOn
    , splitOnSuffix
    , wordsBy
    , splitOnSeq
    , splitOnSuffixSeq
    )
where

import Data.Char (ord, chr)
import Data.Word (Word8)
import System.IO (Handle)
import Prelude hiding (last, length)

import Streamly.Streams.StreamD.Type (Step(..), GroupState)
import Streamly.Benchmark.Inspection (hinspect)
import qualified Streamly.Memory.Array.Types as AT

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.FileSystem.Handle.Internal as FH
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude.Internal as S
import qualified Streamly.Fold as FL
import qualified Streamly.Data.String as SS
import qualified Streamly.Internal as Internal
import qualified Streamly.Streams.StreamD as D

import Test.Inspection

-- | Get the last byte from a file bytestream.
{-# INLINE last #-}
last :: Handle -> IO (Maybe Word8)
last = S.last . FH.read

hinspect $ hasNoTypeClasses 'last
hinspect $ 'last `hasNoType` ''Step
hinspect $ 'last `hasNoType` ''AT.FlattenState

-- assert that flattenArrays constructors are not present
-- | Count the number of bytes in a file.
{-# INLINE countBytes #-}
countBytes :: Handle -> IO Int
countBytes = S.length . FH.read

hinspect $ hasNoTypeClasses 'countBytes
hinspect $ 'countBytes `hasNoType` ''Step
hinspect $ 'countBytes `hasNoType` ''AT.FlattenState

-- | Count the number of lines in a file.
{-# INLINE countLines #-}
countLines :: Handle -> IO Int
countLines =
    S.length
        . SS.foldLines FL.drain
        . SS.decodeChar8
        . FH.read

hinspect $ hasNoTypeClasses 'countLines
hinspect $ 'countLines `hasNoType` ''Step
hinspect $ 'countLines `hasNoType` ''AT.FlattenState

-- | Count the number of lines in a file.
{-# INLINE countLinesU #-}
countLinesU :: Handle -> IO Int
countLinesU inh =
    S.length
        $ SS.foldLines FL.drain
        $ SS.decodeChar8
        $ Internal.concatMapU Internal.readU (FH.readArrays inh)

hinspect $ hasNoTypeClasses 'countLinesU
hinspect $ 'countLinesU `hasNoType` ''Step
hinspect $ 'countLinesU `hasNoType` ''Either

-- | Sum the bytes in a file.
{-# INLINE sumBytes #-}
sumBytes :: Handle -> IO Word8
sumBytes = S.sum . FH.read

hinspect $ hasNoTypeClasses 'sumBytes
hinspect $ 'sumBytes `hasNoType` ''Step
hinspect $ 'sumBytes `hasNoType` ''AT.FlattenState

-- | Send the file contents to /dev/null
{-# INLINE cat #-}
cat :: Handle -> Handle -> IO ()
cat devNull inh = S.runFold (FH.write devNull) $ FH.read inh

hinspect $ hasNoTypeClasses 'cat
hinspect $ 'cat `hasNoType` ''Step
hinspect $ 'cat `hasNoType` ''AT.FlattenState

-- | Send the file contents to /dev/null
{-# INLINE catStreamWrite #-}
catStreamWrite :: Handle -> Handle -> IO ()
catStreamWrite devNull inh = Internal.writeS devNull $ FH.read inh

hinspect $ hasNoTypeClasses 'catStreamWrite
hinspect $ 'catStreamWrite `hasNoType` ''Step
hinspect $ 'catStreamWrite `hasNoType` ''AT.FlattenState

-- | Copy file
{-# INLINE copy #-}
copy :: Handle -> Handle -> IO ()
copy inh outh = S.runFold (FH.write outh) (FH.read inh)

hinspect $ hasNoTypeClasses 'copy
hinspect $ 'copy `hasNoType` ''Step
hinspect $ 'copy `hasNoType` ''AT.FlattenState

-- | Copy file
{-# INLINE copyCodecChar8 #-}
copyCodecChar8 :: Handle -> Handle -> IO ()
copyCodecChar8 inh outh =
   S.runFold (FH.write outh)
     $ SS.encodeChar8
     $ SS.decodeChar8
     $ FH.read inh

hinspect $ hasNoTypeClasses 'copyCodecChar8
hinspect $ 'copyCodecChar8 `hasNoType` ''Step
hinspect $ 'copyCodecChar8 `hasNoType` ''AT.FlattenState

-- | Copy file
{-# INLINE copyCodecUtf8 #-}
copyCodecUtf8 :: Handle -> Handle -> IO ()
copyCodecUtf8 inh outh =
   S.runFold (FH.write outh)
     $ SS.encodeUtf8
     $ SS.decodeUtf8
     $ FH.read inh

hinspect $ hasNoTypeClasses 'copyCodecUtf8
-- hinspect $ 'copyCodecUtf8 `hasNoType` ''Step
-- hinspect $ 'copyCodecUtf8 `hasNoType` ''AT.FlattenState

-- | Slice in chunks of size n and get the count of chunks.
{-# INLINE chunksOf #-}
chunksOf :: Int -> Handle -> IO Int
chunksOf n inh =
    S.length $ S.chunksOf n (AT.writeNUnsafe n) (FH.read inh)

hinspect $ hasNoTypeClasses 'chunksOf
hinspect $ 'chunksOf `hasNoType` ''Step
-- hinspect $ 'chunksOf `hasNoType` ''AT.FlattenState
-- hinspect $ 'chunksOf `hasNoType` ''GroupState

-- This is to make sure that the concatMap in FH.read, groupsOf and foldlM'
-- together can fuse.
--
-- | Slice in chunks of size n and get the count of chunks.
{-# INLINE chunksOfD #-}
chunksOfD :: Int -> Handle -> IO Int
chunksOfD n inh =
    D.foldlM' (\i _ -> return $ i + 1) 0
        $ D.groupsOf n (AT.writeNUnsafe n)
        $ D.fromStreamK (FH.read inh)

hinspect $ hasNoTypeClasses 'chunksOf
hinspect $ 'chunksOf `hasNoType` ''Step
-- hinspect $ 'chunksOfD `hasNoType` ''GroupState
-- hinspect $ 'chunksOfD `hasNoType` ''AT.FlattenState

-- | Lines and unlines
{-# INLINE linesUnlinesCopy #-}
linesUnlinesCopy :: Handle -> Handle -> IO ()
linesUnlinesCopy inh outh =
    S.runFold (FH.write outh)
      $ SS.encodeChar8
      $ SS.unlines
      $ SS.lines
      $ SS.decodeChar8
      $ FH.read inh

-- hinspect $ hasNoTypeClasses 'linesUnlinesCopy
-- hinspect $ 'linesUnlinesCopy `hasNoType` ''Step
-- hinspect $ 'linesUnlinesCopy `hasNoType` ''AT.FlattenState

-- | Word, unwords and copy
{-# INLINE wordsUnwordsCopy #-}
wordsUnwordsCopy :: Handle -> Handle -> IO ()
wordsUnwordsCopy inh outh =
    S.runFold (FH.write outh)
      $ SS.encodeChar8
      $ SS.unwords
      $ SS.words
      $ SS.decodeChar8
      $ FH.read inh

-- hinspect $ hasNoTypeClasses 'wordsUnwordsCopy
-- hinspect $ 'wordsUnwordsCopy `hasNoType` ''Step
-- hinspect $ 'wordsUnwordsCopy `hasNoType` ''AT.FlattenState

lf :: Word8
lf = fromIntegral (ord '\n')

toarr :: String -> A.Array Word8
toarr = A.fromList . map (fromIntegral . ord)

foreign import ccall unsafe "u_iswspace"
  iswspace :: Int -> Int

-- Code copied from base/Data.Char to INLINE it
{-# INLINE isSpace #-}
isSpace                 :: Char -> Bool
-- isSpace includes non-breaking space
-- The magic 0x377 isn't really that magical. As of 2014, all the codepoints
-- at or below 0x377 have been assigned, so we shouldn't have to worry about
-- any new spaces appearing below there. It would probably be best to
-- use branchless ||, but currently the eqLit transformation will undo that,
-- so we'll do it like this until there's a way around that.
isSpace c
  | uc <= 0x377 = uc == 32 || uc - 0x9 <= 4 || uc == 0xa0
  | otherwise = iswspace (ord c) /= 0
  where
    uc = fromIntegral (ord c) :: Word

isSp :: Word8 -> Bool
isSp = isSpace . chr . fromIntegral

-- | Split on line feed.
{-# INLINE splitOn #-}
splitOn :: Handle -> IO Int
splitOn inh =
    (S.length $ S.splitOn (== lf) FL.drain
        $ FH.read inh) -- >>= print

hinspect $ hasNoTypeClasses 'splitOn
hinspect $ 'splitOn `hasNoType` ''Step
hinspect $ 'splitOn `hasNoType` ''AT.FlattenState

-- | Split suffix on line feed.
{-# INLINE splitOnSuffix #-}
splitOnSuffix :: Handle -> IO Int
splitOnSuffix inh =
    (S.length $ S.splitOnSuffix (== lf) FL.drain
        $ FH.read inh) -- >>= print

hinspect $ hasNoTypeClasses 'splitOnSuffix
hinspect $ 'splitOnSuffix `hasNoType` ''Step
hinspect $ 'splitOnSuffix `hasNoType` ''AT.FlattenState

-- | Words by space
{-# INLINE wordsBy #-}
wordsBy :: Handle -> IO Int
wordsBy inh =
    (S.length $ S.wordsBy isSp FL.drain
        $ FH.read inh) -- >>= print

hinspect $ hasNoTypeClasses 'wordsBy
hinspect $ 'wordsBy `hasNoType` ''Step
hinspect $ 'wordsBy `hasNoType` ''AT.FlattenState

-- | Split on a character sequence.
{-# INLINE splitOnSeq #-}
splitOnSeq :: String -> Handle -> IO Int
splitOnSeq str inh =
    (S.length $ Internal.splitOnSeq (toarr str) FL.drain
        $ FH.read inh) -- >>= print

hinspect $ hasNoTypeClasses 'splitOnSeq
-- hinspect $ 'splitOnSeq `hasNoType` ''Step
-- hinspect $ 'splitOnSeq `hasNoType` ''AT.FlattenState

-- | Split on suffix sequence.
{-# INLINE splitOnSuffixSeq #-}
splitOnSuffixSeq :: String -> Handle -> IO Int
splitOnSuffixSeq str inh =
    (S.length $ Internal.splitOnSuffixSeq (toarr str) FL.drain
        $ FH.read inh) -- >>= print

hinspect $ hasNoTypeClasses 'splitOnSuffixSeq
-- hinspect $ 'splitOnSuffixSeq `hasNoType` ''Step
-- hinspect $ 'splitOnSuffixSeq `hasNoType` ''AT.FlattenState
