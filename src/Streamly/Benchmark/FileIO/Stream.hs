-- |
-- Module      : Streamly.Benchmark.FileIO.Stream
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Streamly.Benchmark.FileIO.Stream
    (
    -- * FileIO
      last
    , countBytes
    , countLines
    , countLinesU
    , countWords
    , sumBytes
    , cat
    , catStreamWrite
    , copy
    , linesUnlinesCopy
    , wordsUnwordsCopyWord8
    , wordsUnwordsCopy
    , readWord8
    , decodeChar8
    , copyCodecChar8
    , copyCodecUtf8
    , decodeUtf8Lenient
    , copyCodecUtf8Lenient
    , chunksOf
    , chunksOfD
    , splitOn
    , splitOnSuffix
    , wordsBy
    , splitOnSeq
    , splitOnSeqUtf8
    , splitOnSuffixSeq
    )
where

import Data.Char (ord, chr)
import Data.Word (Word8)
import System.IO (Handle)
import Prelude hiding (last, length)

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Memory.Array as A
import qualified Streamly.Internal.Memory.Array.Types as AT
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.String as SS
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Streams.StreamD as D

#ifdef INSPECTION
import Foreign.Storable (Storable)
import Streamly.Streams.StreamD.Type (Step(..), GroupState)
import Test.Inspection
#endif

-- | Get the last byte from a file bytestream.
{-# INLINE last #-}
last :: Handle -> IO (Maybe Word8)
last = S.last . FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'last
inspect $ 'last `hasNoType` ''Step
inspect $ 'last `hasNoType` ''AT.FlattenState
inspect $ 'last `hasNoType` ''D.ConcatMapUState
#endif

-- assert that flattenArrays constructors are not present
-- | Count the number of bytes in a file.
{-# INLINE countBytes #-}
countBytes :: Handle -> IO Int
countBytes = S.length . FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countBytes
inspect $ 'countBytes `hasNoType` ''Step
inspect $ 'countBytes `hasNoType` ''AT.FlattenState
inspect $ 'countBytes `hasNoType` ''D.ConcatMapUState
#endif

-- | Count the number of lines in a file.
{-# INLINE countLines #-}
countLines :: Handle -> IO Int
countLines =
    S.length
        . SS.foldLines FL.drain
        . SS.decodeChar8
        . FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countLines
inspect $ 'countLines `hasNoType` ''Step
inspect $ 'countLines `hasNoType` ''AT.FlattenState
inspect $ 'countLines `hasNoType` ''D.ConcatMapUState
#endif

-- | Count the number of words in a file.
{-# INLINE countWords #-}
countWords :: Handle -> IO Int
countWords =
    S.length
        . SS.foldWords FL.drain
        . SS.decodeChar8
        . FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countWords
-- inspect $ 'countWords `hasNoType` ''Step
-- inspect $ 'countWords `hasNoType` ''D.ConcatMapUState
#endif

-- | Count the number of lines in a file.
{-# INLINE countLinesU #-}
countLinesU :: Handle -> IO Int
countLinesU inh =
    S.length
        $ SS.foldLines FL.drain
        $ SS.decodeChar8
        $ S.concatUnfold A.read (FH.readArrays inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countLinesU
inspect $ 'countLinesU `hasNoType` ''Step
inspect $ 'countLinesU `hasNoType` ''D.ConcatMapUState
#endif

-- | Sum the bytes in a file.
{-# INLINE sumBytes #-}
sumBytes :: Handle -> IO Word8
sumBytes = S.sum . FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sumBytes
inspect $ 'sumBytes `hasNoType` ''Step
inspect $ 'sumBytes `hasNoType` ''AT.FlattenState
inspect $ 'sumBytes `hasNoType` ''D.ConcatMapUState
#endif

-- | Send the file contents to /dev/null
{-# INLINE cat #-}
cat :: Handle -> Handle -> IO ()
cat devNull inh = S.fold (FH.write devNull) $ FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cat
inspect $ 'cat `hasNoType` ''Step
inspect $ 'cat `hasNoType` ''AT.FlattenState
inspect $ 'cat `hasNoType` ''D.ConcatMapUState
#endif

-- | Send the file contents to /dev/null
{-# INLINE catStreamWrite #-}
catStreamWrite :: Handle -> Handle -> IO ()
catStreamWrite devNull inh = FH.writeS devNull $ FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'catStreamWrite
inspect $ 'catStreamWrite `hasNoType` ''Step
inspect $ 'catStreamWrite `hasNoType` ''AT.FlattenState
inspect $ 'catStreamWrite `hasNoType` ''D.ConcatMapUState
#endif

-- | Copy file
{-# INLINE copy #-}
copy :: Handle -> Handle -> IO ()
copy inh outh = S.fold (FH.write outh) (FH.read inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copy
inspect $ 'copy `hasNoType` ''Step
inspect $ 'copy `hasNoType` ''AT.FlattenState
inspect $ 'copy `hasNoType` ''D.ConcatMapUState
#endif

{-# INLINE readWord8 #-}
readWord8 :: Handle -> IO ()
readWord8 inh = S.drain $ FH.read inh

{-# INLINE decodeChar8 #-}
decodeChar8 :: Handle -> IO ()
decodeChar8 inh =
   S.drain
     $ SS.decodeChar8
     $ FH.read inh

-- | Copy file
{-# INLINE copyCodecChar8 #-}
copyCodecChar8 :: Handle -> Handle -> IO ()
copyCodecChar8 inh outh =
   S.fold (FH.write outh)
     $ SS.encodeChar8
     $ SS.decodeChar8
     $ FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyCodecChar8
inspect $ 'copyCodecChar8 `hasNoType` ''Step
inspect $ 'copyCodecChar8 `hasNoType` ''AT.FlattenState
inspect $ 'copyCodecChar8 `hasNoType` ''D.ConcatMapUState
#endif

{-# INLINE decodeUtf8Lenient #-}
decodeUtf8Lenient :: Handle -> IO ()
decodeUtf8Lenient inh =
   S.drain
     $ SS.decodeUtf8Lenient
     $ FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'decodeUtf8Lenient
-- inspect $ 'decodeUtf8Lenient `hasNoType` ''Step
-- inspect $ 'decodeUtf8Lenient `hasNoType` ''AT.FlattenState
-- inspect $ 'decodeUtf8Lenient `hasNoType` ''D.ConcatMapUState
#endif

-- | Copy file
{-# INLINE copyCodecUtf8 #-}
copyCodecUtf8 :: Handle -> Handle -> IO ()
copyCodecUtf8 inh outh =
   S.fold (FH.write outh)
     $ SS.encodeUtf8
     $ SS.decodeUtf8
     $ FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyCodecUtf8
-- inspect $ 'copyCodecUtf8 `hasNoType` ''Step
-- inspect $ 'copyCodecUtf8 `hasNoType` ''AT.FlattenState
-- inspect $ 'copyCodecUtf8 `hasNoType` ''D.ConcatMapUState
#endif

-- | Copy file
{-# INLINE copyCodecUtf8Lenient #-}
copyCodecUtf8Lenient :: Handle -> Handle -> IO ()
copyCodecUtf8Lenient inh outh =
   S.fold (FH.write outh)
     $ SS.encodeUtf8
     $ SS.decodeUtf8Lenient
     $ FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyCodecUtf8Lenient
-- inspect $ 'copyCodecUtf8Lenient `hasNoType` ''Step
-- inspect $ 'copyCodecUtf8Lenient `hasNoType` ''AT.FlattenState
-- inspect $ 'copyCodecUtf8Lenient `hasNoType` ''D.ConcatMapUState
#endif

-- | Slice in chunks of size n and get the count of chunks.
{-# INLINE chunksOf #-}
chunksOf :: Int -> Handle -> IO Int
chunksOf n inh =
    -- writeNUnsafe gives 2.5x boost here over writeN.
    S.length $ S.chunksOf n (AT.writeNUnsafe n) (FH.read inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'chunksOf
inspect $ 'chunksOf `hasNoType` ''Step
inspect $ 'chunksOf `hasNoType` ''AT.FlattenState
inspect $ 'chunksOf `hasNoType` ''D.ConcatMapUState
inspect $ 'chunksOf `hasNoType` ''GroupState
#endif

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

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'chunksOf
inspect $ 'chunksOf `hasNoType` ''Step
inspect $ 'chunksOfD `hasNoType` ''GroupState
inspect $ 'chunksOfD `hasNoType` ''AT.FlattenState
inspect $ 'chunksOfD `hasNoType` ''D.ConcatMapUState
#endif

-- XXX splitSuffixOn requires -funfolding-use-threshold=150 for better fusion
-- | Lines and unlines
{-# INLINE linesUnlinesCopy #-}
linesUnlinesCopy :: Handle -> Handle -> IO ()
linesUnlinesCopy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeChar8
      $ SS.unlines
      $ SS.lines
      $ SS.decodeChar8
      $ FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'linesUnlinesCopy [''Storable]
-- inspect $ 'linesUnlinesCopy `hasNoType` ''Step
-- inspect $ 'linesUnlinesCopy `hasNoType` ''AT.FlattenState
-- inspect $ 'linesUnlinesCopy `hasNoType` ''D.ConcatMapUState
#endif

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

{-# INLINE isSp #-}
isSp :: Word8 -> Bool
isSp = isSpace . chr . fromIntegral

-- | Word, unwords and copy
{-# INLINE wordsUnwordsCopyWord8 #-}
wordsUnwordsCopyWord8 :: Handle -> Handle -> IO ()
wordsUnwordsCopyWord8 inh outh =
    S.fold (FH.write outh)
        $ S.concatUnfold IUF.fromList
        $ S.intersperse [32]
        $ S.wordsBy isSp FL.toList
        $ FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'wordsUnwordsCopyWord8
-- inspect $ 'wordsUnwordsCopyWord8 `hasNoType` ''Step
-- inspect $ 'wordsUnwordsCopyWord8 `hasNoType` ''D.ConcatMapUState
#endif

-- | Word, unwords and copy
{-# INLINE wordsUnwordsCopy #-}
wordsUnwordsCopy :: Handle -> Handle -> IO ()
wordsUnwordsCopy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeChar8
      $ S.concatUnfold IUF.fromList
      $ S.intersperse " "
      -- Array allocation is too expensive for such small strings. So just use
      -- lists instead.
      --
      -- -- $ SS.unwords
      -- -- $ SS.words
      --
      -- XXX This pipeline does not fuse with wordsBy but fuses with splitOn
      -- with -funfolding-use-threshold=300.  With wordsBy it does not fuse
      -- even with high limits for inlining and spec-constr ghc options. With
      -- -funfolding-use-threshold=400 it performs pretty well and there
      -- is no evidence in the core that a join point involving Step
      -- constructors is not getting inlined. Not being able to fuse at all in
      -- this case could be an unknown issue, need more investigation.
      $ S.wordsBy isSpace FL.toList
      -- -- $ S.splitOn isSpace FL.toList
      $ SS.decodeChar8
      $ FH.read inh

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'wordsUnwordsCopy
-- inspect $ 'wordsUnwordsCopy `hasNoType` ''Step
-- inspect $ 'wordsUnwordsCopy `hasNoType` ''AT.FlattenState
-- inspect $ 'wordsUnwordsCopy `hasNoType` ''D.ConcatMapUState
#endif

lf :: Word8
lf = fromIntegral (ord '\n')

toarr :: String -> A.Array Word8
toarr = A.fromList . map (fromIntegral . ord)

-- | Split on line feed.
{-# INLINE splitOn #-}
splitOn :: Handle -> IO Int
splitOn inh =
    (S.length $ S.splitOn (== lf) FL.drain
        $ FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitOn
inspect $ 'splitOn `hasNoType` ''Step
inspect $ 'splitOn `hasNoType` ''AT.FlattenState
inspect $ 'splitOn `hasNoType` ''D.ConcatMapUState
#endif

-- | Split suffix on line feed.
{-# INLINE splitOnSuffix #-}
splitOnSuffix :: Handle -> IO Int
splitOnSuffix inh =
    (S.length $ S.splitOnSuffix (== lf) FL.drain
        $ FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitOnSuffix
inspect $ 'splitOnSuffix `hasNoType` ''Step
inspect $ 'splitOnSuffix `hasNoType` ''AT.FlattenState
inspect $ 'splitOnSuffix `hasNoType` ''D.ConcatMapUState
#endif

-- | Words by space
{-# INLINE wordsBy #-}
wordsBy :: Handle -> IO Int
wordsBy inh =
    (S.length $ S.wordsBy isSp FL.drain
        $ FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'wordsBy
inspect $ 'wordsBy `hasNoType` ''Step
inspect $ 'wordsBy `hasNoType` ''AT.FlattenState
inspect $ 'wordsBy `hasNoType` ''D.ConcatMapUState
#endif

-- | Split on a word8 sequence.
{-# INLINE splitOnSeq #-}
splitOnSeq :: String -> Handle -> IO Int
splitOnSeq str inh =
    (S.length $ IP.splitOnSeq (toarr str) FL.drain
        $ FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitOnSeq
-- inspect $ 'splitOnSeq `hasNoType` ''Step
-- inspect $ 'splitOnSeq `hasNoType` ''AT.FlattenState
-- inspect $ 'splitOnSeq `hasNoType` ''D.ConcatMapUState
#endif

-- | Split on a character sequence.
{-# INLINE splitOnSeqUtf8 #-}
splitOnSeqUtf8 :: String -> Handle -> IO Int
splitOnSeqUtf8 str inh =
    (S.length $ IP.splitOnSeq (A.fromList str) FL.drain
        $ SS.decodeUtf8ArraysLenient
        $ FH.readArrays inh) -- >>= print

-- | Split on suffix sequence.
{-# INLINE splitOnSuffixSeq #-}
splitOnSuffixSeq :: String -> Handle -> IO Int
splitOnSuffixSeq str inh =
    (S.length $ IP.splitOnSuffixSeq (toarr str) FL.drain
        $ FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitOnSuffixSeq
-- inspect $ 'splitOnSuffixSeq `hasNoType` ''Step
-- inspect $ 'splitOnSuffixSeq `hasNoType` ''AT.FlattenState
-- inspect $ 'splitOnSuffixSeq `hasNoType` ''D.ConcatMapUState
#endif
