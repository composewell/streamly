{-# OPTIONS_GHC -Wno-deprecations #-}

--
-- Module      : Streamly.Unicode.Stream
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

import Streamly.Data.Stream (Stream)
import Streamly.Data.Fold (Fold)
import Prelude hiding (last, length)
import System.IO (Handle)

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.Unicode.Array as UnicodeArr
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Streamly.Internal.Data.MutByteArray (Unbox)
import Streamly.Internal.Data.Stream (Step(..))
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Tuple.Strict as Strict

import Test.Inspection
#endif

moduleName :: String
moduleName = "Unicode.Stream"

-- | Copy file
{-# NOINLINE copyCodecUtf8ArraysLenient #-}
copyCodecUtf8ArraysLenient :: Handle -> Handle -> IO ()
copyCodecUtf8ArraysLenient inh outh =
   Stream.fold (Handle.write outh)
     $ Unicode.encodeUtf8'
     $ Unicode.decodeUtf8Chunks
     $ Handle.readChunks inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyCodecUtf8ArraysLenient
-- inspect $ 'copyCodecUtf8ArraysLenient `hasNoType` ''Step
#endif

o_1_space_decode_encode_chunked :: BenchEnv -> [Benchmark]
o_1_space_decode_encode_chunked env =
    [ bgroup "decode-encode/toChunks"
        [
        mkBenchSmall "encodeUtf8' . decodeUtf8Arrays" env $ \inH outH ->
            copyCodecUtf8ArraysLenient inH outH
        ]
    ]

-------------------------------------------------------------------------------
-- copy with group/ungroup transformations
-------------------------------------------------------------------------------

{-# INLINE splitOnSuffix #-}
splitOnSuffix
    :: (Monad m)
    => (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
splitOnSuffix predicate f = Stream.foldMany (Fold.takeEndBy_ predicate f)

{-# NOINLINE linesUnlinesCopy #-}
linesUnlinesCopy :: Handle -> Handle -> IO ()
linesUnlinesCopy inh outh =
    Stream.fold (Handle.write outh)
      $ Unicode.encodeLatin1'
      $ Unicode.unlines Unfold.fromList
      $ splitOnSuffix (== '\n') Fold.toList
      $ Unicode.decodeLatin1
      $ Stream.unfold Handle.reader inh

{-# NOINLINE linesUnlinesArrayWord8Copy #-}
linesUnlinesArrayWord8Copy :: Handle -> Handle -> IO ()
linesUnlinesArrayWord8Copy inh outh =
    Stream.fold (Handle.write outh)
      $ Stream.interposeSuffix 10 Array.reader
      $ splitOnSuffix (== 10) Array.write
      $ Stream.unfold Handle.reader inh

-- XXX splitSuffixOn requires -funfolding-use-threshold=150 for better fusion
-- | Lines and unlines
{-# NOINLINE linesUnlinesArrayCharCopy #-}
linesUnlinesArrayCharCopy :: Handle -> Handle -> IO ()
linesUnlinesArrayCharCopy inh outh =
    Stream.fold (Handle.write outh)
      $ Unicode.encodeLatin1'
      $ UnicodeArr.unlines
      $ UnicodeArr.lines
      $ Unicode.decodeLatin1
      $ Stream.unfold Handle.reader inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'linesUnlinesArrayCharCopy [''Unbox]
-- inspect $ 'linesUnlinesArrayCharCopy `hasNoType` ''Step
#endif

-- XXX to write this we need to be able to map decodeUtf8 on the Array.read fold.
-- For that we have to write decodeUtf8 as a Pipe.
{-
{-# INLINE linesUnlinesArrayUtf8Copy #-}
linesUnlinesArrayUtf8Copy :: Handle -> Handle -> IO ()
linesUnlinesArrayUtf8Copy inh outh =
    Stream.fold (Handle.write outh)
      $ Unicode.encodeLatin1'
      $ Stream.intercalate (Array.fromList [10]) (pipe Unicode.decodeUtf8P Array.read)
      $ Stream.splitOnSuffix (== '\n') (IFold.map Unicode.encodeUtf8' Array.write)
      $ Unicode.decodeLatin1
      $ Stream.unfold Handle.read inh
-}

-- | Word, unwords and copy
{-# NOINLINE wordsUnwordsCopyWord8 #-}
wordsUnwordsCopyWord8 :: Handle -> Handle -> IO ()
wordsUnwordsCopyWord8 inh outh =
    Stream.fold (Handle.write outh)
        $ Stream.interposeSuffix 32 Unfold.fromList
        $ Stream.wordsBy isSp Fold.toList
        $ Stream.unfold Handle.reader inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'wordsUnwordsCopyWord8
-- inspect $ 'wordsUnwordsCopyWord8 `hasNoType` ''Step
#endif

-- | Word, unwords and copy
{-# NOINLINE wordsUnwordsCopy #-}
wordsUnwordsCopy :: Handle -> Handle -> IO ()
wordsUnwordsCopy inh outh =
    Stream.fold (Handle.write outh)
      $ Unicode.encodeLatin1'
      $ Unicode.unwords Unfold.fromList
      -- XXX This pipeline does not fuse with wordsBy but fuses with splitOn
      -- with -funfolding-use-threshold=300.  With wordsBy it does not fuse
      -- even with high limits for inlining and spec-constr ghc options. With
      -- -funfolding-use-threshold=400 it performs pretty well and there
      -- is no evidence in the core that a join point involving Step
      -- constructors is not getting inlined. Not being able to fuse at all in
      -- this case could be an unknown issue, need more investigation.
      $ Stream.wordsBy isSpace Fold.toList
      -- -- $ Stream.splitOn isSpace Fold.toList
      $ Unicode.decodeLatin1
      $ Stream.unfold Handle.reader inh

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'wordsUnwordsCopy
-- inspect $ 'wordsUnwordsCopy `hasNoType` ''Step
#endif

{-# NOINLINE wordsUnwordsCharArrayCopy #-}
wordsUnwordsCharArrayCopy :: Handle -> Handle -> IO ()
wordsUnwordsCharArrayCopy inh outh =
    Stream.fold (Handle.write outh)
      $ Unicode.encodeLatin1'
      $ UnicodeArr.unwords
      $ UnicodeArr.words
      $ Unicode.decodeLatin1
      $ Stream.unfold Handle.reader inh

o_1_space_copy_read_group_ungroup :: BenchEnv -> [Benchmark]
o_1_space_copy_read_group_ungroup env =
    [ bgroup "ungroup-group"
        [ mkBenchSmall "unlines . splitOnSuffix ([Word8])" env
            $ \inh outh -> linesUnlinesCopy inh outh
        , mkBenchSmall "interposeSuffix . splitOnSuffix (Array Word8)" env
            $ \inh outh -> linesUnlinesArrayWord8Copy inh outh
        , mkBenchSmall "UnicodeArr.unlines . UnicodeArr.lines (Array Char)" env
            $ \inh outh -> linesUnlinesArrayCharCopy inh outh

        , mkBenchSmall "interposeSuffix . wordsBy ([Word8])" env
            $ \inh outh -> wordsUnwordsCopyWord8 inh outh
        , mkBenchSmall "unwords . wordsBy ([Char])" env
            $ \inh outh -> wordsUnwordsCopy inh outh
        , mkBenchSmall "UnicodeArr.unwords . UnicodeArr.words (Array Char)" env
            $ \inh outh -> wordsUnwordsCharArrayCopy inh outh
        ]
    ]

-------------------------------------------------------------------------------
-- copy unfold
-------------------------------------------------------------------------------

-- | Copy file (encodeLatin1')
{-# NOINLINE copyStreamLatin1' #-}
copyStreamLatin1' :: Handle -> Handle -> IO ()
copyStreamLatin1' inh outh =
   Stream.fold (Handle.write outh)
     $ Unicode.encodeLatin1'
     $ Unicode.decodeLatin1
     $ Stream.unfold Handle.reader inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyStreamLatin1'
inspect $ 'copyStreamLatin1' `hasNoType` ''Step
inspect $ 'copyStreamLatin1' `hasNoType` ''Unfold.ConcatState -- Handle.read/UF.many

inspect $ 'copyStreamLatin1' `hasNoType` ''Fold.Step
inspect $ 'copyStreamLatin1' `hasNoType` ''MutArray.ArrayUnsafe -- Handle.write/writeNUnsafe
                                                             -- Handle.read/Array.read
inspect $ 'copyStreamLatin1' `hasNoType` ''Strict.Tuple3' -- Handle.write/chunksOf
#endif

-- | Copy file (encodeLatin1)
{-# NOINLINE copyStreamLatin1 #-}
copyStreamLatin1 :: Handle -> Handle -> IO ()
copyStreamLatin1 inh outh =
   Stream.fold (Handle.write outh)
     $ Unicode.encodeLatin1
     $ Unicode.decodeLatin1
     $ Stream.unfold Handle.reader inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyStreamLatin1
inspect $ 'copyStreamLatin1 `hasNoType` ''Step
inspect $ 'copyStreamLatin1 `hasNoType` ''Unfold.ConcatState -- Handle.read/UF.many

inspect $ 'copyStreamLatin1 `hasNoType` ''Fold.ManyState
inspect $ 'copyStreamLatin1 `hasNoType` ''Fold.Step
inspect $ 'copyStreamLatin1 `hasNoType` ''MutArray.ArrayUnsafe -- Handle.write/writeNUnsafe
                                                            -- Handle.read/Array.read
inspect $ 'copyStreamLatin1 `hasNoType` ''Strict.Tuple3' -- Handle.write/chunksOf
#endif

-- | Copy file
_copyStreamUtf8' :: Handle -> Handle -> IO ()
_copyStreamUtf8' inh outh =
   Stream.fold (Handle.write outh)
     $ Unicode.encodeUtf8'
     $ Unicode.decodeUtf8'
     $ Stream.unfold Handle.reader inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses '_copyStreamUtf8'
-- inspect $ '_copyStreamUtf8 `hasNoType` ''Step
-- inspect $ '_copyStreamUtf8 `hasNoType` ''Array.FlattenState
-- inspect $ '_copyStreamUtf8 `hasNoType` ''D.ConcatMapUState
#endif

-- | Copy file
{-# NOINLINE copyStreamUtf8 #-}
copyStreamUtf8 :: Handle -> Handle -> IO ()
copyStreamUtf8 inh outh =
   Stream.fold (Handle.write outh)
     $ Unicode.encodeUtf8
     $ Unicode.decodeUtf8
     $ Stream.unfold Handle.reader inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyStreamUtf8
-- inspect $ 'copyStreamUtf8Lax `hasNoType` ''Step
-- inspect $ 'copyStreamUtf8Lax `hasNoType` ''Array.FlattenState
-- inspect $ 'copyStreamUtf8Lax `hasNoType` ''D.ConcatMapUState
#endif

{-# NOINLINE _copyStreamUtf8'Fold #-}
_copyStreamUtf8'Fold :: Handle -> Handle -> IO ()
_copyStreamUtf8'Fold inh outh =
   Stream.fold (Handle.write outh)
     $ Unicode.encodeUtf8
     $ Stream.catRights
     $ Stream.parseMany Unicode.writeCharUtf8'
     $ Stream.unfold Handle.reader inh

{-# NOINLINE _copyStreamUtf8Parser #-}
_copyStreamUtf8Parser :: Handle -> Handle -> IO ()
_copyStreamUtf8Parser inh outh =
   Stream.fold (Handle.write outh)
     $ Unicode.encodeUtf8
     $ Stream.catRights $ Stream.parseMany
           (Unicode.parseCharUtf8With Unicode.TransliterateCodingFailure)
     $ Stream.unfold Handle.reader inh

o_1_space_decode_encode_read :: BenchEnv -> [Benchmark]
o_1_space_decode_encode_read env =
    [ bgroup "decode-encode"
        [
        -- This needs an ascii file, as decode just errors out.
          mkBench "encodeLatin1' . decodeLatin1" env $ \inh outh ->
            copyStreamLatin1' inh outh
        , mkBench "encodeLatin1 . decodeLatin1" env $ \inh outh ->
            copyStreamLatin1 inh outh
#ifdef INCLUDE_STRICT_UTF8
        -- Requires valid unicode input
        , mkBench "encodeUtf8' . decodeUtf8'" env $ \inh outh ->
            _copyStreamUtf8' inh outh
        , mkBench "encodeUtf8' . foldMany writeCharUtf8'" env $ \inh outh ->
            _copyStreamUtf8'Fold inh outh
#endif
        , mkBenchSmall "encodeUtf8 . parseMany parseCharUtf8" env
              $ \inh outh -> _copyStreamUtf8Parser inh outh
        , mkBenchSmall "encodeUtf8 . decodeUtf8" env $ \inh outh ->
            copyStreamUtf8 inh outh
        ]
    ]

main :: IO ()
main = do
#ifndef FUSION_CHECK
    env <- mkHandleBenchEnv
    defaultMain (allBenchmarks env)

    where

    allBenchmarks env =
        [ bgroup (o_1_space_prefix moduleName) $ Prelude.concat
            [ o_1_space_copy_read_group_ungroup env
            , o_1_space_decode_encode_chunked env
            , o_1_space_decode_encode_read env
            ]
        ]
#else
    -- Enable FUSION_CHECK macro at the beginning of the file
    -- Enable one benchmark below, and run the benchmark
    -- Check the .dump-simpl output
    env <- mkHandleBenchEnv
    let mkHandles (RefHandles {bigInH = inh, outputH = outh}) = Handles inh outh
    (Handles inh outh) <- getHandles env mkHandles
    copyStreamLatin1' inh outh
    return ()
#endif
