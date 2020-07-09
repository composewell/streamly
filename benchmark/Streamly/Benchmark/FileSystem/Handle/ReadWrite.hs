-- |
-- Module      : Streamly.Benchmark.FileSystem.Handle
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Handle.ReadWrite
    (allBenchmarks)
where

import Control.Exception (SomeException)
import System.IO (Handle, hClose)
import Prelude hiding (last, length)

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Unicode.Stream as SS
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Internal.Data.Unicode.Stream as IUS
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Data.Prim.Pinned.Unicode.Array as IUA
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Internal.Data.Prim.Pinned.Array as A
import qualified Streamly.Internal.Data.Prim.Pinned.Array.Types as A
import qualified Streamly.Prelude as S

import Gauge hiding (env)
import Handle.Common

#ifdef INSPECTION
-- XXX Is this required anymore?
import Foreign.Storable (Storable)
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))

import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Strict as Strict

import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- copy chunked
-------------------------------------------------------------------------------

-- | Copy file
copyChunks :: Handle -> Handle -> IO ()
copyChunks inh outh = S.fold (IFH.writeChunks outh) $ IFH.toChunks inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyChunks
inspect $ 'copyChunks `hasNoType` ''Step
#endif

-- | Copy file
copyCodecUtf8ArraysLenient :: Handle -> Handle -> IO ()
copyCodecUtf8ArraysLenient inh outh =
   S.fold (FH.write outh)
     $ SS.encodeUtf8
     $ IUS.decodeUtf8ArraysLenient
     $ IFH.toChunks inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyCodecUtf8ArraysLenient
-- inspect $ 'copyCodecUtf8ArraysLenient `hasNoType` ''Step
#endif

o_1_space_copy_chunked :: BenchEnv -> [Benchmark]
o_1_space_copy_chunked env =
    [ bgroup "copy/toChunks"
        [ mkBench "toNull" env $ \inH _ ->
            copyChunks inH (nullH env)
        , mkBench "raw" env $ \inH outH ->
            copyChunks inH outH
        , mkBenchSmall "decodeEncodeUtf8Lenient" env $ \inH outH ->
            copyCodecUtf8ArraysLenient inH outH
        ]
    ]

-- TBD reading with unfold

-------------------------------------------------------------------------------
-- Exceptions readChunks
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
readChunksOnException :: Handle -> Handle -> IO ()
readChunksOnException inh devNull =
    let readEx = IUF.onException (\_ -> hClose inh) FH.readChunks
    in IUF.fold readEx (IFH.writeChunks devNull) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readChunksOnException
-- inspect $ 'readChunksOnException `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null with exception handling
readChunksBracket :: Handle -> Handle -> IO ()
readChunksBracket inh devNull =
    let readEx = IUF.bracket return (\_ -> hClose inh) FH.readChunks
    in IUF.fold readEx (IFH.writeChunks devNull) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readChunksBracket
-- inspect $ 'readChunksBracket `hasNoType` ''Step
#endif

readChunksBracketIO :: Handle -> Handle -> IO ()
readChunksBracketIO inh devNull =
    let readEx = IUF.bracketIO return (\_ -> hClose inh) FH.readChunks
    in IUF.fold readEx (IFH.writeChunks devNull) inh

o_1_space_copy_exceptions_readChunks :: BenchEnv -> [Benchmark]
o_1_space_copy_exceptions_readChunks env =
    [ bgroup "copy/readChunks/exceptions"
        [ mkBench "UF.onException" env $ \inH _ ->
            readChunksOnException inH (nullH env)
        , mkBench "UF.bracket" env $ \inH _ ->
            readChunksBracket inH (nullH env)
        , mkBench "UF.bracketIO" env $ \inH _ ->
            readChunksBracketIO inH (nullH env)
        ]
    ]

-------------------------------------------------------------------------------
-- Exceptions toChunks
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
toChunksBracket :: Handle -> Handle -> IO ()
toChunksBracket inh devNull =
    let readEx = S.bracket
            (return ())
            (\_ -> hClose inh)
            (\_ -> IFH.toChunks inh)
    in S.fold (IFH.writeChunks devNull) $ readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksBracket
-- inspect $ 'toChunksBracket `hasNoType` ''Step
#endif

toChunksBracketIO :: Handle -> Handle -> IO ()
toChunksBracketIO inh devNull =
    let readEx = IP.bracketIO
            (return ())
            (\_ -> hClose inh)
            (\_ -> IFH.toChunks inh)
    in S.fold (IFH.writeChunks devNull) $ readEx

o_1_space_copy_exceptions_toChunks :: BenchEnv -> [Benchmark]
o_1_space_copy_exceptions_toChunks env =
    [ bgroup "copy/toChunks/exceptions"
        [ mkBench "S.bracket" env $ \inH _ ->
            toChunksBracket inH (nullH env)
        , mkBench "S.bracketIO" env $ \inH _ ->
            toChunksBracketIO inH (nullH env)
        ]
    ]

-------------------------------------------------------------------------------
-- copy unfold
-------------------------------------------------------------------------------

-- | Copy file
copyStream :: Handle -> Handle -> IO ()
copyStream inh outh = S.fold (FH.write outh) (S.unfold FH.read inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyStream
inspect $ 'copyStream `hasNoType` ''Step -- S.unfold
inspect $ 'copyStream `hasNoType` ''IUF.ConcatState -- FH.read/UF.concat
-- inspect $ 'copyStream `hasNoType` ''A.ReadUState  -- FH.read/A.read
-- inspect $ 'copyStream `hasNoType` ''A.ArrayUnsafe -- FH.write/writeNUnsafe
inspect $ 'copyStream `hasNoType` ''Strict.Tuple3' -- FH.write/lchunksOf
#endif

-- | Copy file
copyStreamLatin1 :: Handle -> Handle -> IO ()
copyStreamLatin1 inh outh =
   S.fold (FH.write outh)
     $ SS.encodeLatin1
     $ SS.decodeLatin1
     $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyStreamLatin1
inspect $ 'copyStreamLatin1 `hasNoType` ''Step
inspect $ 'copyStreamLatin1 `hasNoType` ''IUF.ConcatState -- FH.read/UF.concat
-- inspect $ 'copyStreamLatin1 `hasNoType` ''A.ReadUState  -- FH.read/A.read
-- inspect $ 'copyStreamLatin1 `hasNoType` ''A.ArrayUnsafe -- FH.write/writeNUnsafe
inspect $ 'copyStreamLatin1 `hasNoType` ''Strict.Tuple3' -- FH.write/lchunksOf
#endif

-- | Copy file
_copyStreamUtf8 :: Handle -> Handle -> IO ()
_copyStreamUtf8 inh outh =
   S.fold (FH.write outh)
     $ SS.encodeUtf8
     $ SS.decodeUtf8
     $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses '_copyStreamUtf8
-- inspect $ '_copyStreamUtf8 `hasNoType` ''Step
-- inspect $ '_copyStreamUtf8 `hasNoType` ''A.FlattenState
-- inspect $ '_copyStreamUtf8 `hasNoType` ''D.ConcatMapUState
#endif

-- | Copy file
copyStreamUtf8Lax :: Handle -> Handle -> IO ()
copyStreamUtf8Lax inh outh =
   S.fold (FH.write outh)
     $ SS.encodeUtf8
     $ SS.decodeUtf8Lax
     $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyStreamUtf8Lax
-- inspect $ 'copyStreamUtf8Lax `hasNoType` ''Step
-- inspect $ 'copyStreamUtf8Lax `hasNoType` ''A.FlattenState
-- inspect $ 'copyStreamUtf8Lax `hasNoType` ''D.ConcatMapUState
#endif

o_1_space_copy_read :: BenchEnv -> [Benchmark]
o_1_space_copy_read env =
    [ bgroup "copy/read"
        [ mkBench "rawToNull" env $ \inh _ ->
            copyStream inh (nullH env)
        , mkBench "rawToFile" env $ \inh outh ->
            copyStream inh outh
        -- This needs an ascii file, as decode just errors out.
        , mkBench "SS.encodeLatin1 . SS.decodeLatin1" env $ \inh outh ->
            copyStreamLatin1 inh outh
#ifdef DEVBUILD
        , mkBench "copyUtf8" env $ \inh outh ->
            _copyStreamUtf8 inh outh
#endif
        , mkBenchSmall "SS.encodeUtf8 . SS.decodeUtf8Lax" env $ \inh outh ->
            copyStreamUtf8Lax inh outh
        ]
    ]

-------------------------------------------------------------------------------
-- copy stream
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null
readFromBytesNull :: Handle -> Handle -> IO ()
readFromBytesNull inh devNull = IFH.fromBytes devNull $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readFromBytesNull
inspect $ 'readFromBytesNull `hasNoType` ''Step
inspect $ 'readFromBytesNull `hasNoType` ''A.SpliceState
-- inspect $ 'readFromBytesNull `hasNoType` ''A.ArrayUnsafe -- FH.fromBytes/S.arraysOf
inspect $ 'readFromBytesNull `hasNoType` ''D.GroupState
#endif

o_1_space_copy_fromBytes :: BenchEnv -> [Benchmark]
o_1_space_copy_fromBytes env =
    [ bgroup "copy/fromBytes"
        [ mkBench "rawToNull" env $ \inh _ ->
            readFromBytesNull inh (nullH env)
        ]
    ]

-------------------------------------------------------------------------------
-- unfold exceptions
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
readWriteOnExceptionUnfold :: Handle -> Handle -> IO ()
readWriteOnExceptionUnfold inh devNull =
    let readEx = IUF.onException (\_ -> hClose inh) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteOnExceptionUnfold
-- inspect $ 'readWriteOnExceptionUnfold `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null with exception handling
readWriteHandleExceptionUnfold :: Handle -> Handle -> IO ()
readWriteHandleExceptionUnfold inh devNull =
    let handler (_e :: SomeException) = hClose inh >> return 10
        readEx = IUF.handle (IUF.singletonM handler) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteHandleExceptionUnfold
-- inspect $ 'readWriteHandleExceptionUnfold `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null with exception handling
readWriteFinallyUnfold :: Handle -> Handle -> IO ()
readWriteFinallyUnfold inh devNull =
    let readEx = IUF.finally (\_ -> hClose inh) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteFinallyUnfold
-- inspect $ 'readWriteFinallyUnfold `hasNoType` ''Step
#endif

readWriteFinallyIOUnfold :: Handle -> Handle -> IO ()
readWriteFinallyIOUnfold inh devNull =
    let readEx = IUF.finallyIO (\_ -> hClose inh) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

-- | Send the file contents to /dev/null with exception handling
readWriteBracketUnfold :: Handle -> Handle -> IO ()
readWriteBracketUnfold inh devNull =
    let readEx = IUF.bracket return (\_ -> hClose inh) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteBracketUnfold
-- inspect $ 'readWriteBracketUnfold `hasNoType` ''Step
#endif

readWriteBracketIOUnfold :: Handle -> Handle -> IO ()
readWriteBracketIOUnfold inh devNull =
    let readEx = IUF.bracketIO return (\_ -> hClose inh) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

o_1_space_copy_read_exceptions :: BenchEnv -> [Benchmark]
o_1_space_copy_read_exceptions env =
    [ bgroup "copy/read/exceptions"
       [ mkBenchSmall "UF.onException" env $ \inh _ ->
           readWriteOnExceptionUnfold inh (nullH env)
       , mkBenchSmall "UF.handle" env $ \inh _ ->
           readWriteHandleExceptionUnfold inh (nullH env)
       , mkBenchSmall "UF.finally" env $ \inh _ ->
           readWriteFinallyUnfold inh (nullH env)
       , mkBenchSmall "UF.finallyIO" env $ \inh _ ->
           readWriteFinallyIOUnfold inh (nullH env)
       , mkBenchSmall "UF.bracket" env $ \inh _ ->
           readWriteBracketUnfold inh (nullH env)
       , mkBenchSmall "UF.bracketIO" env $ \inh _ ->
           readWriteBracketIOUnfold inh (nullH env)
        ]
    ]

-------------------------------------------------------------------------------
-- stream exceptions
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
readWriteOnExceptionStream :: Handle -> Handle -> IO ()
readWriteOnExceptionStream inh devNull =
    let readEx = S.onException (hClose inh) (S.unfold FH.read inh)
    in S.fold (FH.write devNull) $ readEx

-- | Send the file contents to /dev/null with exception handling
readWriteHandleExceptionStream :: Handle -> Handle -> IO ()
readWriteHandleExceptionStream inh devNull =
    let handler (_e :: SomeException) = S.yieldM (hClose inh >> return 10)
        readEx = S.handle handler (S.unfold FH.read inh)
    in S.fold (FH.write devNull) $ readEx

-- | Send the file contents to /dev/null with exception handling
readWriteFinallyStream :: Handle -> Handle -> IO ()
readWriteFinallyStream inh devNull =
    let readEx = S.finally (hClose inh) (S.unfold FH.read inh)
    in S.fold (FH.write devNull) readEx

readWriteFinallyIOStream :: Handle -> Handle -> IO ()
readWriteFinallyIOStream inh devNull =
    let readEx = IP.finallyIO (hClose inh) (S.unfold FH.read inh)
    in S.fold (FH.write devNull) readEx

-- | Send the file contents to /dev/null with exception handling
fromToBytesBracketStream :: Handle -> Handle -> IO ()
fromToBytesBracketStream inh devNull =
    let readEx = S.bracket (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.toBytes inh)
    in IFH.fromBytes devNull $ readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fromToBytesBracketStream
-- inspect $ 'fromToBytesBracketStream `hasNoType` ''Step
#endif

fromToBytesBracketIOStream :: Handle -> Handle -> IO ()
fromToBytesBracketIOStream inh devNull =
    let readEx = IP.bracketIO (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.toBytes inh)
    in IFH.fromBytes devNull $ readEx

o_1_space_copy_stream_exceptions :: BenchEnv -> [Benchmark]
o_1_space_copy_stream_exceptions env =
    [ bgroup "copy/read/exceptions"
       [ mkBenchSmall "S.onException" env $ \inh _ ->
           readWriteOnExceptionStream inh (nullH env)
       , mkBenchSmall "S.handle" env $ \inh _ ->
           readWriteHandleExceptionStream inh (nullH env)
       , mkBenchSmall "S.finally" env $ \inh _ ->
           readWriteFinallyStream inh (nullH env)
       , mkBenchSmall "S.finallyIO" env $ \inh _ ->
           readWriteFinallyIOStream inh (nullH env)
       ]
    , bgroup "copy/fromToBytes/exceptions"
       [ mkBenchSmall "S.bracket" env $ \inh _ ->
           fromToBytesBracketStream inh (nullH env)
       , mkBenchSmall "S.bracketIO" env $ \inh _ ->
           fromToBytesBracketIOStream inh (nullH env)
        ]
    ]

-------------------------------------------------------------------------------
-- copy with group/ungroup transformations
-------------------------------------------------------------------------------

-- | Lines and unlines
copyChunksSplitInterposeSuffix :: Handle -> Handle -> IO ()
copyChunksSplitInterposeSuffix inh outh =
    S.fold (IFH.write outh)
        $ IP.interposeSuffix 10 A.read
        $ IP.splitInnerBySuffix (A.breakOn 10) A.spliceTwo
        $ IFH.toChunks inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'copyChunksSplitInterposeSuffix [''Storable]
-- inspect $ 'copyChunksSplitInterposeSuffix `hasNoType` ''Step
#endif

-- | Words and unwords
copyChunksSplitInterpose :: Handle -> Handle -> IO ()
copyChunksSplitInterpose inh outh =
    S.fold (IFH.write outh)
        $ IP.interpose 32 A.read
        -- XXX this is not correct word splitting combinator
        $ IP.splitInnerBy (A.breakOn 32) A.spliceTwo
        $ IFH.toChunks inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'copyChunksSplitInterpose [''Storable]
-- inspect $ 'copyChunksSplitInterpose `hasNoType` ''Step
#endif

o_1_space_copy_toChunks_group_ungroup :: BenchEnv -> [Benchmark]
o_1_space_copy_toChunks_group_ungroup env =
    [ bgroup "copy/toChunks/group-ungroup"
        [ mkBench "IP.interposeSuffix . IP.splitInnerBySuffix (A.breakOn 10) A.spliceTwo" env $ \inh outh ->
            copyChunksSplitInterposeSuffix inh outh
        , mkBenchSmall "IP.interpose . IP.splitInnerBy (A.breakOn 32) A.spliceTwo" env $ \inh outh ->
            copyChunksSplitInterpose inh outh
        ]
    ]

-------------------------------------------------------------------------------
-- copy with group/ungroup transformations
-------------------------------------------------------------------------------

linesUnlinesCopy :: Handle -> Handle -> IO ()
linesUnlinesCopy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeLatin1
      $ IUS.unlines IUF.fromList
      $ S.splitOnSuffix (== '\n') FL.toList
      $ SS.decodeLatin1
      $ S.unfold FH.read inh

linesUnlinesArrayWord8Copy :: Handle -> Handle -> IO ()
linesUnlinesArrayWord8Copy inh outh =
    S.fold (FH.write outh)
      $ IP.interposeSuffix 10 A.read
      $ S.splitOnSuffix (== 10) A.write
      $ S.unfold FH.read inh

-- XXX splitSuffixOn requires -funfolding-use-threshold=150 for better fusion
-- | Lines and unlines
linesUnlinesArrayCharCopy :: Handle -> Handle -> IO ()
linesUnlinesArrayCharCopy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeLatin1
      $ IUA.unlines
      $ IUA.lines
      $ SS.decodeLatin1
      $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'linesUnlinesArrayCharCopy [''Storable]
-- inspect $ 'linesUnlinesArrayCharCopy `hasNoType` ''Step
#endif

-- XXX to write this we need to be able to map decodeUtf8 on the A.read fold.
-- For that we have to write decodeUtf8 as a Pipe.
{-
{-# INLINE linesUnlinesArrayUtf8Copy #-}
linesUnlinesArrayUtf8Copy :: Handle -> Handle -> IO ()
linesUnlinesArrayUtf8Copy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeLatin1
      $ IP.intercalate (A.fromList [10]) (pipe SS.decodeUtf8P A.read)
      $ S.splitOnSuffix (== '\n') (IFL.lmap SS.encodeUtf8 A.write)
      $ SS.decodeLatin1
      $ S.unfold FH.read inh
-}

-- | Word, unwords and copy
wordsUnwordsCopyWord8 :: Handle -> Handle -> IO ()
wordsUnwordsCopyWord8 inh outh =
    S.fold (FH.write outh)
        $ IP.interposeSuffix 32 IUF.fromList
        $ S.wordsBy isSp FL.toList
        $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'wordsUnwordsCopyWord8
-- inspect $ 'wordsUnwordsCopyWord8 `hasNoType` ''Step
#endif

-- | Word, unwords and copy
wordsUnwordsCopy :: Handle -> Handle -> IO ()
wordsUnwordsCopy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeLatin1
      $ IUS.unwords IUF.fromList
      -- XXX This pipeline does not fuse with wordsBy but fuses with splitOn
      -- with -funfolding-use-threshold=300.  With wordsBy it does not fuse
      -- even with high limits for inlining and spec-constr ghc options. With
      -- -funfolding-use-threshold=400 it performs pretty well and there
      -- is no evidence in the core that a join point involving Step
      -- constructors is not getting inlined. Not being able to fuse at all in
      -- this case could be an unknown issue, need more investigation.
      $ S.wordsBy isSpace FL.toList
      -- -- $ S.splitOn isSpace FL.toList
      $ SS.decodeLatin1
      $ S.unfold FH.read inh

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'wordsUnwordsCopy
-- inspect $ 'wordsUnwordsCopy `hasNoType` ''Step
#endif

wordsUnwordsCharArrayCopy :: Handle -> Handle -> IO ()
wordsUnwordsCharArrayCopy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeLatin1
      $ IUA.unwords
      $ IUA.words
      $ SS.decodeLatin1
      $ S.unfold FH.read inh

o_1_space_copy_read_group_ungroup :: BenchEnv -> [Benchmark]
o_1_space_copy_read_group_ungroup env =
    [ bgroup "copy/read/group-ungroup"
        [ mkBenchSmall "US.unlines . S.splitOnSuffix ([Word8])" env
            $ \inh outh -> linesUnlinesCopy inh outh
        , mkBenchSmall "S.interposeSuffix . S.splitOnSuffix(Array Word8)" env
            $ \inh outh -> linesUnlinesArrayWord8Copy inh outh
        , mkBenchSmall "UA.unlines . UA.lines (Array Char)" env
            $ \inh outh -> linesUnlinesArrayCharCopy inh outh

        , mkBenchSmall "S.interposeSuffix . S.wordsBy ([Word8])" env
            $ \inh outh -> wordsUnwordsCopyWord8 inh outh
        , mkBenchSmall "US.unwords . S.wordsBy ([Char])" env
            $ \inh outh -> wordsUnwordsCopy inh outh
        , mkBenchSmall "UA.unwords . UA.words (Array Char)" env
            $ \inh outh -> wordsUnwordsCharArrayCopy inh outh
        ]
    ]

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

allBenchmarks :: BenchEnv -> [Benchmark]
allBenchmarks env = Prelude.concat
    [ o_1_space_copy_chunked env
    , o_1_space_copy_exceptions_readChunks env
    , o_1_space_copy_exceptions_toChunks env
    , o_1_space_copy_read env
    , o_1_space_copy_fromBytes env
    , o_1_space_copy_read_exceptions env
    , o_1_space_copy_stream_exceptions env
    , o_1_space_copy_toChunks_group_ungroup env
    , o_1_space_copy_read_group_ungroup env
    ]
