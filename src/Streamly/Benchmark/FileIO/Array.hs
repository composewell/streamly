-- |
-- Module      : Streamly.Benchmark.FileIO.Array
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

module Streamly.Benchmark.FileIO.Array
    (
      last
    , countBytes
    , countLines
    , countWords
    , sumBytes
    , cat
    , copy
    , linesUnlinesCopy
    , wordsUnwordsCopy
    , decodeUtf8Lenient
    , copyCodecUtf8Lenient
    )
where

import Data.Functor.Identity (runIdentity)
import Data.Word (Word8)
import System.IO (Handle)
import Prelude hiding (last)

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude as S
import qualified Streamly.Data.String as SS

import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Prelude as Internal
import qualified Streamly.Internal.Memory.Array as IA
import qualified Streamly.Internal.Memory.ArrayStream as AS

#ifdef INSPECTION
import Foreign.Storable (Storable)
import Streamly.Streams.StreamD.Type (Step(..))
import Test.Inspection
#endif

-- | Get the last byte from a file bytestream.
{-# INLINE last #-}
last :: Handle -> IO (Maybe Word8)
last inh = do
    let s = IFH.toStreamArrays inh
    larr <- S.last s
    return $ case larr of
        Nothing -> Nothing
        Just arr -> A.readIndex arr (A.length arr - 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'last
inspect $ 'last `hasNoType` ''Step
#endif

-- | Count the number of bytes in a file.
{-# INLINE countBytes #-}
countBytes :: Handle -> IO Int
countBytes inh =
    let s = IFH.toStreamArrays inh
    in S.sum (S.map A.length s)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countBytes
inspect $ 'countBytes `hasNoType` ''Step
#endif

-- | Count the number of lines in a file.
{-# INLINE countLines #-}
countLines :: Handle -> IO Int
countLines = S.length . AS.splitOnSuffix 10 . IFH.toStreamArrays

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countLines
inspect $ 'countLines `hasNoType` ''Step
#endif

-- XXX use a word splitting combinator instead of splitOn and test it.
-- | Count the number of lines in a file.
{-# INLINE countWords #-}
countWords :: Handle -> IO Int
countWords = S.length . AS.splitOn 32 . IFH.toStreamArrays

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countWords
inspect $ 'countWords `hasNoType` ''Step
#endif

-- | Sum the bytes in a file.
{-# INLINE sumBytes #-}
sumBytes :: Handle -> IO Word8
sumBytes inh = do
    let foldlArr' f z = runIdentity . S.foldl' f z . IA.toStream
    let s = IFH.toStreamArrays inh
    S.foldl' (\acc arr -> acc + foldlArr' (+) 0 arr) 0 s

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sumBytes
inspect $ 'sumBytes `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null
{-# INLINE cat #-}
cat :: Handle -> Handle -> IO ()
cat devNull inh =
    S.fold (IFH.writeArrays devNull) $ IFH.toStreamArraysOf (256*1024) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cat
inspect $ 'cat `hasNoType` ''Step
#endif

-- | Copy file
{-# INLINE copy #-}
copy :: Handle -> Handle -> IO ()
copy inh outh =
    let s = IFH.toStreamArrays inh
    in S.fold (IFH.writeArrays outh) s

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copy
inspect $ 'copy `hasNoType` ''Step
#endif

-- | Lines and unlines
{-# INLINE linesUnlinesCopy #-}
linesUnlinesCopy :: Handle -> Handle -> IO ()
linesUnlinesCopy inh outh =
    S.fold (IFH.writeArraysInChunksOf (1024*1024) outh)
        $ Internal.insertAfterEach (return $ A.fromList [10])
        $ AS.splitOnSuffix 10
        $ IFH.toStreamArraysOf (1024*1024) inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'linesUnlinesCopy [''Storable]
-- inspect $ 'linesUnlinesCopy `hasNoType` ''Step
#endif

-- | Words and unwords
{-# INLINE wordsUnwordsCopy #-}
wordsUnwordsCopy :: Handle -> Handle -> IO ()
wordsUnwordsCopy inh outh =
    S.fold (IFH.writeArraysInChunksOf (1024*1024) outh)
        $ S.intersperse (A.fromList [32])
        -- XXX use a word splitting combinator
        $ AS.splitOn 32
        $ IFH.toStreamArraysOf (1024*1024) inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'wordsUnwordsCopy [''Storable]
-- inspect $ 'wordsUnwordsCopy `hasNoType` ''Step
#endif

{-# INLINE decodeUtf8Lenient #-}
decodeUtf8Lenient :: Handle -> IO ()
decodeUtf8Lenient inh =
   S.drain
     $ SS.decodeUtf8ArraysLenient
     $ IFH.toStreamArraysOf (1024*1024) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'decodeUtf8Lenient
-- inspect $ 'decodeUtf8Lenient `hasNoType` ''Step
-- inspect $ 'decodeUtf8Lenient `hasNoType` ''AT.FlattenState
-- inspect $ 'decodeUtf8Lenient `hasNoType` ''D.ConcatMapUState
#endif

-- | Copy file
{-# INLINE copyCodecUtf8Lenient #-}
copyCodecUtf8Lenient :: Handle -> Handle -> IO ()
copyCodecUtf8Lenient inh outh =
   S.fold (FH.write outh)
     $ SS.encodeUtf8
     $ SS.decodeUtf8ArraysLenient
     $ IFH.toStreamArraysOf (1024*1024) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyCodecUtf8Lenient
-- inspect $ 'copyCodecUtf8Lenient `hasNoType` ''Step
-- inspect $ 'copyCodecUtf8Lenient `hasNoType` ''AT.FlattenState
-- inspect $ 'copyCodecUtf8Lenient `hasNoType` ''D.ConcatMapUState
#endif
