
-- |
-- Module      : Array.Stream
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Array.Stream
  (
    Arrays
  , alloc
  , benchmarks
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad (void, when)
import Control.Monad.Catch (MonadCatch)
import Data.Maybe (isJust)
import Streamly.Internal.Data.Stream (Stream, Step)
import Streamly.Internal.Data.StreamK (StreamK)
import System.Random (randomRIO)
import Prelude hiding ()

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.ParserK as ParserK
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Streamly.Internal.Data.SVar.Type as SVar

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import Control.Monad.IO.Class (MonadIO)
import GHC.Classes (IP)
import GHC.Stack (CallStack, SrcLoc)
import Streamly.Data.MutByteArray (Unbox)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX these can be moved to the common module

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: MonadIO m => Int -> Int -> Stream.Stream m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE benchIO #-}
benchIO
    :: NFData b
    => String -> (Int -> Stream IO a) -> (Stream IO a -> IO b) -> Benchmark
benchIO name src sink =
    bench name $ nfIO $ randomRIO (1,1) >>= sink . src

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE drainWhile #-}
drainWhile :: MonadCatch m => (a -> Bool) -> Parser.Parser a m ()
drainWhile p = Parser.takeWhile p Fold.drain

-------------------------------------------------------------------------------
-- Folds and parsers
-------------------------------------------------------------------------------

-- NOTE: Ideally we should not pass streams to fused IO actions, the stream
-- boundary will remain unfused. But if we avoid that then we will have to pass
-- lists and then generate stream from list inside, but then the list-stream
-- boundary will remain unfused.

{-# ANN foldBreak_Drain (PermitPatternMatches
    [''Stream, ''SVar.State, ''Step]) #-}
{-# ANN foldBreak_Drain (PermitConstructions
    [''Fold.Step, ''(), ''SVar.State, ''Maybe]) #-}
{-# ANN foldBreak_Drain (PermitTypeClasses [''MonadIO, ''Unbox]) #-}
{-# ANN foldBreak_Drain DumpCore #-}
{-# NOINLINE foldBreak_Drain #-}
foldBreak_Drain :: Stream IO (Array.Array Int) -> IO ()
foldBreak_Drain s = void $ Array.foldBreak Fold.drain $ StreamK.fromStream s

{-# ANN parseBreak_TakeWhile (PermitPatternMatches
    [ ''[], ''ParserK.Step, ''Array.Array, ''(,), ''IO, ''Int, ''()
    , ''ParserK.Input, ''SVar.State, ''Step
    ]) #-}
{-# ANN parseBreak_TakeWhile (PermitConstructions
    [ ''Int, ''SVar.State, ''Maybe, ''Bool, ''[], ''SrcLoc, ''CallStack
    , ''(,), ''Either, ''Array.Array, ''ParserK.Step, ''(), ''ParserK.Input
    ]) #-}
{-# ANN parseBreak_TakeWhile (PermitTypeClasses [''IP, ''Show]) #-}
{-# NOINLINE parseBreak_TakeWhile #-}
parseBreak_TakeWhile :: Int -> Stream IO (Array.Array Int) -> IO ()
parseBreak_TakeWhile value s =
    void $ Array.parseBreak
            (Array.toParserK (drainWhile (< value)))
            (StreamK.fromStream s)

{-# ANN foldBreak_One_Recursive (PermitPatternMatches [''(,), ''Maybe]) #-}
{-# ANN foldBreak_One_Recursive (PermitConstructions
    [''Fold.Step, ''Maybe]) #-}
{-# ANN foldBreak_One_Recursive (PermitTypeClasses
    [''MonadIO, ''Unbox]) #-}
{-# NOINLINE foldBreak_One_Recursive #-}
foldBreak_One_Recursive :: StreamK IO (Array.Array Int) -> IO ()
foldBreak_One_Recursive s = do
    (r, s1) <- Array.foldBreak Fold.one s
    when (isJust r) $ foldBreak_One_Recursive s1

{-# ANN parseBreak_One_Recursive (PermitPatternMatches
    [ ''Int, ''(), ''ParserK.Input, ''Array.Array, ''[], ''ParserK.Step
    , ''(,), ''IO, ''Either
    ]) #-}
{-# ANN parseBreak_One_Recursive (PermitConstructions
    [ ''Int, ''(), ''ParserK.Step, ''[], ''SrcLoc, ''CallStack, ''(,)
    , ''Either, ''Array.Array, ''SVar.State, ''Maybe, ''Bool, ''ParserK.Input
    ]) #-}
{-# ANN parseBreak_One_Recursive (PermitTypeClasses [''IP, ''Show]) #-}
{-# NOINLINE parseBreak_One_Recursive #-}
parseBreak_One_Recursive :: StreamK IO (Array.Array Int) -> IO ()
parseBreak_One_Recursive s = do
    r <- Array.parseBreak (Array.toParserK Parser.one) s
    case r of
        (Left _, _) -> return ()
        (Right _, s1) -> parseBreak_One_Recursive s1


-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

type Arrays = ([Array.Array Int], [Array.Array Int])

alloc :: Int -> IO Arrays
alloc value =
    if value <= 0
    then return  (undefined, undefined)
    else
        do
        small <- Stream.toList $ Array.chunksOf 100 $ sourceUnfoldrM value 0
        big <- Stream.toList $ Array.chunksOf value $ sourceUnfoldrM value 0
        return (small, big)

-- Note: Name each benchmark (and its IO action) after the exported function it
-- benchmarks, using the format functionName_dimension1_dimension2..., where
-- the dimensions are optional variants/type specializations. Keep extra info
-- in parenthetical notes in the description.
benchmarks :: Arrays -> Int -> [(SpaceComplexity, Benchmark)]
benchmarks arrays value =
    let (arraysSmall, arraysBig) = arrays
    in
      [ (SpaceO_1, benchIO "foldBreak_Drain (100-elem arrays)"
            (\_ -> Stream.fromList arraysSmall) foldBreak_Drain)
      , (SpaceO_1, benchIO "foldBreak_Drain (one large array)"
            (\_ -> Stream.fromList arraysBig) foldBreak_Drain)
      , (SpaceO_1, benchIO
            "foldBreak_One_Recursive (100-elem arrays)"
            (\_ -> Stream.fromList arraysSmall)
            (foldBreak_One_Recursive . StreamK.fromStream))
      , (SpaceO_1, benchIO "parseBreak_TakeWhile (100-elem arrays)"
            (\_ -> Stream.fromList arraysSmall)
            $ parseBreak_TakeWhile value)
      , (SpaceO_1, benchIO "parseBreak_TakeWhile (one large array)"
            (\_ -> Stream.fromList arraysBig)
            $ parseBreak_TakeWhile value)
      , (SpaceO_1, benchIO
            "parseBreak_One_Recursive (100-elem arrays)"
            (\_ -> Stream.fromList arraysSmall)
            (parseBreak_One_Recursive . StreamK.fromStream))
      ]
