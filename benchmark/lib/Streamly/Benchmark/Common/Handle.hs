-- |
-- Module      : Streamly.Benchmark.Common.Handle
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
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

module Streamly.Benchmark.Common.Handle
    ( BenchEnv (..)
    , RefHandles (..)
    , scratchDir
    , inFileSmall
    , inFileBig
    , outfile
    , mkBench
    , mkBenchSmall
    , isSpace
    , isSp
    , mkHandleBenchEnv
    , Handles(..)
    , getHandles
    )
where

import Control.DeepSeq (NFData(rnf))
import Data.Char (ord, chr)
#if MIN_VERSION_base(4,17,0)
import Data.Char (generalCategory, GeneralCategory(Space))
#endif
import Data.Word (Word8)
import System.Directory (getFileSize)
import System.Environment (lookupEnv)
import System.IO (openFile, IOMode(..), Handle, hClose, stderr, hPutStrLn)
import System.Process (callCommand)

import Data.IORef
import Prelude hiding (last, length)
import Test.Tasty.Bench hiding (env)


scratchDir :: String
scratchDir = "benchmark-tmp/"

outfile :: String
outfile = scratchDir ++ "out.txt"

inFileSmall :: String
inFileSmall = scratchDir ++ "in-10MB.txt"

inFileBig :: String
inFileBig = scratchDir ++ "in-100MB.txt"

data RefHandles = RefHandles
    { smallInH :: Handle
    , bigInH :: Handle
    , outputH :: Handle
    }

data Handles = Handles !Handle !Handle

instance NFData Handles where
    rnf _ = ()

data BenchEnv = BenchEnv
    { href :: IORef RefHandles
    , smallSize :: Int
    , bigSize :: Int
    , nullH :: Handle
    , smallInFile :: String
    , bigInFile :: String
    }

withScaling :: BenchEnv -> String -> String
withScaling env str =
    let factor = round (fromIntegral (bigSize env)
                    / (fromIntegral (smallSize env) :: Double)) :: Int
    in if factor == 1
       then str
       else str ++ " (1/" ++ show factor ++ ")"

getHandles :: BenchEnv -> (RefHandles -> Handles) -> IO Handles
getHandles env mkHandles = do
    r <- readIORef $ href env
    -- close old handles
    hClose $ smallInH r
    hClose $ bigInH r
    hClose $ outputH r

    -- reopen
    smallInHandle <- openFile (smallInFile env) ReadMode
    bigInHandle <- openFile (bigInFile env) ReadMode
    outHandle <- openFile outfile WriteMode

    let refHandles = RefHandles
            { smallInH = smallInHandle
            , bigInH = bigInHandle
            , outputH = outHandle
            }

    -- update
    writeIORef (href env) refHandles
    return $ mkHandles refHandles

mkBenchCommon ::
       NFData b
    => (RefHandles -> Handles)
    -> String
    -> BenchEnv
    -> (Handle -> Handle -> IO b)
    -> Benchmark
mkBenchCommon mkHandles name env action =
     bench name $ nfIO $ do
        -- XXX adds significant cpu time to the benchmarks
        -- tasty-bench should provide a better way to do this
        (Handles h1 h2) <- getHandles env mkHandles
        action h1 h2

mkBench ::
    NFData b => String -> BenchEnv -> (Handle -> Handle -> IO b) -> Benchmark
mkBench name env action = mkBenchCommon useBigH name env action

    where

    useBigH (RefHandles {bigInH = inh, outputH = outh}) = Handles inh outh

mkBenchSmall ::
    NFData b => String -> BenchEnv -> (Handle -> Handle -> IO b) -> Benchmark
mkBenchSmall name env action =
    mkBenchCommon useSmallH (withScaling env name) env action

    where

    useSmallH (RefHandles {smallInH = inh, outputH = outh}) = Handles inh outh

#if !MIN_VERSION_base(4,17,0)
foreign import ccall unsafe "u_iswspace"
  iswspace :: Int -> Int
#endif

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
#if MIN_VERSION_base(4,17,0)
  | otherwise = generalCategory c == Space
#else
  | otherwise = iswspace (ord c) /= 0
#endif
  where
    uc = fromIntegral (ord c) :: Word

{-# INLINE isSp #-}
isSp :: Word8 -> Bool
isSp = isSpace . chr . fromIntegral

smallFileSize :: Int
smallFileSize = 10 * 1024 * 1024

bigFileSize :: Int
bigFileSize = 100 * 1024 * 1024

mkHandleBenchEnv :: IO BenchEnv
mkHandleBenchEnv = do
    r <- lookupEnv "Benchmark_FileSystem_Handle_InputFile"
    (small, big) <-
        case r of
            Just inFileName -> return (inFileName, inFileName)
            Nothing -> do
                -- XXX will this work on windows/msys?
                let cmd infile size =
                        "mkdir -p " ++ scratchDir
                            ++ "; test -e " ++ infile
                            ++ " || { echo \"creating input file " ++ infile
                            ++ "\" && head -c " ++ show size
                            ++ " </dev/urandom >" ++ infile
                            ++ ";}"
                callCommand (cmd inFileSmall smallFileSize)
                callCommand (cmd inFileBig bigFileSize)
                return (inFileSmall, inFileBig)

    hPutStrLn stderr $ "Using small input file: " ++ small
    smallHandle <- openFile small ReadMode

    hPutStrLn stderr $ "Using big input file: " ++ big
    bigHandle <- openFile big ReadMode

    hPutStrLn stderr $ "Using output file: " ++ outfile
    outHandle <- openFile outfile WriteMode
    devNull <- openFile "/dev/null" WriteMode

    ssize <- fromIntegral <$> getFileSize small
    bsize <- fromIntegral <$> getFileSize big

    ref <- newIORef $ RefHandles
        { smallInH = smallHandle
        , bigInH = bigHandle
        , outputH = outHandle
        }

    let env = BenchEnv
            { href = ref
            , smallSize = ssize
            , bigSize = bsize
            , nullH = devNull
            , smallInFile = small
            , bigInFile = big
            }
    return env
