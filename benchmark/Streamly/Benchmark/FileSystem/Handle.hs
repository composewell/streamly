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

#if __GLASGOW_HASKELL__ >= 800
import System.Directory (getFileSize)
#endif
import System.Environment (lookupEnv)
import System.IO (openFile, IOMode(..))
import System.Process.Typed (shell, runProcess_)
import Prelude hiding (last, length)

import qualified Handle.ReadWrite as RW
import qualified Handle.Read as RO

import Data.IORef
import Gauge hiding (env)
import Streamly.Benchmark.Common
import Handle.Common

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "FileSystem.Handle"

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

smallFileSize :: Int
smallFileSize = 10 * 1024 * 1024

bigFileSize :: Int
bigFileSize = 100 * 1024 * 1024

blockSize :: Int
blockSize = 32768

blockCount :: Int -> Int
blockCount size = (size + blockSize - 1) `div` blockSize

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

main :: IO ()
main = do
    (_, cfg, benches) <- parseCLIOpts defaultStreamSize
    r <- lookupEnv "Benchmark.FileSystem.Handle.InputFile"
    (small, big) <-
        case r of
            Just inFileName -> return (inFileName, inFileName)
            Nothing -> do
                -- XXX will this work on windows/msys?
                let cmd infile size =
                        "mkdir -p " ++ scratchDir
                            ++ "; test -e " ++ infile
                            ++ " || { echo \"creating input file " ++ infile
                            ++ "\" && dd if=/dev/random of=" ++ infile
                            ++ " bs=" ++ show blockSize
                            ++ " count=" ++ show (blockCount size)
                            ++ ";}"
                runProcess_ (shell (cmd inFileSmall smallFileSize))
                runProcess_ (shell (cmd inFileBig bigFileSize))
                return (inFileSmall, inFileBig)

    smallHandle <- openFile small ReadMode
    bigHandle <- openFile big ReadMode
    outHandle <- openFile outfile WriteMode
    devNull <- openFile "/dev/null" WriteMode

#if __GLASGOW_HASKELL__ >= 800
    ssize <- fromIntegral <$> getFileSize small
    bsize <- fromIntegral <$> getFileSize big
#else
    let ssize = smallFileSize
    let bsize = bigFileSize
#endif

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
            }

    runMode (mode cfg) cfg benches (allBenchmarks env)

    where

    allBenchmarks env =
        [ bgroup (o_1_space_prefix moduleName) $ Prelude.concat
            [ RO.allBenchmarks env
            , RW.allBenchmarks env
            ]
        ]
