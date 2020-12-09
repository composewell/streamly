-- |
-- Module      : Streamly.Benchmark.FileSystem.Handle
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

#if __GLASGOW_HASKELL__ >= 800
#endif

import Streamly.Benchmark.Common.Handle (mkHandleBenchEnv)

import qualified Handle.ReadWrite as RW
import qualified Handle.Read as RO

import Gauge hiding (env)
import Prelude hiding (last, length)
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "FileSystem.Handle"

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

main :: IO ()
main = do
    (_, cfg, benches) <- parseCLIOpts defaultStreamSize
    env <- mkHandleBenchEnv
    runMode (mode cfg) cfg benches (allBenchmarks env)

    where

    allBenchmarks env =
        [ bgroup (o_1_space_prefix moduleName) $ Prelude.concat
            [ RO.allBenchmarks env
            , RW.allBenchmarks env
            ]
        ]
