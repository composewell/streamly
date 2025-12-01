-- |
-- Module      : Streamly.Benchmark.Data.Unfold.Prelude
-- Copyright   : (c) 2018 Composewell
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import System.IO (Handle, hClose)

import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Unfold.Prelude as UF
import qualified Streamly.FileSystem.Handle as FH

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

readWriteFinallyUnfold :: Handle -> Handle -> IO ()
readWriteFinallyUnfold inh devNull =
    let readEx = UF.finally (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

readWriteBracketUnfold :: Handle -> Handle -> IO ()
readWriteBracketUnfold inh devNull =
    let readEx = UF.bracket return (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

o_1_space_copy_read_exceptions :: BenchEnv -> [Benchmark]
o_1_space_copy_read_exceptions env =
    [ bgroup "exceptions"
       [ mkBenchSmall "UF.finally" env $ \inh _ ->
           readWriteFinallyUnfold inh (nullH env)
       , mkBenchSmall "UF.bracket" env $ \inh _ ->
           readWriteBracketUnfold inh (nullH env)
        ]
    ]

moduleName :: String
moduleName = "Data.Unfold.Prelude"

main :: IO ()
main = do
    env <- mkHandleBenchEnv
    runWithCLIOpts defaultStreamSize (allBenchmarks env)

    where

    allBenchmarks env _size =
        [ bgroup (o_1_space_prefix moduleName)
            $ o_1_space_copy_read_exceptions env
        ]
