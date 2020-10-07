-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

import Streamly.Prelude (wSerial, wSerially)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as Internal
import qualified Streamly.Internal.Data.Unfold as UF

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Gauge

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection

import qualified Streamly.Internal.Data.Stream.StreamD as D
#endif

moduleName :: String
moduleName = "Prelude.WSerial"

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup "mapping"
        [ benchIOSink value "fmap" $ fmapN wSerially 1 ]
    ]

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------

{-# INLINE wSerial2 #-}
wSerial2 :: Int -> Int -> IO ()
wSerial2 value n =
    S.drain $ wSerial
        (sourceUnfoldrM (value `div` 2) n)
        (sourceUnfoldrM (value `div` 2) (n + 1))

{-# INLINE interleave2 #-}
interleave2 :: Int -> Int -> IO ()
interleave2 value n =
    S.drain $
    Internal.interleave
        (sourceUnfoldrM (value `div` 2) n)
        (sourceUnfoldrM (value `div` 2) (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'interleave2
inspect $ 'interleave2 `hasNoType` ''SPEC
inspect $ 'interleave2 `hasNoType` ''D.InterleaveState
#endif

{-# INLINE roundRobin2 #-}
roundRobin2 :: Int -> Int -> IO ()
roundRobin2 value n =
    S.drain $
    Internal.roundrobin
        (sourceUnfoldrM (value `div` 2) n)
        (sourceUnfoldrM (value `div` 2) (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'roundRobin2
inspect $ 'roundRobin2 `hasNoType` ''SPEC
inspect $ 'roundRobin2 `hasNoType` ''D.InterleaveState
#endif

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining"
        [ benchIOSrc1 "wSerial (2,x/2)" (wSerial2 value)
        , benchIOSrc1 "interleave (2,x/2)" (interleave2 value)
        , benchIOSrc1 "roundRobin (2,x/2)" (roundRobin2 value)
        ]
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

{-# INLINE concatMapWithWSerial #-}
concatMapWithWSerial :: Int -> Int -> Int -> IO ()
concatMapWithWSerial = concatStreamsWith wSerial

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithWSerial
inspect $ 'concatMapWithWSerial `hasNoType` ''SPEC
#endif

o_1_space_concat :: Int -> [Benchmark]
o_1_space_concat value =
    [ bgroup "concat"
        [ benchIOSrc1
              "concatMapWithWSerial (2,x/2)"
              (concatMapWithWSerial 2 (value `div` 2))
        , benchIOSrc1
              "concatMapWithWSerial (x/2,2)"
              (concatMapWithWSerial (value `div` 2) 2)
        ]
    ]

{-# INLINE concatUnfoldInterleaveRepl4xN #-}
concatUnfoldInterleaveRepl4xN :: Int -> Int -> IO ()
concatUnfoldInterleaveRepl4xN value n =
    S.drain $ Internal.concatUnfoldInterleave
        (UF.lmap return (UF.replicateM 4))
        (sourceUnfoldrM (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatUnfoldInterleaveRepl4xN
-- inspect $ 'concatUnfoldInterleaveRepl4xN `hasNoType` ''SPEC
-- inspect $ 'concatUnfoldInterleaveRepl4xN `hasNoType`
--      ''D.ConcatUnfoldInterleaveState
#endif

{-# INLINE concatUnfoldRoundrobinRepl4xN #-}
concatUnfoldRoundrobinRepl4xN :: Int -> Int -> IO ()
concatUnfoldRoundrobinRepl4xN value n =
    S.drain $ Internal.concatUnfoldRoundrobin
        (UF.lmap return (UF.replicateM 4))
        (sourceUnfoldrM (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatUnfoldRoundrobinRepl4xN
-- inspect $ 'concatUnfoldRoundrobinRepl4xN `hasNoType` ''SPEC
-- inspect $ 'concatUnfoldRoundrobinRepl4xN `hasNoType`
--      ''D.ConcatUnfoldInterleaveState
#endif

o_n_heap_concat :: Int -> [Benchmark]
o_n_heap_concat value =
    [ bgroup "concat"
        [
          -- interleave x/4 streams of 4 elements each. Needs to buffer
          -- proportional to x/4. This is different from WSerial because
          -- WSerial expands slowly because of binary interleave behavior and
          -- this expands immediately because of Nary interleave behavior.
          benchIOSrc1
              "concatUnfoldInterleaveRepl (x/4,4)"
              (concatUnfoldInterleaveRepl4xN value)
        , benchIOSrc1
              "concatUnfoldRoundrobinRepl (x/4,4)"
              (concatUnfoldRoundrobinRepl4xN value)
        ]
    ]

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

o_1_space_outerProduct :: Int -> [Benchmark]
o_1_space_outerProduct value =
    [ bgroup "monad-outer-product"
        [ benchIO "toNullAp" $ toNullAp value wSerially
        , benchIO "toNullM" $ toNullM value wSerially
        , benchIO "toNullM3" $ toNullM3 value wSerially
        , benchIO "filterAllOutM" $ filterAllOutM value wSerially
        , benchIO "filterAllInM" $ filterAllInM value wSerially
        , benchIO "filterSome" $ filterSome value wSerially
        , benchIO "breakAfterSome" $ breakAfterSome value wSerially
        ]
    ]

o_n_space_outerProduct :: Int -> [Benchmark]
o_n_space_outerProduct value =
    [ bgroup
        "monad-outer-product"
        [ benchIO "toList" $ toListM value wSerially
        , benchIO "toListSome" $ toListSome value wSerially
        ]
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    value `seq` runMode (mode cfg) cfg benches (allBenchmarks value)

    where

    allBenchmarks size =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_mapping size
            , o_1_space_joining size
            , o_1_space_concat size
            , o_1_space_outerProduct size
            ]
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_concat size)
        , bgroup (o_n_space_prefix moduleName) (o_n_space_outerProduct size)
        ]
