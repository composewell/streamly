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

import Prelude hiding (zip)

import Streamly (zipSerially)
import qualified Streamly.Prelude  as S

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Gauge

moduleName :: String
moduleName = "Prelude.ZipSerial"

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

{-# INLINE zip #-}
zip :: Int -> Int -> IO ()
zip count n =
    S.drain $
    S.zipWith (,) (sourceUnfoldrMN count n) (sourceUnfoldrMN count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'zip
inspect $ 'zip `hasNoType` ''SPEC
inspect $ 'zip `hasNoType` ''D.Step
#endif

{-# INLINE zipM #-}
zipM :: Int -> Int -> IO ()
zipM count n =
    S.drain $
    S.zipWithM
        (curry return)
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'zipM
inspect $ 'zipM `hasNoType` ''SPEC
inspect $ 'zipM `hasNoType` ''D.Step
#endif

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining"
        [ benchIOSrc1 "zip (2,x/2)" (zip (value `div` 2))
        , benchIOSrc1 "zipM (2,x/2)" (zipM (value `div` 2))
        ]
    ]

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup "mapping"
        [ benchIOSink value "fmap" $ fmapN zipSerially 1
        ]
    ]

-------------------------------------------------------------------------------
-- Monad outer product
-------------------------------------------------------------------------------

o_1_space_outerProduct :: Int -> [Benchmark]
o_1_space_outerProduct value =
    [ bgroup "monad-outer-product"
        -- XXX needs fixing
        [ benchIO "toNullAp" $ toNullAp value zipSerially
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
            [ o_1_space_joining size
            , o_1_space_mapping size
            , o_1_space_outerProduct size
            ]
        ]

