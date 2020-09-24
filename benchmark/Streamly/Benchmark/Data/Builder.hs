-- |
-- Module      : Streamly.Benchmark.Prelude
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

module Main where

import Data.Functor.Identity (Identity)
import Streamly.Prelude (SerialT, serially)

import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.Data.Builder as Builder
import qualified Streamly.Internal.Data.Monoid.Builder as MBuilder

import Gauge
import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

moduleName :: String
moduleName = "Data.Builder"

-------------------------------------------------------------------------------
-- Right Associated Appends
-------------------------------------------------------------------------------

{-# INLINE appendListSourceR #-}
appendListSourceR :: Int -> Int -> [Int]
appendListSourceR value n =
    Prelude.foldr (Prelude.++) [] (Prelude.fmap (: []) [n..n+value])

{-# INLINE appendListBuilderSourceR #-}
appendListBuilderSourceR :: Int -> Int -> [Int]
appendListBuilderSourceR value n =
    Builder.close $ foldMap (Builder.bag . (: [])) [n..n+value]

{-# INLINE consListBuilderSourceR #-}
consListBuilderSourceR :: Int -> Int -> [Int]
consListBuilderSourceR value n =
    Builder.close $ foldMap Builder.add [n..n+value]

{-# INLINE appendSourceR #-}
appendSourceR :: Int -> Int -> SerialT m Int
appendSourceR value n = foldMap Stream.yield [n..n+value]

{-# INLINE consStreamBuilderSourceR #-}
consStreamBuilderSourceR :: Int -> Int -> SerialT m Int
consStreamBuilderSourceR value n =
    Builder.close $ foldMap Builder.add [n..n+value]

{-# INLINE appendStreamBuilderSourceR #-}
appendStreamBuilderSourceR :: Int -> Int -> SerialT m Int
appendStreamBuilderSourceR value n =
    Builder.close $ foldMap (Builder.bag . Stream.yield) [n..n+value]

o_1_space_appendR :: Int -> [Benchmark]
o_1_space_appendR value =
    [ bgroup "appendR"
        [ benchPure "singleton lists" (appendListSourceR value) id
        , benchIOSrc serially "singleton streams" (appendSourceR value)
        , benchPure "consed list builders" (consListBuilderSourceR value) id
        , benchPure
            "singleton list builders" (appendListBuilderSourceR value) id
        , benchIOSrc
            serially
            "singleton stream builders"
            (appendStreamBuilderSourceR value)
        , benchIOSrc
            serially
            "consed stream builders"
            (consStreamBuilderSourceR value)
        ]
    ]

-------------------------------------------------------------------------------
-- Left Associated Appends
-------------------------------------------------------------------------------

{-# INLINE appendListSourceL #-}
appendListSourceL :: Int -> Int -> [Int]
appendListSourceL value n =
    Prelude.foldl (Prelude.++) [] (map (: []) [n..n+value])

{-# INLINE appendListBuilderSourceL #-}
appendListBuilderSourceL :: Int -> Int -> [Int]
appendListBuilderSourceL value n =
    Builder.close
        $ Prelude.foldl
            (<>) mempty (map (Builder.bag . (: [])) [n..n+value])

{-# INLINE appendListMonoidBuilderSourceL #-}
appendListMonoidBuilderSourceL :: Int -> Int -> [Int]
appendListMonoidBuilderSourceL value n =
    MBuilder.close
        $ Prelude.foldl (<>) mempty (map (MBuilder.add . (: [])) [n..n+value])

{-# INLINE consListBuilderSourceL #-}
consListBuilderSourceL :: Int -> Int -> [Int]
consListBuilderSourceL value n =
    Builder.close
        $ Prelude.foldl (<>) mempty (map  Builder.add [n..n+value])

{-# INLINE appendSourceL #-}
appendSourceL :: Int -> Int -> SerialT m Int
appendSourceL value n =
    Prelude.foldl
        (Prelude.<>) Stream.nil (Prelude.fmap Stream.yield [n..n+value])

{-# INLINE appendStreamBuilderSourceL #-}
appendStreamBuilderSourceL :: Int -> Int -> SerialT m Int
appendStreamBuilderSourceL value n =
    Builder.close
        $ Prelude.foldl
            (<>)
            mempty
            (map (Builder.bag . Stream.yield) [n..n+value])

{-# INLINE consStreamBuilderSourceL #-}
consStreamBuilderSourceL :: Int -> Int -> SerialT m Int
consStreamBuilderSourceL value n =
    Builder.close
        $ Prelude.foldl (<>) mempty (map Builder.add [n..n+value])

-- Use builder of streams and concat
{-# INLINE streamConcatStreamBuilderSourceL #-}
streamConcatStreamBuilderSourceL :: Monad m => Int -> Int -> SerialT m Int
streamConcatStreamBuilderSourceL value n =
    Stream.concat
        $ Builder.close
        $ Prelude.foldl
            (<>)
            mempty
            (map (Builder.add . Stream.yield) [n..n+value])

{-# INLINE builderConcatStreamBuilderSourceL #-}
builderConcatStreamBuilderSourceL :: Int -> Int -> SerialT Identity Int
builderConcatStreamBuilderSourceL value n =
    Builder.concat
        $ Prelude.foldl
            (<>)
            mempty
            (map (Builder.add . (Stream.yield :: Int -> SerialT Identity Int)) [n..n+value])

o_1_space_appendL :: Int -> [Benchmark]
o_1_space_appendL value =
    [ bgroup "appendL"
        [ benchPure
            "singleton lists (n/100)" (appendListSourceL (value `div` 100)) id
        , benchPure
            "singleton list builders"
            (appendListBuilderSourceL value)
            id
        , benchPure
            "singleton list monoid builders"
            (appendListMonoidBuilderSourceL value)
            id
        , benchPure
            "consed list builders"
            (consListBuilderSourceL value)
            id
        , benchIOSrc
            serially
            "singleton streams (n/100)"
            (appendSourceL (value `div` 100))
        , benchIOSrc
            serially
            "singleton stream builders"
            (appendStreamBuilderSourceL value)
        , benchIOSrc
            serially
            "consed stream builders"
            (consStreamBuilderSourceL value)
        , benchIOSrc
            serially
            "Stream.concat stream builders"
            (streamConcatStreamBuilderSourceL value)
        , benchPureSrc
            "Builder.concat stream builders"
            (builderConcatStreamBuilderSourceL value)
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
        [ bgroup (o_1_space_prefix moduleName) $ Prelude.concat
            [ o_1_space_appendR size
            , o_1_space_appendL size
            ]
        ]
