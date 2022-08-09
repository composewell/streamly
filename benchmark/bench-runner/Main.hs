module Main (main) where

import BenchRunner (mainWith)
import BuildLib (Quickness(..))
import Control.Applicative ((<|>))
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)

import Targets (targets)

rtsOpts :: String -> String -> String
rtsOpts exeName benchName0 = unwords [general, exeSpecific, benchSpecific]

    where

    -- Drop All.
    benchName = drop 4 benchName0
    general
        | "o-1-sp" `isInfixOf` benchName = "-K36K -M16M"
        | "o-n-h" `isInfixOf` benchName = "-K36K -M32M"
        | "o-n-st" `isInfixOf` benchName = "-K1M -M16M"
        | "o-n-sp" `isInfixOf` benchName = "-K1M -M32M"
        | otherwise = ""
    exeSpecific
        | "Prelude.Concurrent" `isSuffixOf` exeName = "-K512K -M384M"
        | otherwise = ""
    benchSpecific
        | "Data.Stream.StreamD/o-n-space.elimination.toList" == benchName =
            "-K2M"
        | "Data.Stream.StreamK/o-n-space.elimination.toList" == benchName =
            "-K2M"
        | "Prelude.Parallel/o-n-heap.mapping.mapM" == benchName = "-M256M"
        | "Prelude.Parallel/o-n-heap.monad-outer-product."
             `isPrefixOf` benchName = "-M256M"
        | "Prelude.Parallel/o-n-space.monad-outer-product."
             `isPrefixOf` benchName = "-K2M -M256M"
        | "Prelude.Rate/o-1-space." `isPrefixOf` benchName = "-K128K"
        | "Prelude.Rate/o-1-space.asyncly." `isPrefixOf` benchName = "-K128K"
        | "Data.Stream/o-1-space.mixed.sum-product-fold" == benchName =
            "-K64M"
        | "Data.Stream/o-n-heap.grouping.classifySessionsOf"
            `isPrefixOf` benchName = "-K1M -M32M"
        | "Data.Stream/o-n-heap.Functor." `isPrefixOf` benchName =
            "-K4M -M32M"
        | "Data.Stream/o-n-heap.transformer." `isPrefixOf` benchName =
            "-K8M -M64M"
        | "Data.Stream/o-n-space.Functor." `isPrefixOf` benchName =
            "-K4M -M64M"
        | "Data.Stream/o-n-space.Applicative." `isPrefixOf` benchName =
            "-K8M -M128M"
        | "Data.Stream/o-n-space.Monad." `isPrefixOf` benchName =
            "-K8M -M64M"
        | "Data.Stream/o-n-space.grouping." `isPrefixOf` benchName = ""
        | "Data.Stream/o-n-space." `isPrefixOf` benchName = "-K4M"
        | "Prelude.WSerial/o-n-space." `isPrefixOf` benchName = "-K4M"
        | "Prelude.Async/o-n-space.monad-outer-product." `isPrefixOf` benchName =
            "-K4M"
        | "Prelude.Ahead/o-n-space.monad-outer-product." `isPrefixOf` benchName =
            "-K4M"
        | "Prelude.Ahead/o-1-space." `isPrefixOf` benchName = "-K128K"
        | "Prelude.WAsync/o-n-heap.monad-outer-product.toNull3" == benchName =
            "-M64M"
        | "Prelude.WAsync/o-n-space.monad-outer-product." `isPrefixOf` benchName =
            "-K4M"
        | "Data.Parser.ParserD/o-1-space.some" == benchName = "-K8M"
        | "Data.Parser/o-1-space.some" == benchName = "-K8M"
        | "Data.Parser.ParserD/o-1-space.manyTill" == benchName = "-K4M"
        | "Data.Parser/o-1-space.manyTill" == benchName = "-K4M"
        | "Data.Parser/o-n-heap.manyAlt" == benchName = "-K4M -M128M"
        | "Data.Parser/o-n-heap.someAlt" == benchName = "-K4M -M128M"
        | "Data.Parser/o-n-heap.choice" == benchName = "-K16M -M32M"
        | "Data.Parser.ParserK/o-n-heap.manyAlt" == benchName = "-K4M -M128M"
        | "Data.Parser.ParserK/o-n-heap.someAlt" == benchName = "-K4M -M128M"
        | "Data.Parser.ParserK/o-n-heap.sequence" == benchName = "-M64M"
        | "Data.Parser.ParserK/o-n-heap.sequenceA" == benchName = "-M64M"
        | "Data.SmallArray.o-1-sp" `isPrefixOf` benchName = "-K128K"
        | "Data.Array" `isPrefixOf` benchName
             && "/o-1-space.generation.show" `isSuffixOf` benchName = "-M32M"
        | "Data.Array/o-1-space.transformationX4.map" == benchName = "-M32M"
        | "Data.Array.Unboxed/o-1-space.elimination.foldable.foldl"
             `isPrefixOf` benchName = "-K8M"
        | "Data.Array.Unboxed/o-1-space.elimination.foldable.sum" == benchName =
            "-K8M"
        | "Unicode.Char/o-1-space." `isPrefixOf` benchName = "-M32M"
        | otherwise = ""

speedOpts :: String -> String -> Maybe Quickness
speedOpts exeName benchName0 = exeSpecific <|> benchSpecific

    where

    -- slowestOf Quicker _ = Quicker
    -- slowestOf _ Quicker = Quicker
    -- slowestOf _ _ = SuperQuick

    -- Drop All.
    benchName = drop 4 benchName0
    exeSpecific
        | "Prelude.Concurrent" == exeName = Just SuperQuick
        | "Prelude.Rate" == exeName = Just SuperQuick
        | "Prelude.Adaptive" == exeName = Just SuperQuick
        | otherwise = Nothing
    benchSpecific
        | "Prelude.Parallel/o-n-heap.mapping.mapM" == benchName =
            Just SuperQuick
        | "Prelude.Parallel/o-n-heap.monad-outer-product."
             `isPrefixOf` benchName = Just SuperQuick
        | "Prelude.Parallel.o-n-space.monad-outer-product."
             `isPrefixOf` benchName = Just SuperQuick
        | "Prelude.Parallel/o-n-heap.generation." `isPrefixOf` benchName =
            Just Quicker
        | "Prelude.Parallel/o-n-heap.mapping." `isPrefixOf` benchName =
            Just Quicker
        | "Prelude.Parallel/o-n-heap.concat-foldable." `isPrefixOf` benchName =
            Just Quicker
        | "Prelude.Async/o-1-space.monad-outer-product." `isPrefixOf` benchName =
            Just Quicker
        | "Prelude.Async/o-n-space.monad-outer-product." `isPrefixOf` benchName =
            Just Quicker
        | "Prelude.Ahead/o-1-space.monad-outer-product." `isPrefixOf` benchName =
            Just Quicker
        | "Prelude.Ahead/o-n-space.monad-outer-product." `isPrefixOf` benchName =
            Just Quicker
        | "Prelude.WAsync/o-n-heap.monad-outer-product." `isPrefixOf` benchName =
            Just Quicker
        | "Prelude.WAsync/o-n-space.monad-outer-product." `isPrefixOf` benchName =
            Just Quicker
        | "FileSystem.Handle." `isPrefixOf` benchName = Just Quicker
        | otherwise = Nothing

main :: IO ()
main = mainWith targets speedOpts rtsOpts
