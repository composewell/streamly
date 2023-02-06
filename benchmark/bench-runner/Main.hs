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
        | "Prelude.Parallel/o-n-heap.mapping.mapM" == benchName = "-M256M"
        | "Prelude.Parallel/o-n-heap.monad-outer-product."
             `isPrefixOf` benchName = "-M256M"
        | "Prelude.Parallel/o-n-space.monad-outer-product."
             `isPrefixOf` benchName = "-K2M -M256M"
        | "Prelude.Rate/o-1-space." `isPrefixOf` benchName = "-K128K"
        | "Prelude.Rate/o-1-space.asyncly." `isPrefixOf` benchName = "-K128K"
        | "Prelude.WSerial/o-n-space." `isPrefixOf` benchName = "-K4M"
        | "Prelude.Async/o-n-space.monad-outer-product." `isPrefixOf` benchName =
            "-K4M"
        | "Prelude.Ahead/o-1-space.monad-outer-product." `isPrefixOf` benchName =
            "-K128K -M32M"
        | "Prelude.Ahead/o-1-space." `isPrefixOf` benchName = "-K128K"
        | "Prelude.Ahead/o-n-space.monad-outer-product." `isPrefixOf` benchName =
            "-K4M"
        | "Prelude.WAsync/o-n-heap.monad-outer-product.toNull3" == benchName =
            "-M64M"
        | "Prelude.WAsync/o-n-space.monad-outer-product." `isPrefixOf` benchName =
            "-K4M"

        -----------------------------------------------------------------------

        | "Data.Stream.StreamD/o-n-space.elimination.toList" == benchName =
            "-K2M"
        | "Data.Stream.StreamK/o-n-space.elimination.toList" == benchName =
            "-K2M"

        -----------------------------------------------------------------------

        | "Data.Stream/o-1-space.grouping.classifySessionsOf"
            `isPrefixOf` benchName = "-K512K"
        | "Data.Stream/o-n-space.foldr.foldrM/"
            `isPrefixOf` benchName = "-K4M"
        | "Data.Stream/o-n-space.iterated."
            `isPrefixOf` benchName = "-K4M"

        | "Data.Stream.StreamDK/o-1-space.grouping.classifySessionsOf"
            `isPrefixOf` benchName = "-K512K"
        | "Data.Stream.StreamDK/o-n-space.foldr.foldrM/"
            `isPrefixOf` benchName = "-K4M"
        | "Data.Stream.StreamDK/o-n-space.iterated."
            `isPrefixOf` benchName = "-K4M -M64M"
        | "Data.Stream.StreamDK/o-n-space.traversable."
            `isPrefixOf` benchName = "-K2M"

        | "Data.Stream.ConcurrentInterleaved/o-1-space.monad-outer-product.toNullAp"
            `isPrefixOf` benchName = "-M32M"

        | "Data.Stream.ConcurrentEager/o-1-space.monad-outer-product.toNullAp"
            `isPrefixOf` benchName = "-M768M"
        | "Data.Stream.ConcurrentEager/o-1-space."
            `isPrefixOf` benchName = "-M128M"

        ----------------------------------------------------------------------

        | "Data.Array" `isPrefixOf` benchName
             && "/o-1-space.generation.read" `isSuffixOf` benchName = "-M32M"
        | "Data.Array" `isPrefixOf` benchName
             && "/o-1-space.generation.show" `isSuffixOf` benchName = "-M32M"
        | "Data.Array.Generic/o-1-space.transformationX4.map"
            `isPrefixOf` benchName = "-M32M"

        -----------------------------------------------------------------------

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
