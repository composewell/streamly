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
        -- GHC-9.6 requires 32M heap vs 16M for GHC-9.4
        | "o-1-sp" `isInfixOf` benchName = "-K36K -M32M"
        | "o-n-h" `isInfixOf` benchName = "-K36K -M32M"
        -- GHC-9.6 requires 32M heap vs 16M for GHC-9.4
        | "o-n-st" `isInfixOf` benchName = "-K1M -M32M"
        | "o-n-sp" `isInfixOf` benchName = "-K1M -M32M"
        | otherwise = ""

    exeSpecific
        | "Prelude.Concurrent" `isSuffixOf` exeName = "-K512K -M384M"
        | otherwise = ""

    benchSpecific
        -- GHC-9.6 requires 64M, earlier it was 16M
        | "Data.Fold/o-n-heap.key-value.toHashMapIO (max buckets) sum" == benchName =
            "-M64M"

        -- This is required only for the --long case because we allocate all
        -- the arrays upfront. depends on the size of the stream.
        | "Data.Parser/o-1-space" `isPrefixOf` benchName =
            "-M128M"

        -----------------------------------------------------------------------

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

        -- This module is dev only now, and can be removed at some point
        | "Data.Stream.StreamDK/o-1-space.grouping.classifySessionsOf"
            `isPrefixOf` benchName = "-K512K"
        | "Data.Stream.StreamDK/o-n-space.foldr.foldrM/"
            `isPrefixOf` benchName = "-K4M"
        | "Data.Stream.StreamDK/o-n-space.iterated."
            `isPrefixOf` benchName = "-K4M -M64M"
        -- GHC 9.4.4 requires 4M
        | "Data.Stream.StreamDK/o-n-space.traversable."
            `isPrefixOf` benchName = "-K4M"
        -- GHC-9.6 requires 64M, earlier it was 32M
        | "Data.Stream.StreamDK/o-n-heap.buffered.readsPrec pure streams" == benchName =
            "-M64M"
        -- GHC-9.6 requires 64M, earlier it was 32M
        | "Data.Stream.StreamDK/o-n-heap.buffered.showPrec Haskell lists" == benchName =
            "-M64M"

        -----------------------------------------------------------------------

        | "Data.StreamD/o-n-space.elimination.toList" == benchName =
            "-K2M"
        | "Data.StreamK/o-n-space.elimination.toList" == benchName =
            "-K2M"

        -----------------------------------------------------------------------

        | "Data.Stream/o-1-space.grouping.classifySessionsOf"
            `isPrefixOf` benchName = "-K512K"
        | "Data.Stream/o-n-space.foldr.foldrM/"
            `isPrefixOf` benchName = "-K4M"
        | "Data.Stream/o-n-space.iterated."
            `isPrefixOf` benchName = "-K4M"

        -- GHC-9.6 requires 64M, earlier it was 32M
        | "Data.Stream/o-n-heap.buffered.showPrec Haskell lists" == benchName =
            "-M64M"
        -- GHC-9.6 requires 64M, earlier it was 32M
        | "Data.Stream/o-n-heap.buffered.readsPrec pure streams" == benchName =
            "-M64M"

        | "Data.Stream.ConcurrentEager/o-n-heap.monad-outer-product.toNullAp"
            `isPrefixOf` benchName = "-M1024M"
        | "Data.Stream.ConcurrentEager/o-1-space."
            `isPrefixOf` benchName = "-M128M"

        | "Data.Stream.ConcurrentOrdered/o-1-space.concat-foldable.foldMapWith"
            `isPrefixOf` benchName = "-K128K"

        ----------------------------------------------------------------------

        | "Data.Array" `isPrefixOf` benchName
             && "/o-1-space.generation.read" `isSuffixOf` benchName = "-M32M"
        -- XXX GHC 9.6 onwards needs 64M, earlier it was 32M
        | "Data.Array" `isPrefixOf` benchName
             && "/o-1-space.generation.show" `isSuffixOf` benchName = "-M64M"
        -- XXX GHC 9.6 onwards needs 64M, earlier it was 32M
        | "Data.Array.Generic/o-1-space.transformationX4.map"
            `isPrefixOf` benchName = "-M64M"

        -- XXX For --long option, need to check why so much heap is required.
        -- Note, if we remove the chunked stream module we need to keep the
        -- chunked stream benchmarks in the stream module.
        | "Data.Array.Stream/o-1-space"
            `isPrefixOf` benchName = "-K4M -M512M"
        -- XXX Takes up to 160MB heap for --long, we use chunked stream for
        -- this, so the reason may be related to chunked streams.
        | "Data.ParserK/o-1-space"
            `isPrefixOf` benchName = "-K4M -M256M"

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
