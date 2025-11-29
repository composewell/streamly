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
        -- | "Data.Stream.ConcurrentEager" `isSuffixOf` exeName = "-K512K -M384M"
        -- placeholder to remind usage of exeName
        | "abc" `isSuffixOf` exeName = ""
        | otherwise = ""

    benchSpecific

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

        ----------------------------------------------------------------------

        -- GHC-9.6 requires 64M, earlier it was 16M
        | "Data.Fold/o-n-heap.key-value.toHashMapIO (max buckets) sum"
            == benchName = "-M64M"

        ----------------------------------------------------------------------

        -- This is required only for the --long case because we allocate all
        -- the arrays upfront. depends on the size of the stream.
        | "Data.Parser/o-1-space"
            `isPrefixOf` benchName = "-M128M"

        -- XXX Takes up to 160MB heap for --long, we use chunked stream for
        -- this, so the reason may be related to chunked streams.
        | "Data.ParserK/o-1-space"
            `isPrefixOf` benchName = "-K4M -M256M"
        | "Data.ParserK.Chunked/o-1-space"
            `isPrefixOf` benchName = "-K4M -M256M"
        | "Data.ParserK.Chunked.Generic/o-1-space"
            `isPrefixOf` benchName = "-K4M -M256M"

        -----------------------------------------------------------------------

        | "Data.Stream/o-1-space.grouping.classifySessionsOf"
            `isPrefixOf` benchName = "-K512K"

        -- GHC-9.6 requires 64M, earlier it was 32M
        | "Data.Stream/o-n-heap.buffered.showPrec Haskell lists"
            == benchName = "-M64M"
        -- GHC-9.6 requires 64M, earlier it was 32M
        | "Data.Stream/o-n-heap.buffered.readsPrec pure streams"
            == benchName = "-M64M"

        | "Data.Stream/o-n-space.foldr.foldrM/"
            `isPrefixOf` benchName = "-K4M"
        | "Data.Stream/o-n-space.iterated."
            `isPrefixOf` benchName = "-K4M"
        | "Data.Stream/o-n-space.toList.toList"
            `isPrefixOf` benchName = "-K2M"
        | "Data.Stream/o-n-space.Monad.toList"
            `isPrefixOf` benchName = "-K2M"

        -----------------------------------------------------------------------

        | "Data.StreamK/o-n-space.elimination.toList"
            == benchName = "-K2M"
        -- XXX Memory required for these has increased in streamly-core 0.3
        | "Data.StreamK/o-1-space.list.nested"
            `isPrefixOf` benchName = "-M500M"

        ----------------------------------------------------------------------
        -- Concurrent streams
        ----------------------------------------------------------------------

        | "Data.Stream.ConcurrentInterleaved/o-n-heap.cross-product.monad3"
            `isPrefixOf` benchName = "-M128M"

        | "Data.Stream.ConcurrentEager/o-1-space."
            `isPrefixOf` benchName = "-M128M"

        | "Data.Stream.ConcurrentEager/o-n-heap.cross-product"
            `isPrefixOf` benchName = "-M500M"

        | "Data.Stream.ConcurrentOrdered/o-1-space.concat-foldable.foldMapWith"
            `isPrefixOf` benchName = "-K128K"

        -----------------------------------------------------------------------

        | "Unicode.Char/o-1-space." `isPrefixOf` benchName = "-M32M"

        | otherwise = ""

speedOpts :: String -> String -> Maybe Quickness
speedOpts exeName benchName0 = exeSpecific <|> benchSpecific

    where

    -- slowestOf Quicker _ = Quicker
    -- slowestOf _ Quicker = Quicker
    -- slowestOf _ _ = SuperQuick

    -- Drop the "All." prefix
    benchName = drop 4 benchName0
    exeSpecific
        | "Data.Stream.ConcurrentThreadHeavy" == exeName = Just SuperQuick
        | "Data.Stream.Rate" == exeName = Just SuperQuick
        | "Data.Stream.Adaptive" == exeName = Just SuperQuick
        | otherwise = Nothing
    benchSpecific
        | "-maxBuffer-1" `isInfixOf` benchName = Just SuperQuick
        | "FileSystem.Handle." `isPrefixOf` benchName = Just Quicker
        | otherwise = Nothing

main :: IO ()
main = mainWith targets speedOpts rtsOpts
