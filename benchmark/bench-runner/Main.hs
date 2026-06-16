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
             && "/o-1-space.read" `isSuffixOf` benchName = "-M64M"
        -- XXX GHC 9.6 onwards needs 64M, earlier it was 32M
        | "Data.Array" `isPrefixOf` benchName
             && "/o-1-space.show" `isSuffixOf` benchName = "-M64M"
        -- XXX For --long option, need to check why so much heap is required.
        | "Data.Array/o-1-space.foldBreak"
            `isPrefixOf` benchName = "-K4M -M512M"
        | "Data.Array/o-1-space.parseBreak"
            `isPrefixOf` benchName = "-K4M -M512M"
        -- XXX GHC 9.6 onwards needs 64M, earlier it was 32M
        | "Data.Array.Generic/o-1-space.mapX4"
            `isPrefixOf` benchName = "-M64M"


        ----------------------------------------------------------------------

        -- GHC-9.6 requires 64M, earlier it was 16M
        | "Data.Fold.Prelude/o-n-heap.toHashMapIO (max buckets) sum"
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

        | "Data.Stream/o-1-space.classifySessionsOf"
            `isPrefixOf` benchName = "-K512K"

        -- GHC-9.6 requires 64M, earlier it was 32M
        | "Data.Stream/o-n-heap.showsPrec Haskell lists"
            == benchName = "-M64M"
        -- GHC-9.6 requires 64M, earlier it was 32M
        | "Data.Stream/o-n-heap.readsPrec pure streams"
            == benchName = "-M64M"

        | "Data.Stream/o-n-space.foldrM/"
            `isPrefixOf` benchName = "-K4M"
        | "Data.Stream/o-n-space.iterated/"
            `isPrefixOf` benchName = "-K4M"
        | "Data.Stream/o-n-space.toList"
            `isPrefixOf` benchName = "-K2M"

        -----------------------------------------------------------------------

        | "Data.StreamK/o-n-space.toList"
            == benchName = "-K2M"

        ----------------------------------------------------------------------
        -- Concurrent streams
        ----------------------------------------------------------------------

        | "Data.Stream.ConcurrentInterleaved/o-n-heap.monad3"
            `isPrefixOf` benchName = "-M128M"

        | "Data.Stream.ConcurrentEager/o-1-space."
            `isPrefixOf` benchName = "-M128M"

        | "Data.Stream.ConcurrentEager/o-n-heap.monad"
            `isPrefixOf` benchName = "-M500M"

        | "Data.Stream.ConcurrentEager/o-n-heap.parCross"
            `isPrefixOf` benchName = "-M500M"

        | "Data.Stream.ConcurrentOrdered/o-1-space.foldMapWith"
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
