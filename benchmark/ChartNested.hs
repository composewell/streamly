{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Data.List.Split
import BenchGraph (bgraph, defaultConfig, Config(..), ComparisonStyle(..))
import Control.Exception (handle, catch, SomeException, ErrorCall)

main :: IO ()
main = do
    let cfg = defaultConfig
            { outputDir = "charts"
            , comparisonStyle = CompareFull
            }

        ignoringErr a = catch a (\(_ :: ErrorCall) ->
            putStrLn "Failed. Skipping.")
    -- bgraph <input> <output> <field in csv file to be plotted>
    -- other interesting fields to plot are:
    -- allocated
    -- bytesCopied
    -- mutatorCpuSeconds
    -- gcCpuSeconds
    ignoringErr $ bgraph "charts/results.csv" "nested-ops" "time" $ cfg
        { chartTitle = Just "Nested operations (time)"
        , classifyBenchmark = \b ->
            let ls = splitOn "/" b
            in case head ls of
                "linear" -> Nothing
                _ -> Just (head ls, last ls)
        , sortBenchmarks = nub
        , comparisonStyle = CompareFull
        }

    ignoringErr $ bgraph "charts/results.csv" "nested-serial-comparative" "time" $ cfg
        { chartTitle = Just "Nested serial diff (time)"
        , classifyBenchmark = \b ->
            let ls = splitOn "/" b
            in case head ls of
                "serially" -> Just (head ls, last ls)
                _ -> Nothing
        , sortBenchmarks = nub
        , comparisonStyle = CompareDelta
        }
