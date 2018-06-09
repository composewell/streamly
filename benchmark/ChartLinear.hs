{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Data.List.Split
import BenchGraph (bgraph, defaultConfig, Config(..), ComparisonStyle(..))
import Control.Exception (handle, catch, SomeException, ErrorCall(..))

main :: IO ()
main = do
    let cfg = defaultConfig
            { outputDir = "charts"
            , comparisonStyle = CompareDelta
            }

        ignoringErr a = catch a (\(ErrorCall err :: ErrorCall) ->
            putStrLn $ "Failed with error:\n" ++ err ++ "\nSkipping.")
    -- bgraph <input> <output> <field in csv file to be plotted>
    -- other interesting fields to plot are:
    -- allocated
    -- bytesCopied
    -- mutatorCpuSeconds
    -- gcCpuSeconds
    ignoringErr $ bgraph "charts/results.csv" "operations" "time" $ cfg
        { chartTitle = Just "Streamly operations (time)"
        , classifyBenchmark = \b ->
                if (not $ "serially/" `isPrefixOf` b)
                   || "/generation" `isInfixOf` b
                   || "/compose" `isInfixOf` b
                   || "/concat" `isSuffixOf` b
                then Nothing
                else Just ("Streamly", last $ splitOn "/" b)
        }

    ignoringErr $ bgraph "charts/results.csv" "generation" "time" $ cfg
        { chartTitle = Just "Stream generation (time)"
        , classifyBenchmark = \b ->
                if "serially/generation" `isPrefixOf` b
                then Just ("Streamly", last $ splitOn "/" b)
                else Nothing
        }

    ignoringErr $ bgraph "charts/results.csv" "composition" "time" $ cfg
        { chartTitle = Just "Streamly composition performance (time)"
        , classifyBenchmark = fmap ("Streamly",) . stripPrefix "serially/compose/"
        }

    ignoringErr $ bgraph "charts/results.csv" "composition-scaling" "time"
        $ cfg
        { chartTitle = Just "Streamly composition scaling (time)"
        , classifyBenchmark = fmap ("Streamly",) . stripPrefix "serially/compose-"
        }
