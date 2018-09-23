{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (handle, catch, SomeException, ErrorCall)
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.List
import Data.List.Split
import Data.Ord (comparing)
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)

import BenchGraph
import Utils

ignoringErr a = catch a (\(_ :: ErrorCall) ->
    putStrLn "Failed. Skipping.")

makeGraphs :: Config -> IO ()
makeGraphs cfg = do
    ignoringErr $ graph inputFile "nested-serial-diff" $ cfg
        { title = Just "Nested serial"
        , classifyBenchmark = \b ->
            let ls = splitOn "/" b
            in case head ls of
                "serially" -> Just (head ls, last ls)
                _ -> Nothing
        }

-- Primitive command line options parsing
data Options = Options
    { genGraphs :: Bool
    }

inputFile :: String
inputFile = "charts/nested/results.csv"

main :: IO ()
main = do
    let cfg = defaultConfig
            { outputDir = Just "charts/nested"
            , presentation = Groups PercentDiff
            }

    args <- getArgs
    res <- runMaybeT $ flip evalStateT args $ do
        x <- shift
        case x of
            Just "--graphs" -> return $ Options { genGraphs = True }
            Nothing         -> return $ Options { genGraphs = False }
            Just str -> do
                liftIO $ putStrLn $ "Unrecognized option " ++ str
                mzero

    case res of
        Nothing -> return ()
        Just Options{..} ->
            if genGraphs
            then makeGraphs cfg
            else
                ignoringErr $ report inputFile Nothing $ cfg
                    { selectBenchmarks =
                          \f ->
                              reverse $ map fst $
                              sortBy (comparing snd) $ f $ ColumnIndex 1
                    }
