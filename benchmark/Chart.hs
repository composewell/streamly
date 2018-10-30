{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (handle, catch, SomeException, ErrorCall(..))
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.List
import Data.List.Split
import Data.Ord (comparing)
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)

import BenchShow

------------------------------------------------------------------------------
-- Command line parsing
------------------------------------------------------------------------------

data BenchType = Linear | LinearAsync | LinearRate | Nested | Base

data Options = Options
    { genGraphs :: Bool
    , benchType :: BenchType
    }

defaultOptions = Options False Linear

setGenGraphs val = do
    (args, opts) <- get
    put (args, opts { genGraphs = val })

setBenchType val = do
    (args, opts) <- get
    put (args, opts { benchType = val })

-- Like the shell "shift" to shift the command line arguments
shift :: StateT ([String], Options) (MaybeT IO) (Maybe String)
shift = do
    s <- get
    case s of
        ([], _) -> return Nothing
        (x : xs, opts) -> put (xs, opts) >> return (Just x)

parseBench :: StateT ([String], Options) (MaybeT IO) ()
parseBench = do
    x <- shift
    case x of
        Just "linear" -> setBenchType Linear
        Just "linear-async" -> setBenchType LinearAsync
        Just "linear-rate" -> setBenchType LinearRate
        Just "nested" -> setBenchType Nested
        Just "base" -> setBenchType Base
        Just str -> do
                liftIO $ putStrLn $ "unrecognized benchmark type " <> str
                mzero
        Nothing -> do
                liftIO $ putStrLn "please provide a benchmark type "
                mzero

-- totally imperative style option parsing
parseOptions :: IO (Maybe Options)
parseOptions = do
    args <- getArgs
    runMaybeT $ flip evalStateT (args, defaultOptions) $ do
        x <- shift
        case x of
            Just "--graphs" -> setGenGraphs True
            Just "--benchmark" -> parseBench
            Just str -> do
                liftIO $ putStrLn $ "Unrecognized option " <> str
                mzero
            Nothing -> return ()
        fmap snd get

ignoringErr a = catch a (\(ErrorCall err :: ErrorCall) ->
    putStrLn $ "Failed with error:\n" <> err <> "\nSkipping.")

------------------------------------------------------------------------------
-- Linear composition charts
------------------------------------------------------------------------------

makeLinearGraphs :: Config -> String -> IO ()
makeLinearGraphs cfg inputFile = do
    ignoringErr $ graph inputFile "generation" $ cfg
        { title = Just "Generation"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/generation/"
        }

    ignoringErr $ graph inputFile "elimination" $ cfg
        { title = Just "Elimination"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/elimination/"
        }

    ignoringErr $ graph inputFile "transformation-zip" $ cfg
        { title = Just "Transformation & Zip"
        , classifyBenchmark = \b ->
                if    "serially/transformation/" `isPrefixOf` b
                   || "serially/zip" `isPrefixOf` b
                then Just ("Streamly", last $ splitOn "/" b)
                else Nothing
        }

    ignoringErr $ graph inputFile "filtering" $ cfg
        { title = Just "Filtering"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/filtering/"
        }

    ignoringErr $ graph inputFile "composed-transformation" $ cfg
        { title = Just "Composed Transformation"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/transformationN/"
        }

    ignoringErr $ graph inputFile "composed-filtering"
        $ cfg
        { title = Just "Composed Filtering"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/filteringN/"
        }

    ignoringErr $ graph inputFile "composed-mixed"
        $ cfg
        { title = Just "Composed Mixed"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/composed/"
        }

    ignoringErr $ graph inputFile "iterated"
        $ cfg
        { title = Just "Iterated Ops"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/iterated/"
        }

------------------------------------------------------------------------------
-- Nested composition charts
------------------------------------------------------------------------------

makeNestedGraphs :: Config -> String -> IO ()
makeNestedGraphs cfg inputFile =
    ignoringErr $ graph inputFile "nested-serial" $ cfg
        { title = Just "Nested serial"
        , classifyBenchmark = \b ->
            let ls = splitOn "/" b
            in case head ls of
                "serially" -> Just (head ls, last ls)
                _ -> Nothing
        }

------------------------------------------------------------------------------
-- Charts for parallel streams
------------------------------------------------------------------------------

makeLinearAsyncGraphs :: Config -> String -> IO ()
makeLinearAsyncGraphs cfg inputFile = do
    putStrLn "Not implemented"
    return ()

makeLinearRateGraphs :: Config -> String -> IO ()
makeLinearRateGraphs cfg inputFile = do
    putStrLn "Not implemented"
    return ()

------------------------------------------------------------------------------
-- Charts for base streams
------------------------------------------------------------------------------

makeBaseGraphs :: Config -> String -> IO ()
makeBaseGraphs cfg inputFile = do
    putStrLn "Not implemented"
    return ()

------------------------------------------------------------------------------
-- text reports
------------------------------------------------------------------------------

selectBench :: (SortColumn -> Either String [(String, Double)]) -> [String]
selectBench f =
    reverse
    $ fmap fst
    $ either
      (const $ either error id $ f $ ColumnIndex 0)
      (sortOn snd)
      $ f $ ColumnIndex 1

benchShow Options{..} cfg func inp out =
    if genGraphs
    then func cfg {outputDir = Just out} inp
    else ignoringErr $ report inp Nothing $ cfg
            { selectBenchmarks = selectBench }

showStreamDVsK Options{..} cfg func inp out =
    let cfg' = cfg
            { classifyBenchmark = classify
            , selectBenchmarks = selectBench
            }
    in if genGraphs
       then ignoringErr $ graph inp "streamD-vs-streamK"
                cfg' {outputDir = Just out}
       else ignoringErr $ report inp Nothing cfg'

    where

    classify b
        | "streamD/" `isPrefixOf` b = ("streamD",) <$> stripPrefix "streamD/" b
        | "streamK/" `isPrefixOf` b = ("streamK",) <$> stripPrefix "streamK/" b
        | otherwise = Nothing

main :: IO ()
main = do
    let cfg = defaultConfig { presentation = Groups PercentDiff }
    res <- parseOptions

    case res of
        Nothing -> do
            putStrLn "cannot parse options"
            return ()
        Just opts@Options{..} ->
            case benchType of
                Linear -> benchShow opts cfg makeLinearGraphs
                            "charts/linear/results.csv"
                            "charts/linear"
                LinearAsync -> benchShow opts cfg makeLinearAsyncGraphs
                            "charts/linear-async/results.csv"
                            "charts/linear-async"
                LinearRate -> benchShow opts cfg makeLinearRateGraphs
                            "charts/linear-rate/results.csv"
                            "charts/linear-rate"
                Nested -> benchShow opts cfg makeNestedGraphs
                            "charts/nested/results.csv"
                            "charts/nested"
                Base -> showStreamDVsK opts cfg makeBaseGraphs
                            "charts/base/results.csv"
                            "charts/base"
