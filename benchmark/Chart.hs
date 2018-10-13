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
    ignoringErr $ graph inputFile "operations" $ cfg
        { title = Just "Streamly operations"
        , classifyBenchmark = \b ->
                if not ("serially/" `isPrefixOf` b)
                   || "/generation" `isInfixOf` b
                   || "/compose" `isInfixOf` b
                   || "/concat" `isSuffixOf` b
                then Nothing
                else Just ("Streamly", last $ splitOn "/" b)
        }

    ignoringErr $ graph inputFile "generation" $ cfg
        { title = Just "Stream generation"
        , classifyBenchmark = \b ->
                if "serially/generation" `isPrefixOf` b
                then Just ("Streamly", last $ splitOn "/" b)
                else Nothing
        }

    ignoringErr $ graph inputFile "composition" $ cfg
        { title = Just "Streamly composition performance"
        , classifyBenchmark = fmap ("Streamly",) . stripPrefix "serially/compose/"
        }

    ignoringErr $ graph inputFile "composition-scaling"
        $ cfg
        { title = Just "Streamly composition scaling"
        , classifyBenchmark = fmap ("Streamly",) . stripPrefix "serially/compose-"
        }

------------------------------------------------------------------------------
-- Nested composition charts
------------------------------------------------------------------------------

makeNestedGraphs :: Config -> String -> IO ()
makeNestedGraphs cfg inputFile =
    ignoringErr $ graph inputFile "nested-serial-diff" $ cfg
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

benchShow Options{..} cfg func inp out =
    if genGraphs
    then func cfg {outputDir = Just out} inp
    else
        ignoringErr $ report inp Nothing $ cfg
            { selectBenchmarks =
                  \f ->
                        reverse
                      $ fmap fst
                      $ either
                          (const $ either error id $ f $ ColumnIndex 0)
                          (sortOn snd)
                          $ f $ ColumnIndex 1
            }

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
                Base -> benchShow opts cfg makeBaseGraphs
                            "charts/base/results.csv"
                            "charts/base"
