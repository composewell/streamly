{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (handle, catch, SomeException, ErrorCall(..))
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Char (toLower)
import Data.Function (on, (&))
import Data.List
import Data.List.Split
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)

import BenchShow

------------------------------------------------------------------------------
-- Command line parsing
------------------------------------------------------------------------------

data BenchType
    = Linear
    | LinearAsync
    | LinearRate
    | NestedConcurrent
    | FileIO
    | Concurrent
    | Parallel
    | Adaptive
    | Compare String
    | Standard String
    deriving Show

data Options = Options
    { genGraphs :: Bool
    , benchType :: BenchType
    } deriving Show

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
        Just "nested-concurrent" -> setBenchType NestedConcurrent
        Just "fileio" -> setBenchType FileIO
        Just "concurrent" -> setBenchType Concurrent
        Just "parallel" -> setBenchType Parallel
        Just "adaptive" -> setBenchType Adaptive
        Just str | "_cmp" `isSuffixOf` str -> setBenchType (Compare str)
        Just str -> setBenchType (Standard str)
        Nothing -> do
                liftIO $ putStrLn "please provide a benchmark type "
                mzero

-- totally imperative style option parsing
parseOptions :: IO (Maybe Options)
parseOptions = do
    args <- getArgs
    runMaybeT $ flip evalStateT (args, defaultOptions) $ do
        parseLoop
        fmap snd get

    where

    parseOpt opt =
        case opt of
            "--graphs"     -> setGenGraphs True
            "--benchmark"  -> parseBench
            str -> do
                liftIO $ putStrLn $ "Unrecognized option " <> str
                mzero

    parseLoop = do
        next <- shift
        case next of
            Just opt -> parseOpt opt >> parseLoop
            Nothing -> return ()

ignoringErr a = catch a (\(ErrorCall err :: ErrorCall) ->
    putStrLn $ "Failed with error:\n" <> err <> "\nSkipping.")

------------------------------------------------------------------------------
-- Linear composition charts
------------------------------------------------------------------------------

makeLinearGraphs :: Config -> String -> IO ()
makeLinearGraphs cfg@Config{..} inputFile = do
    ignoringErr $ graph inputFile "generation" $ cfg
        { title = (++) <$> title <*> Just " generation"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/generation/"
        }

    ignoringErr $ graph inputFile "elimination" $ cfg
        { title = (++) <$> title <*> Just " Elimination"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/elimination/"
        }

    ignoringErr $ graph inputFile "transformation-zip" $ cfg
        { title = (++) <$> title <*> Just " Transformation & Zip"
        , classifyBenchmark = \b ->
                if    "serially/transformation/" `isPrefixOf` b
                   || "serially/zipping" `isPrefixOf` b
                then Just ("Streamly", last $ splitOn "/" b)
                else Nothing
        }

    ignoringErr $ graph inputFile "filtering" $ cfg
        { title = (++) <$> title <*> Just " Filtering"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/filtering/"
        }

    ignoringErr $ graph inputFile "transformationX4" $ cfg
        { title = (++) <$> title <*> Just " Transformation x 4"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/transformationX4/"
        }

    ignoringErr $ graph inputFile "filteringX4"
        $ cfg
        { title = (++) <$> title <*> Just " Filtering x 4"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/filteringX4/"
        }

    ignoringErr $ graph inputFile "mixedX4"
        $ cfg
        { title = (++) <$> title <*> Just " Mixed x 4"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/mixedX4/"
        }

    ignoringErr $ graph inputFile "iterated"
        $ cfg
        { title = Just "iterate 10,000 times over 10 elems"
        , classifyBenchmark =
            fmap ("Streamly",) . stripPrefix "serially/iterated/"
        }

------------------------------------------------------------------------------
-- Stream type based comparison charts
------------------------------------------------------------------------------

makeStreamComparisonGraphs :: String -> [String] -> Config -> String -> IO ()
makeStreamComparisonGraphs outputPrefix benchPrefixes cfg inputFile =
    ignoringErr $ graph inputFile outputPrefix $ cfg
        { presentation = Groups Absolute
        , classifyBenchmark = classifyNested
        , selectGroups = \gs ->
            groupBy ((==) `on` snd) gs
            & fmap (\xs -> mapMaybe (\x -> (x,) <$> lookup x xs) benchPrefixes)
            & concat
        }

    where

    classifyNested b
        | "serially/" `isPrefixOf` b =
            ("serially",) <$> stripPrefix "serially/" b
        | "asyncly/" `isPrefixOf` b =
            ("asyncly",) <$> stripPrefix "asyncly/" b
        | "wAsyncly/" `isPrefixOf` b =
            ("wAsyncly",) <$> stripPrefix "wAsyncly/" b
        | "aheadly/" `isPrefixOf` b =
            ("aheadly",) <$> stripPrefix "aheadly/" b
        | "parallely/" `isPrefixOf` b =
            ("parallely",) <$> stripPrefix "parallely/" b
        | otherwise = Nothing

linearAsyncPrefixes = ["asyncly", "wAsyncly", "aheadly", "parallely"]
nestedBenchPrefixes = ["serially"] ++ linearAsyncPrefixes

------------------------------------------------------------------------------
-- Generic
------------------------------------------------------------------------------

makeGraphs :: String -> Config -> String -> IO ()
makeGraphs name cfg@Config{..} inputFile =
    ignoringErr $ graph inputFile name cfg

------------------------------------------------------------------------------
-- Arrays
------------------------------------------------------------------------------

showComparisons Options{..} cfg inp out =
    let cfg1 = cfg { classifyBenchmark = classifyComparison }
     in if genGraphs
        then ignoringErr $ graph inp "comparison"
                cfg1 { outputDir = Just out
                     , presentation = Groups Absolute
                     }
        else ignoringErr $ report inp Nothing cfg1

    where

    dropComponent = dropWhile (== '/') . dropWhile (/= '/')

    classifyComparison b = Just $
        ( takeWhile (/= '/') b
        , dropComponent b
        )

------------------------------------------------------------------------------
-- text reports
------------------------------------------------------------------------------

selectBench
    :: (SortColumn -> Maybe GroupStyle -> Either String [(String, Double)])
    -> [String]
selectBench f =
    reverse
    $ fmap fst
    $ either
      (const $ either error (sortOn snd) $ f (ColumnIndex 0) (Just PercentDiff))
      (sortOn snd)
      $ f (ColumnIndex 1) (Just PercentDiff)

benchShow Options{..} cfg func inp out =
    if genGraphs
    then func cfg {outputDir = Just out} inp
    else ignoringErr $ report inp Nothing cfg

main :: IO ()
main = do
    let cfg = defaultConfig
            { presentation = Groups PercentDiff
            , selectBenchmarks = selectBench
            , selectFields = filter
                ( flip elem ["time" , "mean"
                            , "maxrss", "cputime"
                            ]
                . map toLower
                )
            }
    res <- parseOptions

    case res of
        Nothing -> do
            putStrLn "cannot parse options"
            return ()
        Just opts@Options{..} ->
            case benchType of
                Linear -> benchShow opts cfg
                            { title = Just "Linear" }
                            makeLinearGraphs
                            "charts/linear/results.csv"
                            "charts/linear"
                LinearRate -> benchShow opts cfg
                            { title = Just "Linear Rate" }
                            (makeGraphs "linear-rate")
                            "charts/linear-rate/results.csv"
                            "charts/linear-rate"
                LinearAsync -> benchShow opts cfg
                            { title = Just "Linear Async" }
                            (makeStreamComparisonGraphs "linear-async" linearAsyncPrefixes)
                            "charts/linear-async/results.csv"
                            "charts/linear-async"
                NestedConcurrent -> benchShow opts cfg
                            { title = Just "Nested concurrent loops" }
                            (makeStreamComparisonGraphs "nested-concurrent" nestedBenchPrefixes)
                            "charts/nested-concurrent/results.csv"
                            "charts/nested-concurrent"
                FileIO -> benchShow opts cfg
                            { title = Just "File IO" }
                            (makeGraphs "fileIO")
                            "charts/fileio/results.csv"
                            "charts/fileio"
                Concurrent -> benchShow opts cfg
                            { title = Just "Concurrent Ops" }
                            (makeGraphs "Concurrent")
                            "charts/concurrent/results.csv"
                            "charts/concurrent"
                Parallel -> benchShow opts cfg
                            { title = Just "Parallel" }
                            (makeGraphs "parallel")
                            "charts/parallel/results.csv"
                            "charts/parallel"
                Adaptive -> benchShow opts cfg
                            { title = Just "Adaptive" }
                            (makeGraphs "adaptive")
                            "charts/adaptive/results.csv"
                            "charts/adaptive"
                Compare str -> showComparisons opts cfg
                            { title = Just $ str }
                            ("charts/" ++ str ++ "/results.csv")
                            ("charts/" ++ str)
                Standard str -> benchShow opts cfg
                            { title = Just str }
                            (makeGraphs str)
                            ("charts/" ++ str ++ "/results.csv")
                            ("charts/" ++ str)
