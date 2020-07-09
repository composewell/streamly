{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (catch, ErrorCall(..))
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Char (toLower)
import Data.List
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)

import BenchShow

------------------------------------------------------------------------------
-- Command line parsing
------------------------------------------------------------------------------

data BenchType
    = Compare String
    | Standard String
    deriving Show

data Options = Options
    { genGraphs :: Bool
    , benchType :: Maybe BenchType
    , fields :: [String]
    } deriving Show

defaultOptions :: Options
defaultOptions = Options False Nothing ["time"]

setGenGraphs :: Monad m => Bool -> StateT (a, Options) m ()
setGenGraphs val = do
    (args, opts) <- get
    put (args, opts { genGraphs = val })

setBenchType :: Monad m => BenchType -> StateT (a, Options) m ()
setBenchType val = do
    (args, opts) <- get
    put (args, opts { benchType = Just val })

setFields :: Monad m => [String] -> StateT (a, Options) m ()
setFields val = do
    (args, opts) <- get
    put (args, opts { fields = val })

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
        Just str | "_cmp" `isSuffixOf` str -> setBenchType (Compare str)
        Just str -> setBenchType (Standard str)
        Nothing -> do
                liftIO $ putStrLn "please provide a benchmark type "
                mzero

parseFields :: StateT ([String], Options) (MaybeT IO) ()
parseFields = do
    x <- shift
    case x of
        Just str -> setFields (words str)
        Nothing -> do
                liftIO $ putStrLn
                    "please provide a list of fields after --fields"
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
            "--graphs"    -> setGenGraphs True
            "--benchmark" -> parseBench
            "--fields"    -> parseFields
            str -> do
                liftIO $ putStrLn $ "Unrecognized option " <> str
                mzero

    parseLoop = do
        next <- shift
        case next of
            Just opt -> parseOpt opt >> parseLoop
            Nothing -> return ()

ignoringErr :: IO () -> IO ()
ignoringErr a = catch a (\(ErrorCall err :: ErrorCall) ->
    putStrLn $ "Failed with error:\n" <> err <> "\nSkipping.")

------------------------------------------------------------------------------
-- Generic
------------------------------------------------------------------------------

makeGraphs :: String -> Config -> String -> IO ()
makeGraphs name cfg@Config{..} inputFile =
    ignoringErr $ graph inputFile name cfg

------------------------------------------------------------------------------
-- Arrays
------------------------------------------------------------------------------

showComparisons :: Options -> Config -> FilePath -> FilePath -> IO ()
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

    classifyComparison b = Just
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

benchShow ::
       Options
    -> Config
    -> (Config -> String -> IO ())
    -> String
    -> FilePath
    -> IO ()
benchShow Options{..} cfg func inp out =
    if genGraphs
    then func cfg {outputDir = Just out} inp
    else ignoringErr $ report inp Nothing cfg

main :: IO ()
main = do
    res <- parseOptions
    case res of
        Nothing -> do
            putStrLn "cannot parse options"
            return ()
        Just opts@Options{fields = fs, benchType = btype} ->
            let cfg = defaultConfig
                    { presentation = Groups PercentDiff
                    , selectBenchmarks = selectBench
                    , selectFields = filter
                        ( flip elem fs
                        . fmap toLower
                        )
                    }
            in case btype of
                Just (Compare str) ->
                    showComparisons opts cfg
                        { title = Just str }
                        ("charts/" ++ str ++ "/results.csv")
                        ("charts/" ++ str)
                Just (Standard str) ->
                    benchShow opts cfg
                        { title = Just str }
                        (makeGraphs str)
                        ("charts/" ++ str ++ "/results.csv")
                        ("charts/" ++ str)
                Nothing ->
                    error "Please specify a benchmark using --benchmark."
