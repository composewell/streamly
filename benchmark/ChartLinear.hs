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

import BenchGraph
import Utils

ignoringErr a = catch a (\(ErrorCall err :: ErrorCall) ->
    putStrLn $ "Failed with error:\n" ++ err ++ "\nSkipping.")

makeGraphs cfg = do
    ignoringErr $ graph inputFile "operations" $ cfg
        { title = Just "Streamly operations"
        , classifyBenchmark = \b ->
                if (not $ "serially/" `isPrefixOf` b)
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

inputFile :: String
inputFile = "charts/linear/results.csv"

-- Primitive command line options parsing
data Options = Options
    { genGraphs :: Bool
    }

main :: IO ()
main = do
    let cfg = defaultConfig
            { outputDir = Just "charts/linear"
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
