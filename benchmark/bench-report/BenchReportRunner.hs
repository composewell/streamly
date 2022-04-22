{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (catch, ErrorCall(..))
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Char (toLower)
import Data.List
import System.Environment (getArgs)
import Text.Read (readMaybe)

import BenchReport

main :: IO ()
main = do
    res <- parseOptions
    case res of
        Nothing -> do
            putStrLn "cannot parse options"
            return ()
        Just opts@Options{fields = fs, benchType = btype} ->
            let cfg = defaultConfig
                    { presentation = Groups (diffStyle opts)
                    , selectBenchmarks = selectBench opts
                    , selectFields = filter
                        ( flip elem (fmap (fmap toLower) fs)
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
