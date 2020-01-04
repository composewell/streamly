-- |
-- Module      : Main
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

module Common (parseCLIOpts) where

import Control.Exception (evaluate)
import Control.Monad (when)
import Data.List (scanl')
import Data.Maybe (catMaybes)
import System.Console.GetOpt
       (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt')
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Gauge

-------------------------------------------------------------------------------
-- Parse custom CLI options
-------------------------------------------------------------------------------

data BenchOpts = StreamSize Int deriving Show

options :: [OptDescr BenchOpts]
options =
    [
      Option [] ["stream-size"] (ReqArg getSize "COUNT") "Stream element count"
    ]

    where

    getSize :: String -> BenchOpts
    getSize size =
        case (readMaybe size :: Maybe Int) of
            Just x -> StreamSize x
            Nothing -> error "Stream size must be  numeric"

deleteOptArgs
    :: (Maybe String, Maybe String) -- (prev, yielded)
    -> String
    -> (Maybe String, Maybe String)
deleteOptArgs (Nothing, _) opt =
    if opt == "--stream-size"
    then (Just opt, Nothing)
    else (Just opt, Just opt)

deleteOptArgs (Just prev, _) opt =
    if opt == "--stream-size" || prev == "--stream-size"
    then (Just opt, Nothing)
    else (Just opt, Just opt)

parseCLIOpts :: Int -> IO (Int, Config, [String])
parseCLIOpts defaultStreamSize = do
    args <- getArgs

    -- Parse custom options
    let (opts, _, _, errs) = getOpt' Permute options args
    when (not $ null errs) $ error $ concat errs
    (streamSize, args') <- evaluate $
        case opts of
            StreamSize x : _ ->
                -- Hack! remove the option and its argument from args
                -- getOpt should have a way to return the unconsumed args in
                -- correct order.
                let newArgs =
                          catMaybes
                        $ map snd
                        $ scanl' deleteOptArgs (Nothing, Nothing) args
                in (x, newArgs)
            _ -> (defaultStreamSize, args)

    -- Parse gauge options
    let config = defaultConfig
                { timeLimit = Just 1
                , minDuration = 0
                , includeFirstIter = streamSize > defaultStreamSize
                }
    let (cfg, benches) = parseWith config args'
    return (streamSize, cfg, benches)
