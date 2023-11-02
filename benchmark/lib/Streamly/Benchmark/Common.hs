{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Streamly.Benchmark.Common
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

module Streamly.Benchmark.Common
    ( o_1_space_prefix
    , o_n_space_prefix
    , o_n_heap_prefix
    , o_n_stack_prefix

    , runWithCLIOptsEnv
    , runWithCLIOpts

    , benchIOSink1
    , benchPure
    , benchPureSink1
    , benchFold

    , benchIOSrc1
    , benchPureSrc

    , mkString
    , mkList
    , mkListString

    , defaultStreamSize
    )
where

#ifdef MIN_VERSION_gauge
import Control.Exception (evaluate)
import Control.Monad (when)
import Text.Read (readMaybe)
import Data.List (scanl')
import Data.Maybe (mapMaybe)
import System.Console.GetOpt
       (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt')
import System.Environment (getArgs, lookupEnv, setEnv)
#else
import Data.Proxy (Proxy(..))
import Test.Tasty.Ingredients.Basic (includingOptions)
import Test.Tasty.Options
    (IsOption(..), OptionDescription(..), lookupOption, safeRead)
import Test.Tasty.Runners (Ingredient, defaultMainWithIngredients, parseOptions)
#endif
import Control.DeepSeq (NFData(..))
import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as S

import Test.Tasty.Bench

-------------------------------------------------------------------------------
-- Benchmark Prefixes
-------------------------------------------------------------------------------

o_1_space_prefix :: String -> String
o_1_space_prefix name = name ++ "/o-1-space"

o_n_space_prefix :: String -> String
o_n_space_prefix name = name ++ "/o-n-space"

o_n_heap_prefix :: String -> String
o_n_heap_prefix name = name ++ "/o-n-heap"

o_n_stack_prefix :: String -> String
o_n_stack_prefix name = name ++ "/o-n-stack"

-------------------------------------------------------------------------------
-- Benchmarking utilities
-------------------------------------------------------------------------------

-- XXX once we convert all the functions to use this we can rename this to
-- benchIOSink
{-# INLINE benchIOSink1 #-}
benchIOSink1 :: NFData b => String -> (Int -> IO b) -> Benchmark
benchIOSink1 name f = bench name $ nfIO $ randomRIO (1,1) >>= f

{-# INLINE benchIOSrc1 #-}
benchIOSrc1 :: String -> (Int -> IO ()) -> Benchmark
benchIOSrc1 name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.
{-# INLINE benchFold #-}
benchFold :: NFData b
    => String -> (t IO Int -> IO b) -> (Int -> t IO Int) -> Benchmark
benchFold name f src = bench name $ nfIO $ randomRIO (1,1) >>= f . src

{-# INLINE benchPure #-}
benchPure :: NFData b => String -> (Int -> a) -> (a -> b) -> Benchmark
benchPure name src f = bench name $ nfIO $ randomRIO (1,1) >>= return . f . src

-- XXX once we convert all the functions to use this we can rename this to
-- benchPureSink
{-# INLINE benchPureSink1 #-}
benchPureSink1 :: NFData b => String -> (Int -> Identity b) -> Benchmark
benchPureSink1 name f =
    bench name $ nfIO $ randomRIO (1,1) >>= return . runIdentity . f

{-# INLINE benchPureSrc #-}
benchPureSrc :: String -> (Int -> S.Stream Identity a) -> Benchmark
benchPureSrc name src = benchPure name src (runIdentity . drain)

    where

    drain = S.fold Fold.drain

-------------------------------------------------------------------------------
-- String/List generation for read instances
-------------------------------------------------------------------------------

{-# INLINABLE mkString #-}
mkString :: Int -> String
mkString value = "fromList [1" ++ concat (replicate value ",1") ++ "]"

{-# INLINABLE mkListString #-}
mkListString :: Int -> String
mkListString value = "[1" ++ concat (replicate value ",1") ++ "]"

{-# INLINABLE mkList #-}
mkList :: Int -> [Int]
mkList value = [1..value]

-------------------------------------------------------------------------------
-- Stream size
-------------------------------------------------------------------------------

defaultStreamSize :: Int
defaultStreamSize = 100000

-------------------------------------------------------------------------------
-- Parse custom CLI options
-------------------------------------------------------------------------------

newtype BenchOpts = StreamSize Int deriving Show

#ifdef MIN_VERSION_gauge
getStreamSize :: String -> Int
getStreamSize size =
    case (readMaybe size :: Maybe Int) of
        Just x -> x
        Nothing -> error "Stream size must be numeric"

options :: [OptDescr BenchOpts]
options =
    [
        System.Console.GetOpt.Option [] ["stream-size"] (ReqArg getSize "COUNT") "Stream element count"
    ]

    where

    getSize = StreamSize . getStreamSize

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
parseCLIOpts defStreamSize = do
    args <- getArgs
    -- Parse custom options
    let (opts, _, _, errs) = getOpt' Permute options args
    when (not $ null errs) $ error $ concat errs
    (streamSize, args') <-
        case opts of
            StreamSize x : _ -> do
                putStrLn $ "Stream size: " ++ show x
                -- When using the gauge "--measure-with" option we need to make
                -- sure that we pass the stream size to child process forked by
                -- gauge. So we use this env var for that purpose.
                setEnv "STREAM_SIZE" (show x)
                -- Hack! remove the option and its argument from args
                -- getOpt should have a way to return the unconsumed args in
                -- correct order.
                newArgs <-
                          evaluate
                        $ mapMaybe snd
                        $ scanl' deleteOptArgs (Nothing, Nothing) args
                return (x, newArgs)
            _ -> do
                r <- lookupEnv "STREAM_SIZE"
                case r of
                    Just x -> do
                        s <- evaluate $ getStreamSize x
                        return (s, args)
                    Nothing -> do
                        setEnv "STREAM_SIZE" (show defStreamSize)
                        return (defStreamSize, args)

    -- Parse gauge options
    let config = defaultConfig
                { timeLimit = Just 1
                , minDuration = 0
                , includeFirstIter = streamSize > defStreamSize
                }
    let (cfg, benches) = parseWith config args'
    streamSize `seq` return (streamSize, cfg, benches)

#else
instance IsOption BenchOpts where
    defaultValue = StreamSize defaultStreamSize
    parseValue = fmap StreamSize . safeRead
    optionName = pure "stream-size"
    optionHelp = pure "Size of the stream to be used in benchmarks"

parseCLIOpts :: Int -> Benchmark -> IO (Int, [Ingredient])
parseCLIOpts defStreamSize benches = do
    let customOpts  = [Test.Tasty.Options.Option (Proxy :: Proxy BenchOpts)]
        ingredients = includingOptions customOpts : benchIngredients
    opts <- parseOptions ingredients benches -- (TestGroup "" [])
    let StreamSize size = lookupOption opts
    -- putStrLn $ "Stream size: " ++ show size
    if size == defaultStreamSize
    then return (defStreamSize, ingredients)
    else return (size, ingredients)
#endif

runWithCLIOptsEnv :: Int -> (Int -> IO a) -> (a -> Int -> [Benchmark]) -> IO ()
runWithCLIOptsEnv defStreamSize alloc mkBench = do

#ifdef MIN_VERSION_gauge
    (value, cfg, benches) <- parseCLIOpts defStreamSize
    r <- alloc value
    value `seq` runMode (mode cfg) cfg benches (mkBench r value)
#else
    r0 <- alloc 0
    (value, ingredients) <-
        parseCLIOpts defStreamSize $ bgroup "All" (mkBench r0 0)
    r <- alloc value
    value `seq` defaultMainWithIngredients ingredients
        $ bgroup "All" (mkBench r value)
#endif

runWithCLIOpts :: Int -> (Int -> [Benchmark]) -> IO ()
runWithCLIOpts defStreamSize f =
    runWithCLIOptsEnv defStreamSize
        (const $ return undefined)
        (\_ v -> f v)
