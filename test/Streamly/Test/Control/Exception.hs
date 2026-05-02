module Streamly.Test.Control.Exception (main)

where

import Control.Exception (finally)
import Data.Foldable (sequenceA_)
import Data.Function ((&))
import Data.IORef (newIORef)
import Streamly.Internal.Data.Stream.Prelude (Config)

import qualified Streamly.Internal.Control.Exception as Exception
import qualified Streamly.Internal.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold

import Streamly.Test.Control.Exception.Common

testEffect :: Int -> (Config -> Config) -> IO ()
testEffect t cfg = do
    ref <- newIORef (0 :: Int)
    Exception.withAcquireIO (\aref ->
        stream ref (cfg . Stream.useAcquire aref)
        & Stream.take 1
        & Stream.fold Fold.drain
      ) `finally` finalAction False ref t

testEffectRelease :: Int -> (Config -> Config) -> IO ()
testEffectRelease count cfg = do
    ref1 <- newIORef (0 :: Int)
    ref2 <- newIORef (0 :: Int)
    Exception.withAcquireIO (\aref -> do
            let cfg1 = cfg . Stream.useAcquire aref
            streamRelease aref ref1 ref2 cfg1
                & Stream.take 1
                & Stream.fold Fold.drain
      ) `finally` do
            putStrLn "Checking MANUALLY released resources..."
            finalAction False ref1 count
            putStrLn "Checking AUTO released resources..."
            finalAction False ref2 count

funcs :: [(String, Int -> (Stream.Config -> Stream.Config) -> IO ())]
funcs =
    [ ("Exception.withAcquireIO", testEffect)
    , ("Exception.withAcquireIO release", testEffectRelease)
    ]

main :: IO ()
main = do
    let cfg = id -- Stream.inspect True

    -- TODO: Interrupt test
    -- Run the main test in a separate thread. Keep the thread-id in a global
    -- variable which will be used to interrupt the thread. Once one thread is
    -- over then the next test will keep it's threadId in the global var.
    -- Run another thread which sleeps for random intervals and sends
    -- UserInterrupt exception to the current test thread-id stored in the
    -- glbal variable in a loop.
    -- TODO: test for non-concurrent use cases as well
    sequenceA_
        [ putStrLn ("Running: " ++ fst f ++ " " ++ fst x1 ++ " " ++ fst x2)
            >> snd f
                (if fst x1 == "default" then 500000 else 100000)
                (snd x1 . snd x2 . cfg)
        | f <- funcs, x1 <- limits, x2 <- sched
        ]
