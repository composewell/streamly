-- |
-- Module      : Streamly.Test.Control.Exception.Common
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Control.Exception.Common
    ( incr
    , decr
    , handler
    , run
    , timeout
    , takeCount
    , stream
    , streamRelease
    , finalAction
    , limits
    , sched
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throw, catch, bracket_)
import Control.Monad (when)
import Data.Function ((&))
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import Streamly.Internal.Control.Exception (AcquireIO, acquire)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Stream.Prelude (Config)
import System.Mem (performMajorGC)

import qualified Streamly.Internal.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream as Stream

-- IMPORTANT: do not use a blocking operation inside it, otherwise the tests
-- might fail because the operation will become interruptible..
incr :: Num a => IORef a -> IO ()
incr ref = do
    -- tid <- myThreadId
    -- putStrLn $ "Incrementing the counter: " ++ show tid
    atomicModifyIORef' ref (\x -> (x + 1, ()))

-- IMPORTANT: do not use a blocking operation inside it, otherwise the tests
-- might fail because the operation will become interruptible..
decr :: Num a => IORef a -> IO ()
decr ref = do
    atomicModifyIORef' ref (\x -> (x - 1, ()))
    -- tid <- myThreadId
    -- putStrLn $ "Decremented the counter: " ++ show tid

handler :: SomeException -> IO b
handler (e :: SomeException) = do
    -- tid <- myThreadId
    -- putStrLn $ "Child: " ++ show tid ++ " " ++ show e
    -- Rethrowing the exception is important, otherwise the thread will not
    -- exit.
    throw e

run :: Num a => IORef a -> IO c -> IO c
run ref x = bracket_ (incr ref) (decr ref) (x `catch` handler)

timeout :: Int
timeout = 1000000

takeCount :: Int
takeCount = 1

stream :: IORef Int -> (Config -> Config) -> Stream.Stream IO ()
stream ref modifier =
      Stream.enumerateFrom (1 :: Int)
        & Stream.parMapM modifier
            ( \x ->
              -- somehow if all of them have same timeout then the chances of
              -- failure are more.
              run ref $ threadDelay (if x == 1 then 1000000 else timeout)
            )
        & Stream.take takeCount

streamRelease ::
    AcquireIO -> IORef Int -> IORef Int -> (Config -> Config) -> Stream IO ()
streamRelease aref ref1 ref2 modifier =
      Stream.enumerateFrom (1 :: Int)
        & Stream.parMapM modifier
            ( \x -> do
              if x <= 10
              then do
                -- IMPORTANT: do not put interruptile operations in the
                -- release function, otherwise the tests might fail,
                -- because the operation will become interruptible.
                  ((), release) <-
                        acquire aref (incr ref1) (\() -> decr ref1)
                  -- 1000 makes a particular bug surface, not less, not more
                  threadDelay 1000
                  -- putStrLn $ "release: " ++ show x
                  release
              else do
                  run ref2 $ threadDelay timeout
            )
        & Stream.take 10

finalAction :: Bool -> IORef Int -> Int -> IO ()
finalAction gc ref t = do
    -- When cleanup happens via GC, ghc creates a thread for the finalizer to
    -- run, actual cleanup time depends on when that thread is scheduled. The
    -- thread may outlive one or more GCs. So we have to give it some time to
    -- finish. But it cannot be deterministic.
    -- threadDelay 1000000
    when gc $ do
        performMajorGC
        threadDelay t
        performMajorGC
        threadDelay t
    r <- readIORef ref
    putStrLn $ "Pending computations: " ++ show r
    -- Delay for letting any gc based cleanup threads drain and print output
    -- for debugging
    -- when gc $ threadDelay 1000000
    when (r /= 0) $ error "Failed"

-- XXX Include rate as well
limits :: [(String, Stream.Config -> Stream.Config)]
limits =
    [ ("default", id)
    , ("maxBuffer 10", Stream.maxBuffer 10)
    , ("maxThreads 10", Stream.maxThreads 10)
    ]

sched :: [(String, Stream.Config -> Stream.Config)]
sched =
    [ ("default", id)
    , ("eager", Stream.eager True)
    , ("ordered", Stream.ordered True)
    , ("interleaved", Stream.interleaved True)
    ]
