module Streamly.Test.Data.Stream.Exception (main)

where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throw, catch, finally, bracket_)
import Control.Monad (when)
import Data.Foldable (sequenceA_)
import Data.Function ((&))
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import System.Mem (performMajorGC)

import qualified Streamly.Internal.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold

incr :: Num a => IORef a -> IO ()
incr ref = do
    -- tid <- myThreadId
    -- putStrLn $ "Incrementing the counter: " ++ show tid
    atomicModifyIORef ref (\x -> (x + 1, ()))

decr :: Num a => IORef a -> IO ()
decr ref = do
    atomicModifyIORef ref (\x -> (x - 1, ()))
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

stream :: Num a =>
    IORef a -> (Stream.Config -> Stream.Config) -> Stream.Stream IO ()
stream ref modifier =
      Stream.enumerateFrom (1 :: Int)
        & Stream.parMapM modifier
            ( \x ->
              -- somehow if all of them have same timeout then the chances of
              -- failure are more.
              run ref $ threadDelay (if x == 1 then 1000000 else timeout)
            )
        & Stream.take takeCount

finalAction :: (Show a, Eq a, Num a) => Bool -> IORef a -> Int -> IO ()
finalAction gc ref t = do
    -- We have initiated cleanup but we do not wait for the threads to
    -- exit, therefore, we have to give them some time to be scheduled and
    -- run the exception handler.
    when gc $ performMajorGC
    threadDelay t
    r <- readIORef ref
    putStrLn $ "Pending computations: " ++ show r
    when (r /= 0) $ error "Failed"

cleanup :: Int -> (Stream.Config -> Stream.Config) -> IO ()
cleanup t cfg = do
    ref <- newIORef (0 :: Int)
    (Stream.cleanupIO (\f -> stream ref (cfg . Stream.addCleanup f))
        & Stream.fold Fold.drain) `finally` finalAction False ref t

cleanupEffect :: Int -> (Stream.Config -> Stream.Config) -> IO ()
cleanupEffect t cfg = do
    ref <- newIORef (0 :: Int)
    Stream.cleanupEffectIO (\f -> stream ref (cfg . Stream.addCleanup f)
        & Stream.fold Fold.drain) `finally` finalAction False ref t

finallyGC :: Int -> (Stream.Config -> Stream.Config) -> IO ()
finallyGC t cfg = do
    ref <- newIORef (0 :: Int)
    Stream.finallyIO (finalAction True ref t) (stream ref cfg)
        & Stream.fold Fold.drain

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

funcs :: [(String, Int -> (Stream.Config -> Stream.Config) -> IO ())]
funcs =
    [ ("cleanup", cleanup)
    , ("cleanupEffect", cleanupEffect)
    , ("finallyGC", finallyGC)
    ]

main :: IO ()
main = do
    let cfg = id -- Stream.inspect True

    -- TODO: Interrupt test
    sequenceA_
        [ putStrLn ("Running: " ++ fst f ++ " " ++ fst x1 ++ " " ++ fst x2)
            >> (snd f)
                (if fst x1 == "default" then 100000 else 100000)
                (snd x1 . snd x2 . cfg)
        | f <- funcs, x1 <- limits, x2 <- sched
        ]
