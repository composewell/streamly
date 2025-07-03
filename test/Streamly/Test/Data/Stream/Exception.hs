module Streamly.Test.Data.Stream.Exception (main)

where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throw, catch, finally, bracket_)
import Control.Monad (when)
import Data.Foldable (sequenceA_)
import Data.Function ((&))
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import Streamly.Internal.Control.Exception (AllocateIO, acquire)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Stream.Prelude (Config)
import System.Mem (performMajorGC)

import qualified Streamly.Internal.Control.Exception as Exception
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

streamRelease :: AllocateIO -> IORef Int -> IORef Int -> (Config -> Config) -> Stream IO ()
streamRelease bracket ref1 ref2 modifier =
      Stream.enumerateFrom (1 :: Int)
        & Stream.parMapM modifier
            ( \x -> do
              if x <= 10
              then do
                  ((), release) <- acquire bracket (incr ref1) (\() -> decr ref1)
                  -- 1000 makes a particular bug surface, not less, not more
                  threadDelay 1000
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

testStream :: Int -> (Config -> Config) -> IO ()
testStream t cfg = do
    ref <- newIORef (0 :: Int)
    (Stream.withAllocateIO (\reg -> stream ref (cfg . Stream.setReleaseCb reg))
        -- XXX enable this when stream finalization is implemented
        -- & Stream.take 1
        & Stream.fold Fold.drain) `finally` finalAction False ref t

testStreamRelease :: Int -> (Config -> Config) -> IO ()
testStreamRelease count cfg = do
    ref1 <- newIORef (0 :: Int)
    ref2 <- newIORef (0 :: Int)
    (Stream.withAllocateIO (\alloc -> do
            let cfg1 = cfg . Stream.setReleaseCb alloc
            streamRelease alloc ref1 ref2 cfg1)
        -- XXX enable this when stream finalization is implemented
        -- & Stream.take 1
        & Stream.fold Fold.drain
       )
       `finally` do
            putStrLn "Checking MANUALLY released resources..."
            finalAction False ref1 count
            putStrLn "Checking AUTO released resources..."
            finalAction False ref2 count

testEffect :: Int -> (Config -> Config) -> IO ()
testEffect t cfg = do
    ref <- newIORef (0 :: Int)
    Exception.withAllocateIO (\reg ->
        stream ref (cfg . Stream.setReleaseCb reg)
        & Stream.take 1
        & Stream.fold Fold.drain
      ) `finally` finalAction False ref t

testEffectRelease :: Int -> (Config -> Config) -> IO ()
testEffectRelease count cfg = do
    ref1 <- newIORef (0 :: Int)
    ref2 <- newIORef (0 :: Int)
    Exception.withAllocateIO (\alloc -> do
            let cfg1 = cfg . Stream.setReleaseCb alloc
            streamRelease alloc ref1 ref2 cfg1
                & Stream.take 1
                & Stream.fold Fold.drain
      ) `finally` do
            putStrLn "Checking MANUALLY released resources..."
            finalAction False ref1 count
            putStrLn "Checking AUTO released resources..."
            finalAction False ref2 count

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
    [ ("Stream.withAllocateIO", testStream)
    , ("Exception.withAllocateIO", testEffect)
    , ("Stream.withAllocateIO release", testStreamRelease)
    , ("Exception.withAllocateIO release", testEffectRelease)
    , ("finallyGC", finallyGC)
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
