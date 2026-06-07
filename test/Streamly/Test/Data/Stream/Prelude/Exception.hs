module Streamly.Test.Data.Stream.Prelude.Exception (main)

where

import Control.Exception (Exception, finally)
import Control.Monad.Catch (throwM)
import Data.Foldable (sequenceA_)
import Data.Function ((&))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Streamly.Internal.Data.Stream.Prelude (Config)
import Test.Hspec

import qualified Data.Map.Strict as Map
import qualified Streamly.Internal.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold

import Streamly.Test.Control.Exception.Common

testStream :: Int -> (Config -> Config) -> IO ()
testStream t cfg = do
    ref <- newIORef (0 :: Int)
    (Stream.withAcquireIO (\aref -> stream ref (cfg . Stream.useAcquire aref))
        -- XXX enable this when stream finalization is implemented
        -- & Stream.take 1
        & Stream.fold Fold.drain) `finally` finalAction False ref t

testStreamRelease :: Int -> (Config -> Config) -> IO ()
testStreamRelease count cfg = do
    ref1 <- newIORef (0 :: Int)
    ref2 <- newIORef (0 :: Int)
    (Stream.withAcquireIO (\aref -> do
            let cfg1 = cfg . Stream.useAcquire aref
            streamRelease aref ref1 ref2 cfg1)
        -- XXX enable this when stream finalization is implemented
        -- & Stream.take 1
        & Stream.fold Fold.drain
       )
       `finally` do
            putStrLn "Checking MANUALLY released resources..."
            finalAction False ref1 count
            putStrLn "Checking AUTO released resources..."
            finalAction False ref2 count

finallyGC :: Int -> (Stream.Config -> Stream.Config) -> IO ()
finallyGC t cfg = do
    ref <- newIORef (0 :: Int)
    Stream.finallyIO (finalAction True ref t) (stream ref cfg)
        & Stream.fold Fold.drain

newtype ExampleException = ExampleException String deriving (Eq, Show, Ord)

instance Exception ExampleException

retry :: Spec
retry = do
    ref <- runIO $ newIORef (0 :: Int)
    res <- runIO $ Stream.toList (Stream.retry emap handler1 (stream1 ref))
    refVal <- runIO $ readIORef ref
    spec res refVal

    where

    emap = Map.singleton (ExampleException "E") 10

    stream1 ref =
        Stream.fromListM
            [ return 1
            , return 2
            , atomicModifyIORef' ref (\a -> (a + 1, ()))
                  >> throwM (ExampleException "E")
                  >> return 3
            , return 4
            ]

    stream2 = Stream.fromList [5, 6, 7 :: Int]
    handler1 = const stream2
    expectedRes = [1, 2, 5, 6, 7]
    expectedRefVal = 11

    spec res refVal = do
        it "Runs the exception handler properly" $ res `shouldBe` expectedRes
        it "Runs retires the exception correctly"
            $ refVal `shouldBe` expectedRefVal

funcs :: [(String, Int -> (Stream.Config -> Stream.Config) -> IO ())]
funcs =
    [ ("Stream.withAcquireIO", testStream)
    , ("Stream.withAcquireIO release", testStreamRelease)
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
    hspec $ describe "Stream.retry" retry
