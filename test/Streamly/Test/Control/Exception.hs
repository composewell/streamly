module Streamly.Test.Control.Exception (main)

where

import Control.Exception
    (ErrorCall(..), SomeException, evaluate, finally, throwIO, try)
import Control.Monad (unless)
import Data.Foldable (sequenceA_)
import Data.Function ((&))
import Data.IORef (modifyIORef, newIORef, readIORef)
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

-- | Raise an exception if the two values differ.
expectEq :: (Eq a, Show a) => String -> a -> a -> IO ()
expectEq name expected actual =
    unless (expected == actual)
        $ error $ name ++ ": expected " ++ show expected
            ++ ", got " ++ show actual

-- | 'Exception.verify' returns the value when the predicate is True and
-- throws an exception when it is False.
testVerify :: IO ()
testVerify = do
    r1 <- try (evaluate (Exception.verify True (1 :: Int)))
    expectEq "verify True" (Right 1)
        (either (\(ErrorCall e) -> Left e) Right r1)
    r2 <- try (evaluate (Exception.verify False (1 :: Int)))
    expectEq "verify False" True
        (either (\(ErrorCall _) -> True) (const False) r2)

-- | All resources are released even if releasing one of them throws an
-- exception, the exception is rethrown after all are released.
testReleaseAfterException :: IO ()
testReleaseAfterException = do
    ref <- newIORef []
    r <- try $ Exception.withAcquireIO $ \aref -> do
        let free i = modifyIORef ref (i :)
        _ <- Exception.acquire aref (return ()) (\_ -> free (1 :: Int))
        _ <- Exception.acquire aref (return ())
                (\_ -> throwIO (ErrorCall "release"))
        _ <- Exception.acquire aref (return ()) (\_ -> free 3)
        _ <- Exception.acquireWith Exception.Priority1 aref (return ())
                (\_ -> throwIO (ErrorCall "release1"))
        _ <- Exception.acquireWith Exception.Priority1 aref (return ())
                (\_ -> free 0)
        return ()
    released <- readIORef ref
    expectEq "released" [0, 1, 3] (reverse released)
    expectEq "exception" (Just "release1")
        (either (\e -> Just (show (e :: SomeException))) (const Nothing) r)

funcs :: [(String, Int -> (Stream.Config -> Stream.Config) -> IO ())]
funcs =
    [ ("Exception.withAcquireIO", testEffect)
    , ("Exception.withAcquireIO release", testEffectRelease)
    ]

main :: IO ()
main = do
    putStrLn "Running: verify"
    testVerify
    putStrLn "Running: withAcquireIO release after exception"
    testReleaseAfterException

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
