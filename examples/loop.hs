import Control.Applicative ((<|>), empty)
import Control.Concurrent (threadDelay, myThreadId)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import System.Random (randomIO)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import Asyncly

main = do
    liftIO $ hSetBuffering stdout LineBuffering

    putStrLn $ "\nloopTail:\n"
    runAsyncly $ do
        x <- loopTail (0 :: Int)
        liftIO $ print x

    putStrLn $ "\nloopHead:\n"
    runAsyncly $ do
        x <- loopHead (0 :: Int)
        liftIO $ print x

    putStrLn $ "\nloopTailA:\n"
    runAsyncly $ do
        x <- loopTailA (0 :: Int)
        liftIO $ print x

    putStrLn $ "\nloopHeadA:\n"
    runAsyncly $ do
        x <- loopHeadA (0 :: Int)
        liftIO $ print x

    putStrLn $ "\ninterleave:\n"
    runAsyncly $ do
        x <- return (0 :: Int) <> return 1 <=> return 100 <> return 101
        liftIO $ print x

    putStrLn $ "\nParallel interleave:\n"
    runAsyncly $ do
        x <- return (0 :: Int) <> return 1 <|> return 100 <> return 101
        liftIO $ print x

    where

-------------------------------------------------------------------------------
-- Serial (single-threaded) stream generator loops
-------------------------------------------------------------------------------

    -- In a <> composition the action on the left is executed and only after it
    -- finished then the action on the right is executed. In other words the
    -- actions are run serially.

    -- Generates a value and then loops. Can be used to generate an infinite
    -- stream. Interleaves the generator and the consumer.
    loopTail :: Int -> AsyncT IO Int
    loopTail x = do
        liftIO $ putStrLn "LoopTail..."
        return x <> (if x < 3 then loopTail (x + 1) else empty)

    -- Loops and then generates a value. The consumer can run only after the
    -- loop has finished.  An infinite generator will not let the consumer run
    -- at all.
    loopHead :: Int -> AsyncT IO Int
    loopHead x = do
        liftIO $ putStrLn "LoopHead..."
        (if x < 3 then loopHead (x + 1) else empty) <> return x

-------------------------------------------------------------------------------
-- Concurrent (multi-threaded) adaptive demand-based stream generator loops
-------------------------------------------------------------------------------

    -- In a <| composition the action on the left is executed first. However,
    -- if it is not fast enough to generate results at the consumer's speed
    -- then the action on the right is also spawned concurrently. In other
    -- words, both actions may run concurrently based on the need.

    loopTailA :: Int -> AsyncT IO Int
    loopTailA x = do
        liftIO $ putStrLn "LoopTailA..."
        return x <| (if x < 3 then loopTailA (x + 1) else empty)

    loopHeadA :: Int -> AsyncT IO Int
    loopHeadA x = do
        liftIO $ putStrLn "LoopHeadA..."
        (if x < 3 then loopHeadA (x + 1) else empty) <| return x

-------------------------------------------------------------------------------
-- Parallel (fairly scheduled, multi-threaded) stream generator loops
-------------------------------------------------------------------------------

    -- In a <|> composition both actions are run concurrently in a fair
    -- manner, no one action is preferred over another. Both actions are
    -- spawned right away in their own independent threads. In other words, the
    -- actions will run concurrently.
