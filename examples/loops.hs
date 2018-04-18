import Streamly
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

main = do
    liftIO $ hSetBuffering stdout LineBuffering

    putStrLn $ "\nloopTail:\n"
    runStream $ do
        x <- loopTail 0
        liftIO $ print (x :: Int)

    putStrLn $ "\nloopHead:\n"
    runStream $ do
        x <- loopHead 0
        liftIO $ print (x :: Int)

    putStrLn $ "\nloopTailA:\n"
    runStream $ do
        x <- loopTailA 0
        liftIO $ print (x :: Int)

    putStrLn $ "\nloopHeadA:\n"
    runStream $ do
        x <- loopHeadA 0
        liftIO $ print (x :: Int)

    putStrLn $ "\ninterleave:\n"
    runStream $ do
        x <- (return 0 <> return 1) `interleave` (return 100 <> return 101)
        liftIO $ print (x :: Int)

    putStrLn $ "\nParallel interleave:\n"
    runStream $ do
        x <- (return 0 <> return 1) `parallel` (return 100 <> return 101)
        liftIO $ print (x :: Int)

    where

-------------------------------------------------------------------------------
-- Serial (single-threaded) stream generator loops
-------------------------------------------------------------------------------

    -- In a <> composition the action on the left is executed and only after it
    -- finished then the action on the right is executed. In other words the
    -- actions are run serially.

    -- Generates a value and then loops. Can be used to generate an infinite
    -- stream. Interleaves the generator and the consumer.
    loopTail :: Int -> SerialT IO Int
    loopTail x = do
        liftIO $ putStrLn "LoopTail..."
        return x <> (if x < 3 then loopTail (x + 1) else empty)

    -- Loops and then generates a value. The consumer can run only after the
    -- loop has finished.  An infinite generator will not let the consumer run
    -- at all.
    loopHead :: Int -> SerialT IO Int
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

    loopTailA :: Int -> SerialT IO Int
    loopTailA x = do
        liftIO $ putStrLn "LoopTailA..."
        return x `aparallel` (if x < 3 then loopTailA (x + 1) else empty)

    loopHeadA :: Int -> SerialT IO Int
    loopHeadA x = do
        liftIO $ putStrLn "LoopHeadA..."
        (if x < 3 then loopHeadA (x + 1) else empty) `aparallel` return x

-------------------------------------------------------------------------------
-- Parallel (fairly scheduled, multi-threaded) stream generator loops
-------------------------------------------------------------------------------

    -- In a <|> composition both actions are run concurrently in a fair
    -- manner, no one action is preferred over another. Both actions are
    -- spawned right away in their own independent threads. In other words, the
    -- actions will run concurrently.
