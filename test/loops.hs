import Streamly
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import Streamly.Prelude (nil, once)

main = do
    hSetBuffering stdout LineBuffering

    putStrLn $ "\nloopTail:\n"
    runStream $ do
        x <- loopTail 0
        once $ print (x :: Int)

    putStrLn $ "\nloopHead:\n"
    runStream $ do
        x <- loopHead 0
        once $ print (x :: Int)

    putStrLn $ "\nloopTailA:\n"
    runStream $ do
        x <- loopTailA 0
        once $ print (x :: Int)

    putStrLn $ "\nloopHeadA:\n"
    runStream $ do
        x <- loopHeadA 0
        once $ print (x :: Int)

    putStrLn $ "\ncosplice:\n"
    runStream $ do
        x <- (return 0 <> return 1) `cosplice` (return 100 <> return 101)
        once $ print (x :: Int)

    putStrLn $ "\nParallel interleave:\n"
    runStream $ do
        x <- (return 0 <> return 1) `parallel` (return 100 <> return 101)
        once $ print (x :: Int)

    where

-------------------------------------------------------------------------------
-- Serial (single-threaded) stream generator loops
-------------------------------------------------------------------------------

    -- Generates a value and then loops. Can be used to generate an infinite
    -- stream. Interleaves the generator and the consumer.
    loopTail :: Int -> Stream Int
    loopTail x = do
        once $ putStrLn "LoopTail..."
        return x <> (if x < 3 then loopTail (x + 1) else nil)

    -- Loops and then generates a value. The consumer can run only after the
    -- loop has finished.  An infinite generator will not let the consumer run
    -- at all.
    loopHead :: Int -> Stream Int
    loopHead x = do
        once $ putStrLn "LoopHead..."
        (if x < 3 then loopHead (x + 1) else nil) <> return x

-------------------------------------------------------------------------------
-- Concurrent (multi-threaded) adaptive demand-based stream generator loops
-------------------------------------------------------------------------------

    loopTailA :: Int -> Stream Int
    loopTailA x = do
        once $ putStrLn "LoopTailA..."
        return x `coparallel` (if x < 3 then loopTailA (x + 1) else nil)

    loopHeadA :: Int -> Stream Int
    loopHeadA x = do
        once $ putStrLn "LoopHeadA..."
        (if x < 3 then loopHeadA (x + 1) else nil) `coparallel` return x

-------------------------------------------------------------------------------
-- Parallel (fairly scheduled, multi-threaded) stream generator loops
-------------------------------------------------------------------------------
