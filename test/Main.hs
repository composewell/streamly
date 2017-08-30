module Main (main) where

import Asyncly
import Control.Applicative ((<|>), empty)
import Control.Monad (mzero)
import Control.Concurrent (myThreadId, threadDelay)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Data.List (nub, sort)
import Test.Hspec

default (Int)

main :: IO ()
main = hspec $ do
    describe "Runners" $ do
        it "simple runAsyncly" $
            runAsyncly (return 0) `shouldReturn` ()
        it "simple runAsyncly with IO" $
            runAsyncly (liftIO $ putStrLn "hello") `shouldReturn` ()
        it "Captures a return value using toList" $
            toList (return 0) `shouldReturn` ([0] :: [Int])

    describe "Empty" $ do
        it "Monoid - mempty" $
            (toList mempty) `shouldReturn` ([] :: [Int])
        it "Alternative - empty" $
            (toList empty) `shouldReturn` ([] :: [Int])
        it "MonadPlus - mzero" $
            (toList mzero) `shouldReturn` ([] :: [Int])

    describe "Bind" bind

    describe "Serial Composition (<>)" $ compose (<>) id
    describe "Left Biased parallel Composition (<|)" $ compose (<|) sort
    describe "Fair parallel Composition (<|>)" $ compose (<|>) sort
    describe "Serial interleaved (<=>)" $ interleaved (<=>)
    describe "Parallel interleaved (<|>)" $ interleaved (<|>)

    describe "Serial loops (<>)" $ loops (<>) id reverse
    describe "Left Biased parallel loops (<|)" $ loops (<|) sort sort
    describe "Fair parallel loops (<|>)" $ loops (<|>) sort sort

bind = do
    it "Simple runAsyncly and 'then' with IO" $
        runAsyncly (liftIO (putStrLn "hello") >> liftIO (putStrLn "world"))
            `shouldReturn` ()
    it "Then and toList" $
        toList (return 1 >> return 2) `shouldReturn` ([2] :: [Int])
    it "Bind and toList" $
        toList (do x <- return 1; y <- return 2; return (x + y))
            `shouldReturn` ([3] :: [Int])

interleaved f =
    it "Interleave four" $
        toList ((return (0 :: Int) <> return 1) `f` (return 100 <> return 101))
            `shouldReturn` ([0, 100, 1, 101])

compose f srt = do
    it "Compose mempty, mempty" $
        (toList (mempty `f` mempty)) `shouldReturn` ([] :: [Int])
    it "Compose empty, empty" $
        (toList (empty `f` empty)) `shouldReturn` ([] :: [Int])
    it "Compose empty at the beginning" $
        (toList $ (empty `f` return 1)) `shouldReturn` ([1] :: [Int])
    it "Compose empty at the end" $
        (toList $ (return 1 `f` empty)) `shouldReturn` ([1] :: [Int])
    it "Compose two" $
        (toList (return 0 `f` return 1) >>= return . srt)
            `shouldReturn` ([0, 1] :: [Int])
    it "Compose three - empty in the middle" $
        ((toList $ (return 0 `f` empty `f` return 1)) >>= return . srt)
            `shouldReturn` ([0, 1] :: [Int])
    it "Compose left associated" $
        ((toList $ (((return 0 `f` return 1) `f` return 2) `f` return 3))
            >>= return . srt) `shouldReturn` ([0, 1, 2, 3] :: [Int])
    it "Compose right associated" $
        ((toList $ (return 0 `f` (return 1 `f` (return 2 `f` return 3))))
            >>= return . srt) `shouldReturn` ([0, 1, 2, 3] :: [Int])
    it "Compose many" $
        ((toList $ forEachWith f [1..100] return) >>= return . srt)
            `shouldReturn` ([1..100] :: [Int])
    it "Compose many (right fold) with bind" $
        (toList (forEachWith f [1..10 :: Int] $ \x -> return x >>= return . id)
            >>= return . srt) `shouldReturn` ([1..10] :: [Int])
    it "Compose many (left fold) with bind" $
        let forL xs k = foldl f empty $ map k xs
         in (toList (forL [1..10 :: Int] $ \x -> return x >>= return . id)
                >>= return . srt) `shouldReturn` ([1..10] :: [Int])
    it "Compose hierarchical (multiple levels)" $
        ((toList $ (((return 0 `f` return 1) `f` (return 2 `f` return 3))
                `f` ((return 4 `f` return 5) `f` (return 6 `f` return 7)))
            ) >>= return . srt) `shouldReturn` ([0..7] :: [Int])

loops f tsrt hsrt = do
    it "Tail recursive loop" $
        (toList (loopTail (0 :: Int)) >>= return . tsrt)
            `shouldReturn` ([0..3] :: [Int])

    it "Head recursive loop" $
        (toList (loopHead (0 :: Int)) >>= return . hsrt)
            `shouldReturn` ([0..3] :: [Int])

    where
        loopHead x = do
            -- this print line is important for the test (causes a bind)
            liftIO $ putStrLn "LoopHead..."
            (if x < 3 then loopHead (x + 1) else empty) `f` return x

        loopTail x = do
            -- this print line is important for the test (causes a bind)
            liftIO $ putStrLn "LoopTail..."
            return x `f` (if x < 3 then loopTail (x + 1) else empty)

    {-
    it "Alternative composition of async and sync tasks" $
        ((wait (threads 0 ((async (return 0) <|> return 1)))) >>= return .  sort)
            `shouldReturn` ([0,1] :: [Int])
    it "Alternative composition of async tasks" $
        (wait (async (return 0) <|> async (return 1)) >>= return . sort)
            `shouldReturn` ([0,1] :: [Int])
    it "Nested async tasks with Alternative" $
        (wait (async (async $ return 0 <|> return 1) <|> return 1) >>= return . sort)
            `shouldReturn` ([0,1,1] :: [Int])
    it "General example" $
        (wait generalExample >>= return . sort)
            `shouldReturn` ([8,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,12,12,12,12,12,13] :: [Int])
    it "General example synchronous" $
        (wait (threads 0 generalExample) >>= return . sort)
            `shouldReturn` ([8,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,12,12,12,12,12,13] :: [Int])
    it "Alternative forks threads - 1" $
        ((wait $ (liftIO myThreadId) <|> (liftIO myThreadId))
            >>= (\x -> print x >> return x)
            >>= return . \x -> (length x, length (nub x)))
        `shouldReturn` ((2, 2) :: (Int, Int))
    it "Alternative forks threads - 2" $
        ((wait $
                (liftIO myThreadId)
            <|> (liftIO myThreadId)
            <|> (liftIO myThreadId))
            >>= (\x -> print x >> return x)
            >>= return . \x -> (length x, length (nub x)))
        `shouldReturn` ((3, 3) :: (Int, Int))
    it "Alternative does not fork when using threads 0" $
        ((wait $ threads 0 $ (liftIO myThreadId) <|> (liftIO myThreadId))
            >>= (\x -> print x >> return x)
            >>= return . \x -> (length x, length (nub x)))
        `shouldReturn` ((2, 1) :: (Int, Int))
    it "async primitive forces a fork at the end of Alternative\
        \ (undesired but that's how the implementation behaves)" $
        ((wait $ threads 0 $ (liftIO myThreadId) <|> async (liftIO myThreadId))
            >>= (\x -> print x >> return x)
            >>= return . \x -> (length x, length (nub x)))
        `shouldReturn` ((2, 2) :: (Int, Int))
    it "async primitive forces a fork at the beginning" $
        ((wait $ threads 0 $ async (liftIO myThreadId) <|> (liftIO myThreadId))
            >>= (\x -> print x >> return x)
            >>= return . \x -> (length x, length (nub x)))
        `shouldReturn` ((2, 2) :: (Int, Int))
    it "async primitive forces a fork in the middle" $
        ((wait $ threads 0 $
            (liftIO myThreadId)
            <|> async (liftIO (threadDelay 1000000) >> liftIO myThreadId)
            <|> (liftIO myThreadId))
          >>= (\x -> print x >> return x)
          >>= return . \x -> (length x, length (nub x)))
        `shouldReturn` ((3, 2) :: (Int, Int))
    it "async primitive forces a fork - 3" $
        ((wait $ threads 0 $
            (liftIO myThreadId)
            <|> async (liftIO (threadDelay 1000000) >> liftIO myThreadId)
            <|> async (liftIO (threadDelay 1000000) >> liftIO myThreadId))
          >>= (\x -> print x >> return x)
          >>= return . \x -> (length x, length (nub x)))
        `shouldReturn` ((3, 3) :: (Int, Int))
    it "async primitive forces a fork - 4" $
        ((wait $ threads 0 $
                async (liftIO (threadDelay 1000000) >> liftIO myThreadId)
            <|> async (liftIO (threadDelay 1000000) >> liftIO myThreadId)
            <|> async (liftIO (threadDelay 1000000) >> liftIO myThreadId))
          >>= (\x -> print x >> return x)
          >>= return . \x -> (length x, length (nub x)))
        `shouldReturn` ((3, 3) :: (Int, Int))

    -- Both 0 and 1 must be printed on console
    it "*>> works as expected" $
        (wait $ (async (liftIO (putStrLn "0") >> return 0)
                   <|> (liftIO (putStrLn "1") >> return 1))
                *>> (liftIO (putStrLn "2") >> return 2))
        `shouldReturn` ([2] :: [Int])

    -- Both 0 and 1 must be printed on console
    it ">>* works as expected" $
        (wait $ (return 2
                 >>* (async (liftIO (putStrLn "0") >> return 0)
                        <|> (liftIO (putStrLn "1") >> return 1))
                )
                >>* return 2
        )
        `shouldReturn` ([2] :: [Int])

    -- 2 and 3 should be printed once and only once
    it ">>| works as expected" $
        ((wait $ ((async (liftIO (putStrLn "0") >> return 0)
                    <|> (liftIO (putStrLn "1") >> return 1))
                 >>| (async (liftIO (putStrLn "2") >> return 2)
                        <|> (liftIO (putStrLn "3") >> return 3))
                )
        ) >>= return . sort)
        `shouldReturn` ([0, 1] :: [Int])
        -}
{-
generalExample :: AsyncT IO Int
generalExample = do
    liftIO $ return ()
    liftIO $ putStr ""
    x <- return 1
    y <- return 2
    z <- do
            x1 <- async (return 1) <|> async (return 2)
            liftIO $ return ()
            liftIO $ putStr ""
            y1 <- (return 1) <|> async (return 2)
            z1 <- do
                x11 <- async (return 1) <|> (return 2)
                y11 <- async (return 1) <|> async (return 2)
                z11 <- async (return 1) <|> (return 2)
                liftIO $ return ()
                liftIO $ putStr ""
                return (x11 + y11 + z11)
            return (x1 + y1 + z1)
    return (x + y + z)
    -}
