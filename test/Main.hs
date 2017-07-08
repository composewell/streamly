module Main (main) where

import Asyncly
import Control.Applicative ((<|>))
import Control.Concurrent (myThreadId, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.List (nub, sort)
import Test.Hspec

default (Int)

main :: IO ()
main = hspec $ do
    it "Captures a return value using wait" $
        wait (return 0) `shouldReturn` ([0] :: [Int])
    it "Alternative composition of sync tasks" $
        ((wait $ threads 0 $ (return 0 <|> return 1)) >>= return . sort) `shouldReturn` ([0, 1] :: [Int])
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
