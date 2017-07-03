module Main (main) where

import Test.Hspec

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Asyncly
import Data.List (sort)

default (Int)

main :: IO ()
main = hspec $ do
    it "Captures a return value using wait" $
        wait (return 0) `shouldReturn` ([0] :: [Int])
    it "Alternative composition of sync tasks" $
        wait (return 0 <|> return 1) `shouldReturn` ([0] :: [Int])
    it "Creates a single async thread" $
        wait (async $ return 0) `shouldReturn` ([0] :: [Int])
    it "async ignoring return value" $
        wait (async (return 0 :: AsyncT IO Int) >> return 1)
            `shouldReturn` ([1] :: [Int])
    it "Alternative composition of async and sync tasks" $
        (wait (async (return 0) <|> return 1) >>= return . sort)
            `shouldReturn` ([0,1] :: [Int])
    it "Alternative composition of async tasks" $
        (wait (async (return 0) <|> async (return 1)) >>= return . sort)
            `shouldReturn` ([0,1] :: [Int])
    it "Nested async tasks" $
        (wait $ (async $ async $ return 0)) `shouldReturn` ([0] :: [Int])
    it "Nested async tasks ignoring return values" $
        (wait $ (async $ async $ return 0 :: AsyncT IO Int) >> return 1)
            `shouldReturn` ([1] :: [Int])
    it "Nested async tasks with Alternative" $
        (wait (async (async $ return 0) <|> return 1) >>= return . sort)
            `shouldReturn` ([0,1] :: [Int])
    it "General example" $
        (wait generalExample >>= return . sort)
            `shouldReturn` ([7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10] :: [Int])
    it "General example synchronous" $
        (wait (threads 0 generalExample) >>= return . sort)
            `shouldReturn` ([7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10] :: [Int])

generalExample :: AsyncT IO Int
generalExample = do
    liftIO $ return ()
    liftIO $ print "hello"
    x <- async (return 1)
    y <- async (return 2)
    z <- do
            x1 <- async (return 1) <|> async (return 2)
            liftIO $ return ()
            liftIO $ print "hello"
            y1 <- (return 1) <|> async (return 2)
            z1 <- do
                x11 <- async (return 1) <|> (return 2)
                y11 <- async (return 1) <|> async (return 2)
                z11 <- async (return 1) <|> (return 2)
                liftIO $ return ()
                liftIO $ print "hello"
                return (x11 + y11)
            return (x1 + y1 + z1)
    return (x + y + z)
