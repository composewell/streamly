{-# LANGUAGE FlexibleContexts #-}

-- This example is adapted from Gabriel Gonzalez's pipes-concurrency package.
-- https://hackage.haskell.org/package/pipes-concurrency-2.0.8/docs/Pipes-Concurrent-Tutorial.html

module Streamly.Examples.AcidRainGame where

import Streamly
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.State (MonadState, get, modify, runStateT)
import Data.Semigroup (cycle1)

data Event = Harm Int | Heal Int | Quit deriving (Show)

userAction :: MonadIO m => StreamT m Event
userAction = cycle1 $ fromIO askUser
    where
    askUser = do
        command <- getLine
        case command of
            "potion" -> return (Heal 10)
            "quit"   -> return  Quit
            _        -> putStrLn "What?" >> askUser

acidRain :: MonadIO m => StreamT m Event
acidRain = cycle1 $ fromIO (threadDelay 1000000) >> return (Harm 1)

game :: (MonadParallel m, MonadState Int m) => StreamT m ()
game = do
    event <- userAction `parallel` acidRain
    case event of
        Harm n -> modify $ \h -> h - n
        Heal n -> modify $ \h -> h + n
        Quit   -> fail "quit"

    h <- get
    when (h <= 0) $ fail "You die!"
    fromIO $ putStrLn $ "Health = " ++ show h

acidRainGame :: IO ()
acidRainGame = do
    putStrLn "Your health is deteriorating due to acid rain,\
             \ type \"potion\" or \"quit\""
    _ <- runStateT (runStream game) 60
    return ()
