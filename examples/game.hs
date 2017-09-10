{-# LANGUAGE FlexibleContexts #-}

-- This example is adapted from Gabriel Gonzalez's pipes-concurrency package.
-- https://hackage.haskell.org/package/pipes-concurrency-2.0.8/docs/Pipes-Concurrent-Tutorial.html

import Asyncly
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.State
import Data.Semigroup (cycle1)

data Event = Harm Int | Heal Int | Quit deriving (Show)

userAction :: MonadIO m => AsyncT m Event
userAction = cycle1 $ liftIO askUser
    where
    askUser = do
        command <- getLine
        case command of
            "potion" -> return (Heal 10)
            "quit"   -> return  Quit
            _        -> putStrLn "What?" >> askUser

acidRain :: MonadIO m => AsyncT m Event
acidRain = cycle1 $ liftIO (threadDelay 1000000) >> return (Harm 1)

game :: (MonadAsync m, MonadState Int m) => AsyncT m ()
game = do
    event <- userAction <|> acidRain
    case event of
        Harm n -> modify $ \h -> h - n
        Heal n -> modify $ \h -> h + n
        Quit   -> error "quit"

    h <- get
    when (h <= 0) $ error "You die!"
    liftIO $ putStrLn $ "Health = " ++ show h

main = do
    putStrLn "Your health is deteriorating due to acid rain,\
             \ type \"potion\" or \"quit\""
    runStateT (runAsyncly game) 60
