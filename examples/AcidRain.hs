{-# LANGUAGE FlexibleContexts #-}

-- This example is adapted from Gabriel Gonzalez's pipes-concurrency package.
-- https://hackage.haskell.org/package/pipes-concurrency-2.0.8/docs/Pipes-Concurrent-Tutorial.html

import Streamly
import Streamly.Prelude as S
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState, get, modify, runStateT, put)

data Event = Harm Int | Heal Int deriving (Show)

userAction :: MonadAsync m => SerialT m Event
userAction = S.repeatM $ liftIO askUser
    where
    askUser = do
        command <- getLine
        case command of
            "potion" -> return (Heal 10)
            "quit"   -> fail "quit"
            _        -> putStrLn "What?" >> askUser

acidRain :: MonadAsync m => SerialT m Event
acidRain = asyncly $ constRate 1 $ S.repeatM $ liftIO $ return $ Harm 1

game :: (MonadAsync m, MonadState Int m) => SerialT m ()
game = do
    event <- userAction `parallel` acidRain
    case event of
        Harm n -> modify $ \h -> h - n
        Heal n -> modify $ \h -> h + n

    h <- get
    when (h <= 0) $ fail "You die!"
    liftIO $ putStrLn $ "Health = " <> show h

main :: IO ()
main = do
    putStrLn "Your health is deteriorating due to acid rain,\
             \ type \"potion\" or \"quit\""
    _ <- runStateT (runStream game) 60
    return ()
