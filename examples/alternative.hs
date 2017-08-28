import Control.Concurrent (threadDelay, myThreadId)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomIO)
import System.IO
import Control.Applicative

import Asyncly

main = runAsyncly $ do
    liftIO $ hSetBuffering stdout LineBuffering
    mainThread <- liftIO myThreadId
    liftIO $ putStrLn $ "Main thread: " ++ show mainThread

    eventA <|> eventB

eventA = do
    liftIO $ threadDelay 1000000
    x <- liftIO (randomIO :: IO Int)
    evThread <- liftIO myThreadId
    liftIO $ putStrLn $ "A Event thread: " ++ show evThread
    liftIO $ putStrLn $ "a = " ++ (show x)
    return x

eventB = do
    y <- liftIO (randomIO :: IO Int)

    -- liftIO $ threadDelay 10000000
    evThread <- liftIO myThreadId
    liftIO $ putStrLn $ "B Event thread: " ++ show evThread
    liftIO $ putStrLn $ "b = " ++ (show y)
    return y
