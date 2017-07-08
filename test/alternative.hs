import Control.Concurrent (threadDelay, myThreadId)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomIO)
import System.IO
import Control.Applicative

import Asyncly

main = wait_ $ threads 1 $ do
    liftIO $ hSetBuffering stdout LineBuffering
    mainThread <- liftIO myThreadId
    liftIO $ putStrLn $ "Main thread: " ++ show mainThread

    eventA <|> eventB

eventA = do
    x <- liftIO (randomIO :: IO Int)
    evThread <- liftIO myThreadId
    liftIO $ putStrLn $ "X Event thread: " ++ show evThread
    liftIO $ putStrLn $ "x = " ++ (show x)
    return x

eventB = do
    y <- liftIO (randomIO :: IO Int)

    -- liftIO $ threadDelay 10000000
    evThread <- liftIO myThreadId
    liftIO $ putStrLn $ "Y Event thread: " ++ show evThread
    liftIO $ putStrLn $ "y = " ++ (show y)
    return y
