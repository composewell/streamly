import Control.Concurrent (threadDelay, myThreadId)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomIO)
import System.IO

import Strands

main = waitAsync $ threads 3 $ do
    liftIO $ hSetBuffering stdout LineBuffering
    mainThread <- liftIO myThreadId
    liftIO $ putStrLn $ "Main thread: " ++ show mainThread
    x <- sample (randomIO :: IO Int) 1000000
    evThread <- liftIO myThreadId
    liftIO $ putStrLn $ "X Event thread: " ++ show evThread
    liftIO $ putStrLn $ "x = " ++ (show x)

    y <- sample (randomIO :: IO Int) 1000000

    -- liftIO $ threadDelay 10000000
    evThread <- liftIO myThreadId
    liftIO $ putStrLn $ "Y Event thread: " ++ show evThread
    liftIO $ putStrLn $ "y = " ++ (show y)
