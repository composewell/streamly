import Control.Concurrent (threadDelay, myThreadId)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomIO)
import System.IO

import Strands

main = do
    hSetBuffering stdout LineBuffering
    xs <- waitAsync $ threads 4 $ do
        liftIO $ hSetBuffering stdout LineBuffering
        mainThread <- liftIO myThreadId
        liftIO $ putStrLn $ "Main thread: " ++ show mainThread
        x <- async (randomIO :: IO Int)
        liftIO $ putStrLn $ show x

        y <- async (randomIO :: IO Int)

        liftIO $ threadDelay 1000000
        evThread <- liftIO myThreadId
        liftIO $ putStrLn $ "Event thread: " ++ show evThread
        liftIO $ putStrLn $ show y
        return (x, y)
    putStrLn $ show xs
