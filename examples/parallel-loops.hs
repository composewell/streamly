import Control.Concurrent (myThreadId, threadDelay)
import Control.Monad.IO.Class (liftIO)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.Random (randomIO)
import Streamly

main = runStream $ do
    liftIO $ hSetBuffering stdout LineBuffering
    x <- loop "A" `parallel` loop "B"
    liftIO $ myThreadId >>= putStr . show
             >> putStr " "
             >> print x

    where

    loop :: String -> StreamT IO (String, Int)
    loop name = do
        liftIO $ threadDelay 1000000
        rnd <- liftIO (randomIO :: IO Int)
        return (name, rnd) `parallel` loop name
