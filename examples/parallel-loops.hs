import Control.Concurrent (myThreadId, threadDelay)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.Random (randomIO)
import Streamly

main = runStream $ do
    fromIO $ hSetBuffering stdout LineBuffering
    x <- loop "A" `parallel` loop "B"
    fromIO $ myThreadId >>= putStr . show
             >> putStr " "
             >> print x

    where

    loop :: String -> Stream (String, Int)
    loop name = do
        fromIO $ threadDelay 1000000
        rnd <- fromIO (randomIO :: IO Int)
        return (name, rnd) `parallel` loop name
