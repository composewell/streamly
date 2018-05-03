import Control.Concurrent (myThreadId, threadDelay)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.Random (randomIO)
import Streamly
import Streamly.Prelude (once)

main = runStream $ do
    once $ hSetBuffering stdout LineBuffering
    x <- loop "A" `parallel` loop "B"
    once $ myThreadId >>= putStr . show
             >> putStr " "
             >> print x

    where

    loop :: String -> Stream (String, Int)
    loop name = do
        once $ threadDelay 1000000
        rnd <- once (randomIO :: IO Int)
        return (name, rnd) `parallel` loop name
