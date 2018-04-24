import Control.Concurrent (myThreadId)
import Control.Monad.IO.Class (liftIO)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.Random (randomIO)
import Streamly
import Streamly.Prelude (nil)

main = runStream $ do
    liftIO $ hSetBuffering stdout LineBuffering
    x <- loop "A " 2
    y <- loop "B " 2
    liftIO $ myThreadId >>= putStr . show
             >> putStr " "
             >> print (x, y)

    where

    loop :: String -> Int -> StreamT IO String
    loop name n = do
        rnd <- liftIO (randomIO :: IO Int)
        let result = (name ++ show rnd)
            repeat = if n > 1 then loop name (n - 1) else nil
         in (return result) `parallel` repeat
