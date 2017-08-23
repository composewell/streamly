import Control.Applicative ((<|>), empty)
import Control.Concurrent (myThreadId)
import Control.Monad.IO.Class (liftIO)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.Random (randomIO)
import Asyncly

main = runAsyncly $ do
    liftIO $ hSetBuffering stdout LineBuffering
    x <- loop "A " 2
    y <- loop "B " 2
    liftIO $ myThreadId >>= putStr . show
             >> putStr " "
             >> print (x, y)

    where

    loop name n = do
        rnd <- liftIO (randomIO :: IO Int)
        let result = (name ++ show rnd)
            repeat = if n > 1 then loop name (n - 1) else empty
         in (return result) <|> repeat
