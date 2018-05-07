import Control.Concurrent (myThreadId)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.Random (randomIO)
import Streamly
import Streamly.Prelude (nil, once)

main = runStream $ do
    once $ hSetBuffering stdout LineBuffering
    x <- loop "A " 2
    y <- loop "B " 2
    once $ myThreadId >>= putStr . show
             >> putStr " "
             >> print (x, y)

    where

    -- we can just use
    -- parallely $ mconcat $ replicate n $ once (...)
    loop :: String -> Int -> StreamT IO String
    loop name n = do
        rnd <- once (randomIO :: IO Int)
        let result = (name ++ show rnd)
            repeat = if n > 1 then loop name (n - 1) else nil
         in (return result) `coparAhead` repeat
