import Control.Concurrent (threadDelay, myThreadId)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomIO)
import System.IO

import Asyncly

--- Check space leaks

main = do
    x <- wait $ do
        y <- gather $ each [1..10]
        liftIO $ print y
    print x
