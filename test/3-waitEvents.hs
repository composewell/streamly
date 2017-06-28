import Control.Concurrent (threadDelay, myThreadId)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomIO)
import System.IO

import Strands

--- Check space leaks

main = wait_ $ do
    x <- waitEvents (randomIO :: IO Int)
    y <- waitEvents (randomIO :: IO Int)
    return (x, y)
