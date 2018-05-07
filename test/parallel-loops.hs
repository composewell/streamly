import Control.Concurrent (myThreadId, threadDelay)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.Random (randomIO)
import Streamly
import qualified Streamly.Prelude as S

main = do
    hSetBuffering stdout LineBuffering
    runStream $ do
        x <- S.take 10 $ loop "A" `coparAhead` loop "B"
        S.once $ myThreadId >>= putStr . show
               >> putStr " got "
               >> print x

    where

    -- we can just use
    -- parallely $ cycle1 $ once (...)
    loop :: String -> Stream (String, Int)
    loop name = do
        S.once $ threadDelay 1000000
        rnd <- S.once (randomIO :: IO Int)
        S.once $ myThreadId >>= putStr . show
               >> putStr " yielding "
               >> print rnd
        return (name, rnd) `coparAhead` loop name
