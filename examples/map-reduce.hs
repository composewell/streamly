import Control.Applicative ((<|>), empty)
import Control.Monad.IO.Class (liftIO)
import Data.List (sum)
import Asyncly

main = do
    squares <- wait $ do
        x <- foldl (<|>) empty $ map return [1..100]
        return (x * x)
    print . sum $ squares
