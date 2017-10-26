import Data.List (sum)
import Asyncly
import Asyncly.Prelude

main = do
    xs <- toList $ serially $ forEachWith (<|) [1..100] $ \x -> return (x * x)
    print . sum $ xs
