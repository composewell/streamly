import Streamly
import Streamly.Prelude as A

main = do
    s <- A.sum $ serially $ forEachWith (<|) [1..100] $ \x -> return (x * x)
    print s
