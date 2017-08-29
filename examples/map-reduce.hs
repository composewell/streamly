import Data.List (sum)
import Asyncly

main = do
    xs <- toList $ forEachWith (|>) [1..100] $ \x -> return (x * x) :: AsyncT IO Int
    print . sum $ xs
