{-# LANGUAGE CPP #-}

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif
import Streamly.Prelude (drain, nil, yieldM, (|:))
import Network.HTTP.Simple

import qualified Streamly.Prelude as S

-- | Runs three search engine queries in parallel and prints the search engine
-- names in the fastest first order.
--
-- Does it twice using two different ways.
--
main :: IO ()
main = do
    putStrLn "Using parallel stream construction"
    drain . S.parallely $ google |: bing |: duckduckgo |: nil

    putStrLn "\nUsing parallel semigroup composition"
    drain . S.parallely $ yieldM google <> yieldM bing <> yieldM duckduckgo

    putStrLn "\nUsing parallel applicative zip"
    drain . S.zipAsyncly $
        (,,) <$> yieldM google <*> yieldM bing <*> yieldM duckduckgo

    where
        get :: String -> IO ()
        get s = httpNoBody (parseRequest_ s) >> print s

        google, bing, duckduckgo :: IO ()
        google     = get "https://www.google.com/search?q=haskell"
        bing       = get "https://www.bing.com/search?q=haskell"
        duckduckgo = get "https://www.duckduckgo.com/?q=haskell"
