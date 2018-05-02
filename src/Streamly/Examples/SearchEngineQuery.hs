module Streamly.Examples.SearchEngineQuery where

import Streamly
import Streamly.Prelude (once)
import Network.HTTP.Simple

-- Runs three search engine queries in parallel.
searchEngineQuery :: IO ()
searchEngineQuery = do
    putStrLn "Using parallel alternative"
    runStream . parallely $ google <> bing <> duckduckgo

    putStrLn "\nUsing parallel applicative zip"
    runStream . zipParallely $ (,,) <$> google <*> bing <*> duckduckgo

    where
        get :: IsStream t => String -> t IO ()
        get s = once (httpNoBody (parseRequest_ s) >> putStrLn (show s))

        google, bing, duckduckgo :: IsStream t => t IO ()
        google     = get "https://www.google.com/search?q=haskell"
        bing       = get "https://www.bing.com/search?q=haskell"
        duckduckgo = get "https://www.duckduckgo.com/?q=haskell"
