#!/usr/bin/env stack
-- stack runghc

import Asyncly
import Asyncly.Prelude (ZipAsync (..))
import Network.HTTP.Simple

-- Runs three search engine queries in parallel.
main = do
    putStrLn "Using parallel alternative"
    runAsyncly $ google <|> bing <|> duckduckgo

    putStrLn "\nUsing parallel applicative zip"
    runAsyncly $ getZipAsync $
        (,,) <$> ZipAsync google <*> ZipAsync bing <*> ZipAsync duckduckgo

    where
        get s = liftIO (httpNoBody (parseRequest_ s) >> putStrLn (show s))
        google     = get "https://www.google.com/search?q=haskell"
        bing       = get "https://www.bing.com/search?q=haskell"
        duckduckgo = get "https://www.duckduckgo.com/?q=haskell"
