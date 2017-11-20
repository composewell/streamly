module Streamly.Examples.SearchEngineQuery where

import Streamly
import Network.HTTP.Simple

-- Runs three search engine queries in parallel.
searchEngineQuery :: IO ()
searchEngineQuery = do
    putStrLn "Using parallel alternative"
    runStreamT $ google <|> bing <|> duckduckgo

    putStrLn "\nUsing parallel applicative zip"
    runZipAsync $ (,,) <$> pure google <*> pure bing <*> pure duckduckgo

    where
        get s = liftIO (httpNoBody (parseRequest_ s) >> putStrLn (show s))
        google     = get "https://www.google.com/search?q=haskell"
        bing       = get "https://www.bing.com/search?q=haskell"
        duckduckgo = get "https://www.duckduckgo.com/?q=haskell"
