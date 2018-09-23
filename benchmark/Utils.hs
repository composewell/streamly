module Utils where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe

-- Like the shell "shift" to shift the command line arguments
shift :: StateT [String] (MaybeT IO) (Maybe String)
shift = do
    s <- get
    case s of
        [] -> return Nothing
        x : xs -> put xs >> return (Just x)

