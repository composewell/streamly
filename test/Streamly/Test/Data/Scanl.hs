-- |
-- Module      : Streamly.Test.Data.Scanl
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Main (main) where

import qualified Streamly.Test.Data.Scanl.Combinators as Combinators
import qualified Streamly.Test.Data.Scanl.Container as Container
import qualified Streamly.Test.Data.Scanl.Type as Type
import qualified Streamly.Test.Data.Scanl.Window as Window

main :: IO ()
main = do
    Type.main
    Combinators.main
    Container.main
    Window.main
