-- |
-- Module      : Streamly.Test.Data.Fold
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Main (main) where

import qualified Streamly.Test.Data.Fold.Combinators as Combinators
import qualified Streamly.Test.Data.Fold.Container as Container
import qualified Streamly.Test.Data.Fold.Exception as Exception
import qualified Streamly.Test.Data.Fold.Tee as Tee
import qualified Streamly.Test.Data.Fold.Type as Type
import qualified Streamly.Test.Data.Fold.Window as Window

main :: IO ()
main = do
    Type.main
    Combinators.main
    Container.main
    Window.main
    Exception.main
    Tee.main
