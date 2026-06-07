-- |
-- Module      : Streamly.Test.Data.Stream
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream (main) where

import qualified Streamly.Test.Data.Stream.Container as Container
import qualified Streamly.Test.Data.Stream.Nesting as Nesting
import qualified Streamly.Test.Data.Stream.Parse as Parse
import qualified Streamly.Test.Data.Stream.Serial as Serial
import qualified Streamly.Test.Data.Stream.Top as Top
import qualified Streamly.Test.Data.Stream.Transform as Transform
import qualified Streamly.Test.Data.Stream.Transformer as Transformer
import qualified Streamly.Test.Data.Stream.Type as Type

main :: IO ()
main = do
    Type.main
    Nesting.main
    Container.main
    Parse.main
    Top.main
    Transform.main
    Transformer.main
    Serial.main
