-- |
-- Module      : Streamly.Test.Data.Stream.Transformer
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Transformer (main) where

import Control.Monad.Trans.Identity (runIdentityT)
import Streamly.Internal.Data.Stream (Stream)
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Hspec as H

checkFoldrTLaziness :: IO ()
checkFoldrTLaziness =
    runIdentityT (Stream.foldrT (\x xs -> if odd x then return True else xs)
                        (return False)
                        (Stream.fromList (2:4:5:undefined) :: Stream IO Int))
        `shouldReturn` True

moduleName :: String
moduleName = "Data.Stream.Transformer"

main :: IO ()
main = hspec
  $ H.parallel
  $ describe moduleName $ do
    it "foldrT is lazy enough" checkFoldrTLaziness
