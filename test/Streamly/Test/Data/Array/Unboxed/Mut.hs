module Streamly.Test.Data.Array.Unboxed.Mut (main) where

import Test.QuickCheck (listOf)

import Streamly.Test.Common (chooseInt)
import Test.Hspec (hspec, describe)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, Property)
import Test.QuickCheck.Monadic (monadicIO, assert)

import qualified Streamly.Internal.Data.Array.Unboxed.Mut as MArray
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Test.Hspec as Hspec

maxTestCount :: Int
maxTestCount = 100

moduleName :: String
moduleName = "Data.Array.Unboxed.Mut"

testAppend ::  Property
testAppend =
   forAll (listOf (chooseInt (-50, 100))) $ \ls0 ->
        monadicIO $ action ls0

        where

        action ls = do
            x <- Stream.fold
                    (MArray.append (MArray.newArray 0))
                    (Stream.fromList (ls::[Int]))
            lst <- MArray.toList x
            assert (ls == lst)

main :: IO ()
main =
    hspec $
    Hspec.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
        describe moduleName $ do
            describe "Stream Append" $ do
                prop "testAppend" testAppend
