module Streamly.Test.Data.Array.TreeArray (main) where

import Streamly.Internal.Data.Unboxed (sizeOf)

import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary, choose)
import Test.QuickCheck.Monadic (monadicIO, assert)
import Test.Hspec as H

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Array.Tree.Type as T

-- Coverage build takes too long with default number of tests
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

-- helper methods
allocOverhead :: Int
allocOverhead = 2 * sizeOf (undefined :: Int)

-- XXX this should be in sync with the defaultChunkSize in Array code, or we
-- should expose that and use that. For fast testing we could reduce the
-- defaultChunkSize under CPP conditionals.
--
defaultChunkSize :: Int
defaultChunkSize = 32 * k - allocOverhead
   where k = 1024

maxArrLen :: Int
maxArrLen = defaultChunkSize * 8

moduleName :: String
moduleName = "Data.Array.TreeArray"

testAppend :: Property
testAppend =
  forAll (choose (0, maxArrLen)) $ \len ->
    forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
      monadicIO $ do
        arr <- S.fold (T.appendN T.newArray $ length list) $ S.fromList list
        assert (T.count arr == length list)

testRead :: Property
testRead =
  forAll (choose (0, 7)) $ \len ->
    forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
      monadicIO $ do
        arr <- S.fold (T.appendN T.newArray $ length list) $ S.fromList list
        readArr <- S.toList $ S.unfold T.read arr
        assert (readArr == list)

main :: IO ()
main =
  hspec $
    H.parallel $
      modifyMaxSuccess (const maxTestCount) $ do
        describe moduleName $ do
          describe "Construction" $ do
            prop "testAppend" testAppend
            prop "testRead" testRead
