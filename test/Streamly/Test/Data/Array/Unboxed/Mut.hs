module Streamly.Test.Data.Array.Unboxed.Mut (main) where

import Test.QuickCheck (listOf)

import GHC.Int (Int32(..), Int64(..))
import GHC.Word (Word32(..), Word64(..), Word8(..))
import Streamly.Internal.Data.Unboxed (Unboxed)
import Streamly.Test.Common (chooseInt)
import Test.Hspec (hspec, describe, it, shouldReturn)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, Property)
import Test.QuickCheck.Monadic (monadicIO, assert)

import qualified Data.Char as Char (chr)
import qualified Streamly.Internal.Data.Array.Unboxed.Mut as MArray
import qualified Streamly.Internal.Data.Stream as Stream
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

toListFromListId :: (Eq a, Show a, Unboxed a) => [a] -> IO ()
toListFromListId lst = do
    arr <- MArray.fromList lst
    MArray.toList arr `shouldReturn` lst

-- XXX This should be in test/Data.Unbox
testUnbox :: Hspec.SpecWith ()
testUnbox = do
    let listChr = [1 .. 100]
        list = [-100 .. 100]
    describe "Array.toList . Array.fromList == id" $ do
        it "Char" $ toListFromListId (Char.chr <$> listChr)
        it "Int32" $ toListFromListId ((fromIntegral :: Int -> Int32) <$> list)
        it "Int" $ toListFromListId list
        it "Int64"
            $ toListFromListId ((fromIntegral :: Int -> Int64) <$> list)
        it "Word"
            $ toListFromListId ((fromIntegral :: Int -> Word) <$> list)
        it "Word8"
            $ toListFromListId ((fromIntegral :: Int -> Word8) <$> list)
        it "Word32"
            $ toListFromListId ((fromIntegral :: Int -> Word32) <$> list)
        it "Word64"
            $ toListFromListId ((fromIntegral :: Int -> Word64) <$> list)
        it "Double"
            $ toListFromListId ((fromIntegral :: Int -> Double) <$> list)

main :: IO ()
main =
    hspec $
    Hspec.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
        describe moduleName $ do
            describe "Stream Append" $ do
                prop "testAppend" testAppend
            testUnbox
