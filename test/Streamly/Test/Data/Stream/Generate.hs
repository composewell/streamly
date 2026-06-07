-- |
-- Module      : Streamly.Test.Data.Stream.Generate
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Generate (main) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word8, Word16)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Storable (poke)
import GHC.Ptr (Ptr(..))

import qualified Streamly.Internal.Data.Stream as Stream

import Test.Hspec

toList :: Monad m => Stream.Stream m a -> m [a]
toList = Stream.toList

-------------------------------------------------------------------------------
-- Primitives
-------------------------------------------------------------------------------

testCons :: Expectation
testCons =
    toList (Stream.cons 1 (Stream.fromList [2, 3 :: Int]))
        `shouldReturn` [1, 2, 3]

testConsEmpty :: Expectation
testConsEmpty =
    toList (Stream.cons (1 :: Int) Stream.nil)
        `shouldReturn` [1]

-------------------------------------------------------------------------------
-- Unfolding
-------------------------------------------------------------------------------

testUnfoldrM :: Expectation
testUnfoldrM = do
    let step b =
            if b > 2
            then return Nothing
            else return (Just (b, b + 1))
    toList (Stream.unfoldrM step 0) `shouldReturn` [0, 1, 2 :: Int]

testUnfoldrMEmpty :: Expectation
testUnfoldrMEmpty = do
    toList (Stream.unfoldrM (const (return Nothing)) (0 :: Int))
        `shouldReturn` ([] :: [Int])

-------------------------------------------------------------------------------
-- From Generators
-------------------------------------------------------------------------------

testGenerate :: Expectation
testGenerate =
    toList (Stream.generate 5 id) `shouldReturn` [0, 1, 2, 3, 4 :: Int]

testGenerateM :: Expectation
testGenerateM =
    toList (Stream.generateM 3 (\i -> return (i * 2 :: Int)))
        `shouldReturn` [0, 2, 4]

testGenerateEmpty :: Expectation
testGenerateEmpty =
    toList (Stream.generate 0 id) `shouldReturn` ([] :: [Int])

testGenerateSideEffect :: Expectation
testGenerateSideEffect = do
    ref <- newIORef (0 :: Int)
    let act i = do
            writeIORef ref (i + 1)
            return i
    xs <- toList (Stream.generateM 3 act)
    xs `shouldBe` [0, 1, 2]
    readIORef ref `shouldReturn` 3

-------------------------------------------------------------------------------
-- From Containers
-------------------------------------------------------------------------------

testFromFoldableM :: Expectation
testFromFoldableM =
    toList (Stream.fromFoldableM [return 1, return 2, return 3 :: IO Int])
        `shouldReturn` [1, 2, 3]

testFromFoldableMEmpty :: Expectation
testFromFoldableMEmpty =
    toList (Stream.fromFoldableM ([] :: [IO Int]))
        `shouldReturn` []

testFromFoldableMSideEffect :: Expectation
testFromFoldableMSideEffect = do
    ref <- newIORef ([] :: [Int])
    let act i = do
            old <- readIORef ref
            writeIORef ref (old ++ [i])
            return i
    xs <- toList (Stream.fromFoldableM [act 1, act 2, act 3])
    xs `shouldBe` [1, 2, 3]
    readIORef ref `shouldReturn` [1, 2, 3]

-------------------------------------------------------------------------------
-- From Pointers
-------------------------------------------------------------------------------

testFromPtr :: Expectation
testFromPtr =
    alloca $ \p -> do
        poke p (42 :: Word8)
        xs <- toList (Stream.take 1 (Stream.fromPtr p))
        xs `shouldBe` [42]

testFromPtrN :: Expectation
testFromPtrN =
    withArray [10, 20, 30 :: Word8] $ \p -> do
        xs <- toList (Stream.fromPtrN 3 p)
        xs `shouldBe` [10, 20, 30]

testFromPtrNEmpty :: Expectation
testFromPtrNEmpty =
    withArray [1 :: Word8] $ \p -> do
        xs <- toList (Stream.fromPtrN 0 p)
        xs `shouldBe` []

testFromCString :: Expectation
testFromCString = do
    xs <- toList (Stream.fromCString# "\1\2\3\0"#)
    xs `shouldBe` [1, 2, 3 :: Word8]

testFromCStringEmpty :: Expectation
testFromCStringEmpty = do
    xs <- toList (Stream.fromCString# "\0"#)
    xs `shouldBe` ([] :: [Word8])

testFromW16CString :: Expectation
testFromW16CString =
    withArray [1, 2, 3, 0 :: Word16] $ \(Ptr addr#) -> do
        xs <- toList (Stream.fromW16CString# addr#)
        xs `shouldBe` [1, 2, 3]

testFromW16CStringEmpty :: Expectation
testFromW16CStringEmpty =
    withArray [0 :: Word16] $ \(Ptr addr#) -> do
        xs <- toList (Stream.fromW16CString# addr#)
        xs `shouldBe` []

-------------------------------------------------------------------------------
-- Enumeration primitives
-------------------------------------------------------------------------------

testEnumerateFromBounded :: Expectation
testEnumerateFromBounded =
    toList (Stream.take 5 (Stream.enumerateFromBounded (0 :: Int)))
        `shouldReturn` [0, 1, 2, 3, 4]

testEnumerateFromIntegral :: Expectation
testEnumerateFromIntegral =
    toList (Stream.take 5 (Stream.enumerateFromIntegral (0 :: Int)))
        `shouldReturn` [0, 1, 2, 3, 4]

testEnumerateFromNum :: Expectation
testEnumerateFromNum =
    toList (Stream.take 5 (Stream.enumerateFromNum (0 :: Int)))
        `shouldReturn` [0, 1, 2, 3, 4]

testEnumerateFromStepNum :: Expectation
testEnumerateFromStepNum =
    toList (Stream.take 5 (Stream.enumerateFromStepNum (0 :: Int) 3))
        `shouldReturn` [0, 3, 6, 9, 12]

testEnumerateFromStepIntegral :: Expectation
testEnumerateFromStepIntegral =
    toList (Stream.take 5 (Stream.enumerateFromStepIntegral (0 :: Int) 2))
        `shouldReturn` [0, 2, 4, 6, 8]

testEnumerateFromThenNum :: Expectation
testEnumerateFromThenNum =
    toList (Stream.take 5 (Stream.enumerateFromThenNum (0 :: Int) 3))
        `shouldReturn` [0, 3, 6, 9, 12]

testEnumerateFromThenIntegral :: Expectation
testEnumerateFromThenIntegral = do
    toList (Stream.take 4 (Stream.enumerateFromThenIntegral (0 :: Int) 2))
        `shouldReturn` [0, 2, 4, 6]
    toList (Stream.take 4 (Stream.enumerateFromThenIntegral (0 :: Int) (-2)))
        `shouldReturn` [0, -2, -4, -6]

testEnumerateFromThenFractional :: Expectation
testEnumerateFromThenFractional =
    toList (Stream.take 4 (Stream.enumerateFromThenFractional (1.0 :: Double) 2.0))
        `shouldReturn` [1.0, 2.0, 3.0, 4.0]

testEnumerateFromThenToIntegral :: Expectation
testEnumerateFromThenToIntegral = do
    toList (Stream.enumerateFromThenToIntegral (0 :: Int) 2 6)
        `shouldReturn` [0, 2, 4, 6]
    toList (Stream.enumerateFromThenToIntegral (0 :: Int) (-2) (-6))
        `shouldReturn` [0, -2, -4, -6]

testEnumerateFromThenToFractional :: Expectation
testEnumerateFromThenToFractional =
    toList (Stream.enumerateFromThenToFractional (0.1 :: Double) 2.0 6.0)
        `shouldReturn` [0.1, 2.0, 3.9, 5.799999999999999]

testEnumerateFromThenSmallBounded :: Expectation
testEnumerateFromThenSmallBounded =
    toList (Stream.take 4 (Stream.enumerateFromThenSmallBounded 'a' 'c'))
        `shouldReturn` "aceg"

testEnumerateFromToSmall :: Expectation
testEnumerateFromToSmall =
    toList (Stream.enumerateFromToSmall 'a' 'e')
        `shouldReturn` "abcde"

testEnumerateFromThenToSmall :: Expectation
testEnumerateFromThenToSmall =
    toList (Stream.enumerateFromThenToSmall 'a' 'c' 'g')
        `shouldReturn` "aceg"

testEnumerateFromFractional :: Expectation
testEnumerateFromFractional =
    toList (Stream.take 4 (Stream.enumerateFromFractional (1.1 :: Double)))
        `shouldReturn` [1.1, 2.1, 3.1, 4.1]

testEnumerateFromToFractional :: Expectation
testEnumerateFromToFractional =
    toList (Stream.enumerateFromToFractional (1.1 :: Double) 4.0)
        `shouldReturn` [1.1, 2.1, 3.1, 4.1]

-------------------------------------------------------------------------------
-- Time Enumeration (smoke tests - verify elements are produced)
-------------------------------------------------------------------------------

testTimesWith :: Expectation
testTimesWith = do
    xs <- toList (Stream.take 3 (Stream.timesWith 0.01))
    length xs `shouldBe` 3

testTimes :: Expectation
testTimes = do
    xs <- toList (Stream.take 3 Stream.times)
    length xs `shouldBe` 3

testRelTimesWith :: Expectation
testRelTimesWith = do
    xs <- toList (Stream.take 3 (Stream.relTimesWith 0.01))
    length xs `shouldBe` 3

testRelTimes :: Expectation
testRelTimes = do
    xs <- toList (Stream.take 3 Stream.relTimes)
    length xs `shouldBe` 3

testAbsTimesWith :: Expectation
testAbsTimesWith = do
    xs <- toList (Stream.take 3 (Stream.absTimesWith 0.01))
    length xs `shouldBe` 3

testAbsTimes :: Expectation
testAbsTimes = do
    xs <- toList (Stream.take 3 Stream.absTimes)
    length xs `shouldBe` 3

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Stream.Generate"

main :: IO ()
main = hspec $ describe moduleName $ do
    describe "Primitives" $ do
        it "cons" testCons
        it "cons empty tail" testConsEmpty

    describe "Unfolding" $ do
        it "unfoldrM" testUnfoldrM
        it "unfoldrM empty" testUnfoldrMEmpty

    describe "From Generators" $ do
        it "generate" testGenerate
        it "generateM" testGenerateM
        it "generate empty" testGenerateEmpty
        it "generateM side effects" testGenerateSideEffect

    describe "From Containers" $ do
        it "fromFoldableM" testFromFoldableM
        it "fromFoldableM empty" testFromFoldableMEmpty
        it "fromFoldableM side effects" testFromFoldableMSideEffect

    describe "From Pointers" $ do
        it "fromPtr" testFromPtr
        it "fromPtrN" testFromPtrN
        it "fromPtrN zero length" testFromPtrNEmpty
        it "fromCString#" testFromCString
        it "fromCString# empty" testFromCStringEmpty
        it "fromW16CString#" testFromW16CString
        it "fromW16CString# empty" testFromW16CStringEmpty

    describe "Enumeration Primitives" $ do
        it "enumerateFromBounded" testEnumerateFromBounded
        it "enumerateFromIntegral" testEnumerateFromIntegral
        it "enumerateFromNum" testEnumerateFromNum
        it "enumerateFromStepNum" testEnumerateFromStepNum
        it "enumerateFromStepIntegral" testEnumerateFromStepIntegral
        it "enumerateFromThenNum" testEnumerateFromThenNum
        it "enumerateFromThenIntegral" testEnumerateFromThenIntegral
        it "enumerateFromThenFractional" testEnumerateFromThenFractional
        it "enumerateFromThenToIntegral" testEnumerateFromThenToIntegral
        it "enumerateFromThenToFractional" testEnumerateFromThenToFractional
        it "enumerateFromThenSmallBounded" testEnumerateFromThenSmallBounded
        it "enumerateFromToSmall" testEnumerateFromToSmall
        it "enumerateFromThenToSmall" testEnumerateFromThenToSmall
        it "enumerateFromFractional" testEnumerateFromFractional
        it "enumerateFromToFractional" testEnumerateFromToFractional

    describe "Time Enumeration" $ do
        it "timesWith produces elements" testTimesWith
        it "times produces elements" testTimes
        it "relTimesWith produces elements" testRelTimesWith
        it "relTimes produces elements" testRelTimes
        it "absTimesWith produces elements" testAbsTimesWith
        it "absTimes produces elements" testAbsTimes
