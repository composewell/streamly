{-# OPTIONS_GHC -Wno-deprecations #-}
-- |
-- Module      : Streamly.Test.Data.Stream.Generate
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Generate (main) where

import Data.Functor.Identity (Identity(..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int8)
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
    -- Regression test, matches [from, then .. to] from the Prelude.
    -- Large stride with "then" included.
    toList
        (Stream.enumerateFromThenToIntegral
            (-7537527385297985025)
            5092559113693760989
            (6977257977275108264 :: Int))
        `shouldReturn` [-7537527385297985025, 5092559113693760989]

testEnumerateFromThenToFractional :: Expectation
testEnumerateFromThenToFractional =
    toList (Stream.enumerateFromThenToFractional (0.1 :: Double) 2.0 6.0)
        `shouldReturn` [0.1, 2.0, 3.9, 5.799999999999999]

testEnumerateFromThenDownToNum :: Expectation
testEnumerateFromThenDownToNum = do
    toList (Stream.enumerateDownFromThenToNum (6 :: Int) 4 0)
        `shouldReturn` [6, 4, 2, 0]
    -- stride does not evenly divide (from - to)
    toList (Stream.enumerateDownFromThenToNum (7 :: Int) 5 0)
        `shouldReturn` [7, 5, 3, 1]
    -- then > from returns an empty stream
    toList (Stream.enumerateDownFromThenToNum (0 :: Int) 4 (-6))
        `shouldReturn` []
    -- to is above then (but at or below from) returns just the single
    -- "from" element
    toList (Stream.enumerateDownFromThenToNum (6 :: Int) 0 3)
        `shouldReturn` [6]
    -- matches [from, then .. to] from the Prelude
    toList (Stream.enumerateDownFromThenToNum (6 :: Int) 4 (-1))
        `shouldReturn` Prelude.takeWhile (>= (-1)) [6, 4 ..]

testEnumerateFromThenSmall :: Expectation
testEnumerateFromThenSmall =
    toList (Stream.take 4 (Stream.enumerateFromThenSmall 'a' 'c'))
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
-- Enumerable type class dispatch
--
-- The tests above exercise the concrete per-type functions (e.g.
-- Stream.enumerateFromToIntegral) directly. The tests below instead go
-- through the polymorphic 'Enumerable' class methods (Stream.enumerateFrom,
-- Stream.enumerateFromTo, Stream.enumerateFromThen,
-- Stream.enumerateFromThenTo) so that a bug in how a particular instance
-- wires the class methods to the underlying functions (e.g. enumerateFromTo
-- accidentally calling enumerateFromThen) is actually caught. The
-- 'Identity' instance is hand-written rather than macro generated and is
-- therefore particularly prone to such copy-paste mistakes.
-------------------------------------------------------------------------------

testEnumerableFromInt :: Expectation
testEnumerableFromInt =
    toList (Stream.take 5 (Stream.enumerateFrom (0 :: Int)))
        `shouldReturn` [0, 1, 2, 3, 4]

testEnumerableFromToInt :: Expectation
testEnumerableFromToInt =
    toList (Stream.enumerateFromTo (0 :: Int) 4)
        `shouldReturn` [0, 1, 2, 3, 4]

testEnumerableFromThenInt :: Expectation
testEnumerableFromThenInt =
    toList (Stream.take 4 (Stream.enumerateFromThen (0 :: Int) 2))
        `shouldReturn` [0, 2, 4, 6]

testEnumerableFromThenToInt :: Expectation
testEnumerableFromThenToInt =
    toList (Stream.enumerateFromThenTo (0 :: Int) 2 6)
        `shouldReturn` [0, 2, 4, 6]

testEnumerableFromToChar :: Expectation
testEnumerableFromToChar =
    toList (Stream.enumerateFromTo 'a' 'e')
        `shouldReturn` "abcde"

testEnumerableFromToDouble :: Expectation
testEnumerableFromToDouble =
    toList (Stream.enumerateFromTo (1.1 :: Double) 4.0)
        `shouldReturn` [1.1, 2.1, 3.1, 4.1]

-- Regression test: the Identity Enumerable instance's enumerateFromTo in
-- the Unfold.Enumeration module was once wired to enumerateFromThen instead
-- of enumerateFromTo. Stream.Enumeration's instance was not affected, but
-- we test it here too so both modules stay covered symmetrically.
testEnumerableFromIdentity :: Expectation
testEnumerableFromIdentity =
    toList (Stream.take 4 (Stream.enumerateFrom (Identity (0 :: Int))))
        `shouldReturn` fmap Identity [0, 1, 2, 3]

testEnumerableFromToIdentity :: Expectation
testEnumerableFromToIdentity =
    toList (Stream.enumerateFromTo (Identity (0 :: Int)) (Identity 4))
        `shouldReturn` fmap Identity [0, 1, 2, 3, 4]

testEnumerableFromThenIdentity :: Expectation
testEnumerableFromThenIdentity =
    toList
        (Stream.take 4
            (Stream.enumerateFromThen (Identity (0 :: Int)) (Identity 2)))
        `shouldReturn` fmap Identity [0, 2, 4, 6]

testEnumerableFromThenToIdentity :: Expectation
testEnumerableFromThenToIdentity =
    toList
        (Stream.enumerateFromThenTo
            (Identity (0 :: Int)) (Identity 2) (Identity 6))
        `shouldReturn` fmap Identity [0, 2, 4, 6]

-------------------------------------------------------------------------------
-- Overflow at the bound of a fixed-size Integral type
--
-- All of these are guarded with 'Stream.take' so a regression
-- that reintroduces unbounded wraparound fails instead of hanging.
-------------------------------------------------------------------------------

testEnumerateFromToIntegralOverflow :: Expectation
testEnumerateFromToIntegralOverflow =
    toList (Stream.take 10 (Stream.enumerateFromToIntegral (253 :: Word8) 255))
        `shouldReturn` [253, 254, 255]

testEnumerateFromIntegralOverflow :: Expectation
testEnumerateFromIntegralOverflow =
    toList (Stream.take 10 (Stream.enumerateFromIntegral (253 :: Word8)))
        `shouldReturn` [253, 254, 255]

testEnumerateFromThenIntegralOverflowUp :: Expectation
testEnumerateFromThenIntegralOverflowUp =
    toList (Stream.take 10 (Stream.enumerateFromThenIntegral (250 :: Word8) 252))
        `shouldReturn` [250, 252, 254]

testEnumerateFromThenIntegralOverflowDn :: Expectation
testEnumerateFromThenIntegralOverflowDn =
    toList (Stream.take 10 (Stream.enumerateFromThenIntegral (-124 :: Int8) (-126)))
        `shouldReturn` [-124, -126, -128]

testEnumerateFromThenToIntegralOverflowUp :: Expectation
testEnumerateFromThenToIntegralOverflowUp =
    toList (Stream.take 10 (Stream.enumerateFromThenToIntegral (250 :: Word8) 252 255))
        `shouldReturn` [250, 252, 254]

testEnumerateFromThenToIntegralOverflowDn :: Expectation
testEnumerateFromThenToIntegralOverflowDn =
    toList
        (Stream.take 10
            (Stream.enumerateFromThenToIntegral (-124 :: Int8) (-126) (-128)))
        `shouldReturn` [-124, -126, -128]

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

    -- TODO: Use the Down functor in the up direction versions and check if
    -- that gives correct results in the down direction. If so update the
    -- general documentation in the Stream/Unfold modules.
    describe "Enumeration Primitives" $ do
        it "enumerateFromIntegral" testEnumerateFromIntegral
        it "enumerateFromNum" testEnumerateFromNum
        it "enumerateFromStepNum" testEnumerateFromStepNum
        it "enumerateFromThenNum" testEnumerateFromThenNum
        it "enumerateFromThenIntegral" testEnumerateFromThenIntegral
        it "enumerateFromThenFractional" testEnumerateFromThenFractional
        it "enumerateFromThenToIntegral" testEnumerateFromThenToIntegral
        it "enumerateFromThenToFractional" testEnumerateFromThenToFractional
        it "enumerateDownFromThenToNum" testEnumerateFromThenDownToNum
        it "enumerateFromThenSmall" testEnumerateFromThenSmall
        it "enumerateFromToSmall" testEnumerateFromToSmall
        it "enumerateFromThenToSmall" testEnumerateFromThenToSmall
        it "enumerateFromFractional" testEnumerateFromFractional
        it "enumerateFromToFractional" testEnumerateFromToFractional

    describe "Enumerable type class dispatch" $ do
        it "enumerateFrom Int" testEnumerableFromInt
        it "enumerateFromTo Int" testEnumerableFromToInt
        it "enumerateFromThen Int" testEnumerableFromThenInt
        it "enumerateFromThenTo Int" testEnumerableFromThenToInt
        it "enumerateFromTo Char" testEnumerableFromToChar
        it "enumerateFromTo Double" testEnumerableFromToDouble
        it "enumerateFrom Identity" testEnumerableFromIdentity
        it "enumerateFromTo Identity" testEnumerableFromToIdentity
        it "enumerateFromThen Identity" testEnumerableFromThenIdentity
        it "enumerateFromThenTo Identity" testEnumerableFromThenToIdentity

    describe "Enumeration overflow at type bound" $ do
        it "enumerateFromToIntegral overflow" testEnumerateFromToIntegralOverflow
        it "enumerateFromIntegral overflow" testEnumerateFromIntegralOverflow
        it "enumerateFromThenIntegral overflow up"
            testEnumerateFromThenIntegralOverflowUp
        it "enumerateFromThenIntegral overflow dn"
            testEnumerateFromThenIntegralOverflowDn
        it "enumerateFromThenToIntegral overflow up"
            testEnumerateFromThenToIntegralOverflowUp
        it "enumerateFromThenToIntegral overflow dn"
            testEnumerateFromThenToIntegralOverflowDn

    describe "Time Enumeration" $ do
        it "timesWith produces elements" testTimesWith
        it "times produces elements" testTimes
        it "relTimesWith produces elements" testRelTimesWith
        it "relTimes produces elements" testRelTimes
        it "absTimesWith produces elements" testAbsTimesWith
        it "absTimes produces elements" testAbsTimes
