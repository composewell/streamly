-- |
-- Module      : Streamly.Test.Data.Stream.Eliminate
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Eliminate (main) where

import Data.Char (toLower)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Hspec

toList :: Monad m => Stream.Stream m a -> m [a]
toList = Stream.toList

-------------------------------------------------------------------------------
-- Right Folds
-------------------------------------------------------------------------------

testFoldr1 :: Expectation
testFoldr1 =
    Stream.foldr1 (+) (Stream.fromList [1, 2, 3 :: Int])
        `shouldReturn` Just 6

testFoldr1Single :: Expectation
testFoldr1Single =
    Stream.foldr1 (+) (Stream.fromList [42 :: Int])
        `shouldReturn` Just 42

testFoldr1Empty :: Expectation
testFoldr1Empty =
    Stream.foldr1 (+) (Stream.fromList ([] :: [Int]))
        `shouldReturn` Nothing

-------------------------------------------------------------------------------
-- Fold Maybes / Eithers
-------------------------------------------------------------------------------

testFoldMaybesAllJust :: Expectation
testFoldMaybesAllJust =
    Stream.foldMaybes Fold.sum
        (Stream.fromList [Just 1, Just 2, Just 3])
        `shouldReturn` Just (6 :: Int)

testFoldMaybesWithNothing :: Expectation
testFoldMaybesWithNothing =
    Stream.foldMaybes Fold.sum
        (Stream.fromList [Just 1, Nothing, Just 3 :: Maybe Int])
        `shouldReturn` Nothing

testFoldMaybesEmpty :: Expectation
testFoldMaybesEmpty =
    Stream.foldMaybes Fold.sum (Stream.fromList ([] :: [Maybe Int]))
        `shouldReturn` Just 0

testFoldEithersAllRight :: Expectation
testFoldEithersAllRight =
    Stream.foldEithers Fold.sum
        (Stream.fromList [Right 1, Right 2, Right 3 :: Either String Int])
        `shouldReturn` Right 6

testFoldEithersWithLeft :: Expectation
testFoldEithersWithLeft =
    Stream.foldEithers Fold.sum
        (Stream.fromList [Right 1, Left "oops", Right 3 :: Either String Int])
        `shouldReturn` Left "oops"

testFoldEithersEmpty :: Expectation
testFoldEithersEmpty =
    Stream.foldEithers Fold.sum
        (Stream.fromList ([] :: [Either String Int]))
        `shouldReturn` Right 0

-------------------------------------------------------------------------------
-- To Containers
-------------------------------------------------------------------------------

testToListRev :: Expectation
testToListRev =
    Stream.toListRev (Stream.fromList [1, 2, 3 :: Int])
        `shouldReturn` [3, 2, 1]

testToListRevEmpty :: Expectation
testToListRevEmpty =
    Stream.toListRev (Stream.fromList ([] :: [Int]))
        `shouldReturn` []

testToListRevSingle :: Expectation
testToListRevSingle =
    Stream.toListRev (Stream.fromList [42 :: Int])
        `shouldReturn` [42]

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

testParse :: Expectation
testParse = do
    r <- Stream.parse (Parser.takeEQ 3 Fold.toList)
             (Stream.fromList [1, 2, 3 :: Int])
    r `shouldBe` Right [1, 2, 3]

testParseError :: Expectation
testParseError = do
    r <- Stream.parse (Parser.takeEQ 5 Fold.drain)
             (Stream.fromList [1, 2 :: Int])
    case r of
        Left _  -> return ()
        Right _ -> expectationFailure "expected parse error"

testParsePos :: Expectation
testParsePos = do
    r <- Stream.parsePos (Parser.takeEQ 3 Fold.toList)
             (Stream.fromList [1, 2, 3 :: Int])
    r `shouldBe` Right [1, 2, 3]

testParsePosError :: Expectation
testParsePosError = do
    r <- Stream.parsePos (Parser.takeEQ 5 Fold.drain)
             (Stream.fromList [1, 2 :: Int])
    case r of
        Left _  -> return ()
        Right _ -> expectationFailure "expected parse error"

testParseBreak :: Expectation
testParseBreak = do
    (r, rest) <- Stream.parseBreak (Parser.takeEQ 2 Fold.toList)
                     (Stream.fromList [1, 2, 3, 4 :: Int])
    r `shouldBe` Right [1, 2]
    toList rest `shouldReturn` [3, 4]

testParseBreakPos :: Expectation
testParseBreakPos = do
    (r, rest) <- Stream.parseBreakPos (Parser.takeEQ 2 Fold.toList)
                     (Stream.fromList [1, 2, 3, 4 :: Int])
    r `shouldBe` Right [1, 2]
    toList rest `shouldReturn` [3, 4]

-------------------------------------------------------------------------------
-- Multi-Stream Folds
-------------------------------------------------------------------------------

testIsInfixOf :: Expectation
testIsInfixOf = do
    Stream.isInfixOf
        (Stream.fromList [2, 3 :: Int])
        (Stream.fromList [1, 2, 3, 4]) `shouldReturn` True
    Stream.isInfixOf
        (Stream.fromList [1, 2, 3 :: Int])
        (Stream.fromList [1, 2, 3]) `shouldReturn` True
    Stream.isInfixOf
        (Stream.fromList [5 :: Int])
        (Stream.fromList [1, 2, 3]) `shouldReturn` False
    Stream.isInfixOf
        (Stream.fromList ([] :: [Int]))
        (Stream.fromList [1, 2, 3]) `shouldReturn` True

testIsSuffixOf :: Expectation
testIsSuffixOf = do
    Stream.isSuffixOf
        (Stream.fromList [3, 4 :: Int])
        (Stream.fromList [1, 2, 3, 4]) `shouldReturn` True
    Stream.isSuffixOf
        (Stream.fromList [1, 2, 3 :: Int])
        (Stream.fromList [1, 2, 3]) `shouldReturn` True
    Stream.isSuffixOf
        (Stream.fromList [1 :: Int])
        (Stream.fromList [1, 2, 3]) `shouldReturn` False
    Stream.isSuffixOf
        (Stream.fromList ([] :: [Int]))
        (Stream.fromList [1, 2, 3]) `shouldReturn` True

testIsSuffixOfUnbox :: Expectation
testIsSuffixOfUnbox = do
    Stream.isSuffixOfUnbox
        (Stream.fromList [3, 4 :: Int])
        (Stream.fromList [1, 2, 3, 4]) `shouldReturn` True
    Stream.isSuffixOfUnbox
        (Stream.fromList [1 :: Int])
        (Stream.fromList [1, 2, 3]) `shouldReturn` False

testStripPrefixBy :: Expectation
testStripPrefixBy = do
    r1 <- Stream.stripPrefixBy (==)
              (Stream.fromList [1, 2 :: Int])
              (Stream.fromList [1, 2, 3, 4])
    case r1 of
        Nothing -> expectationFailure "expected Just"
        Just s  -> toList s `shouldReturn` [3, 4]
    r2 <- Stream.stripPrefixBy (==)
              (Stream.fromList [5 :: Int])
              (Stream.fromList [1, 2, 3])
    case r2 of
        Nothing -> return ()
        Just _  -> expectationFailure "expected Nothing"

testStripPrefixByCaseInsensitive :: Expectation
testStripPrefixByCaseInsensitive = do
    let eqCI a b = toLower a == toLower b
    r <- Stream.stripPrefixBy eqCI
             (Stream.fromList "Hello")
             (Stream.fromList "hello world")
    case r of
        Nothing -> expectationFailure "expected Just"
        Just s  -> toList s `shouldReturn` " world"

testStripSuffix :: Expectation
testStripSuffix = do
    r1 <- Stream.stripSuffix
              (Stream.fromList [3, 4 :: Int])
              (Stream.fromList [1, 2, 3, 4])
    case r1 of
        Nothing -> expectationFailure "expected Just"
        Just s  -> toList s `shouldReturn` [1, 2]
    r2 <- Stream.stripSuffix
              (Stream.fromList [5 :: Int])
              (Stream.fromList [1, 2, 3])
    case r2 of
        Nothing -> return ()
        Just _  -> expectationFailure "expected Nothing"

testStripSuffixSelf :: Expectation
testStripSuffixSelf = do
    r <- Stream.stripSuffix
             (Stream.fromList [1, 2, 3 :: Int])
             (Stream.fromList [1, 2, 3])
    case r of
        Nothing -> expectationFailure "expected Just"
        Just s  -> toList s `shouldReturn` []

testStripSuffixUnbox :: Expectation
testStripSuffixUnbox = do
    r1 <- Stream.stripSuffixUnbox
              (Stream.fromList [3, 4 :: Int])
              (Stream.fromList [1, 2, 3, 4])
    case r1 of
        Nothing -> expectationFailure "expected Just"
        Just s  -> toList s `shouldReturn` [1, 2]
    r2 <- Stream.stripSuffixUnbox
              (Stream.fromList [5 :: Int])
              (Stream.fromList [1, 2, 3])
    case r2 of
        Nothing -> return ()
        Just _  -> expectationFailure "expected Nothing"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Stream.Eliminate"

main :: IO ()
main = hspec $ describe moduleName $ do
    describe "Right Folds" $ do
        it "foldr1" testFoldr1
        it "foldr1 single element" testFoldr1Single
        it "foldr1 empty" testFoldr1Empty

    describe "Fold Maybes" $ do
        it "foldMaybes all Just" testFoldMaybesAllJust
        it "foldMaybes with Nothing" testFoldMaybesWithNothing
        it "foldMaybes empty" testFoldMaybesEmpty

    describe "Fold Eithers" $ do
        it "foldEithers all Right" testFoldEithersAllRight
        it "foldEithers with Left" testFoldEithersWithLeft
        it "foldEithers empty" testFoldEithersEmpty

    describe "To Containers" $ do
        it "toListRev" testToListRev
        it "toListRev empty" testToListRevEmpty
        it "toListRev single" testToListRevSingle

    describe "Parsing" $ do
        it "parse success" testParse
        it "parse error" testParseError
        it "parsePos success" testParsePos
        it "parsePos error" testParsePosError
        it "parseBreak" testParseBreak
        it "parseBreakPos" testParseBreakPos

    describe "Multi-Stream Folds" $ do
        it "isInfixOf" testIsInfixOf
        it "isSuffixOf" testIsSuffixOf
        it "isSuffixOfUnbox" testIsSuffixOfUnbox
        it "stripPrefixBy" testStripPrefixBy
        it "stripPrefixBy case-insensitive" testStripPrefixByCaseInsensitive
        it "stripSuffix" testStripSuffix
        it "stripSuffix self" testStripSuffixSelf
        it "stripSuffixUnbox" testStripSuffixUnbox
