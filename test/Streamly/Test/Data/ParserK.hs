-- XXX We are using head/tail at one place
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Main (main) where

import Data.Either (fromRight)
import Test.Hspec (Spec, hspec, describe, it, expectationFailure, shouldBe)
import Test.Hspec.QuickCheck

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.ParserK as ParserK
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Test.Hspec as H

import Streamly.Test.Data.Parser.CommonUtilities
import qualified Streamly.Test.Data.Parser.CommonTests as Common

import Prelude hiding (sequence)

maxTestCount :: Int
maxTestCount = 100

toParser :: Spec
toParser = do
    let p = ParserK.toParser (ParserK.toParserK Parser.one)
        runP xs = Stream.parsePos p (Stream.fromList xs)
    describe "toParser . toParserK" $ do
        it "empty stream" $ do
            r1 <- runP ([] :: [Int])
            case r1 of
                Left e -> print e
                Right x ->
                    expectationFailure $ "Expecting failure, got: " ++ show x
        it "exact stream" $ do
            r2 <- runP [0::Int]
            fromRight undefined r2 `shouldBe` 0
        it "longer stream" $ do
            r3 <- runP [0,1::Int]
            fromRight undefined r3 `shouldBe` 0

    let p1 = ParserK.toParserK $ ParserK.toParser (ParserK.toParserK Parser.one)
        runP1 xs = StreamK.parsePos p1 (StreamK.fromStream $ Stream.fromList xs)
    describe "toParserK . toParser . toParserK" $ do
        it "empty stream" $ do
            r1 <- runP1 ([] :: [Int])
            case r1 of
                Left e -> print e
                Right x ->
                    expectationFailure $ "Expecting failure, got: " ++ show x
        it "exact stream" $ do
            r2 <- runP1 [0::Int]
            fromRight undefined r2 `shouldBe` 0
        it "longer stream" $ do
            r3 <- runP1 [0,1::Int]
            fromRight undefined r3 `shouldBe` 0

    -- NOTE: Without fusionBreaker this test would pass even if toParser has
    -- incorrect implementation because of fusion rules.
    let p2 = Parser.takeWhile (<= 3) FL.toList
        runP2 xs = Stream.parseBreakPos p2 (Stream.fromList xs)

        p3 = ParserK.toParserK (Parser.takeWhile (<= 3) FL.toList)
        runP3 xs = StreamK.parseBreakPos p3 (StreamK.fromList xs)

        p4 =
            ParserK.toParser
                $ fusionBreaker
                $ ParserK.toParserK (Parser.takeWhile (<= 3) FL.toList)
        runP4 xs = Stream.parseBreakPos p4 (Stream.fromList xs)
    describe "toParser . toParserK" $ do
        it "(<= 3) for [1, 2, 3, 4, 5]" $ do
            (a, b) <- runP2 ([1, 2, 3, 4, 5] :: [Int])
            fromRight undefined a `shouldBe` [1, 2, 3]
            rest <- Stream.toList b
            rest `shouldBe` [4, 5]
        it "(<= 3) for [1, 2, 3, 4, 5]" $ do
            (a, b) <- runP3 ([1, 2, 3, 4, 5] :: [Int])
            fromRight undefined a `shouldBe` [1, 2, 3]
            rest <- StreamK.toList b
            rest `shouldBe` [4, 5]
        it "(<= 3) for [1, 2, 3, 4, 5]" $ do
            (a, b) <- runP4 ([1, 2, 3, 4, 5] :: [Int])
            fromRight undefined a `shouldBe` [1,2,3]
            rest <- Stream.toList b
            rest `shouldBe` [4, 5]
        it "(<= 3) for [1, 2, 3]" $ do
            (a, b) <- runP4 ([1, 2, 3] :: [Int])
            fromRight undefined a `shouldBe` [1, 2, 3]
            rest <- Stream.toList b
            rest `shouldBe` []

{-# NOINLINE fusionBreaker #-}
fusionBreaker :: a -> a
fusionBreaker = id

-------------------------------------------------------------------------------
-- Parser driver sanity tests
-------------------------------------------------------------------------------

sanityParseBreak :: [Move] -> H.SpecWith ()
sanityParseBreak jumps = it (show jumps) $ do
    (val, rest) <-
        StreamK.parseBreakPos (ParserK.toParserK (jumpParser jumps))
            $ StreamK.fromList tape
    lst <- StreamK.toList rest
    (val, lst) `shouldBe` (expectedResult jumps tape)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.ParserK"

main :: IO ()
main =
  hspec $
  H.parallel $
  modifyMaxSuccess (const maxTestCount) $ do
  describe moduleName $ do
    Common.mainCommon Common.TMParserKStreamK
    toParser
    parserSanityTests "StreamK.parseBreak" sanityParseBreak
