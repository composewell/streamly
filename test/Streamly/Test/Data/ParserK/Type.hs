module Streamly.Test.Data.ParserK.Type (spec) where

import Data.Either (fromRight)
import Test.Hspec
       (Spec, describe, it, expectationFailure, shouldBe)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.ParserK as ParserK
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.StreamK as StreamK

import Streamly.Test.Data.Parser.CommonTestDriver (TestMode(..))
import qualified Streamly.Test.Data.Parser.CommonTypeTests as CommonType

-------------------------------------------------------------------------------
-- Adapting from/to Parser
-------------------------------------------------------------------------------

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
-- Spec
-------------------------------------------------------------------------------

spec :: Spec
spec = do
    CommonType.mainCommonType TMParserKStreamK
    toParser
