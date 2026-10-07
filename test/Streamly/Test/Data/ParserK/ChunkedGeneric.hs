module Main (main) where

import Control.Applicative ((<|>), optional)
import Test.Hspec (hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)

import qualified Streamly.Internal.Data.Array.Generic as AG
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as P
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Test.Hspec as H

import Streamly.Test.Data.Parser.CommonTestDriver (TestMode(..))
import Streamly.Test.Data.Parser.CommonUtilities
import qualified Streamly.Test.Data.Parser.CommonTests as Common
import qualified Streamly.Test.Data.Parser.CommonTypeTests as CommonType

maxTestCount :: Int
maxTestCount = 100

sanityParseBreakChunksGeneric :: [Move] -> H.SpecWith ()
sanityParseBreakChunksGeneric jumps = it (show jumps) $ do
    (val, rest) <-
        AG.parseBreakPos (AG.toParserK (jumpParser jumps))
            $ StreamK.fromList $ map AG.fromList chunkedTape
    lst <- map AG.toList <$> StreamK.toList rest
    (val, concat lst) `shouldBe` (expectedResult jumps tape)

-- Parsers composed with the ParserK instances, not converted from a composed
-- Parser.

nativeParserK :: H.SpecWith ()
nativeParserK = do
    let k = AG.toParserK
        run p chunks = do
            (r, rest) <- AG.parseBreak p (StreamK.fromList (map AG.fromList chunks))
            rest1 <- concatMap AG.toList <$> StreamK.toList rest
            return (either (const Nothing) Just r, rest1)
        -- Gives back the 1 at the end of input
        optionalPair =
            optional (P.satisfy (== 1) *> P.satisfy (== (2 :: Int)))
        -- Fails at the end of input after consuming all of it
        failsAtEnd = k (P.takeWhile (< 5) FL.toList) *> k (P.satisfy (== 9) >> pure [])
    it "input given back at the end goes to the next parser" $ do
        r <- run (k optionalPair *> k (P.fromFold FL.toList)) [[1]]
        r `shouldBe` (Just [1], [])
    it "alternative backtracks at the end of input" $ do
        r <- run (failsAtEnd <|> k (P.takeEQ 3 FL.toList)) [[1, 2, 3 :: Int]]
        r `shouldBe` (Just [1, 2, 3], [])
    it "alternative backtracks at the end of input, one element chunks" $ do
        r <-
            run
                (failsAtEnd <|> k (P.takeEQ 3 FL.toList))
                [[1], [2], [3 :: Int]]
        r `shouldBe` (Just [1, 2, 3], [])

moduleName :: String
moduleName = "Data.ParserK.Chunked.Generic"

main :: IO ()
main =
  hspec $
  H.parallel $
  modifyMaxSuccess (const maxTestCount) $
  describe moduleName $ do
    CommonType.mainCommonType TMParserKStreamKChunksGeneric
    Common.mainCommon TMParserKStreamKChunksGeneric
    parserSanityTests "StreamK.parseBreakChunksGeneric" sanityParseBreakChunksGeneric
    describe "ParserK instances" nativeParserK
