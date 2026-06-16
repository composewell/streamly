module Main (main) where

import Test.Hspec (hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)

import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Test.Hspec as H

import Streamly.Test.Data.Parser.CommonUtilities
import qualified Streamly.Test.Data.Parser.CommonTests as Common
import qualified Streamly.Test.Data.Parser.CommonTypeTests as CommonType

maxTestCount :: Int
maxTestCount = 100

sanityParseBreakChunks :: [Move] -> H.SpecWith ()
sanityParseBreakChunks jumps = it (show jumps) $ do
    (val, rest) <-
        A.parseBreakPos (A.toParserK (jumpParser jumps))
            $ StreamK.fromList $ map A.fromList chunkedTape
    lst <- map A.toList <$> StreamK.toList rest
    (val, concat lst) `shouldBe` (expectedResult jumps tape)

moduleName :: String
moduleName = "Data.ParserK.Chunked"

main :: IO ()
main =
  hspec $
  H.parallel $
  modifyMaxSuccess (const maxTestCount) $
  describe moduleName $ do
    Common.mainCommon Common.TMParserKStreamKChunks
    CommonType.mainCommonType Common.TMParserKStreamKChunks
    parserSanityTests "StreamK.parseBreakChunks" sanityParseBreakChunks
