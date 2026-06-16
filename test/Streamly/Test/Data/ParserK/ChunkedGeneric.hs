module Main (main) where

import Test.Hspec (hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)

import qualified Streamly.Internal.Data.Array.Generic as AG
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Test.Hspec as H

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

moduleName :: String
moduleName = "Data.ParserK.Chunked.Generic"

main :: IO ()
main =
  hspec $
  H.parallel $
  modifyMaxSuccess (const maxTestCount) $
  describe moduleName $ do
    Common.mainCommon Common.TMParserKStreamKChunksGeneric
    CommonType.mainCommonType Common.TMParserKStreamKChunksGeneric
    parserSanityTests "StreamK.parseBreakChunksGeneric" sanityParseBreakChunksGeneric
