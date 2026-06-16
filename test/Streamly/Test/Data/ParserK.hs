-- XXX We are using head/tail at one place
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Main (main) where

import Test.Hspec (hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck

import qualified Streamly.Internal.Data.ParserK as ParserK
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Test.Hspec as H

import Streamly.Test.Data.Parser.CommonUtilities
import qualified Streamly.Test.Data.Parser.CommonTests as Common
import qualified Streamly.Test.Data.ParserK.Type as Type

import Prelude hiding (sequence)

maxTestCount :: Int
maxTestCount = 100

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
    parserSanityTests "StreamK.parseBreak" sanityParseBreak
    Type.spec
