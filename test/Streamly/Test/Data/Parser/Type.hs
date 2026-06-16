module Streamly.Test.Data.Parser.Type (spec) where

import Test.Hspec

import Streamly.Test.Data.Parser.CommonTestDriver (TestMode(..))
import qualified Streamly.Test.Data.Parser.CommonTypeTests as CommonType

-------------------------------------------------------------------------------
-- Spec
-------------------------------------------------------------------------------

spec :: Spec
spec = CommonType.mainCommonType TMParserStream
