module Streamly.Test.Data.Parser.Type (spec) where

import Test.Hspec

import qualified Streamly.Test.Data.Parser.CommonTests as Common
import qualified Streamly.Test.Data.Parser.CommonTypeTests as CommonType

-------------------------------------------------------------------------------
-- Spec
-------------------------------------------------------------------------------

spec :: Spec
spec = CommonType.mainCommonType Common.TMParserStream
