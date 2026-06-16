module Streamly.Test.Data.Parser.Type (spec) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO(..))

import qualified Streamly.Data.Stream as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as P

import Test.Hspec

import qualified Streamly.Test.Data.Parser.CommonTests as Common

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

{-# INLINE alt #-}
alt :: MonadIO m => S.Stream m Int -> m (Either P.ParseError [Int])
alt =
    S.parse
        (   Common.takeWhileFailD (<= 5) FL.toList
        <|> P.takeWhile (<= 7) FL.toList
        )

-------------------------------------------------------------------------------
-- Spec
-------------------------------------------------------------------------------

spec :: Spec
spec =
    it "alt [1..20]" $ alt (S.fromList [1..20]) `shouldReturn` Right [1..7]
