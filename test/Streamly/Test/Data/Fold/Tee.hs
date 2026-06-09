{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- |
-- Module      : Streamly.Test.Data.Fold.Tee
-- Copyright   : (c) 2025 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Fold.Tee (main) where

import Streamly.Internal.Data.Fold (Tee(..), unTee, toFold)
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Hspec

unTeeTest :: Expectation
unTeeTest = do
    let avg = (/) <$> Tee Fold.sum <*> Tee (fmap fromIntegral Fold.length)
    result <- Stream.fold (unTee avg) (Stream.fromList [1.0..5.0 :: Double])
    result `shouldBe` 3.0

teeApplicative :: Expectation
teeApplicative = do
    let t = (,) <$> Tee Fold.sum <*> Tee Fold.length
    result <- Stream.fold (unTee t) (Stream.fromList [1,2,3 :: Int])
    result `shouldBe` (6, 3)

teeSemigroup :: Expectation
teeSemigroup = do
    let t = Tee Fold.toList <> Tee (Fold.lmap (* 2) Fold.toList)
    result <- Stream.fold (unTee t) (Stream.fromList [1,2,3 :: Int])
    result `shouldBe` [1,2,3,2,4,6]

teeNum :: Expectation
teeNum = do
    let t = Tee Fold.sum + Tee Fold.length
    result <- Stream.fold (unTee t) (Stream.fromList [1,2,3 :: Int])
    result `shouldBe` (9 :: Int)

toFoldTest :: Expectation
toFoldTest = do
    let t = Tee Fold.sum
    result <- Stream.fold (toFold t) (Stream.fromList [1,2,3 :: Int])
    result `shouldBe` (6 :: Int)

moduleName :: String
moduleName = "Data.Fold.Tee"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        it "unTee" unTeeTest
        it "Tee Applicative" teeApplicative
        it "Tee Semigroup" teeSemigroup
        it "Tee Num" teeNum
        it "toFold" toFoldTest
