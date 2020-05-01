module Main (main) where

import Streamly.Internal.Data.Parser
import qualified Streamly.Internal.Prelude as S

import Test.Hspec(hspec)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, chooseInt, Property)

testYield :: Property
testYield = 
    forAll (chooseInt (0, 10000)) $ \x ->
        case S.parse (yield x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

testYieldM :: Property
testYieldM =
    forAll (chooseInt (0, 10000)) $ \x ->
        case S.parse (yieldM $ return x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

main :: IO ()
main = hspec $ do
    prop "test yield function" testYield
    prop "test yieldM function" testYieldM