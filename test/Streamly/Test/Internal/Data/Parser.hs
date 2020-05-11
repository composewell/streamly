module Main (main) where

import Streamly.Internal.Data.Parser as P hiding(die, dieM)
import Streamly.Internal.Data.Parser.ParserD(die, dieM)
import qualified Streamly.Internal.Prelude as S

import Test.Hspec(hspec)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, chooseInt, Property, property, listOf)

testAny :: Property
testAny =
    forAll (listOf $ chooseInt (0, 10000)) $ \ls ->
        case S.parse (P.any (> 5000)) (S.fromList ls) of
            Right r -> r == (Prelude.any (> 5000) ls)
            Left _ -> False

testAll :: Property
testAll =
    forAll (listOf $ chooseInt (0, 10000)) $ \ls ->
        case S.parse (P.all (> 5000)) (S.fromList ls) of
            Right r -> r == (Prelude.all (> 5000) ls)
            Left _ -> False

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

testDie :: Property
testDie =
    property $
    case S.parseD (die "die test") (S.fromList [0 :: Int]) of
        Right _ -> False
        Left _ -> True

testDieM :: Property
testDieM =
    property $
    case S.parseD (dieM (Right "die test")) (S.fromList [0 :: Int]) of
        Right _ -> False
        Left _ -> True

main :: IO ()
main = hspec $ do
    prop "test any function" testAny
    prop "test all function" testAll
    prop "test yield function" testYield
    prop "test yieldM function" testYieldM
    prop "test die function" testDie
    prop "test dieM function" testDieM