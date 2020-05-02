module Main (main) where

import Streamly.Internal.Data.Fold
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL

import Test.Hspec as H
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary, choose)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

maxStreamLen :: Int
maxStreamLen = 1000

intMin :: Int
intMin = minBound

intMax :: Int
intMax = maxBound

{-# INLINE maxStreamLen #-}
{-# INLINE intMin #-}
{-# INLINE intMax #-}

testRollingHashFirstN :: Property
testRollingHashFirstN =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (choose (0, len)) $ \n ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
                a <- run $ S.fold rollingHash $ S.take n $ S.fromList vec
                b <- run $ S.fold (rollingHashFirstN n) $ S.fromList vec
                assert $ a == b


testHead :: [Int] -> Expectation
testHead ls = S.fold FL.head (S.fromList ls) `shouldReturn` headl ls
            where
            headl [] = Nothing
            headl (x:_) = Just x


testLength :: [Int] -> Expectation
testLength ls = S.fold FL.length (S.fromList ls) `shouldReturn` Prelude.length ls


testSum :: [Int] -> Expectation
testSum ls = S.fold FL.sum (S.fromList ls) `shouldReturn` foldl (+) 0 ls


testProduct :: [Int] -> Expectation
testProduct ls = S.fold FL.product (S.fromList ls) `shouldReturn` foldl (*) 1 ls


lesser :: (a -> a -> Ordering) -> a -> a -> a
lesser f x y = if f x y == LT then x else y


greater :: (a -> a -> Ordering) -> a -> a -> a
greater f x y = if f x y == GT then x else y


foldMaybe :: (b -> a -> b) -> b -> [a] -> Maybe b
foldMaybe f acc ls = case ls of
                        [] -> Nothing
                        _ -> Just (foldl f acc ls)


testMaximumBy :: (Ord a, Show a) => a -> (a -> a -> Ordering) -> [a] -> Expectation
testMaximumBy genmin f ls = S.fold (FL.maximumBy f) (S.fromList ls) `shouldReturn` foldMaybe (greater f) genmin ls


testMaximum :: (Show a, Ord a) => a -> [a] -> Expectation
testMaximum genmin ls = S.fold FL.maximum (S.fromList ls) `shouldReturn` foldMaybe (greater compare) genmin ls


testMinimumBy :: (Ord a, Show a) => a -> (a -> a -> Ordering) -> [a] -> Expectation
testMinimumBy genmax f ls = S.fold (FL.minimumBy f) (S.fromList ls) `shouldReturn` foldMaybe (lesser f) genmax ls


testMinimum :: (Show a, Ord a) => a -> [a] -> Expectation
testMinimum genmax ls = S.fold FL.minimum (S.fromList ls) `shouldReturn` foldMaybe (lesser compare) genmax ls


testToList :: [Int] -> Expectation
testToList ls = S.fold FL.toList (S.fromList ls) `shouldReturn` ls


safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs


testLast :: [String] -> Expectation
testLast ls = S.fold FL.last (S.fromList ls) `shouldReturn` safeLast ls


nth :: Int -> [a] -> Maybe a
nth idx (x:xs) = if idx == 0
                 then Just x
                 else if idx < 0
                 then Nothing
                 else nth (idx - 1) xs
nth _ [] = Nothing


testIndex :: Int -> [String] -> Expectation
testIndex idx ls = let
                     x = S.fold (FL.index idx) (S.fromList ls)
                   in
                     x `shouldReturn` (nth idx ls)


testFind :: (Show a, Eq a) => (a -> Bool) -> [a] -> Expectation
testFind f ls = do
                   let x = S.fold (FL.findIndex f) (S.fromList ls)
                   y <- x
                   case y of
                        Nothing  -> S.fold (FL.find f) (S.fromList ls) `shouldReturn` Nothing
                        Just idx -> S.fold (FL.any f) (S.fromList $ take idx ls) `shouldReturn` False


neg :: (a -> Bool) -> a -> Bool
neg f x = if f x == True then False else True

testFindIndex :: (a -> Bool) -> [a] -> Expectation
testFindIndex f ls = do
                        let x = S.fold (FL.findIndex f) (S.fromList ls)
                        y <- x
                        case y of
                              Nothing  -> S.fold (FL.all $ neg f) (S.fromList ls) `shouldReturn` True
                              Just idx -> if idx == 0
                                          then S.fold (FL.all f) (S.fromList []) `shouldReturn` True
                                          else S.fold (FL.all f) (S.fromList $ (take idx ls)) `shouldReturn` False

predicate :: Int -> Bool
predicate x = if x * x < 100 then True else False

testElemIndex :: Int -> [Int] -> Expectation
testElemIndex elm ls = do
                           let x = S.fold (FL.elemIndex elm) (S.fromList ls)
                           y <- x
                           case y of
                                 Nothing  -> S.fold (FL.any (\z -> if z == elm then True else False)) (S.fromList ls) `shouldReturn` False
                                 Just idx -> S.fold (FL.any (\z -> if z == elm then True else False)) (S.fromList $ (take idx ls)) `shouldReturn` False

testNull :: [Int] -> Expectation
testNull ls = S.fold FL.null (S.fromList ls) `shouldReturn` case ls of
                                                                  [] -> True
                                                                  _ -> False
testElem :: Int -> [Int] -> Expectation
testElem elm ls = do
                     let x = S.fold (FL.elem elm) (S.fromList ls)
                     y <- x
                     S.fold (FL.any (\z -> if z == elm then True else False)) (S.fromList ls) `shouldReturn` y


testNotElem :: Int -> [Int] -> Expectation
testNotElem elm ls = do
                        let x = S.fold (FL.notElem elm) (S.fromList ls)
                        y <- x
                        S.fold (FL.any (\z -> if z == elm then True else False)) (S.fromList ls) `shouldReturn` (if y == True then False else True)


testAll :: (a -> Bool) -> [a] -> Expectation
testAll f ls = S.fold (FL.all f) (S.fromList ls) `shouldReturn` Prelude.and (map f ls)


testAny :: (a -> Bool) -> [a] -> Expectation
testAny f ls = S.fold (FL.any f) (S.fromList ls) `shouldReturn` Prelude.any f ls


testAnd :: [Bool] -> Expectation
testAnd ls = S.fold FL.and (S.fromList ls) `shouldReturn` Prelude.and ls


testOr :: [Bool] -> Expectation
testOr ls = S.fold FL.or (S.fromList ls) `shouldReturn` Prelude.or ls


main :: IO ()
main = hspec $
    describe "Fold Tests" $ do
        prop "testRollingHashFirstN" testRollingHashFirstN
        prop "testIndex" $ testIndex
        prop "testHead" testHead
        prop "testLast" testLast
        prop "testLength" testLength
        prop "testSum" testSum
        prop "testProduct" testProduct
        prop "testMaximumBy" $ testMaximumBy intMin compare
        prop "testMaximum" $ testMaximum intMin
        prop "testMinimumBy" $ testMinimumBy intMax compare
        prop "testMinimum" $ testMinimum intMax
        prop "testToList" testToList
        prop "testFind" $ testFind predicate
        prop "testFindIndex" $ testFindIndex predicate
        prop "testElemIndex" $ testElemIndex 10
        prop "testNull" testNull
        prop "testElem" $ testElem 10
        prop "testNotElem" $ testNotElem 10
        prop "testAll" $ testAll predicate
        prop "testAny" $ testAny predicate
        prop "testAnd" testAnd
        prop "testOr" testOr
