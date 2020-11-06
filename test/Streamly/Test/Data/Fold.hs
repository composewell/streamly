module Main (main) where

import Prelude hiding (maximum, minimum, elem, notElem, null, product, sum, head, last)
import qualified Streamly.Internal.Data.Fold as F
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

rollingHashFirstN :: Property
rollingHashFirstN =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (choose (0, len)) $ \n ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
                a <- run $ S.fold F.rollingHash $ S.take n $ S.fromList vec
                b <- run $ S.fold (F.rollingHashFirstN n) $ S.fromList vec
                assert $ a == b


head :: [Int] -> Expectation
head ls = S.fold FL.head (S.fromList ls) `shouldReturn` headl ls
            where
            headl [] = Nothing
            headl (x:_) = Just x


length :: [Int] -> Expectation
length ls = S.fold FL.length (S.fromList ls) `shouldReturn` Prelude.length ls


sum :: [Int] -> Expectation
sum ls = S.fold FL.sum (S.fromList ls) `shouldReturn` foldl (+) 0 ls


product :: [Int] -> Expectation
product ls = S.fold FL.product (S.fromList ls) `shouldReturn` foldl (*) 1 ls


lesser :: (a -> a -> Ordering) -> a -> a -> a
lesser f x y = if f x y == LT then x else y


greater :: (a -> a -> Ordering) -> a -> a -> a
greater f x y = if f x y == GT then x else y


foldMaybe :: (b -> a -> b) -> b -> [a] -> Maybe b
foldMaybe f acc ls =
                  case ls of
                     [] -> Nothing
                     _ -> Just (foldl f acc ls)


maximumBy :: (Ord a, Show a) => a -> (a -> a -> Ordering) -> [a] -> Expectation
maximumBy genmin f ls = S.fold (FL.maximumBy f) (S.fromList ls) `shouldReturn` foldMaybe (greater f) genmin ls


maximum :: (Show a, Ord a) => a -> [a] -> Expectation
maximum genmin ls = S.fold FL.maximum (S.fromList ls) `shouldReturn` foldMaybe (greater compare) genmin ls


minimumBy :: (Ord a, Show a) => a -> (a -> a -> Ordering) -> [a] -> Expectation
minimumBy genmax f ls = S.fold (FL.minimumBy f) (S.fromList ls) `shouldReturn` foldMaybe (lesser f) genmax ls


minimum :: (Show a, Ord a) => a -> [a] -> Expectation
minimum genmax ls = S.fold FL.minimum (S.fromList ls) `shouldReturn` foldMaybe (lesser compare) genmax ls


toList :: [Int] -> Expectation
toList ls = S.fold FL.toList (S.fromList ls) `shouldReturn` ls


safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs


last :: [String] -> Expectation
last ls = S.fold FL.last (S.fromList ls) `shouldReturn` safeLast ls


nth :: Int -> [a] -> Maybe a
nth idx (x:xs) = if idx == 0
                 then Just x
                 else if idx < 0
                      then Nothing
                      else nth (idx - 1) xs
nth _ [] = Nothing


index :: Int -> [String] -> Expectation
index idx ls = let x = S.fold (FL.index idx) (S.fromList ls)
                in x `shouldReturn` (nth idx ls)


find :: (Show a, Eq a) => (a -> Bool) -> [a] -> Expectation
find f ls = do
   let x = S.fold (FL.findIndex f) (S.fromList ls)
   y <- x
   case y of
      Nothing  -> S.fold (FL.find f) (S.fromList ls) `shouldReturn` Nothing
      Just idx -> S.fold (FL.any f) (S.fromList $ take idx ls) `shouldReturn` False


neg :: (a -> Bool) -> a -> Bool
neg f x = if f x == True then False else True

findIndex :: (a -> Bool) -> [a] -> Expectation
findIndex f ls = do
   let x = S.fold (FL.findIndex f) (S.fromList ls)
   y <- x
   case y of
      Nothing  -> S.fold (FL.all $ neg f) (S.fromList ls) `shouldReturn` True
      Just idx -> if idx == 0
                  then S.fold (FL.all f) (S.fromList []) `shouldReturn` True
                  else S.fold (FL.all f) (S.fromList $ (take idx ls)) `shouldReturn` False

predicate :: Int -> Bool
predicate x = if x * x < 100 then True else False

elemIndex :: Int -> [Int] -> Expectation
elemIndex elm ls = do
   let x = S.fold (FL.elemIndex elm) (S.fromList ls)
   y <- x
   case y of
      Nothing  -> S.fold (FL.any (\z -> if z == elm
                                        then True
                                        else False)) (S.fromList ls) `shouldReturn` False
      Just idx -> S.fold (FL.any (\z -> if z == elm
                                        then True
                                        else False)) (S.fromList $ (take idx ls)) `shouldReturn` False

null :: [Int] -> Expectation
null ls = S.fold FL.null (S.fromList ls) `shouldReturn` case ls of
                                                            [] -> True
                                                            _ -> False
elem :: Int -> [Int] -> Expectation
elem elm ls = do
   let x = S.fold (FL.elem elm) (S.fromList ls)
   y <- x
   S.fold (FL.any (\z -> if z == elm
                         then True
                         else False)) (S.fromList ls) `shouldReturn` y


notElem :: Int -> [Int] -> Expectation
notElem elm ls = do
   let x = S.fold (FL.notElem elm) (S.fromList ls)
   y <- x
   S.fold (FL.any (\z -> if z == elm
                         then True
                         else False)) (S.fromList ls) `shouldReturn` (if y == True
                                                                      then False
                                                                      else True)


all :: (a -> Bool) -> [a] -> Expectation
all f ls = S.fold (FL.all f) (S.fromList ls) `shouldReturn` Prelude.and (map f ls)


any :: (a -> Bool) -> [a] -> Expectation
any f ls = S.fold (FL.any f) (S.fromList ls) `shouldReturn` Prelude.any f ls


and :: [Bool] -> Expectation
and ls = S.fold FL.and (S.fromList ls) `shouldReturn` Prelude.and ls


or :: [Bool] -> Expectation
or ls = S.fold FL.or (S.fromList ls) `shouldReturn` Prelude.or ls


main :: IO ()
main = hspec $
    describe "Fold s" $ do
        prop "RollingHashFirstN" rollingHashFirstN
        prop "Index" $ index
        prop "Head" head
        prop "Last" last
        prop "Length" Main.length
        prop "Sum" sum
        prop "Product" product
        prop "MaximumBy" $ maximumBy intMin compare
        prop "Maximum" $ maximum intMin
        prop "MinimumBy" $ minimumBy intMax compare
        prop "Minimum" $ minimum intMax
        prop "ToList" toList
        prop "Find" $ find predicate
        prop "FindIndex" $ findIndex predicate
        prop "ElemIndex" $ elemIndex 10
        prop "Null" null
        prop "Elem" $ elem 10
        prop "NotElem" $ notElem 10
        prop "All" $ Main.all predicate
        prop "Any" $ Main.any predicate
        prop "And" Main.and
        prop "Or" Main.or
