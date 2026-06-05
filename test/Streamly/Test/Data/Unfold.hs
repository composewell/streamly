{-# OPTIONS_GHC -Wno-deprecations #-}
-- |
-- Module      : Main
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Main (main) where

import Streamly.Internal.Data.Unfold (Unfold)

import qualified Data.List as List
import qualified Data.Tuple as Tuple
import qualified Prelude
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.StreamK as K

import Control.Exception (Exception, SomeException, try)
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.State.Strict
import Data.Functor.Identity
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign.Marshal.Array (withArray)
import Prelude hiding (const, take, drop, concat, mapM, either, filter, dropWhile, repeat, scanl)
import Test.Hspec as H
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Function

newtype TestException = TestException String deriving (Eq, Show)
instance Exception TestException

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

-- | @testUnfoldM unf seed initial final xs@, runs an unfold under state monad
-- using @initial@ as the initial state. @final@ is the expected state after
-- running it and @xs@ is the list of elements the stream must produce.
testUnfoldM ::
       (Eq s, Eq b) => Unfold (State s) a b -> a -> s -> s -> [b] -> Bool
testUnfoldM unf seed si sf lst = evalState action si

    where

    action = do
        x <- S.fold Fold.toList $ S.unfold unf seed
        y <- get
        return $ x == lst && y == sf

testUnfoldMD :: Unfold (State Int) a Int -> a -> Int -> Int -> [Int] -> Bool
testUnfoldMD = testUnfoldM

-- | This is similar to 'testUnfoldM' but without the state monad.
testUnfold :: Eq b => Unfold Identity a b -> a -> [b] -> Bool
testUnfold unf seed lst = runIdentity action

    where

    action = do
        x <- S.fold Fold.toList $ S.unfold unf seed
        return $ x == lst

testUnfoldD :: Unfold Identity a Int -> a -> [Int] -> Bool
testUnfoldD = testUnfold

-- | Like 'testUnfold' but compares the outputs as multisets (order
-- independent). Useful for combinators like 'UF.fairCross' and
-- 'UF.unfoldEachInterleave' that produce the same elements as their
-- non-interleaving counterparts but in a different order.
testUnfoldSorted :: Ord b => Unfold Identity a b -> a -> [b] -> Bool
testUnfoldSorted unf seed lst = runIdentity action

    where

    action = do
        x <- S.fold Fold.toList $ S.unfold unf seed
        return $ List.sort x == List.sort lst

-------------------------------------------------------------------------------
-- Operations on input
-------------------------------------------------------------------------------

lmapM :: Bool
lmapM =
    let unf = UF.lmapM (\x -> modify (+ 1) >> return x) (UF.function id)
     in testUnfoldMD unf 1 0 1 [1]

both :: Bool
both =
    let unf = UF.supply 1 (UF.function id)
     in testUnfold unf () ([1] :: [Int])

first :: Bool
first =
    let unf = UF.supplyFirst 1 (UF.function id)
     in testUnfold unf 2 ([(1, 2)] :: [(Int, Int)])

second :: Bool
second =
    let unf = UF.supplySecond 1 (UF.function id)
     in testUnfold unf 2 ([(2, 1)] :: [(Int, Int)])

discardFirst :: Bool
discardFirst =
    let unf = UF.discardFirst (UF.function id)
     in testUnfold unf ((1, 2) :: (Int, Int)) [2]

discardSecond :: Bool
discardSecond =
    let unf = UF.discardSecond (UF.function id)
     in testUnfold unf ((1, 2) :: (Int, Int)) [1]

swap :: Bool
swap =
    let unf = UF.lmap Tuple.swap (UF.function id)
     in testUnfold unf ((1, 2) :: (Int, Int)) [(2, 1)]

consInput :: Bool
consInput =
    let unf = UF.consInput (UF.function (* 2))
     in testUnfold unf (3 :: Int) [3, 6]

consInputWith :: Bool
consInputWith =
    let unf = UF.consInputWith length UF.fromList
     in testUnfold unf ([1, 2, 3] :: [Int]) [3, 1, 2, 3]

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

fromStream :: Property
fromStream =
    property
        $ \list ->
              testUnfoldD
                  UF.fromStream
                  (S.fromList list :: S.Stream Identity Int)
                  list

fromStreamD :: Property
fromStreamD =
    property
        $ \list -> testUnfoldD UF.fromStreamD (D.fromList list) (list :: [Int])

fromStreamK :: Property
fromStreamK =
    property
        $ \list -> testUnfoldD UF.fromStreamK (K.fromList list) (list :: [Int])

nilM :: Bool
nilM =
    let unf = UF.nilM put
     in testUnfoldMD unf 1 0 1 []

consM :: Bool
consM =
    let cns = UF.consM (\a -> modify (+ a) >> get)
        unf = cns $ cns $ UF.nilM $ \a -> modify (+ a)
     in testUnfoldMD unf 1 0 3 [1, 2]

functionM :: Bool
functionM =
    let unf = UF.functionM (\a -> modify (+ a) >> get)
     in testUnfoldMD unf 1 0 1 [1]

const :: Bool
const =
    let unf = UF.fromEffect (modify (+ 1) >> get)
     in testUnfoldMD unf (0 :: Int) 0 1 [1]

unfoldrM :: Property
unfoldrM =
    property
        $ \gen ->
              let genA = apply gen :: Int -> Maybe (Int, Int)
                  genM x = modify (+ 1) >> return (genA x)
                  list = Prelude.take 100 $ List.unfoldr genA 1
                  unf = UF.take 100 $ UF.unfoldrM genM
                  ll = length list
                  fs = if ll < 100 then ll + 1 else 100
               in testUnfoldMD unf 1 0 fs list

fromListM :: Property
fromListM =
    property
        $ \list ->
              let listM = Prelude.map (\x -> modify (+ 1) >> return x) list
               in testUnfoldMD UF.fromListM listM 0 (length list) list

replicateM :: Property
replicateM =
    property
        $ \i ->
              let ns = max 0 i
                  seed = (i, modify (+ 1) >> get)
               in testUnfoldMD UF.replicateM seed 0 ns [1 .. i]

repeatM :: Bool
repeatM =
    testUnfoldMD (UF.take 10 UF.repeatM) (modify (+ 1) >> get) 0 10 [1 .. 10]

iterateM :: Property
iterateM =
    property
        $ \next ->
              let nextA = apply next :: Int -> Int
                  nextM x = modify (+ 1) >> return (nextA x)
                  list = Prelude.take 100 $ List.iterate nextA 1
                  unf = UF.take 100 $ UF.iterateM nextM
               in testUnfoldMD unf (modify (+ 10) >> return 1) 0 110 list

fromIndicesM :: Property
fromIndicesM =
    property
        $ \indF ->
              let indFA = apply indF :: Int -> Int
                  indFM x = modify (+ 1) >> return (indFA x)
                  list = Prelude.take 100 $ Prelude.map indFA [1 ..]
                  unf = UF.take 100 $ UF.fromIndicesM indFM
               in testUnfoldMD unf 1 0 (length list) list

nil :: Bool
nil = testUnfold UF.nil (1 :: Int) ([] :: [Int])

repeat :: Bool
repeat = testUnfold (UF.take 5 UF.repeat) (1 :: Int) [1,1,1,1,1]

-------------------------------------------------------------------------------
-- Test for Num type
-------------------------------------------------------------------------------
enumerateFromNum :: Property
enumerateFromNum =
    property
        $ \f ->
                let unf = UF.take 50 UF.enumerateFromNum
                in testUnfold unf (f :: Int) $
                    Prelude.take 50 $ Prelude.enumFrom f

enumerateFromThenNum :: Property
enumerateFromThenNum =
    property
        $ \f th ->
                let unf = UF.take 50 UF.enumerateFromThenNum
                in testUnfold unf (f :: Int, th) $
                    Prelude.take 50 $ Prelude.enumFromThen f th

-------------------------------------------------------------------------------
-- Test for Integral type
-------------------------------------------------------------------------------
enumerateFromIntegral :: Property
enumerateFromIntegral =
    property
        $ \f ->
                let unf = UF.take 50 UF.enumerateFromIntegral
                in testUnfold unf (f :: Integer) $
                    Prelude.take 50 $ Prelude.enumFrom f

enumerateFromThenIntegral :: Property
enumerateFromThenIntegral =
    property
        $ \f th ->
                let unf = UF.take 50 UF.enumerateFromThenIntegral
                in testUnfold unf (f :: Integer, th) $
                    Prelude.take 50 $ Prelude.enumFromThen f th

enumerateFromThenToIntegral :: Property
enumerateFromThenToIntegral =
    property
        $ \f th to ->
                let unf = UF.take 50 UF.enumerateFromThenToIntegral
                in testUnfold unf (f :: Integer, th, to) $
                    Prelude.take 50 $ Prelude.enumFromThenTo f th to

enumerateFromToIntegral :: Property
enumerateFromToIntegral =
    property
        $ \f to ->
                let unf = UF.take 50 UF.enumerateFromToIntegral
                in testUnfold unf (f :: Integer, to) $
                    Prelude.take 50 $ Prelude.enumFromTo f to

enumerateFromIntegralBounded :: Property
enumerateFromIntegralBounded =
    property
        $ \f ->
                let unf = UF.take 50 UF.enumerateFromIntegralBounded
                in testUnfold unf (f :: Int) $
                    Prelude.take 50 $ Prelude.enumFrom f

enumerateFromThenIntegralBounded :: Property
enumerateFromThenIntegralBounded =
    property
        $ \f th ->
                let unf = UF.take 50 UF.enumerateFromThenIntegralBounded
                in testUnfold unf (f :: Int, th) $
                    Prelude.take 50 $ Prelude.enumFromThen f th

enumerateFromToIntegralBounded :: Property
enumerateFromToIntegralBounded =
    property
        $ \f to ->
                let unf = UF.take 50 UF.enumerateFromToIntegralBounded
                in testUnfold unf (f :: Int, to) $
                    Prelude.take 50 $ Prelude.enumFromTo f to

enumerateFromThenToIntegralBounded :: Property
enumerateFromThenToIntegralBounded =
    property
        $ \f th to ->
                let unf = UF.take 50 UF.enumerateFromThenToIntegralBounded
                in testUnfold unf (f :: Int, th, to) $
                    Prelude.take 50 $ Prelude.enumFromThenTo f th to

enumerateFromSmallBounded :: Property
enumerateFromSmallBounded =
    property
        $ \f ->
                let unf = UF.take 50 UF.enumerateFromSmallBounded
                in testUnfold unf (f :: Char) $
                    Prelude.take 50 $ Prelude.enumFrom f

enumerateFromThenSmallBounded :: Property
enumerateFromThenSmallBounded =
    property
        $ \f th ->
                let unf = UF.take 50 UF.enumerateFromThenSmallBounded
                in testUnfold unf (f :: Char, th) $
                    Prelude.take 50 $ Prelude.enumFromThen f th

enumerateFromToSmall :: Property
enumerateFromToSmall =
    property
        $ \f to ->
                let unf = UF.take 50 UF.enumerateFromToSmall
                in testUnfold unf (f :: Char, to) $
                    Prelude.take 50 $ Prelude.enumFromTo f to

enumerateFromThenToSmall :: Property
enumerateFromThenToSmall =
    property
        $ \f th to ->
                let unf = UF.take 50 UF.enumerateFromThenToSmall
                in testUnfold unf (f :: Char, th, to) $
                    Prelude.take 50 $ Prelude.enumFromThenTo f th to

enumerateFromSmallBoundedOrd :: Property
enumerateFromSmallBoundedOrd =
    property
        $ \f ->
                let unf = UF.take 3 UF.enumerateFromSmallBounded
                in testUnfold unf (f :: Ordering) $
                    Prelude.take 3 $ Prelude.enumFrom f

enumerateFromThenSmallBoundedOrd :: Property
enumerateFromThenSmallBoundedOrd =
    property
        $ \f th  ->
                let unf = UF.take 3 UF.enumerateFromThenSmallBounded
                in testUnfold unf (f :: Ordering, th) $
                    Prelude.take 3 $ Prelude.enumFromThen f th

enumerateFromToSmallOrd :: Property
enumerateFromToSmallOrd =
    property
        $ \f to ->
                let unf = UF.take 3 UF.enumerateFromToSmall
                in testUnfold unf (f :: Ordering, to) $
                    Prelude.take 3 $ Prelude.enumFromTo f to

enumerateFromThenToSmallOrd :: Property
enumerateFromThenToSmallOrd =
    property
        $ \f th to ->
                let unf = UF.take 3 UF.enumerateFromThenToSmall
                in testUnfold unf (f :: Ordering, th, to) $
                    Prelude.take 3 $ Prelude.enumFromThenTo f th to

-------------------------------------------------------------------------------
enumerateFromSmallBoundedBool :: Property
enumerateFromSmallBoundedBool =
    property
        $ \f ->
                let unf = UF.take 2 UF.enumerateFromSmallBounded
                in testUnfold unf (f :: Bool) $
                    Prelude.take 2 $ Prelude.enumFrom f

enumerateFromThenSmallBoundedBool :: Property
enumerateFromThenSmallBoundedBool =
    property
        $ \f th  ->
                let unf = UF.take 2 UF.enumerateFromThenSmallBounded
                in testUnfold unf (f :: Bool, th) $
                    Prelude.take 2 $ Prelude.enumFromThen f th

enumerateFromToSmallBool :: Property
enumerateFromToSmallBool =
    property
        $ \f to ->
                let unf = UF.take 2 UF.enumerateFromToSmall
                in testUnfold unf (f :: Bool, to) $
                    Prelude.take 2 $ Prelude.enumFromTo f to

enumerateFromThenToSmallBool :: Property
enumerateFromThenToSmallBool =
    property
        $ \f th to ->
                let unf = UF.take 2 UF.enumerateFromThenToSmall
                in testUnfold unf (f :: Bool, th, to) $
                    Prelude.take 2 $ Prelude.enumFromThenTo f th to
-------------------------------------------------------------------------------
enumerateFromSmallBoundedUnit :: Property
enumerateFromSmallBoundedUnit =
    property
        $ \f ->
                let unf = UF.take 1 UF.enumerateFromSmallBounded
                in testUnfold unf (f :: ()) $
                    Prelude.take 1 $ Prelude.enumFrom f

enumerateFromThenSmallBoundedUnit :: Property
enumerateFromThenSmallBoundedUnit =
    property
        $ \f th  ->
                let unf = UF.take 1 UF.enumerateFromThenSmallBounded
                in testUnfold unf (f :: (), th) $
                    Prelude.take 1 $ Prelude.enumFromThen f th

enumerateFromToSmallUnit :: Property
enumerateFromToSmallUnit =
    property
        $ \f to ->
                let unf = UF.take 1 UF.enumerateFromToSmall
                in testUnfold unf (f :: (), to) $
                    Prelude.take 1 $ Prelude.enumFromTo f to

enumerateFromThenToSmallUnit :: Property
enumerateFromThenToSmallUnit =
    property
        $ \f th to ->
                let unf = UF.take 1 UF.enumerateFromThenToSmall
                in testUnfold unf (f :: (), th, to) $
                    Prelude.take 1 $ Prelude.enumFromThenTo f th to

enumerateFromFractional :: Property
enumerateFromFractional =
    property
        $ \f ->
                let unf = UF.take 50 UF.enumerateFromFractional
                in testUnfold unf (f :: Double) $
                    Prelude.take 50 $ Prelude.enumFrom f

enumerateFromThenFractional :: Property
enumerateFromThenFractional =
    property
        $ \f th ->
                let unf = UF.take 50 UF.enumerateFromThenFractional
                in testUnfold unf (f :: Double, th) $
                    Prelude.take 50 $ Prelude.enumFromThen f th

enumerateFromThenToFractional :: Property
enumerateFromThenToFractional =
    property
        $ \f th to ->
                let unf = UF.take 50 UF.enumerateFromThenToFractional
                in testUnfold  unf (f :: Double, th, to) $
                    Prelude.take 50 $ Prelude.enumFromThenTo f th to

enumerateFromToFractional :: Property
enumerateFromToFractional =
    property
        $ \f t ->
                let unf = UF.enumerateFromToFractional
                in testUnfold unf (f :: Double, t) [f..(t :: Double)]

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

postscan :: Property
postscan =
    property
        $ \(ls :: [Int]) ->
              let unf = UF.postscanl Scanl.sum UF.fromList
                  mList = scanl1 (+) ls
              in testUnfold unf ls mList

fold :: Bool
fold = runIdentity (UF.fold Fold.sum UF.fromList [1..10 :: Int]) == 55

scanl :: Bool
scanl =
    let unf = UF.scanl (Scanl.take 2 Scanl.sum) UF.fromList
    in testUnfold unf ([1,2,3,4,5] :: [Int]) [0,1,3]

scanlMany :: Bool
scanlMany =
    let unf = UF.scanlMany (Scanl.take 2 Scanl.sum) UF.fromList
    in testUnfold unf ([1,2,3,4,5] :: [Int]) [0,1,3,0,3,7,0,5]

foldMany :: Bool
foldMany =
    let unf = UF.foldMany (Fold.take 2 Fold.toList) UF.fromList
    in testUnfold unf ([1,2,3,4,5] :: [Int]) [[1,2],[3,4],[5]]

either :: Bool
either =
    let unf = UF.either UF.fromList UF.fromList
    in testUnfold unf (Left [1,2,3 :: Int]) [1,2,3]
       && testUnfold unf (Right [4,5,6 :: Int]) [4,5,6]

postscanlM' :: Bool
postscanlM' =
    let unf = UF.postscanlM' (\b a -> return (b + a)) (return (0 :: Int)) UF.fromList
    in testUnfold unf [1,2,3,4,5 :: Int] [1,3,6,10,15]

scan :: Bool
scan =
    let unf = UF.scan (Fold.take 2 Fold.sum) UF.fromList
    in testUnfold unf ([1,2,3,4,5] :: [Int]) [0,1,3]

scanMany :: Bool
scanMany =
    let unf = UF.scanMany (Fold.take 2 Fold.sum) UF.fromList
    in testUnfold unf ([1,2,3,4,5] :: [Int]) [0,1,3,0,3,7,0,5]

mapM :: Property
mapM =
    property
        $ \f list ->
              let fA = apply f :: Int -> Int
                  fM x = modify (+ 1) >> return (fA x)
                  unf = UF.mapM fM UF.fromList
                  mList = Prelude.map fA list
               in testUnfoldMD unf list 0 (length list) mList

mapM2 :: Property
mapM2 =
    property
        $ \f list ->
              let fA = applyFun2 f :: [Int] -> Int -> Int
                  fM (x, y) = modify (+ 1) >> return (fA x y)
                  unf = UF.mapM fM (UF.carryInput UF.fromList)
                  mList = Prelude.map (fA list) list
               in testUnfoldMD unf list 0 (length list) mList

take :: Property
take =
    property
        $ \i ->
              testUnfoldD
                  (UF.take i UF.repeatM)
                  (return 1)
                  (Prelude.take i (Prelude.repeat 1))

takeWhileM :: Property
takeWhileM =
    property
        $ \f list ->
              let fM x =
                      if apply f x
                      then modify (+ 1) >> return True
                      else return False
                  unf = UF.takeWhileM fM UF.fromList
                  fL = Prelude.takeWhile (apply f) list
                  fS = Prelude.length fL
               in testUnfoldMD unf list 0 fS fL

filterM :: Property
filterM =
    property
        $ \f list ->
              let fM x =
                      if apply f x
                      then modify (+ 1) >> return True
                      else return False
                  unf = UF.filterM fM UF.fromList
                  fL = Prelude.filter (apply f) list
                  fS = Prelude.length fL
               in testUnfoldMD unf list 0 fS fL

drop :: Property
drop =
    property
        $ \i list ->
              let unf = UF.drop i UF.fromList
                  fL = Prelude.drop i list
               in testUnfoldD unf list fL

dropWhileM :: Property
dropWhileM =
    property
        $ \f list ->
              let fM x =
                      if apply f x
                      then modify (+ 1) >> return True
                      else return False
                  unf = UF.dropWhileM fM UF.fromList
                  fL = Prelude.dropWhile (apply f) list
                  fS = Prelude.length list - Prelude.length fL
               in testUnfoldMD unf list 0 fS fL

filter :: Property
filter =
    property
        $ \f list ->
              let unf = UF.filter (apply f) UF.fromList
                  fL = Prelude.filter (apply f) (list :: [Int])
               in testUnfoldD unf list fL

dropWhile :: Property
dropWhile =
    property
        $ \f list ->
              let unf = UF.dropWhile (apply f) UF.fromList
                  fL = Prelude.dropWhile (apply f) (list :: [Int])
               in testUnfoldD unf list fL

mapMaybe :: Property
mapMaybe =
    property
        $ \list ->
              let f x = if even x then Just (x * 2 :: Int) else Nothing
                  unf = UF.mapMaybe f UF.fromList
                  expected = [x * 2 | x <- list :: [Int], even x]
               in testUnfoldD unf list expected

mapMaybeM :: Property
mapMaybeM =
    property
        $ \list ->
              let fM x =
                      if even x
                      then modify (+ 1) >> return (Just (x * 2))
                      else return Nothing
                  unf = UF.mapMaybeM fM UF.fromList
                  expected = [x * 2 | x <- list :: [Int], even x]
                  evens = Prelude.length $ Prelude.filter even list
               in testUnfoldMD unf list 0 evens expected

catMaybes :: Property
catMaybes =
    property
        $ \list ->
              let lst = Prelude.map (\x -> if even x then Just x else Nothing) (list :: [Int])
                  unf = UF.catMaybes UF.fromList
                  expected = [x | x <- list, even x]
               in testUnfoldD unf lst expected

-------------------------------------------------------------------------------
-- Stream combination
-------------------------------------------------------------------------------

zipWithM :: Property
zipWithM =
    property
        $ \f ->
            let unf1 = UF.enumerateFromToIntegral
                unf2 = UF.enumerateFromToIntegral
                fA = applyFun2 f :: Int -> Int -> Int
                fM a b = modify (+ 1) >> return (fA a b)
                unf = UF.zipWithM fM (UF.lmap fst unf1) (UF.lmap snd unf2)
                lst = Prelude.zipWith fA [1 .. 10] [1 .. 20]
            in testUnfoldMD unf ((1,10), (1,20)) 0 10 lst

concat :: Bool
concat =
    let unfIn = UF.replicateM
        unfOut = UF.map ((10,) . return) UF.enumerateFromToIntegral
        unf = UF.unfoldEach unfIn unfOut
        lst = Prelude.concat $ Prelude.map (Prelude.replicate 10) [1 .. 10]
     in testUnfoldD unf (1, 10) lst

outerProduct :: Bool
outerProduct =
    let unf1 = UF.enumerateFromToIntegral
        unf2 = UF.enumerateFromToIntegral
        unf = crossProduct unf1 unf2
        lst = [(a, b) :: (Int, Int) | a <- [0 .. 10], b <- [0 .. 20]]
     in testUnfold unf (((0,10) ,(0,20)) :: ((Int, Int), (Int, Int))) lst

    where

    crossProduct u1 u2 = UF.cross (UF.lmap fst u1) (UF.lmap snd u2)

crossApply :: Bool
crossApply =
    let unf1 = UF.enumerateFromToIntegral
        unf2 = UF.enumerateFromToIntegral
        unf = UF.crossApply
                  (UF.map (+) (UF.lmap fst unf1))
                  (UF.lmap snd unf2)
        lst = [a + b :: Int | a <- [0 .. 10], b <- [0 .. 20]]
     in testUnfold unf (((0, 10), (0, 20)) :: ((Int, Int), (Int, Int))) lst

crossApplyFst :: Bool
crossApplyFst =
    let unf1 = UF.enumerateFromToIntegral
        unf2 = UF.enumerateFromToIntegral
        unf = UF.crossApplyFst (UF.lmap fst unf1) (UF.lmap snd unf2)
        lst = [a :: Int | a <- [0 .. 10], _ <- [0 .. 20 :: Int]]
     in testUnfold unf (((0, 10), (0, 20)) :: ((Int, Int), (Int, Int))) lst

crossApplySnd :: Bool
crossApplySnd =
    let unf1 = UF.enumerateFromToIntegral
        unf2 = UF.enumerateFromToIntegral
        unf = UF.crossApplySnd (UF.lmap fst unf1) (UF.lmap snd unf2)
        lst = [b :: Int | _ <- [0 .. 10 :: Int], b <- [0 .. 20]]
     in testUnfold unf (((0, 10), (0, 20)) :: ((Int, Int), (Int, Int))) lst

concatMapM :: Bool
concatMapM =
    let inner b =
          let u = UF.lmap (\_ -> (10, modify (+ 1) >> return b)) UF.replicateM
           in modify (+ 1) >> return u
        unf = UF.concatMapM inner UF.enumerateFromToIntegral
        list = List.concatMap (replicate 10) [1 .. 10]
     in testUnfoldMD unf (1, 10) 0 110 list

fromTuple :: Bool
fromTuple = testUnfold UF.fromTuple ((1, 2) :: (Int, Int)) [1, 2]

interleave :: Bool
interleave =
    let unf = UF.interleave UF.fromList UF.fromList
     in testUnfold unf ([1, 3, 5], [2, 4, 6]) ([1 .. 6] :: [Int])

-- | 'fairCross' yields the same pairs as 'cross' (outerProduct) but in a
-- breadth-first order, so compare as multisets.
fairCross :: Bool
fairCross =
    let unf1 = UF.enumerateFromToIntegral
        unf2 = UF.enumerateFromToIntegral
        unf = UF.fairCross (UF.lmap fst unf1) (UF.lmap snd unf2)
        lst = [(a, b) :: (Int, Int) | a <- [0 .. 10], b <- [0 .. 20]]
     in testUnfoldSorted unf (((0, 10), (0, 20)) :: ((Int, Int), (Int, Int))) lst

-- | 'unfoldEachInterleave' yields the same elements as 'unfoldEach' (concat)
-- but interleaved breadth-first, so compare as multisets.
unfoldEachInterleave :: Bool
unfoldEachInterleave =
    let unfIn = UF.replicateM
        unfOut = UF.map ((10,) . return) UF.enumerateFromToIntegral
        unf = UF.unfoldEachInterleave unfIn unfOut
        lst = Prelude.concat $ Prelude.map (Prelude.replicate 10) [1 .. 10]
     in testUnfoldSorted unf (1, 10) (lst :: [Int])

innerJoin :: Bool
innerJoin =
    let unf = UF.innerJoin (==) (UF.lmap fst UF.fromList) (UF.lmap snd UF.fromList)
        ls1 = [1,2,3,4 :: Int]
        ls2 = [2,3,4,5 :: Int]
        expected = [(a,b) | a <- ls1, b <- ls2, a == b]
    in testUnfold unf (ls1, ls2) expected

zipRepeat :: Property
zipRepeat =
    property
        $ \list ->
              let c = 42 :: Int
                  unf = UF.zipRepeat UF.fromList
                  expected = Prelude.map (c,) (list :: [Int])
               in testUnfold unf (c, list) expected

-------------------------------------------------------------------------------
-- Resource management / exception tests
-------------------------------------------------------------------------------

testResourceManagement :: Spec
testResourceManagement =
    describe "Resource Management" $ do
        it "before" $ do
            ref <- newIORef (0 :: Int)
            xs <- S.fold Fold.toList
                      $ S.unfold
                          (UF.before (\_ -> writeIORef ref 1) UF.fromList)
                          [1,2,3 :: Int]
            val <- readIORef ref
            xs `shouldBe` [1,2,3]
            val `shouldBe` 1
        it "after_" $ do
            ref <- newIORef (0 :: Int)
            xs <- S.fold Fold.toList
                      $ S.unfold
                          (UF.after_ (\_ -> writeIORef ref 1) UF.fromList)
                          [1,2,3 :: Int]
            val <- readIORef ref
            xs `shouldBe` [1,2,3]
            val `shouldBe` 1
        it "afterIO" $ do
            ref <- newIORef (0 :: Int)
            xs <- S.fold Fold.toList
                      $ S.unfold
                          (UF.afterIO (\_ -> writeIORef ref 1) UF.fromList)
                          [1,2,3 :: Int]
            val <- readIORef ref
            xs `shouldBe` [1,2,3]
            val `shouldBe` 1
        it "finally_ normal end" $ do
            ref <- newIORef (0 :: Int)
            xs <- S.fold Fold.toList
                      $ S.unfold
                          (UF.finally_ (\_ -> writeIORef ref 1) UF.fromList)
                          [1,2,3 :: Int]
            val <- readIORef ref
            xs `shouldBe` [1,2,3]
            val `shouldBe` 1
        it "finally_ on exception" $ do
            ref <- newIORef (0 :: Int)
            let throwingList = [return 1 :: IO Int, throwM (TestException "e")]
            res <- try . S.fold Fold.drain
                       $ S.unfold
                           (UF.finally_ (\_ -> writeIORef ref 1) UF.fromListM)
                           throwingList
            val <- readIORef ref
            res `shouldBe` (Left (TestException "e") :: Either TestException ())
            val `shouldBe` 1
        it "finallyIO normal end" $ do
            ref <- newIORef (0 :: Int)
            xs <- S.fold Fold.toList
                      $ S.unfold
                          (UF.finallyIO (\_ -> writeIORef ref 1) UF.fromList)
                          [1,2,3 :: Int]
            val <- readIORef ref
            xs `shouldBe` [1,2,3]
            val `shouldBe` 1
        it "bracket_ alloc and cleanup" $ do
            ref <- newIORef (0 :: Int)
            xs <- S.fold Fold.toList
                      $ S.unfold
                          (UF.bracket_
                              (\(lst :: [Int]) -> writeIORef ref 1 >> return lst)
                              (\_ -> writeIORef ref 2)
                              UF.fromList)
                          [1,2,3 :: Int]
            val <- readIORef ref
            xs `shouldBe` [1,2,3]
            val `shouldBe` 2
        it "bracketIO alloc and cleanup" $ do
            ref <- newIORef (0 :: Int)
            xs <- S.fold Fold.toList
                      $ S.unfold
                          (UF.bracketIO
                              (\(lst :: [Int]) -> writeIORef ref 1 >> return lst)
                              (\_ -> writeIORef ref 2)
                              UF.fromList)
                          [1,2,3 :: Int]
            val <- readIORef ref
            xs `shouldBe` [1,2,3]
            val `shouldBe` 2
        it "onException runs on exception" $ do
            ref <- newIORef (0 :: Int)
            let throwingList = [return 1 :: IO Int, throwM (TestException "e"), return 3]
            res <- try . S.fold Fold.drain
                       $ S.unfold
                           (UF.onException (\_ -> writeIORef ref 1) UF.fromListM)
                           throwingList
            val <- readIORef ref
            res `shouldBe` (Left (TestException "e") :: Either TestException ())
            val `shouldBe` 1
        it "handle catches exception" $ do
            let throwingList = [return 1 :: IO Int, throwM (TestException "e"), return 3]
            xs <- S.fold Fold.toList
                      $ S.unfold
                          (UF.handle
                              (UF.lmap (\(_ :: TestException) -> [99 :: Int]) UF.fromList)
                              UF.fromListM)
                          throwingList
            xs `shouldBe` [1, 99]
        it "gbracket_ normal end" $ do
            ref <- newIORef (0 :: Int)
            xs <- S.fold Fold.toList
                      $ S.unfold
                          (UF.gbracket_
                              (\(lst :: [Int]) -> writeIORef ref 1 >> return lst)
                              (fmap Right)
                              (\_ -> writeIORef ref 2)
                              (UF.nil :: Unfold IO ([Int], SomeException) Int)
                              UF.fromList)
                          [1,2,3 :: Int]
            val <- readIORef ref
            xs `shouldBe` [1,2,3]
            val `shouldBe` 2
        it "gbracketIO normal end" $ do
            ref <- newIORef (0 :: Int)
            xs <- S.fold Fold.toList
                      $ S.unfold
                          (UF.gbracketIO
                              (\(lst :: [Int]) -> writeIORef ref 1 >> return lst)
                              (\_ -> writeIORef ref 2)
                              (\_ -> return ())
                              (UF.nil :: Unfold IO SomeException Int)
                              (fmap Right)
                              UF.fromList)
                          [1,2,3 :: Int]
            val <- readIORef ref
            xs `shouldBe` [1,2,3]
            val `shouldBe` 2

-------------------------------------------------------------------------------
-- Test groups
-------------------------------------------------------------------------------

testInputOps :: Spec
testInputOps =
    describe "Input"
        $ do
            -- prop "lmap" lmap
            prop "lmapM" lmapM
            prop "both" both
            prop "first" first
            prop "second" second
            prop "discardFirst" discardFirst
            prop "discardSecond" discardSecond
            prop "swap" swap
            prop "consInput" consInput
            prop "consInputWith" consInputWith

testGeneration :: Spec
testGeneration =
    describe "Generation"
        $ do
            prop "fromStream" fromStream
            prop "fromStreamK" fromStreamK
            prop "fromStreamD" fromStreamD
            prop "nilM" nilM
            prop "nil" nil
            prop "consM" consM
            prop "functionM" functionM
            -- prop "function" function
            -- prop "identity" identity
            prop "const" const
            prop "unfoldrM" unfoldrM
            -- prop "fromList" fromList
            prop "fromListM" fromListM
            prop "fromTuple" fromTuple
            -- prop "fromSVar" fromSVar
            -- prop "fromProducer" fromProducer
            prop "replicateM" replicateM
            prop "repeatM" repeatM
            prop "repeat" repeat
            prop "iterateM" iterateM
            prop "fromIndicesM" fromIndicesM
            it "fromPtr" $ withArray [1,2,3,4,5 :: Int] $ \ptr -> do
                xs <- S.fold Fold.toList $ S.unfold (UF.take 5 UF.fromPtr) ptr
                xs `shouldBe` [1,2,3,4,5]
            ----------- Enumerate from Num ------------------------------------
            prop "enumerateFromNum" enumerateFromNum
            prop "enumerateFromThenNum" enumerateFromThenNum
            ----------- Enumerate from Integral -------------------------------
            prop "enumerateFromIntegral" enumerateFromIntegral
            prop "enumerateFromThenIntegral" enumerateFromThenIntegral
            prop "enumerateFromToIntegral" enumerateFromToIntegral
            prop "enumerateFromThenToIntegral" enumerateFromThenToIntegral

            prop "enumerateFromIntegralBounded" enumerateFromIntegralBounded
            prop "enumerateFromThenIntegralBounded" enumerateFromThenIntegralBounded
            prop "enumerateFromToIntegralBounded" enumerateFromToIntegralBounded
            prop "enumerateFromThenToIntegralBounded" enumerateFromThenToIntegralBounded
            ----------- Enumerate from Small Integral -------------------------
            prop "enumerateFromSmallBounded" enumerateFromSmallBounded
            prop "enumerateFromThenSmallBounded" enumerateFromThenSmallBounded
            prop "enumerateFromToSmall" enumerateFromToSmall
            prop "enumerateFromThenToSmall" enumerateFromThenToSmall
            --
            prop "enumerateFromSmallBoundedOrd" enumerateFromSmallBoundedOrd
            prop "enumerateFromThenSmallBoundedOrd" enumerateFromThenSmallBoundedOrd
            prop "enumerateFromToSmallOrd" enumerateFromToSmallOrd
            prop "enumerateFromThenToSmallOrd" enumerateFromThenToSmallOrd

            prop "enumerateFromSmallBoundedUnit" enumerateFromSmallBoundedUnit
            prop "enumerateFromThenSmallBoundedUnit" enumerateFromThenSmallBoundedUnit
            prop "enumerateFromToSmallUnit" enumerateFromToSmallUnit
            prop "enumerateFromThenToSmallUnit" enumerateFromThenToSmallUnit

            prop "enumerateFromSmallBoundedUnit" enumerateFromSmallBoundedBool
            prop "enumerateFromThenSmallBoundedBool" enumerateFromThenSmallBoundedBool
            prop "enumerateFromToSmallBool" enumerateFromToSmallBool
            prop "enumerateFromThenToSmallBool" enumerateFromThenToSmallBool

            prop "enumerateFromFractional" enumerateFromFractional
            prop "enumerateFromThenFractional" enumerateFromThenFractional
            prop "enumerateFromToFractional" enumerateFromToFractional
            prop "enumerateFromThenToFractional" enumerateFromThenToFractional

testTransformation :: Spec
testTransformation =
    describe "Transformation"
        $ do
            -- prop "map" map
            prop "postscan" postscan
            prop "fold" fold
            prop "scanl" scanl
            prop "scanlMany" scanlMany
            prop "foldMany" foldMany
            prop "either" either
            prop "mapM" mapM
            prop "mapM2" mapM2
            prop "takeWhileM" takeWhileM
            -- prop "takeWhile" takeWhile
            prop "take" take
            prop "filter" filter
            prop "filterM" filterM
            prop "drop" drop
            prop "dropWhile" dropWhile
            prop "dropWhileM" dropWhileM
            prop "mapMaybe" mapMaybe
            prop "mapMaybeM" mapMaybeM
            prop "catMaybes" catMaybes
            -- deprecated
            prop "postscanlM'" postscanlM'
            prop "scan" scan
            prop "scanMany" scanMany

testCombination :: Spec
testCombination =
    describe "Transformation"
        $ do
            prop "zipWithM" zipWithM
            -- prop "zipWith" zipWith
            -- prop "teeZipWith" teeZipWith
            prop "concat" concat
            prop "concatMapM" concatMapM
            prop "outerProduct" outerProduct
            prop "crossApply" crossApply
            prop "crossApplyFst" crossApplyFst
            prop "crossApplySnd" crossApplySnd
            prop "fairCross" fairCross
            prop "interleave" interleave
            prop "unfoldEachInterleave" unfoldEachInterleave
            prop "innerJoin" innerJoin
            prop "zipRepeat" zipRepeat

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Unfold"

main :: IO ()
main =
    hspec
        $ describe moduleName
        $ do
            testInputOps
            testGeneration
            testTransformation
            testCombination
            testResourceManagement
