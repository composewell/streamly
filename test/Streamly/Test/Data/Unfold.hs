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
import qualified Prelude
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.StreamK as K

import Control.Monad.Trans.State.Strict
import Data.Functor.Identity
import Prelude hiding (const, take, drop, concat, mapM)
import Test.Hspec as H
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Function

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

-------------------------------------------------------------------------------
-- Operations on input
-------------------------------------------------------------------------------

lmapM :: Bool
lmapM =
    let unf = UF.lmapM (\x -> modify (+ 1) >> return x) (UF.function id)
     in testUnfoldMD unf 1 0 1 [1]

both :: Bool
both =
    let unf = UF.both 1 (UF.function id)
     in testUnfold unf undefined ([1] :: [Int])

first :: Bool
first =
    let unf = UF.first 1 (UF.function id)
     in testUnfold unf 2 ([(1, 2)] :: [(Int, Int)])

second :: Bool
second =
    let unf = UF.second 1 (UF.function id)
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
    let unf = UF.swap (UF.function id)
     in testUnfold unf ((1, 2) :: (Int, Int)) [(2, 1)]

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
              let unf = UF.postscan Fold.sum UF.fromList
                  mList = scanl1 (+) ls
              in testUnfold unf ls mList

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
                  fM x y = modify (+ 1) >> return (fA x y)
                  unf = UF.mapM2 fM UF.fromList
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
        unf = UF.many unfIn unfOut
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

concatMapM :: Bool
concatMapM =
    let inner b =
          let u = UF.lmap (\_ -> (10, modify (+ 1) >> return b)) UF.replicateM
           in modify (+ 1) >> return u
        unf = UF.concatMapM inner UF.enumerateFromToIntegral
        list = List.concatMap (replicate 10) [1 .. 10]
     in testUnfoldMD unf (1, 10) 0 110 list

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

testGeneration :: Spec
testGeneration =
    describe "Generation"
        $ do
            prop "fromStream" fromStream
            prop "fromStreamK" fromStreamK
            prop "fromStreamD" fromStreamD
            prop "nilM" nilM
            prop "consM" consM
            prop "functionM" functionM
            -- prop "function" function
            -- prop "identity" identity
            prop "const" const
            prop "unfoldrM" unfoldrM
            -- prop "fromList" fromList
            prop "fromListM" fromListM
            -- prop "fromSVar" fromSVar
            -- prop "fromProducer" fromProducer
            prop "replicateM" replicateM
            prop "repeatM" repeatM
            prop "iterateM" iterateM
            prop "fromIndicesM" fromIndicesM
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
            prop "mapM" mapM
            prop "mapM2" mapM2
            prop "takeWhileM" takeWhileM
            -- prop "takeWhile" takeWhile
            prop "take" take
            -- prop "filter" filter
            prop "filterM" filterM
            prop "drop" drop
            -- prop "dropWhile" dropWhile
            prop "dropWhileM" dropWhileM

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
            -- prop "ap" ap
            -- prop "apDiscardFst" apDiscardFst
            -- prop "apDiscardSnd" apDiscardSnd

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
