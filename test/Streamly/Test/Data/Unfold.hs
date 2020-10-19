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

import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Prelude
import qualified Data.List as List

import Control.Monad.Trans.State.Strict
import Data.Functor.Identity
import Prelude hiding (const, take, drop, concat)
import Streamly.Prelude (SerialT)
import Test.Hspec as H
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Function

-- We check for side effects in most cases

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

testUnfoldA ::
       (Eq s, Eq b) => Unfold (State s) a b -> s -> s -> a -> [b] -> Bool
testUnfoldA unf si sf seed lst = evalState action si

    where

    action = do
        x <- S.toList $ S.unfold unf seed
        y <- get
        return $ x == lst && y == sf

testUnfoldAD :: Unfold (State Int) a Int -> Int -> Int -> a -> [Int] -> Bool
testUnfoldAD = testUnfoldA

testUnfold :: Eq b => Unfold Identity a b -> a -> [b] -> Bool
testUnfold unf seed lst = runIdentity action

    where

    action = do
        x <- S.toList $ S.unfold unf seed
        return $ x == lst

testUnfoldD :: Unfold Identity a Int -> a -> [Int] -> Bool
testUnfoldD = testUnfold

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

fromStream :: Property
fromStream =
    property
        $ \n ->
              testUnfoldD UF.fromStream (S.fromList n :: SerialT Identity Int) n

nilM :: Bool
nilM =
    let unf = UF.nilM put
     in testUnfoldAD unf 0 1 1 []

consM :: Bool
consM =
    let cns = UF.consM (\a -> modify (+ a) >> get)
        unf = cns $ cns $ UF.nilM $ \a -> modify (+ a)
     in testUnfoldAD unf 0 3 1 [1, 2]

effect :: Bool
effect =
    let unf = UF.effect (modify (+ 1) >> get)
     in testUnfoldAD unf 0 1 undefined [1]

singletonM :: Bool
singletonM =
    let unf = UF.singletonM (\a -> modify (+ a) >> get)
     in testUnfoldAD unf 0 1 1 [1]

const :: Bool
const =
    let unf = UF.take 10 $ UF.const (modify (+ 1) >> get)
     in testUnfoldAD unf 0 10 (0 :: Int) [1 .. 10]

fromListM :: Property
fromListM =
    property
        $ \n ->
              let lst = Prelude.map (\x -> modify (+ 1) >> return x) n
               in testUnfoldAD UF.fromListM 0 (length n) lst n

replicateM :: Property
replicateM =
    property
        $ \i ->
              let ns = max 0 i + 1
                  seed = modify (+ 1) >> get
               in testUnfoldAD (UF.replicateM i) 0 ns seed [1 .. i]

repeatM :: Bool
repeatM =
    testUnfoldAD (UF.take 10 UF.repeatM) 0 10 (modify (+ 1) >> get) [1 .. 10]

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
        $ \f n ->
              let fM x =
                      if apply f x
                      then modify (+ 1) >> return True
                      else return False
                  unf = UF.takeWhileM fM UF.fromList
                  fL = Prelude.takeWhile (apply f) n
                  fS = Prelude.length fL
               in testUnfoldAD unf 0 fS n fL

filterM :: Property
filterM =
    property
        $ \f n ->
              let fM x =
                      if apply f x
                      then modify (+ 1) >> return True
                      else return False
                  unf = UF.filterM fM UF.fromList
                  fL = Prelude.filter (apply f) n
                  fS = Prelude.length fL
               in testUnfoldAD unf 0 fS n fL

drop :: Property
drop =
    property
        $ \i n ->
              let unf = UF.drop i UF.fromList
                  fL = Prelude.drop i n
               in testUnfoldD unf n fL

dropWhileM :: Property
dropWhileM =
    property
        $ \f n ->
              let fM x =
                      if apply f x
                      then modify (+ 1) >> return True
                      else return False
                  unf = UF.dropWhileM fM UF.fromList
                  fL = Prelude.dropWhile (apply f) n
                  fS = Prelude.length n - Prelude.length fL
               in testUnfoldAD unf 0 fS n fL

enumerateFromStepIntegral :: Property
enumerateFromStepIntegral =
    property
        $ \f s ->
              let unf = UF.take 10 UF.enumerateFromStepIntegral
                  lst = Prelude.take 10 $ List.unfoldr (\x -> Just (x, x + s)) f
               in testUnfoldD unf (f, s) lst

enumerateFromToIntegral :: Property
enumerateFromToIntegral =
    property
        $ \f t ->
              let unf = UF.enumerateFromToIntegral t
               in testUnfoldD unf f [f .. t]

zipWithM :: Property
zipWithM =
    property
        $ \f ->
              let unf1 = UF.enumerateFromToIntegral 10
                  unf2 = UF.enumerateFromToIntegral 20
                  fA = applyFun2 f :: Int -> Int -> Int
                  fM a b = modify (+ 1) >> return (fA a b)
                  unf = UF.zipWithM fM unf1 unf2
                  lst = Prelude.zipWith fA [1 .. 10] [1 .. 20]
               in testUnfoldAD unf 0 10 (1, 1) lst

concat :: Bool
concat =
    let unfIn = UF.replicateM 10
        unfOut = UF.map return $ UF.enumerateFromToIntegral 10
        unf = UF.concat unfOut unfIn
        lst = Prelude.concat $ Prelude.map (Prelude.replicate 10) [1 .. 10]
     in testUnfoldD unf 1 lst

outerProduct :: Bool
outerProduct =
    let unf1 = UF.enumerateFromToIntegral 10
        unf2 = UF.enumerateFromToIntegral 20
        unf = UF.outerProduct unf1 unf2
        lst = [(a, b) :: (Int, Int) | a <- [0 .. 10], b <- [0 .. 20]]
     in testUnfold unf ((0, 0) :: (Int, Int)) lst

main :: IO ()
main = hspec $
    describe "Unfold tests" $ do
       prop "fromStream" fromStream
       prop "nilM" nilM
       prop "consM" consM
       prop "effect" effect
       prop "singletonM" singletonM
       prop "const" const
       prop "fromListM" fromListM
       prop "replicateM" replicateM
       prop "repeatM" repeatM
       prop "take" take
       prop "takeWhileM" takeWhileM
       prop "filterM" filterM
       prop "drop" drop
       prop "dropWhileM" dropWhileM
       prop "enumerateFromStepIntegral" enumerateFromStepIntegral
       prop "enumerateFromToIntegral" enumerateFromToIntegral
       prop "zipWithM" zipWithM
       prop "concat" concat
       prop "outerProduct" outerProduct
       -- prop "concatMapM" concatMapM
       -- prop "gbracket" gbracket
       -- prop "gbracketIO" gbracketIO
       -- prop "before" before
       -- prop "after" after
       -- prop "afterIO" afterIO
       -- prop "onException" onException
       -- prop "finally" finally
       -- prop "finallyIO" finallyIO
       -- prop "bracket" bracket
       -- prop "bracketIO" bracketIO
