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

import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Prelude as P

import Control.Monad.Trans.State.Strict
import Data.Functor.Identity
import Prelude hiding (const, take, drop, concat)
import Streamly.Prelude (SerialT)
import Test.Hspec as H
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Function

fromStream :: Property
fromStream =
    property $ \n -> do
        let strm = S.fromList n :: SerialT IO Int
        n1 <- S.toList $ S.unfold UF.fromStream strm
        n1 `shouldBe` (n :: [Int])

nilM :: Bool
nilM = evalState action undefined
    where
    action = do
        x <- S.toList $ S.unfold (UF.nilM put) (1 :: Int)
        y <- get
        return $ x == ([] :: [Int]) && y == 1

consM :: Bool
consM = evalState action 0
  where
    action = do
        x <-
            S.toList $
            S.unfold
                (UF.consM (\a -> modify (+ a) >> get) $
                 UF.consM (\a -> modify (+ a) >> get) $ UF.nilM put)
                1
        y <- get
        return $ x == ([1, 2] :: [Int]) && y == 1

effect :: Bool
effect = evalState action undefined
  where
    action = do
        x <- S.toList $ S.unfold (UF.effect (put (1 :: Int))) undefined
        y <- get
        return $ x == [()] && y == 1

singletonM :: Bool
singletonM = evalState action undefined
  where
    action = do
        x <- S.toList $ S.unfold (UF.singletonM put) (1 :: Int)
        y <- get
        return $ x == [()] && y == 1

const :: Bool
const = evalState action 0
  where
    action = do
        x <-
            S.toList $
            S.take 10 $ S.unfold (UF.const (modify (+ 1) >> get)) undefined
        y <- get
        let rList = [1..10] :: [Int]
        return $ x == rList && y == 10

fromListM :: Property
fromListM = property $ \x -> evalState (action x) 0
  where
    action x = do
        let list = P.map (\a -> modify (+ a) >> return a) (x :: [Int])
        x1 <- S.toList $ S.unfold UF.fromListM list
        y <- get
        return $ x == x1 && y == foldr (+) 0 x

replicateM :: Property
replicateM =
    property $ \x ->
        runIdentity $ do
            x1 <- S.toList $ S.unfold (UF.replicateM x) (return ())
            let x2 = P.replicate x ()
            return $ x1 == x2

repeatM :: Property
repeatM =
    property $ \x ->
        runIdentity $ do
            x1 <- S.toList $ S.take x $ S.unfold UF.repeatM (return ())
            let x2 = P.take x $ P.repeat ()
            return $ x1 == x2

take :: Property
take =
    property $ \x ->
        runIdentity $ do
            x1 <- S.toList $ S.unfold (UF.take x UF.repeatM) (return ())
            x2 <- S.toList $ S.take x $ S.unfold UF.repeatM (return ())
            return $ x1 == x2

takeWhileM :: Property
takeWhileM = property $ \f x -> evalState (action f x) 0
  where
    action f x = do
        let f' z = if apply f z
                   then modify (+ 1) >> return True
                   else return False
        x1 <-
            S.toList $ S.unfold (UF.takeWhileM f' UF.fromListM) (P.map return x)
        let x2 = P.takeWhile (apply f) (x :: [Int])
        y1 <- get
        let y2 = P.length x2
        return $ x1 == x2 && y1 == y2

filterM :: Property
filterM = property $ \f x -> evalState (action f x) 0
  where
    action f x = do
        let f' z = if apply f z
                   then modify (+ 1) >> return True
                   else return False
        x1 <-
            S.toList $ S.unfold (UF.filterM f' UF.fromListM) (P.map return x)
        let x2 = P.filter (apply f) (x :: [Int])
        y1 <- get
        let y2 = P.length x2
        return $ x1 == x2 && y1 == y2

drop :: Property
drop =
    property $ \x y ->
        runIdentity $ do
            x1 <- S.toList
                      $ S.unfold (UF.drop x (UF.replicateM (x + y))) (return ())
            x2 <- S.toList
                      $ S.drop x $ S.unfold (UF.replicateM (x + y)) (return ())
            return $ x1 == x2

dropWhileM :: Property
dropWhileM = property $ \f x -> evalState (action f x) 0
  where
    action f x = do
        let f' z = if apply f z
                   then modify (+ 1) >> return True
                   else return False
        x1 <-
            S.toList $ S.unfold (UF.dropWhileM f' UF.fromListM) (P.map return x)
        let x2 = P.dropWhile (apply f) (x :: [Int])
        y1 <- get
        let y2 = P.length x - P.length x2
        return $ x1 == x2 && y1 == y2

enumerateFromStepIntegral :: Property
enumerateFromStepIntegral =
    property $ \x y ->
        runIdentity $ do
            x1 <-
                S.toList $
                S.unfold (UF.take y UF.enumerateFromStepIntegral) (0, x)
            let x2 = P.take y $ P.map (x *) [0 .. y]
            return $ x1 == x2

enumerateFromToIntegral :: Property
enumerateFromToIntegral =
    property $ \x ->
        runIdentity $ do
            x1 <- S.toList $ S.unfold (UF.enumerateFromToIntegral x) 0
            let x2 = [0 .. x] :: [Int]
            return $ x1 == x2

zipWithM :: Property
zipWithM = property $ \x -> evalState (action x) (0 :: Int)
  where
    action x = do
        let f a b = modify (+ 1) >> return (a + b)
        x1 <-
            S.toList $
            S.unfold
                (UF.zipWithM
                     f
                     (UF.enumerateFromToIntegral x)
                     (UF.enumerateFromToIntegral x))
                (1, 1)
        let x2 = P.zipWith (+) ([1 .. x] :: [Int]) ([1 .. x] :: [Int])
        y <- get
        return $ x1 == x2 && y == max 0 x

concat :: Property
concat =
    property $ \x y ->
        runIdentity $ do
            x1 <-
                S.toList $
                S.unfold
                    (UF.concat
                         (UF.enumerateFromToIntegral (x :: Int))
                         (UF.enumerateFromToIntegral (y :: Int)))
                    0
            let x2 = P.concat $ P.map (\k -> [k..y]) [0 .. x]
            return $ x1 == x2

outerProduct :: Property
outerProduct =
    property $ \x y ->
        runIdentity $ do
            x1 <-
                S.toList $
                S.unfold
                    (UF.outerProduct
                         (UF.enumerateFromToIntegral (x :: Int))
                         (UF.enumerateFromToIntegral (y :: Int)))
                    (0, 0)
            let x2 = [(a, b) | a <- [0 .. x], b <- [0 .. y]]
            return $ x1 == x2

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
