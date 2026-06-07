-- |
-- Module      : Streamly.Test.Data.Stream.Lift
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Lift (main) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify, put, runStateT)
import Data.Functor.Identity (Identity(..))

import Streamly.Internal.Data.Stream (Stream)
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Hspec

-------------------------------------------------------------------------------
-- morphInner
-------------------------------------------------------------------------------

testMorphInner :: Expectation
testMorphInner = do
    let s = Stream.fromList [1, 2, 3 :: Int] :: Stream Identity Int
        s' = Stream.morphInner (return . runIdentity) s :: Stream IO Int
    Stream.toList s' `shouldReturn` [1, 2, 3]

testMorphInnerEmpty :: Expectation
testMorphInnerEmpty = do
    let s = Stream.fromList ([] :: [Int]) :: Stream Identity Int
        s' = Stream.morphInner (return . runIdentity) s :: Stream IO Int
    Stream.toList s' `shouldReturn` []

-------------------------------------------------------------------------------
-- generalizeInner
-------------------------------------------------------------------------------

testGeneralizeInner :: Expectation
testGeneralizeInner = do
    let s = Stream.fromList [1, 2, 3 :: Int] :: Stream Identity Int
    Stream.toList (Stream.generalizeInner s :: Stream IO Int)
        `shouldReturn` [1, 2, 3]

testGeneralizeInnerEmpty :: Expectation
testGeneralizeInnerEmpty = do
    let s = Stream.fromList ([] :: [Int]) :: Stream Identity Int
    Stream.toList (Stream.generalizeInner s :: Stream IO Int)
        `shouldReturn` []

-------------------------------------------------------------------------------
-- liftInnerWith
-------------------------------------------------------------------------------

testLiftInnerWith :: Expectation
testLiftInnerWith = do
    let s = Stream.fromList [1, 2, 3 :: Int]
        sLifted = Stream.liftInnerWith lift s :: Stream (StateT Int IO) Int
        sWithState = Stream.mapM (\x -> put x >> return x) sLifted
    (xs, finalState) <- runStateT (Stream.toList sWithState) 0
    xs `shouldBe` [1, 2, 3]
    finalState `shouldBe` 3

testLiftInnerWithAccum :: Expectation
testLiftInnerWithAccum = do
    let s = Stream.fromList [1, 2, 3 :: Int]
        sLifted = Stream.liftInnerWith lift s :: Stream (StateT Int IO) Int
        sWithState = Stream.mapM (\x -> modify (+x) >> get) sLifted
    (xs, finalState) <- runStateT (Stream.toList sWithState) 0
    xs `shouldBe` [1, 3, 6]
    finalState `shouldBe` 6

-------------------------------------------------------------------------------
-- runInnerWith
-------------------------------------------------------------------------------

testRunInnerWith :: Expectation
testRunInnerWith = do
    let s = Stream.mapM (\x -> put x >> return x)
                (Stream.fromList [1, 2, 3 :: Int])
              :: Stream (StateT Int IO) Int
        sIO = Stream.runInnerWith (\m -> fmap fst (runStateT m 0)) s
    Stream.toList sIO `shouldReturn` [1, 2, 3]

testRunInnerWithInitialState :: Expectation
testRunInnerWithInitialState = do
    -- Each step is run independently from the same initial state (100).
    -- State is not threaded across steps; use runInnerWithState for that.
    let s = Stream.mapM (\x -> modify (+x) >> get)
                (Stream.fromList [10, 20, 30 :: Int])
              :: Stream (StateT Int IO) Int
        sIO = Stream.runInnerWith (\m -> fmap fst (runStateT m 100)) s
    Stream.toList sIO `shouldReturn` [110, 120, 130]

testRunInnerWithEval :: Expectation
testRunInnerWithEval = do
    let s = Stream.fromList [1, 2, 3 :: Int] :: Stream (StateT Int IO) Int
        sIO = Stream.runInnerWith (`evalStateT` 0) s
    Stream.toList sIO `shouldReturn` [1, 2, 3]

-------------------------------------------------------------------------------
-- runInnerWithState
-------------------------------------------------------------------------------

testRunInnerWithState :: Expectation
testRunInnerWithState = do
    let s = Stream.mapM (\x -> modify (+x) >> return x)
                (Stream.fromList [1, 2, 3 :: Int])
              :: Stream (StateT Int IO) Int
        sWithState = Stream.runInnerWithState
                         (flip runStateT)
                         (return 0)
                         s
    pairs <- Stream.toList sWithState
    pairs `shouldBe` [(1, 1), (3, 2), (6, 3)]

testRunInnerWithStateNoModify :: Expectation
testRunInnerWithStateNoModify = do
    let s = Stream.fromList [10, 20, 30 :: Int] :: Stream (StateT Int IO) Int
        sWithState = Stream.runInnerWithState
                         (flip runStateT)
                         (return 0)
                         s
    pairs <- Stream.toList sWithState
    map fst pairs `shouldBe` [0, 0, 0]
    map snd pairs `shouldBe` [10, 20, 30]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Stream.Lift"

main :: IO ()
main = hspec $ describe moduleName $ do
    describe "morphInner" $ do
        it "transforms inner monad" testMorphInner
        it "transforms empty stream" testMorphInnerEmpty

    describe "generalizeInner" $ do
        it "generalizes Identity to IO" testGeneralizeInner
        it "generalizes empty stream" testGeneralizeInnerEmpty

    describe "liftInnerWith" $ do
        it "lifts stream into transformer" testLiftInnerWith
        it "accumulates state after lift" testLiftInnerWithAccum

    describe "runInnerWith" $ do
        it "runs transformer back to base monad" testRunInnerWith
        it "uses initial state for each step" testRunInnerWithInitialState
        it "evalStateT discard state" testRunInnerWithEval

    describe "runInnerWithState" $ do
        it "threads state across steps" testRunInnerWithState
        it "state unchanged when no modifications" testRunInnerWithStateNoModify
