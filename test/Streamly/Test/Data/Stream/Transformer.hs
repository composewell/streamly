-- |
-- Module      : Streamly.Test.Data.Stream.Transformer
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Transformer (main) where

import Control.Monad.Trans.Identity (IdentityT(..), runIdentityT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT, runStateT)
import Streamly.Internal.Data.Stream (Stream)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State.Strict as State
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Hspec as H

-------------------------------------------------------------------------------
-- foldlT
-------------------------------------------------------------------------------

testFoldlT :: Expectation
testFoldlT = do
    result <- runIdentityT $
                  Stream.foldlT
                      (\acc x -> (+ x) <$> acc)
                      (return 0 :: IdentityT IO Int)
                      (Stream.fromList [1, 2, 3 :: Int])
    result `shouldBe` 6

testFoldlTEmpty :: Expectation
testFoldlTEmpty = do
    result <- runIdentityT $
                  Stream.foldlT
                      (\acc x -> (+ x) <$> acc)
                      (return 0 :: IdentityT IO Int)
                      (Stream.fromList ([] :: [Int]))
    result `shouldBe` 0

-------------------------------------------------------------------------------
-- foldrT
-------------------------------------------------------------------------------

checkFoldrTLaziness :: IO ()
checkFoldrTLaziness =
    runIdentityT (Stream.foldrT (\x xs -> if odd x then return True else xs)
                        (return False)
                        (Stream.fromList (2:4:5:undefined) :: Stream IO Int))
        `shouldReturn` True

-------------------------------------------------------------------------------
-- liftInner
-------------------------------------------------------------------------------

testLiftInner :: Expectation
testLiftInner = do
    let s = Stream.fromList [1, 2, 3 :: Int]
        sLifted = Stream.liftInner s :: Stream (ReaderT Int IO) Int
    xs <- Reader.runReaderT (Stream.toList sLifted) 0
    xs `shouldBe` [1, 2, 3]

testLiftInnerWithEffect :: Expectation
testLiftInnerWithEffect = do
    let s = Stream.fromList [1, 2, 3 :: Int]
        -- lift then add the reader env to each element
        sLifted = Stream.mapM (\x -> (x +) <$> Reader.ask)
                      (Stream.liftInner s)
                  :: Stream (ReaderT Int IO) Int
    xs <- Reader.runReaderT (Stream.toList sLifted) 10
    xs `shouldBe` [11, 12, 13]

-------------------------------------------------------------------------------
-- runReaderT
-------------------------------------------------------------------------------

testRunReaderT :: Expectation
testRunReaderT = do
    let s = Stream.mapM (\x -> (x +) <$> Reader.ask)
                (Stream.fromList [1, 2, 3 :: Int])
              :: Stream (ReaderT Int IO) Int
    xs <- Stream.toList $ Stream.runReaderT (return 10) s
    xs `shouldBe` [11, 12, 13]

testRunReaderTZeroEnv :: Expectation
testRunReaderTZeroEnv = do
    let s = Stream.mapM (\x -> (x +) <$> Reader.ask)
                (Stream.fromList [1, 2, 3 :: Int])
              :: Stream (ReaderT Int IO) Int
    xs <- Stream.toList $ Stream.runReaderT (return 0) s
    xs `shouldBe` [1, 2, 3]

-------------------------------------------------------------------------------
-- usingReaderT
-------------------------------------------------------------------------------

testUsingReaderT :: Expectation
testUsingReaderT = do
    let addEnv = Stream.mapM (\x -> (x +) <$> Reader.ask)
    xs <- Stream.toList $
              Stream.usingReaderT (return 10) addEnv
                  (Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [11, 12, 13]

-------------------------------------------------------------------------------
-- withReaderT
-------------------------------------------------------------------------------

testWithReaderT :: Expectation
testWithReaderT = do
    let s = Stream.mapM (\x -> (x +) <$> Reader.ask)
                (Stream.fromList [1, 2, 3 :: Int])
              :: Stream (ReaderT Int IO) Int
        -- double the environment before using it
        s2 = Stream.withReaderT (* 2) s
    xs <- Stream.toList $ Stream.runReaderT (return 5) s2
    xs `shouldBe` [11, 12, 13]

-------------------------------------------------------------------------------
-- localReaderT
-------------------------------------------------------------------------------

testLocalReaderT :: Expectation
testLocalReaderT = do
    let s = Stream.mapM (\x -> (x +) <$> Reader.ask)
                (Stream.fromList [1, 2, 3 :: Int])
              :: Stream (ReaderT Int IO) Int
        s2 = Stream.localReaderT (* 2) s
    xs <- Stream.toList $ Stream.runReaderT (return 5) s2
    xs `shouldBe` [11, 12, 13]

-------------------------------------------------------------------------------
-- evalStateT
-------------------------------------------------------------------------------

testEvalStateT :: Expectation
testEvalStateT = do
    let s = Stream.mapM (\x -> State.modify (+ x) >> return x)
                (Stream.fromList [1, 2, 3 :: Int])
              :: Stream (StateT Int IO) Int
    xs <- Stream.toList $ Stream.evalStateT (return 0) s
    xs `shouldBe` [1, 2, 3]

testEvalStateTAccum :: Expectation
testEvalStateTAccum = do
    -- each element is replaced by running sum
    let s = Stream.mapM (\x -> State.modify (+ x) >> State.get)
                (Stream.fromList [1, 2, 3 :: Int])
              :: Stream (StateT Int IO) Int
    xs <- Stream.toList $ Stream.evalStateT (return 0) s
    xs `shouldBe` [1, 3, 6]

-------------------------------------------------------------------------------
-- runStateT
-------------------------------------------------------------------------------

testRunStateT :: Expectation
testRunStateT = do
    let s = Stream.mapM (\x -> State.modify (+ x) >> return x)
                (Stream.fromList [1, 2, 3 :: Int])
              :: Stream (StateT Int IO) Int
    pairs <- Stream.toList $ Stream.runStateT (return 0) s
    pairs `shouldBe` [(1, 1), (3, 2), (6, 3)]

testRunStateTInitial :: Expectation
testRunStateTInitial = do
    -- starting with initial state 10
    let s = Stream.mapM (\x -> State.modify (+ x) >> return x)
                (Stream.fromList [1, 2 :: Int])
              :: Stream (StateT Int IO) Int
    pairs <- Stream.toList $ Stream.runStateT (return 10) s
    pairs `shouldBe` [(11, 1), (13, 2)]

-------------------------------------------------------------------------------
-- usingStateT
-------------------------------------------------------------------------------

testUsingStateT :: Expectation
testUsingStateT = do
    let addRunning = Stream.mapM (\x -> State.modify (+ x) >> State.get)
    xs <- Stream.toList $
              Stream.usingStateT (return 0) addRunning
                  (Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 3, 6]

testUsingStateTIdentity :: Expectation
testUsingStateTIdentity = do
    -- identity transformation, just pass elements through
    xs <- Stream.toList $
              Stream.usingStateT (return (0 :: Int)) id
                  (Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]

-------------------------------------------------------------------------------
-- runStateT (to unwrap inner StateT result type)
-------------------------------------------------------------------------------

testRunStateTOuter :: Expectation
testRunStateTOuter = do
    -- use base runStateT to get final state from a StateT computation
    let s = Stream.fromList [1, 2, 3 :: Int] :: Stream (StateT Int IO) Int
    (xs, finalState) <- runStateT (Stream.toList s) 99
    xs `shouldBe` [1, 2, 3]
    finalState `shouldBe` 99

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Stream.Transformer"

main :: IO ()
main = hspec
  $ H.parallel
  $ describe moduleName $ do

    describe "foldlT" $ do
        it "sums stream elements" testFoldlT
        it "empty stream" testFoldlTEmpty

    it "foldrT is lazy enough" checkFoldrTLaziness

    describe "liftInner" $ do
        it "lifts stream to transformer" testLiftInner
        it "lifted stream can use transformer effects" testLiftInnerWithEffect

    describe "runReaderT" $ do
        it "adds env to each element" testRunReaderT
        it "zero env leaves elements unchanged" testRunReaderTZeroEnv

    describe "usingReaderT" $ do
        it "runs transformation with env" testUsingReaderT

    describe "withReaderT" $ do
        it "modifies env before use" testWithReaderT

    describe "localReaderT" $ do
        it "modifies env before use" testLocalReaderT

    describe "evalStateT" $ do
        it "runs state, discards final state" testEvalStateT
        it "accumulates running sum" testEvalStateTAccum

    describe "runStateT" $ do
        it "emits (state, value) pairs" testRunStateT
        it "starts from given initial state" testRunStateTInitial
        it "stream in StateT with outer runStateT" testRunStateTOuter

    describe "usingStateT" $ do
        it "runs stateful transformation" testUsingStateT
        it "identity transformation" testUsingStateTIdentity
