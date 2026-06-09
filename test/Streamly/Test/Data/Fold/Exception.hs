{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Streamly.Test.Data.Fold.Exception
-- Copyright   : (c) 2025 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Fold.Exception (main) where

import Control.Exception (SomeException, try, evaluate)
import Control.Monad (void)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Hspec

beforeFold :: Expectation
beforeFold = do
    ref <- newIORef (0 :: Int)
    let action = writeIORef ref 1
        fld = Fold.before action Fold.sum
    result <- Stream.fold fld (Stream.fromList [1,2,3 :: Int])
    result `shouldBe` 6
    val <- readIORef ref
    val `shouldBe` 1

finallyIOFold :: Expectation
finallyIOFold = do
    ref <- newIORef (0 :: Int)
    let action = writeIORef ref 1 >> return (0 :: Int)
        fld = Fold.finallyIO action Fold.sum
    result <- Stream.fold fld (Stream.fromList [1,2,3 :: Int])
    result `shouldBe` 6
    val <- readIORef ref
    val `shouldBe` 1

bracketIOFold :: Expectation
bracketIOFold = do
    ref <- newIORef (0 :: Int)
    let acquire = void (writeIORef ref 1)
        release _ = writeIORef ref 2
        use _ = Fold.sum :: Fold.Fold IO Int Int
        fld = Fold.bracketIO acquire release use :: Fold.Fold IO Int Int
    result <- Stream.fold fld (Stream.fromList [1,2,3 :: Int])
    result `shouldBe` 6
    val <- readIORef ref
    val `shouldBe` 2

onExceptionFold :: Expectation
onExceptionFold = do
    ref <- newIORef (0 :: Int)
    let action = writeIORef ref 1
        throwFold = Fold.Fold
            (\_ _ -> error "test error")
            (return (Fold.Partial ()))
            (\_ -> return (0 :: Int))
            (\_ -> return 0)
        fld = Fold.onException action throwFold
    _ <- try @SomeException $ evaluate =<< Stream.fold fld (Stream.fromList [1 :: Int])
    val <- readIORef ref
    val `shouldBe` 1

moduleName :: String
moduleName = "Data.Fold.Exception"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        it "before" beforeFold
        it "finallyIO" finallyIOFold
        it "bracketIO" bracketIOFold
        it "onException" onExceptionFold
