{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Hspec
import qualified GHC.Exts as GHC

import Data.Functor.Identity
import Streamly
import Streamly.List (pattern Cons, pattern Nil)
import qualified Streamly.Prelude as S
import qualified Streamly.List as S

main :: IO ()
main = hspec $ do
    describe "OverloadedLists for 'SerialT Identity' type" $ do
        it "overloaded lists" $ do
            ([1..3] :: SerialT Identity Int) `shouldBe` S.fromList [1..3]
            GHC.toList ([1..3] :: SerialT Identity Int) `shouldBe` [1..3]

        it "Show instance" $ do
            show (S.fromList [1..3] :: SerialT Identity Int)
                `shouldBe` "fromList [1,2,3]"
        it "Read instance" $ do
            (read "fromList [1,2,3]" :: SerialT Identity Int) `shouldBe` [1..3]

        it "Eq instance" $ do
            ([1,2,3] :: SerialT Identity Int) == [1,2,3] `shouldBe` True

        it "Ord instance" $ do
            ([1,2,3] :: SerialT Identity Int) > [1,2,1] `shouldBe` True

        it "Monad comprehension" $ do
            [(x,y) | x <- [1..2], y <- [1..2]] `shouldBe`
                ([(1,1), (1,2), (2,1), (2,2)] :: SerialT Identity (Int, Int))

    describe "OverloadedLists for List type" $ do
        it "overloaded lists" $ do
            [1..3 :: Int] `shouldBe` S.fromSerial (S.fromList [1..3])
            GHC.toList ([1..3] :: S.List Int) `shouldBe` [1..3]

        it "pattern constructor constructs a list" $ do
            ('x' `Cons` Nil) `shouldBe` ['x']
            (1 `Cons` [2 :: Int]) `shouldBe` [1,2]

        it "pattern match on non-empty list" $
            case [1..10 :: Int] of
                Cons x xs -> do
                    x `shouldBe` 1
                    xs `shouldBe` [2..10]
                _ -> expectationFailure "not reached"

        it "Show instance" $ do
            show ([1..3] :: S.List Int) `shouldBe` "fromList [1,2,3]"

        it "Read instance" $ do
            (read "fromList [1,2,3]" :: S.List Int) `shouldBe` [1..3]

        it "Eq instance" $ do
            ([1,2,3] :: S.List Int) == [1,2,3] `shouldBe` True

        it "Ord instance" $ do
            ([1,2,3] :: S.List Int) > [1,2,1] `shouldBe` True

        it "Monad comprehension" $ do
            [(x,y) | x <- [1..2], y <- [1..2]] `shouldBe`
                ([(1,1), (1,2), (2,1), (2,2)] :: S.List (Int, Int))

    describe "OverloadedStrings for 'SerialT Identity' type" $ do
        it "overloaded strings" $ do
            ("hello" :: SerialT Identity Char) `shouldBe` S.fromList "hello"

    describe "OverloadedStrings for List type" $ do
        it "overloaded strings" $ do
            "hello" `shouldBe` S.fromSerial (S.fromList "hello")

        it "pattern match on empty string" $
            case "" of
                Nil -> return ()
                _ -> expectationFailure "not reached"

        it "pattern matches on singleton string" $
            case "a" of
                Cons x Nil -> x `shouldBe` 'a'
                _ -> expectationFailure "not reached"

        it "pattern matches on non-empty string" $
            case "hello" <> "world" of
                Cons x1 (Cons x2 xs) -> do
                    x1 `shouldBe` 'h'
                    x2 `shouldBe` 'e'
                    xs `shouldBe` "lloworld"
                _ -> expectationFailure "not reached"
