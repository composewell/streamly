{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Hspec
import Data.Functor.Identity
import qualified GHC.Exts as GHC
import Prelude hiding (String)

import Streamly
import qualified Streamly.Prelude as S

type List = SerialT Identity
type String = List Char

------------------------------------------------------------------------------
-- Patterns
------------------------------------------------------------------------------

-- | A pattern that matches an empty pure stream.
pattern Nil :: SerialT Identity a
pattern Nil <- (runIdentity . S.null -> True) where
    Nil = S.nil

-- | A pattern that deconstructs a pure stream into its head and tail.
pattern Cons :: a -> SerialT Identity a -> SerialT Identity a
pattern Cons x xs <- (runIdentity . S.uncons -> Just (x, xs)) where
    Cons x xs = S.cons x xs

main :: IO ()
main = hspec $ do
    -- Need type annotation to resolve the monad ambiguity
    -- Use toList or runIdentity . S.toList to convert to Haskell lists

    it "overloaded lists" $ do
        ([1..3] :: Main.List Int) `shouldBe` S.fromList [1..3]
        GHC.toList ([1..3] :: Main.List Int) `shouldBe` [1..3]

    it "overloaded strings" $ do
        ("hello" :: Main.String) `shouldBe` S.fromList "hello"

    it "pattern match constructs a list" $ do
        ('x' `Cons` Nil) `shouldBe` ('x' `S.cons` S.nil :: Main.String)
        (1 `Cons` [2] :: Main.List Int) `shouldBe` S.fromList [1,2]

    it "pattern match on empty string" $
        case "" :: Main.String of
            Nil -> return ()
            _ -> expectationFailure "not reached"

    it "pattern matches on singleton string" $
        case "a" :: Main.String of
            Cons x Nil -> x `shouldBe` 'a'
            _ -> expectationFailure "not reached"

    it "pattern matches on non-empty string" $
        case "hello" <> "world" :: Main.String of
            Cons x1 (Cons x2 xs) -> do
                x1 `shouldBe` 'h'
                x2 `shouldBe` 'e'
                xs `shouldBe` "lloworld"
            _ -> expectationFailure "not reached"

    it "pattern match on non-empty list" $
        case S.map (+1) [1..10] :: Main.List Int of
            Cons x xs -> do
                x `shouldBe` 2
                xs `shouldBe` [3..11]
            _ -> expectationFailure "not reached"

    it "Shows as string" $ do
        show (S.fromList [1..3] :: Main.List Int) `shouldBe` "fromList [1,2,3]"

    it "Reads from a string" $ do
        (read "fromList [1,2,3]" :: Main.List Int) `shouldBe` S.fromList [1..3]

    it "Eq instance" $ do
        (S.fromList [1,2,3] :: Main.List Int) == S.fromList [1,2,3] `shouldBe` True

    it "Ord instance" $ do
        (S.fromList [1,2,3] :: Main.List Int) > S.fromList [1,2,1] `shouldBe` True
