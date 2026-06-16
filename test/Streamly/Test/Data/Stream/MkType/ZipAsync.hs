{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Streamly.Test.Data.Stream.ZipAsync
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.MkType.ZipAsync (main) where

import Control.Applicative (ZipList(..))
import Test.Hspec.QuickCheck
import Test.Hspec as H
import Test.QuickCheck.Monadic (monadicIO, run)

import Streamly.Data.Stream.MkType
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.StreamK as StreamK

import Streamly.Test.Common

-- XXX add merge tests like zip tests
-- for mergeBy, we can split a list randomly into two lists and
-- then merge them, it should result in original list
-- describe "Merge operations" $ do

parZipApply :: Stream.MonadAsync m => Stream.Stream m (a -> b) -> Stream.Stream m a -> Stream.Stream m b
parZipApply = Stream.parZipWith id ($)

$(mkZipType "ParZip" "parZipApply" True)

-- NOTE: it is possible to define concurrent merging semigroup instance, though
-- it won't be associative. Also, binary concurrent merging operation is not as
-- scalable as n-ary merge using parMap. For better performance we could use a
-- State monad with a shared Channel for concurrent evaluation.
instance Monad m => Semigroup (ParZip m a) where
    a <> b = mkParZip $ StreamK.toStream
        $ StreamK.append
            (StreamK.fromStream (unParZip a))
            (StreamK.fromStream (unParZip b))

instance Monad m => Monoid (ParZip m a) where
    mempty = mkParZip Stream.nil

fromParZipList :: [a] -> ParZip IO a
fromParZipList = mkParZip . Stream.fromList

toParZipList :: ParZip IO a -> IO [a]
toParZipList = Stream.toList . unParZip

moduleName :: String
moduleName = "Data.Stream.Prelude.ParZip"

main :: IO ()
main = hspec
  $ H.parallel
  $ describe moduleName $ do

    describe "Functor" $ do
        prop "fmap id" $ \xs ->
            monadicIO $ do
                result <- run $ toParZipList $ fmap id (fromParZipList (xs :: [Int]))
                listEquals (==) result xs
        prop "fmap (+1)" $ \xs ->
            monadicIO $ do
                result <- run $ toParZipList $ fmap (+ 1) (fromParZipList (xs :: [Int]))
                listEquals (==) result (map (+ 1) xs)
        prop "fmap over semigroup composition" $ \xs ys ->
            monadicIO $ do
                result <- run $ toParZipList $
                    fmap (+ 1) (fromParZipList (xs :: [Int]) <> fromParZipList ys)
                listEquals (==) result (map (+ 1) (xs ++ ys))

    describe "Semigroup" $ do
        prop "append xs ys = xs ++ ys" $ \xs ys ->
            monadicIO $ do
                result <- run $ toParZipList $ fromParZipList (xs :: [Int]) <> fromParZipList ys
                listEquals (==) result (xs ++ ys)
        prop "associativity" $ \xs ys zs ->
            monadicIO $ do
                r1 <- run $ toParZipList $
                    (fromParZipList (xs :: [Int]) <> fromParZipList ys) <> fromParZipList zs
                r2 <- run $ toParZipList $
                    fromParZipList xs <> (fromParZipList ys <> fromParZipList zs)
                listEquals (==) r1 r2

    describe "Monoid" $ do
        it "mempty is empty" $
            toParZipList (mempty :: ParZip IO Int) `shouldReturn` []
        prop "left identity: mempty <> xs = xs" $ \xs ->
            monadicIO $ do
                result <- run $ toParZipList $ mempty <> fromParZipList (xs :: [Int])
                listEquals (==) result xs
        prop "right identity: xs <> mempty = xs" $ \xs ->
            monadicIO $ do
                result <- run $ toParZipList $ fromParZipList (xs :: [Int]) <> mempty
                listEquals (==) result xs

    describe "Applicative (Zip)" $ do
        it "pure generates repeated elements" $
            Stream.toList (Stream.take 5 (unParZip (pure (42 :: Int))))
                `shouldReturn` [42, 42, 42, 42, 42]
        prop "(<*>) zips function and value streams" $ \xs ys ->
            monadicIO $ do
                result <- run $ toParZipList $
                    (,) <$> fromParZipList (xs :: [Int]) <*> fromParZipList (ys :: [Int])
                let expected = getZipList $ (,) <$> ZipList xs <*> ZipList ys
                listEquals (==) result expected
        prop "fmap f xs = pure f <*> xs" $ \xs ->
            monadicIO $ do
                r1 <- run $ toParZipList $ fmap (+ 1) (fromParZipList (xs :: [Int]))
                r2 <- run $ toParZipList $ pure (+ 1) <*> fromParZipList xs
                listEquals (==) r1 r2
