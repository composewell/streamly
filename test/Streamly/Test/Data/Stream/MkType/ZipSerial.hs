{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Streamly.Test.Data.Stream.ZipSerial
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.MkType.ZipSerial (main) where

import Control.Applicative (ZipList(..))
import Test.Hspec.QuickCheck
import Test.Hspec as H
import Test.QuickCheck.Monadic (monadicIO, run)

import Streamly.Data.Stream.MkType
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.StreamK as StreamK

import Streamly.Test.Common

zipApply :: Monad m => Stream.Stream m (a -> b) -> Stream.Stream m a -> Stream.Stream m b
zipApply = Stream.zipWith ($)

$(mkZipType "Zip" "zipApply" False)

instance Monad m => Semigroup (Zip m a) where
    a <> b = mkZip $ StreamK.toStream
        $ StreamK.append
            (StreamK.fromStream (unZip a))
            (StreamK.fromStream (unZip b))

instance Monad m => Monoid (Zip m a) where
    mempty = mkZip Stream.nil

fromZipList :: [a] -> Zip IO a
fromZipList = mkZip . Stream.fromList

toZipList :: Zip IO a -> IO [a]
toZipList = Stream.toList . unZip

moduleName :: String
moduleName = "Data.Stream.Zip"

main :: IO ()
main = hspec
  $ H.parallel
  $ describe moduleName $ do

    describe "Functor" $ do
        prop "fmap id" $ \xs ->
            monadicIO $ do
                result <- run $ toZipList $ fmap id (fromZipList (xs :: [Int]))
                listEquals (==) result xs
        prop "fmap (+1)" $ \xs ->
            monadicIO $ do
                result <- run $ toZipList $ fmap (+ 1) (fromZipList (xs :: [Int]))
                listEquals (==) result (map (+ 1) xs)
        prop "fmap over semigroup composition" $ \xs ys ->
            monadicIO $ do
                result <- run $ toZipList $
                    fmap (+ 1) (fromZipList (xs :: [Int]) <> fromZipList ys)
                listEquals (==) result (map (+ 1) (xs ++ ys))

    describe "Semigroup" $ do
        prop "append xs ys = xs ++ ys" $ \xs ys ->
            monadicIO $ do
                result <- run $ toZipList $ fromZipList (xs :: [Int]) <> fromZipList ys
                listEquals (==) result (xs ++ ys)
        prop "associativity" $ \xs ys zs ->
            monadicIO $ do
                r1 <- run $ toZipList $
                    (fromZipList (xs :: [Int]) <> fromZipList ys) <> fromZipList zs
                r2 <- run $ toZipList $
                    fromZipList xs <> (fromZipList ys <> fromZipList zs)
                listEquals (==) r1 r2

    describe "Monoid" $ do
        it "mempty is empty" $
            toZipList (mempty :: Zip IO Int) `shouldReturn` []
        prop "left identity: mempty <> xs = xs" $ \xs ->
            monadicIO $ do
                result <- run $ toZipList $ mempty <> fromZipList (xs :: [Int])
                listEquals (==) result xs
        prop "right identity: xs <> mempty = xs" $ \xs ->
            monadicIO $ do
                result <- run $ toZipList $ fromZipList (xs :: [Int]) <> mempty
                listEquals (==) result xs

    describe "Applicative (Zip)" $ do
        it "pure generates repeated elements" $
            Stream.toList (Stream.take 5 (unZip (pure (42 :: Int))))
                `shouldReturn` [42, 42, 42, 42, 42]
        prop "(<*>) zips function and value streams" $ \xs ys ->
            monadicIO $ do
                result <- run $ toZipList $
                    (,) <$> fromZipList (xs :: [Int]) <*> fromZipList (ys :: [Int])
                let expected = getZipList $ (,) <$> ZipList xs <*> ZipList ys
                listEquals (==) result expected
        prop "fmap f xs = pure f <*> xs" $ \xs ->
            monadicIO $ do
                r1 <- run $ toZipList $ fmap (+ 1) (fromZipList (xs :: [Int]))
                r2 <- run $ toZipList $ pure (+ 1) <*> fromZipList xs
                listEquals (==) r1 r2
