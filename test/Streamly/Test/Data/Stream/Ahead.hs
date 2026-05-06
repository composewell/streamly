{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Test.Data.Stream.Ahead
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Ahead (main) where

import Control.Exception (Exception, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort)
import Test.Hspec.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run)
import Test.Hspec as H

import Streamly.Data.Stream.MkType
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.StreamK as StreamK
import Streamly.Test.Common

bind :: Stream.MonadAsync m => Stream.Stream m a -> (a -> Stream.Stream m b) -> Stream.Stream m b
bind = flip (Stream.parConcatMap (Stream.ordered True))
$(mkCrossType "OrderedParallel" "bind" True)

instance Monad m => Semigroup (OrderedParallel m a) where
    a <> b = mkOrderedParallel $ StreamK.toStream
        $ StreamK.append
            (StreamK.fromStream (unOrderedParallel a))
            (StreamK.fromStream (unOrderedParallel b))

instance Monad m => Monoid (OrderedParallel m a) where
    mempty = mkOrderedParallel Stream.nil

fromOPList :: [a] -> OrderedParallel IO a
fromOPList = mkOrderedParallel . Stream.fromList

toOPList :: OrderedParallel IO a -> IO [a]
toOPList = Stream.toList . unOrderedParallel

newtype ExampleException = ExampleException String deriving (Eq, Show, Ord)

instance Exception ExampleException

moduleName :: String
moduleName = "Data.Stream.Prelude.OrderedParallel"

main :: IO ()
main = hspec
  $ H.parallel
#ifdef COVERAGE_BUILD
  $ modifyMaxSuccess (const 10)
#endif
  $ describe moduleName $ do
    describe "Functor" $ do
        prop "fmap id" $ \xs ->
            monadicIO $ do
                result <- run $ toOPList $ fmap id (fromOPList (xs :: [Int]))
                listEquals (==) result xs
        prop "fmap (+1)" $ \xs ->
            monadicIO $ do
                result <- run $ toOPList $ fmap (+ 1) (fromOPList (xs :: [Int]))
                listEquals (==) result (map (+ 1) xs)
        prop "fmap over semigroup composition" $ \xs ys ->
            monadicIO $ do
                result <- run $ toOPList $
                    fmap (+ 1) (fromOPList (xs :: [Int]) <> fromOPList ys)
                listEquals (==) result (map (+ 1) (xs ++ ys))

    describe "Semigroup" $ do
        prop "append xs ys = xs ++ ys" $ \xs ys ->
            monadicIO $ do
                result <- run $ toOPList $ fromOPList (xs :: [Int]) <> fromOPList ys
                listEquals (==) result (xs ++ ys)
        prop "associativity" $ \xs ys zs ->
            monadicIO $ do
                r1 <- run $ toOPList $
                    (fromOPList (xs :: [Int]) <> fromOPList ys) <> fromOPList zs
                r2 <- run $ toOPList $
                    fromOPList xs <> (fromOPList ys <> fromOPList zs)
                listEquals (==) r1 r2

    describe "Monoid" $ do
        it "mempty is empty" $
            toOPList (mempty :: OrderedParallel IO Int) `shouldReturn` []
        prop "left identity: mempty <> xs = xs" $ \xs ->
            monadicIO $ do
                result <- run $ toOPList $ mempty <> fromOPList (xs :: [Int])
                listEquals (==) result xs
        prop "right identity: xs <> mempty = xs" $ \xs ->
            monadicIO $ do
                result <- run $ toOPList $ fromOPList (xs :: [Int]) <> mempty
                listEquals (==) result xs

    describe "Applicative" $ do
        it "pure gives singleton" $
            toOPList (pure (42 :: Int)) `shouldReturn` [42]
        prop "(<*>) applies cross-product in order" $ \xs ys ->
            monadicIO $ do
                result <- run $ toOPList $
                    (,) <$> fromOPList (xs :: [Int]) <*> fromOPList (ys :: [Int])
                let expected = (,) <$> xs <*> ys
                listEquals (==) result expected
        prop "fmap f xs = pure f <*> xs" $ \xs ->
            monadicIO $ do
                r1 <- run $ toOPList $ fmap (+ 1) (fromOPList (xs :: [Int]))
                r2 <- run $ toOPList $ pure (+ 1) <*> fromOPList xs
                listEquals (==) r1 r2

    describe "Monad" $ do
        prop "monad then" $ \xs ys ->
            monadicIO $ do
                result <- run $ toOPList $
                    fromOPList (xs :: [Int]) >> fromOPList (ys :: [Int])
                listEquals (==) result (xs >> ys)
        prop "monad bind" $ \xs ys ->
            monadicIO $ do
                result <- run $ toOPList $
                    fromOPList (xs :: [Int]) >>= \x -> fmap (+ x) (fromOPList (ys :: [Int]))
                listEquals (==) result (xs >>= \x -> fmap (+ x) ys)

    describe "Recursive appends" $ do
        it "Tail recursive loop" $ do
            let loopTail :: Int -> OrderedParallel IO Int
                loopTail x = do
                    _ <- mkOrderedParallel $ Stream.fromEffect $ putStrLn "LoopTail..."
                    mkOrderedParallel (Stream.fromPure x)
                        <> if x < 3 then loopTail (x + 1) else mempty
            toOPList (loopTail 0) `shouldReturn` [0..3]
        it "Head recursive loop" $ do
            let loopHead :: Int -> OrderedParallel IO Int
                loopHead x = do
                    _ <- mkOrderedParallel $ Stream.fromEffect $ putStrLn "LoopHead..."
                    (if x < 3 then loopHead (x + 1) else mempty)
                        <> mkOrderedParallel (Stream.fromPure x)
            result <- toOPList (loopHead 0)
            reverse result `shouldBe` [0..3]

    describe "Semigroup and Monad" $ do
        prop "compose many right fold with bind" $ \list ->
            monadicIO $ do
                let s = foldr (<>) mempty $ map (\x -> fromOPList [x :: Int]) list
                result <- run $ toOPList s
                listEquals (==) result list

        prop "compose many left fold with bind" $ \list ->
            monadicIO $ do
                let s = foldl (<>) mempty $ map (\x -> fromOPList [x :: Int]) list
                result <- run $ toOPList s
                listEquals (==) result list

        it "nest two streams monadic" $ do
            let s1 = fromOPList [1..4 :: Int]
                s2 = fromOPList [5..8 :: Int]
            result <- toOPList $ do
                x <- s1
                y <- s2
                return (x + y)
            result `shouldBe` [6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12]

        it "nest two streams applicative" $ do
            let s1 = fromOPList [1..4 :: Int]
                s2 = fromOPList [5..8 :: Int]
            result <- toOPList $ (+) <$> s1 <*> s2
            result `shouldBe` [6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12]

        it "bind and compose nested hierarchy" $ do
            let c1 = foldr (<>) mempty $ map fromOPList [[1],[2],[3 :: Int]]
                c2 = foldr (<>) mempty $ map fromOPList [[4],[5],[6 :: Int]]
                c3 = foldr (<>) mempty $ map fromOPList [[7],[8],[9 :: Int]]
                b = c1 >>= \x -> c2 >>= \y -> c3 >>= \z -> return (x + y + z)
            result <- toOPList b
            sort result `shouldBe`
                sort ( [12, 18]
                    <> replicate 3 13
                    <> replicate 3 17
                    <> replicate 6 14
                    <> replicate 6 16
                    <> replicate 7 15 :: [Int])

    describe "Exceptions" $ do
        it "before runs action before stream" $ do
            ref <- newIORef (0 :: Int)
            let s = mkOrderedParallel $
                        Stream.before (writeIORef ref 1) $
                        Stream.fromList [2, 3, 4 :: Int]
            _ <- toOPList s
            readIORef ref `shouldReturn` 1

        it "afterIO runs action after stream" $ do
            ref <- newIORef (0 :: Int)
            let s = mkOrderedParallel $
                        Stream.afterIO (writeIORef ref 1) $
                        Stream.fromList [2, 3, 4 :: Int]
            _ <- toOPList s
            readIORef ref `shouldReturn` 1

        it "bracketIO acquires and releases resource" $ do
            ref <- newIORef (0 :: Int)
            let s = mkOrderedParallel $
                        Stream.bracketIO
                            (writeIORef ref 1 >> return ref)
                            (\r -> writeIORef r 2)
                            (\_ -> Stream.fromList [10, 20, 30 :: Int])
            _ <- toOPList s
            readIORef ref `shouldReturn` 2

        it "onException runs on exception" $ do
            ref <- newIORef (0 :: Int)
            res <- try $ toOPList $ mkOrderedParallel $
                Stream.onException (writeIORef ref 1) $
                Stream.fromEffect (throwM (ExampleException "E") :: IO Int)
            res `shouldBe` (Left (ExampleException "E") :: Either ExampleException [Int])
            readIORef ref `shouldReturn` 1

        it "finallyIO runs after stream ends" $ do
            ref <- newIORef (0 :: Int)
            let s = mkOrderedParallel $
                        Stream.finallyIO (writeIORef ref 1) $
                        Stream.fromList [2, 3, 4 :: Int]
            _ <- toOPList s
            readIORef ref `shouldReturn` 1

        it "finallyIO runs on exception" $ do
            ref <- newIORef (0 :: Int)
            res <- try $ toOPList $ mkOrderedParallel $
                Stream.finallyIO (writeIORef ref 1) $
                Stream.fromEffect (throwM (ExampleException "E") :: IO Int)
            res `shouldBe` (Left (ExampleException "E") :: Either ExampleException [Int])
            readIORef ref `shouldReturn` 1

        it "handle catches exception and continues" $ do
            let s :: OrderedParallel IO Int
                s = mkOrderedParallel $
                        Stream.handle
                            (\(ExampleException i) ->
                                return $ Stream.fromList [read i :: Int, 99])
                            (Stream.fromList [1, 2]
                                `Stream.append`
                                Stream.fromEffect
                                    (throwM (ExampleException "0")))
            result <- toOPList s
            result `shouldBe` [1, 2, 0, 99]

    describe "MonadThrow" $ do
        it "throwM <> mempty produces exception" $
            try (toOPList (throwM (ExampleException "E") <> mempty
                            :: OrderedParallel IO Int))
                `shouldReturn`
                    (Left (ExampleException "E") :: Either ExampleException [Int])

        it "mempty <> throwM produces exception" $
            try (toOPList (mempty <> throwM (ExampleException "E")
                            :: OrderedParallel IO Int))
                `shouldReturn`
                    (Left (ExampleException "E") :: Either ExampleException [Int])

        it "one level nested sum with throwM" $ do
            let nested = fromOPList [1..10 :: Int]
                            <> throwM (ExampleException "E")
                            <> fromOPList [1..10]
            try (toOPList (mempty <> nested))
                `shouldReturn`
                    (Left (ExampleException "E") :: Either ExampleException [Int])

        it "one level nested product with throwM" $ do
            let s1 = fromOPList [1..4 :: Int]
                s2 = fromOPList [5..8 :: Int]
            try (toOPList $ do
                x <- s1
                y <- s2
                if x + y > 10
                then throwM (ExampleException "E")
                else return (x + y))
                `shouldReturn`
                    (Left (ExampleException "E") :: Either ExampleException [Int])
