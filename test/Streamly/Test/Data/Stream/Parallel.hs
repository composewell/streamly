-- |
-- Module      : Streamly.Test.Data.Stream.Async
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module is almost identical to other concurrent stream test modules. Any
-- changes in one should be made in others as well.

module Streamly.Test.Data.Stream.Parallel (main) where

#ifdef DEVBUILD
import Control.Concurrent (threadDelay)
#endif
import Control.Exception (Exception, try)
import Control.Monad (replicateM)
import Control.Monad.Catch (throwM)
import Data.List (sort)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup ((<>))
#endif
import Data.Word (Word8)
import Streamly.Data.Stream (Stream)
import Test.Hspec.QuickCheck
import Test.QuickCheck (Testable, Property, choose, forAll, withMaxSuccess)
import Test.QuickCheck.Monadic (monadicIO, run)
import Test.Hspec as H

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.Parallel as Async

import Streamly.Test.Common (listEquals)

moduleName :: String
moduleName = "Data.Stream.Parallel"

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX Move these to common module.

sortEq :: Ord a => [a] -> [a] -> Bool
sortEq a b = sort a == sort b

cmp :: Stream IO Int -> ([Int] -> [Int] -> Bool) -> [Int] -> Property
cmp s eq list =
    monadicIO $ do
        stream <- run $ Stream.fold Fold.toList  s
        listEquals eq stream list

prop1 :: Testable prop => String -> prop -> SpecWith ()
prop1 x y = modifyMaxSuccess (const 1) $ prop x y

-- Coverage build takes too long with default number of tests
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

transformCombineFromList ::
       ([Int] -> Stream IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> ([Int] -> [Int])
    -> (Stream IO Int -> Stream IO Int)
    -> [Int]
    -> [Int]
    -> [Int]
    -> Property
transformCombineFromList constr eq listOp op a b c =
    withMaxSuccess maxTestCount $
        monadicIO $ do
            stream <- run (Stream.fold Fold.toList $
                constr a `Async.parallel` op (constr b `Async.parallel` constr c))
            let list = a <> listOp (b <> c)
            listEquals eq stream list

commonOpConfigs :: [(String, Async.Config -> Async.Config)]
commonOpConfigs =
    [ ("default", id)
#ifndef COVERAGE_BUILD
    , ("maxBuffer 0", Async.maxBuffer 0)
#endif
    ]

opConfigs :: [(String, Async.Config -> Async.Config)]
opConfigs = commonOpConfigs
    ++ [
#ifndef COVERAGE_BUILD
              ("maxBuffer 1", Async.maxBuffer 1)
#endif
    ]

makeSpec :: [(String, a)] -> (a -> Spec) -> Spec
makeSpec cfg spec = mapM_ (\(desc, arg) -> describe desc $ spec arg) cfg
asyncSpec :: ((Async.Config -> Async.Config) -> Spec) -> Spec
asyncSpec =
    makeSpec $ opConfigs
#ifndef COVERAGE_BUILD
        <> [("maxBuffer (-1)", Async.maxBuffer (-1))]
#endif

-------------------------------------------------------------------------------
-- Compose with MonadThrow
-------------------------------------------------------------------------------

newtype ExampleException = ExampleException String deriving (Eq, Show, Ord)

instance Exception ExampleException

exceptionPropagation ::
    (Stream IO Int -> Stream IO Int -> Stream IO Int) -> Spec
exceptionPropagation f = do
    it "append throwM, nil" $
        try (tl (throwM (ExampleException "E") `f` Stream.nil))
        `shouldReturn`
            (Left (ExampleException "E") :: Either ExampleException [Int])
    it "append nil, throwM" $
        try (tl (Stream.nil `f` throwM (ExampleException "E")))
        `shouldReturn`
            (Left (ExampleException "E") :: Either ExampleException [Int])
    it "append nested throwM" $ do
        let nested =
                Stream.fromFoldable [1..10]
                    `f` throwM (ExampleException "E")
                    `f` Stream.fromFoldable [1..10]
        try (tl (Stream.nil `f` nested `f` Stream.fromFoldable [1..10]))
            `shouldReturn`
                (Left (ExampleException "E")
                    :: Either ExampleException [Int])
    it "sequence throwM" $
        let stream = Stream.fromList [throwM (ExampleException "E")]
         in try (tl (Stream.nil `f` Async.sequence stream))
            `shouldReturn`
                (Left (ExampleException "E") :: Either ExampleException [Int])

    it "concatMap throwM" $ do
        let s1 = Async.concatList $ fmap Stream.fromPure [1..4]
            s2 = Async.concatList $ fmap Stream.fromPure [5..8]
        try $ tl (
            let bind = flip Async.concatMap
             in bind s1 $ \x ->
                bind s2 $ \y ->
                    if x + y > 10
                    then throwM (ExampleException "E")
                    else return (x + y)
            )
        `shouldReturn`
            (Left (ExampleException "E") :: Either ExampleException [Int])

    where

    tl = Stream.fold Fold.toList

---------------------------------------------------------------------------
-- Time ordering
---------------------------------------------------------------------------

#ifdef DEVBUILD
timeOrdering :: (Stream IO Int -> Stream IO Int -> Stream IO Int) -> Spec
timeOrdering f = do
    it "Parallel ordering left associated" $
        Stream.fold Fold.toList (((event 4 `f` event 3) `f` event 2) `f` event 1)
            `shouldReturn` [1..4]

    it "Parallel ordering right associated" $
        Stream.fold Fold.toList (event 4 `f` (event 3 `f` (event 2 `f` event 1)))
            `shouldReturn` [1..4]

    where event n = Stream.fromEffect (threadDelay (n * 200000)) >> return n
#endif

-------------------------------------------------------------------------------
-- Some ad-hoc tests that failed at times
-------------------------------------------------------------------------------

takeCombined :: Int -> IO ()
takeCombined n = do
    let constr = Stream.fromFoldable
    r <- Stream.fold Fold.toList $
            Stream.take n
                (constr ([] :: [Int]) `Async.parallel` constr ([] :: [Int]))
    r `shouldBe` []

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

constructWithLenM
    :: (Int -> Stream IO Int)
    -> (Int -> IO [Int])
    -> Word8
    -> Property
constructWithLenM mkStream mkList len =
    withMaxSuccess maxTestCount
        $ monadicIO $ do
            stream <-
                run
                    $ Stream.fold Fold.toList
                    $ mkStream (fromIntegral len)
            list <- run $ mkList (fromIntegral len)
            listEquals (==) stream list

sequenceReplicate
    :: (Async.Config -> Async.Config)
    -> Word8
    -> Property
sequenceReplicate cfg = constructWithLenM stream list

    where

    list = flip replicateM (return 1 :: IO Int)
    stream = Async.sequenceWith cfg . flip Stream.replicate (return 1 :: IO Int)

main :: IO ()
main = hspec
  $ H.parallel
#ifdef COVERAGE_BUILD
  $ modifyMaxSuccess (const 10)
#endif
  $ describe moduleName $ do
        let transform = transformCombineFromList Stream.fromList sortEq

        prop "eval" $
            transform
                (fmap (+2))
                (fmap (+1) . Async.eval . fmap (+1))

        asyncSpec $ prop "sequenceWith" . sequenceReplicate
        -- XXX Need to use asyncSpec in all tests
        prop "mapM (+1)" $
            transform (fmap (+1)) (Async.mapM (\x -> return (x + 1)))

        -- Binary append
        prop1 "append [] []"
            $ cmp (Stream.nil `Async.parallel` Stream.nil) sortEq []
        prop1 "append [] [1]"
            $ cmp (Stream.nil `Async.parallel` Stream.fromPure 1) sortEq [1]
        prop1 "append [1] []"
            $ cmp (Stream.fromPure 1 `Async.parallel` Stream.nil) sortEq [1]
        prop1 "append [0] [1]"
            $ let stream =
                    Stream.fromPure 0 `Async.parallel` Stream.fromPure 1
               in cmp stream sortEq [0, 1]

        prop1 "append [0] [] [1]"
            $ let stream =
                    Stream.fromPure 0
                        `Async.parallel` Stream.nil
                        `Async.parallel` Stream.fromPure 1
               in cmp stream sortEq [0, 1]

        prop1 "append left associated"
            $ let stream =
                    Stream.fromPure 0
                        `Async.parallel` Stream.fromPure 1
                        `Async.parallel` Stream.fromPure 2
                        `Async.parallel` Stream.fromPure 3
               in cmp stream sortEq [0, 1, 2, 3]

        prop1 "append right associated"
            $ let stream =
                    Stream.fromPure 0
                        `Async.parallel` (Stream.fromPure 1
                        `Async.parallel` (Stream.fromPure 2
                        `Async.parallel` Stream.fromPure 3))
               in cmp stream sortEq [0, 1, 2, 3]

        prop1 "append balanced"
            $ let leaf x y = Stream.fromPure x `Async.parallel` Stream.fromPure y
                  leaf11 = leaf 0 1 `Async.parallel` leaf 2 (3 :: Int)
                  leaf12 = leaf 4 5 `Async.parallel` leaf 6 7
                  stream = leaf11 `Async.parallel` leaf12
               in cmp stream sortEq [0, 1, 2, 3, 4, 5, 6,7]

        prop1 "appendWith (maxBuffer 1)"
            $ let stream =
                    Async.parallelWith Async.StopNone (Async.maxBuffer 1)
                        (Stream.fromList [1,2,3,4,5])
                        (Stream.fromList [6,7,8,9,10])
               in cmp stream sortEq [1,2,3,4,5,6,7,8,9,10]

        prop1 "apply (async arg1)"
            $ let s1 = Async.apply (pure (,)) (pure 1 `Async.parallel` pure 2)
                  s2 = Async.apply s1 (pure 3) :: Stream IO (Int, Int)
                  xs = Stream.fold Fold.toList s2
               in sort <$> xs `shouldReturn` [(1, 3), (2, 3)]

        prop1 "apply (async arg2)"
            $ let s1 = pure (1,)
                  s2 = Async.apply s1 (pure 2 `Async.parallel` pure 3)
                  xs = Stream.fold Fold.toList s2 :: IO [(Int, Int)]
               in sort <$> xs `shouldReturn` [(1, 2), (1, 3)]

        -- concat
        prop1 "concat"
            $ let stream =
                    Async.concat
                        $ fmap Stream.fromPure
                        $ Stream.fromList [1..100]
                in cmp stream sortEq [1..100]

        prop "concatMap" $
            forAll (choose (0, 100)) $ \n ->
                transform
                    (concatMap (const [1..n]))
                    (Async.concatMap (const (Stream.fromList [1..n])))

#ifdef DEVBUILD
        describe "Time ordering" $ timeOrdering Async.parallel
#endif
        describe "Exception propagation" $ exceptionPropagation Async.parallel
        -- Ad-hoc tests
        it "takes n from stream of streams" $ takeCombined 2
