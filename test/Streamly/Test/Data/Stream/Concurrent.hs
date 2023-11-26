-- |
-- Module      : Streamly.Test.Data.Stream.Concurrent
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Concurrent (main) where

#ifdef DEVBUILD
import Control.Concurrent (threadDelay)
#endif
import Control.Exception (Exception, try)
import Control.Monad (replicateM)
import Control.Monad.Catch (throwM)
import Data.List (sort)
import Data.Word (Word8)
import Streamly.Data.Stream (Stream)
import Test.Hspec.QuickCheck
import Test.QuickCheck (Testable, Property, choose, forAll, withMaxSuccess)
import Test.QuickCheck.Monadic (monadicIO, run)
import Test.Hspec as H

import qualified Streamly.Data.Fold as Fold ( toList )
import qualified Streamly.Data.Stream as Stream
    ( replicate, fromEffect, fromPure, fromList, fold, take, nil )
import qualified Streamly.Internal.Data.Stream.Prelude as Async

import Streamly.Test.Common (listEquals)

moduleName :: String
moduleName = "Data.Stream.Concurrent"

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

sortEq :: Ord a => [a] -> [a] -> Bool
sortEq a b = sort a == sort b

cmp :: (Show a, Ord a) => ([a] -> [a] -> Bool) -> [a] -> Stream IO a -> Property
cmp eq list s =
    monadicIO $ do
        stream <- run $ sort <$> Stream.fold Fold.toList  s
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
            let s1 = op (Async.parList id [constr b, constr c])
            let s2 = Async.parList id [constr a, s1]
            stream <- run (Stream.fold Fold.toList s2)
            let list = a <> listOp (b <> c)
            listEquals eq stream list

commonOpConfigs :: [(String, Async.Config -> Async.Config)]
commonOpConfigs =
    [ ("default", id)
#ifndef COVERAGE_BUILD
    , ("rate AvgRate 10000", Async.avgRate 10000)
    , ("rate Nothing", Async.rate Nothing)
    , ("maxBuffer 0", Async.maxBuffer 0)
    , ("maxThreads 0", Async.maxThreads 0)
    , ("maxThreads 1", Async.maxThreads 1)
    , ("eager", Async.eager True)
    -- XXX Need to use an unsorted eq operation for ahead
    , ("ordered", Async.ordered True)
#ifdef USE_LARGE_MEMORY
    , ("maxThreads -1", Async.maxThreads (-1))
#endif
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
        try (tl (Stream.fromEffect (throwM (ExampleException "E")) `f` Stream.nil))
        `shouldReturn`
            (Left (ExampleException "E") :: Either ExampleException [Int])
    it "append nil, throwM" $
        try (tl (Stream.nil `f` Stream.fromEffect (throwM (ExampleException "E"))))
        `shouldReturn`
            (Left (ExampleException "E") :: Either ExampleException [Int])
    it "append nested throwM" $ do
        let nested =
                (Stream.fromList [1..10])
                    `f` Stream.fromEffect (throwM (ExampleException "E"))
                    `f` (Stream.fromList [1..10])
        try (tl (Stream.nil `f` nested `f` (Stream.fromList [1..10])))
            `shouldReturn`
                (Left (ExampleException "E")
                    :: Either ExampleException [Int])
    it "sequence throwM" $
        let stream = Stream.fromList [throwM (ExampleException "E")]
         in try (tl (Stream.nil `f` Async.parSequence id stream))
            `shouldReturn`
                (Left (ExampleException "E") :: Either ExampleException [Int])

    it "concatMap throwM" $ do
        let s1 = Async.parList id $ fmap Stream.fromPure [1..4]
            s2 = Async.parList id $ fmap Stream.fromPure [5..8]
        try $ tl (
            let bind = flip (Async.parConcatMap id)
             in bind s1 $ \x ->
                bind s2 $ \y ->
                    if x + y > 10
                    then Stream.fromEffect (throwM (ExampleException "E"))
                    else Stream.fromPure (x + y)
            )
        `shouldReturn`
            (Left (ExampleException "E") :: Either ExampleException [Int])

    where

    tl = Stream.fold Fold.toList

---------------------------------------------------------------------------
-- Time ordering
---------------------------------------------------------------------------

#ifdef DEVBUILD
timeOrdering :: ([Stream IO Int] -> Stream IO Int) -> Spec
timeOrdering f = do
    it "Parallel event ordering check" $
        Stream.fold Fold.toList (f [event 4, event 3, event 2, event 1])
            `shouldReturn` [1..4]

    where event n = Stream.fromEffect (threadDelay (n * 200000) >> return n)
#endif

-------------------------------------------------------------------------------
-- Some ad-hoc tests that failed at times
-------------------------------------------------------------------------------

takeCombined :: Int -> IO ()
takeCombined n = do
    let constr = Stream.fromList
    let s = Async.parList id [constr ([] :: [Int]), constr ([] :: [Int])]
    r <- Stream.fold Fold.toList $ Stream.take n s
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
    stream = Async.parSequence cfg . flip Stream.replicate (return 1 :: IO Int)

main :: IO ()
main = hspec
  $ H.parallel
#ifdef COVERAGE_BUILD
  $ modifyMaxSuccess (const 10)
#endif
  $ describe moduleName $ do
        let transform = transformCombineFromList Stream.fromList sortEq

        prop "parEval" $
            transform
                (fmap (+2))
                (fmap (+1) . Async.parEval id . fmap (+1))

        asyncSpec $ prop "parSequence" . sequenceReplicate

        asyncSpec $
            prop "parMapM (+1)"
                . transform (fmap (+1))
                . (`Async.parMapM` (\x -> return (x + 1)))

        -- XXX Need to use eq instead of sortEq for ahead oeprations
        -- Binary append
        asyncSpec $
            let appWith cfg = Async.parList cfg [Stream.nil, Stream.nil]
            in prop1 "parList [] []" . cmp sortEq ([] :: [Int]) . appWith

        asyncSpec $
            let appWith cfg = Async.parList cfg [Stream.nil, Stream.fromPure 1]
            in prop1 "parList [] [1]" . cmp sortEq [1 :: Int] . appWith

        asyncSpec $
            let appWith cfg = Async.parList cfg [Stream.fromPure 1, Stream.nil]
            in prop1 "parList [1] []" . cmp sortEq [1 :: Int] . appWith

        asyncSpec $
            let appWith cfg =
                    Async.parList cfg [Stream.fromPure 0, Stream.fromPure 1]
            in prop1 "parList [0] [1]" . cmp sortEq [0, 1 :: Int] . appWith

        asyncSpec $
            let appWith cfg =
                    Async.parList
                        cfg [Stream.fromPure 0, Stream.nil, Stream.fromPure 1]
            in prop1 "parList [0] [] [1]" . cmp sortEq [0, 1 :: Int] . appWith

        asyncSpec $
            let appWith cfg =
                    Async.parTwo cfg
                        (Async.parTwo cfg
                            (Async.parTwo cfg
                                (Stream.fromPure 0) (Stream.fromPure 1))
                            (Stream.fromPure 2))
                        (Stream.fromPure 3)
            in prop1 "parTwo left associated"
                    . cmp sortEq [0, 1, 2, 3 :: Int] . appWith

        asyncSpec $
            let appWith cfg =
                    Async.parTwo cfg
                        (Stream.fromPure 0)
                        (Async.parTwo cfg
                            (Stream.fromPure 1)
                            (Async.parTwo cfg
                                (Stream.fromPure 2) (Stream.fromPure 3))
                        )
            in prop1 "parTwo right associated"
                    . cmp sortEq [0, 1, 2, 3 :: Int] . appWith

        asyncSpec $
            let leaf x y cfg =
                    Async.parTwo cfg (Stream.fromPure x)
                        (Stream.fromPure y)
                leaf11 cfg =
                    Async.parTwo cfg (leaf 0 1 cfg) $ leaf 2 (3 :: Int) cfg
                leaf12 cfg =
                    Async.parTwo cfg (leaf 4 5 cfg) $ leaf 6 7 cfg
                appWith cfg =
                    Async.parTwo cfg (leaf11 cfg) (leaf12 cfg)
            in prop1 "parTwo balanced"
                    . cmp sortEq [0, 1, 2, 3, 4, 5, 6,7] . appWith

        asyncSpec $
            let appWith cfg =
                    Async.parTwo cfg
                        (Stream.fromList [1,2,3,4,5 :: Int])
                        (Stream.fromList [6,7,8,9,10])
            in prop1 "parTwo" . cmp (==) [1,2,3,4,5,6,7,8,9,10] . appWith

        asyncSpec $
            let par2 cfg =
                    Async.parTwo
                        cfg
                        (Stream.fromPure 1)
                        (Stream.fromPure 2)
                s1 cfg =
                    Async.parApply
                        cfg
                        (Stream.fromPure (,))
                        (par2 cfg)
                s2 cfg =
                    Async.parApply
                        cfg
                        (s1 cfg)
                        (Stream.fromPure 3) :: Stream IO (Int, Int)
            in prop1
                "parApply (async arg1)" . cmp (==) ( [(1, 3), (2, 3)]) . s2

        asyncSpec $
            let par2 cfg =
                    Async.parTwo
                        cfg
                        (Stream.fromPure (2 :: Int))
                        (Stream.fromPure 3)
                s1 = Stream.fromPure (1 :: Int,)
                s2 cfg = Async.parApply cfg s1 (par2 cfg)
            in prop1 "apply (async arg2)" . cmp (==) ([(1, 2), (1, 3)]) . s2

        -- concat
        asyncSpec $
            let stream cfg =
                    Async.parConcat cfg
                        $ fmap Stream.fromPure
                        $ Stream.fromList [1..100 :: Int]
            in prop1 "parConcat" . cmp sortEq [1..100] . stream

        asyncSpec $
            let f cfg =
                    forAll (choose (0, 100)) $ \n ->
                        transform
                            (concatMap (const [1..n]))
                            (Async.parConcatMap
                                cfg (const (Stream.fromList [1..n]))
                            )
             in prop "parConcatMap" . f

#ifdef DEVBUILD
        describe "Time ordering" $ timeOrdering (Async.parList id)
#endif
        let async = Async.parTwo id
        describe "Exception propagation" $ exceptionPropagation async
        -- Ad-hoc tests
        it "takes n from stream of streams" $ takeCombined 2
