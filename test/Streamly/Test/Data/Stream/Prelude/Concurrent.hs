-- |
-- Module      : Streamly.Test.Data.Stream.Prelude.Concurrent
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Prelude.Concurrent (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, try)
import Control.Monad (replicateM)
import Control.Monad.Catch (throwM)
import Data.Function ( (&) )
import Data.IORef ( newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Maybe ( isJust, fromJust )
import Data.Word (Word8)
import Streamly.Data.Stream (Stream)
import Test.Hspec.QuickCheck
import Test.QuickCheck (Testable, Property, choose, forAll)
import Test.QuickCheck.Monadic (monadicIO, run)
import Test.Hspec as H

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream.Prelude as Async

import Streamly.Test.Common (withNumTests, listEquals)

#ifdef DEVBUILD
import Control.Monad (when)
import Data.Int (Int64)
import Streamly.Internal.Data.Time.Units
       (AbsTime, NanoSecond64(..), toRelTime64, diffAbsTime64)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
#endif

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
    withNumTests maxTestCount $
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
                Stream.fromList [1..10]
                    `f` Stream.fromEffect (throwM (ExampleException "E"))
                    `f` Stream.fromList [1..10]
        try (tl (Stream.nil `f` nested `f` Stream.fromList [1..10]))
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

    it "mergeMap throwM" $ do
        let g x =
                if x == 3
                then Stream.fromEffect (throwM (ExampleException "E"))
                else Stream.fromPure x
        try (tl (Stream.nil `f` Async.parMergeMap id g (Stream.fromList [1..5])))
        `shouldReturn`
            (Left (ExampleException "E") :: Either ExampleException [Int])

    it "mergeIterate throwM" $ do
        let g x =
                if x == 3
                then Stream.fromEffect (throwM (ExampleException "E"))
                else if x < 10
                then Stream.fromList [2 * x + 1, 2 * x + 2]
                else Stream.nil
            s = Async.parMergeIterate id g (Stream.fromPure 0)
        try (tl (Stream.nil `f` s))
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

tenPow8 :: Int64
tenPow8 = 10^(8 :: Int)

tenPow7 :: Int64
tenPow7 = 10^(7 :: Int)

takeDropTime :: NanoSecond64
takeDropTime = NanoSecond64 $ 5 * tenPow8

checkTakeDropTime :: (Maybe AbsTime, Maybe AbsTime) -> IO Bool
checkTakeDropTime (mt0, mt1) = do
    let graceTime = NanoSecond64 $ 8 * tenPow7
    case mt0 of
        Nothing -> return True
        Just t0 ->
            case mt1 of
                Nothing -> return True
                Just t1 -> do
                    let tMax = toRelTime64 (takeDropTime + graceTime)
                    let tMin = toRelTime64 (takeDropTime - graceTime)
                    let t = diffAbsTime64 t1 t0
                    let r = t >= tMin && t <= tMax
                    when (not r) $ putStrLn $
                        "t = " ++ show t ++
                        " tMin = " ++ show tMin ++
                        " tMax = " ++ show tMax
                    return r

testTakeInterval :: IO Bool
testTakeInterval = do
    r <-
          Stream.fold (Fold.tee Fold.head Fold.last)
        $ Stream.takeInterval (fromIntegral takeDropTime * 10**(-9))
        $ Stream.repeatM (threadDelay 1000 >> getTime Monotonic)
    checkTakeDropTime r

testDropInterval :: IO Bool
testDropInterval = do
    t0 <- getTime Monotonic
    mt1 <-
          Stream.fold Fold.head
        $ Stream.dropInterval (fromIntegral takeDropTime * 10**(-9))
        $ Stream.repeatM (threadDelay 1000 >> getTime Monotonic)
    checkTakeDropTime (Just t0, mt1)
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

-- Infinite inner streams must be stopped when the output is abandoned.
takeMergeMap :: Int -> IO ()
takeMergeMap n = do
    let s = Async.parMergeMap id (Stream.repeat :: Int -> Stream IO Int)
    r <- Stream.fold Fold.toList $ Stream.take n $ s $ Stream.fromList [1..5]
    length r `shouldBe` n

-- An infinitely deep tree must be stopped when the output is abandoned.
--
-- XXX This test is disabled, parMergeIterate does not stop the workers of an
-- infinite tree when the output is abandoned.
--
-- Each element of a channel creates new channels for its children. The
-- buffer of each channel is bounded but the number of channels is not, so
-- the workers expand the tree far ahead of the consumer, taking 1000
-- elements takes 0.75 to 9 seconds. When the consumer abandons the output,
-- the workers of the outermost channel are killed. The channels created by
-- these workers have no consumer, but their workers are runnable and keep
-- creating deeper channels, so they never block and are never garbage
-- collected. After taking 1000 elements the live heap was 816 MB after a
-- major GC and growing, the test process keeps running at 100% CPU.
--
-- parMergeMap with infinite inner streams and parMergeIterate on an
-- infinite chain with one child per node do not have this problem, the live
-- heap is about 70 KB after the take.
takeMergeIterate :: Int -> IO ()
takeMergeIterate n = do
    let g x = Stream.fromList [2 * x + 1, 2 * x + 2] :: Stream IO Int
    r <-
        Stream.fold Fold.toList
            $ Stream.take n
            $ Async.parMergeIterate id g
            $ Stream.fromPure 0
    length r `shouldBe` n

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

constructWithLenM
    :: (Int -> Stream IO Int)
    -> (Int -> IO [Int])
    -> Word8
    -> Property
constructWithLenM mkStream mkList len =
    withNumTests maxTestCount
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

-- With "ordered" the output of parMergeMap must be the same as serial concat.
mergeMapOrdered :: (Async.Config -> Async.Config) -> [[Int]] -> Property
mergeMapOrdered cfg xss =
    withNumTests maxTestCount
        $ monadicIO $ do
            stream <-
                run
                    $ Stream.fold Fold.toList
                    $ Async.parMergeMap (Async.ordered True . cfg) Stream.fromList
                    $ Stream.fromList xss
            listEquals (==) stream (concat xss)

-- A complete k-ary tree with nodes numbered in level order from 0, node n
-- has the children k * n + 1 to k * n + k if n < limit.
treeChildren :: Int -> Int -> Int -> [Int]
treeChildren k limit n
    | n < limit = [k * n + 1 .. k * n + k]
    | otherwise = []

-- Serial depth first pre-order traversal of the tree rooted at n.
treePreOrder :: Int -> Int -> Int -> [Int]
treePreOrder k limit n =
    n : concatMap (treePreOrder k limit) (treeChildren k limit n)

-- Traverse the trees rooted at 0 to r - 1 using parMergeIterate and compare
-- the output with the serial pre-order traversal using eq.
mergeIterateTree ::
    ([Int] -> [Int] -> Bool) -> (Async.Config -> Async.Config) -> Property
mergeIterateTree eq cfg =
    withNumTests maxTestCount
        $ forAll (choose (0, 3)) $ \r ->
          forAll (choose (1, 4)) $ \k ->
          forAll (choose (0, 100)) $ \limit ->
            monadicIO $ do
                stream <-
                    run
                        $ Stream.fold Fold.toList
                        $ Async.parMergeIterate
                            cfg
                            (Stream.fromList . treeChildren k limit)
                        $ Stream.fromList [0 .. r - 1]
                listEquals
                    eq stream (concatMap (treePreOrder k limit) [0 .. r - 1])

drainMapM :: Monad m => (a -> m b) -> Stream m a -> m ()
drainMapM f = Stream.fold (Fold.drainMapM f)

testFromCallback :: IO Int
testFromCallback = do
    ref <- newIORef Nothing
    let stream =
           Stream.parList (Stream.eager True)
                [ fmap Just (Stream.fromCallback (setCallback ref))
                , runCallback ref
                ]
    Stream.fold Fold.sum $ fmap fromJust $ Stream.takeWhile isJust stream

    where

    setCallback ref cb = do
        writeIORef ref (Just cb)

    runCallback ref = Stream.fromEffect $ do
        cb <-
             Stream.repeatM (readIORef ref)
                & Stream.delayPost 0.1
                & Stream.mapMaybe id
                & Stream.fold Fold.one

        Stream.fromList [1..100]
            & Stream.delayPost 0.001
            & drainMapM (fromJust cb)
        threadDelay 100000
        return Nothing

main :: IO ()
main = hspec
  $ H.parallel
#ifdef COVERAGE_BUILD
  $ modifyMaxSuccess (const 10)
#endif
  $ describe moduleName $ do
        let transform = transformCombineFromList Stream.fromList sortEq

        prop "parBuffered" $
            transform
                (fmap (+2))
                (fmap (+1) . Async.parBuffered id . fmap (+1))

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
                    Async.parCrossApply
                        cfg
                        (Stream.fromPure (,))
                        (par2 cfg)
                s2 cfg =
                    Async.parCrossApply
                        cfg
                        (s1 cfg)
                        (Stream.fromPure 3) :: Stream IO (Int, Int)
            in prop1
                "parCrossApply (async arg1)" . cmp (==) [(1, 3), (2, 3)] . s2

        asyncSpec $
            let par2 cfg =
                    Async.parTwo
                        cfg
                        (Stream.fromPure (2 :: Int))
                        (Stream.fromPure 3)
                s1 = Stream.fromPure (1 :: Int,)
                s2 cfg = Async.parCrossApply cfg s1 (par2 cfg)
            in prop1 "apply (async arg2)" . cmp (==) [(1, 2), (1, 3)] . s2

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

        asyncSpec $
            let stream cfg =
                    Async.parMergeMap cfg Stream.fromPure
                        $ Stream.fromList ([] :: [Int])
            in prop1 "parMergeMap []" . cmp sortEq [] . stream

        asyncSpec $
            let stream cfg =
                    Async.parMergeMap cfg Stream.fromPure
                        $ Stream.fromList [1..100 :: Int]
            in prop1 "parMergeMap fromPure" . cmp sortEq [1..100] . stream

        asyncSpec $
            let f cfg =
                    forAll (choose (0, 100)) $ \n ->
                        transform
                            (concatMap (const [1..n]))
                            (Async.parMergeMap
                                cfg (const (Stream.fromList [1..n]))
                            )
             in prop "parMergeMap" . f

        asyncSpec $ prop "parMergeMap ordered" . mergeMapOrdered

        asyncSpec $ prop "parMergeIterate" . mergeIterateTree sortEq

        asyncSpec $
            prop "parMergeIterate ordered"
                . mergeIterateTree (==)
                . (Async.ordered True .)

#ifdef DEVBUILD
        describe "Time ordering" $ timeOrdering (Async.parList id)
        describe "Filtering" $ do
            it "takeInterval" (testTakeInterval `shouldReturn` True)
#ifdef INCLUDE_FLAKY_TESTS
            it "dropInterval" (testDropInterval `shouldReturn` True)
#endif
#endif
        let async = Async.parTwo id
        describe "Exception propagation" $ exceptionPropagation async
        -- Ad-hoc tests
        it "takes n from stream of streams" $ takeCombined 2
        it "takes n from parMergeMap of infinite streams" $ takeMergeMap 1000
        -- XXX Disabled, see takeMergeIterate
        xit "takes n from parMergeIterate of an infinite tree"
            $ takeMergeIterate 1000
        it "fromCallback" $ testFromCallback `shouldReturn` (50*101)
