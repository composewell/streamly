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
import Control.Monad (replicateM, when)
import Control.Monad.Catch (throwM)
import Data.Function ( (&) )
import Data.IORef ( newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Maybe ( isJust, fromJust )
import Data.Word (Word8)
import Streamly.Data.Stream (Stream)
import Test.Hspec.QuickCheck
import System.Random (randomRIO)
import Test.QuickCheck
    (Testable, Property, choose, counterexample, forAll, ioProperty)
import Test.QuickCheck.Monadic (monadicIO, run)
import Test.Hspec as H

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream.Prelude as Async

import Streamly.Test.Common (withNumTests, listEquals)
import Streamly.Test.Data.Stream.TreeModel

#ifdef DEVBUILD
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

    mapM_ traversalThrow treeTraversals

    where

    tl = Stream.fold Fold.toList

    traversalThrow t =
        it (ttName t ++ " throwM") $ do
            let g x
                    | x == 3 =
                        Just $ Stream.fromEffect (throwM (ExampleException "E"))
                    | x < 10 = Just $ Stream.fromList [2 * x + 1, 2 * x + 2]
                    | otherwise = Nothing
                run cfg = tl (Stream.nil `f` ttRun t cfg g (Stream.fromPure 0))
            try (run id)
                `shouldReturn`
                    (Left (ExampleException "E")
                        :: Either ExampleException [Int])
            try (run (Async.ordered True))
                `shouldReturn`
                    (Left (ExampleException "E")
                        :: Either ExampleException [Int])

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

-------------------------------------------------------------------------------
-- Tree traversals
-------------------------------------------------------------------------------

-- | A parallel tree traversal and the model of the serial traversal with the
-- same order.
data TreeTraversal = TreeTraversal
    { ttName :: String
    , ttRun ::
           (Async.Config -> Async.Config)
        -> (Int -> Maybe (Stream IO Int))
        -> Stream IO Int
        -> Stream IO Int
    , ttModel :: (Int -> [Int]) -> [Int] -> [Int]
    -- | Each node is emitted after its children, otherwise before.
    , ttPost :: Bool
    -- | With maxThreads 1 the output is the serial order.
    , ttSerial :: Bool
    }

treeTraversals :: [TreeTraversal]
treeTraversals =
    [ TreeTraversal "parConcatIterate" Async.parConcatIterate
        refPre False True
    , TreeTraversal "parConcatIteratePost" Async.parConcatIteratePost
        refPost True True
    , TreeTraversal "parConcatIterateBatch" Async.parConcatIterateBatch
        (refBatchPre False) False True
    , TreeTraversal "parConcatIterateBatchRev" Async.parConcatIterateBatchRev
        (refBatchPre True) False True
    , TreeTraversal "parConcatIterateBatchPost"
        Async.parConcatIterateBatchPost (refBatchPost False) True True
    , TreeTraversal "parConcatIterateBatchPostRev"
        Async.parConcatIterateBatchPostRev (refBatchPost True) True True
    , TreeTraversal "parWideConcatIterate" Async.parWideConcatIterate
        refWide False True
    -- The nodes completed together at the end of a stream are emitted in
    -- separate turns of the round robin with maxThreads 1.
    , TreeTraversal "parWideConcatIteratePost" Async.parWideConcatIteratePost
        refWidePost True False
    , TreeTraversal "parWideConcatIterateBatch"
        Async.parWideConcatIterateBatch (refWideBatchPre False) False True
    , TreeTraversal "parWideConcatIterateBatchRev"
        Async.parWideConcatIterateBatchRev (refWideBatchPre True) False True
    , TreeTraversal "parWideConcatIterateBatchPost"
        Async.parWideConcatIterateBatchPost (refWideBatchPost False) True True
    , TreeTraversal "parWideConcatIterateBatchPostRev"
        Async.parWideConcatIterateBatchPostRev
        (refWideBatchPost True) True True
    ]

-- | The children of a node as a stream, each element delayed by a random
-- time of up to 20 microseconds so that the workers overlap.
delayedChildren :: (Int -> [Int]) -> Int -> Maybe (Stream IO Int)
delayedChildren children n =
    case children n of
        [] -> Nothing
        cs -> Just $ Stream.mapM delay (Stream.fromList cs)

    where

    delay x = do
        t <- randomRIO (0, 20)
        threadDelay t
        return x

runTraversal ::
       TreeTraversal
    -> (Async.Config -> Async.Config)
    -> (Int -> [Int])
    -> [Int]
    -> IO [Int]
runTraversal t cfg children roots =
    Stream.fold Fold.toList
        $ ttRun t cfg (delayedChildren children) (Stream.fromList roots)

-- | Every node is emitted exactly once, after all its children in post
-- order, before them in pre-order.
traversalOrderOk :: Bool -> (Int -> [Int]) -> [Int] -> [Int] -> Bool
traversalOrderOk post children expected out =
    sort out == sort expected && all childrenOk out

    where

    pos = Map.fromList (zip out [0 :: Int ..])
    before x c = (pos Map.! c < pos Map.! x) == post
    childrenOk x = all (before x) (children x)

-- | Compare the output of the traversal of random forests using cfg with the
-- serial order, @exact@ requires the same order, otherwise only the parent
-- child order is checked.
treeTraversalProp ::
    Bool -> TreeTraversal -> (Async.Config -> Async.Config) -> Property
treeTraversalProp exact t cfg =
    withNumTests maxTestCount
        $ forAll genForest $ \(m, roots) ->
            let children = forestChildren m
                expected = ttModel t children roots
             in ioProperty $ do
                    out <- runTraversal t cfg children roots
                    let ok =
                            if exact
                            then out == expected
                            else traversalOrderOk (ttPost t) children expected out
                    return
                        $ counterexample
                            (show (IntMap.toList m, roots, expected, out)) ok

-- | The ordered traversals on the example tree give the outputs in the order
-- table of docs/Developer/tree-traversal.md.
treeTraversalExample :: TreeTraversal -> [Int] -> Spec
treeTraversalExample t expected =
    it (ttName t ++ " ordered example") $
        runTraversal t (Async.ordered True) exampleChildren [1]
            `shouldReturn` expected

-- | Take the first n nodes of an infinite tree, the traversal must stop when
-- the output is abandoned. The odd nodes have the children 4n to 4n + 3, the
-- even nodes are leaves, so that the post order traversals emit leaves.
takeInfiniteTree :: TreeTraversal -> (Async.Config -> Async.Config) -> IO ()
takeInfiniteTree t cfg = do
    xs <-
        Stream.fold Fold.toList
            $ Stream.take 1000
            $ ttRun t cfg g (Stream.fromPure (1 :: Int))
    length xs `shouldBe` 1000

    where

    g x =
        if even x
        then Nothing
        else Just $ Stream.fromList [4 * x .. 4 * x + 3]

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

        describe "Tree traversals" $ do
            let examples =
                    [ [1, 2, 4, 8, 5, 3, 6, 9, 7]
                    , [8, 4, 5, 2, 9, 6, 7, 3, 1]
                    , [1, 2, 3, 4, 5, 8, 6, 7, 9]
                    , [1, 2, 3, 6, 7, 9, 4, 5, 8]
                    , [5, 8, 4, 2, 7, 9, 6, 3, 1]
                    , [7, 9, 6, 3, 5, 8, 4, 2, 1]
                    , [1, 2, 3, 4, 6, 5, 8, 7, 9]
                    , [5, 8, 7, 9, 4, 2, 6, 3, 1]
                    , [1, 2, 3, 4, 5, 6, 7, 8, 9]
                    , [1, 2, 3, 6, 7, 4, 5, 8, 9]
                    , [5, 7, 8, 9, 4, 6, 2, 3, 1]
                    , [7, 5, 8, 9, 4, 6, 3, 2, 1]
                    ]
            sequence_ $ zipWith treeTraversalExample treeTraversals examples
            let traversalSpec t = do
                    asyncSpec
                        $ prop (ttName t) . treeTraversalProp False t
                    asyncSpec
                        $ prop (ttName t ++ " ordered")
                            . treeTraversalProp True t
                            . (Async.ordered True .)
                    when (ttSerial t)
                        $ prop (ttName t ++ " maxThreads 1")
                        $ treeTraversalProp True t (Async.maxThreads 1)
                    it (ttName t ++ " take from an infinite tree")
                        $ takeInfiniteTree t id
                    it (ttName t ++ " ordered take from an infinite tree")
                        $ takeInfiniteTree t (Async.ordered True)
            mapM_ traversalSpec treeTraversals

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
