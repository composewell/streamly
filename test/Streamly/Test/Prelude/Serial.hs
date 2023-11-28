{-# OPTIONS_GHC -Wno-deprecations #-}
{-# Language TypeApplications #-}
-- XXX We are using head/tail at one place
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

-- |
-- Module      : Streamly.Test.Prelude.Serial
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude.Serial (checkTakeDropTime, main) where

import Control.Concurrent ( threadDelay )
import Control.Monad ( when, forM_ )
import Data.Function ( (&) )
import Data.IORef ( newIORef, readIORef, writeIORef, modifyIORef' )
import Data.Int (Int64)
import Data.List (sort, group, intercalate)
import Data.Maybe ( isJust, fromJust )
import Data.Word (Word8)
import Data.Semigroup (Sum(..), getSum)
import Foreign.Storable (Storable)
import Streamly.Internal.Data.MutByteArray (Unbox)
import Test.Hspec.QuickCheck
import Test.QuickCheck
    ( Gen
    , Property
    , Arbitrary(..)
    , choose
    , elements
    , forAll
    , frequency
    , listOf
    , listOf1
    , suchThat
    , vectorOf
    , withMaxSuccess
    )
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import Test.Hspec as H

import Streamly.Prelude (SerialT, IsStream, serial, fromSerial)
#ifndef COVERAGE_BUILD
import Streamly.Prelude (avgRate, maxBuffer)
#endif
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream as IS
import qualified Streamly.Internal.Data.StreamK as K
import qualified Streamly.Data.Array as A
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Prelude as SP

import Streamly.Internal.Data.Time.Units
       (AbsTime, NanoSecond64(..), toRelTime64, diffAbsTime64)

#ifdef DEVBUILD
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
#endif

import Streamly.Test.Common
import Streamly.Test.Prelude.Common
import Control.Monad.IO.Class (MonadIO)

toList :: Monad m => S.Stream m a -> m [a]
toList = S.fold FL.toList

splitOn :: Monad m =>
    (a -> Bool) -> FL.Fold m a b -> S.Stream m a -> S.Stream m b
splitOn predicate f = IS.foldManyPost (FL.takeEndBy_ predicate f)

splitOnSuffix :: Monad m =>
    (a -> Bool) -> FL.Fold m a b -> S.Stream m a -> S.Stream m b
splitOnSuffix predicate f = S.foldMany (FL.takeEndBy_ predicate f)

splitOnSeq' :: (MonadIO m, Unbox a, Storable a, Enum a, Eq a) =>
    A.Array a -> FL.Fold m a b -> S.Stream m a -> S.Stream m b
splitOnSeq' patt f m = IS.foldManyPost (FL.takeEndBySeq_ patt f) m

splitOnSuffixSeq' :: (MonadIO m, Unbox a, Storable a, Enum a, Eq a) =>
    A.Array a -> FL.Fold m a b -> S.Stream m a -> S.Stream m b
splitOnSuffixSeq' patt f m = S.foldMany (FL.takeEndBySeq_ patt f) m

groupsBy :: Monad m =>
    (a -> a -> Bool) -> FL.Fold m a b -> S.Stream m a -> S.Stream m b
groupsBy cmp f m = S.catRights $ S.parseMany (Parser.groupBy cmp f) m

groupsByRolling :: Monad m =>
    (a -> a -> Bool) -> FL.Fold m a b -> S.Stream m a -> S.Stream m b
groupsByRolling cmp f m =
    S.catRights $ S.parseMany (Parser.groupByRolling cmp f) m

drainWhile :: Monad m => (a -> Bool) -> SerialT m a -> m ()
drainWhile p m = SP.fold FL.drain $ SP.takeWhile p m

drainMapM :: Monad m => (a -> m b) -> S.Stream m a -> m ()
drainMapM f = S.fold (FL.drainMapM f)

splitOnSeq :: Spec
splitOnSeq = do
    describe "Tests for splitOnSeq" $ do
        it "splitOnSeq_ \"hello\" \"\" = [\"\"]"
          $ splitOnSeq_ "hello" "" `shouldReturn` [""]
        it "splitOnSeq_ \"hello\" \"hello\" = [\"\", \"\"]"
          $ splitOnSeq_ "hello" "hello" `shouldReturn` ["", ""]
        it "splitOnSeq_ \"x\" \"hello\" = [\"hello\"]"
          $ splitOnSeq_ "x" "hello" `shouldReturn` ["hello"]
        it "splitOnSeq_ \"h\" \"hello\" = [\"\", \"ello\"]"
          $ splitOnSeq_ "h" "hello" `shouldReturn` ["", "ello"]
        it "splitOnSeq_ \"o\" \"hello\" = [\"hell\", \"\"]"
          $ splitOnSeq_ "o" "hello" `shouldReturn` ["hell", ""]
        it "splitOnSeq_ \"e\" \"hello\" = [\"h\", \"llo\"]"
          $ splitOnSeq_ "e" "hello" `shouldReturn` ["h", "llo"]
        it "splitOnSeq_ \"l\" \"hello\" = [\"he\", \"\", \"o\"]"
          $ splitOnSeq_ "l" "hello" `shouldReturn` ["he", "", "o"]
        it "splitOnSeq_ \"ll\" \"hello\" = [\"he\", \"o\"]"
          $ splitOnSeq_ "ll" "hello" `shouldReturn` ["he", "o"]

    where

    splitOnSeq_ pat xs = toList $
        splitOnSeq' (A.fromList pat) FL.toList (S.fromList xs)

splitOnSuffixSeq :: Spec
splitOnSuffixSeq = do
    describe "Tests for splitOnSuffixSeq" $ do
        it "splitSuffixOn_ \".\" \"\" []"
          $ splitSuffixOn_ "." "" `shouldReturn` []
        it "splitSuffixOn_ \".\" \".\" [\"\"]"
          $ splitSuffixOn_ "." "." `shouldReturn` [""]
        it "splitSuffixOn_ \".\" \"a\" [\"a\"]"
          $ splitSuffixOn_ "." "a" `shouldReturn` ["a"]
        it "splitSuffixOn_ \".\" \".a\" [\"\",\"a\"]"
          $ splitSuffixOn_ "." ".a" `shouldReturn` ["", "a"]
        it "splitSuffixOn_ \".\" \"a.\" [\"a\"]"
          $ splitSuffixOn_ "." "a." `shouldReturn` ["a"]
        it "splitSuffixOn_ \".\" \"a.b\" [\"a\",\"b\"]"
          $ splitSuffixOn_ "." "a.b" `shouldReturn` ["a", "b"]
        it "splitSuffixOn_ \".\" \"a.b.\" [\"a\",\"b\"]"
          $ splitSuffixOn_ "." "a.b." `shouldReturn` ["a", "b"]
        it "splitSuffixOn_ \".\" \"a..b..\" [\"a\",\"\",\"b\",\"\"]"
          $ splitSuffixOn_ "." "a..b.." `shouldReturn` ["a", "", "b", ""]

    where

    splitSuffixOn_ pat xs = toList $
        splitOnSuffixSeq' (A.fromList pat) FL.toList (S.fromList xs)

splitterProperties ::
       forall a. (Arbitrary a, Eq a, Show a, Storable a, Unbox a, Enum a)
    => a
    -> String
    -> Spec
splitterProperties sep desc = do
    describe (desc <> " splitOnSeq")
        $ do

            forM_ [0, 1, 2, 4]
                $ intercalateSplitEqId splitOnSeq_ intercalate IS.intercalate

            forM_ [0, 1, 2, 4]
                $ concatSplitIntercalateEqConcat
                      splitOnSeq_
                      intercalate
                      IS.intercalate

            -- Exclusive case
            splitIntercalateEqId splitOnSeq_ intercalate IS.intercalate

    describe (desc <> " splitOn")
        $ do

            intercalateSplitEqId splitOn_ intercalate IS.intercalate 1

            concatSplitIntercalateEqConcat
                splitOn_ intercalate IS.intercalate 1

            -- Exclusive case
            splitIntercalateEqId splitOn_ intercalate IS.intercalate

    describe (desc <> " splitOnSuffixSeq")
        $ do

            forM_ [0, 1, 2, 4]
                $ intercalateSplitEqIdNoSepEnd
                      splitOnSuffixSeq_
                      intercalate
                      IS.intercalate

            forM_ [0, 1, 2, 4]
                $ concatSplitIntercalateEqConcat
                      splitOnSuffixSeq_
                      intercalateSuffix
                      IS.intercalateSuffix

            -- Exclusive case
            splitIntercalateEqId
                splitOnSuffixSeq_
                intercalateSuffix
                IS.intercalateSuffix

    describe (desc <> " splitOnSuffix")
        $ do

            intercalateSplitEqIdNoSepEnd
                splitOnSuffix_ intercalate IS.intercalate 1

            concatSplitIntercalateEqConcat
                splitOnSuffix_ intercalateSuffix IS.intercalateSuffix 1

            -- Exclusive case
            splitIntercalateEqId
                splitOnSuffix_ intercalateSuffix IS.intercalateSuffix

    where

    splitOnSeq_ xs ys =
        toList $ splitOnSeq' (A.fromList ys) FL.toList (S.fromList xs)

    splitOnSuffixSeq_ xs ys =
        toList $ splitOnSuffixSeq' (A.fromList ys) FL.toList (S.fromList xs)

    splitOn_ xs ys =
        toList $ splitOn (== (head ys)) FL.toList (S.fromList xs)

    splitOnSuffix_ xs ys =
        toList $ splitOnSuffix (== (head ys)) FL.toList (S.fromList xs)

    intercalateSuffix xs yss = intercalate xs yss ++ xs

    nonSepElem :: Gen a
    nonSepElem = suchThat arbitrary (/= sep)

    listWithSep :: Gen [a]
    listWithSep = listOf $ frequency [(3, arbitrary), (1, elements [sep])]

    listWithoutSep :: Gen [a]
    listWithoutSep = vectorOf 4 nonSepElem

    listsWithoutSep :: Gen [[a]]
    listsWithoutSep = listOf listWithoutSep

    listsWithoutSep1 :: Gen [[a]]
    listsWithoutSep1 = listOf1 listWithoutSep

    intercalateSplitEqId splitter lIntercalater sIntercalater i =
        let name =
                "intercalater . splitter == id ("
                    <> show i <> " element separator)"
         in prop name
                $ forAll listWithSep
                $ \xs -> withMaxSuccess maxTestCount $ monadicIO $ testCase xs

        where

        testCase xs = do
            ys <- splitter xs (replicate i sep)
            szs <-
                toList
                    $ sIntercalater UF.fromList (replicate i sep)
                    $ IS.fromList ys
            let lzs = lIntercalater (replicate i sep) ys
            listEquals (==) szs xs
            listEquals (==) lzs xs

    intercalateSplitEqIdNoSepEnd splitter lIntercalater sIntercalater i =
        let name =
                "intercalater . splitter . (++ [x \\= sep]) == id ("
                    <> show i <> " element separator)"
         in prop name
                $ forAll ((,) <$> listWithSep <*> nonSepElem)
                $ \(xs_, nonSep) -> do
                      let xs = xs_ ++ [nonSep]
                      withMaxSuccess maxTestCount $ monadicIO $ testCase xs

        where

        testCase xs = do
            ys <- splitter xs (replicate i sep)
            szs <-
                toList
                    $ sIntercalater UF.fromList (replicate i sep)
                    $ IS.fromList ys
            let lzs = lIntercalater (replicate i sep) ys
            listEquals (==) szs xs
            listEquals (==) lzs xs

    concatSplitIntercalateEqConcat splitter lIntercalater sIntercalater i =
        let name =
                "concat . splitter . S.intercalater == "
                    <> "concat ("
                    <> show i <> " element separator/possibly empty list)"
         in prop name
                $ forAll listsWithoutSep
                $ \xss -> withMaxSuccess maxTestCount $ monadicIO $ testCase xss

        where

        testCase xss = do
            let lxs = lIntercalater (replicate i sep) xss
            lys <- splitter lxs (replicate i sep)
            sxs <-
                toList
                    $ sIntercalater UF.fromList (replicate i sep)
                    $ S.fromList xss
            sys <- splitter sxs (replicate i sep)
            listEquals (==) (concat lys) (concat xss)
            listEquals (==) (concat sys) (concat xss)

    splitIntercalateEqId splitter lIntercalater sIntercalater =
        let name =
                "splitter . intercalater == id"
                    <> " (exclusive separator/non-empty list)"
         in prop name
                $ forAll listsWithoutSep1
                $ \xss -> do
                      withMaxSuccess maxTestCount $ monadicIO $ testCase xss

        where

        testCase xss = do
            let lxs = lIntercalater [sep] xss
            lys <- splitter lxs [sep]
            sxs <- toList $ sIntercalater UF.fromList [sep] $ S.fromList xss
            sys <- splitter sxs [sep]
            listEquals (==) lys xss
            listEquals (==) sys xss

intercalateSplitOnId ::
       forall a. (Arbitrary a, Eq a, Show a, Num a) =>
       a -> String -> Spec
intercalateSplitOnId x desc =
    prop (desc <> " intercalate [x] . splitOn (== x) == id") $
        forAll listWithZeroes $ \xs -> do
            withMaxSuccess maxTestCount $
                monadicIO $ do
                    ys <- toList $ splitOn (== x) toListFL (S.fromList xs)
                    listEquals (==) (intercalate [x] ys) xs

    where

    listWithZeroes :: Gen [a]
    listWithZeroes = listOf $ frequency [(3, arbitrary), (1, elements [0])]

groupSplitOps :: String -> Spec
groupSplitOps desc = do
    -- splitting
    splitOnSeq
    splitOnSuffixSeq

    -- splitting properties
    splitterProperties (0 :: Int) desc
    splitterProperties (0 :: Word8) desc
    intercalateSplitOnId (0 :: Int) desc
    intercalateSplitOnId (0 :: Word8) desc

-- |
-- After grouping (and folding) Int stream using @<@ operation,
-- the first @Int@ of every @[Int]@ in the @[Int]@ stream should be the minimum.
testGroupsBy :: Property
testGroupsBy =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
            r <- run $ S.fold (FL.all (\ls ->
                case ls of
                    [] -> True
                    (x:_) -> x == minimum ls))
                $ groupsBy (<) FL.toList
                $ S.fromList vec
            assert r

testGroups :: Property
testGroups =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
            r <- toList $ groupsBy (==) FL.toList $ S.fromList vec
            assert $ r == group vec

testGroupsByRolling :: Property
testGroupsByRolling =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
            r <- toList $ groupsByRolling (==) FL.toList $ S.fromList vec
            assert $ r == group vec

-- |
-- If the list is empty, returns Nothing,
-- else wraps the minimum value of the list in Just.
maybeMinimum :: [Int] -> Maybe Int
maybeMinimum [] = Nothing
maybeMinimum ls = Just $ minimum ls

-- |
-- Checks if the @[Int]@ is non-increasing.
decreasing :: [Maybe Int] -> Bool
decreasing [] = True
decreasing xs = all (== True) $ zipWith (<=) (tail xs) xs

-- |
-- To check if the minimum elements (after grouping on @<@)
-- are non-increasing (either decrease or remain the same).
-- Had an element been strictly greater, it would have been grouped
-- with that element only.
testGroupsBySep :: Property
testGroupsBySep =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
            a <- toList
                    $ fmap maybeMinimum
                    $ groupsBy (<) FL.toList
                    $ S.fromList vec
            assert $ decreasing a

groupingOps :: Spec
groupingOps = do
    prop "groupsBy" testGroupsBy
    prop "S.groups = groups" testGroups
    prop "S.groupsByRolling = groups" testGroupsByRolling
    prop "testGroupsBySep" testGroupsBySep

associativityCheck
    :: String
    -> (SerialT IO Int -> SerialT IO Int)
    -> Spec
associativityCheck desc t = prop desc assocCheckProp
  where
    assocCheckProp :: [Int] -> [Int] -> [Int] -> Property
    assocCheckProp xs ys zs =
        monadicIO $ do
            let xStream = SP.fromList xs
                yStream = SP.fromList ys
                zStream = SP.fromList zs
            infixAssocstream <-
                run $ SP.toList $ t $ xStream `serial` yStream `serial` zStream
            assocStream <- run $ SP.toList $ t $ xStream <> yStream <> zStream
            listEquals (==) infixAssocstream assocStream

max_length :: Int
max_length = 1000

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

#ifdef DEVBUILD
testTakeInterval :: IO Bool
testTakeInterval = do
    r <-
          S.fold (FL.tee FL.head FL.last)
        $ S.takeInterval (fromIntegral takeDropTime * 10**(-9))
        $ S.repeatM (threadDelay 1000 >> getTime Monotonic)
    checkTakeDropTime r

testDropInterval :: IO Bool
testDropInterval = do
    t0 <- getTime Monotonic
    mt1 <-
          S.fold FL.head
        $ S.dropInterval (fromIntegral takeDropTime * 10**(-9))
        $ S.repeatM (threadDelay 1000 >> getTime Monotonic)
    checkTakeDropTime (Just t0, mt1)
#endif

unfold :: Property
unfold = monadicIO $ do
    a <- pick $ choose (0, max_length `div` 2)
    b <- pick $ choose (0, max_length)
    let unf = UF.second b UF.enumerateFromToIntegral
    ls <- toList $ S.unfold unf a
    return $ ls == [a..b]

testFromCallback :: IO Int
testFromCallback = do
    ref <- newIORef Nothing
    let stream =
            S.parList (S.eager True)
                [ fmap Just (S.fromCallback (setCallback ref))
                , runCallback ref
                ]
    S.fold FL.sum $ fmap fromJust $ S.takeWhile isJust stream

    where

    setCallback ref cb = do
        writeIORef ref (Just cb)

    runCallback ref = S.fromEffect $ do
        cb <-
              S.repeatM (readIORef ref)
                & IS.delayPost 0.1
                & S.mapMaybe id
                & S.fold FL.head

        S.fromList [1..100]
            & IS.delayPost 0.001
            & drainMapM (fromJust cb)
        threadDelay 100000
        return Nothing

foldIterateM :: Property
foldIterateM =
    forAll (listOf (chooseInt (0, max_length))) $ \lst -> monadicIO $ do
        let s1 = Prelude.sum lst
            strm = S.fromList lst
        ms2 <-
            S.fold FL.last
                $ fmap getSum
                $ IS.foldIterateM
                      (return . FL.take 1 . FL.sconcat)
                      (return (Sum 0))
                $ fmap Sum strm
        case ms2 of
            Nothing -> assert $ s1 == 0
            Just s2 -> assert $ s1 == s2

sortBy :: Property
sortBy = forAll (listOf (chooseInt (0, max_length))) $ \lst -> monadicIO $ do
        let s1 = sort lst
        s2 <- toList $ K.toStream (K.sortBy compare $ K.fromStream $ S.fromList lst)
        assert $ s1 == s2

moduleName :: String
moduleName = "Prelude.Serial"

main :: IO ()
main = hspec
  $ H.parallel
#ifdef COVERAGE_BUILD
  $ modifyMaxSuccess (const 10)
#endif
  $ describe moduleName $ do
    let serialOps :: IsStream t => ((SerialT IO a -> t IO a) -> Spec) -> Spec
        serialOps spec = mapOps spec $ makeOps fromSerial
#ifndef COVERAGE_BUILD
            <> [("rate AvgRate 0.00000001", fromSerial . avgRate 0.00000001)]
            <> [("maxBuffer -1", fromSerial . maxBuffer (-1))]
#endif
    let toListSerial :: SerialT IO a -> IO [a]
        toListSerial = SP.toList

    describe "Runners" $ do
        -- XXX use an IORef to store and check the side effects
        it "simple serially" $
            (SP.fold FL.drain . fromSerial)
            (return (0 :: Int)) `shouldReturn` ()
        it "simple serially with IO" $
            (S.fold FL.drain)
            (S.fromEffect $ putStrLn "hello") `shouldReturn` ()

    describe "Empty" $
        it "Monoid - mempty" $
            toListSerial mempty `shouldReturn` ([] :: [Int])
        -- it "Alternative - empty" $
        --     (toListSerial empty) `shouldReturn` ([] :: [Int])
        -- it "MonadPlus - mzero" $
        --     (toListSerial mzero) `shouldReturn` ([] :: [Int])

    describe "Construction" $ do
        -- Add all the construction tests for all stream types.
        serialOps   $ prop "serially repeat" . constructWithRepeat
        serialOps   $ prop "serially repeatM" . constructWithRepeatM
        serialOps   $ prop "serially replicate" . constructWithReplicate
        serialOps   $ prop "serially replicateM" . constructWithReplicateM
        serialOps   $ prop "serially intFromThenTo" .
                            constructWithIntFromThenTo
        serialOps   $ prop "serially DoubleFromThenTo" .
                            constructWithDoubleFromThenTo
        serialOps   $ prop "serially iterate" . constructWithIterate
        -- XXX test for all types of streams
        serialOps   $ prop "serially iterateM" . constructWithIterateM
        serialOps $ prop "serially enumerate" . constructWithEnumerate id
        serialOps $ prop "serially enumerateTo" . constructWithEnumerateTo id
        serialOps $ prop "serially fromIndices" . constructWithFromIndices
        serialOps $ prop "serially fromIndicesM" . constructWithFromIndicesM
        serialOps $ prop "serially fromList" . constructWithFromList id
        serialOps $ prop "serially fromListM" . constructWithFromListM id
        serialOps $ prop "serially unfoldr" . constructWithUnfoldr id
        serialOps $ prop "serially fromPure" . constructWithFromPure id
        serialOps $ prop "serially fromEffect" . constructWithFromEffect id
        serialOps $ prop "serially cons" . constructWithCons SP.cons
        serialOps $ prop "serially consM" . constructWithConsM SP.consM id

        describe "From Generators" $ do
            prop "unfold" unfold

    describe "Simple Operations" $ serialOps simpleOps

    describe "Functor operations" $ do
        serialOps    $ functorOps (SP.fromFoldable) "serially" (==)
        serialOps    $ functorOps folded "serially folded" (==)

    describe "Monoid operations" $ do
        serialOps $ monoidOps "serially" mempty (==)

    describe "Serial loops" $ loops fromSerial id reverse

    describe "Bind and Monoidal composition combinations" $ do
        -- XXX Taking a long time when serialOps is used.
        bindAndComposeSimpleOps "Serial" sortEq fromSerial
        bindAndComposeHierarchyOps "Serial" fromSerial
        serialOps $ nestTwoStreams "Serial" id id
        serialOps $ nestTwoStreamsApp "Serial" id id
        composeAndComposeSimpleSerially "Serial <> " (repeat [1..9]) fromSerial
        composeAndComposeSimpleAheadly "Serial <> " (repeat [1 .. 9]) fromSerial
        composeAndComposeSimpleWSerially
            "Serial <> "
            [[1..9], [1..9], [1,3,2,4,6,5,7,9,8], [1,3,2,4,6,5,7,9,8]]
            fromSerial

    describe "Semigroup operations" $ do
        serialOps $ semigroupOps "serially" (==)
        serialOps $ associativityCheck "serial == <>"

    describe "Applicative operations" $ do
        -- The tests using sorted equality are weaker tests
        -- We need to have stronger unit tests for all those
        -- XXX applicative with three arguments
        serialOps $ applicativeOps (SP.fromFoldable) "serially" (==)
        serialOps $ applicativeOps folded "serially folded" (==)
        serialOps $ applicativeOps1 (SP.fromFoldable) "serially" (==)
        serialOps $ applicativeOps1 (SP.fromFoldable) "serially folded" (==)

    -- XXX add tests for indexed/indexedR
    describe "Zip operations" $ do
        -- We test only the serial zip with serial streams and the parallel
        -- stream, because the rate setting in these streams can slow down
        -- zipAsync.
        serialOps   $ prop "zip monadic serially" . zipMonadic (SP.fromFoldable) (==)
        serialOps   $ prop "zip monadic serially folded" . zipMonadic folded (==)

    -- XXX add merge tests like zip tests
    -- for mergeBy, we can split a list randomly into two lists and
    -- then merge them, it should result in original list
    -- describe "Merge operations" $ do

    describe "Monad operations" $ do
        serialOps   $ prop "serially monad then" . monadThen (SP.fromFoldable) (==)
        serialOps   $ prop "serially monad then folded" . monadThen folded (==)
        serialOps   $ prop "serially monad bind" . monadBind (SP.fromFoldable) (==)
        serialOps   $ prop "serially monad bind folded"  . monadBind folded (==)

    describe "Stream transform and combine operations" $ do
        serialOps    $ transformCombineOpsCommon (SP.fromFoldable) "serially" (==)
        serialOps    $ transformCombineOpsCommon folded "serially" (==)
        serialOps    $ transformCombineOpsOrdered (SP.fromFoldable) "serially" (==)
        serialOps    $ transformCombineOpsOrdered folded "serially" (==)

#ifdef DEVBUILD
        describe "Filtering" $ do
            it "takeInterval" (testTakeInterval `shouldReturn` True)
            it "dropInterval" (testDropInterval `shouldReturn` True)
#endif

    -- Just some basic sanity tests for now
    let input = [[1,1] :: [Int],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7],[8,8]]
        mustBe g inp out =
            toList (K.toStream (K.mergeMapWith g K.fromList (K.fromList inp)))
                `shouldReturn` out
     in do
        it "concatPairsWith serial"
            $ mustBe K.append input [1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8]
        it "concatPairsWith wSerial"
            $ mustBe K.interleave input [1,5,3,7,2,6,4,8,1,5,3,7,2,6,4,8]
        it "concatPairsWith mergeBy sorted"
            $ mustBe
                (K.mergeBy compare) input [1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8]
        it "concatPairsWith mergeBy reversed"
            $ mustBe
                (K.mergeBy compare)
                (reverse input)
                [1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8]
        prop "sortBy" sortBy

    describe "Stream group and split operations" $ do
        groupSplitOps "serially"

    describe "Stream elimination operations" $ do
        serialOps    $ eliminationOps (SP.fromFoldable) "serially"
        serialOps    $ eliminationOps folded "serially folded"
        serialOps    $ eliminationOpsWord8 (SP.fromFoldable) "serially"
        serialOps    $ eliminationOpsWord8 folded "serially folded"
        serialOps $ \t ->
            prop "drainWhile (> 0)" $ \n ->
                withMaxSuccess maxTestCount $
                monadicIO $ do
                    let xs = [1..n]
                    ioRef <- run $ newIORef ([] :: [Int])
                    run $
                        drainWhile (> 0) . t $
                            SP.mapM (\a -> modifyIORef' ioRef (a :) >> return a) $
                                SP.fromList xs
                    strm <- run $ readIORef ioRef
                    listEquals (==) (reverse strm) (takeWhile (> 0) xs)

    -- XXX Add a test where we chain all transformation APIs and make sure that
    -- the state is being passed through all of them.
    describe "Stream serial elimination operations" $ do
        serialOps    $ eliminationOpsOrdered (SP.fromFoldable) "serially"
        serialOps    $ eliminationOpsOrdered folded "serially folded"

    describe "Tests for S.groupsBy" groupingOps

    describe "Tests for exceptions" $ serialOps $ exceptionOps "serially"

    describe "Composed MonadThrow serially" $ composeWithMonadThrow fromSerial

    it "fromCallback" $ testFromCallback `shouldReturn` (50*101)

    describe "Nesting" $ do
        prop "foldIterateM" foldIterateM
