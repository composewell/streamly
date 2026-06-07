{-# OPTIONS_GHC -Wno-deprecations #-}
-- XXX We are using head/tail at one place
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

-- |
-- Module      : Streamly.Test.Data.Stream.Serial
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Serial (main) where

import Control.Monad ( forM_ )
import Data.Function ( (&) )
import Data.IORef ( newIORef, readIORef, modifyIORef' )
import Data.List (sort, group, intercalate)
import Data.Word (Word8)
import Data.Semigroup (Sum(..), getSum)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.MutByteArray (Unbox)
import Test.Hspec.QuickCheck
import Test.QuickCheck
    ( Gen
    , Property
    , Arbitrary(..)
    , choose
    , forAll
    , frequency
    , listOf
    , listOf1
    , suchThat
    , vectorOf
    )
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import Test.Hspec as H

import qualified Streamly.Data.Stream as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream as IS
import qualified Streamly.Internal.Data.StreamK as K
import qualified Streamly.Data.Array as A
import qualified Streamly.Internal.Data.Parser as Parser


import Streamly.Test.Common
import Streamly.Test.Data.Stream.Serial.Common
import Control.Monad.IO.Class (MonadIO)

toList :: Monad m => S.Stream m a -> m [a]
toList = S.fold FL.toList

splitOn :: Monad m =>
    (a -> Bool) -> FL.Fold m a b -> S.Stream m a -> S.Stream m b
splitOn predicate f = IS.foldManyPost (FL.takeEndBy_ predicate f)

splitOnSuffix :: Monad m =>
    (a -> Bool) -> FL.Fold m a b -> S.Stream m a -> S.Stream m b
splitOnSuffix predicate f = S.foldMany (FL.takeEndBy_ predicate f)

splitOnSeq' :: (MonadIO m, Unbox a, Enum a, Eq a) =>
    A.Array a -> FL.Fold m a b -> S.Stream m a -> S.Stream m b
splitOnSeq' patt f = IS.foldManyPost (FL.takeEndBySeq_ patt f)

splitOnSuffixSeq' :: (MonadIO m, Unbox a, Enum a, Eq a) =>
    A.Array a -> FL.Fold m a b -> S.Stream m a -> S.Stream m b
splitOnSuffixSeq' patt f = S.foldMany (FL.takeEndBySeq_ patt f)

groupsBy :: Monad m =>
    (a -> a -> Bool) -> FL.Fold m a b -> S.Stream m a -> S.Stream m b
groupsBy cmp f m = S.catRights $ S.parseMany (Parser.groupBy cmp f) m

groupsByRolling :: Monad m =>
    (a -> a -> Bool) -> FL.Fold m a b -> S.Stream m a -> S.Stream m b
groupsByRolling cmp f m =
    S.catRights $ S.parseMany (Parser.groupByRolling cmp f) m

drainWhile :: Monad m => (a -> Bool) -> Stream m a -> m ()
drainWhile p m = S.fold FL.drain $ S.takeWhile p m

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
       forall a. (Arbitrary a, Eq a, Show a, Unbox a, Enum a)
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

    splitOnSeq_ xs pat =
        toList $ splitOnSeq' (A.fromList pat) FL.toList (S.fromList xs)

    splitOnSuffixSeq_ xs pat =
        toList $ splitOnSuffixSeq' (A.fromList pat) FL.toList (S.fromList xs)

    splitOn_ xs pat =
        toList $ splitOn (== head pat) FL.toList (S.fromList xs)

    splitOnSuffix_ xs pat =
        toList $ splitOnSuffix (== head pat) FL.toList (S.fromList xs)

    intercalateSuffix xs yss = intercalate xs yss ++ xs

    nonSepElem :: Gen a
    nonSepElem = suchThat arbitrary (/= sep)

    listWithSep :: Gen [a]
    listWithSep = listOf $ frequency [(3, arbitrary), (1, return sep)]

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
                $ \xs -> withNumTests maxTestCount $ monadicIO $ testCase xs

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
                      withNumTests maxTestCount $ monadicIO $ testCase xs

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
                $ \xss -> withNumTests maxTestCount $ monadicIO $ testCase xss

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
                      withNumTests maxTestCount $ monadicIO $ testCase xss

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
            withNumTests maxTestCount $
                monadicIO $ do
                    ys <- toList $ splitOn (== x) toListFL (S.fromList xs)
                    listEquals (==) (intercalate [x] ys) xs

    where

    listWithZeroes :: Gen [a]
    listWithZeroes = listOf $ frequency [(3, arbitrary), (1, return 0)]

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
{- HLINT ignore "Redundant ==" -}
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
    -> Spec
associativityCheck desc = prop desc assocCheckProp
  where
    assocCheckProp :: [Int] -> [Int] -> [Int] -> Property
    assocCheckProp xs ys zs =
        monadicIO $ do
            let xStream = IS.fromList xs
                yStream = IS.fromList ys
                zStream = IS.fromList zs
            leftAssoc <-
                run $ IS.toList $ IS.append (IS.append xStream yStream) zStream
            rightAssoc <-
                run $ IS.toList $ IS.append xStream (IS.append yStream zStream)
            listEquals (==) leftAssoc rightAssoc

max_length :: Int
max_length = 1000

unfold :: Property
unfold = monadicIO $ do
    a <- pick $ choose (0, max_length `div` 2)
    b <- pick $ choose (0, max_length)
    let unf = UF.supplySecond b UF.enumerateFromToIntegral
    ls <- toList $ S.unfold unf a
    return $ ls == [a..b]

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
    let toListSerial :: Stream IO a -> IO [a]
        toListSerial = S.toList

    describe "Runners" $ do
        -- XXX use an IORef to store and check the side effects
        it "simple serially" $
            IS.fold FL.drain
            (IS.fromPure (0 :: Int)) `shouldReturn` ()
        it "simple serially with IO" $
            S.fold FL.drain
            (S.fromEffect $ putStrLn "hello") `shouldReturn` ()

    describe "Empty" $
        it "Monoid - mempty" $
            toListSerial S.nil `shouldReturn` ([] :: [Int])

    describe "Construction" $ do
        -- Add all the construction tests for all stream types.
        prop "serially repeat" constructWithRepeat
        prop "serially repeatM" constructWithRepeatM
        prop "serially replicate" constructWithReplicate
        prop "serially replicateM" constructWithReplicateM
        prop "serially intFromThenTo" constructWithIntFromThenTo
        prop "serially DoubleFromThenTo" constructWithDoubleFromThenTo
        prop "serially iterate" constructWithIterate
        -- XXX test for all types of streams
        prop "serially iterateM" constructWithIterateM
        prop "serially enumerate" (constructWithEnumerate id)
        prop "serially enumerateTo" (constructWithEnumerateTo id)
        prop "serially fromIndices" constructWithFromIndices
        prop "serially fromIndicesM" constructWithFromIndicesM
        prop "serially fromList" (constructWithFromList id)
        prop "serially fromListM" (constructWithFromListM id)
        prop "serially unfoldr" (constructWithUnfoldr id)
        prop "serially fromPure" (constructWithFromPure id)
        prop "serially fromEffect" (constructWithFromEffect id)

        describe "From Generators" $ do
            prop "unfold" unfold

    describe "Simple Operations" simpleOps

    describe "Functor operations" $ do
        functorOps IS.fromFoldable "serially" (==)
        functorOps folded "serially folded" (==)

    describe "Monoid operations" $ do
        monoidOps "serially" S.nil (==)

    describe "Serial loops" $ loops id id reverse

    describe "Bind and Monoidal composition combinations" $ do
        -- XXX Taking a long time when serialOps is used.
        bindAndComposeSimpleOps "Serial" sortEq
        bindAndComposeHierarchyOps "Serial"
        nestTwoStreams "Serial" id id
        nestTwoStreamsApp "Serial" id id
        composeAndComposeSimpleSerially "Serial <> " (repeat [1..9])
        {-
        -- XXX Move this to WSerial type test or the wserial/interleave op test
        composeAndComposeSimpleWSerially
            "Serial <> "
            [[1..9], [1..9], [1,3,2,4,6,5,7,9,8], [1,3,2,4,6,5,7,9,8]]
            fromSerial
        -}

    describe "Semigroup operations" $ do
        semigroupOps "serially" (==)
        associativityCheck "serial == <>"

    describe "Applicative operations" $ do
        -- The tests using sorted equality are weaker tests
        -- We need to have stronger unit tests for all those
        -- XXX applicative with three arguments
        applicativeOps IS.fromFoldable "serially" (==)
        applicativeOps folded "serially folded" (==)
        applicativeOps1 IS.fromFoldable "serially" (==)
        applicativeOps1 IS.fromFoldable "serially folded" (==)

    -- XXX add tests for indexed/indexedR
    describe "Zip operations" $ do
        -- We test only the serial zip with serial streams and the parallel
        -- stream, because the rate setting in these streams can slow down
        -- zipAsync.
        prop "zip monadic serially" (zipMonadic IS.fromFoldable (==))
        prop "zip monadic serially folded" (zipMonadic folded (==))

    -- XXX add merge tests like zip tests
    -- for mergeBy, we can split a list randomly into two lists and
    -- then merge them, it should result in original list
    -- describe "Merge operations" $ do

    describe "Monad operations" $ do
        prop "serially monad then" (monadThen IS.fromFoldable (==))
        prop "serially monad then folded" (monadThen folded (==))
        prop "serially monad bind" (monadBind IS.fromFoldable (==))
        prop "serially monad bind folded" (monadBind folded (==))

    describe "Stream transform and combine operations" $ do
        transformCombineOpsCommon IS.fromFoldable "serially" (==)
        transformCombineOpsCommon folded "serially" (==)
        transformCombineOpsOrdered IS.fromFoldable "serially" (==)
        transformCombineOpsOrdered folded "serially" (==)

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
        eliminationOps IS.fromFoldable "serially"
        eliminationOps folded "serially folded"
        eliminationOpsWord8 IS.fromFoldable "serially"
        eliminationOpsWord8 folded "serially folded"
        prop "drainWhile (> 0)" $ \n ->
            withNumTests maxTestCount $
            monadicIO $ do
                let xs = [1..n]
                ioRef <- run $ newIORef ([] :: [Int])
                run $
                    drainWhile (> 0) $
                        S.mapM (\a -> modifyIORef' ioRef (a :) >> return a) $
                            S.fromList xs
                strm <- run $ readIORef ioRef
                listEquals (==) (reverse strm) (takeWhile (> 0) xs)

    -- XXX Add a test where we chain all transformation APIs and make sure that
    -- the state is being passed through all of them.
    describe "Stream serial elimination operations" $ do
        eliminationOpsOrdered IS.fromFoldable "serially"
        eliminationOpsOrdered folded "serially folded"

    describe "Tests for S.groupsBy" groupingOps

    describe "Tests for exceptions" $ exceptionOps "serially"

    describe "Composed MonadThrow serially" composeWithMonadThrow

    describe "Nesting" $ do
        prop "foldIterateM" foldIterateM
