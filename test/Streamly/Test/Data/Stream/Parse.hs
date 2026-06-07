-- XXX We are using head/tail at one place
{-# OPTIONS_GHC -Wno-missing-signatures #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

-- |
-- Module      : Streamly.Test.Data.Stream.Parse
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Parse (main) where

import Control.Monad ( forM_ )
import Control.Monad.IO.Class (MonadIO)
import Data.Functor.Identity (Identity)
import Data.List (group, intercalate)
import Data.Semigroup (Sum(..), getSum)
import Data.Word (Word8)
import Streamly.Internal.Data.Array (Array)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.MutByteArray (Unbox)
import Streamly.Internal.Data.Stream (Stream)
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
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Parser as Parser

import Test.Hspec as H
import Test.Hspec.QuickCheck

import Streamly.Test.Common

toList :: Monad m => Stream m a -> m [a]
toList = Stream.toList

-- XXX There are takeEndBy_ tests in Data.Fold module as well, need to
-- deduplicate.
-- XXX Where are the tests for "takeEndBy"?
splitOn :: Monad m =>
    (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
splitOn predicate f = Stream.foldManyPost (Fold.takeEndBy_ predicate f)

splitOnSuffix :: Monad m =>
    (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
splitOnSuffix predicate f = Stream.foldMany (Fold.takeEndBy_ predicate f)

-- XXX Where are the tests for "takeEndBySeq"?
splitOnSeqFold :: (MonadIO m, Unbox a, Enum a, Eq a) =>
   Array.Array a -> Fold m a b -> Stream m a -> Stream m b
splitOnSeqFold patt f = Stream.foldManyPost (Fold.takeEndBySeq_ patt f)

splitOnSeqStream :: (MonadIO m, Unbox a, Enum a, Eq a) =>
   Array.Array a -> Fold m a b -> Stream m a -> Stream m b
splitOnSeqStream = Stream.splitSepBySeq_

splitOnSuffixSeqFold :: (MonadIO m, Unbox a, Enum a, Eq a) =>
   Array.Array a -> Fold m a b -> Stream m a -> Stream m b
splitOnSuffixSeqFold patt f = Stream.foldMany (Fold.takeEndBySeq_ patt f)

-- XXX Where are the tests for Stream.splitOnSuffixSeq True ?
splitOnSuffixSeqStream :: (MonadIO m, Unbox a, Enum a, Eq a) =>
   Array.Array a -> Fold m a b -> Stream m a -> Stream m b
splitOnSuffixSeqStream = Stream.splitOnSuffixSeq False

groupsBy :: Monad m =>
    (a -> a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
groupsBy cmp f m = Stream.catRights $ Stream.parseMany (Parser.groupBy cmp f) m

groupsByRolling :: Monad m =>
    (a -> a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
groupsByRolling cmp f m =
   Stream.catRights $ Stream.parseMany (Parser.groupByRolling cmp f) m

{-
drainWhile :: Monad m => (a -> Bool) -> Stream m a -> m ()
drainWhile p m = Stream.fold Fold.drain $ Stream.takeWhile p m
-}

splitOnSeq ::
    (Array Char -> Fold IO a [a] -> Stream IO Char -> Stream IO String)
    -> Spec
splitOnSeq op = do
    describe "Tests for splitOnSeq" $ do
        -- Empty pattern case
        it "splitOnSeq_ \"\" \"\" = []"
          $ splitOnSeq_ "" "" `shouldReturn` []

        -- Single element pattern cases
        it "splitOnSeq_ \"x\" \"\" = []"
          $ splitOnSeq_ "x" "" `shouldReturn` []
        it "splitOnSeq_ \"x\" \"hello\" = [\"hello\"]"
          $ splitOnSeq_ "x" "hello" `shouldReturn` ["hello"]
        it "splitOnSeq_ \"h\" \"hello\" = [\"\", \"ello\"]"
          $ splitOnSeq_ "h" "hello" `shouldReturn` ["", "ello"]
        it "splitOnSeq_ \"e\" \"hello\" = [\"h\", \"llo\"]"
          $ splitOnSeq_ "e" "hello" `shouldReturn` ["h", "llo"]
        it "splitOnSeq_ \"l\" \"hello\" = [\"he\", \"\", \"o\"]"
          $ splitOnSeq_ "l" "hello" `shouldReturn` ["he", "", "o"]
        it "splitOnSeq_ \"o\" \"hello\" = [\"hell\", \"\"]"
          $ splitOnSeq_ "o" "hello" `shouldReturn` ["hell", ""]

        -- multi-element pattern fitting in a Word
        it "splitOnSeq_ \"he\" \"\" = []"
          $ splitOnSeq_ "he" "" `shouldReturn` []
        it "splitOnSeq_ \"he\" \"hello\" = [\"\", \"llo\"]"
          $ splitOnSeq_ "he" "hello" `shouldReturn` ["", "llo"]
        it "splitOnSeq_ \"ll\" \"hello\" = [\"he\", \"o\"]"
          $ splitOnSeq_ "ll" "hello" `shouldReturn` ["he", "o"]
        it "splitOnSeq_ \"lo\" \"hello\" = [\"hel\", \"\"]"
          $ splitOnSeq_ "lo" "hello" `shouldReturn` ["hel", ""]

        -- multi-element pattern - Rabin-Karp cases
        it "splitOnSeq_ \"hello\" \"\" = []"
          $ splitOnSeq_ "hello" "" `shouldReturn` []
        it "splitOnSeq_ \"hel\" \"hello\" = [\"\", \"lo\"]"
          $ splitOnSeq_ "hel" "hello" `shouldReturn` ["", "lo"]
        it "splitOnSeq_ \"ell\" \"hello\" = [\"h\", \"o\"]"
          $ splitOnSeq_ "ell" "hello" `shouldReturn` ["h", "o"]
        it "splitOnSeq_ \"llo\" \"hello\" = [\"he\", \"\"]"
          $ splitOnSeq_ "llo" "hello" `shouldReturn` ["he", ""]
        it "splitOnSeq_ \"hello\" \"hello\" = [\"\", \"\"]"
          $ splitOnSeq_ "hello" "hello" `shouldReturn` ["", ""]

    where

    splitOnSeq_ pat xs = toList $
        op (Array.fromList pat) Fold.toList (Stream.fromList xs)

splitOnSuffixSeq ::
    (Array Char -> Fold IO a [a] -> Stream IO Char -> Stream IO String)
    -> Spec
splitOnSuffixSeq op = do
    describe "Tests for splitOnSuffixSeq" $ do
        -- Empty pattern case

        -- Single element pattern cases
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

        -- multi-element pattern fitting in a Word
        it "splitSuffixOn_ \"he\" \"hello\" = [\"\", \"llo\"]"
          $ splitSuffixOn_ "he" "hello" `shouldReturn` ["", "llo"]
        it "splitSuffixOn_ \"el\" \"hello\" = [\"h\", \"lo\"]"
          $ splitSuffixOn_ "el" "hello" `shouldReturn` ["h", "lo"]
        it "splitSuffixOn_ \"lo\" \"hello\" = [\"hel\"]"
          $ splitSuffixOn_ "lo" "hello" `shouldReturn` ["hel"]

        -- multi-element pattern - Rabin-Karp cases
        it "splitSuffixOn_ \"hello\" \"\" = []"
          $ splitSuffixOn_ "hello" "" `shouldReturn` []
        it "splitSuffixOn_ \"hel\" \"hello\" = [\"\", \"lo\"]"
          $ splitSuffixOn_ "hel" "hello" `shouldReturn` ["", "lo"]
        it "splitSuffixOn_ \"ell\" \"hello\" = [\"h\", \"o\"]"
          $ splitSuffixOn_ "ell" "hello" `shouldReturn` ["h", "o"]
        it "splitSuffixOn_ \"llo\" \"hello\" = [\"he\"]"
          $ splitSuffixOn_ "llo" "hello" `shouldReturn` ["he"]
        it "splitSuffixOn_ \"hello\" \"hello\" = [\"\", \"\"]"
          $ splitSuffixOn_ "hello" "hello" `shouldReturn` [""]

    where

    splitSuffixOn_ pat xs = toList $
        op (Array.fromList pat) Fold.toList (Stream.fromList xs)

intercalateSuffix xs yss = intercalate xs yss ++ xs

nonSepElem :: (Arbitrary a, Eq a) => a -> Gen a
nonSepElem sep = suchThat arbitrary (/= sep)

listWithSep :: Arbitrary a => a -> Gen [a]
listWithSep sep = listOf $ frequency [(3, arbitrary), (1, return sep)]

listWithoutSep :: (Arbitrary a, Eq a) => a -> Gen [a]
listWithoutSep sep = vectorOf 4 (nonSepElem sep)

listsWithoutSep :: (Arbitrary a, Eq a) => a -> Gen [[a]]
listsWithoutSep sep = listOf (listWithoutSep sep)

listsWithoutSep1 :: (Arbitrary a, Eq a) => a -> Gen [[a]]
listsWithoutSep1 sep = listOf1 (listWithoutSep sep)

intercalateSplitEqId sep splitter lIntercalater sIntercalater i =
    let name =
            "intercalater . splitter == id ("
                <> show i <> " element separator)"
     in prop name
            $ forAll (listWithSep sep)
            $ \xs -> withNumTests maxTestCount $ monadicIO $ testCase xs

    where

    testCase xs = do
        ys <- splitter xs (replicate i sep)
        szs <-
            toList
                $ sIntercalater (replicate i sep) Unfold.fromList
                $ Stream.fromList ys
        let lzs = lIntercalater (replicate i sep) ys
        listEquals (==) szs xs
        listEquals (==) lzs xs

intercalateSplitEqIdNoSepEnd sep splitter lIntercalater sIntercalater i =
    let name =
            "intercalater . splitter . (++ [x \\= sep]) == id ("
                <> show i <> " element separator)"
     in prop name
            $ forAll ((,) <$> listWithSep sep <*> nonSepElem sep)
            $ \(xs_, nonSep) -> do
                  let xs = xs_ ++ [nonSep]
                  withNumTests maxTestCount $ monadicIO $ testCase xs

    where

    testCase xs = do
        ys <- splitter xs (replicate i sep)
        szs <-
            toList
                $ sIntercalater (replicate i sep) Unfold.fromList
                $ Stream.fromList ys
        let lzs = lIntercalater (replicate i sep) ys
        listEquals (==) szs xs
        listEquals (==) lzs xs

concatSplitIntercalateEqConcat sep splitter lIntercalater sIntercalater i =
    let name =
            "concat . splitter .Stream.intercalater == "
                <> "concat ("
                <> show i <> " element separator/possibly empty list)"
     in prop name
            $ forAll (listsWithoutSep sep)
            $ \xss -> withNumTests maxTestCount $ monadicIO $ testCase xss

    where

    testCase xss = do
        let lxs = lIntercalater (replicate i sep) xss
        lys <- splitter lxs (replicate i sep)
        sxs <-
            toList
                $ sIntercalater (replicate i sep) Unfold.fromList
                $ Stream.fromList xss
        sys <- splitter sxs (replicate i sep)
        listEquals (==) (concat lys) (concat xss)
        listEquals (==) (concat sys) (concat xss)

splitIntercalateEqId sep splitter lIntercalater sIntercalater =
    let name =
            "splitter . intercalater == id"
                <> " (exclusive separator/non-empty list)"
     in prop name
            $ forAll (listsWithoutSep1 sep)
            $ \xss -> do
                  withNumTests maxTestCount $ monadicIO $ testCase xss

    where

    testCase xss = do
        let lxs = lIntercalater [sep] xss
        lys <- splitter lxs [sep]
        sxs <- toList
                $ sIntercalater [sep] Unfold.fromList
                $ Stream.fromList xss
        sys <- splitter sxs [sep]
        listEquals (==) lys xss
        listEquals (==) sys xss

splitterProperties ::
       forall a. (Arbitrary a, Eq a, Show a)
    => a
    -> String
    -> Spec
splitterProperties sep desc = do
    describe (desc <> " splitOn")
        $ do

            intercalateSplitEqId
                sep splitOn_ intercalate Stream.unfoldEachSepBySeq 1

            concatSplitIntercalateEqConcat
                sep splitOn_ intercalate Stream.unfoldEachSepBySeq 1

            -- Exclusive case
            splitIntercalateEqId
                sep splitOn_ intercalate Stream.unfoldEachSepBySeq

    describe (desc <> " splitOnSuffix")
        $ do

            intercalateSplitEqIdNoSepEnd
                sep splitOnSuffix_ intercalate Stream.unfoldEachSepBySeq 1

            concatSplitIntercalateEqConcat
                sep splitOnSuffix_ intercalateSuffix Stream.unfoldEachEndBySeq 1

            -- Exclusive case
            splitIntercalateEqId
                sep splitOnSuffix_ intercalateSuffix Stream.unfoldEachEndBySeq

    where

    splitOn_ xs pat =
        toList $ splitOn (== head pat) Fold.toList (Stream.fromList xs)

    splitOnSuffix_ xs pat =
        toList $ splitOnSuffix (== head pat) Fold.toList (Stream.fromList xs)

seqSplitterProperties ::
       forall a. (Arbitrary a, Eq a, Show a, Unbox a, Enum a)
    => a
    -> String
    -> Spec
seqSplitterProperties sep desc = do
    describe (desc <> " splitOnSeq fold") (splitOnSeqWith splitOnSeq_)
    describe (desc <> " splitOnSeq stream") (splitOnSeqWith splitOnSeqStream_)
    describe (desc <> " splitOnSuffixSeq fold")
        (splitOnSuffixSeqWith splitOnSuffixSeq_)
    describe (desc <> " splitOnSuffixSeq stream")
        (splitOnSuffixSeqWith splitOnSuffixSeqStream_)

    where

    splitOnSeq_ xs pat =
        toList $ splitOnSeqFold (Array.fromList pat) Fold.toList (Stream.fromList xs)

    splitOnSeqStream_ xs pat =
        toList $ splitOnSeqStream (Array.fromList pat) Fold.toList (Stream.fromList xs)

    splitOnSuffixSeq_ xs pat =
        toList $ splitOnSuffixSeqFold (Array.fromList pat) Fold.toList (Stream.fromList xs)

    splitOnSuffixSeqStream_ xs pat =
        toList $ splitOnSuffixSeqStream (Array.fromList pat) Fold.toList (Stream.fromList xs)

    splitOnSeqWith op = do
        forM_ [0, 1, 2, 4]
            $ intercalateSplitEqId sep op intercalate Stream.unfoldEachSepBySeq

        forM_ [0, 1, 2, 4]
            $ concatSplitIntercalateEqConcat
                sep op intercalate Stream.unfoldEachSepBySeq

        -- Exclusive case
        splitIntercalateEqId sep op intercalate Stream.unfoldEachSepBySeq

    splitOnSuffixSeqWith op = do
        forM_ [0, 1, 2, 4]
            $ intercalateSplitEqIdNoSepEnd
                sep op intercalate Stream.unfoldEachSepBySeq

        forM_ [0, 1, 2, 4]
            $ concatSplitIntercalateEqConcat
                  sep op intercalateSuffix Stream.unfoldEachEndBySeq

        -- Exclusive case
        splitIntercalateEqId
            sep op intercalateSuffix Stream.unfoldEachEndBySeq

intercalateSplitOnId ::
       forall a. (Arbitrary a, Eq a, Show a, Num a) =>
       a -> String -> Spec
intercalateSplitOnId x desc =
    prop (desc <> " intercalate [x] . splitOn (== x) == id") $
        forAll listWithZeroes $ \xs -> do
            withNumTests maxTestCount $
                monadicIO $ do
                    ys <- toList $ splitOn (== x) Fold.toList (Stream.fromList xs)
                    listEquals (==) (intercalate [x] ys) xs

    where

    listWithZeroes :: Gen [a]
    listWithZeroes = listOf $ frequency [(3, arbitrary), (1, return 0)]

groupSplitOps :: String -> Spec
groupSplitOps desc = do
    -- splitting

    -- The foldManyPost implementation on an empty stream produces a single
    -- value. The behaviour of foldManyPost implementation and the direct stream
    -- implementation is not different.
    -- splitOnSeq splitOnSeqFold

    splitOnSeq splitOnSeqStream
    splitOnSuffixSeq splitOnSuffixSeqFold

    -- XXX there are no tests for withSep = True option
    splitOnSuffixSeq splitOnSuffixSeqStream
    -- Some ad-hoc tests
    it "splitEndBySeq word hash cases" $ do
        let f sep input result =
                Stream.toList
                    ( Stream.splitEndBySeq (Array.fromList sep) Fold.toList
                    $ Stream.fromList input
                    ) `shouldReturn` result

        f "ab" "a" ["a"]
        f "ab" "ab" ["ab"]
        f "ab" "aba" ["ab","a"]
        f "ab" "abab" ["ab","ab"]
        f "ab" "abc" ["ab","c"]
        f "ab" "xab" ["xab"]
        f "" "" []
        f "." "" []
        f ".." "" []
        f "..." "" []
        f "" "a...b" ["a",".",".",".","b"]
        f "." "a...b" ["a.",".",".","b"]
        f ".." "a...b" ["a..",".b"]
        f "..." "a...b" ["a...","b"]
        f "." "abc" ["abc"]
        f ".." "abc" ["abc"]
        f "..." "abc" ["abc"]
        f "." "." ["."]
        f ".." ".." [".."]
        f "..." "..." ["..."]
        f "." ".a" [".","a"]
        f "." "a." ["a."]

    it "splitEndBySeq_ word hash cases" $ do
        let f sep input result =
                Stream.toList
                    ( Stream.splitEndBySeq_ (Array.fromList sep) Fold.toList
                    $ Stream.fromList input
                    ) `shouldReturn` result
        f "" "" []
        f "." "" []
        f ".." "" []
        f "..." "" []
        f "" "a...b" ["a",".",".",".","b"]
        f "." "a...b" ["a","","","b"]
        f ".." "a...b" ["a",".b"]
        f "..." "a...b" ["a","b"]
        f "." "abc" ["abc"]
        f ".." "abc" ["abc"]
        f "..." "abc" ["abc"]
        f "." "." [""]
        f ".." ".." [""]
        f "..." "..." [""]
        f "." ".a" ["","a"]
        f "." "a." ["a"]

    it "splitSepBySeq_ word hash cases" $ do
        let f sep input result =
                Stream.toList
                    ( Stream.splitSepBySeq_ (Array.fromList sep) Fold.toList
                    $ Stream.fromList input
                    ) `shouldReturn` result
        f "" "" []
        f "." "" []
        f ".." "" []
        f "..." "" []
        f "" "a...b" ["a",".",".",".","b"]
        f "." "a...b" ["a","","","b"]
        f ".." "a...b" ["a",".b"]
        f "..." "a...b" ["a","b"]
        f "." "abc" ["abc"]
        f ".." "abc" ["abc"]
        f "..." "abc" ["abc"]
        f "." "." ["",""]
        f ".." ".." ["",""]
        f "..." "..." ["",""]
        f "." ".a" ["","a"]
        f "." "a." ["a",""]

    let takeEndBySeq pat input result =
                Stream.toList
                    ( Stream.takeEndBySeq (Array.fromList pat)
                    $ Stream.fromList input
                    ) `shouldReturn` result
    it "takeEndBySeq empty pattern" $ do
        let f = takeEndBySeq ""
        f "" ""
        f "abcd" ""
    it "takeEndBySeq single element pattern" $ do
        let f = takeEndBySeq "a"
        f "" ""
        f "a" "a"
        f "ab" "a"
        f "xa" "xa"
        f "xab" "xa"
    it "takeEndBySeq word hash cases" $ do
        let f = takeEndBySeq "ab"
        f "" ""
        f "a" "a"
        f "ab" "ab"
        f "abc" "ab"
        f "aba" "ab"
        f "abab" "ab"
        f "x" "x"
        f "xa" "xa"
        f "xab" "xab"
        f "xabc" "xab"
    it "takeEndBySeq karp-rabin cases" $ do
        let f = takeEndBySeq "abc"
        f "" ""
        f "a" "a"
        f "ab" "ab"
        f "abc" "abc"
        f "abcd" "abc"
        f "abca" "abc"
        f "abcabc" "abc"
        f "x" "x"
        f "xa" "xa"
        f "xab" "xab"
        f "xabc" "xabc"
        f "xabcd" "xabc"

    -- splitting properties
    splitterProperties (0 :: Int) desc
    splitterProperties (0 :: Word8) desc

    seqSplitterProperties (0 :: Int) desc
    seqSplitterProperties (0 :: Word8) desc

    intercalateSplitOnId (0 :: Int) desc
    intercalateSplitOnId (0 :: Word8) desc

-- |
-- After grouping (and folding) Int stream using @<@ operation,
-- the first @Int@ of every @[Int]@ in the @[Int]@ stream should be the minimum.
testGroupsBy :: Property
testGroupsBy =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
            r <- run $ Stream.fold (Fold.all (\ls ->
                case ls of
                    [] -> True
                    (x:_) -> x == minimum ls))
                $ groupsBy (<) Fold.toList
                $ Stream.fromList vec
            assert r

testGroups :: Property
testGroups =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
            r <- toList $ groupsBy (==) Fold.toList $ Stream.fromList vec
            assert $ r == group vec

testGroupsByRolling :: Property
testGroupsByRolling =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec -> monadicIO $ do
            r <- toList $ groupsByRolling (==) Fold.toList $ Stream.fromList vec
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
decreasing xs = and $ zipWith (<=) (tail xs) xs

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
                    $ groupsBy (<) Fold.toList
                    $ Stream.fromList vec
            assert $ decreasing a

groupingOps :: Spec
groupingOps = do
    prop "groupsBy" testGroupsBy
    prop "Stream.groups = groups" testGroups
    prop "Stream.groupsByRolling = groups" testGroupsByRolling
    prop "testGroupsBySep" testGroupsBySep

{-
-- XXX to be fixed
associativityCheck
    :: String
    -> (Stream IO Int -> Stream IO Int)
    -> Spec
associativityCheck desc t = prop desc assocCheckProp
  where
    assocCheckProp :: [Int] -> [Int] -> [Int] -> Property
    assocCheckProp xs ys zs =
        monadicIO $ do
            let xStream = Stream.fromList xs
                yStream = Stream.fromList ys
                zStream = Stream.fromList zs
            infixAssocstream <-
                run $ Stream.toList $ t $ xStream `Stream.append` yStream `Stream.append` zStream
            assocStream <- run $ Stream.toList $ t $ xStream <> yStream <> zStream
            listEquals (==) infixAssocstream assocStream
-}

maxStreamLen :: Int
maxStreamLen = 1000

-- Coverage build takes too long with default number of tests
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

max_length :: Int
max_length = 1000

foldIterateM :: Property
foldIterateM =
    forAll (listOf (chooseInt (0, max_length))) $ \lst -> monadicIO $ do
        let s1 = Prelude.sum lst
            strm =Stream.fromList lst
        ms2 <-
           Stream.fold Fold.latest
                $ fmap getSum
                $ Stream.foldIterateM
                      (return . Fold.take 1 . Fold.sconcat)
                      (return (Sum 0))
                $ fmap Sum strm
        case ms2 of
            Nothing -> assert $ s1 == 0
            Just s2 -> assert $ s1 == s2

-------------------------------------------------------------------------------
-- groupsRollingBy
-------------------------------------------------------------------------------

testGroupsRollingBy :: Expectation
testGroupsRollingBy =
    Stream.toList
        (Stream.groupsRollingBy (==) Fold.toList
            (Stream.fromList [1, 1, 2, 2, 3, 3 :: Int]))
        `shouldReturn` [[1, 1], [2, 2], [3, 3]]

testGroupsRollingByEmpty :: Expectation
testGroupsRollingByEmpty =
    Stream.toList
        (Stream.groupsRollingBy (==) Fold.toList
            (Stream.fromList ([] :: [Int])))
        `shouldReturn` []

testGroupsRollingByNonEq :: Expectation
testGroupsRollingByNonEq =
    Stream.toList
        (Stream.groupsRollingBy (<) Fold.toList
            (Stream.fromList [1, 2, 3, 1, 2 :: Int]))
        `shouldReturn` [[1, 2, 3], [1, 2]]

-------------------------------------------------------------------------------
-- groupsWhile
-------------------------------------------------------------------------------

testGroupsWhile :: Expectation
testGroupsWhile =
    Stream.toList
        (Stream.groupsWhile (==) Fold.toList (Stream.fromList [1, 1, 2, 2, 3 :: Int]))
        `shouldReturn` [[1, 1], [2, 2], [3]]

testGroupsWhileEmpty :: Expectation
testGroupsWhileEmpty =
    Stream.toList
        (Stream.groupsWhile (==) Fold.toList (Stream.fromList ([] :: [Int])))
        `shouldReturn` []

-------------------------------------------------------------------------------
-- wordsBy
-------------------------------------------------------------------------------

testWordsBy :: Expectation
testWordsBy =
    Stream.toList
        (Stream.wordsBy (== '.') Fold.toList (Stream.fromList "a.b"))
        `shouldReturn` ["a", "b"]

testWordsByMultiSep :: Expectation
testWordsByMultiSep =
    Stream.toList
        (Stream.wordsBy (== '.') Fold.toList (Stream.fromList ".a..b."))
        `shouldReturn` ["a", "b"]

testWordsByEmpty :: Expectation
testWordsByEmpty =
    Stream.toList
        (Stream.wordsBy (== '.') Fold.toList (Stream.fromList ""))
        `shouldReturn` []

-------------------------------------------------------------------------------
-- parseMany / parseManyPos
-------------------------------------------------------------------------------

testParseMany :: Expectation
testParseMany = do
    xs <- Stream.toList $ Stream.catRights $
              Stream.parseMany
                  (Parser.takeBetween 2 2 Fold.toList)
                  (Stream.fromList [1 .. 6 :: Int])
    xs `shouldBe` [[1, 2], [3, 4], [5, 6]]

testParseManyPos :: Expectation
testParseManyPos = do
    xs <- Stream.toList $ Stream.catRights $
              Stream.parseManyPos
                  (Parser.takeBetween 2 2 Fold.toList)
                  (Stream.fromList [1 .. 4 :: Int])
    xs `shouldBe` [[1, 2], [3, 4]]

-------------------------------------------------------------------------------
-- parseIterate / parseIteratePos
-------------------------------------------------------------------------------

testParseIterate :: Expectation
testParseIterate = do
    xs <- Stream.toList $ Stream.catRights $
              Stream.parseIterate
                  (\_ -> Parser.takeBetween 2 2 Fold.toList)
                  ([] :: [Int])
                  (Stream.fromList [1 .. 6 :: Int])
    xs `shouldBe` [[1, 2], [3, 4], [5, 6]]

testParseIteratePos :: Expectation
testParseIteratePos = do
    xs <- Stream.toList $ Stream.catRights $
              Stream.parseIteratePos
                  (\_ -> Parser.takeBetween 2 2 Fold.toList)
                  ([] :: [Int])
                  (Stream.fromList [1 .. 4 :: Int])
    xs `shouldBe` [[1, 2], [3, 4]]

-------------------------------------------------------------------------------
-- splitSepByOneOf
-------------------------------------------------------------------------------

testSplitSepByOneOfSingle :: Expectation
testSplitSepByOneOfSingle = do
    let pats = map Array.fromList [".", ","] :: [Array Char]
    xs <- Stream.toList $
              Stream.splitSepByOneOf pats Fold.toList
                  (Stream.fromList "a.b,c")
    xs `shouldBe` ["a", "b", "c"]

testSplitSepByOneOfSeq :: Expectation
testSplitSepByOneOfSeq = do
    let pats = [Array.fromList ".."] :: [Array Char]
    xs <- Stream.toList $
              Stream.splitSepByOneOf pats Fold.toList
                  (Stream.fromList "a..b..c")
    xs `shouldBe` ["a", "b", "c"]

testSplitSepByOneOfEmpty :: Expectation
testSplitSepByOneOfEmpty = do
    let pats = [Array.fromList "."] :: [Array Char]
    xs <- Stream.toList $
              Stream.splitSepByOneOf pats Fold.toList
                  (Stream.fromList "")
    xs `shouldBe` []

-------------------------------------------------------------------------------
-- splitInnerBy / splitInnerBySuffix
-------------------------------------------------------------------------------

-- splitter splits at '.' for String chunks
innerSplitter :: String -> IO (String, Maybe String)
innerSplitter xs =
    let (a, b) = span (/= '.') xs
    in return $ if null b then (a, Nothing) else (a, Just (tail b))

innerJoiner :: String -> String -> IO String
innerJoiner a b = return (a ++ b)

testSplitInnerBy :: Expectation
testSplitInnerBy = do
    xs <- Stream.toList $
              Stream.splitInnerBy innerSplitter innerJoiner
                  (Stream.fromList ["a.b", "c"])
    xs `shouldBe` ["a", "bc"]

testSplitInnerBySuffix :: Expectation
testSplitInnerBySuffix = do
    xs <- Stream.toList $
              Stream.splitInnerBySuffix null innerSplitter innerJoiner
                  (Stream.fromList ["a.", "b."])
    xs `shouldBe` ["a", "b"]

testSplitInnerBySuffixNoTrail :: Expectation
testSplitInnerBySuffixNoTrail = do
    xs <- Stream.toList $
              Stream.splitInnerBySuffix null innerSplitter innerJoiner
                  (Stream.fromList ["a.", "bc"])
    xs `shouldBe` ["a", "bc"]

-------------------------------------------------------------------------------
-- dropCommonPrefixBy
-------------------------------------------------------------------------------

testDropCommonPrefixByMatch :: Expectation
testDropCommonPrefixByMatch = do
    xs <- Stream.toList $
              Stream.dropCommonPrefixBy (==)
                  (Stream.fromList [1, 2 :: Int])
                  (Stream.fromList [1, 2, 3, 4])
    xs `shouldBe` [3, 4]

testDropCommonPrefixByMismatch :: Expectation
testDropCommonPrefixByMismatch = do
    xs <- Stream.toList $
              Stream.dropCommonPrefixBy (==)
                  (Stream.fromList [1, 3 :: Int])
                  (Stream.fromList [1, 2, 3, 4])
    xs `shouldBe` [2, 3, 4]

testDropCommonPrefixByEmpty :: Expectation
testDropCommonPrefixByEmpty = do
    xs <- Stream.toList $
              Stream.dropCommonPrefixBy (==)
                  (Stream.fromList ([] :: [Int]))
                  (Stream.fromList [1, 2, 3])
    xs `shouldBe` [1, 2, 3]

-------------------------------------------------------------------------------
-- dropPrefix
-------------------------------------------------------------------------------

testDropPrefixMatch :: Expectation
testDropPrefixMatch = do
    xs <- Stream.toList $
              Stream.dropPrefix
                  (Stream.fromList [1, 2] :: Stream Identity Int)
                  (Stream.fromList [1, 2, 3, 4])
    xs `shouldBe` [3, 4]

testDropPrefixNoMatch :: Expectation
testDropPrefixNoMatch = do
    xs <- Stream.toList $
              Stream.dropPrefix
                  (Stream.fromList [1, 3] :: Stream Identity Int)
                  (Stream.fromList [1, 2, 3, 4])
    xs `shouldBe` [1, 2, 3, 4]

testDropPrefixEmpty :: Expectation
testDropPrefixEmpty = do
    xs <- Stream.toList $
              Stream.dropPrefix
                  (Stream.fromList [] :: Stream Identity Int)
                  (Stream.fromList [1, 2, 3])
    xs `shouldBe` [1, 2, 3]

-------------------------------------------------------------------------------
-- dropMatches
-------------------------------------------------------------------------------

testDropMatches :: Expectation
testDropMatches =
    Stream.toList
        (Stream.dropMatches (Array.fromList "..") (Stream.fromList "a..b..c"))
        `shouldReturn` "abc"

testDropMatchesNoMatch :: Expectation
testDropMatchesNoMatch =
    Stream.toList
        (Stream.dropMatches (Array.fromList "..") (Stream.fromList "abc"))
        `shouldReturn` "abc"

testDropMatchesEmptyPat :: Expectation
testDropMatchesEmptyPat =
    Stream.toList
        (Stream.dropMatches (Array.fromList "") (Stream.fromList "abc"))
        `shouldReturn` "abc"

testDropMatchesOverlap :: Expectation
testDropMatchesOverlap =
    Stream.toList
        (Stream.dropMatches (Array.fromList "aa") (Stream.fromList "aaaa"))
        `shouldReturn` ""

-------------------------------------------------------------------------------
-- dropSuffix
-------------------------------------------------------------------------------

testDropSuffix :: Expectation
testDropSuffix =
    Stream.toList
        (Stream.dropSuffix (Array.fromList "..") (Stream.fromList "ab.."))
        `shouldReturn` "ab"

testDropSuffixNoMatch :: Expectation
testDropSuffixNoMatch =
    Stream.toList
        (Stream.dropSuffix (Array.fromList "..") (Stream.fromList "abc"))
        `shouldReturn` "abc"

testDropSuffixEmptyPat :: Expectation
testDropSuffixEmptyPat =
    Stream.toList
        (Stream.dropSuffix (Array.fromList "") (Stream.fromList "abc"))
        `shouldReturn` "abc"

-------------------------------------------------------------------------------
-- replaceMatches
-------------------------------------------------------------------------------

testReplaceMatches :: Expectation
testReplaceMatches =
    Stream.toList
        (Stream.replaceMatches (Array.fromList "..") (Array.fromList "!")
            (Stream.fromList "a..b..c"))
        `shouldReturn` "a!b!c"

testReplaceMatchesLonger :: Expectation
testReplaceMatchesLonger =
    Stream.toList
        (Stream.replaceMatches (Array.fromList "..") (Array.fromList "---")
            (Stream.fromList "a..b..c"))
        `shouldReturn` "a---b---c"

testReplaceMatchesEmptyRepl :: Expectation
testReplaceMatchesEmptyRepl =
    Stream.toList
        (Stream.replaceMatches (Array.fromList "..") (Array.fromList "")
            (Stream.fromList "a..b..c"))
        `shouldReturn` "abc"

testReplaceMatchesOverlap :: Expectation
testReplaceMatchesOverlap =
    Stream.toList
        (Stream.replaceMatches (Array.fromList "aa") (Array.fromList "b")
            (Stream.fromList "aaaa"))
        `shouldReturn` "bb"

-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Stream"

main :: IO ()
main = hspec
  $ H.parallel
#ifdef COVERAGE_BUILD
  $ modifyMaxSuccess (const 10)
#endif
  $ describe moduleName $ do
    describe "Stream group and split operations" $ do
        groupSplitOps "serially"

    {-
    describe "Stream elimination operations" $ do
        serialOps    $ eliminationOps (Stream.fromFoldable) "serially"
        serialOps    $ eliminationOps folded "serially folded"
        serialOps    $ eliminationOpsWord8 (Stream.fromFoldable) "serially"
        serialOps    $ eliminationOpsWord8 folded "serially folded"
        serialOps $ \t ->
            prop "drainWhile (> 0)" $ \n ->
                withNumTests maxTestCount $
                monadicIO $ do
                    let xs = [1..n]
                    ioRef <- run $ newIORef ([] :: [Int])
                    run $
                        drainWhile (> 0) . t $
                            Stream.mapM (\a -> modifyIORef' ioRef (a :) >> return a) $
                                Stream.fromList xs
                    strm <- run $ readIORef ioRef
                    listEquals (==) (reverse strm) (takeWhile (> 0) xs)

    -- XXX Add a test where we chain all transformation APIs and make sure that
    -- the state is being passed through all of them.
    describe "Stream serial elimination operations" $ do
        serialOps    $ eliminationOpsOrdered (Stream.fromFoldable) "serially"
        serialOps    $ eliminationOpsOrdered folded "serially folded"
    -}

    describe "Tests for Stream.groupsBy" groupingOps

    -- describe "Tests for exceptions" $ serialOps $ exceptionOps "serially"

    -- describe "Composed MonadThrow serially" $ composeWithMonadThrow fromSerial

    describe "Nesting" $ do
        prop "foldIterateM" foldIterateM

    describe "groupsRollingBy" $ do
        it "groups equal consecutive elements" testGroupsRollingBy
        it "empty stream" testGroupsRollingByEmpty
        it "groups by rolling predicate" testGroupsRollingByNonEq

    describe "groupsWhile" $ do
        it "groups equal consecutive elements" testGroupsWhile
        it "empty stream" testGroupsWhileEmpty

    describe "wordsBy" $ do
        it "splits on separator" testWordsBy
        it "multiple and leading/trailing seps dropped" testWordsByMultiSep
        it "empty stream" testWordsByEmpty

    describe "parseMany" $ do
        it "applies parser repeatedly" testParseMany

    describe "parseManyPos" $ do
        it "applies parser with positions" testParseManyPos

    describe "parseIterate" $ do
        it "iterates parser on stream" testParseIterate

    describe "parseIteratePos" $ do
        it "iterates parser with positions" testParseIteratePos

    describe "splitSepByOneOf" $ do
        it "splits on any of the patterns" testSplitSepByOneOfSingle
        it "splits on multi-element pattern" testSplitSepByOneOfSeq
        it "empty stream" testSplitSepByOneOfEmpty

    describe "splitInnerBy" $ do
        it "splits inner containers" testSplitInnerBy

    describe "splitInnerBySuffix" $ do
        it "drops trailing empty segment" testSplitInnerBySuffix
        it "preserves non-empty trailing segment" testSplitInnerBySuffixNoTrail

    describe "dropCommonPrefixBy" $ do
        it "drops matching prefix" testDropCommonPrefixByMatch
        it "stops at mismatch" testDropCommonPrefixByMismatch
        it "empty prefix" testDropCommonPrefixByEmpty

    describe "dropPrefix" $ do
        it "drops prefix when matched" testDropPrefixMatch
        it "returns original when not matched" testDropPrefixNoMatch
        it "empty prefix" testDropPrefixEmpty

    describe "dropMatches" $ do
        it "drops all occurrences" testDropMatches
        it "no match leaves stream unchanged" testDropMatchesNoMatch
        it "empty pattern leaves stream unchanged" testDropMatchesEmptyPat
        it "overlapping matches" testDropMatchesOverlap

    describe "dropSuffix" $ do
        it "drops matching suffix" testDropSuffix
        it "no suffix leaves stream unchanged" testDropSuffixNoMatch
        it "empty pattern leaves stream unchanged" testDropSuffixEmptyPat

    describe "replaceMatches" $ do
        it "replaces with shorter replacement" testReplaceMatches
        it "replaces with longer replacement" testReplaceMatchesLonger
        it "replaces with empty (drops)" testReplaceMatchesEmptyRepl
        it "overlapping pattern" testReplaceMatchesOverlap
