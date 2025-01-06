-- XXX We are using head/tail at one place
{-# OPTIONS_GHC -Wno-missing-signatures #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

-- |
-- Module      : Streamly.Test.Data.Stream
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream (main) where

import Control.Monad ( forM_ )
import Control.Monad.IO.Class (MonadIO)
import Data.List (sort, group, intercalate)
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
    , withMaxSuccess
    )
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Streamly.Internal.Data.Parser as Parser

import Test.Hspec as H
import Test.Hspec.QuickCheck

import Streamly.Test.Common
-- import Streamly.Test.Prelude.Common

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
            $ \xs -> withMaxSuccess maxTestCount $ monadicIO $ testCase xs

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
                  withMaxSuccess maxTestCount $ monadicIO $ testCase xs

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
            $ \xss -> withMaxSuccess maxTestCount $ monadicIO $ testCase xss

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
                  withMaxSuccess maxTestCount $ monadicIO $ testCase xss

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
            withMaxSuccess maxTestCount $
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

testgroupsOf ::  Expectation
testgroupsOf =
    Stream.toList
        (Stream.groupsOf 2 Fold.sum (Stream.enumerateFromTo 1 10))
        `shouldReturn` [3::Int, 7, 11, 15, 19]

groupingOps :: Spec
groupingOps = do
    prop "groupsBy" testGroupsBy
    prop "Stream.groups = groups" testGroups
    prop "Stream.groupsByRolling = groups" testGroupsByRolling
    prop "testGroupsBySep" testGroupsBySep
    prop "testgroupsOf" testgroupsOf

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

max_length :: Int
max_length = 1000

unfold :: Property
unfold = monadicIO $ do
    a <- pick $ choose (0, max_length `div` 2)
    b <- pick $ choose (0, max_length)
    let unf = Unfold.second b Unfold.enumerateFromToIntegral
    ls <- toList $ Stream.unfold unf a
    return $ ls == [a..b]

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

sortBy :: Property
sortBy =
    forAll (listOf (chooseInt (0, max_length)))
        $ \lst -> monadicIO $ do
            let s1 = sort lst
            s2 <- toList
                $ StreamK.toStream
                    ( StreamK.sortBy compare
                    $ StreamK.fromStream
                    $ Stream.fromList lst
                    )
            assert $ s1 == s2

------------------------------------------------------------------------------

maxStreamLen :: Int
maxStreamLen = 1000

-- Coverage build takes too long with default number of tests
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

moduleName :: String
moduleName = "Data.Stream"

main :: IO ()
main = hspec
  $ H.parallel
#ifdef COVERAGE_BUILD
  $ modifyMaxSuccess (const 10)
#endif
  $ describe moduleName $ do
    describe "Runners" $ do
        -- XXX use an IORef to store and check the side effects
        it "simple serially" $
            Stream.fold Fold.drain
            (Stream.fromPure (0 :: Int)) `shouldReturn` ()
        it "simple serially with IO" $
            Stream.fold Fold.drain
            (Stream.fromEffect $ putStrLn "hello") `shouldReturn` ()

    describe "Construction" $ do
    {-
        -- XXX To be fixed. Copied from old stream Prelude tests.
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
        serialOps $ prop "serially cons" . constructWithCons Stream.cons
        serialOps $ prop "serially consM" . constructWithConsM Stream.consM id
        -}

        describe "From Generators" $ do
            prop "unfold" unfold

    {-
    describe "Simple Operations" $ serialOps simpleOps

    describe "Functor operations" $ do
        serialOps    $ functorOps (Stream.fromFoldable) "serially" (==)
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
        serialOps $ applicativeOps (Stream.fromFoldable) "serially" (==)
        serialOps $ applicativeOps folded "serially folded" (==)
        serialOps $ applicativeOps1 (Stream.fromFoldable) "serially" (==)
        serialOps $ applicativeOps1 (Stream.fromFoldable) "serially folded" (==)

    -- XXX add tests for indexed/indexedR
    describe "Zip operations" $ do
        -- We test only the serial zip with serial streams and the parallel
        -- stream, because the rate setting in these streams can slow down
        -- zipAsync.
        serialOps   $ prop "zip monadic serially" . zipMonadic (Stream.fromFoldable) (==)
        serialOps   $ prop "zip monadic serially folded" . zipMonadic folded (==)

    -- XXX add merge tests like zip tests
    -- for mergeBy, we can split a list randomly into two lists and
    -- then merge them, it should result in original list
    -- describe "Merge operations" $ do

    describe "Monad operations" $ do
        serialOps   $ prop "serially monad then" . monadThen (Stream.fromFoldable) (==)
        serialOps   $ prop "serially monad then folded" . monadThen folded (==)
        serialOps   $ prop "serially monad bind" . monadBind (Stream.fromFoldable) (==)
        serialOps   $ prop "serially monad bind folded"  . monadBind folded (==)

    describe "Stream transform and combine operations" $ do
        serialOps    $ transformCombineOpsCommon (Stream.fromFoldable) "serially" (==)
        serialOps    $ transformCombineOpsCommon folded "serially" (==)
        serialOps    $ transformCombineOpsOrdered (Stream.fromFoldable) "serially" (==)
        serialOps    $ transformCombineOpsOrdered folded "serially" (==)
    -}

    -- Just some basic sanity tests for now
    let input = [[1,1] :: [Int],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7],[8,8]]
        mustBe g inp out =
            Stream.toList (StreamK.toStream (StreamK.mergeMapWith g StreamK.fromList (StreamK.fromList inp)))
                `shouldReturn` out
     in do
        it "concatPairsWith serial"
            $ mustBe StreamK.append input [1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8]
        it "concatPairsWith wSerial"
            $ mustBe StreamK.interleave input [1,5,3,7,2,6,4,8,1,5,3,7,2,6,4,8]
        it "concatPairsWith mergeBy sorted"
            $ mustBe
                (StreamK.mergeBy compare) input [1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8]
        it "concatPairsWith mergeBy reversed"
            $ mustBe
                (StreamK.mergeBy compare)
                (reverse input)
                [1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8]
        prop "sortBy" sortBy

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
                withMaxSuccess maxTestCount $
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
