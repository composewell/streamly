{-# Language NoMonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
-- XXX We are using head/tail at one place
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

-- |
-- Module      : Streamly.Test.Data.Stream.Serial.Common
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Serial.Common
    (
    -- * Construction operations
      constructWithRepeat
    , constructWithRepeatM
    , constructWithReplicate
    , constructWithReplicateM
    , constructWithIntFromThenTo
    , constructWithDoubleFromThenTo
    , constructWithIterate
    , constructWithIterateM
    , constructWithEnumerate
    , constructWithEnumerateTo
    , constructWithFromIndices
    , constructWithFromIndicesM
    , constructWithFromList
    , constructWithFromListM
    , constructWithUnfoldr
    , constructWithFromPure
    , constructWithFromEffect
    , simpleOps
    -- * Applicative operations
    , applicativeOps
    , applicativeOps1
    -- * Elimination operations
    , eliminationOpsOrdered
    , eliminationOpsWord8
    , eliminationOps
    -- * Functor operations
    , functorOps
    -- * Monoid operations
    , monoidOps
    , loops
    , bindAndComposeSimpleOps
    , bindAndComposeHierarchyOps
    , nestTwoStreams
    , nestTwoStreamsApp
    , composeAndComposeSimpleSerially
    -- , composeAndComposeSimpleWSerially
    -- * Semigroup operations
    , semigroupOps
    , parallelCheck
    -- * Transformation operations
    , transformCombineOpsOrdered
    , transformCombineOpsCommon
    , toListFL
    -- * Monad operations
    , monadBind
    , monadThen
    -- * Zip operations
    , zipApplicative
    , zipMonadic
    , zipAsyncApplicative
    , zipAsyncMonadic
    -- * Exception operations
    , exceptionOps
    -- * MonadThrow operations
    , composeWithMonadThrow
    -- * Cleanup tests
    , checkCleanup
    -- * Adhoc tests
    , takeCombined
    -- * Default values
    , maxTestCount
    , maxStreamLen
    -- * Helper operations
    , folded
    , makeCommonOps
    , makeOps
    , mapOps
    , sortEq
    ) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Applicative (ZipList(..))
import Control.Exception (Exception, try)
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
#ifdef DEVBUILD
import Control.Monad (when)
#endif
import Control.Monad.Catch (throwM)
import Data.IORef ( IORef, atomicModifyIORef', modifyIORef', newIORef
                  , readIORef, writeIORef)
import Data.Foldable
    (elem, foldl, foldr, length, maximum, minimum, null, product, sum)
import Data.List
    ( delete
    , deleteBy
    , elemIndex
    , elemIndices
    , find
    , findIndex
    , findIndices
    , foldl'
    , foldl1'
#ifdef DEVBUILD
    , foldr1
#endif
    , insert
    , intersperse
    , isPrefixOf
    , isSubsequenceOf
    , maximumBy
    , minimumBy
    , scanl'
    , sort
    , stripPrefix
    , unfoldr
    )
import Data.Maybe (mapMaybe)
import GHC.Word (Word8)
import System.Mem (performMajorGC)
import Test.Hspec.QuickCheck
import Test.Hspec
import Test.QuickCheck (Property, choose, forAll, listOf)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Streamly.Internal.Data.Stream (Stream, nil)
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream as IS
import qualified Streamly.Internal.Data.Stream.Prelude as SP
import qualified Streamly.Internal.Data.Unfold as UF

import qualified Data.Map.Strict as Map

import Streamly.Test.Common
import Prelude hiding (Foldable(..))

maxStreamLen :: Int
maxStreamLen = 1000

-- Coverage build takes too long with default number of tests
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

singleton :: Applicative m => a -> Stream m a
singleton a = S.cons a nil

sortEq :: Ord a => [a] -> [a] -> Bool
sortEq a b = sort a == sort b

-------------------------------------------------------------------------------
-- Construction operations
-------------------------------------------------------------------------------

constructWithLen
    :: (Show a, Eq a)
    => (Int -> Stream IO a)
    -> (Int -> [a])
    -> Word8
    -> Property
constructWithLen mkStream mkList len = withNumTests maxTestCount $
    monadicIO $ do
        stream <- run $ S.toList (mkStream (fromIntegral len))
        let list = mkList (fromIntegral len)
        listEquals (==) stream list

constructWithLenM
    :: (Int -> Stream IO Int)
    -> (Int -> IO [Int])
    -> Word8
    -> Property
constructWithLenM mkStream mkList len = withNumTests maxTestCount $
    monadicIO $ do
        stream <- run $ S.toList (mkStream (fromIntegral len))
        list <- run $ mkList (fromIntegral len)
        listEquals (==) stream list

constructWithReplicate, constructWithReplicateM, constructWithIntFromThenTo
    :: Word8 -> Property

constructWithReplicateM = constructWithLenM stream list
    where list = flip replicateM (return 1 :: IO Int)
          stream = flip S.replicateM (return 1 :: IO Int)

constructWithReplicate = constructWithLen stream list
    where list = flip replicate (1 :: Int)
          stream = flip S.replicate (1 :: Int)

constructWithIntFromThenTo l =
    forAll (choose (minBound, maxBound)) $ \(from :: Int) ->
    forAll (choose (minBound, maxBound)) $ \(next :: Int) ->
    forAll (choose (minBound, maxBound)) $ \(to :: Int) ->
        let list len = take len [from,next..to]
            stream len = S.take len $ S.enumerateFromThenTo from next to
        in constructWithLen stream list l

constructWithRepeat, constructWithRepeatM :: Word8 -> Property
constructWithRepeat = constructWithLenM stream list
  where
    stream n = S.take n $ S.repeat 1
    list n = return $ replicate n 1

constructWithRepeatM = constructWithLenM stream list
  where
    stream n = S.take n $ S.repeatM (return 1)
    list n = return $ replicate n 1

-- XXX try very small steps close to 0
constructWithDoubleFromThenTo :: Word8 -> Property
constructWithDoubleFromThenTo l =
    forAll (choose (-9007199254740999,9007199254740999)) $ \(from :: Double) ->
    forAll (choose (-9007199254740999,9007199254740999)) $ \(next :: Double) ->
    forAll (choose (-9007199254740999,9007199254740999)) $ \(to :: Double) ->
        let list len = take len [from,next..to]
            stream len = S.take len $ S.enumerateFromThenTo from next to
        in constructWithLen stream list l

constructWithIterate :: Word8 -> Property
constructWithIterate len =
    withNumTests maxTestCount $
    monadicIO $ do
        stream <-
            run $
            S.toList (S.take (fromIntegral len) (S.iterate (+ 1) (0 :: Int)))
        let list = take (fromIntegral len) (iterate (+ 1) 0)
        listEquals (==) stream list

constructWithIterateM :: Word8 -> Property
constructWithIterateM len =
    withNumTests maxTestCount $
    monadicIO $ do
        mvl <- run (newIORef [] :: IO (IORef [Int]))
        let addM mv x y = modifyIORef' mv (++ [y + x]) >> return (y + x)
            list = take (fromIntegral len) (iterate (+ 1) 0)
        run $
            S.drain $
            S.take (fromIntegral len) $
            S.iterateM (addM mvl 1) (addM mvl 0 0 :: IO Int)
        streamEffect <- run $ readIORef mvl
        listEquals (==) streamEffect list

constructWithFromIndices :: Word8 -> Property
constructWithFromIndices len =
    withNumTests maxTestCount $
    monadicIO $ do
        stream <-
            run $ S.toList (S.take (fromIntegral len) (S.fromIndices id))
        let list = take (fromIntegral len) (iterate (+ 1) 0)
        listEquals (==) stream list

constructWithFromIndicesM :: Word8 -> Property
constructWithFromIndicesM len =
    withNumTests maxTestCount $
    monadicIO $ do
        mvl <- run (newIORef [] :: IO (IORef [Int]))
        let addIndex mv i = modifyIORef' mv (++ [i]) >> return i
            list = take (fromIntegral len) (iterate (+ 1) 0)
        run $
            S.drain $
            S.take (fromIntegral len) $ S.fromIndicesM (addIndex mvl)
        streamEffect <- run $ readIORef mvl
        listEquals (==) streamEffect list

constructWithEnumerate
    :: ([Int] -> [Int])
    -> Word8
    -> Property
constructWithEnumerate listT len =
    withNumTests maxTestCount $
    monadicIO $ do
        strm <- run $ S.toList . S.take (fromIntegral len) $ S.enumerate
        let list = take (fromIntegral len) (enumFrom minBound)
        listEquals (==) (listT strm) list

constructWithEnumerateTo
    :: ([Int] -> [Int])
    -> Word8
    -> Property
constructWithEnumerateTo listT len =
    withNumTests maxTestCount $
    monadicIO $ do
        -- It takes forever to enumerate from minBound to len, so
        -- instead we just do till len elements
        strm <- run $ S.toList $ S.enumerateTo (minBound + fromIntegral len)
        let list = enumFromTo minBound (minBound + fromIntegral len)
        listEquals (==) (listT strm) list

constructWithFromList
    :: ([Int] -> [Int])
    -> Word8
    -> Property
constructWithFromList listT len =
    withNumTests maxTestCount $
    monadicIO $ do
        strm <- run $ S.toList . S.fromList $ [0 .. fromIntegral len]
        let list = [0 .. fromIntegral len]
        listEquals (==) (listT strm) list

constructWithFromListM
    :: ([Int] -> [Int])
    -> Word8
    -> Property
constructWithFromListM listT len =
    withNumTests maxTestCount $
    monadicIO $ do
        strm <-
            run $
            S.toList . S.fromListM . fmap pure $ [0 .. fromIntegral len]
        let list = [0 .. fromIntegral len]
        listEquals (==) (listT strm) list

constructWithUnfoldr
    :: ([Int] -> [Int])
    -> Word8
    -> Property
constructWithUnfoldr listT len =
    withNumTests maxTestCount $
    monadicIO $ do
        strm <- run $ S.toList $ S.unfoldr unfoldStep 0
        let list = unfoldr unfoldStep 0
        listEquals (==) (listT strm) list
  where
    unfoldStep seed =
        if seed > fromIntegral len
        then Nothing
        else Just (seed, seed + 1)

constructWithFromPure :: ([Int] -> [Int]) -> Word8 -> Property
constructWithFromPure listT len =
    withNumTests maxTestCount $
    monadicIO $ do
        strm <-
            run
                $ S.toList . S.take (fromIntegral len)
                $ S.repeat 0
        let list = replicate (fromIntegral len) 0
        listEquals (==) (listT strm) list

constructWithFromEffect :: ([Int] -> [Int]) -> Word8 -> Property
constructWithFromEffect listT len =
    withNumTests maxTestCount $
    monadicIO $ do
        strm <-
            run
                $ S.toList . S.take (fromIntegral len)
                $ S.repeatM (return 0)
        let list = replicate (fromIntegral len) 0
        listEquals (==) (listT strm) list

simpleProps :: (Int -> Stream IO Int) -> Int -> Property
simpleProps constr a = monadicIO $ do
  strm <- run $ S.toList (constr a)
  listEquals (==) strm [a]

simpleOps :: Spec
simpleOps = do
  prop "fromPure a = a" $ simpleProps S.fromPure
  prop "fromEffect a = a" $ simpleProps (S.fromEffect . return)

-------------------------------------------------------------------------------
-- Applicative operations
-------------------------------------------------------------------------------

applicativeOps
    :: ([Int] -> Stream IO Int)
    -> String
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> Spec
applicativeOps constr desc eq = do
    prop (desc <> " <*>") $
        transformFromList2
            constr
            eq
            (\a b -> (,) <$> a <*> b)
            (S.crossApply . S.map (,))
    prop (desc <> " liftA2") $
        transformFromList2 constr eq (liftA2 (,)) (S.crossApply . S.map (,))
    prop (desc <> " Apply - composed first argument") $
        sort <$>
        S.toList (S.crossApply (S.map (,) (S.append (S.fromPure (1 :: Int)) (S.fromPure 2))) (S.fromPure 3)) `shouldReturn`
        [(1, 3), (2, 3 :: Int)]
    prop (desc <> " Apply - composed second argument") $
        sort <$>
        S.toList (S.crossApply (S.fromPure ((,) (1 :: Int))) (S.append (S.fromPure (2 :: Int)) (S.fromPure 3))) `shouldReturn`
        [(1, 2), (1, 3 :: Int)]

-- XXX we can combine this with applicativeOps by making the type sufficiently
-- polymorphic.
applicativeOps1
    :: ([Int] -> Stream IO Int)
    -> String
    -> ([Int] -> [Int] -> Bool)
    -> Spec
applicativeOps1 constr desc eq = do
    prop (desc <> " *>") $
        transformFromList2 constr eq (*>) S.crossApplySnd
    prop (desc <> " <*") $
        transformFromList2 constr eq (<*) S.crossApplyFst

transformFromList2
  :: (Eq c, Show c)
  => ([a] -> Stream IO a)
  -> ([c] -> [c] -> Bool)
  -> ([a] -> [a] -> [c])
  -> (Stream IO a -> Stream IO a -> Stream IO c)
  -> ([a], [a])
  -> Property
transformFromList2 constr eq listOp op (a, b) =
    withNumTests maxTestCount $
    monadicIO $ do
        stream <- run (S.toList $ op (constr a) (constr b))
        let list = listOp a b
        listEquals eq stream list

-------------------------------------------------------------------------------
-- Elimination operations
-------------------------------------------------------------------------------

eliminateOp
    :: (Show a, Eq a)
    => ([s] -> Stream IO s)
    -> ([s] -> a)
    -> (Stream IO s -> IO a)
    -> [s]
    -> Property
eliminateOp constr listOp op a =
    monadicIO $ do
        stream <- run $ op (constr a)
        let list = listOp a
        equals (==) stream list

wrapMaybe :: ([a1] -> a2) -> [a1] -> Maybe a2
wrapMaybe f x = if null x then Nothing else Just (f x)

wrapOutOfBounds :: ([a1] -> Int -> a2) -> Int -> [a1] -> Maybe a2
wrapOutOfBounds f i x | null x = Nothing
                      | i >= length x = Nothing
                      | otherwise = Just (f x i)

wrapThe :: Eq a => [a] -> Maybe a
wrapThe (x:xs)
    | all (x ==) xs = Just x
    | otherwise = Nothing
wrapThe [] = Nothing

-- This is the reference uniq implementation to compare uniq against,
-- we can use uniq from vector package, but for now this should
-- suffice.
referenceUniq :: Eq a => [a] -> [a]
referenceUniq = go
  where
    go [] = []
    go (x:[]) = [x]
    go (x:y:xs)
        | x == y = go (x : xs)
        | otherwise = x : go (y : xs)

{- HLINT ignore "Use all" -}
{- HLINT ignore "Use any" -}
eliminationOps
    :: ([Int] -> Stream IO Int)
    -> String
    -> Spec
eliminationOps constr desc = do
    -- Elimination
    prop (desc <> " null") $ eliminateOp constr null S.null
    prop (desc <> " foldl'") $
        eliminateOp constr (foldl' (+) 0) $ S.foldl' (+) 0
    prop (desc <> " foldl1'") $
        eliminateOp constr (wrapMaybe $ foldl1' (+)) $ S.fold (FL.foldl1' (+))
#ifdef DEVBUILD
    prop (desc <> " foldr1") $
        eliminateOp constr (wrapMaybe $ foldr1 (+)) $ S.foldr1 (+)
#endif
    prop (desc <> " all") $ eliminateOp constr (all even) $ S.all even
    prop (desc <> " any") $ eliminateOp constr (any even) $ S.any even
    prop (desc <> " and") $ eliminateOp constr (and . fmap (> 0)) $
        S.fold FL.and . S.map (> 0)
    prop (desc <> " or") $ eliminateOp constr (or . fmap (> 0)) $
        S.fold FL.or . S.map (> 0)
    prop (desc <> " length") $ eliminateOp constr length $ S.fold FL.length
    prop (desc <> " sum") $ eliminateOp constr sum $ S.fold FL.sum
    prop (desc <> " product") $ eliminateOp constr product $ S.fold FL.product
    prop (desc <> " mapM_ sumIORef") $
        eliminateOp constr sum
        (\strm -> do
             ioRef <- newIORef 0
             let sumInRef a = modifyIORef' ioRef (a +)
             S.mapM_ sumInRef strm
             readIORef ioRef)
    prop (desc <> "trace sumIORef") $
        eliminateOp constr sum
        (\strm -> do
             ioRef <- newIORef 0
             let sumInRef a = modifyIORef' ioRef (a +)
             S.drain $ S.trace sumInRef strm
             readIORef ioRef)

    prop (desc <> " maximum") $
        eliminateOp constr (wrapMaybe maximum) S.maximum
    prop (desc <> " minimum") $
        eliminateOp constr (wrapMaybe minimum) S.minimum

    prop (desc <> " maximumBy compare") $
        eliminateOp constr (wrapMaybe maximum) $
        S.maximumBy compare
    prop (desc <> " maximumBy flip compare") $
        eliminateOp constr (wrapMaybe $ maximumBy $ flip compare) $
        S.maximumBy (flip compare)
    prop (desc <> " minimumBy compare") $
        eliminateOp constr (wrapMaybe minimum) $
        S.minimumBy compare
    prop (desc <> " minimumBy flip compare") $
        eliminateOp constr (wrapMaybe $ minimumBy $ flip compare) $
        S.minimumBy (flip compare)

    prop (desc <> " findIndex") $
        eliminateOp constr (findIndex odd) $ S.fold (FL.findIndex odd)
    prop (desc <> " elemIndex") $
        eliminateOp constr (elemIndex 3) $ S.fold (FL.elemIndex 3)

    prop (desc <> " !! 5") $
        eliminateOp constr (wrapOutOfBounds (!!) 5) (S.!! 5)
    prop (desc <> " !! 4") $
        eliminateOp constr (wrapOutOfBounds (!!) 0) (S.!! 0)

    prop (desc <> " find") $ eliminateOp constr (find even) $ S.find even
    prop (desc <> " findM") $ eliminateOp constr (find even) $ S.findM (return . even)
    prop (desc <> " lookup") $
        eliminateOp constr (lookup 3 . flip zip [1..]) $
            S.lookup 3 . S.zipWith (\a b -> (b, a)) (S.fromList [(1::Int)..])
    prop (desc <> " the") $ eliminateOp constr wrapThe S.the

    -- Multi-stream eliminations
    -- XXX Write better tests for substreams.
    prop (desc <> " eqBy (==) t t") $
        eliminateOp constr (\s -> s == s) (\s -> S.eqBy (==) s s)
    prop (desc <> " cmpBy (==) t t") $
        eliminateOp constr (\s -> compare s s) (\s -> S.cmpBy compare s s)
    prop (desc <> " isPrefixOf 10") $ eliminateOp constr (isPrefixOf [1..10]) $
        S.isPrefixOf (S.fromList [(1::Int)..10])
    prop (desc <> " isSubsequenceOf 10") $
        eliminateOp constr (isSubsequenceOf $ filter even [1..10]) $
        S.isSubsequenceOf (S.fromList $ filter even [(1::Int)..10])
    prop (desc <> " stripPrefix 10") $ eliminateOp constr (stripPrefix [1..10]) $
        (\s -> s >>= maybe (return Nothing) (fmap Just . S.toList)) .
        S.stripPrefix (S.fromList [(1::Int)..10])

-- head/tail/last may depend on the order in case of parallel streams
-- so we test these only for serial streams.
eliminationOpsOrdered
    :: ([Int] -> Stream IO Int)
    -> String
    -> Spec
eliminationOpsOrdered constr desc = do
    prop (desc <> " head") $ eliminateOp constr (wrapMaybe head) S.head
    prop (desc <> " tail") $ eliminateOp constr (wrapMaybe tail) $ \x -> do
        r <- S.tail x
        case r of
            Nothing -> return Nothing
            Just s -> Just <$> S.toList s
    prop (desc <> " last") $ eliminateOp constr (wrapMaybe last) S.last
    prop (desc <> " init") $ eliminateOp constr (wrapMaybe init) $ \x -> do
        r <- S.init x
        case r of
            Nothing -> return Nothing
            Just s -> Just <$> S.toList s

elemOp
    :: ([Word8] -> Stream IO Word8)
    -> (Word8 -> Stream IO Word8 -> IO Bool)
    -> (Word8 -> [Word8] -> Bool)
    -> (Word8, [Word8])
    -> Property
elemOp constr streamOp listOp (x, xs) =
    monadicIO $ do
        stream <- run $ streamOp x (constr xs)
        let list = listOp x xs
        equals (==) stream list

eliminationOpsWord8
    :: ([Word8] -> Stream IO Word8)
    -> String
    -> Spec
eliminationOpsWord8 constr desc = do
    prop (desc <> " elem") $ elemOp constr S.elem elem
    prop (desc <> " notElem") $ elemOp constr S.notElem notElem

-------------------------------------------------------------------------------
-- Functor operations
-------------------------------------------------------------------------------

functorOps
    :: ([Int] -> Stream IO Int)
    -> String
    -> ([Int] -> [Int] -> Bool)
    -> Spec
functorOps constr desc eq = do
    prop (desc <> " id") $ transformFromList constr eq id id
    prop (desc <> " fmap (+1)") $
        transformFromList constr eq (fmap (+ 1)) (fmap (+ 1))
    prop (desc <> " fmap on composed (<>)") $
        sort <$>
        S.toList (fmap (+ 1) (S.append (constr [1]) (constr [2]))) `shouldReturn`
        ([2, 3] :: [Int])

transformFromList
    :: (Eq b, Show b) =>
       ([a] -> Stream IO a)
    -> ([b] -> [b] -> Bool)
    -> ([a] -> [b])
    -> (Stream IO a -> Stream IO b)
    -> [a]
    -> Property
transformFromList constr eq listOp op a =
    monadicIO $ do
        stream <- run ((S.toList . op) (constr a))
        let list = listOp a
        listEquals eq stream list


------------------------------------------------------------------------------
-- Monoid operations
------------------------------------------------------------------------------

monoidOps
    :: String
    -> Stream IO Int
    -> ([Int] -> [Int] -> Bool)
    -> Spec
monoidOps desc z eq = do
    -- XXX these should get covered by the property tests
    prop (desc <> " Compose mempty, mempty") $ spec (S.append z z) []
    prop (desc <> " Compose empty at the beginning") $ spec (S.append z (singleton 1)) [1]
    prop (desc <> " Compose empty at the end") $ spec (S.append (singleton 1) z) [1]
    prop (desc <> " Compose two") $ spec (S.append (singleton 0) (singleton 1)) [0, 1]
    prop (desc <> " Compose many") $
        spec ((S.concatMap singleton . S.fromFoldable) [1 .. 100]) [1 .. 100]

    -- These are not covered by the property tests
    prop (desc <> " Compose three - empty in the middle") $
        spec (S.append (singleton 0) (S.append z (singleton 1))) [0, 1]
    prop (desc <> " Compose left associated") $
        spec
            (S.append (S.append (S.append (singleton 0) (singleton 1)) (singleton 2)) (singleton 3))
            [0, 1, 2, 3]
    prop (desc <> " Compose right associated") $
        spec
            (S.append (singleton 0) (S.append (singleton 1) (S.append (singleton 2) (singleton 3))))
            [0, 1, 2, 3]
    prop (desc <> " Compose hierarchical (multiple levels)") $
        spec
            (S.append
             (S.append (S.append (singleton 0) (singleton 1)) (S.append (singleton 2) (singleton 3)))
             (S.append (S.append (singleton 4) (singleton 5)) (S.append (singleton 6) (singleton 7))))
            [0 .. 7]

    where

    tl = S.toList
    spec s list =
        monadicIO $ do
            stream <- run $ tl s
            listEquals eq stream list

---------------------------------------------------------------------------
-- Monoidal composition recursion loops
---------------------------------------------------------------------------

loops
    :: (Stream IO Int -> Stream IO Int)
    -> ([Int] -> [Int])
    -> ([Int] -> [Int])
    -> Spec
loops t tsrt hsrt = do
    it "Tail recursive loop" $ (tsrt <$> S.toList (loopTail 0))
            `shouldReturn` [0..3]

    it "Head recursive loop" $ (hsrt <$> S.toList (loopHead 0))
            `shouldReturn` [0..3]

    where
        loopHead x =
            S.concatMap (\_ -> t $ S.append (if x < 3 then loopHead (x + 1) else nil) (S.fromPure x))
                        (S.fromEffect $ putStrLn "LoopHead...")

        loopTail x =
            S.concatMap (\_ -> t $ S.append (S.fromPure x) (if x < 3 then loopTail (x + 1) else nil))
                        (S.fromEffect $ putStrLn "LoopTail...")

---------------------------------------------------------------------------
-- Bind and monoidal composition combinations
---------------------------------------------------------------------------

bindAndComposeSimpleOps
    :: String
    -> ([Int] -> [Int] -> Bool)
    -> Spec
bindAndComposeSimpleOps desc eq = do
    bindAndComposeSimple
        ("Bind and compose " <> desc <> " Stream serially/")
        id

    where

    bindAndComposeSimple
        :: String
        -> (Stream IO Int -> Stream IO Int)
        -> Spec
    bindAndComposeSimple idesc t2 = do
      -- XXX need a bind in the body of forEachWith instead of a simple return
      prop (idesc <> " Compose many (right fold) with bind") $ \list ->
          monadicIO $ do
              stream <-
                  run $
                  S.toList
                      (t2 $ S.fromFoldable list)
              listEquals eq stream list

      prop (idesc <> " Compose many (left fold) with bind") $ \list ->
          monadicIO $ do
              let forL xs k = foldl S.append nil $ fmap k xs
              stream <-
                  run $ S.toList (t2 $ forL list S.fromPure)
              listEquals eq stream list

---------------------------------------------------------------------------
-- Bind and monoidal composition combinations
---------------------------------------------------------------------------

bindAndComposeHierarchyOps ::
       String
    -> Spec
bindAndComposeHierarchyOps desc = do
    let fldldesc = "Bind and compose foldl, " <> desc <> " Stream "
        fldrdesc = "Bind and compose foldr, " <> desc <> " Stream "

    bindAndComposeHierarchy
        (fldldesc <> "serially") id fldl
    bindAndComposeHierarchy
        (fldrdesc <> "serially") id fldr

  where

    bindAndComposeHierarchy
        :: String
        -> (Stream IO Int -> Stream IO Int)
        -> ([Stream IO Int] -> Stream IO Int)
        -> Spec
    bindAndComposeHierarchy specdesc t2 g =
        describe specdesc $
        it "Bind and compose nested" $
            (sort <$> S.toList bindComposeNested)
                `shouldReturn` (sort (
                    [12, 18]
                    <> replicate 3 13
                    <> replicate 3 17
                    <> replicate 6 14
                    <> replicate 6 16
                    <> replicate 7 15) :: [Int])

        where

        -- bindComposeNested :: WAsyncT IO Int
        bindComposeNested =
            let c1 = tripleCompose (S.fromPure 1) (S.fromPure 2) (S.fromPure 3)
                c2 = tripleCompose (S.fromPure 4) (S.fromPure 5) (S.fromPure 6)
                c3 = tripleCompose (S.fromPure 7) (S.fromPure 8) (S.fromPure 9)
                b = tripleBind c1 c2 c3
        -- it seems to be causing a huge space leak in hspec so disabling this for now
        --            c = tripleCompose b b b
        --            m = tripleBind c c c
        --         in m
            in b

        tripleCompose a b c = t2 $ g [a, b, c]
        tripleBind mx my mz =
            S.concatMap (\x ->
                S.concatMap (\y ->
                    S.concatMap (\z -> S.fromPure (x + y + z)) mz) my) mx

    fldr :: [Stream IO Int] -> Stream IO Int
    fldr = foldr S.append nil
    fldl :: [Stream IO Int] -> Stream IO Int
    fldl = foldl S.append nil

-- Nest two lists using different styles of product compositions
nestTwoStreams
    :: String
    -> ([Int] -> [Int])
    -> ([Int] -> [Int])
    -> Spec
nestTwoStreams desc streamListT listT =
    it ("Nests two streams using monadic " <> desc <> " composition") $ do
    let s1 = S.fromFoldable [1..4]
        s2 = S.fromFoldable [5..8]
    r <- S.toList $ S.concatMap (\x -> S.concatMap (\y -> S.fromPure (x + y)) s2) s1
    streamListT r `shouldBe` listT [6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12]

nestTwoStreamsApp
    :: String
    -> ([Int] -> [Int])
    -> ([Int] -> [Int])
    -> Spec
nestTwoStreamsApp desc streamListT listT =
    it ("Nests two streams using applicative " <> desc <> " composition") $ do
    let s1 = S.fromFoldable [1..4]
        s2 = S.fromFoldable [5..8]
        r  = S.toList (S.crossApply (S.map (+) s1) s2)
    streamListT <$> r
        `shouldReturn` listT [6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12]


-- TBD need more such combinations to be tested.
composeAndComposeSimple
    :: (Stream IO Int -> Stream IO Int)
    -> [[Int]] -> Spec
composeAndComposeSimple t2 answer = do
    let rfold = t2 . S.fromFoldable
    it "Compose right associated outer expr, right folded inner" $
         S.toList (S.append (rfold [1,2,3]) (S.append (rfold [4,5,6]) (rfold [7,8,9])))
            `shouldReturn` head answer

    it "Compose left associated outer expr, right folded inner" $
         S.toList (S.append (S.append (rfold [1,2,3]) (rfold [4,5,6])) (rfold [7,8,9]))
            `shouldReturn` (answer !! 1)

    let lfold xs = t2 $ foldl S.append nil $ fmap S.fromPure xs
    it "Compose right associated outer expr, left folded inner" $
         S.toList (S.append (lfold [1,2,3]) (S.append (lfold [4,5,6]) (lfold [7,8,9])))
            `shouldReturn` (answer !! 2)

    it "Compose left associated outer expr, left folded inner" $
         S.toList (S.append (S.append (lfold [1,2,3]) (lfold [4,5,6])) (lfold [7,8,9]))
            `shouldReturn` (answer !! 3)

composeAndComposeSimpleSerially
    :: String
    -> [[Int]]
    -> Spec
composeAndComposeSimpleSerially desc answer =
    describe (desc <> " and Serial <>") $ composeAndComposeSimple id answer

{-
-- XXX Move this to WSerial type test or the wserial/interleave op test
composeAndComposeSimpleWSerially
    :: (IsStream t, Semigroup (t IO Int))
    => String
    -> [[Int]]
    -> (t IO Int -> SerialT IO Int)
    -> Spec
composeAndComposeSimpleWSerially desc answer t = do
    describe (desc <> " and WSerial <>") $ composeAndComposeSimple t S.fromWSerial answer
-}

-------------------------------------------------------------------------------
-- Semigroup operations
-------------------------------------------------------------------------------

foldFromList
    :: ([Int] -> Stream IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> [Int]
    -> Property
foldFromList constr eq = transformFromList constr eq id id

-- XXX concatenate streams of multiple elements rather than single elements
semigroupOps
    :: String
    -> ([Int] -> [Int] -> Bool)
    -> Spec
semigroupOps desc eq = do
    prop (desc <> " <>") $ foldFromList (S.concatMap singleton . S.fromFoldable) eq

-------------------------------------------------------------------------------
-- Transformation operations
-------------------------------------------------------------------------------

transformCombineFromList
    :: ([Int] -> Stream IO Int)
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
            stream <- run (S.toList $
                S.append (constr a) (op (S.append (constr b) (constr c))))
            let list = a <> listOp (b <> c)
            listEquals eq stream list

takeEndBy :: Property
takeEndBy = forAll (listOf (chooseInt (0, maxStreamLen))) $ \lst -> monadicIO $ do
    let (s1, s3) = span (<= 200) lst
    let s4 = [head s3 | not (null s3)]
    s2 <- run $ S.toList $ IS.takeEndBy (> 200) $ S.fromList lst
    assert $ s1 ++ s4 == s2

-- XXX add tests for MonadReader and MonadError etc. In case an SVar is
-- accidentally passed through them.
--
-- This tests transform ops along with detecting illegal sharing of SVar across
-- conurrent streams. These tests work for all stream types whereas
-- transformCombineOpsOrdered work only for ordered stream types i.e. excluding
-- the Async type.
transformCombineOpsCommon
    :: ([Int] -> Stream IO Int)
    -> String
    -> ([Int] -> [Int] -> Bool)
    -> Spec
transformCombineOpsCommon constr desc eq = do
    let transform = transformCombineFromList constr eq

    -- Filtering
    prop (desc <> " filter False") $
        transform (filter (const False)) (S.filter (const False))
    prop (desc <> " filter True") $
        transform (filter (const True)) (S.filter (const True))
    prop (desc <> " filter even") $
        transform (filter even) (S.filter even)

    prop (desc <> " filterM False") $
        transform (filter (const False)) (S.filterM (const $ return False))
    prop (desc <> " filterM True") $
        transform (filter (const True)) (S.filterM (const $ return True))
    prop (desc <> " filterM even") $
        transform (filter even) (S.filterM (return . even))

    prop (desc <> " take maxBound") $
        transform (take maxBound) (S.take maxBound)
    prop (desc <> " take 0") $ transform (take 0) (S.take 0)

    prop (desc <> " takeWhile True") $
        transform (takeWhile (const True)) (S.takeWhile (const True))
    prop (desc <> " takeWhile False") $
        transform (takeWhile (const False)) (S.takeWhile (const False))

    prop (desc <> " takeWhileM True") $
        transform (takeWhile (const True)) (S.takeWhileM (const $ return True))
    prop (desc <> " takeWhileM False") $
        transform (takeWhile (const False)) (S.takeWhileM (const $ return False))

    prop "takeEndBy" takeEndBy

    prop (desc <> " drop maxBound") $
        transform (drop maxBound) (S.drop maxBound)
    prop (desc <> " drop 0") $ transform (drop 0) (S.drop 0)

    prop (desc <> " dropWhile True") $
        transform (dropWhile (const True)) (S.dropWhile (const True))
    prop (desc <> " dropWhile False") $
        transform (dropWhile (const False)) (S.dropWhile (const False))

    prop (desc <> " dropWhileM True") $
        transform (dropWhile (const True)) (S.dropWhileM (const $ return True))
    prop (desc <> " dropWhileM False") $
        transform (dropWhile (const False)) (S.dropWhileM (const $ return False))

    prop (desc <> " deleteBy (<=) maxBound") $
        transform (deleteBy (<=) maxBound) (S.deleteBy (<=) maxBound)
    prop (desc <> " deleteBy (==) 4") $
        transform (delete 4) (S.deleteBy (==) 4)

    -- transformation
    prop (desc <> " mapM (+1)") $
        transform (fmap (+1)) (S.mapM (\x -> return (x + 1)))

    prop (desc <> " scanl'") $ transform (scanl' (const id) 0)
                                       (S.scanl' (const id) 0)
    prop (desc <> " postscanl'") $ transform (tail . scanl' (const id) 0)
                                       (S.postscanl' (const id) 0)
    prop (desc <> " scanlM'") $ transform (scanl' (const id) 0)
                                       (S.scanlM' (\_ a -> return a) (return 0))
    prop (desc <> " postscanlM'") $ transform (tail . scanl' (const id) 0)
                                       (S.postscanlM' (\_ a -> return a) (return 0))
    prop (desc <> " mapAccumM running sum") $
        transform (tail . scanl' (+) 0)
                  (S.mapAccumM (\s a -> let s1 = s + a in return (s1, s1))
                               (return 0))
    prop (desc <> " mapAccumM identity (== postscanlM')") $
        transform (tail . scanl' (const id) 0)
                  (S.mapAccumM (\_ a -> return (a, a)) (return 0))
    let modifyLastList _ [] = []
        modifyLastList f xs = init xs ++ [f (last xs)]
    prop (desc <> " modifyLast negate") $
        transform (modifyLastList negate) (S.modifyLast negate)
    prop (desc <> " modifyLast id") $
        transform (modifyLastList id) (S.modifyLast id)
    prop (desc <> " scanl1'") $ transform (scanl1 (const id))
                                         (S.scanl1' (const id))
    prop (desc <> " scanl1M'") $ transform (scanl1 (const id))
                                          (S.scanl1M' (\_ a -> return a))

    let f x = if odd x then Just (x + 100) else Nothing
    prop (desc <> " mapMaybe") $ transform (mapMaybe f) (S.mapMaybe f)
    prop (desc <> " mapMaybeM") $
        transform (mapMaybe f) (S.mapMaybeM (return . f))

    -- tap
    prop (desc <> " tap FL.sum . map (+1)") $ \a b ->
        withNumTests maxTestCount $
        monadicIO $ do
            cref <- run $ newIORef 0
            let fldstp _ e = modifyIORef' cref (e +)
                sumfoldinref = FL.foldlM' fldstp (return ())
                op = S.tap sumfoldinref . S.mapM (\x -> return (x+1))
                listOp = fmap (+1)
            stream <- run (S.toList $ op (S.append (constr a) (constr b)))
            let list = listOp (a <> b)
            ssum <- run $ readIORef cref
            assert (sum list == ssum)
            listEquals eq stream list

    -- reordering
    prop (desc <> " reverse") $ transform reverse S.reverse
    prop (desc <> " reverse'") $ transform reverse S.reverseUnbox

    -- inserting
    prop (desc <> " intersperseM") $
        forAll (choose (minBound, maxBound)) $ \n ->
            transform (intersperse n) (S.intersperseM $ return n)
    prop (desc <> " intersperse") $
        forAll (choose (minBound, maxBound)) $ \n ->
            transform (intersperse n) (S.intersperse n)
    prop (desc <> " insertBy 0") $
        forAll (choose (minBound, maxBound)) $ \n ->
            transform (insert n) (S.insertBy compare n)

    -- multi-stream
    prop (desc <> " concatMap") $
        forAll (choose (0, 100)) $ \n ->
            transform (concatMap (const [1..n]))
                (S.concatMap (const (S.fromList [1..n])))
    prop (desc <> " concatMapM") $
        forAll (choose (0, 100)) $ \n ->
            transform (concatMap (const [1..n]))
                (S.concatMapM (const (return $ S.fromList [1..n])))
    prop (desc <> " unfoldMany") $
        forAll (choose (0, 100)) $ \n ->
            transform (concatMap (const [1..n]))
                (S.unfoldMany (UF.lmap (const undefined)
                                   $ UF.both [1..n] UF.fromList))

toListFL :: Monad m => FL.Fold m a [a]
toListFL = FL.toList

-- transformation tests that can only work reliably for ordered streams i.e.
-- Serial, Ahead and Zip. For example if we use "take 1" on an async stream, it
-- might yield a different result every time.
transformCombineOpsOrdered
    :: ([Int] -> Stream IO Int)
    -> String
    -> ([Int] -> [Int] -> Bool)
    -> Spec
transformCombineOpsOrdered constr desc eq = do
    let transform = transformCombineFromList constr eq

    -- Filtering
    prop (desc <> " take 1") $ transform (take 1) (S.take 1)
#ifdef DEVBUILD
    prop (desc <> " take 2") $ transform (take 2) (S.take 2)
    prop (desc <> " take 3") $ transform (take 3) (S.take 3)
    prop (desc <> " take 4") $ transform (take 4) (S.take 4)
    prop (desc <> " take 5") $ transform (take 5) (S.take 5)
#endif
    prop (desc <> " take 10") $ transform (take 10) (S.take 10)

    prop (desc <> " takeWhile > 0") $
        transform (takeWhile (> 0)) (S.takeWhile (> 0))
    prop (desc <> " takeWhileM > 0") $
        transform (takeWhile (> 0)) (S.takeWhileM (return . (> 0)))

    prop (desc <> " drop 1") $ transform (drop 1) (S.drop 1)
    prop (desc <> " drop 10") $ transform (drop 10) (S.drop 10)

    prop (desc <> " dropWhile > 0") $
        transform (dropWhile (> 0)) (S.dropWhile (> 0))
    prop (desc <> " dropWhileM > 0") $
        transform (dropWhile (> 0)) (S.dropWhileM (return . (> 0)))
    prop (desc <> " scan") $ transform (scanl' (+) 0) (S.scanl' (+) 0)

    prop (desc <> " uniq") $ transform referenceUniq S.uniq

    prop (desc <> " deleteBy (<=) 0") $
        transform (deleteBy (<=) 0) (S.deleteBy (<=) 0)

    prop (desc <> " findIndices") $
        transform (findIndices odd) (S.findIndices odd)
    prop (desc <> " findIndices . filter") $
        transform (findIndices odd . filter odd)
                  (S.findIndices odd . S.filter odd)
    prop (desc <> " elemIndices") $
        transform (elemIndices 0) (S.elemIndices 0)

    -- XXX this does not fail when the SVar is shared, need to fix.
    prop (desc <> " concurrent application") $
        transform (fmap (+1)) (S.map (+1))

-------------------------------------------------------------------------------
-- Monad operations
-------------------------------------------------------------------------------

monadThen
    :: ([Int] -> Stream IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> ([Int], [Int])
    -> Property
monadThen constr eq (a, b) = withNumTests maxTestCount $ monadicIO $ do
    stream <- run (S.toList (S.crossApplySnd (constr a) (constr b)))
    let list = a >> b
    listEquals eq stream list

monadBind
    :: ([Int] -> Stream IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> ([Int], [Int])
    -> Property
monadBind constr eq (a, b) = withNumTests maxTestCount $
    monadicIO $ do
        stream <-
            run
                (S.toList
                     (S.concatMap (\x -> S.map (+ x) (constr b)) (constr a)))
        let list = a >>= \x -> (+ x) <$> b
        listEquals eq stream list

-------------------------------------------------------------------------------
-- Zip operations
-------------------------------------------------------------------------------

zipApplicative
    :: ([Int] -> Stream IO Int)
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> ([Int], [Int])
    -> Property
zipApplicative constr eq (a, b) = withNumTests maxTestCount $
    monadicIO $ do
        stream1 <- run (S.toList (S.zipWith (,) (constr a) (constr b)))
        stream2 <- run (S.toList (S.zipWith (,) (constr a) (constr b)))
        stream3 <- run (S.toList (S.zipWith (,) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        listEquals eq stream1 list
        listEquals eq stream2 list
        listEquals eq stream3 list

zipMonadic
    :: ([Int] -> Stream IO Int)
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> ([Int], [Int])
    -> Property
zipMonadic constr eq (a, b) = withNumTests maxTestCount $
    monadicIO $ do
        stream1 <-
            run
                (S.toList
                     (S.zipWithM (curry return) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        listEquals eq stream1 list

zipAsyncMonadic
    :: ([Int] -> Stream IO Int)
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> ([Int], [Int])
    -> Property
zipAsyncMonadic constr eq (a, b) = withNumTests maxTestCount $
    monadicIO $ do
        stream1 <-
            run
                (S.toList
                     (S.zipWithM (curry return) (constr a) (constr b)))
        stream2 <-
            run
                (S.toList
                     (S.zipWithM (curry return) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        listEquals eq stream1 list
        listEquals eq stream2 list

zipAsyncApplicative
    :: ([Int] -> Stream IO Int)
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> ([Int], [Int])
    -> Property
zipAsyncApplicative constr eq (a, b) = withNumTests maxTestCount $
    monadicIO $ do
        stream <-
            run
                (S.toList
                     (S.zipWith (,) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        listEquals eq stream list

---------------------------------------------------------------------------
-- Semigroup/Monoidal Composition strict ordering checks
---------------------------------------------------------------------------

parallelCheck
    :: (Stream IO Int -> Stream IO Int -> Stream IO Int)
    -> Spec
parallelCheck f = do
    it "Parallel ordering left associated" $
        S.toList (((event 4 `f` event 3) `f` event 2) `f` event 1)
            `shouldReturn` [1..4]

    it "Parallel ordering right associated" $
        S.toList (event 4 `f` (event 3 `f` (event 2 `f` event 1)))
            `shouldReturn` [1..4]

    where event n = S.fromEffect (threadDelay (n * 200000) >> return n)

-------------------------------------------------------------------------------
-- Exception ops
-------------------------------------------------------------------------------

beforeProp :: [Int] -> Property
beforeProp vec =
    withNumTests maxTestCount $
    monadicIO $ do
        ioRef <- run $ newIORef []
        run
            $ S.drain
            $ S.before (writeIORef ioRef [0])
            $ S.mapM (\a -> do atomicModifyIORef' ioRef (\xs -> (xs ++ [a], ()))
                               return a)
            $ S.fromList vec
        refValue <- run $ readIORef ioRef
        listEquals (==) (head refValue : sort (tail refValue)) (0:sort vec)

afterProp :: [Int] -> Property
afterProp vec =
    withNumTests maxTestCount $
    monadicIO $ do
        ioRef <- run $ newIORef []
        run
            $ S.drain
            $ S.afterIO(modifyIORef' ioRef (0:))
            $ S.mapM (\a -> do atomicModifyIORef' ioRef (\xs -> (a:xs, ()))
                               return a)
            $ S.fromList vec
        refValue <- run $ readIORef ioRef
        listEquals (==) (head refValue : sort (tail refValue)) (0:sort vec)

bracketProp :: [Int] -> Property
bracketProp vec =
    withNumTests maxTestCount $
    monadicIO $ do
        ioRef <- run $ newIORef (0 :: Int)
        run $
            S.drain $
            S.bracketIO
                (return ioRef)
                (`writeIORef` 1)
                (\ioref ->
                     S.mapM
                         (\a -> writeIORef ioref 2 >> return a)
                         (S.fromList vec))
        refValue <- run $ readIORef ioRef
        assert $ refValue == 1

#ifdef DEVBUILD
bracketPartialStreamProp :: [Int] -> Property
bracketPartialStreamProp vec =
    forAll (choose (0, length vec)) $ \len -> do
        withNumTests maxTestCount $
            monadicIO $ do
                ioRef <- run $ newIORef (0 :: Int)
                run $
                    S.drain $
                    S.take len $
                    S.bracketIO
                        (writeIORef ioRef 1 >> return ioRef)
                        (`writeIORef` 3)
                        (\ioref ->
                             S.mapM
                                 (\a -> writeIORef ioref 2 >> return a)
                                 (S.fromList vec))
                run $ do
                    performMajorGC
                    threadDelay 1000000
                refValue <- run $ readIORef ioRef
                when (refValue /= 0 && refValue /= 3) $
                    error $ "refValue == " ++ show refValue
#endif

bracketExceptionProp :: Property
bracketExceptionProp =
    withNumTests maxTestCount $
    monadicIO $ do
        ioRef <- run $ newIORef (0 :: Int)
        res <-
            run $
            try . S.drain $
            S.bracketIO
                (return ioRef)
                (`writeIORef` 1)
                (const $ S.fromEffect (throwM (ExampleException "E")))
        assert $ res == Left (ExampleException "E")
        refValue <- run $ readIORef ioRef
        assert $ refValue == 1

finallyProp :: [Int] -> Property
finallyProp vec =
    withNumTests maxTestCount $
    monadicIO $ do
        ioRef <- run $ newIORef (0 :: Int)
        run $
            S.drain $
            S.finallyIO
                (writeIORef ioRef 1)
                (S.mapM (\a -> writeIORef ioRef 2 >> return a) (S.fromList vec))
        refValue <- run $ readIORef ioRef
        assert $ refValue == 1

retry :: Spec
retry = do
    ref <- runIO $ newIORef (0 :: Int)
    res <- runIO $ S.toList (SP.retry emap handler (stream1 ref))
    refVal <- runIO $ readIORef ref
    spec res refVal

    where

    emap = Map.singleton (ExampleException "E") 10

    stream1 ref =
        S.fromListM
            [ return 1
            , return 2
            , atomicModifyIORef' ref (\a -> (a + 1, ()))
                  >> throwM (ExampleException "E")
                  >> return 3
            , return 4
            ]

    stream2 = S.fromList [5, 6, 7 :: Int]
    handler = const stream2
    expectedRes = [1, 2, 5, 6, 7]
    expectedRefVal = 11

    spec res refVal = do
        it "Runs the exception handler properly" $ res `shouldBe` expectedRes
        it "Runs retires the exception correctly"
            $ refVal `shouldBe` expectedRefVal

#ifdef DEVBUILD
finallyPartialStreamProp :: [Int] -> Property
finallyPartialStreamProp vec =
    forAll (choose (0, length vec)) $ \len -> do
        withNumTests maxTestCount $
            monadicIO $ do
                ioRef <- run $ newIORef (0 :: Int)
                run $
                    S.drain $
                    S.take len $
                    S.finallyIO
                        (writeIORef ioRef 2)
                        (S.mapM
                             (\a -> writeIORef ioRef 1 >> return a)
                             (S.fromList vec))
                run $ do
                    performMajorGC
                    threadDelay 100000
                refValue <- run $ readIORef ioRef
                when (refValue /= 0 && refValue /= 2) $
                    error $ "refValue == " ++ show refValue
#endif

finallyExceptionProp :: Property
finallyExceptionProp =
    withNumTests maxTestCount $
    monadicIO $ do
        ioRef <- run $ newIORef (0 :: Int)
        res <-
            run $
            try . S.drain $
            S.finallyIO
                (writeIORef ioRef 1)
                (S.fromEffect (throwM (ExampleException "E")))
        assert $ res == Left (ExampleException "E")
        refValue <- run $ readIORef ioRef
        assert $ refValue == 1

onExceptionProp :: Property
onExceptionProp =
    withNumTests maxTestCount $
    monadicIO $ do
        ioRef <- run $ newIORef (0 :: Int)
        res <-
            run $
            try . S.drain $
            S.onException
                (writeIORef ioRef 1)
                (S.fromEffect (throwM (ExampleException "E")))
        assert $ res == Left (ExampleException "E")
        refValue <- run $ readIORef ioRef
        assert $ refValue == 1

handleProp :: [Int] -> Property
handleProp vec =
    withNumTests maxTestCount $
    monadicIO $ do
        res <-
            run $
            S.toList $
            S.handle
                (\(ExampleException i) -> return (S.cons (read i) (S.fromList vec)))
                (S.append (S.fromList vec) (S.fromEffect (throwM (ExampleException "0"))))
        assert $ res == vec ++ [0] ++ vec

exceptionOps :: String -> Spec
exceptionOps desc = do
    prop (desc <> " before") beforeProp
    prop (desc <> " after") afterProp
    prop (desc <> " bracket end of stream") bracketProp
#ifdef INCLUDE_FLAKY_TESTS
    prop (desc <> " bracket partial stream") bracketPartialStreamProp
#endif
    prop (desc <> " bracket exception in stream") bracketExceptionProp
    prop (desc <> " onException") onExceptionProp
    prop (desc <> " finally end of stream") finallyProp
#ifdef INCLUDE_FLAKY_TESTS
    prop (desc <> " finally partial stream") finallyPartialStreamProp
#endif
    prop (desc <> " finally exception in stream") finallyExceptionProp
    prop (desc <> " handle") handleProp
    retry

-------------------------------------------------------------------------------
-- Compose with MonadThrow
-------------------------------------------------------------------------------

newtype ExampleException = ExampleException String deriving (Eq, Show, Ord)

instance Exception ExampleException

composeWithMonadThrow :: Spec
composeWithMonadThrow = do
    it "Compose throwM, nil" $
        try (tl (S.fromEffect (throwM (ExampleException "E"))))
        `shouldReturn` (Left (ExampleException "E") :: Either ExampleException [Int])
    it "Compose nil, throwM" $
        try (tl (S.fromEffect (throwM (ExampleException "E"))))
        `shouldReturn` (Left (ExampleException "E") :: Either ExampleException [Int])
    oneLevelNestedSum "serially" id
    -- XXX add two level nesting

    oneLevelNestedProduct "serially" id

    where
    tl = S.toList
    oneLevelNestedSum desc t1 =
        it ("One level nested sum " <> desc) $ do
            let nested = S.append (S.fromFoldable [1..10])
                           (S.append (S.fromEffect (throwM (ExampleException "E"))) (S.fromFoldable [1..10]))
            try (tl (S.append S.nil (S.append (t1 nested) (S.fromFoldable [1..10]))))
            `shouldReturn` (Left (ExampleException "E") :: Either ExampleException [Int])

    oneLevelNestedProduct desc t1 =
        it ("One level nested product" <> desc) $ do
            let s1 = S.fromFoldable [1..4]
                s2 = t1 $ S.fromFoldable [5..8]
            try $ tl $ S.concatMap (\x ->
                S.concatMap (\y ->
                    if x + y > 10
                    then S.fromEffect (throwM (ExampleException "E"))
                    else S.fromPure (x + y)) s2) s1
            `shouldReturn` (Left (ExampleException "E") :: Either ExampleException [Int])

-------------------------------------------------------------------------------
-- Cleanup tests
-------------------------------------------------------------------------------

checkCleanup :: Int -> (Stream IO Int -> Stream IO Int) -> IO ()
checkCleanup d op = do
    r <- newIORef (-1 :: Int)
    S.drain $ S.map (const ()) $ op $ S.consM (delay r 0) $ S.consM (delay r 1) $ S.consM (delay r 2) S.nil
    performMajorGC
    threadDelay 500000
    res <- readIORef r
    res `shouldBe` 0
    where
    delay ref i = threadDelay (i*d*100000) >> writeIORef ref i >> return i

-------------------------------------------------------------------------------
-- Some ad-hoc tests that failed at times
-------------------------------------------------------------------------------

takeCombined :: Int -> IO ()
takeCombined n = do
    let constr = S.fromFoldable
    r <- S.toList (S.take n (S.append (constr ([] :: [Int])) (constr ([] :: [Int]))))
    r `shouldBe` ([] :: [Int])

-------------------------------------------------------------------------------
-- Helper operations
-------------------------------------------------------------------------------

folded :: [a] -> Stream IO a
folded xs =
    case xs of
        [x] -> S.fromPure x
        _ -> S.fromFoldable xs

makeCommonOps :: b -> [(String, b)]
makeCommonOps t = [("default", t)]

makeOps :: b -> [(String, b)]
makeOps = makeCommonOps

mapOps :: (a -> Spec) -> [(String, a)] -> Spec
mapOps spec = mapM_ (\(desc, f) -> describe desc $ spec f)
