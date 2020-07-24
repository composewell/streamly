-- |
-- Module      : Streamly.Test.Prelude
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude
    (
    -- * Construction operations
      constructWithReplicate
    , constructWithReplicateM
    , constructWithIntFromThenTo
#if __GLASGOW_HASKELL__ >= 806
    , constructWithDoubleFromThenTo
#endif
    , constructWithIterate
    , constructWithIterateM
    , constructWithFromIndices
    , constructWithFromIndicesM
    -- * Applicative operations
    , applicativeOps
    , applicativeOps1
    -- * Elimination operations
    , eliminationOpsOrdered
    , eliminationOpsWord8
    , eliminationOps
    -- * Functor operations
    , functorOps
    -- * Semigroup operations
    , semigroupOps
    -- * Transformation operations
    , transformCombineOpsOrdered
    , transformCombineOpsCommon
    , toListFL
    -- * Monad operations
    , monadBind
    , monadThen
    -- * ZipOperations
    , zipApplicative
    , zipMonadic
    , zipAsyncMonadic
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

import Control.Applicative (ZipList(..), liftA2)
import Control.Monad (replicateM)
import Data.Function ((&))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List
    ( deleteBy
    , elemIndex
    , elemIndices
    , find
    , findIndex
    , findIndices
    , foldl'
    , foldl1'
    , insertBy
    , intersperse
    , isPrefixOf
    , isSubsequenceOf
    , maximumBy
    , minimumBy
    , scanl'
    , sort
    , stripPrefix
    )
import Data.Maybe (mapMaybe)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup ((<>))
#endif
import GHC.Word (Word8)
import Test.Hspec.QuickCheck
import Test.Hspec
import Test.QuickCheck (Property, choose, forAll, withMaxSuccess)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Streamly
import Streamly.Prelude ((.:), nil)
import Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

import Streamly.Test.Common

maxStreamLen :: Int
maxStreamLen = 1000

-- Coverage build takes too long with default number of tests
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

singleton :: IsStream t => a -> t m a
singleton a = a .: nil

sortEq :: Ord a => [a] -> [a] -> Bool
sortEq a b = sort a == sort b

-------------------------------------------------------------------------------
-- Construction operations
-------------------------------------------------------------------------------

constructWithLen
    :: (Show a, Eq a)
    => (Int -> t IO a)
    -> (Int -> [a])
    -> (t IO a -> SerialT IO a)
    -> Word8
    -> Property
constructWithLen mkStream mkList op len = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <- run $ (S.toList . op) (mkStream (fromIntegral len))
        let list = mkList (fromIntegral len)
        listEquals (==) stream list

constructWithLenM
    :: (Int -> t IO Int)
    -> (Int -> IO [Int])
    -> (t IO Int -> SerialT IO Int)
    -> Word8
    -> Property
constructWithLenM mkStream mkList op len = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <- run $ (S.toList . op) (mkStream (fromIntegral len))
        list <- run $ mkList (fromIntegral len)
        listEquals (==) stream list

constructWithReplicate, constructWithReplicateM, constructWithIntFromThenTo
    :: IsStream t
    => (t IO Int -> SerialT IO Int)
    -> Word8
    -> Property

constructWithReplicateM = constructWithLenM stream list
    where list = flip replicateM (return 1 :: IO Int)
          stream = flip S.replicateM (return 1 :: IO Int)

constructWithReplicate = constructWithLen stream list
    where list = flip replicate (1 :: Int)
          stream = flip S.replicate (1 :: Int)

constructWithIntFromThenTo op l =
    forAll (choose (minBound, maxBound)) $ \from ->
    forAll (choose (minBound, maxBound)) $ \next ->
    forAll (choose (minBound, maxBound)) $ \to ->
        let list len = take len [from,next..to]
            stream len = S.take len $ S.enumerateFromThenTo from next to
        in constructWithLen stream list op l

#if __GLASGOW_HASKELL__ >= 806
-- XXX try very small steps close to 0
constructWithDoubleFromThenTo
    :: IsStream t
    => (t IO Double -> SerialT IO Double)
    -> Word8
    -> Property
constructWithDoubleFromThenTo op l =
    forAll (choose (-9007199254740999,9007199254740999)) $ \from ->
    forAll (choose (-9007199254740999,9007199254740999)) $ \next ->
    forAll (choose (-9007199254740999,9007199254740999)) $ \to ->
        let list len = take len [from,next..to]
            stream len = S.take len $ S.enumerateFromThenTo from next to
        in constructWithLen stream list op l
#endif

constructWithIterate ::
       IsStream t => (t IO Int -> SerialT IO Int) -> Word8 -> Property
constructWithIterate op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <-
            run $
            (S.toList . op . S.take (fromIntegral len))
                (S.iterate (+ 1) (0 :: Int))
        let list = take (fromIntegral len) (iterate (+ 1) 0)
        listEquals (==) stream list

constructWithIterateM ::
       IsStream t => (t IO Int -> SerialT IO Int) -> Word8 -> Property
constructWithIterateM op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        mvl <- run (newIORef [] :: IO (IORef [Int]))
        let addM mv x y = modifyIORef' mv (++ [y + x]) >> return (y + x)
            list = take (fromIntegral len) (iterate (+ 1) 0)
        run $
            S.drain . op $
            S.take (fromIntegral len) $
            S.iterateM (addM mvl 1) (addM mvl 0 0 :: IO Int)
        streamEffect <- run $ readIORef mvl
        listEquals (==) streamEffect list

constructWithFromIndices ::
       IsStream t => (t IO Int -> SerialT IO Int) -> Word8 -> Property
constructWithFromIndices op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <-
            run $ (S.toList . op . S.take (fromIntegral len)) (S.fromIndices id)
        let list = take (fromIntegral len) (iterate (+ 1) 0)
        listEquals (==) stream list

constructWithFromIndicesM ::
       IsStream t => (t IO Int -> SerialT IO Int) -> Word8 -> Property
constructWithFromIndicesM op len =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        mvl <- run (newIORef [] :: IO (IORef [Int]))
        let addIndex mv i = modifyIORef' mv (++ [i]) >> return i
            list = take (fromIntegral len) (iterate (+ 1) 0)
        run $
            S.drain . op $
            S.take (fromIntegral len) $ S.fromIndicesM (addIndex mvl)
        streamEffect <- run $ readIORef mvl
        listEquals (==) streamEffect list

-------------------------------------------------------------------------------
-- Applicative operations
-------------------------------------------------------------------------------

applicativeOps
    :: Applicative (t IO)
    => ([Int] -> t IO Int)
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> (t IO (Int, Int) -> SerialT IO (Int, Int))
    -> ([Int], [Int])
    -> Property
applicativeOps constr eq t (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <- run ((S.toList . t) ((,) <$> constr a <*> constr b))
        let list = (,) <$> a <*> b
        listEquals eq stream list

        stream1 <- run ((S.toList . t) (liftA2 (,) (constr a) (constr b)))
        listEquals eq stream1 list

-- XXX we can combine this with applicativeOps by making the type sufficiently
-- polymorphic.
applicativeOps1
    :: Applicative (t IO)
    => ([Int] -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> ([Int], [Int])
    -> Property
applicativeOps1 constr eq t (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <- run ((S.toList . t) (constr a *> constr b))
        let list = a *> b
        listEquals eq stream list

        stream1 <- run ((S.toList . t) (constr a <* constr b))
        let list1 = a <* b
        listEquals eq stream1 list1

-------------------------------------------------------------------------------
-- Elimination operations
-------------------------------------------------------------------------------

eliminateOp
    :: (Show a, Eq a)
    => ([s] -> t IO s)
    -> ([s] -> a)
    -> (t IO s -> IO a)
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

eliminationOps
    :: ([Int] -> t IO Int)
    -> String
    -> (t IO Int -> SerialT IO Int)
    -> Spec
eliminationOps constr desc t = do
    -- Elimination
    prop (desc <> " null") $ eliminateOp constr null $ S.null . t
    prop (desc <> " foldl'") $
        eliminateOp constr (foldl' (+) 0) $ S.foldl' (+) 0 . t
    prop (desc <> " foldl1'") $
        eliminateOp constr (wrapMaybe $ foldl1' (+)) $ S.foldl1' (+) . t
#ifdef DEVBUILD
    prop (desc <> " foldr1") $
        eliminateOp constr (wrapMaybe $ foldr1 (+)) $ S.foldr1 (+) . t
#endif
    prop (desc <> " all") $ eliminateOp constr (all even) $ S.all even . t
    prop (desc <> " any") $ eliminateOp constr (any even) $ S.any even . t
    prop (desc <> " and") $ eliminateOp constr (and . fmap (> 0)) $
        (S.and . S.map (> 0)) . t
    prop (desc <> " or") $ eliminateOp constr (or . fmap (> 0)) $
        (S.or . S.map (> 0)) . t
    prop (desc <> " length") $ eliminateOp constr length $ S.length . t
    prop (desc <> " sum") $ eliminateOp constr sum $ S.sum . t
    prop (desc <> " product") $ eliminateOp constr product $ S.product . t

    prop (desc <> " maximum") $
        eliminateOp constr (wrapMaybe maximum) $ S.maximum . t
    prop (desc <> " minimum") $
        eliminateOp constr (wrapMaybe minimum) $ S.minimum . t

    prop (desc <> " maximumBy compare") $
        eliminateOp constr (wrapMaybe $ maximumBy compare) $
        S.maximumBy compare . t
    prop (desc <> " maximumBy flip compare") $
        eliminateOp constr (wrapMaybe $ maximumBy $ flip compare) $
        S.maximumBy (flip compare) . t
    prop (desc <> " minimumBy compare") $
        eliminateOp constr (wrapMaybe $ minimumBy compare) $
        S.minimumBy compare . t
    prop (desc <> " minimumBy flip compare") $
        eliminateOp constr (wrapMaybe $ minimumBy $ flip compare) $
        S.minimumBy (flip compare) . t

    prop (desc <> " findIndex") $
        eliminateOp constr (findIndex odd) $ S.findIndex odd . t
    prop (desc <> " elemIndex") $
        eliminateOp constr (elemIndex 3) $ S.elemIndex 3 . t

    prop (desc <> " !! 5") $
        eliminateOp constr (wrapOutOfBounds (!!) 5) $ (S.!! 5) . t
    prop (desc <> " !! 4") $
        eliminateOp constr (wrapOutOfBounds (!!) 0) $ (S.!! 0) . t

    prop (desc <> " find") $ eliminateOp constr (find even) $ S.find even . t
    prop (desc <> " lookup") $
        eliminateOp constr (lookup 3 . flip zip [1..]) $
            S.lookup 3 . S.zipWith (\a b -> (b, a)) (S.fromList [(1::Int)..]) . t
    prop (desc <> " the") $ eliminateOp constr wrapThe $ S.the . t

    -- Multi-stream eliminations
    -- Add eqBy, cmpBy
    -- XXX Write better tests for substreams.
    prop (desc <> " isPrefixOf 10") $ eliminateOp constr (isPrefixOf [1..10]) $
        S.isPrefixOf (S.fromList [(1::Int)..10]) . t
    prop (desc <> " isSubsequenceOf 10") $
        eliminateOp constr (isSubsequenceOf $ filter even [1..10]) $
        S.isSubsequenceOf (S.fromList $ filter even [(1::Int)..10]) . t
    prop (desc <> " stripPrefix 10") $ eliminateOp constr (stripPrefix [1..10]) $
        (\s -> s >>= maybe (return Nothing) (fmap Just . S.toList)) .
        S.stripPrefix (S.fromList [(1::Int)..10]) . t

-- head/tail/last may depend on the order in case of parallel streams
-- so we test these only for serial streams.
eliminationOpsOrdered
    :: ([Int] -> t IO Int)
    -> String
    -> (t IO Int -> SerialT IO Int)
    -> Spec
eliminationOpsOrdered constr desc t = do
    prop (desc <> " head") $ eliminateOp constr (wrapMaybe head) $ S.head . t
    prop (desc <> " tail") $ eliminateOp constr (wrapMaybe tail) $ \x -> do
        r <- S.tail (t x)
        case r of
            Nothing -> return Nothing
            Just s -> Just <$> S.toList s
    prop (desc <> " last") $ eliminateOp constr (wrapMaybe last) $ S.last . t
    prop (desc <> " init") $ eliminateOp constr (wrapMaybe init) $ \x -> do
        r <- S.init (t x)
        case r of
            Nothing -> return Nothing
            Just s -> Just <$> S.toList s

elemOp
    :: ([Word8] -> t IO Word8)
    -> (t IO Word8 -> SerialT IO Word8)
    -> (Word8 -> SerialT IO Word8 -> IO Bool)
    -> (Word8 -> [Word8] -> Bool)
    -> (Word8, [Word8])
    -> Property
elemOp constr op streamOp listOp (x, xs) =
    monadicIO $ do
        stream <- run $ (streamOp x . op) (constr xs)
        let list = listOp x xs
        equals (==) stream list

eliminationOpsWord8
    :: ([Word8] -> t IO Word8)
    -> String
    -> (t IO Word8 -> SerialT IO Word8)
    -> Spec
eliminationOpsWord8 constr desc t = do
    prop (desc <> " elem") $ elemOp constr t S.elem elem
    prop (desc <> " notElem") $ elemOp constr t S.notElem notElem

-------------------------------------------------------------------------------
-- Functor operations
-------------------------------------------------------------------------------

functorOps
    :: Functor (t IO)
    => ([Int] -> t IO Int)
    -> String
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> Spec
functorOps constr desc eq t = do
    prop (desc <> " id") $ transformFromList constr eq id t
    prop (desc <> " fmap (+1)") $ transformFromList constr eq (fmap (+1)) $ t . fmap (+1)

-------------------------------------------------------------------------------
-- Semigroup operations
-------------------------------------------------------------------------------

transformFromList
    :: (Eq b, Show b) =>
       ([a] -> t IO a)
    -> ([b] -> [b] -> Bool)
    -> ([a] -> [b])
    -> (t IO a -> SerialT IO b)
    -> [a]
    -> Property
transformFromList constr eq listOp op a =
    monadicIO $ do
        stream <- run ((S.toList . op) (constr a))
        let list = listOp a
        listEquals eq stream list

foldFromList
    :: ([Int] -> t IO Int)
    -> (t IO Int -> SerialT IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> [Int]
    -> Property
foldFromList constr op eq = transformFromList constr eq id op

-- XXX concatenate streams of multiple elements rather than single elements
semigroupOps
    :: (IsStream t

#if __GLASGOW_HASKELL__ < 804
       , Semigroup (t IO Int)
#endif
       , Monoid (t IO Int))
    => String
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> Spec
semigroupOps desc eq t = do
    prop (desc <> " <>") $ foldFromList (S.foldMapWith (<>) singleton) t eq
    prop (desc <> " mappend") $ foldFromList (S.foldMapWith mappend singleton) t eq

-------------------------------------------------------------------------------
-- Transformation operations
-------------------------------------------------------------------------------

transformCombineFromList
    :: Semigroup (t IO Int)
    => ([Int] -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> ([Int] -> [Int])
    -> (t IO Int -> SerialT IO Int)
    -> (t IO Int -> t IO Int)
    -> [Int]
    -> [Int]
    -> [Int]
    -> Property
transformCombineFromList constr eq listOp t op a b c =
    withMaxSuccess maxTestCount $
        monadicIO $ do
            stream <- run ((S.toList . t) $
                constr a <> op (constr b <> constr c))
            let list = a <> listOp (b <> c)
            listEquals eq stream list

-- XXX add tests for MonadReader and MonadError etc. In case an SVar is
-- accidentally passed through them.
--
-- This tests transform ops along with detecting illegal sharing of SVar across
-- conurrent streams. These tests work for all stream types whereas
-- transformCombineOpsOrdered work only for ordered stream types i.e. excluding
-- the Async type.
transformCombineOpsCommon
    :: (IsStream t, Semigroup (t IO Int))
    => ([Int] -> t IO Int)
    -> String
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> Spec
transformCombineOpsCommon constr desc eq t = do
    let transform = transformCombineFromList constr eq

    -- Filtering
    prop (desc <> " filter False") $
        transform (filter (const False)) t (S.filter (const False))
    prop (desc <> " filter True") $
        transform (filter (const True)) t (S.filter (const True))
    prop (desc <> " filter even") $
        transform (filter even) t (S.filter even)

    prop (desc <> " filterM False") $
        transform (filter (const False)) t (S.filterM (const $ return False))
    prop (desc <> " filterM True") $
        transform (filter (const True)) t (S.filterM (const $ return True))
    prop (desc <> " filterM even") $
        transform (filter even) t (S.filterM (return . even))

    prop (desc <> " take maxBound") $
        transform (take maxBound) t (S.take maxBound)
    prop (desc <> " take 0") $ transform (take 0) t (S.take 0)

    prop (desc <> " takeWhile True") $
        transform (takeWhile (const True)) t (S.takeWhile (const True))
    prop (desc <> " takeWhile False") $
        transform (takeWhile (const False)) t (S.takeWhile (const False))

    prop (desc <> " takeWhileM True") $
        transform (takeWhile (const True)) t (S.takeWhileM (const $ return True))
    prop (desc <> " takeWhileM False") $
        transform (takeWhile (const False)) t (S.takeWhileM (const $ return False))

    prop (desc <> " drop maxBound") $
        transform (drop maxBound) t (S.drop maxBound)
    prop (desc <> " drop 0") $ transform (drop 0) t (S.drop 0)

    prop (desc <> " dropWhile True") $
        transform (dropWhile (const True)) t (S.dropWhile (const True))
    prop (desc <> " dropWhile False") $
        transform (dropWhile (const False)) t (S.dropWhile (const False))

    prop (desc <> " dropWhileM True") $
        transform (dropWhile (const True)) t (S.dropWhileM (const $ return True))
    prop (desc <> " dropWhileM False") $
        transform (dropWhile (const False)) t (S.dropWhileM (const $ return False))

    prop (desc <> " deleteBy (<=) maxBound") $
        transform (deleteBy (<=) maxBound) t (S.deleteBy (<=) maxBound)
    prop (desc <> " deleteBy (==) 4") $
        transform (deleteBy (==) 4) t (S.deleteBy (==) 4)

    -- transformation
    prop (desc <> " mapM (+1)") $
        transform (fmap (+1)) t (S.mapM (\x -> return (x + 1)))

    prop (desc <> " scanl'") $ transform (scanl' (flip const) 0) t
                                       (S.scanl' (flip const) 0)
    prop (desc <> " scanlM'") $ transform (scanl' (flip const) 0) t
                                       (S.scanlM' (\_ a -> return a) 0)
    prop (desc <> " scanl") $ transform (scanl' (flip const) 0) t
                                       (S.scanl' (flip const) 0)
    prop (desc <> " scanl1'") $ transform (scanl1 (flip const)) t
                                         (S.scanl1' (flip const))
    prop (desc <> " scanl1M'") $ transform (scanl1 (flip const)) t
                                          (S.scanl1M' (\_ a -> return a))

    let f x = if odd x then Just (x + 100) else Nothing
    prop (desc <> " mapMaybe") $ transform (mapMaybe f) t (S.mapMaybe f)

    -- tap
    prop (desc <> " tap FL.sum . map (+1)") $ \a b ->
        withMaxSuccess maxTestCount $
        monadicIO $ do
            cref <- run $ newIORef 0
            let sumfoldinref = FL.Fold (\_ e -> modifyIORef' cref (e+))
                                       (return ())
                                       (const $ return ())
                op = S.tap sumfoldinref . S.mapM (\x -> return (x+1))
                listOp = fmap (+1)
            stream <- run ((S.toList . t) $ op (constr a <> constr b))
            let list = listOp (a <> b)
            ssum <- run $ readIORef cref
            assert (sum list == ssum)
            listEquals eq stream list

    -- reordering
    prop (desc <> " reverse") $ transform reverse t S.reverse
    -- prop (desc <> " reverse'") $ transform reverse t S.reverse'

    -- inserting
    prop (desc <> " intersperseM") $
        forAll (choose (minBound, maxBound)) $ \n ->
            transform (intersperse n) t (S.intersperseM $ return n)
    prop (desc <> " insertBy 0") $
        forAll (choose (minBound, maxBound)) $ \n ->
            transform (insertBy compare n) t (S.insertBy compare n)

    -- multi-stream
    prop (desc <> " concatMap") $
        forAll (choose (0, 100)) $ \n ->
            transform (concatMap (const [1..n]))
                t (S.concatMap (const (S.fromList [1..n])))

toListFL :: Monad m => FL.Fold m a [a]
toListFL = FL.toList

-- transformation tests that can only work reliably for ordered streams i.e.
-- Serial, Ahead and Zip. For example if we use "take 1" on an async stream, it
-- might yield a different result every time.
transformCombineOpsOrdered
    :: (IsStream t, Semigroup (t IO Int))
    => ([Int] -> t IO Int)
    -> String
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> Spec
transformCombineOpsOrdered constr desc eq t = do
    let transform = transformCombineFromList constr eq

    -- Filtering
    prop (desc <> " take 1") $ transform (take 1) t (S.take 1)
#ifdef DEVBUILD
    prop (desc <> " take 2") $ transform (take 2) t (S.take 2)
    prop (desc <> " take 3") $ transform (take 3) t (S.take 3)
    prop (desc <> " take 4") $ transform (take 4) t (S.take 4)
    prop (desc <> " take 5") $ transform (take 5) t (S.take 5)
#endif
    prop (desc <> " take 10") $ transform (take 10) t (S.take 10)

    prop (desc <> " takeWhile > 0") $
        transform (takeWhile (> 0)) t (S.takeWhile (> 0))

    prop (desc <> " drop 1") $ transform (drop 1) t (S.drop 1)
    prop (desc <> " drop 10") $ transform (drop 10) t (S.drop 10)

    prop (desc <> " dropWhile > 0") $
        transform (dropWhile (> 0)) t (S.dropWhile (> 0))
    prop (desc <> " scan") $ transform (scanl' (+) 0) t (S.scanl' (+) 0)

    prop (desc <> " uniq") $ transform referenceUniq t S.uniq

    prop (desc <> " deleteBy (<=) 0") $
        transform (deleteBy (<=) 0) t (S.deleteBy (<=) 0)

    prop (desc <> " findIndices") $
        transform (findIndices odd) t (S.findIndices odd)
    prop (desc <> " findIndices . filter") $
        transform (findIndices odd . filter odd)
                  t
                  (S.findIndices odd . S.filter odd)
    prop (desc <> " elemIndices") $
        transform (elemIndices 0) t (S.elemIndices 0)

    -- XXX this does not fail when the SVar is shared, need to fix.
    prop (desc <> " concurrent application") $
        transform (& fmap (+1)) t (|& S.map (+1))

-------------------------------------------------------------------------------
-- Monad operations
-------------------------------------------------------------------------------

monadThen
    :: Monad (t IO)
    => ([Int] -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> ([Int], [Int])
    -> Property
monadThen constr eq t (a, b) = withMaxSuccess maxTestCount $ monadicIO $ do
    stream <- run ((S.toList . t) (constr a >> constr b))
    let list = a >> b
    listEquals eq stream list

monadBind
    :: Monad (t IO)
    => ([Int] -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> ([Int], [Int])
    -> Property
monadBind constr eq t (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <-
            run
                ((S.toList . t)
                     (constr a >>= \x -> (+ x) <$> constr b))
        let list = a >>= \x -> (+ x) <$> b
        listEquals eq stream list

-------------------------------------------------------------------------------
-- Zip operations
-------------------------------------------------------------------------------

zipApplicative
    :: (IsStream t, Applicative (t IO))
    => ([Int] -> t IO Int)
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> (t IO (Int, Int) -> SerialT IO (Int, Int))
    -> ([Int], [Int])
    -> Property
zipApplicative constr eq t (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream1 <- run ((S.toList . t) ((,) <$> constr a <*> constr b))
        stream2 <- run ((S.toList . t) (pure (,) <*> constr a <*> constr b))
        stream3 <- run ((S.toList . t) (S.zipWith (,) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        listEquals eq stream1 list
        listEquals eq stream2 list
        listEquals eq stream3 list

zipMonadic
    :: IsStream t
    => ([Int] -> t IO Int)
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> (t IO (Int, Int) -> SerialT IO (Int, Int))
    -> ([Int], [Int])
    -> Property
zipMonadic constr eq t (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream1 <-
            run
                ((S.toList . t)
                     (S.zipWithM (curry return) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        listEquals eq stream1 list

zipAsyncMonadic
    :: IsStream t
    => ([Int] -> t IO Int)
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> (t IO (Int, Int) -> SerialT IO (Int, Int))
    -> ([Int], [Int])
    -> Property
zipAsyncMonadic constr eq t (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream1 <-
            run
                ((S.toList . t)
                     (S.zipWithM (curry return) (constr a) (constr b)))
        stream2 <-
            run
                ((S.toList . t)
                     (S.zipAsyncWithM (curry return) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        listEquals eq stream1 list
        listEquals eq stream2 list

-------------------------------------------------------------------------------
-- Helper operations
-------------------------------------------------------------------------------

folded :: IsStream t => [a] -> t IO a
folded =
    serially .
    (\xs ->
         case xs of
             [x] -> return x -- singleton stream case
             _ -> S.foldMapWith (<>) return xs)

makeCommonOps :: IsStream t => (t m a -> c) -> [(String, t m a -> c)]
makeCommonOps t =
            [ ("default", t)
#ifndef COVERAGE_BUILD
            , ("rate AvgRate 10000", t . avgRate 10000)
            , ("rate Nothing", t . rate Nothing)
            , ("maxBuffer 0", t . maxBuffer 0)
            , ("maxThreads 0", t . maxThreads 0)
            , ("maxThreads 1", t . maxThreads 1)
            , ("maxThreads -1", t . maxThreads (-1))
#endif
            ]

makeOps :: IsStream t => (t m a -> c) -> [(String, t m a -> c)]
makeOps t = makeCommonOps t ++
            [
#ifndef COVERAGE_BUILD
              ("maxBuffer 1", t . maxBuffer 1)
#endif
            ]

mapOps :: (a -> Spec) -> [(String, a)] -> Spec
mapOps spec = mapM_ (\(desc, f) -> describe desc $ spec f)
