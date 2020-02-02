{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (readIORef, newIORef, modifyIORef')
import Data.List (sort, scanl', insertBy, intersperse, (\\), deleteBy)
import Data.Maybe (mapMaybe)

import Test.Hspec.QuickCheck
import Test.QuickCheck
       (counterexample, Property, withMaxSuccess, forAll, choose)
import Test.QuickCheck.Monadic (run, monadicIO, monitor, assert, PropertyM)

import Test.Hspec as H

import Streamly (SerialT, AsyncT, IsStream, maxThreads, maxBuffer, asyncly,
    serially, avgRate, rate)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

maxTestCount :: Int
maxTestCount = 100

sortEq :: Ord a => [a] -> [a] -> Bool
sortEq a b = sort a == sort b

listEquals
    :: (Show a, Eq a, MonadIO m)
    => ([a] -> [a] -> Bool) -> [a] -> [a] -> PropertyM m ()
listEquals eq stream list = do
    when (not $ stream `eq` list) $ liftIO $ putStrLn $
                  "stream " <> show stream
             <> "\nlist   " <> show list
             <> "\nstream \\\\ list " <> show (stream \\ list)
             <> "\nlist \\\\ stream " <> show (list \\ stream)
    when (not $ stream `eq` list) $
        monitor
            (counterexample $
                  "stream " <> show stream
             <> "\nlist   " <> show list
             <> "\nstream \\\\ list " <> show (stream \\ list)
             <> "\nlist \\\\ stream " <> show (list \\ stream)
             )
    assert (stream `eq` list)

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

main :: IO ()
main = hspec
    $ H.parallel
    $ do
    let makeCommonOps :: IsStream t => (t m a -> c) -> [(String, t m a -> c)]
        makeCommonOps t =
            [ ("default", t)
            , ("rate AvgRate 10000", t . avgRate 10000)
            , ("rate Nothing", t . rate Nothing)
            , ("maxBuffer 0", t . maxBuffer 0)
            , ("maxThreads 0", t . maxThreads 0)
            , ("maxThreads 1", t . maxThreads 1)
            , ("maxThreads -1", t . maxThreads (-1))
            ]

    let makeOps :: IsStream t => (t m a -> c) -> [(String, t m a -> c)]
        makeOps t = makeCommonOps t ++
            [
              ("maxBuffer 1", t . maxBuffer 1)
            ]

    let mapOps spec = mapM_ (\(desc, f) -> describe desc $ spec f)
    let serialOps :: IsStream t => ((SerialT IO a -> t IO a) -> Spec) -> Spec
        serialOps spec = mapOps spec $ makeOps serially
            <> [("rate AvgRate 0.00000001", serially . avgRate 0.00000001)]
            <> [("maxBuffer -1", serially . maxBuffer (-1))]
    let asyncOps :: IsStream t => ((AsyncT IO a -> t IO a) -> Spec) -> Spec
        asyncOps spec = mapOps spec $ makeOps asyncly
            <> [("maxBuffer (-1)", asyncly . maxBuffer (-1))]

    describe "Applicative operations" $ do
        asyncOps    $ prop "asyncly applicative" . applicativeOps S.fromFoldable sortEq
        asyncOps    $ prop "asyncly applicative" . applicativeOps S.fromFoldable sortEq
        asyncOps    $ prop "asyncly applicative" . applicativeOps S.fromFoldable sortEq
        asyncOps    $ prop "asyncly applicative" . applicativeOps S.fromFoldable sortEq
        asyncOps    $ prop "asyncly applicative" . applicativeOps S.fromFoldable sortEq

    describe "Stream transform and combine operations" $ do
        serialOps    $ transformCombineOpsCommon S.fromFoldable "serially" (==)
        serialOps    $ transformCombineOpsCommon S.fromFoldable "serially" (==)
        serialOps    $ transformCombineOpsCommon S.fromFoldable "serially" (==)
        serialOps    $ transformCombineOpsCommon S.fromFoldable "serially" (==)
        serialOps    $ transformCombineOpsCommon S.fromFoldable "serially" (==)
        serialOps    $ transformCombineOpsCommon S.fromFoldable "serially" (==)
        serialOps    $ transformCombineOpsCommon S.fromFoldable "serially" (==)
        serialOps    $ transformCombineOpsCommon S.fromFoldable "serially" (==)
        serialOps    $ transformCombineOpsCommon S.fromFoldable "serially" (==)
        serialOps    $ transformCombineOpsCommon S.fromFoldable "serially" (==)
        serialOps    $ transformCombineOpsCommon S.fromFoldable "serially" (==)
