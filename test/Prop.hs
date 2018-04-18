{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Monad (when)
import Control.Applicative (ZipList(..))
import Control.Monad (replicateM)
import Data.List (sort, foldl', scanl')
import GHC.Word (Word8)

import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample, Property)
import Test.QuickCheck.Monadic (run, monadicIO, monitor, assert, PropertyM)

import Test.Hspec

import Streamly
import Streamly.Prelude ((.:), nil)
import qualified Streamly.Prelude as A

singleton :: IsStream t => a -> t m a
singleton a = a .: nil

sortEq :: Ord a => [a] -> [a] -> Bool
sortEq a b = sort a == sort b

equals
    :: (Show a, Monad m)
    => (a -> a -> Bool) -> a -> a -> PropertyM m ()
equals eq stream list = do
    when (not $ stream `eq` list) $
        monitor
            (counterexample $
             "stream " ++ show stream ++ " /= list " ++ show list)
    assert (stream `eq` list)

constructWithReplicateM
    :: IsStream t
    => (t IO Int -> SerialT IO Int)
    -> Word8
    -> Property
constructWithReplicateM op len =
    monadicIO $ do
        let x = return (1 :: Int)
        stream <- run $ (A.toList . op) (A.replicateM (fromIntegral len) x)
        list <- run $ replicateM (fromIntegral len) x
        equals (==) stream list

transformFromList
    :: ([Int] -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> ([Int] -> [Int])
    -> (t IO Int -> SerialT IO Int)
    -> [Int]
    -> Property
transformFromList constr eq listOp op a =
    monadicIO $ do
        stream <- run ((A.toList . op) (constr a))
        let list = listOp a
        equals eq stream list

foldFromList
    :: ([Int] -> t IO Int)
    -> (t IO Int -> SerialT IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> [Int]
    -> Property
foldFromList constr op eq a = transformFromList constr eq id op a

eliminateOp
    :: (Show a, Eq a)
    => ([Int] -> t IO Int)
    -> ([Int] -> a)
    -> (t IO Int -> IO a)
    -> [Int]
    -> Property
eliminateOp constr listOp op a =
    monadicIO $ do
        stream <- run $ op (constr a)
        let list = listOp a
        equals (==) stream list

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

functorOps
    :: Functor (t IO)
    => ([Int] -> t IO Int)
    -> String
    -> (t IO Int -> SerialT IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> Spec
functorOps constr desc t eq = do
    prop (desc ++ " id") $ transformFromList constr eq id $ t
    prop (desc ++ " fmap (+1)") $ transformFromList constr eq (fmap (+1)) $ t . (fmap (+1))

transformOps
    :: IsStream t
    => ([Int] -> t IO Int)
    -> String
    -> (t IO Int -> SerialT IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> Spec
transformOps constr desc t eq = do
    let transform = transformFromList constr eq
    -- Filtering
    prop (desc ++ " filter False") $
        transform (filter (const False)) $ t . (A.filter (const False))
    prop (desc ++ " filter True") $
        transform (filter (const True)) $ t . (A.filter (const True))
    prop (desc ++ " filter even") $
        transform (filter even) $ t . (A.filter even)

    prop (desc ++ " take maxBound") $
        transform (take maxBound) $ t . (A.take maxBound)
    prop (desc ++ " take 0") $ transform (take 0) $ t . (A.take 0)
    prop (desc ++ " take 1") $ transform (take 1) $ t . (A.take 1)
    prop (desc ++ " take 10") $ transform (take 10) $ t . (A.take 10)

    prop (desc ++ " takeWhile True") $
        transform (takeWhile (const True)) $ t . (A.takeWhile (const True))
    prop (desc ++ " takeWhile False") $
        transform (takeWhile (const False)) $ t . (A.takeWhile (const False))
    prop (desc ++ " takeWhile > 0") $
        transform (takeWhile (> 0)) $ t . (A.takeWhile (> 0))

    prop (desc ++ " drop maxBound") $
        transform (drop maxBound) $ t . (A.drop maxBound)
    prop (desc ++ " drop 0") $ transform (drop 0) $ t . (A.drop 0)
    prop (desc ++ " drop 1") $ transform (drop 1) $ t . (A.drop 1)
    prop (desc ++ " drop 10") $ transform (drop 10) $ t . (A.drop 10)

    prop (desc ++ " dropWhile True") $
        transform (dropWhile (const True)) $ t . (A.dropWhile (const True))
    prop (desc ++ " dropWhile False") $
        transform (dropWhile (const False)) $ t . (A.dropWhile (const False))
    prop (desc ++ " dropWhile > 0") $
        transform (dropWhile (> 0)) $ t . (A.dropWhile (> 0))
    prop (desc ++ " scan") $ transform (scanl' (+) 0) $ t . (A.scanl' (+) 0)
    prop (desc ++ "reverse") $ transform reverse $ t . A.reverse

wrapMaybe :: Eq a1 => ([a1] -> a2) -> [a1] -> Maybe a2
wrapMaybe f =
    \x ->
        if x == []
            then Nothing
            else Just (f x)

eliminationOps
    :: ([Int] -> t IO Int)
    -> String
    -> (t IO Int -> SerialT IO Int)
    -> Spec
eliminationOps constr desc t = do
    -- Elimination
    prop (desc ++ " null") $ eliminateOp constr null $ A.null . t
    prop (desc ++ " foldl") $
        eliminateOp constr (foldl' (+) 0) $ (A.foldl' (+) 0) . t
    prop (desc ++ " all") $ eliminateOp constr (all even) $ (A.all even) . t
    prop (desc ++ " any") $ eliminateOp constr (any even) $ (A.any even) . t
    prop (desc ++ " length") $ eliminateOp constr length $ A.length . t
    prop (desc ++ " sum") $ eliminateOp constr sum $ A.sum . t
    prop (desc ++ " product") $ eliminateOp constr product $ A.product . t

    prop (desc ++ " maximum") $ eliminateOp constr (wrapMaybe maximum) $ A.maximum . t
    prop (desc ++ " minimum") $ eliminateOp constr (wrapMaybe minimum) $ A.minimum . t

-- head/tail/last may depend on the order in case of parallel streams
-- so we test these only for serial streams.
serialEliminationOps
    :: ([Int] -> t IO Int)
    -> String
    -> (t IO Int -> SerialT IO Int)
    -> Spec
serialEliminationOps constr desc t = do
    prop (desc ++ " head") $ eliminateOp constr (wrapMaybe head) $ A.head . t
    prop (desc ++ " tail") $ eliminateOp constr (wrapMaybe tail) $ \x -> do
        r <- A.tail (t x)
        case r of
            Nothing -> return Nothing
            Just s -> A.toList s >>= return . Just
    prop (desc ++ " last") $ eliminateOp constr (wrapMaybe last) $ A.last . t

transformOpsWord8
    :: ([Word8] -> t IO Word8)
    -> String
    -> (t IO Word8 -> SerialT IO Word8)
    -> Spec
transformOpsWord8 constr desc t = do
    prop (desc ++ " elem") $ elemOp constr t A.elem elem
    prop (desc ++ " elem") $ elemOp constr t A.notElem notElem

-- XXX concatenate streams of multiple elements rather than single elements
semigroupOps
    :: (IsStream t

#if __GLASGOW_HASKELL__ < 804
       , Semigroup (t IO Int)
#endif
       , Monoid (t IO Int))
    => String
    -> (t IO Int -> SerialT IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> Spec
semigroupOps desc t eq = do
    prop (desc ++ " <>") $ foldFromList (foldMapWith (<>) singleton) t eq
    prop (desc ++ " mappend") $ foldFromList (foldMapWith mappend singleton) t eq

applicativeOps
    :: Applicative (t IO)
    => ([Int] -> t IO Int)
    -> (t IO (Int, Int) -> SerialT IO (Int, Int))
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> ([Int], [Int])
    -> Property
applicativeOps constr t eq (a, b) = monadicIO $ do
    stream <- run ((A.toList . t) ((,) <$> (constr a) <*> (constr b)))
    let list = (,) <$> a <*> b
    equals eq stream list

zipApplicative
    :: (IsStream t, Applicative (t IO))
    => ([Int] -> t IO Int)
    -> (t IO (Int, Int) -> SerialT IO (Int, Int))
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> ([Int], [Int])
    -> Property
zipApplicative constr t eq (a, b) = monadicIO $ do
    stream1 <- run ((A.toList . t) ((,) <$> (constr a) <*> (constr b)))
    stream2 <- run ((A.toList . t) (pure (,) <*> (constr a) <*> (constr b)))
    stream3 <- run ((A.toList . t) (A.zipWith (,) (constr a) (constr b)))
    let list = getZipList $ (,) <$> ZipList a <*> ZipList b
    equals eq stream1 list
    equals eq stream2 list
    equals eq stream3 list

zipMonadic
    :: (IsStream t, Monad (t IO))
    => ([Int] -> t IO Int)
    -> (t IO (Int, Int) -> SerialT IO (Int, Int))
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> ([Int], [Int])
    -> Property
zipMonadic constr t eq (a, b) =
    monadicIO $ do
        stream1 <-
            run
                ((A.toList . t)
                     (A.zipWithM (\x y -> return (x, y)) (constr a) (constr b)))
        stream2 <-
            run
                ((A.toList . t)
                     (A.zipParallelWithM (\x y -> return (x, y)) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        equals eq stream1 list
        equals eq stream2 list

monadThen
    :: Monad (t IO)
    => ([Int] -> t IO Int)
    -> (t IO Int -> SerialT IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> ([Int], [Int])
    -> Property
monadThen constr t eq (a, b) = monadicIO $ do
    stream <- run ((A.toList . t) ((constr a) >> (constr b)))
    let list = a >> b
    equals eq stream list

monadBind
    :: Monad (t IO)
    => ([Int] -> t IO Int)
    -> (t IO Int -> SerialT IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> ([Int], [Int])
    -> Property
monadBind constr t eq (a, b) =
    monadicIO $ do
        stream <-
            run
                ((A.toList . t)
                     ((constr a) >>= \x -> (constr b) >>= return . (+ x)))
        let list = a >>= \x -> b >>= return . (+ x)
        equals eq stream list

main :: IO ()
main = hspec $ do
    describe "Construction" $ do
        -- XXX test for all types of streams
        prop "serially replicateM" $ constructWithReplicateM serially
        it "iterate" $
            (A.toList . serially . (A.take 100) $ (A.iterate (+ 1) (0 :: Int)))
            `shouldReturn` (take 100 $ iterate (+ 1) 0)

        it "iterateM" $ do
            let addM = (\ y -> return (y + 1))
            A.toList . serially . (A.take 100) $ A.iterateM addM (0 :: Int)
            `shouldReturn` (take 100 $ iterate (+ 1) 0)

    let folded :: IsStream t => [a] -> t IO a
        folded = serially . (\xs ->
            case xs of
                [x] -> return x -- singleton stream case
                _ -> foldMapWith (<>) return xs
            )
    describe "Functor operations" $ do
        functorOps A.fromFoldable "serially" serially (==)
        functorOps folded "serially folded" serially (==)
        functorOps A.fromFoldable "coserially" coserially (==)
        functorOps folded "coserially folded" coserially (==)
        functorOps A.fromFoldable "coparallely" coparallely sortEq
        functorOps folded "coparallely folded" coparallely sortEq
        functorOps A.fromFoldable "parallely" parallely sortEq
        functorOps folded "parallely folded" parallely sortEq
        functorOps A.fromFoldable "zipSerially" zipSerially (==)
        functorOps folded "zipSerially folded" zipSerially (==)
        functorOps A.fromFoldable "zipParallely" zipParallely (==)
        functorOps folded "zipParallely folded" zipParallely (==)

    describe "Semigroup operations" $ do
        semigroupOps "serially" serially (==)
        semigroupOps "coserially" coserially (==)
        semigroupOps "coparallely" coparallely sortEq
        semigroupOps "parallely" parallely sortEq
        semigroupOps "zipSerially" zipSerially (==)
        semigroupOps "zipParallely" zipParallely (==)

    describe "Applicative operations" $ do
        -- The tests using sorted equality are weaker tests
        -- We need to have stronger unit tests for all those
        -- XXX applicative with three arguments
        prop "serially applicative" $ applicativeOps A.fromFoldable serially (==)
        prop "serially applicative folded" $ applicativeOps folded serially (==)
        prop "coserially applicative" $ applicativeOps A.fromFoldable coserially sortEq
        prop "coserially applicative folded" $ applicativeOps folded coserially sortEq
        prop "coparallely applicative" $ applicativeOps A.fromFoldable coparallely sortEq
        prop "coparallely applicative folded" $ applicativeOps folded coparallely sortEq
        prop "parallely applicative folded" $ applicativeOps folded parallely sortEq

    describe "Zip operations" $ do
        prop "zipSerially applicative" $ zipApplicative A.fromFoldable zipSerially (==)
        -- XXX this hangs
        -- prop "zipParallely applicative" $ zipApplicative zipParallely (==)
        prop "zip monadic serially" $ zipMonadic A.fromFoldable serially (==)
        prop "zip monadic serially folded" $ zipMonadic folded serially (==)
        prop "zip monadic coserially" $ zipMonadic A.fromFoldable coserially (==)
        prop "zip monadic coserially folded" $ zipMonadic folded coserially (==)
        prop "zip monadic coparallely" $ zipMonadic A.fromFoldable coparallely (==)
        prop "zip monadic coparallely folded" $ zipMonadic folded coparallely (==)
        prop "zip monadic parallely" $ zipMonadic A.fromFoldable parallely (==)
        prop "zip monadic parallely folded" $ zipMonadic folded parallely (==)

    describe "Monad operations" $ do
        prop "serially monad then" $ monadThen A.fromFoldable serially (==)
        prop "coserially monad then" $ monadThen A.fromFoldable coserially sortEq
        prop "coparallely monad then" $ monadThen A.fromFoldable coparallely sortEq
        prop "parallely monad then" $ monadThen A.fromFoldable parallely sortEq

        prop "serially monad then folded" $ monadThen folded serially (==)
        prop "coserially monad then folded" $ monadThen folded coserially sortEq
        prop "coparallely monad then folded" $ monadThen folded coparallely sortEq
        prop "parallely monad then folded" $ monadThen folded parallely sortEq

        prop "serially monad bind" $ monadBind A.fromFoldable serially (==)
        prop "coserially monad bind" $ monadBind A.fromFoldable coserially sortEq
        prop "coparallely monad bind" $ monadBind A.fromFoldable coparallely sortEq
        prop "parallely monad bind" $ monadBind A.fromFoldable parallely sortEq

    describe "Stream transform operations" $ do
        transformOps A.fromFoldable "serially" serially (==)
        transformOps A.fromFoldable "coserially" coserially (==)
        transformOps A.fromFoldable "zipSerially" zipSerially (==)
        transformOps A.fromFoldable "zipParallely" zipParallely (==)
        transformOps A.fromFoldable "coparallely" coparallely sortEq
        transformOps A.fromFoldable "parallely" parallely sortEq

        transformOps folded "serially folded" serially (==)
        transformOps folded "coserially folded" coserially (==)
        transformOps folded "zipSerially folded" zipSerially (==)
        transformOps folded "zipParallely folded" zipParallely (==)
        transformOps folded "coparallely folded" coparallely sortEq
        transformOps folded "parallely folded" parallely sortEq

        transformOpsWord8 A.fromFoldable "serially" serially
        transformOpsWord8 A.fromFoldable "coserially" coserially
        transformOpsWord8 A.fromFoldable "zipSerially" zipSerially
        transformOpsWord8 A.fromFoldable "zipParallely" zipParallely
        transformOpsWord8 A.fromFoldable "coparallely" coparallely
        transformOpsWord8 A.fromFoldable "parallely" parallely

        transformOpsWord8 folded "serially folded" serially
        transformOpsWord8 folded "coserially folded" coserially
        transformOpsWord8 folded "zipSerially folded" zipSerially
        transformOpsWord8 folded "zipParallely folded" zipParallely
        transformOpsWord8 folded "coparallely folded" coparallely
        transformOpsWord8 folded "parallely folded" parallely

    describe "Stream elimination operations" $ do
        eliminationOps A.fromFoldable "serially" serially
        eliminationOps A.fromFoldable "coserially" coserially
        eliminationOps A.fromFoldable "zipSerially" zipSerially
        eliminationOps A.fromFoldable "zipParallely" zipParallely
        eliminationOps A.fromFoldable "coparallely" coparallely
        eliminationOps A.fromFoldable "parallely" parallely

        eliminationOps folded "serially folded" serially
        eliminationOps folded "coserially folded" coserially
        eliminationOps folded "zipSerially folded" zipSerially
        eliminationOps folded "zipParallely folded" zipParallely
        eliminationOps folded "coparallely folded" coparallely
        eliminationOps folded "parallely folded" parallely

    describe "Stream elimination operations" $ do
        serialEliminationOps A.fromFoldable "serially" serially
        serialEliminationOps A.fromFoldable "coserially" coserially
        serialEliminationOps A.fromFoldable "zipSerially" zipSerially
        serialEliminationOps A.fromFoldable "zipParallely" zipParallely

        serialEliminationOps folded "serially folded" serially
        serialEliminationOps folded "coserially folded" coserially
        serialEliminationOps folded "zipSerially folded" zipSerially
        serialEliminationOps folded "zipParallely folded" zipParallely
