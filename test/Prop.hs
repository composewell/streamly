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
import qualified Streamly.Prelude as A

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
    => (t IO Int -> t IO Int)
    -> Word8
    -> Property
constructWithReplicateM op len =
    monadicIO $ do
        let x = return (1 :: Int)
        stream <- run $ (A.toList . op) (A.replicateM (fromIntegral len) x)
        list <- run $ replicateM (fromIntegral len) x
        equals (==) stream list

transformFromList
    :: IsStream t
    => ([Int] -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> ([Int] -> [Int])
    -> (t IO Int -> t IO Int)
    -> [Int]
    -> Property
transformFromList constr eq listOp op a =
    monadicIO $ do
        stream <- run ((A.toList . op) (constr a))
        let list = listOp a
        equals eq stream list

foldFromList
    :: IsStream t
    => ([Int] -> t IO Int)
    -> (t IO Int -> t IO Int)
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
    -> (t IO Word8 -> t IO Word8)
    -> (Word8 -> t IO Word8 -> IO Bool)
    -> (Word8 -> [Word8] -> Bool)
    -> (Word8, [Word8])
    -> Property
elemOp constr op streamOp listOp (x, xs) =
    monadicIO $ do
        stream <- run $ (streamOp x . op) (constr xs)
        let list = listOp x xs
        equals (==) stream list

functorOps
    :: (IsStream t, Functor (t IO))
    => ([Int] -> t IO Int)
    -> String
    -> (t IO Int -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> Spec
functorOps constr desc t eq = do
    prop (desc ++ " id") $ transformFromList constr eq id $ t
    prop (desc ++ " fmap (+1)") $ transformFromList constr eq (fmap (+1)) $ t . (fmap (+1))

transformOps
    :: IsStream t
    => ([Int] -> t IO Int)
    -> String
    -> (t IO Int -> t IO Int)
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
    :: IsStream t
    => ([Int] -> t IO Int)
    -> String
    -> (t IO Int -> t IO Int)
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
    :: IsStream t
    => ([Int] -> t IO Int)
    -> String
    -> (t IO Int -> t IO Int)
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
    :: IsStream t
    => ([Word8] -> t IO Word8)
    -> String
    -> (t IO Word8 -> t IO Word8)
    -> Spec
transformOpsWord8 constr desc t = do
    prop (desc ++ " elem") $ elemOp constr t A.elem elem
    prop (desc ++ " elem") $ elemOp constr t A.notElem notElem

-- XXX concatenate streams of multiple elements rather than single elements
semigroupOps
    :: (IsStream t, MonadPlus (t IO)

#if __GLASGOW_HASKELL__ < 804
       , Semigroup (t IO Int)
#endif
       , Monoid (t IO Int))
    => String -> (t IO Int -> t IO Int) -> Spec
semigroupOps desc t = do
    prop (desc ++ " <>") $ foldFromList (foldMapWith (<>) return) t (==)
    prop (desc ++ " mappend") $ foldFromList (foldMapWith mappend return) t (==)
    prop (desc ++ " <=>") $ foldFromList (foldMapWith (<=>) return) t (==)
    prop (desc ++ " <|>") $ foldFromList (foldMapWith (<|>) return) t sortEq
    prop (desc ++ " mplus") $ foldFromList (foldMapWith mplus return) t sortEq
    prop (desc ++ " <|") $ foldFromList (foldMapWith (<|) return) t sortEq

applicativeOps
    :: (IsStream t, Applicative (t IO))
    => ([Int] -> t IO Int)
    -> (t IO (Int, Int) -> t IO (Int, Int))
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
    -> (t IO (Int, Int) -> t IO (Int, Int))
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
    -> (t IO (Int, Int) -> t IO (Int, Int))
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
                     (A.zipAsyncWithM (\x y -> return (x, y)) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        equals eq stream1 list
        equals eq stream2 list

monadThen
    :: (IsStream t, Monad (t IO))
    => ([Int] -> t IO Int)
    -> (t IO Int -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> ([Int], [Int])
    -> Property
monadThen constr t eq (a, b) = monadicIO $ do
    stream <- run ((A.toList . t) ((constr a) >> (constr b)))
    let list = a >> b
    equals eq stream list

monadBind
    :: (IsStream t, Monad (t IO))
    => ([Int] -> t IO Int)
    -> (t IO Int -> t IO Int)
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
        folded = adapt . serially . (\xs ->
            case xs of
                [x] -> return x -- singleton stream case
                _ -> foldMapWith (<>) return xs
            )
    describe "Functor operations" $ do
        functorOps A.each "serially" serially (==)
        functorOps folded "serially folded" serially (==)
        functorOps A.each "interleaving" interleaving (==)
        functorOps folded "interleaving folded" interleaving (==)
        functorOps A.each "asyncly" asyncly sortEq
        functorOps folded "asyncly folded" asyncly sortEq
        functorOps A.each "parallely" parallely sortEq
        functorOps folded "parallely folded" parallely sortEq
        functorOps A.each "zipping" zipping (==)
        functorOps folded "zipping folded" zipping (==)
        functorOps A.each "zippingAsync" zippingAsync (==)
        functorOps folded "zippingAsync folded" zippingAsync (==)

    describe "Semigroup operations" $ do
        semigroupOps "serially" serially
        semigroupOps "interleaving" interleaving

    describe "Applicative operations" $ do
        -- The tests using sorted equality are weaker tests
        -- We need to have stronger unit tests for all those
        -- XXX applicative with three arguments
        prop "serially applicative" $ applicativeOps A.each serially (==)
        prop "serially applicative folded" $ applicativeOps folded serially (==)
        prop "interleaving applicative" $ applicativeOps A.each interleaving sortEq
        prop "interleaving applicative folded" $ applicativeOps folded interleaving sortEq
        prop "asyncly applicative" $ applicativeOps A.each asyncly sortEq
        prop "asyncly applicative folded" $ applicativeOps folded asyncly sortEq
        prop "parallely applicative folded" $ applicativeOps folded parallely sortEq

    describe "Zip operations" $ do
        prop "zipping applicative" $ zipApplicative A.each zipping (==)
        -- XXX this hangs
        -- prop "zippingAsync applicative" $ zipApplicative zippingAsync (==)
        prop "zip monadic serially" $ zipMonadic A.each serially (==)
        prop "zip monadic serially folded" $ zipMonadic folded serially (==)
        prop "zip monadic interleaving" $ zipMonadic A.each interleaving (==)
        prop "zip monadic interleaving folded" $ zipMonadic folded interleaving (==)
        prop "zip monadic asyncly" $ zipMonadic A.each asyncly (==)
        prop "zip monadic asyncly folded" $ zipMonadic folded asyncly (==)
        prop "zip monadic parallely" $ zipMonadic A.each parallely (==)
        prop "zip monadic parallely folded" $ zipMonadic folded parallely (==)

    describe "Monad operations" $ do
        prop "serially monad then" $ monadThen A.each serially (==)
        prop "interleaving monad then" $ monadThen A.each interleaving sortEq
        prop "asyncly monad then" $ monadThen A.each asyncly sortEq
        prop "parallely monad then" $ monadThen A.each parallely sortEq

        prop "serially monad then folded" $ monadThen folded serially (==)
        prop "interleaving monad then folded" $ monadThen folded interleaving sortEq
        prop "asyncly monad then folded" $ monadThen folded asyncly sortEq
        prop "parallely monad then folded" $ monadThen folded parallely sortEq

        prop "serially monad bind" $ monadBind A.each serially (==)
        prop "interleaving monad bind" $ monadBind A.each interleaving sortEq
        prop "asyncly monad bind" $ monadBind A.each asyncly sortEq
        prop "parallely monad bind" $ monadBind A.each parallely sortEq

    describe "Stream transform operations" $ do
        transformOps A.each "serially" serially (==)
        transformOps A.each "interleaving" interleaving (==)
        transformOps A.each "zipping" zipping (==)
        transformOps A.each "zippingAsync" zippingAsync (==)
        transformOps A.each "asyncly" asyncly sortEq
        transformOps A.each "parallely" parallely sortEq

        transformOps folded "serially folded" serially (==)
        transformOps folded "interleaving folded" interleaving (==)
        transformOps folded "zipping folded" zipping (==)
        transformOps folded "zippingAsync folded" zippingAsync (==)
        transformOps folded "asyncly folded" asyncly sortEq
        transformOps folded "parallely folded" parallely sortEq

        transformOpsWord8 A.each "serially" serially
        transformOpsWord8 A.each "interleaving" interleaving
        transformOpsWord8 A.each "zipping" zipping
        transformOpsWord8 A.each "zippingAsync" zippingAsync
        transformOpsWord8 A.each "asyncly" asyncly
        transformOpsWord8 A.each "parallely" parallely

        transformOpsWord8 folded "serially folded" serially
        transformOpsWord8 folded "interleaving folded" interleaving
        transformOpsWord8 folded "zipping folded" zipping
        transformOpsWord8 folded "zippingAsync folded" zippingAsync
        transformOpsWord8 folded "asyncly folded" asyncly
        transformOpsWord8 folded "parallely folded" parallely

    describe "Stream elimination operations" $ do
        eliminationOps A.each "serially" serially
        eliminationOps A.each "interleaving" interleaving
        eliminationOps A.each "zipping" zipping
        eliminationOps A.each "zippingAsync" zippingAsync
        eliminationOps A.each "asyncly" asyncly
        eliminationOps A.each "parallely" parallely

        eliminationOps folded "serially folded" serially
        eliminationOps folded "interleaving folded" interleaving
        eliminationOps folded "zipping folded" zipping
        eliminationOps folded "zippingAsync folded" zippingAsync
        eliminationOps folded "asyncly folded" asyncly
        eliminationOps folded "parallely folded" parallely

    describe "Stream elimination operations" $ do
        serialEliminationOps A.each "serially" serially
        serialEliminationOps A.each "interleaving" interleaving
        serialEliminationOps A.each "zipping" zipping
        serialEliminationOps A.each "zippingAsync" zippingAsync

        serialEliminationOps folded "serially folded" serially
        serialEliminationOps folded "interleaving folded" interleaving
        serialEliminationOps folded "zipping folded" zipping
        serialEliminationOps folded "zippingAsync folded" zippingAsync
