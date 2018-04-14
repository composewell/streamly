{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Monad (when)
import Control.Applicative (ZipList(..))
import Control.Monad (replicateM)
import Data.List (sort)
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
    :: (Streaming t)
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
    :: (Streaming t)
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
    :: (Streaming t)
    => ([Int] -> t IO Int)
    -> (t IO Int -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> [Int]
    -> Property
foldFromList constr op eq a = transformFromList constr eq id op a

eliminateOp
    :: (Streaming t, Show a, Eq a)
    => ([Int] -> a)
    -> (t IO Int -> IO a)
    -> [Int]
    -> Property
eliminateOp listOp op a =
    monadicIO $ do
        stream <- run $ op (A.each a)
        let list = listOp a
        equals (==) stream list

elemOp
    :: (Streaming t)
    => (t IO Word8 -> t IO Word8)
    -> (Word8 -> t IO Word8 -> IO Bool)
    -> (Word8 -> [Word8] -> Bool)
    -> (Word8, [Word8])
    -> Property
elemOp op streamOp listOp (x, xs) =
    monadicIO $ do
        stream <- run $ (streamOp x . op) (A.each xs)
        let list = listOp x xs
        equals (==) stream list

transformOp
    :: (Streaming t)
    => ([Int] -> [Int] -> Bool)
    -> ([Int] -> [Int])
    -> (t IO Int -> t IO Int)
    -> [Int]
    -> Property
transformOp = transformFromList (A.each)

functorOps
    :: (Streaming t, Functor (t IO))
    => String
    -> (t IO Int -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> Spec
functorOps desc t eq = do
    prop (desc ++ " id") $ transformOp eq id $ t
    prop (desc ++ " fmap (+1)") $ transformOp eq (fmap (+1)) $ t . (fmap (+1))

transformOps
    :: Streaming t
    => String
    -> (t IO Int -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> Spec
transformOps desc t eq = do
    let transform = transformOp eq
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
    prop (desc ++ " scan") $ transform (scanl (+) 0) $ t . (A.scan (+) 0 id)
    prop (desc ++ "reverse") $ transform reverse $ t . A.reverse

wrapMaybe :: Eq a1 => ([a1] -> a2) -> [a1] -> Maybe a2
wrapMaybe f =
    \x ->
        if x == []
            then Nothing
            else Just (f x)

eliminationOps
    :: Streaming t
    => String
    -> (t IO Int -> t IO Int)
    -> Spec
eliminationOps desc t = do
    -- Elimination
    prop (desc ++ " null") $ eliminateOp null $ A.null . t
    prop (desc ++ " foldl") $
        eliminateOp (foldl (+) 0) $ (A.foldl (+) 0 id) . t
    prop (desc ++ " all") $ eliminateOp (all even) $ (A.all even) . t
    prop (desc ++ " any") $ eliminateOp (any even) $ (A.any even) . t
    prop (desc ++ " length") $ eliminateOp length $ A.length . t
    prop (desc ++ " sum") $ eliminateOp sum $ A.sum . t
    prop (desc ++ " product") $ eliminateOp product $ A.product . t

    prop (desc ++ " maximum") $ eliminateOp (wrapMaybe maximum) $ A.maximum . t
    prop (desc ++ " minimum") $ eliminateOp (wrapMaybe minimum) $ A.minimum . t

-- head/tail/last may depend on the order in case of parallel streams
-- so we test these only for serial streams.
serialEliminationOps
    :: Streaming t
    => String
    -> (t IO Int -> t IO Int)
    -> Spec
serialEliminationOps desc t = do
    prop (desc ++ " head") $ eliminateOp (wrapMaybe head) $ A.head . t
    prop (desc ++ " tail") $ eliminateOp (wrapMaybe tail) $ \x -> do
        r <- A.tail (t x)
        case r of
            Nothing -> return Nothing
            Just s -> A.toList s >>= return . Just
    prop (desc ++ " last") $ eliminateOp (wrapMaybe last) $ A.last . t

transformOpsWord8
    :: Streaming t
    => String -> (t IO Word8 -> t IO Word8) -> Spec
transformOpsWord8 desc t = do
    prop (desc ++ " elem") $ elemOp t A.elem elem
    prop (desc ++ " elem") $ elemOp t A.notElem notElem

-- XXX concatenate streams of multiple elements rather than single elements
semigroupOps
    :: ( Streaming t, MonadPlus (t IO)

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
    :: (Streaming t, Applicative (t IO))
    => (t IO (Int, Int) -> t IO (Int, Int))
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> ([Int], [Int])
    -> Property
applicativeOps t eq (a, b) = monadicIO $ do
    stream <- run ((A.toList . t) ((,) <$> (A.each a) <*> (A.each b)))
    let list = (,) <$> a <*> b
    equals eq stream list

zipApplicative
    :: (Streaming t, Applicative (t IO))
    => (t IO (Int, Int) -> t IO (Int, Int))
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> ([Int], [Int])
    -> Property
zipApplicative t eq (a, b) = monadicIO $ do
    stream1 <- run ((A.toList . t) ((,) <$> (A.each a) <*> (A.each b)))
    stream2 <- run ((A.toList . t) (pure (,) <*> (A.each a) <*> (A.each b)))
    stream3 <- run ((A.toList . t) (A.zipWith (,) (A.each a) (A.each b)))
    let list = getZipList $ (,) <$> ZipList a <*> ZipList b
    equals eq stream1 list
    equals eq stream2 list
    equals eq stream3 list

zipMonadic
    :: (Streaming t, Monad (t IO))
    => (t IO (Int, Int) -> t IO (Int, Int))
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> ([Int], [Int])
    -> Property
zipMonadic t eq (a, b) =
    monadicIO $ do
        stream <-
            run
                ((A.toList . t)
                     (A.zipWithM (\x y -> return (x, y)) (A.each a) (A.each b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        equals eq stream list

monadThen
    :: (Streaming t, Monad (t IO))
    => (t IO Int -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> ([Int], [Int])
    -> Property
monadThen t eq (a, b) = monadicIO $ do
    stream <- run ((A.toList . t) ((A.each a) >> (A.each b)))
    let list = a >> b
    equals eq stream list

monadBind
    :: (Streaming t, Monad (t IO))
    => (t IO Int -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> ([Int], [Int])
    -> Property
monadBind t eq (a, b) =
    monadicIO $ do
        stream <-
            run
                ((A.toList . t)
                     ((A.each a) >>= \x -> (A.each b) >>= return . (+ x)))
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

    describe "Functor operations" $ do
        functorOps "serially" serially (==)
        functorOps "interleaving" interleaving (==)
        functorOps "zipping" zipping (==)
        functorOps "asyncly" asyncly sortEq
        functorOps "parallely" parallely sortEq
        functorOps "zippingAsync" zippingAsync (==)

    describe "Semigroup operations" $ do
        semigroupOps "serially" serially
        semigroupOps "interleaving" interleaving

    describe "Applicative operations" $ do
        -- The tests using sorted equality are weaker tests
        -- We need to have stronger unit tests for all those
        -- XXX applicative with three arguments
        prop "serially applicative" $ applicativeOps serially (==)
        prop "interleaving applicative" $ applicativeOps interleaving sortEq
        prop "asyncly applicative" $ applicativeOps asyncly sortEq
        prop "parallely applicative" $ applicativeOps parallely sortEq

    describe "Zip operations" $ do
        prop "zipping applicative" $ zipApplicative zipping (==)
        -- XXX this hangs
        -- prop "zippingAsync applicative" $ zipApplicative zippingAsync (==)
        prop "zip monadic serially" $ zipMonadic serially (==)
        prop "zip monadic interleaving" $ zipMonadic interleaving (==)
        prop "zip monadic asyncly" $ zipMonadic asyncly (==)
        prop "zip monadic parallely" $ zipMonadic parallely (==)

    describe "Monad operations" $ do
        prop "serially monad then" $ monadThen serially (==)
        prop "interleaving monad then" $ monadThen interleaving sortEq
        prop "asyncly monad then" $ monadThen asyncly sortEq
        prop "parallely monad then" $ monadThen parallely sortEq

        prop "serially monad bind" $ monadBind serially (==)
        prop "interleaving monad bind" $ monadBind interleaving sortEq
        prop "asyncly monad bind" $ monadBind asyncly sortEq
        prop "parallely monad bind" $ monadBind parallely sortEq

    describe "Stream transform operations" $ do
        transformOps "serially" serially (==)
        transformOps "interleaving" interleaving (==)
        transformOps "zipping" zipping (==)
        transformOps "zippingAsync" zippingAsync (==)
        transformOps "asyncly" asyncly sortEq
        transformOps "parallely" parallely sortEq

        transformOpsWord8 "serially" serially
        transformOpsWord8 "interleaving" interleaving
        transformOpsWord8 "zipping" zipping
        transformOpsWord8 "zippingAsync" zippingAsync
        transformOpsWord8 "asyncly" asyncly
        transformOpsWord8 "parallely" parallely

    describe "Stream elimination operations" $ do
        eliminationOps "serially" serially
        eliminationOps "interleaving" interleaving
        eliminationOps "zipping" zipping
        eliminationOps "zippingAsync" zippingAsync
        eliminationOps "asyncly" asyncly
        eliminationOps "parallely" parallely

    describe "Stream elimination operations" $ do
        serialEliminationOps "serially" serially
        serialEliminationOps "interleaving" interleaving
        serialEliminationOps "zipping" zipping
        serialEliminationOps "zippingAsync" zippingAsync
