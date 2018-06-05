{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Exception (BlockedIndefinitelyOnMVar(..), catches,
                          BlockedIndefinitelyOnSTM(..), Handler(..))
import Control.Monad (when)
import Control.Applicative (ZipList(..))
import Control.Concurrent (MVar, takeMVar, putMVar, newEmptyMVar)
import Control.Monad (replicateM, replicateM_)
import Data.List (sort, foldl', scanl')
import Data.Maybe (mapMaybe)
import GHC.Word (Word8)

import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample, Property, withMaxSuccess)
import Test.QuickCheck.Monadic (run, monadicIO, monitor, assert, PropertyM)

import Test.Hspec

import Streamly
import Streamly.Prelude ((.:), nil)
import qualified Streamly.Prelude as A

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

mvarExcHandler :: String -> BlockedIndefinitelyOnMVar -> IO ()
mvarExcHandler label BlockedIndefinitelyOnMVar = do
    error $ label ++ " " ++ "BlockedIndefinitelyOnMVar\n"

stmExcHandler :: String -> BlockedIndefinitelyOnSTM -> IO ()
stmExcHandler label BlockedIndefinitelyOnSTM = do
    error $ label ++ " " ++ "BlockedIndefinitelyOnSTM\n"

dbgMVar :: String -> IO () -> IO ()
dbgMVar label action =
    action `catches` [ Handler (mvarExcHandler label)
                     , Handler (stmExcHandler label)
                     ]

-- | first n actions takeMVar and the last action performs putMVar n times
mvarSequenceOp :: MVar () -> Word8 -> Word8 -> IO Word8
mvarSequenceOp mv n x = do
    let msg = show x ++ "/" ++ show n
    if x < n
    then dbgMVar ("take mvarSequenceOp " ++ msg) (takeMVar mv) >>  return x
    else dbgMVar ("put mvarSequenceOp" ++ msg)
            (replicateM_ (fromIntegral n) (putMVar mv ())) >> return x

concurrentMapM
    :: ([Word8] -> t IO Word8)
    -> ([Word8] -> [Word8] -> Bool)
    -> (Word8 -> MVar () -> t IO Word8 -> SerialT IO Word8)
    -> Word8
    -> Property
concurrentMapM constr eq op n =
    monadicIO $ do
        let list = [0..n]
        stream <- run $ do
            mv <- newEmptyMVar :: IO (MVar ())
            (A.toList . (op n mv)) (constr list)
        equals eq stream list

concurrentFromFoldable
    :: IsStream t
    => ([Word8] -> [Word8] -> Bool)
    -> (t IO Word8 -> SerialT IO Word8)
    -> Word8
    -> Property
concurrentFromFoldable eq op n =
    monadicIO $ do
        let list = [0..n]
        stream <- run $ do
            mv <- newEmptyMVar :: IO (MVar ())
            (A.toList . op) (A.fromFoldableM (map (mvarSequenceOp mv n) list))
        equals eq stream list

sourceUnfoldrM :: IsStream t => MVar () -> Word8 -> t IO Word8
sourceUnfoldrM mv n = A.unfoldrM step 0
    where
    -- argument must be integer to avoid overflow of word8 at 255
    step :: Int -> IO (Maybe (Word8, Int))
    step cnt = do
        let msg = show cnt ++ "/" ++ show n
        if cnt > fromIntegral n
        then return Nothing
        else do
            dbgMVar ("put sourceUnfoldrM " ++ msg) (putMVar mv ())
            return (Just (fromIntegral cnt, cnt + 1))

concurrentUnfoldrM
    :: IsStream t
    => ([Word8] -> [Word8] -> Bool)
    -> (t IO Word8 -> SerialT IO Word8)
    -> Word8
    -> Property
concurrentUnfoldrM eq op n =
    monadicIO $ do
        -- XXX we should test empty list case as well
        let list = [0..n]
        stream <- run $ do
            -- putStrLn $ "concurrentUnfoldrM: " ++ show n
            mv <- newEmptyMVar :: IO (MVar ())
            -- since unfoldr happens in parallel with the stream processing we
            -- can do two takeMVar in one iteration. If it is not parallel then
            -- this will not work and the test will fail.
            A.toList $ do
                x <- op (sourceUnfoldrM mv n)
                let msg = show x ++ "/" ++ show n
                A.once $ do
                    if even x
                    then do
                        dbgMVar ("first take concurrentUnfoldrM " ++ msg)
                                (takeMVar mv)
                        if n > x
                        then dbgMVar ("second take concurrentUnfoldrM " ++ msg)
                                     (takeMVar mv)
                        else return ()
                    else return ()
                return x
        equals eq stream list

concurrentApplication :: Word8 -> Property
concurrentApplication n =
    monadicIO $ do
        -- XXX we should test empty list case as well
        let list = [0..n]
        stream <- run $ do
            -- putStrLn $ "concurrentApplication: " ++ show n
            mv <- newEmptyMVar :: IO (MVar ())
            -- since unfoldr happens in parallel with the stream processing we
            -- can do two takeMVar in one iteration. If it is not parallel then
            -- this will not work and the test will fail.
            A.toList $ do
                sourceUnfoldrM mv n |&
                    (A.mapM $ \x -> do
                        let msg = show x ++ "/" ++ show n
                        if even x
                        then do
                            dbgMVar ("first take concurrentApp " ++ msg)
                                    (takeMVar mv)
                            if n > x
                            then dbgMVar ("second take concurrentApp " ++ msg)
                                         (takeMVar mv)
                            else return ()
                        else return ()
                        return x)
        equals (==) stream list

sourceUnfoldrM1 :: IsStream t => Word8 -> t IO Word8
sourceUnfoldrM1 n = A.unfoldrM step 0
    where
    -- argument must be integer to avoid overflow of word8 at 255
    step :: Int -> IO (Maybe (Word8, Int))
    step cnt = do
        if cnt > fromIntegral n
        then return Nothing
        else return (Just (fromIntegral cnt, cnt + 1))

concurrentFoldlApplication :: Word8 -> Property
concurrentFoldlApplication n =
    monadicIO $ do
        -- XXX we should test empty list case as well
        let list = [0..n]
        stream <- run $ do
            sourceUnfoldrM1 n |&. A.foldlM' (\xs x -> return (x : xs)) []
        equals (==) (reverse stream) list

concurrentFoldrApplication :: Word8 -> Property
concurrentFoldrApplication n =
    monadicIO $ do
        -- XXX we should test empty list case as well
        let list = [0..n]
        stream <- run $ do
            sourceUnfoldrM1 n |&. A.foldrM (\x xs -> return (x : xs)) []
        equals (==) stream list

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
        monadicIO $ do
            stream <- run ((A.toList . t) $
                constr a <> op (constr b <> constr c))
            let list = a <> listOp (b <> c)
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

    let f x = if odd x then Just (x + 100) else Nothing
    prop (desc ++ " mapMaybe") $ transform (mapMaybe f) $ t . (A.mapMaybe f)

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
    prop (desc ++ " reverse") $ transform reverse $ t . A.reverse

concurrentOps
    :: IsStream t
    => ([Word8] -> t IO Word8)
    -> String
    -> (t IO Word8 -> SerialT IO Word8)
    -> ([Word8] -> [Word8] -> Bool)
    -> Spec
concurrentOps constr desc t eq = do
    prop (desc ++ " fromFoldableM") $ withMaxSuccess maxTestCount $
        concurrentFromFoldable eq t
    prop (desc ++ " unfoldrM") $ withMaxSuccess maxTestCount $
        concurrentUnfoldrM eq t
    -- we pass it the length of the stream n and an mvar mv.
    -- The stream is [0..n]. The threads communicate in such a way that the
    -- actions coming first in the stream are dependent on the last action. So
    -- if the stream is not processed concurrently it will block forever.
    -- Note that if the size of the stream is bigger than the thread limit
    -- then it will block even if it is concurrent.
    prop (desc ++ " mapM") $ withMaxSuccess maxTestCount $
        concurrentMapM constr eq $ \n mv stream ->
            t $ A.mapM (mvarSequenceOp mv n) stream

-- XXX add tests for MonadReader and MonadError etc. In case an SVar is
-- accidentally passed through them.
transformCombineOpsCommon
    :: (IsStream t, Semigroup (t IO Int))
    => ([Int] -> t IO Int)
    -> String
    -> (t IO Int -> SerialT IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> Spec
transformCombineOpsCommon constr desc t eq = do
    let transform = transformCombineFromList constr eq
    -- Filtering
    prop (desc ++ " filter False") $
        transform (filter (const False)) t (A.filter (const False))
    prop (desc ++ " filter True") $
        transform (filter (const True)) t (A.filter (const True))
    prop (desc ++ " filter even") $
        transform (filter even) t (A.filter even)

    prop (desc ++ " take maxBound") $
        transform (take maxBound) t (A.take maxBound)
    prop (desc ++ " take 0") $ transform (take 0) t (A.take 0)

    prop (desc ++ " takeWhile True") $
        transform (takeWhile (const True)) t (A.takeWhile (const True))
    prop (desc ++ " takeWhile False") $
        transform (takeWhile (const False)) t (A.takeWhile (const False))

    prop (desc ++ " drop maxBound") $
        transform (drop maxBound) t (A.drop maxBound)
    prop (desc ++ " drop 0") $ transform (drop 0) t (A.drop 0)

    prop (desc ++ " dropWhile True") $
        transform (dropWhile (const True)) t (A.dropWhile (const True))
    prop (desc ++ " dropWhile False") $
        transform (dropWhile (const False)) t (A.dropWhile (const False))
    prop (desc ++ " scan") $ transform (scanl' (flip const) 0) t
                                       (A.scanl' (flip const) 0)
    prop (desc ++ " reverse") $ transform reverse t A.reverse

transformCombineOpsOrdered
    :: (IsStream t, Semigroup (t IO Int))
    => ([Int] -> t IO Int)
    -> String
    -> (t IO Int -> SerialT IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> Spec
transformCombineOpsOrdered constr desc t eq = do
    let transform = transformCombineFromList constr eq
    -- Filtering
    prop (desc ++ " take 1") $ transform (take 1) t (A.take 1)
    prop (desc ++ " take 10") $ transform (take 10) t (A.take 10)

    prop (desc ++ " takeWhile > 0") $
        transform (takeWhile (> 0)) t (A.takeWhile (> 0))

    prop (desc ++ " drop 1") $ transform (drop 1) t (A.drop 1)
    prop (desc ++ " drop 10") $ transform (drop 10) t (A.drop 10)

    prop (desc ++ " dropWhile > 0") $
        transform (dropWhile (> 0)) t (A.dropWhile (> 0))
    prop (desc ++ " scan") $ transform (scanl' (+) 0) t (A.scanl' (+) 0)

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
applicativeOps constr t eq (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
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
zipApplicative constr t eq (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
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
zipMonadic constr t eq (a, b) = withMaxSuccess maxTestCount $
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
    :: Monad (t IO)
    => ([Int] -> t IO Int)
    -> (t IO Int -> SerialT IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> ([Int], [Int])
    -> Property
monadThen constr t eq (a, b) = withMaxSuccess maxTestCount $ monadicIO $ do
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
monadBind constr t eq (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <-
            run
                ((A.toList . t)
                     ((constr a) >>= \x -> (constr b) >>= return . (+ x)))
        let list = a >>= \x -> b >>= return . (+ x)
        equals eq stream list

main :: IO ()
main = hspec $ do
    let folded :: IsStream t => [a] -> t IO a
        folded = serially . (\xs ->
            case xs of
                [x] -> return x -- singleton stream case
                _ -> foldMapWith (<>) return xs
            )
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
        functorOps A.fromFoldable "serially" serially (==)
        functorOps folded "serially folded" serially (==)
        functorOps A.fromFoldable "wSerially" wSerially (==)
        functorOps folded "wSerially folded" wSerially (==)
        functorOps A.fromFoldable "aheadly" aheadly (==)
        functorOps folded "aheadly folded" aheadly (==)
        functorOps A.fromFoldable "asyncly" asyncly sortEq
        functorOps folded "asyncly folded" asyncly sortEq
        functorOps A.fromFoldable "wAsyncly" wAsyncly sortEq
        functorOps folded "wAsyncly folded" wAsyncly sortEq
        functorOps A.fromFoldable "parallely" parallely sortEq
        functorOps folded "parallely folded" parallely sortEq
        functorOps A.fromFoldable "zipSerially" zipSerially (==)
        functorOps folded "zipSerially folded" zipSerially (==)
        functorOps A.fromFoldable "zipAsyncly" zipAsyncly (==)
        functorOps folded "zipAsyncly folded" zipAsyncly (==)

    describe "Semigroup operations" $ do
        semigroupOps "serially" serially (==)
        semigroupOps "wSerially" wSerially (==)
        semigroupOps "aheadly" aheadly (==)
        semigroupOps "asyncly" asyncly sortEq
        semigroupOps "wAsyncly" wAsyncly sortEq
        semigroupOps "parallely" parallely sortEq
        semigroupOps "zipSerially" zipSerially (==)
        semigroupOps "zipAsyncly" zipAsyncly (==)

    describe "Applicative operations" $ do
        -- The tests using sorted equality are weaker tests
        -- We need to have stronger unit tests for all those
        -- XXX applicative with three arguments
        prop "serially applicative" $ applicativeOps A.fromFoldable serially (==)
        prop "serially applicative folded" $ applicativeOps folded serially (==)
        prop "aheadly applicative" $ applicativeOps A.fromFoldable aheadly (==)
        prop "aheadly applicative folded" $ applicativeOps folded aheadly (==)
        prop "wSerially applicative" $ applicativeOps A.fromFoldable wSerially sortEq
        prop "wSerially applicative folded" $ applicativeOps folded wSerially sortEq
        prop "asyncly applicative" $ applicativeOps A.fromFoldable asyncly sortEq
        prop "asyncly applicative folded" $ applicativeOps folded asyncly sortEq
        prop "wAsyncly applicative folded" $ applicativeOps folded wAsyncly sortEq
        prop "parallely applicative folded" $ applicativeOps folded parallely sortEq

    describe "Zip operations" $ do
        prop "zipSerially applicative" $ zipApplicative A.fromFoldable zipSerially (==)
        prop "zipSerially applicative folded" $ zipApplicative folded zipSerially (==)
        prop "zipAsyncly applicative" $ zipApplicative A.fromFoldable zipAsyncly (==)
        prop "zipAsyncly applicative folded" $ zipApplicative folded zipAsyncly (==)

        prop "zip monadic serially" $ zipMonadic A.fromFoldable serially (==)
        prop "zip monadic serially folded" $ zipMonadic folded serially (==)
        prop "zip monadic aheadly" $ zipMonadic A.fromFoldable aheadly (==)
        prop "zip monadic aheadly folded" $ zipMonadic folded aheadly (==)
        prop "zip monadic wSerially" $ zipMonadic A.fromFoldable wSerially (==)
        prop "zip monadic wSerially folded" $ zipMonadic folded wSerially (==)
        prop "zip monadic asyncly" $ zipMonadic A.fromFoldable asyncly (==)
        prop "zip monadic asyncly folded" $ zipMonadic folded asyncly (==)
        prop "zip monadic wAsyncly" $ zipMonadic A.fromFoldable wAsyncly (==)
        prop "zip monadic wAsyncly folded" $ zipMonadic folded wAsyncly (==)
        prop "zip monadic parallely" $ zipMonadic A.fromFoldable parallely (==)
        prop "zip monadic parallely folded" $ zipMonadic folded parallely (==)

    describe "Monad operations" $ do
        prop "serially monad then" $ monadThen A.fromFoldable serially (==)
        prop "aheadly monad then" $ monadThen A.fromFoldable aheadly (==)
        prop "wSerially monad then" $ monadThen A.fromFoldable wSerially sortEq
        prop "asyncly monad then" $ monadThen A.fromFoldable asyncly sortEq
        prop "wAsyncly monad then" $ monadThen A.fromFoldable wAsyncly sortEq
        prop "parallely monad then" $ monadThen A.fromFoldable parallely sortEq

        prop "serially monad then folded" $ monadThen folded serially (==)
        prop "aheadly monad then folded" $ monadThen folded aheadly (==)
        prop "wSerially monad then folded" $ monadThen folded wSerially sortEq
        prop "asyncly monad then folded" $ monadThen folded asyncly sortEq
        prop "wAsyncly monad then folded" $ monadThen folded wAsyncly sortEq
        prop "parallely monad then folded" $ monadThen folded parallely sortEq

        prop "serially monad bind" $ monadBind A.fromFoldable serially (==)
        prop "aheadly monad bind" $ monadBind A.fromFoldable aheadly (==)
        prop "wSerially monad bind" $ monadBind A.fromFoldable wSerially sortEq
        prop "asyncly monad bind" $ monadBind A.fromFoldable asyncly sortEq
        prop "wAsyncly monad bind" $ monadBind A.fromFoldable wAsyncly sortEq
        prop "parallely monad bind" $ monadBind A.fromFoldable parallely sortEq

    describe "Stream transform operations" $ do
        transformOps A.fromFoldable "serially" serially (==)
        transformOps A.fromFoldable "aheadly" aheadly (==)
        transformOps A.fromFoldable "wSerially" wSerially (==)
        transformOps A.fromFoldable "zipSerially" zipSerially (==)
        transformOps A.fromFoldable "zipAsyncly" zipAsyncly (==)
        transformOps A.fromFoldable "asyncly" asyncly sortEq
        transformOps A.fromFoldable "wAsyncly" wAsyncly sortEq
        transformOps A.fromFoldable "parallely" parallely sortEq

        transformOps folded "serially folded" serially (==)
        transformOps folded "aheadly folded" aheadly (==)
        transformOps folded "wSerially folded" wSerially (==)
        transformOps folded "zipSerially folded" zipSerially (==)
        transformOps folded "zipAsyncly folded" zipAsyncly (==)
        transformOps folded "asyncly folded" asyncly sortEq
        transformOps folded "wAsyncly folded" wAsyncly sortEq
        transformOps folded "parallely folded" parallely sortEq

        transformOpsWord8 A.fromFoldable "serially" serially
        transformOpsWord8 A.fromFoldable "aheadly" aheadly
        transformOpsWord8 A.fromFoldable "wSerially" wSerially
        transformOpsWord8 A.fromFoldable "zipSerially" zipSerially
        transformOpsWord8 A.fromFoldable "zipAsyncly" zipAsyncly
        transformOpsWord8 A.fromFoldable "asyncly" asyncly
        transformOpsWord8 A.fromFoldable "wAsyncly" wAsyncly
        transformOpsWord8 A.fromFoldable "parallely" parallely

        transformOpsWord8 folded "serially folded" serially
        transformOpsWord8 folded "aheadly folded" aheadly
        transformOpsWord8 folded "wSerially folded" wSerially
        transformOpsWord8 folded "zipSerially folded" zipSerially
        transformOpsWord8 folded "zipAsyncly folded" zipAsyncly
        transformOpsWord8 folded "asyncly folded" asyncly
        transformOpsWord8 folded "wAsyncly folded" wAsyncly
        transformOpsWord8 folded "parallely folded" parallely

    describe "Stream concurrent operations" $ do
        concurrentOps A.fromFoldable "aheadly" aheadly (==)
        concurrentOps A.fromFoldable "asyncly" asyncly sortEq
        concurrentOps A.fromFoldable "wAsyncly" wAsyncly sortEq
        concurrentOps A.fromFoldable "parallely" parallely sortEq

        concurrentOps folded "aheadly folded" aheadly (==)
        concurrentOps folded "asyncly folded" asyncly sortEq
        concurrentOps folded "wAsyncly folded" wAsyncly sortEq
        concurrentOps folded "parallely folded" parallely sortEq

        prop "concurrent application" $ withMaxSuccess maxTestCount $
            concurrentApplication
        prop "concurrent foldr application" $ withMaxSuccess maxTestCount $
            concurrentFoldrApplication
        prop "concurrent foldl application" $ withMaxSuccess maxTestCount $
            concurrentFoldlApplication

    -- These tests are specifically targeted towards detecting illegal sharing
    -- of SVar across conurrent streams.
    describe "Stream transform and combine operations" $ do
        transformCombineOpsCommon A.fromFoldable "serially" serially (==)
        transformCombineOpsCommon A.fromFoldable "aheadly" aheadly (==)
        transformCombineOpsCommon A.fromFoldable "wSerially" wSerially sortEq
        transformCombineOpsCommon A.fromFoldable "zipSerially" zipSerially (==)
        transformCombineOpsCommon A.fromFoldable "zipAsyncly" zipAsyncly (==)
        transformCombineOpsCommon A.fromFoldable "asyncly" asyncly sortEq
        transformCombineOpsCommon A.fromFoldable "wAsyncly" wAsyncly sortEq
        transformCombineOpsCommon A.fromFoldable "parallely" parallely sortEq

        transformCombineOpsCommon folded "serially" serially (==)
        transformCombineOpsCommon folded "aheadly" aheadly (==)
        transformCombineOpsCommon folded "wSerially" wSerially sortEq
        transformCombineOpsCommon folded "zipSerially" zipSerially (==)
        transformCombineOpsCommon folded "zipAsyncly" zipAsyncly (==)
        transformCombineOpsCommon folded "asyncly" asyncly sortEq
        transformCombineOpsCommon folded "wAsyncly" wAsyncly sortEq
        transformCombineOpsCommon folded "parallely" parallely sortEq

        transformCombineOpsOrdered A.fromFoldable "serially" serially (==)
        transformCombineOpsOrdered A.fromFoldable "serially" aheadly (==)
        transformCombineOpsOrdered A.fromFoldable "zipSerially" zipSerially (==)
        transformCombineOpsOrdered A.fromFoldable "zipAsyncly" zipAsyncly (==)

    describe "Stream elimination operations" $ do
        eliminationOps A.fromFoldable "serially" serially
        eliminationOps A.fromFoldable "aheadly" aheadly
        eliminationOps A.fromFoldable "wSerially" wSerially
        eliminationOps A.fromFoldable "zipSerially" zipSerially
        eliminationOps A.fromFoldable "zipAsyncly" zipAsyncly
        eliminationOps A.fromFoldable "asyncly" asyncly
        eliminationOps A.fromFoldable "wAsyncly" wAsyncly
        eliminationOps A.fromFoldable "parallely" parallely

        eliminationOps folded "serially folded" serially
        eliminationOps folded "aheadly folded" aheadly
        eliminationOps folded "wSerially folded" wSerially
        eliminationOps folded "zipSerially folded" zipSerially
        eliminationOps folded "zipAsyncly folded" zipAsyncly
        eliminationOps folded "asyncly folded" asyncly
        eliminationOps folded "wAsyncly folded" wAsyncly
        eliminationOps folded "parallely folded" parallely

    describe "Stream serial elimination operations" $ do
        serialEliminationOps A.fromFoldable "serially" serially
        serialEliminationOps A.fromFoldable "aheadly" aheadly
        serialEliminationOps A.fromFoldable "wSerially" wSerially
        serialEliminationOps A.fromFoldable "zipSerially" zipSerially
        serialEliminationOps A.fromFoldable "zipAsyncly" zipAsyncly

        serialEliminationOps folded "serially folded" serially
        serialEliminationOps folded "aheadly folded" aheadly
        serialEliminationOps folded "wSerially folded" wSerially
        serialEliminationOps folded "zipSerially folded" zipSerially
        serialEliminationOps folded "zipAsyncly folded" zipAsyncly
