{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Exception (BlockedIndefinitelyOnMVar(..), catches,
                          BlockedIndefinitelyOnSTM(..), Handler(..))
import Control.Monad (when, forM_)
import Control.Applicative (ZipList(..))
import Control.Concurrent (MVar, takeMVar, putMVar, newEmptyMVar)
import Control.Monad (replicateM, replicateM_)
import Data.Function ((&))
import Data.IORef (readIORef, modifyIORef, newIORef)
import Data.List (sort, foldl', scanl', findIndices, findIndex, elemIndices,
                  elemIndex, find, intersperse, foldl1')
import Data.Maybe (mapMaybe)
import GHC.Word (Word8)

import Test.Hspec.QuickCheck
import Test.QuickCheck (counterexample, Property, withMaxSuccess)
import Test.QuickCheck.Monadic (run, monadicIO, monitor, assert, PropertyM)

import Test.Hspec as H

import Streamly
import Streamly.Prelude ((.:), nil)
import qualified Streamly.Prelude as S

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
constructWithReplicateM op len = withMaxSuccess maxTestCount $
    monadicIO $ do
        let x = return (1 :: Int)
        stream <- run $ (S.toList . op) (S.replicateM (fromIntegral len) x)
        list <- run $ replicateM (fromIntegral len) x
        equals (==) stream list

transformFromList
    :: Show b =>
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
            (S.toList . (op n mv)) (constr list)
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
            (S.toList . op) (S.fromFoldableM (map (mvarSequenceOp mv n) list))
        equals eq stream list

sourceUnfoldrM :: IsStream t => MVar () -> Word8 -> t IO Word8
sourceUnfoldrM mv n = S.unfoldrM step 0
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

-- Note that this test is not guaranteed to succeed, because there is no
-- guarantee of parallelism in case of Async/Ahead streams.
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
            cnt <- newIORef 0
            -- since unfoldr happens in parallel with the stream processing we
            -- can do two takeMVar in one iteration. If it is not parallel then
            -- this will not work and the test will fail.
            S.toList $ do
                x <- op (sourceUnfoldrM mv n)
                -- results may not be yielded in order, in case of
                -- Async/WAsync/Parallel. So we use an increasing count
                -- instead.
                i <- S.yieldM $ readIORef cnt
                S.yieldM $ modifyIORef cnt (+1)
                let msg = show i ++ "/" ++ show n
                S.yieldM $ do
                    if even i
                    then do
                        dbgMVar ("first take concurrentUnfoldrM " ++ msg)
                                (takeMVar mv)
                        if n > i
                        then do
                            dbgMVar ("second take concurrentUnfoldrM " ++ msg)
                                     (takeMVar mv)
                        else return ()
                    else return ()
                return x
        equals eq stream list

concurrentApplication :: IsStream t
    => ([Word8] -> [Word8] -> Bool)
    -> (t IO Word8 -> SerialT IO Word8)
    -> Word8
    -> Property
concurrentApplication eq t n = withMaxSuccess maxTestCount $
    monadicIO $ do
        -- XXX we should test empty list case as well
        let list = [0..n]
        stream <- run $ do
            -- putStrLn $ "concurrentApplication: " ++ show n
            mv <- newEmptyMVar :: IO (MVar ())
            -- since unfoldr happens in parallel with the stream processing we
            -- can do two takeMVar in one iteration. If it is not parallel then
            -- this will not work and the test will fail.
            (S.toList . t) $ do
                sourceUnfoldrM mv n |&
                    (S.mapM $ \x -> do
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
        equals eq stream list

sourceUnfoldrM1 :: IsStream t => Word8 -> t IO Word8
sourceUnfoldrM1 n = S.unfoldrM step 0
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
            sourceUnfoldrM1 n |&. S.foldlM' (\xs x -> return (x : xs)) []
        equals (==) (reverse stream) list

concurrentFoldrApplication :: Word8 -> Property
concurrentFoldrApplication n =
    monadicIO $ do
        -- XXX we should test empty list case as well
        let list = [0..n]
        stream <- run $ do
            sourceUnfoldrM1 n |&. S.foldrM (\x xs -> return (x : xs)) []
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
    withMaxSuccess maxTestCount $
        monadicIO $ do
            stream <- run ((S.toList . t) $
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
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> Spec
functorOps constr desc eq t = do
    prop (desc ++ " id") $ transformFromList constr eq id $ t
    prop (desc ++ " fmap (+1)") $ transformFromList constr eq (fmap (+1)) $ t . (fmap (+1))

transformOps
    :: IsStream t
    => ([Int] -> t IO Int)
    -> String
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> Spec
transformOps constr desc eq t = do
    let transform = transformFromList constr eq
    -- Filtering
    prop (desc ++ " filter False") $
        transform (filter (const False)) $ t . (S.filter (const False))
    prop (desc ++ " filter True") $
        transform (filter (const True)) $ t . (S.filter (const True))
    prop (desc ++ " filter even") $
        transform (filter even) $ t . (S.filter even)

    prop (desc ++ " take maxBound") $
        transform (take maxBound) $ t . (S.take maxBound)
    prop (desc ++ " take 0") $ transform (take 0) $ t . (S.take 0)
    prop (desc ++ " take 1") $ transform (take 1) $ t . (S.take 1)
    prop (desc ++ " take 10") $ transform (take 10) $ t . (S.take 10)

    prop (desc ++ " takeWhile True") $
        transform (takeWhile (const True)) $ t . (S.takeWhile (const True))
    prop (desc ++ " takeWhile False") $
        transform (takeWhile (const False)) $ t . (S.takeWhile (const False))
    prop (desc ++ " takeWhile > 0") $
        transform (takeWhile (> 0)) $ t . (S.takeWhile (> 0))

    let f x = if odd x then Just (x + 100) else Nothing
    prop (desc ++ " mapMaybe") $ transform (mapMaybe f) $ t . (S.mapMaybe f)

    prop (desc ++ " drop maxBound") $
        transform (drop maxBound) $ t . (S.drop maxBound)
    prop (desc ++ " drop 0") $ transform (drop 0) $ t . (S.drop 0)
    prop (desc ++ " drop 1") $ transform (drop 1) $ t . (S.drop 1)
    prop (desc ++ " drop 10") $ transform (drop 10) $ t . (S.drop 10)

    prop (desc ++ " dropWhile True") $
        transform (dropWhile (const True)) $ t . (S.dropWhile (const True))
    prop (desc ++ " dropWhile False") $
        transform (dropWhile (const False)) $ t . (S.dropWhile (const False))
    prop (desc ++ " dropWhile > 0") $
        transform (dropWhile (> 0)) $ t . (S.dropWhile (> 0))
    prop (desc ++ " scan") $ transform (scanl' (+) 0) $ t . (S.scanl' (+) 0)
    prop (desc ++ " reverse") $ transform reverse $ t . S.reverse

    prop (desc ++ " findIndices") $ transform (findIndices odd) $ t . (S.findIndices odd)
    prop (desc ++ " elemIndices") $ transform (elemIndices 3) $ t . (S.elemIndices 3)

    prop (desc ++ " intersperseM") $ transform (intersperse 3) $ t . (S.intersperseM (return 3))


concurrentOps
    :: IsStream t
    => ([Word8] -> t IO Word8)
    -> String
    -> ([Word8] -> [Word8] -> Bool)
    -> (t IO Word8 -> SerialT IO Word8)
    -> Spec
concurrentOps constr desc eq t = do
    let prop1 d p = prop d $ withMaxSuccess maxTestCount p

    prop1 (desc ++ " fromFoldableM") $ concurrentFromFoldable eq t
    prop1 (desc ++ " unfoldrM") $ concurrentUnfoldrM eq t
    -- we pass it the length of the stream n and an mvar mv.
    -- The stream is [0..n]. The threads communicate in such a way that the
    -- actions coming first in the stream are dependent on the last action. So
    -- if the stream is not processed concurrently it will block forever.
    -- Note that if the size of the stream is bigger than the thread limit
    -- then it will block even if it is concurrent.
    prop1 (desc ++ " mapM") $
        concurrentMapM constr eq $ \n mv stream ->
            t $ S.mapM (mvarSequenceOp mv n) stream

-- XXX add tests for MonadReader and MonadError etc. In case an SVar is
-- accidentally passed through them.
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
    prop (desc ++ " filter False") $
        transform (filter (const False)) t (S.filter (const False))
    prop (desc ++ " filter True") $
        transform (filter (const True)) t (S.filter (const True))
    prop (desc ++ " filter even") $
        transform (filter even) t (S.filter even)

    prop (desc ++ " filterM False") $
        transform (filter (const False)) t (S.filterM (const $ return False))
    prop (desc ++ " filterM True") $
        transform (filter (const True)) t (S.filterM (const $ return True))
    prop (desc ++ " filterM even") $
        transform (filter even) t (S.filterM (return . even))

    prop (desc ++ " take maxBound") $
        transform (take maxBound) t (S.take maxBound)
    prop (desc ++ " take 0") $ transform (take 0) t (S.take 0)

    prop (desc ++ " takeWhile True") $
        transform (takeWhile (const True)) t (S.takeWhile (const True))
    prop (desc ++ " takeWhile False") $
        transform (takeWhile (const False)) t (S.takeWhile (const False))

    prop (desc ++ " takeWhileM True") $
        transform (takeWhile (const True)) t (S.takeWhileM (const $ return True))
    prop (desc ++ " takeWhileM False") $
        transform (takeWhile (const False)) t (S.takeWhileM (const $ return False))

    prop (desc ++ " drop maxBound") $
        transform (drop maxBound) t (S.drop maxBound)
    prop (desc ++ " drop 0") $ transform (drop 0) t (S.drop 0)

    prop (desc ++ " dropWhile True") $
        transform (dropWhile (const True)) t (S.dropWhile (const True))
    prop (desc ++ " dropWhile False") $
        transform (dropWhile (const False)) t (S.dropWhile (const False))

    prop (desc ++ " dropWhileM True") $
        transform (dropWhile (const True)) t (S.dropWhileM (const $ return True))
    prop (desc ++ " dropWhileM False") $
        transform (dropWhile (const False)) t (S.dropWhileM (const $ return False))

    prop (desc ++ " mapM (+1)") $
        transform (map (+1)) t (S.mapM (\x -> return (x + 1)))

    prop (desc ++ " scan") $ transform (scanl' (flip const) 0) t
                                       (S.scanl' (flip const) 0)
    prop (desc ++ " scanlM'") $ transform (scanl' (flip const) 0) t
                                       (S.scanlM' (\_ a -> return a) 0)
    prop (desc ++ " reverse") $ transform reverse t S.reverse

    prop (desc ++ " intersperseM") $
        transform (intersperse 3) t (S.intersperseM $ return 3)

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
    prop (desc ++ " take 1") $ transform (take 1) t (S.take 1)
#ifdef DEVBUILD
    prop (desc ++ " take 2") $ transform (take 2) t (S.take 2)
    prop (desc ++ " take 3") $ transform (take 3) t (S.take 3)
    prop (desc ++ " take 4") $ transform (take 4) t (S.take 4)
    prop (desc ++ " take 5") $ transform (take 5) t (S.take 5)
#endif
    prop (desc ++ " take 10") $ transform (take 10) t (S.take 10)

    prop (desc ++ " takeWhile > 0") $
        transform (takeWhile (> 0)) t (S.takeWhile (> 0))

    prop (desc ++ " drop 1") $ transform (drop 1) t (S.drop 1)
    prop (desc ++ " drop 10") $ transform (drop 10) t (S.drop 10)

    prop (desc ++ " dropWhile > 0") $
        transform (dropWhile (> 0)) t (S.dropWhile (> 0))
    prop (desc ++ " scan") $ transform (scanl' (+) 0) t (S.scanl' (+) 0)

    -- XXX this does not fail when the SVar is shared, need to fix.
    prop (desc ++ " concurrent application") $
        transform (& (map (+1))) t (|& (S.map (+1)))

    prop (desc ++ " findIndices") $
        transform (findIndices odd) t (S.findIndices odd)
    prop (desc ++ " elemIndices") $
        transform (elemIndices 0) t (S.elemIndices 0)

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
    prop (desc ++ " null") $ eliminateOp constr null $ S.null . t
    prop (desc ++ " foldl'") $
        eliminateOp constr (foldl' (+) 0) $ (S.foldl' (+) 0) . t
    prop (desc ++ " foldl1'") $
        eliminateOp constr (wrapMaybe $ foldl1' (+)) $ (S.foldl1' (+)) . t
    prop (desc ++ " foldr1") $
        eliminateOp constr (wrapMaybe $ foldr1 (+)) $ (S.foldr1 (+)) . t
    prop (desc ++ " all") $ eliminateOp constr (all even) $ (S.all even) . t
    prop (desc ++ " any") $ eliminateOp constr (any even) $ (S.any even) . t
    prop (desc ++ " and") $ eliminateOp constr (and . map (> 0)) $
        (S.and . S.map (> 0)) . t
    prop (desc ++ " or") $ eliminateOp constr (or . map (> 0)) $
        (S.or . S.map (> 0)) . t
    prop (desc ++ " length") $ eliminateOp constr length $ S.length . t
    prop (desc ++ " sum") $ eliminateOp constr sum $ S.sum . t
    prop (desc ++ " product") $ eliminateOp constr product $ S.product . t

    prop (desc ++ " maximum") $ eliminateOp constr (wrapMaybe maximum) $ S.maximum . t
    prop (desc ++ " minimum") $ eliminateOp constr (wrapMaybe minimum) $ S.minimum . t

    prop (desc ++ " findIndex") $ eliminateOp constr (findIndex odd) $ (S.findIndex odd) . t
    prop (desc ++ " elemIndex") $ eliminateOp constr (elemIndex 3) $ (S.elemIndex 3) . t

    prop (desc ++ " find") $ eliminateOp constr (find even) $ (S.find even) . t
    prop (desc ++ " lookup") $
        eliminateOp constr (lookup 3 . flip zip [1..]) $
            S.lookup 3 . S.zipWith (\a b -> (b, a)) (S.fromList [(1::Int)..]) . t

-- head/tail/last may depend on the order in case of parallel streams
-- so we test these only for serial streams.
serialEliminationOps
    :: ([Int] -> t IO Int)
    -> String
    -> (t IO Int -> SerialT IO Int)
    -> Spec
serialEliminationOps constr desc t = do
    prop (desc ++ " head") $ eliminateOp constr (wrapMaybe head) $ S.head . t
    prop (desc ++ " tail") $ eliminateOp constr (wrapMaybe tail) $ \x -> do
        r <- S.tail (t x)
        case r of
            Nothing -> return Nothing
            Just s -> S.toList s >>= return . Just
    prop (desc ++ " last") $ eliminateOp constr (wrapMaybe last) $ S.last . t
    prop (desc ++ " init") $ eliminateOp constr (wrapMaybe init) $ \x -> do
        r <- S.init (t x)
        case r of
            Nothing -> return Nothing
            Just s -> S.toList s >>= return . Just

transformOpsWord8
    :: ([Word8] -> t IO Word8)
    -> String
    -> (t IO Word8 -> SerialT IO Word8)
    -> Spec
transformOpsWord8 constr desc t = do
    prop (desc ++ " elem") $ elemOp constr t S.elem elem
    prop (desc ++ " elem") $ elemOp constr t S.notElem notElem

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
    prop (desc ++ " <>") $ foldFromList (foldMapWith (<>) singleton) t eq
    prop (desc ++ " mappend") $ foldFromList (foldMapWith mappend singleton) t eq

applicativeOps
    :: Applicative (t IO)
    => ([Int] -> t IO Int)
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> (t IO (Int, Int) -> SerialT IO (Int, Int))
    -> ([Int], [Int])
    -> Property
applicativeOps constr eq t (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream <- run ((S.toList . t) ((,) <$> (constr a) <*> (constr b)))
        let list = (,) <$> a <*> b
        equals eq stream list

zipApplicative
    :: (IsStream t, Applicative (t IO))
    => ([Int] -> t IO Int)
    -> ([(Int, Int)] -> [(Int, Int)] -> Bool)
    -> (t IO (Int, Int) -> SerialT IO (Int, Int))
    -> ([Int], [Int])
    -> Property
zipApplicative constr eq t (a, b) = withMaxSuccess maxTestCount $
    monadicIO $ do
        stream1 <- run ((S.toList . t) ((,) <$> (constr a) <*> (constr b)))
        stream2 <- run ((S.toList . t) (pure (,) <*> (constr a) <*> (constr b)))
        stream3 <- run ((S.toList . t) (S.zipWith (,) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        equals eq stream1 list
        equals eq stream2 list
        equals eq stream3 list

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
                     (S.zipWithM (\x y -> return (x, y)) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        equals eq stream1 list

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
                     (S.zipWithM (\x y -> return (x, y)) (constr a) (constr b)))
        stream2 <-
            run
                ((S.toList . t)
                     (S.zipAsyncWithM (\x y -> return (x, y)) (constr a) (constr b)))
        let list = getZipList $ (,) <$> ZipList a <*> ZipList b
        equals eq stream1 list
        equals eq stream2 list

monadThen
    :: Monad (t IO)
    => ([Int] -> t IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> (t IO Int -> SerialT IO Int)
    -> ([Int], [Int])
    -> Property
monadThen constr eq t (a, b) = withMaxSuccess maxTestCount $ monadicIO $ do
    stream <- run ((S.toList . t) ((constr a) >> (constr b)))
    let list = a >> b
    equals eq stream list

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
                     ((constr a) >>= \x -> (constr b) >>= return . (+ x)))
        let list = a >>= \x -> b >>= return . (+ x)
        equals eq stream list

constructWithIterate :: IsStream t => (t IO Int -> SerialT IO Int) -> Spec
constructWithIterate t = do
    it "iterate" $
        (S.toList . t . (S.take 100) $ (S.iterate (+ 1) (0 :: Int)))
        `shouldReturn` (take 100 $ iterate (+ 1) 0)
    it "iterateM" $ do
        let addM = (\ y -> return (y + 1))
        S.toList . t . (S.take 100) $ S.iterateM addM (0 :: Int)
        `shouldReturn` (take 100 $ iterate (+ 1) 0)

main :: IO ()
main = hspec
    $ H.parallel
#ifdef COVERAGE_BUILD
    $ modifyMaxSuccess (const 10)
#endif
    $ do
    let folded :: IsStream t => [a] -> t IO a
        folded = serially . (\xs ->
            case xs of
                [x] -> return x -- singleton stream case
                _ -> foldMapWith (<>) return xs
            )

    let makeOps t =
            [ t
#ifndef COVERAGE_BUILD
            , t . maxRate 0
            , t . maxRate (-1)
            , t . maxBuffer 0
            , t . maxBuffer 1
            , t . maxThreads 0
            , t . maxThreads 1
            , t . maxThreads (-1)
#endif
            ]

    let serialOps :: IsStream t => ((SerialT IO a -> t IO a) -> Spec) -> Spec
        serialOps = forM_ $ (makeOps serially)
#ifndef COVERAGE_BUILD
            ++ [serially . maxRate 0.00000001]
            ++ [serially . maxBuffer (-1)]
#endif
    let wSerialOps :: IsStream t => ((WSerialT IO a -> t IO a) -> Spec) -> Spec
        wSerialOps = forM_ $ makeOps wSerially
#ifndef COVERAGE_BUILD
            ++ [wSerially . maxRate 0.00000001]
            ++ [wSerially . maxBuffer (-1)]
#endif
    let asyncOps :: IsStream t => ((AsyncT IO a -> t IO a) -> Spec) -> Spec
        asyncOps = forM_ $ makeOps asyncly
#ifndef COVERAGE_BUILD
            ++ [asyncly . maxRate 10000]
            ++ [asyncly . maxBuffer (-1)]
#endif
    let wAsyncOps :: IsStream t => ((WAsyncT IO a -> t IO a) -> Spec) -> Spec
        wAsyncOps = forM_ $ makeOps wAsyncly
#ifndef COVERAGE_BUILD
            ++ [wAsyncly . maxRate 10000]
            ++ [wAsyncly . maxBuffer (-1)]
#endif
    let aheadOps :: IsStream t => ((AheadT IO a -> t IO a) -> Spec) -> Spec
        aheadOps = forM_ $ makeOps aheadly
#ifndef COVERAGE_BUILD
             ++ [aheadly . maxRate 10000]
             ++ [aheadly . maxBuffer (-1)]
#endif
    let parallelOps :: IsStream t => ((ParallelT IO a -> t IO a) -> Spec) -> Spec
        parallelOps = forM_ $ makeOps parallely
#ifndef COVERAGE_BUILD
            ++ [parallely . maxRate 0.00000001]
            ++ [parallely . maxBuffer (-1)]
#endif
    let zipSerialOps :: IsStream t => ((ZipSerialM IO a -> t IO a) -> Spec) -> Spec
        zipSerialOps = forM_ $ makeOps zipSerially
#ifndef COVERAGE_BUILD
            ++ [zipSerially . maxRate 0.00000001]
            ++ [zipSerially . maxBuffer (-1)]
#endif
    -- Note, the "pure" of applicative Zip streams generates and infinite
    -- stream and therefore maxBuffer (-1) must not be used for that case.
    let zipAsyncOps :: IsStream t => ((ZipAsyncM IO a -> t IO a) -> Spec) -> Spec
        zipAsyncOps = forM_ $ makeOps zipAsyncly
#ifndef COVERAGE_BUILD
            ++ [zipAsyncly . maxRate 10000]
#endif

    describe "Construction" $ do
        serialOps   $ prop "serially replicateM" . constructWithReplicateM
        wSerialOps  $ prop "wSerially replicateM" . constructWithReplicateM
        aheadOps    $ prop "aheadly replicateM" . constructWithReplicateM
        asyncOps    $ prop "asyncly replicateM" . constructWithReplicateM
        wAsyncOps   $ prop "wAsyncly replicateM" . constructWithReplicateM
        parallelOps $ prop "parallely replicateM" .  constructWithReplicateM
        -- XXX test for all types of streams
        constructWithIterate serially

    describe "Functor operations" $ do
        serialOps    $ functorOps S.fromFoldable "serially" (==)
        serialOps    $ functorOps folded "serially folded" (==)
        wSerialOps   $ functorOps S.fromFoldable "wSerially" (==)
        wSerialOps   $ functorOps folded "wSerially folded" (==)
        aheadOps     $ functorOps S.fromFoldable "aheadly" (==)
        aheadOps     $ functorOps folded "aheadly folded" (==)
        asyncOps     $ functorOps S.fromFoldable "asyncly" sortEq
        asyncOps     $ functorOps folded "asyncly folded" sortEq
        wAsyncOps    $ functorOps S.fromFoldable "wAsyncly" sortEq
        wAsyncOps    $ functorOps folded "wAsyncly folded" sortEq
        parallelOps  $ functorOps S.fromFoldable "parallely" sortEq
        parallelOps  $ functorOps folded "parallely folded" sortEq
        zipSerialOps $ functorOps S.fromFoldable "zipSerially" (==)
        zipSerialOps $ functorOps folded "zipSerially folded" (==)
        zipAsyncOps  $ functorOps S.fromFoldable "zipAsyncly" (==)
        zipAsyncOps  $ functorOps folded "zipAsyncly folded" (==)

    describe "Semigroup operations" $ do
        serialOps    $ semigroupOps "serially" (==)
        wSerialOps   $ semigroupOps "wSerially" (==)
        aheadOps     $ semigroupOps "aheadly" (==)
        asyncOps     $ semigroupOps "asyncly" sortEq
        wAsyncOps    $ semigroupOps "wAsyncly" sortEq
        parallelOps  $ semigroupOps "parallely" sortEq
        zipSerialOps $ semigroupOps "zipSerially" (==)
        zipAsyncOps  $ semigroupOps "zipAsyncly" (==)

    describe "Applicative operations" $ do
        -- The tests using sorted equality are weaker tests
        -- We need to have stronger unit tests for all those
        -- XXX applicative with three arguments
        serialOps   $ prop "serially applicative" . applicativeOps S.fromFoldable (==)
        serialOps   $ prop "serially applicative folded" . applicativeOps folded (==)
        wSerialOps  $ prop "wSerially applicative" . applicativeOps S.fromFoldable sortEq
        wSerialOps  $ prop "wSerially applicative folded" . applicativeOps folded sortEq
        aheadOps    $ prop "aheadly applicative" . applicativeOps S.fromFoldable (==)
        aheadOps    $ prop "aheadly applicative folded" . applicativeOps folded (==)
        asyncOps    $ prop "asyncly applicative" . applicativeOps S.fromFoldable sortEq
        asyncOps    $ prop "asyncly applicative folded" . applicativeOps folded sortEq
        wAsyncOps   $ prop "wAsyncly applicative" . applicativeOps S.fromFoldable sortEq
        wAsyncOps   $ prop "wAsyncly applicative folded" . applicativeOps folded sortEq
        parallelOps $ prop "parallely applicative folded" . applicativeOps folded sortEq

    describe "Zip operations" $ do
        zipSerialOps $ prop "zipSerially applicative" . zipApplicative S.fromFoldable (==)
        zipSerialOps $ prop "zipSerially applicative folded" . zipApplicative folded (==)
        zipAsyncOps  $ prop "zipAsyncly applicative" . zipApplicative S.fromFoldable (==)
        zipAsyncOps  $ prop "zipAsyncly applicative folded" . zipApplicative folded (==)

        -- We test only the serial zip with serial streams and the parallel
        -- stream, because the maxRate setting in these streams can slow down
        -- zipAsync.
        serialOps   $ prop "zip monadic serially" . zipMonadic S.fromFoldable (==)
        serialOps   $ prop "zip monadic serially folded" . zipMonadic folded (==)
        wSerialOps  $ prop "zip monadic wSerially" . zipMonadic S.fromFoldable (==)
        wSerialOps  $ prop "zip monadic wSerially folded" . zipMonadic folded (==)
        aheadOps    $ prop "zip monadic aheadly" . zipAsyncMonadic S.fromFoldable (==)
        aheadOps    $ prop "zip monadic aheadly folded" . zipAsyncMonadic folded (==)
        asyncOps    $ prop "zip monadic asyncly" . zipAsyncMonadic S.fromFoldable (==)
        asyncOps    $ prop "zip monadic asyncly folded" . zipAsyncMonadic folded (==)
        wAsyncOps   $ prop "zip monadic wAsyncly" . zipAsyncMonadic S.fromFoldable (==)
        wAsyncOps   $ prop "zip monadic wAsyncly folded" . zipAsyncMonadic folded (==)
        parallelOps $ prop "zip monadic parallely" . zipMonadic S.fromFoldable (==)
        parallelOps $ prop "zip monadic parallely folded" . zipMonadic folded (==)

    describe "Monad operations" $ do
        serialOps   $ prop "serially monad then" . monadThen S.fromFoldable (==)
        wSerialOps  $ prop "wSerially monad then" . monadThen S.fromFoldable sortEq
        aheadOps    $ prop "aheadly monad then" . monadThen S.fromFoldable (==)
        asyncOps    $ prop "asyncly monad then" . monadThen S.fromFoldable sortEq
        wAsyncOps   $ prop "wAsyncly monad then" . monadThen S.fromFoldable sortEq
        parallelOps $ prop "parallely monad then" . monadThen S.fromFoldable sortEq

        serialOps   $ prop "serially monad then folded" . monadThen folded (==)
        wSerialOps  $ prop "wSerially monad then folded" . monadThen folded sortEq
        aheadOps    $ prop "aheadly monad then folded" . monadThen folded (==)
        asyncOps    $ prop "asyncly monad then folded" . monadThen folded sortEq
        wAsyncOps   $ prop "wAsyncly monad then folded" . monadThen folded sortEq
        parallelOps $ prop "parallely monad then folded" . monadThen folded sortEq

        serialOps   $ prop "serially monad bind" . monadBind S.fromFoldable (==)
        wSerialOps  $ prop "wSerially monad bind" . monadBind S.fromFoldable sortEq
        aheadOps    $ prop "aheadly monad bind" . monadBind S.fromFoldable (==)
        asyncOps    $ prop "asyncly monad bind" . monadBind S.fromFoldable sortEq
        wAsyncOps   $ prop "wAsyncly monad bind" . monadBind S.fromFoldable sortEq
        parallelOps $ prop "parallely monad bind" . monadBind S.fromFoldable sortEq

        serialOps   $ prop "serially monad bind folded"  . monadBind folded (==)
        wSerialOps  $ prop "wSerially monad bind folded" . monadBind folded sortEq
        aheadOps    $ prop "aheadly monad bind folded"   . monadBind folded (==)
        asyncOps    $ prop "asyncly monad bind folded"   . monadBind folded sortEq
        wAsyncOps   $ prop "wAsyncly monad bind folded"  . monadBind folded sortEq
        parallelOps $ prop "parallely monad bind folded" . monadBind folded sortEq

    describe "Stream transform operations" $ do
        serialOps    $ transformOps S.fromFoldable "serially" (==)
        wSerialOps   $ transformOps S.fromFoldable "wSerially" (==)
        aheadOps     $ transformOps S.fromFoldable "aheadly" (==)
        asyncOps     $ transformOps S.fromFoldable "asyncly" sortEq
        wAsyncOps    $ transformOps S.fromFoldable "wAsyncly" sortEq
        parallelOps  $ transformOps S.fromFoldable "parallely" sortEq
        zipSerialOps $ transformOps S.fromFoldable "zipSerially" (==)
        zipAsyncOps  $ transformOps S.fromFoldable "zipAsyncly" (==)

        serialOps    $ transformOps folded "serially folded" (==)
        wSerialOps   $ transformOps folded "wSerially folded" (==)
        aheadOps     $ transformOps folded "aheadly folded" (==)
        asyncOps     $ transformOps folded "asyncly folded" sortEq
        wAsyncOps    $ transformOps folded "wAsyncly folded" sortEq
        parallelOps  $ transformOps folded "parallely folded" sortEq
        zipSerialOps $ transformOps folded "zipSerially folded" (==)
        zipAsyncOps  $ transformOps folded "zipAsyncly folded" (==)

        serialOps    $ transformOpsWord8 S.fromFoldable "serially"
        wSerialOps   $ transformOpsWord8 S.fromFoldable "wSerially"
        aheadOps     $ transformOpsWord8 S.fromFoldable "aheadly"
        asyncOps     $ transformOpsWord8 S.fromFoldable "asyncly"
        wAsyncOps    $ transformOpsWord8 S.fromFoldable "wAsyncly"
        parallelOps  $ transformOpsWord8 S.fromFoldable "parallely"
        zipSerialOps $ transformOpsWord8 S.fromFoldable "zipSerially"
        zipAsyncOps  $ transformOpsWord8 S.fromFoldable "zipAsyncly"

        serialOps    $ transformOpsWord8 folded "serially folded"
        wSerialOps   $ transformOpsWord8 folded "wSerially folded"
        aheadOps     $ transformOpsWord8 folded "aheadly folded"
        asyncOps     $ transformOpsWord8 folded "asyncly folded"
        wAsyncOps    $ transformOpsWord8 folded "wAsyncly folded"
        parallelOps  $ transformOpsWord8 folded "parallely folded"
        zipSerialOps $ transformOpsWord8 folded "zipSerially folded"
        zipAsyncOps  $ transformOpsWord8 folded "zipAsyncly folded"

    -- These tests won't work with maxBuffer or maxThreads set to 1, so we
    -- exclude those cases from these.
    let mkOps t =
            [ t
#ifndef COVERAGE_BUILD
            , t . maxRate 0
            , t . maxRate (-1)
            , t . maxBuffer 0
            , t . maxThreads 0
            , t . maxThreads (-1)
#endif
            ]

    describe "Stream concurrent operations" $ do
        forM_ (mkOps aheadly)   $ concurrentOps S.fromFoldable "aheadly" (==)
        forM_ (mkOps asyncly)   $ concurrentOps S.fromFoldable "asyncly" sortEq
        forM_ (mkOps wAsyncly)  $ concurrentOps S.fromFoldable "wAsyncly" sortEq
        forM_ (mkOps parallely) $ concurrentOps S.fromFoldable "parallely" sortEq

        forM_ (mkOps aheadly)   $ concurrentOps folded "aheadly folded" (==)
        forM_ (mkOps asyncly)   $ concurrentOps folded "asyncly folded" sortEq
        forM_ (mkOps wAsyncly)  $ concurrentOps folded "wAsyncly folded" sortEq
        forM_ (mkOps parallely) $ concurrentOps folded "parallely folded" sortEq

    describe "Concurrent application" $ do
        serialOps $ prop "serial" . concurrentApplication (==)
        asyncOps $ prop "async" . concurrentApplication sortEq
        aheadOps $ prop "ahead" . concurrentApplication (==)
        parallelOps $ prop "parallel" . concurrentApplication sortEq

        prop "concurrent foldr application" $ withMaxSuccess maxTestCount $
            concurrentFoldrApplication
        prop "concurrent foldl application" $ withMaxSuccess maxTestCount $
            concurrentFoldlApplication

    -- These tests are specifically targeted towards detecting illegal sharing
    -- of SVar across conurrent streams. All transform ops must be added here.
    describe "Stream transform and combine operations" $ do
        serialOps    $ transformCombineOpsCommon S.fromFoldable "serially" (==)
        wSerialOps   $ transformCombineOpsCommon S.fromFoldable "wSerially" sortEq
        aheadOps     $ transformCombineOpsCommon S.fromFoldable "aheadly" (==)
        asyncOps     $ transformCombineOpsCommon S.fromFoldable "asyncly" sortEq
        wAsyncOps    $ transformCombineOpsCommon S.fromFoldable "wAsyncly" sortEq
        parallelOps  $ transformCombineOpsCommon S.fromFoldable "parallely" sortEq
        zipSerialOps $ transformCombineOpsCommon S.fromFoldable "zipSerially" (==)
        zipAsyncOps  $ transformCombineOpsCommon S.fromFoldable "zipAsyncly" (==)

        serialOps    $ transformCombineOpsCommon folded "serially" (==)
        wSerialOps   $ transformCombineOpsCommon folded "wSerially" sortEq
        aheadOps     $ transformCombineOpsCommon folded "aheadly" (==)
        asyncOps     $ transformCombineOpsCommon folded "asyncly" sortEq
        wAsyncOps    $ transformCombineOpsCommon folded "wAsyncly" sortEq
        parallelOps  $ transformCombineOpsCommon folded "parallely" sortEq
        zipSerialOps $ transformCombineOpsCommon folded "zipSerially" (==)
        zipAsyncOps  $ transformCombineOpsCommon folded "zipAsyncly" (==)

        serialOps    $ transformCombineOpsOrdered S.fromFoldable "serially" (==)
        aheadOps     $ transformCombineOpsOrdered S.fromFoldable "aheadly" (==)
        zipSerialOps $ transformCombineOpsOrdered S.fromFoldable "zipSerially" (==)
        zipAsyncOps  $ transformCombineOpsOrdered S.fromFoldable "zipAsyncly" (==)

    describe "Stream elimination operations" $ do
        serialOps    $ eliminationOps S.fromFoldable "serially"
        wSerialOps   $ eliminationOps S.fromFoldable "wSerially"
        aheadOps     $ eliminationOps S.fromFoldable "aheadly"
        asyncOps     $ eliminationOps S.fromFoldable "asyncly"
        wAsyncOps    $ eliminationOps S.fromFoldable "wAsyncly"
        parallelOps  $ eliminationOps S.fromFoldable "parallely"
        zipSerialOps $ eliminationOps S.fromFoldable "zipSerially"
        zipAsyncOps  $ eliminationOps S.fromFoldable "zipAsyncly"

        serialOps    $ eliminationOps folded "serially folded"
        wSerialOps   $ eliminationOps folded "wSerially folded"
        aheadOps     $ eliminationOps folded "aheadly folded"
        asyncOps     $ eliminationOps folded "asyncly folded"
        wAsyncOps    $ eliminationOps folded "wAsyncly folded"
        parallelOps  $ eliminationOps folded "parallely folded"
        zipSerialOps $ eliminationOps folded "zipSerially folded"
        zipAsyncOps  $ eliminationOps folded "zipAsyncly folded"

    -- XXX Add a test where we chain all transformation APIs and make sure that
    -- the state is being passed through all of them.
    describe "Stream serial elimination operations" $ do
        serialOps    $ serialEliminationOps S.fromFoldable "serially"
        wSerialOps   $ serialEliminationOps S.fromFoldable "wSerially"
        aheadOps     $ serialEliminationOps S.fromFoldable "aheadly"
        zipSerialOps $ serialEliminationOps S.fromFoldable "zipSerially"
        zipAsyncOps  $ serialEliminationOps S.fromFoldable "zipAsyncly"

        serialOps    $ serialEliminationOps folded "serially folded"
        wSerialOps   $ serialEliminationOps folded "wSerially folded"
        aheadOps     $ serialEliminationOps folded "aheadly folded"
        zipSerialOps $ serialEliminationOps folded "zipSerially folded"
        zipAsyncOps  $ serialEliminationOps folded "zipAsyncly folded"
