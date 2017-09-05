module Main where

import           Control.Applicative (Alternative(..))
import           Control.Exception (assert)
import           Control.Monad (guard)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Criterion.Main (defaultMain, bgroup, bench, nfIO)
import           Data.Atomics (atomicModifyIORefCAS)
import           Data.IORef (IORef, newIORef, writeIORef)
import           System.IO.Unsafe (unsafePerformIO)

import qualified Asyncly as A
import qualified Conduit.Simple as S
import qualified Control.Monad.Logic as LG
import qualified Data.Machine as M
import qualified Transient.Internals as T
import qualified Transient.Indeterminism as T
import qualified ListT        as LT

main :: IO ()
main = do
    defaultMain [
        bgroup "basic"
            [ bench "asyncly-serial"              $ nfIO (asyncly_basic (A.<>))
            , bench "asyncly-interleaved"         $ nfIO (asyncly_basic (A.<=>))
            , bench "asyncly-parleft"             $ nfIO (asyncly_basic (A.<|))
            , bench "asyncly-parinterleaved"      $ nfIO (asyncly_basic (A.<|>))
            , bench "asyncly-serial-nil"          $ nfIO (asyncly_nil (A.<>))
            , bench "asyncly-interleaved-nil"     $ nfIO (asyncly_nil (A.<=>))
            , bench "asyncly-parleft-nil"         $ nfIO (asyncly_nil (A.<|))
            , bench "asyncly-parinterleaved-nil"  $ nfIO (asyncly_nil (A.<|>))
            , bench "transient"     $ nfIO transient_basic
            , bench "transient-nil" $ nfIO transient_nil
            , bench "logict"        $ nfIO logict_basic
            , bench "list-t"        $ nfIO list_t_basic
            , bench "simple-conduit" $ nfIO simple_conduit_basic
            , bench "machines"      $ nfIO machines_basic
            ]
        ]

{-# INLINABLE map #-}
map :: Monad m => (a -> Int) -> a -> m Int
map f x = return $ f x

{-# INLINABLE filter #-}
filter :: (Monad m, Alternative m) => (a -> Bool) -> a -> m a
filter cond x = guard (not $ cond x) >> return x

{-# NOINLINE count #-}
count :: IORef Int
count = unsafePerformIO $ newIORef 0

drop :: (MonadIO m, Alternative m) => Int -> Int -> m Int
drop num x =  do

    mn <- liftIO $ atomicModifyIORefCAS count $ \n ->
            if n < num then (n + 1, False) else (n, True)
    guard mn
    return x

tmap :: (a -> Int) -> a -> T.TransIO Int
tmap = Main.map

tfilter :: (a -> Bool) -> a -> T.TransIO a
tfilter = Main.filter

tdrop :: Int -> Int -> T.TransIO Int
tdrop = Main.drop

transient_basic :: IO (Maybe Int)
transient_basic = T.keep' $ T.threads 0 $ do
    liftIO $ writeIORef count 0
    xs <- T.group 49900  $  do
             T.choose  [1..100000 :: Int]
             >>= tfilter even
             >>= tmap (+1)
             >>= tdrop 100
             >>= tmap (+1)
             >>= tfilter (\x -> x `mod` 2 == 0)

    assert (Prelude.length xs == 49900) $
        T.exit (Prelude.length xs)

transient_nil :: IO (Maybe Int)
transient_nil = T.keep' $ T.threads 0 $ do
    xs <- T.group 49900  $  do
             T.choose  [1..100000 :: Int]
    assert (Prelude.length xs == 49900) $
        T.exit (Prelude.length xs)

amap :: (Int -> Int) -> Int -> A.AsyncT IO Int
amap = Main.map

afilter :: (Int -> Bool) -> Int -> A.AsyncT IO Int
afilter = Main.filter

adrop :: Int -> Int -> A.AsyncT IO Int
adrop = Main.drop

{-# INLINE asyncly_basic #-}
asyncly_basic :: (A.AsyncT IO Int -> A.AsyncT IO Int -> A.AsyncT IO Int)
    -> IO Int
asyncly_basic f = do
    writeIORef count 0
    xs <- A.toList $ do
             (A.forEachWith f [1..100000 :: Int] $ \x ->
                return x
                >>= afilter even
                >>= amap (+1)
                >>= adrop 100
                >>= amap (+1)
                >>= afilter (\y -> y `mod` 2 == 0))
    assert (Prelude.length xs == 49900) $
        return (Prelude.length xs)

{-# INLINE asyncly_nil #-}
asyncly_nil :: (A.AsyncT IO Int -> A.AsyncT IO Int -> A.AsyncT IO Int)
    -> IO Int
asyncly_nil f = do
    xs <- A.toList $ do
             (A.forEachWith f [1..100000:: Int] $
                \x -> return x >>= return . id)
    assert (Prelude.length xs == 100000) $
        return (Prelude.length xs)

lfilter :: (Int -> Bool) -> Int -> LT.ListT IO Int
lfilter = Main.filter

lmap :: (Int -> Int) -> Int -> LT.ListT IO Int
lmap = Main.map

ldrop :: Int -> Int -> LT.ListT IO Int
ldrop = Main.drop

list_t_basic :: IO Int
list_t_basic = do
    writeIORef count 0
    xs <- LT.toList $ do
             LT.fromFoldable [1..100000 :: Int]
             >>= lfilter even
             >>= lmap (+1)
             >>= ldrop 100
             >>= lmap (+1)
             >>= lfilter (\x -> x `mod` 2 == 0)
    assert (Prelude.length xs == 49900) $
        return (Prelude.length xs)

lgfilter :: (Int -> Bool) -> Int -> LG.LogicT IO Int
lgfilter = Main.filter

lgmap :: (Int -> Int) -> Int -> LG.LogicT IO Int
lgmap = Main.map

lgdrop :: Int -> Int -> LG.LogicT IO Int
lgdrop = Main.drop

logict_basic :: IO Int
logict_basic = do
    writeIORef count 0
    --xs <- LG.observeManyT 2900 $ do
    xs <- LG.observeAllT $ do
             LG.msum $ Prelude.map return [1..100000]
             >>= lgfilter even
             >>= lgmap (+1)
             >>= lgdrop 100
             >>= lgmap (+1)
             >>= lgfilter (\x -> x `mod` 2 == 0)
    assert (Prelude.length xs == 49900) $
        return (Prelude.length xs)

simple_conduit_basic :: IO Int
simple_conduit_basic = do
    xs <-   S.sourceList [1..100000]
      S.$= S.filterC even
      S.$= S.mapC ((+1) :: Int -> Int)
      S.$= S.dropC 100
      S.$= S.mapC ((+1) :: Int -> Int)
      S.$= S.filterC (\x -> x `mod` 2 == 0)
      S.$$ S.sinkList
    assert (Prelude.length xs == 49900) $
        return (Prelude.length (xs :: [Int]))

machines_basic :: IO Int
machines_basic = do
    xs <- M.runT $ M.source [1..100000]
      M.~> M.filtered even
      M.~> M.mapping (+1)
      M.~> M.dropping 100
      M.~> M.mapping (+1)
      M.~> M.filtered (\x -> x `mod` 2 == 0)
    assert (Prelude.length xs == 49900) $
        return (Prelude.length (xs ::[Int]))
