{-# LANGUAGE CPP #-}

module Main where

import           Control.Applicative (Alternative(..))
import           Control.Exception (assert)
import           Control.Monad (guard)
import           Criterion.Main (defaultMain, bgroup, bench, nfIO)

import qualified Asyncly as A

#ifdef EXTRA_BENCHMARKS
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Atomics (atomicModifyIORefCAS)
import           Data.IORef (IORef, newIORef, writeIORef)
import           System.IO.Unsafe (unsafePerformIO)

import qualified Conduit.Simple as S
import qualified Control.Monad.Logic as LG
import qualified Data.Machine as M
#if MIN_VERSION_transient(0,5,1)
import qualified Transient.Internals as T
import qualified Transient.Indeterminism as T
#endif
import qualified ListT        as LT
#endif

main :: IO ()
main = do
    -- XXX due to a GHC bug passing bind as an argument causes perf
    -- degradation, so we should keep that in account when comparing.
    let as = asyncly_basic
    defaultMain [
        bgroup "asyncly"
            [ bgroup "serial bind"
                [ bench "serial"        $ nfIO (as (>>=) (A.<>))
                , bench "fair serial"   $ nfIO (as (>>=) (A.<=>))
                , bench "left parallel" $ nfIO (as (>>=) (A.<|))
                , bench "fair parallel" $ nfIO (as (>>=) (A.<|>))
                ]

            , bgroup "fair bind"
                [ bench "serial"        $ nfIO (as (A.>->) (A.<>))
                , bench "fair serial"   $ nfIO (as (A.>->) (A.<=>))
                , bench "left parallel" $ nfIO (as (A.>->) (A.<|))
                , bench "fair parallel" $ nfIO (as (A.>->) (A.<|>))
                ]

            , bgroup "parallel bind"
                [ bench "serial"        $ nfIO (as (A.>>|) (A.<>))
                , bench "fair serial"   $ nfIO (as (A.>>|) (A.<=>))
                , bench "left parallel" $ nfIO (as (A.>>|) (A.<|))
                , bench "fair parallel" $ nfIO (as (A.>>|) (A.<|>))
                ]

            , bgroup "fair parallel bind"
                [ bench "serial"        $ nfIO (as (A.>|>) (A.<>))
                , bench "fair serial"   $ nfIO (as (A.>|>) (A.<=>))
                , bench "left parallel" $ nfIO (as (A.>|>) (A.<|))
                , bench "fair parallel" $ nfIO (as (A.>|>) (A.<|>))
                ]

            -- Benchmark smallest possible actions composed together
            , bgroup "serial bind nil"
                [ bench "serial"        $ nfIO (asyncly_nil (A.<>))
                , bench "fair serial"   $ nfIO (asyncly_nil (A.<=>))
                , bench "left parallel" $ nfIO (asyncly_nil (A.<|))
                , bench "fair parallel" $ nfIO (asyncly_nil (A.<|>))
                ]
            ]
#ifdef EXTRA_BENCHMARKS
#if MIN_VERSION_transient(0,5,1)
        , bgroup "others"
            [ bench "transient"     $ nfIO transient_basic
            , bench "transient-nil" $ nfIO transient_nil
#endif
            , bench "logict"        $ nfIO logict_basic
            , bench "list-t"        $ nfIO list_t_basic
            , bench "simple-conduit" $ nfIO simple_conduit_basic
            , bench "machines"      $ nfIO machines_basic
            ]
#endif
        ]

{-# INLINABLE map #-}
map :: Monad m => (a -> Int) -> a -> m Int
map f x = return $ f x

{-# INLINABLE filter #-}
filter :: (Monad m, Alternative m) => (a -> Bool) -> a -> m a
filter cond x = guard (not $ cond x) >> return x

amap :: (Int -> Int) -> Int -> A.AsyncT IO Int
amap = Main.map

afilter :: (Int -> Bool) -> Int -> A.AsyncT IO Int
afilter = Main.filter

{-# INLINE asyncly_basic #-}
asyncly_basic
    :: (A.AsyncT IO Int -> (Int -> A.AsyncT IO Int) -> A.AsyncT IO Int)
    -> (A.AsyncT IO Int -> A.AsyncT IO Int -> A.AsyncT IO Int)
    -> IO Int
asyncly_basic f g = do
    xs <- A.toList $ do
             A.drop 100 (A.forEachWith g [1..100000 :: Int] $ \x ->
                afilter even x `f` amap (+1))
                    `f` amap (+1)
                    `f` afilter (\y -> y `mod` 2 == 0)
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

#ifdef EXTRA_BENCHMARKS
#if MIN_VERSION_transient(0,5,1)

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
#endif

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
#endif
