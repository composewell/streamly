{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes          #-}

module Main where

import           Control.Applicative (Alternative(..))
import           Control.Exception (assert)
import           Control.Monad (guard)
import           Gauge.Main (defaultMain, bgroup, bench, nfIO)
import           Data.Function ((&))

import qualified Streamly as A
import qualified Streamly.Prelude as A

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
    let as = streamly_serial
        ai = streamly_interleaved
        aa = streamly_async
        ap = streamly_parallel
    defaultMain [
        bgroup "streamly"
            [ bench "function style all serial" $ nfIO streamly_function_style

            , bgroup "serial bind"
                [ bench "serial"        $ nfIO (as (A.serial))
                , bench "fair serial"   $ nfIO (as (A.coserial))
                , bench "left parallel" $ nfIO (as (A.coparallel))
                , bench "fair parallel" $ nfIO (as (A.parallel))
                ]

            , bgroup "interleaved bind"
                [ bench "serial"        $ nfIO (ai (A.serial))
                , bench "fair serial"   $ nfIO (ai (A.coserial))
                , bench "left parallel" $ nfIO (ai (A.coparallel))
                , bench "fair parallel" $ nfIO (ai (A.parallel))
                ]

            , bgroup "async bind"
                [ bench "serial"        $ nfIO (aa (A.serial))
                , bench "fair serial"   $ nfIO (aa (A.coserial))
                , bench "left parallel" $ nfIO (aa (A.coparallel))
                , bench "fair parallel" $ nfIO (aa (A.parallel))
                ]

            , bgroup "parallel bind"
                [ bench "serial"        $ nfIO (ap (A.serial))
                , bench "fair serial"   $ nfIO (ap (A.coserial))
                , bench "left parallel" $ nfIO (ap (A.coparallel))
                , bench "fair parallel" $ nfIO (ap (A.parallel))
                ]

            -- Benchmark smallest possible actions composed together
            , bgroup "serial bind nil"
                [ bench "serial"        $ nfIO (streamly_nil (A.serial))
                , bench "fair serial"   $ nfIO (streamly_nil (A.coserial))
                , bench "left parallel" $ nfIO (streamly_nil (A.coparallel))
                , bench "fair parallel" $ nfIO (streamly_nil (A.parallel))
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
            , bench "simple-conduit-bind" $ nfIO simple_conduit_bind
            , bench "machines"      $ nfIO machines_basic
            ]
#endif
        ]

{-# INLINABLE map #-}
map :: Monad m => (a -> Int) -> a -> m Int
map f x = return $ f x

guard'           :: (A.IsStream t, Applicative (t m)) => Bool -> t m ()
guard' True      =  pure ()
guard' False     =  A.nil

{-# INLINABLE filter #-}
filter :: (Monad m, Alternative m) => (a -> Bool) -> a -> m a
filter cond x = guard (not $ cond x) >> return x

amap :: Monad (s IO) => (Int -> Int) -> Int -> s IO Int
amap = Main.map

afilter :: (A.IsStream s, Monad (s IO)) => (Int -> Bool) -> Int -> s IO Int
afilter cond x = guard' (not $ cond x) >> return x

{-# INLINE streamly_basic #-}
streamly_basic
    :: (Monad (t IO), A.IsStream t)
    => (forall a. t IO a -> IO [a])
    -> (t IO Int -> t IO Int -> t IO Int)
    -> IO Int
streamly_basic tl g = do
    xs <- tl $ do
             A.drop 100 (A.forEachWith g [1..100000 :: Int] $ \x ->
                afilter even x >>= amap (+1))
                    >>= amap (+1)
                    >>= afilter (\y -> y `mod` 2 == 0)
    assert (Prelude.length xs == 49900) $
        return (Prelude.length xs)

{-# INLINE streamly_function_style #-}
streamly_function_style :: IO Int
streamly_function_style = do
    xs <- A.toList $ A.serially $
          A.fromFoldable [1..100000 :: Int]
        & A.filter even
        & fmap (+1)
        & A.drop 100
        & fmap (+1)
        & A.filter (\y -> y `mod` 2 == 0)
    assert (Prelude.length xs == 49900) $
        return (Prelude.length xs)

{-# INLINE streamly_serial #-}
streamly_serial
    :: (A.SerialT IO Int -> A.SerialT IO Int -> A.SerialT IO Int)
    -> IO Int
streamly_serial = streamly_basic (A.toList . A.serially)

{-# INLINE streamly_interleaved #-}
streamly_interleaved
    :: (A.CoserialT IO Int -> A.CoserialT IO Int -> A.CoserialT IO Int)
    -> IO Int
streamly_interleaved = streamly_basic (A.toList . A.coserially)

{-# INLINE streamly_async #-}
streamly_async
    :: (A.CoparallelT IO Int -> A.CoparallelT IO Int -> A.CoparallelT IO Int)
    -> IO Int
streamly_async = streamly_basic (A.toList . A.coparallely)

{-# INLINE streamly_parallel #-}
streamly_parallel
    :: (A.ParallelT IO Int -> A.ParallelT IO Int -> A.ParallelT IO Int)
    -> IO Int
streamly_parallel = streamly_basic (A.toList . A.parallely)

{-# INLINE streamly_nil #-}
streamly_nil :: (A.SerialT IO Int -> A.SerialT IO Int -> A.SerialT IO Int)
    -> IO Int
streamly_nil f = do
    xs <- (A.toList . A.serially) $ do
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

smap :: Monad (s IO) => (Int -> Int) -> Int -> s IO Int
smap = Main.map

sfilter :: (Alternative (s IO), Monad (s IO)) => (Int -> Bool) -> Int -> s IO Int
sfilter = Main.filter

{-# INLINE simple_conduit_bind #-}
simple_conduit_bind :: IO Int
simple_conduit_bind = do
    xs <- S.sinkList $ do
             S.dropC 100 (S.sourceList [1..100000 :: Int] >>= \x ->
                sfilter even x >>= smap (+1))
                    >>= smap (+1)
                    >>= sfilter (\y -> y `mod` 2 == 0)
    assert (Prelude.length xs == 49900) $
        return (Prelude.length xs)

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
