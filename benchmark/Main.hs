{-# LANGUAGE RecordWildCards #-}
module Main where

import           Criterion.Main (defaultMain, bgroup, bench, nfIO)
import           Control.Exception (assert)
--
-- import           Fusion as F hiding ((&))
import           Data.Function ((&))

import           Data.Conduit as C
import           Data.Conduit.Combinators as C
import           Pipes as P
import qualified Pipes.Prelude as P
import qualified Streaming.Prelude as Str
import qualified System.IO.Streams as IOS
import           Conduit.Simple as S

-- Transient
import           Control.Monad (guard)
import           Control.Applicative (Alternative)
import           Data.IORef (IORef, newIORef, writeIORef)
import           Data.Atomics (atomicModifyIORefCAS)
import           System.IO.Unsafe (unsafePerformIO)
import           Transient.Internals as T
import           Transient.Indeterminism as T

import           Data.Machine as M
import           Asyncly as A

main :: IO ()
main = do
    defaultMain [
        bgroup "basic"  [ bench "asyncly"        $ nfIO asyncly_basic
                        , bench "transient"      $ nfIO transient_basic
                        , bench "machines"       $ nfIO machines_basic
                        , bench "stream"         $ nfIO stream_basic
                        , bench "conduit"        $ nfIO conduit_basic
                        , bench "pipes"          $ nfIO pipes_basic
                        , bench "simple-conduit" $ nfIO simple_conduit_basic
                        , bench "iostreams"      $ nfIO iostreams_basic
                     --  , bench "fusion"         $ nfIO fusion_basic

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

tmap :: (a -> Int) -> a -> TransIO Int
tmap = Main.map

tfilter :: (a -> Bool) -> a -> TransIO a
tfilter = Main.filter

tdrop :: Int -> Int -> TransIO Int
tdrop = Main.drop

transient_basic :: IO Int
transient_basic = T.keep' $ T.threads 0 $ do
    liftIO $ writeIORef count 0
    xs <- T.group 499001  $  do
             T.choose  [1..1000001 :: Int]
             >>= tfilter even
             >>= tmap (+1)
             >>= tdrop 1000
             >>= tmap (+1)
             >>= tfilter (\x -> x `mod` 2 == 0)

    assert (Prelude.length xs == 499000) $
        T.exit (Prelude.length xs)

amap :: (a -> Int) -> a -> AsynclyT IO Int
amap = Main.map

afilter :: (a -> Bool) -> a -> AsynclyT IO a
afilter = Main.filter

adrop :: Int -> Int -> AsynclyT IO Int
adrop = Main.drop

asyncly_basic :: IO Int
asyncly_basic = do
    writeIORef count 0
    xs <- wait $ A.threads 0 $ do
             A.each [1..1000000 :: Int]
             >>= afilter even
             >>= amap (+1)
             >>= adrop 1000
             >>= amap (+1)
             >>= afilter (\x -> x `mod` 2 == 0)
    assert (Prelude.length xs == 499000) $
        return (Prelude.length xs)

machines_basic :: IO Int
machines_basic = do
    xs <- M.runT $ M.source [1..1000000]
      M.~> M.filtered even
      M.~> M.mapping (+1)
      M.~> M.dropping 1000
      M.~> M.mapping (+1)
      M.~> M.filtered (\x -> x `mod` 2 == 0)
    assert (Prelude.length xs == 499000) $
        return (Prelude.length (xs ::[Int]))

pipes_basic :: IO Int
pipes_basic = do
    xs <- P.toListM $ P.each [1..1000000]
      >-> P.filter even
      >-> P.map (+1)
      >-> P.drop 1000
      >-> P.map (+1)
      >-> P.filter (\x -> x `mod` 2 == 0)
    assert (Prelude.length xs == 499000) $
        return (Prelude.length (xs ::[Int]))

conduit_basic :: IO Int
conduit_basic = do
    xs <-   C.yieldMany [1..1000000]
      C.$= C.filter even
      C.$= C.map ((+1) :: Int -> Int)
      C.$= (C.drop 1000 >> C.awaitForever C.yield)
      C.$= C.map ((+1) :: Int -> Int)
      C.$= C.filter (\x -> x `mod` 2 == 0)
      C.$$ C.sinkList
    assert (Prelude.length xs == 499000) $
        return (Prelude.length (xs :: [Int]))

simple_conduit_basic :: IO Int
simple_conduit_basic = do
    xs <-   S.sourceList [1..1000000]
      S.$= S.filterC even
      S.$= S.mapC ((+1) :: Int -> Int)
      S.$= S.dropC 1000
      S.$= S.mapC ((+1) :: Int -> Int)
      S.$= S.filterC (\x -> x `mod` 2 == 0)
      S.$$ S.sinkList
    assert (Prelude.length xs == 499000) $
        return (Prelude.length (xs :: [Int]))



-- fusion_basic :: IO Int
-- fusion_basic = do
--     xs <- F.toListM $ F.each [1..1000000]
--       & F.filter even
--       & F.map (+1)
--       & F.drop 1000
--       & F.map (+1)
--       & F.filter (\x -> x `mod` 2 == 0)
--     assert (Prelude.length xs == 499000) $
--         return (Prelude.length (xs :: [Int]))

stream_basic :: IO Int
stream_basic = do
    let ns = [1..1000000] :: [Int]
    xs Str.:> _ <- Str.toList $ Str.each ns
      & Str.filter even
      & Str.map (+1)
      & Str.drop 1000
      & Str.map (+1)
      & Str.filter (\x -> x `mod` 2 == 0)
    assert (Prelude.length xs == 499000) $
         return (Prelude.length xs)

iostreams_basic :: IO Int
iostreams_basic = do
  s0 <- IOS.fromList [1..1000000]
  s1 <- IOS.filter even s0
  s2 <- IOS.map (+1) s1
  s3 <- IOS.drop 1000 s2
  s4 <- IOS.map (+1) s3
  s5 <- IOS.filter (\x -> x `mod` 2 == 0) s4
  xs <- IOS.toList s5
  assert (Prelude.length xs == 499000) $
      return (Prelude.length (xs :: [Int]))
