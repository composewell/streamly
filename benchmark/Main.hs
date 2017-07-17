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
import           Data.IORef (IORef, newIORef, writeIORef)
import           Data.Atomics (atomicModifyIORefCAS)
import           System.IO.Unsafe (unsafePerformIO)
import           Transient.Internals as T
import           Transient.Indeterminism as T

main :: IO ()
main = do
    defaultMain [
        bgroup "basic"  [ bench "transient"      $ nfIO transient_basic
                        , bench "stream"         $ nfIO stream_basic
                        , bench "iostreams"      $ nfIO iostreams_basic
                        , bench "pipes"          $ nfIO pipes_basic
                        , bench "conduit"        $ nfIO conduit_basic
                    --    , bench "simple-conduit" $ nfIO simple_conduit_basic
                     --  , bench "fusion"         $ nfIO fusion_basic

                       ]
        ]

{-# INLINABLE map #-}
map :: (a -> Int) -> a -> TransIO Int
map f x = return $ f x

{-# INLINABLE filter #-}
filter :: (a -> Bool) -> a -> TransIO a
filter cond x = guard (cond x) >> return x

{-# NOINLINE count #-}
count :: IORef Int
count = unsafePerformIO $ newIORef 0

drop :: Int -> Int -> TransIO Int
drop num x =  do

    mn <- liftIO $ atomicModifyIORefCAS count $ \n ->
            let n' = n + 1
            in if n' < num then (n + 1, False) else (n + 1, True)
    guard mn
    return x

transient_basic :: IO Int
transient_basic = T.keep' $ T.threads 0 $ do
    xs <- T.group 499000  $  do
             liftIO $ writeIORef count 0
             T.choose  [1..1000000::Int]
             >>= Main.filter even
             >>= Main.map (+1)
             >>= Main.drop 1000
             >>= Main.map  (+1)
             >>= Main.filter (\x -> x `mod` 2 == 0)

    assert (Prelude.length xs == 499000) $
        T.exit (Prelude.length xs)

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
    xs <- Str.toList $ Str.each ns
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
