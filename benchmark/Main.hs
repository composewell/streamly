module Main where

import           Gauge.Main (defaultMain, bench, nfIO)
import           Data.Function ((&))

import qualified Streamly as S
import qualified Streamly.Prelude as S

{-# INLINE streamlyOp #-}
streamlyOp :: IO Int
streamlyOp = do
    xs <- S.toList $ S.serially $
          S.each [1..100000 :: Int]
        & fmap (+1)
        & fmap (+1)
        & fmap (+1)
        & fmap (+1)
    return (Prelude.length xs)

main :: IO ()
main = do
    defaultMain [ bench "streaming ops" $ nfIO streamlyOp ]
