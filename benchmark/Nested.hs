-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.DeepSeq (NFData)
import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)
import qualified NestedOps as Ops
import Streamly
import Gauge
import System.Environment (getArgs)
import Text.Read (readMaybe)

benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

_benchId :: (NFData b) => String -> (Int -> Identity b) -> Benchmark
_benchId name f = bench name $ nf (\g -> runIdentity (g 1))  f

main :: IO ()
main = do
  -- TBD Study scaling with 10, 100, 1000 loop iterations
  -- Basement.Terminal.initialize required?
  args <- getArgs
  let (linearCount, args') = parseValue args
      (cfg, extra) = parseWith defaultConfig args'
  runMode (mode cfg) cfg extra
    [ bgroup "serially"
      [ benchIO "toNullAp"       $ Ops.toNullAp linearCount       serially
      , benchIO "toNull"         $ Ops.toNull linearCount         serially
      , benchIO "toNull3"        $ Ops.toNull3 linearCount        serially
      , benchIO "toList"         $ Ops.toList linearCount         serially
   --   , benchIO "toListSome"     $ Ops.toListSome     serially
      , benchIO "filterAllOut"   $ Ops.filterAllOut linearCount   serially
      , benchIO "filterAllIn"    $ Ops.filterAllIn linearCount    serially
      , benchIO "filterSome"     $ Ops.filterSome linearCount     serially
      , benchIO "breakAfterSome" $ Ops.breakAfterSome linearCount serially
      ]

    , bgroup "wSerially"
      [ benchIO "toNullAp"       $ Ops.toNullAp linearCount       wSerially
      , benchIO "toNull"         $ Ops.toNull linearCount         wSerially
      , benchIO "toNull3"        $ Ops.toNull3 linearCount        wSerially
      , benchIO "toList"         $ Ops.toList linearCount         wSerially
    --  , benchIO "toListSome"     $ Ops.toListSome     wSerially
      , benchIO "filterAllOut"   $ Ops.filterAllOut linearCount   wSerially
      , benchIO "filterAllIn"    $ Ops.filterAllIn linearCount    wSerially
      , benchIO "filterSome"     $ Ops.filterSome linearCount     wSerially
      , benchIO "breakAfterSome" $ Ops.breakAfterSome linearCount wSerially
      ]

    , bgroup "zipSerially"
      [ benchIO "toNullAp"       $ Ops.toNullAp linearCount       zipSerially
      ]
    ]
  where
      defaultValue = 100000
      parseValue [] = (defaultValue, [])
      parseValue a@(x:xs) =
          case (readMaybe x :: Maybe Int) of
            Just value -> (value, xs)
            Nothing -> (defaultValue, a)
