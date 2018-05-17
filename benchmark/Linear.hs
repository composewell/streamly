-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

import Control.DeepSeq (NFData)
import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)
import qualified LinearOps as Ops

import Streamly
import Gauge

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.
benchIO :: (NFData b) => String -> (Ops.Stream m Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1000) >>= f . Ops.source

benchIOAppend :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIOAppend name f = bench name $ nfIO $ randomRIO (1,1000) >>= f

_benchId :: (NFData b) => String -> (Ops.Stream m Int -> Identity b) -> Benchmark
_benchId name f = bench name $ nf (runIdentity . f) (Ops.source 10)

main :: IO ()
main = do
  defaultMain
    [ bgroup "elimination"
      [ benchIO "toNull" Ops.toNull
      , benchIO "toList" Ops.toList
      , benchIO "fold" Ops.foldl
      , benchIO "last" Ops.last
      ]
    , bgroup "transformation"
      [ benchIO "scan" Ops.scan
      , benchIO "map" Ops.map
      , benchIO "mapM" Ops.mapM
      , benchIO "concat" Ops.concat
      ]
    , bgroup "filtering"
      [ benchIO "filter-even" Ops.filterEven
      , benchIO "filter-all-out" Ops.filterAllOut
      , benchIO "filter-all-in" Ops.filterAllIn
      , benchIO "take-all" Ops.takeAll
      , benchIO "takeWhile-true" Ops.takeWhileTrue
      , benchIO "drop-all" Ops.dropAll
      , benchIO "dropWhile-true" Ops.dropWhileTrue
      ]
    , benchIO "zip" Ops.zip
    , bgroup "append"
      [ benchIOAppend "serially"   $ Ops.append serially
      , benchIOAppend "wSerially"  $ Ops.append wSerially
      , benchIOAppend "aheadly"    $ Ops.append aheadly
      , benchIOAppend "asyncly"    $ Ops.append asyncly
      , benchIOAppend "wAsyncly"   $ Ops.append wAsyncly
      , benchIOAppend "parallely"  $ Ops.append parallely
      ]
    , bgroup "compose"
      [ benchIO "mapM" Ops.composeMapM
      , benchIO "map-with-all-in-filter" Ops.composeMapAllInFilter
      , benchIO "all-in-filters" Ops.composeAllInFilters
      , benchIO "all-out-filters" Ops.composeAllOutFilters
      ]
    , bgroup "compose-scaling"
      -- Scaling with same operation in sequence
      [ benchIO "1" $ Ops.composeScaling 1
      , benchIO "2" $ Ops.composeScaling 2
      , benchIO "3" $ Ops.composeScaling 3
      , benchIO "4" $ Ops.composeScaling 4
      ]
   ]
