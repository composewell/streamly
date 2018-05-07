-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

import Control.DeepSeq (NFData)
import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)
import qualified NestedOps as Ops
import Streamly
import Gauge

benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

_benchId :: (NFData b) => String -> (Int -> Identity b) -> Benchmark
_benchId name f = bench name $ nf (\g -> runIdentity (g 1))  f

main :: IO ()
main = do
  -- TBD Study scaling with 10, 100, 1000 loop iterations
  defaultMain
    [ bgroup "linear"
      [ benchIO "toNullLinear" Ops.toNullLinear
      , benchIO "toListLinear" Ops.toListLinear
      ]

    , bgroup "asStream"
      [ benchIO "append"         $ Ops.append         asStream
      , benchIO "toNull0"        $ Ops.toNull0        asStream
      , benchIO "toList0"        $ Ops.toList0        asStream
      , benchIO "toNull"         $ Ops.toNull         asStream
      , benchIO "toList"         $ Ops.toList         asStream
      , benchIO "toListSome"     $ Ops.toListSome     asStream
      , benchIO "filterAllOut"   $ Ops.filterAllOut   asStream
      , benchIO "filterAllIn"    $ Ops.filterAllIn    asStream
      , benchIO "filterSome"     $ Ops.filterSome     asStream
      , benchIO "breakAfterSome" $ Ops.breakAfterSome asStream
      ]

    , bgroup "asCostream"
      [ benchIO "append"         $ Ops.append         asCostream
      , benchIO "toNull0"        $ Ops.toNull0        asCostream
      , benchIO "toList0"        $ Ops.toList0        asCostream
      , benchIO "toNull"         $ Ops.toNull         asCostream
      , benchIO "toList"         $ Ops.toList         asCostream
      , benchIO "toListSome"     $ Ops.toListSome     asCostream
      , benchIO "filterAllOut"   $ Ops.filterAllOut   asCostream
      , benchIO "filterAllIn"    $ Ops.filterAllIn    asCostream
      , benchIO "filterSome"     $ Ops.filterSome     asCostream
      , benchIO "breakAfterSome" $ Ops.breakAfterSome asCostream
      ]

    , bgroup "CoasCoparAhead"
      [ benchIO "append"         $ Ops.append         asParAhead
      , benchIO "toNull0"        $ Ops.toNull0        asParAhead
      , benchIO "toList0"        $ Ops.toList0        asParAhead
      , benchIO "toNull"         $ Ops.toNull         asParAhead
      , benchIO "toList"         $ Ops.toList         asParAhead
      , benchIO "toListSome"     $ Ops.toListSome     asParAhead
      , benchIO "filterAllOut"   $ Ops.filterAllOut   asParAhead
      , benchIO "filterAllIn"    $ Ops.filterAllIn    asParAhead
      , benchIO "filterSome"     $ Ops.filterSome     asParAhead
      , benchIO "breakAfterSome" $ Ops.breakAfterSome asParAhead
      ]

    , bgroup "Parallely"
      [ benchIO "append"         $ Ops.append         asCoparAhead
      , benchIO "toNull0"        $ Ops.toNull0        asCoparAhead
      , benchIO "toList0"        $ Ops.toList0        asCoparAhead
      , benchIO "toNull"         $ Ops.toNull         asCoparAhead
      , benchIO "toList"         $ Ops.toList         asCoparAhead
      , benchIO "toListSome"     $ Ops.toListSome     asCoparAhead
      , benchIO "filterAllOut"   $ Ops.filterAllOut   asCoparAhead
      , benchIO "filterAllIn"    $ Ops.filterAllIn    asCoparAhead
      , benchIO "filterSome"     $ Ops.filterSome     asCoparAhead
      , benchIO "breakAfterSome" $ Ops.breakAfterSome asCoparAhead
      ]
    ]
