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

    , bgroup "streamly"
      [ benchIO "append"         $ Ops.append         streamly
      , benchIO "toNull0"        $ Ops.toNull0        streamly
      , benchIO "toList0"        $ Ops.toList0        streamly
      , benchIO "toNull"         $ Ops.toNull         streamly
      , benchIO "toList"         $ Ops.toList         streamly
      , benchIO "toListSome"     $ Ops.toListSome     streamly
      , benchIO "filterAllOut"   $ Ops.filterAllOut   streamly
      , benchIO "filterAllIn"    $ Ops.filterAllIn    streamly
      , benchIO "filterSome"     $ Ops.filterSome     streamly
      , benchIO "breakAfterSome" $ Ops.breakAfterSome streamly
      ]

    , bgroup "costreamly"
      [ benchIO "append"         $ Ops.append         costreamly
      , benchIO "toNull0"        $ Ops.toNull0        costreamly
      , benchIO "toList0"        $ Ops.toList0        costreamly
      , benchIO "toNull"         $ Ops.toNull         costreamly
      , benchIO "toList"         $ Ops.toList         costreamly
      , benchIO "toListSome"     $ Ops.toListSome     costreamly
      , benchIO "filterAllOut"   $ Ops.filterAllOut   costreamly
      , benchIO "filterAllIn"    $ Ops.filterAllIn    costreamly
      , benchIO "filterSome"     $ Ops.filterSome     costreamly
      , benchIO "breakAfterSome" $ Ops.breakAfterSome costreamly
      ]

    , bgroup "Coparallely"
      [ benchIO "append"         $ Ops.append         coparallely
      , benchIO "toNull0"        $ Ops.toNull0        coparallely
      , benchIO "toList0"        $ Ops.toList0        coparallely
      , benchIO "toNull"         $ Ops.toNull         coparallely
      , benchIO "toList"         $ Ops.toList         coparallely
      , benchIO "toListSome"     $ Ops.toListSome     coparallely
      , benchIO "filterAllOut"   $ Ops.filterAllOut   coparallely
      , benchIO "filterAllIn"    $ Ops.filterAllIn    coparallely
      , benchIO "filterSome"     $ Ops.filterSome     coparallely
      , benchIO "breakAfterSome" $ Ops.breakAfterSome coparallely
      ]

    , bgroup "Parallely"
      [ benchIO "append"         $ Ops.append         parallely
      , benchIO "toNull0"        $ Ops.toNull0        parallely
      , benchIO "toList0"        $ Ops.toList0        parallely
      , benchIO "toNull"         $ Ops.toNull         parallely
      , benchIO "toList"         $ Ops.toList         parallely
      , benchIO "toListSome"     $ Ops.toListSome     parallely
      , benchIO "filterAllOut"   $ Ops.filterAllOut   parallely
      , benchIO "filterAllIn"    $ Ops.filterAllIn    parallely
      , benchIO "filterSome"     $ Ops.filterSome     parallely
      , benchIO "breakAfterSome" $ Ops.breakAfterSome parallely
      ]
    ]
