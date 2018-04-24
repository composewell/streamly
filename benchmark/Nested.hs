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
    [ bgroup "Linear"
      [ benchIO "toNullLinear" Ops.toNullLinear
      , benchIO "toListLinear" Ops.toListLinear
      ]

    , bgroup "Serially"
      [ benchIO "append"         $ Ops.append         serially
      , benchIO "toNull0"        $ Ops.toNull0        serially
      , benchIO "toList0"        $ Ops.toList0        serially
      , benchIO "toNull"         $ Ops.toNull         serially
      , benchIO "toList"         $ Ops.toList         serially
      , benchIO "toListSome"     $ Ops.toListSome     serially
      , benchIO "filterAllOut"   $ Ops.filterAllOut   serially
      , benchIO "filterAllIn"    $ Ops.filterAllIn    serially
      , benchIO "filterSome"     $ Ops.filterSome     serially
      , benchIO "breakAfterSome" $ Ops.breakAfterSome serially
      ]

    , bgroup "Coserially"
      [ benchIO "append"         $ Ops.append         coserially
      , benchIO "toNull0"        $ Ops.toNull0        coserially
      , benchIO "toList0"        $ Ops.toList0        coserially
      , benchIO "toNull"         $ Ops.toNull         coserially
      , benchIO "toList"         $ Ops.toList         coserially
      , benchIO "toListSome"     $ Ops.toListSome     coserially
      , benchIO "filterAllOut"   $ Ops.filterAllOut   coserially
      , benchIO "filterAllIn"    $ Ops.filterAllIn    coserially
      , benchIO "filterSome"     $ Ops.filterSome     coserially
      , benchIO "breakAfterSome" $ Ops.breakAfterSome coserially
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
