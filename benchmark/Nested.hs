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

    , bgroup "serially"
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

    , bgroup "wSerially"
      [ benchIO "append"         $ Ops.append         wSerially
      , benchIO "toNull0"        $ Ops.toNull0        wSerially
      , benchIO "toList0"        $ Ops.toList0        wSerially
      , benchIO "toNull"         $ Ops.toNull         wSerially
      , benchIO "toList"         $ Ops.toList         wSerially
      , benchIO "toListSome"     $ Ops.toListSome     wSerially
      , benchIO "filterAllOut"   $ Ops.filterAllOut   wSerially
      , benchIO "filterAllIn"    $ Ops.filterAllIn    wSerially
      , benchIO "filterSome"     $ Ops.filterSome     wSerially
      , benchIO "breakAfterSome" $ Ops.breakAfterSome wSerially
      ]

    , bgroup "aheadly"
      [ benchIO "append"         $ Ops.append         aheadly
      , benchIO "toNull0"        $ Ops.toNull0        aheadly
      , benchIO "toList0"        $ Ops.toList0        aheadly
      , benchIO "toNull"         $ Ops.toNull         aheadly
      , benchIO "toList"         $ Ops.toList         aheadly
      , benchIO "toListSome"     $ Ops.toListSome     aheadly
      , benchIO "filterAllOut"   $ Ops.filterAllOut   aheadly
      , benchIO "filterAllIn"    $ Ops.filterAllIn    aheadly
      , benchIO "filterSome"     $ Ops.filterSome     aheadly
      , benchIO "breakAfterSome" $ Ops.breakAfterSome aheadly
      ]

    , bgroup "asyncly"
      [ benchIO "append"         $ Ops.append         asyncly
      , benchIO "toNull0"        $ Ops.toNull0        asyncly
      , benchIO "toList0"        $ Ops.toList0        asyncly
      , benchIO "toNull"         $ Ops.toNull         asyncly
      , benchIO "toList"         $ Ops.toList         asyncly
      , benchIO "toListSome"     $ Ops.toListSome     asyncly
      , benchIO "filterAllOut"   $ Ops.filterAllOut   asyncly
      , benchIO "filterAllIn"    $ Ops.filterAllIn    asyncly
      , benchIO "filterSome"     $ Ops.filterSome     asyncly
      , benchIO "breakAfterSome" $ Ops.breakAfterSome asyncly
      ]

    , bgroup "wAsyncly"
      [ benchIO "append"         $ Ops.append         wAsyncly
      , benchIO "toNull0"        $ Ops.toNull0        wAsyncly
      , benchIO "toList0"        $ Ops.toList0        wAsyncly
      , benchIO "toNull"         $ Ops.toNull         wAsyncly
      , benchIO "toList"         $ Ops.toList         wAsyncly
      , benchIO "toListSome"     $ Ops.toListSome     wAsyncly
      , benchIO "filterAllOut"   $ Ops.filterAllOut   wAsyncly
      , benchIO "filterAllIn"    $ Ops.filterAllIn    wAsyncly
      , benchIO "filterSome"     $ Ops.filterSome     wAsyncly
      , benchIO "breakAfterSome" $ Ops.breakAfterSome wAsyncly
      ]

    , bgroup "parallely"
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
