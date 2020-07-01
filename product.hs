{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData(..))
import Control.Exception (try)
import Data.Functor.Identity (Identity)
import GHC.Exception (ErrorCall)
import System.Random (randomRIO)
import Gauge

import qualified Streamly as S
import qualified Streamly.Prelude as S
import Prelude hiding (product)

{-# INLINE benchIOSink #-}
benchIOSink
    :: (S.IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM count start = S.unfoldrM step start
    where
    step cnt =
        if cnt > start + count
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE product #-}
product :: Monad m => S.SerialT m Int -> m Int
product = S.product

main = do
    defaultMainWith defaultConfig $
        [ benchIOSink 100000 "product" product ]
