{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}

import Streamly
import qualified Streamly.Prelude as S
import Test.Inspection

main :: IO ()
main = pure ()  -- If the test compiles, it passes

-------------------------------------------------------------------------------
-- #214 regression test: 'concatMap (replicate n)' should be specialized
-------------------------------------------------------------------------------

{-# INLINE concatMap1 #-}
concatMap1 :: MonadAsync m => SerialT m Int -> m ()
concatMap1 src = S.drain $ S.concatMap (S.replicate 3) src

{-# INLINE sourceUnfoldrMN #-}
sourceUnfoldrMN :: MonadAsync m => Int -> Int -> SerialT m Int
sourceUnfoldrMN m n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + m
        then return Nothing
        else return (Just (cnt, cnt + 1))

test214 :: IO ()
test214 = concatMap1 (sourceUnfoldrMN 1000000 5)

inspect $ hasNoTypeClasses 'test214
