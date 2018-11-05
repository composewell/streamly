-------------------------------------------------------------------------------
-- Investigate specific benchmarks more closely in isolation, possibly looking
-- at GHC generated code for optimizing specific problematic cases.
-------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
import qualified Streamly.Streams.StreamD as S
import Gauge

maxValue :: Int
maxValue = 100000

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => S.Stream m Int
sourceUnfoldrM = S.unfoldrM step 0
    where
    step cnt =
        if cnt > maxValue
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldrMN #-}
sourceUnfoldrMN :: Monad m => Int -> S.Stream m Int
sourceUnfoldrMN n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n
        then return Nothing
        else return (Just (cnt, cnt + 1))

-------------------------------------------------------------------------------
-- take-drop composition
-------------------------------------------------------------------------------

takeAllDropOne :: Monad m => S.Stream m Int -> S.Stream m Int
takeAllDropOne = S.drop 1 . S.take maxValue

-- Requires -fspec-constr-recursive=5 for better fused code
-- The number depends on how many times we compose it

{-# INLINE takeDrop #-}
takeDrop :: Monad m => S.Stream m Int -> m ()
takeDrop = S.runStream .
    takeAllDropOne . takeAllDropOne . takeAllDropOne . takeAllDropOne

-------------------------------------------------------------------------------
-- dropWhileFalse composition
-------------------------------------------------------------------------------

dropWhileFalse :: Monad m => S.Stream m Int -> S.Stream m Int
dropWhileFalse = S.dropWhile (> maxValue)

-- Requires -fspec-constr-recursive=5 for better fused code
-- The number depends on how many times we compose it

{-# INLINE dropWhileFalseX4 #-}
dropWhileFalseX4 :: Monad m => S.Stream m Int -> m ()
dropWhileFalseX4 = S.runStream
    . dropWhileFalse . dropWhileFalse . dropWhileFalse .  dropWhileFalse

-------------------------------------------------------------------------------
-- iteration
-------------------------------------------------------------------------------

{-# INLINE iterateSource #-}
iterateSource
    :: Monad m
    => (S.Stream m Int -> S.Stream m Int) -> Int -> Int -> S.Stream m Int
iterateSource g i n = f i (sourceUnfoldrMN n)
    where
        f (0 :: Int) m = g m
        f x m = g (f (x - 1) m)

-- Keep only the benchmark that is to be investiagted and comment out the rest.
-- We keep all of them enabled by default for testing the build.
main :: IO ()
main = do
    defaultMain [bench "take-drop" $ nfIO $ takeDrop sourceUnfoldrM]
    defaultMain [bench "dropWhileFalseX4" $
        nfIO $ dropWhileFalseX4 sourceUnfoldrM]
    defaultMain [bench "iterate-mapM" $
        nfIO $ S.runStream $ iterateSource (S.mapM return) 100000 10]
