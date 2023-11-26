-- |
-- Module      : Streamly.Benchmark.Data.Fold
-- Copyright   : (c) 2018 Composewell
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Main (main) where

import Control.DeepSeq (NFData(..))
import Control.Exception (SomeException, ErrorCall, try)
import Data.Char (ord)
import Data.Word (Word8)
import Streamly.Internal.Data.Unfold (Unfold)
import System.IO (Handle, hClose)
import System.Random (randomRIO)

import qualified Prelude
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold.Prelude as UF
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.StreamK as K

import Test.Tasty.Bench hiding (env)
import Prelude hiding (take, filter, zipWith, map, mapM, takeWhile)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Control.Monad.Catch (MonadCatch)
import Test.Inspection
#endif

{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

-- generate numbers up to the argument value
{-# INLINE source #-}
source :: Monad m => Int -> Unfold m Int Int
source n = UF.second n UF.enumerateFromToIntegral

-------------------------------------------------------------------------------
-- Benchmark helpers
-------------------------------------------------------------------------------

{-# INLINE drainGeneration #-}
drainGeneration :: Monad m => Unfold m a b -> a -> m ()
drainGeneration unf seed = UF.fold FL.drain unf seed

{-# INLINE drainTransformation #-}
drainTransformation ::
       Monad m => Unfold m a b -> (Unfold m a b -> Unfold m c d) -> c -> m ()
drainTransformation unf f seed = drainGeneration (f unf) seed

{-# INLINE drainTransformationDefault #-}
drainTransformationDefault ::
       Monad m => Int -> (Unfold m Int Int -> Unfold m c d) -> c -> m ()
drainTransformationDefault to =
    drainTransformation (UF.second to UF.enumerateFromToIntegral)

{-# INLINE drainProduct #-}
drainProduct ::
       Monad m
    => Unfold m a b
    -> Unfold m c d
    -> (Unfold m a b -> Unfold m c d -> Unfold m e f)
    -> e
    -> m ()
drainProduct unf1 unf2 f seed = drainGeneration (f unf1 unf2) seed

{-# INLINE drainProductDefault #-}
drainProductDefault ::
       Monad m
    => Int
    -> (Unfold m Int Int -> Unfold m Int Int -> Unfold m e f)
    -> e
    -> m ()
drainProductDefault to = drainProduct src src

    where

    src = UF.second to UF.enumerateFromToIntegral

-------------------------------------------------------------------------------
-- Operations on input
-------------------------------------------------------------------------------

{-# INLINE lmap #-}
lmap :: Monad m => Int -> Int -> m ()
lmap size start =
    drainTransformationDefault (size + start) (UF.lmap (+ 1)) start

{-# INLINE lmapM #-}
lmapM :: Monad m => Int -> Int -> m ()
lmapM size start =
    drainTransformationDefault (size + start) (UF.lmapM (return . (+) 1)) start

{-# INLINE both #-}
both :: Monad m => Int -> Int -> m ()
both size start =
    drainTransformationDefault (size + start) (UF.both start) undefined


{-# INLINE first #-}
first :: Monad m => Int -> Int -> m ()
first size start =
    drainTransformation
        (UF.take size UF.enumerateFromThenIntegral)
        (UF.first start)
        1

{-# INLINE second #-}
second :: Monad m => Int -> Int -> m ()
second size start =
    drainTransformation
        (UF.take size UF.enumerateFromThenIntegral)
        (UF.second 1)
        start

{-# INLINE discardFirst #-}
discardFirst :: Monad m => Int -> Int -> m ()
discardFirst size start =
    drainTransformationDefault (size + start) UF.discardFirst (start, start)

{-# INLINE discardSecond #-}
discardSecond :: Monad m => Int -> Int -> m ()
discardSecond size start =
    drainTransformationDefault (size + start) UF.discardSecond (start, start)

{-# INLINE swap #-}
swap :: Monad m => Int -> Int -> m ()
swap size start =
    drainTransformation
        (UF.take size UF.enumerateFromThenIntegral)
        UF.swap
        (1, start)

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

{-# INLINE fromStream #-}
fromStream :: Int -> Int -> IO ()
fromStream size start =
    drainGeneration UF.fromStream (S.replicate size start :: S.Stream IO Int)

-- XXX INVESTIGATE: Although the performance of this should be equivalant to
-- fromStream, this is considerably worse. More than 4x worse.
{-# INLINE fromStreamK #-}
fromStreamK :: Monad m => Int -> Int -> m ()
fromStreamK size start = drainGeneration UF.fromStreamK (K.replicate size start)

{-# INLINE fromStreamD #-}
fromStreamD :: Monad m => Int -> Int -> m ()
fromStreamD size start =
    drainGeneration UF.fromStreamD (D.replicate size start)

{-# INLINE _nilM #-}
_nilM :: Monad m => Int -> Int -> m ()
_nilM _ start = drainGeneration (UF.nilM return) start

{-# INLINE consM #-}
consM :: Monad m => Int -> Int -> m ()
consM size start =
    drainTransformationDefault (size + start) (UF.consM return) start

{-# INLINE _functionM #-}
_functionM :: Monad m => Int -> Int -> m ()
_functionM _ start = drainGeneration (UF.functionM return) start

{-# INLINE _function #-}
_function :: Monad m => Int -> Int -> m ()
_function _ start = drainGeneration (UF.function id) start

{-# INLINE _identity #-}
_identity :: Monad m => Int -> Int -> m ()
_identity _ start = drainGeneration UF.identity start

{-# INLINE _const #-}
_const :: Monad m => Int -> Int -> m ()
_const size start =
    drainGeneration (UF.take size (UF.fromEffect (return start))) undefined

{-# INLINE unfoldrM #-}
unfoldrM :: Monad m => Int -> Int -> m ()
unfoldrM size start = drainGeneration (UF.unfoldrM step) start

    where

    step i =
        return
            $ if i < start + size
              then Just (i, i + 1)
              else Nothing

{-# INLINE fromList #-}
fromList :: Monad m => Int -> Int -> m ()
fromList size start = drainGeneration UF.fromList [start .. start + size]

{-# INLINE fromListM #-}
fromListM :: Monad m => Int -> Int -> m ()
fromListM size start =
    drainGeneration UF.fromListM (Prelude.map return [start .. start + size])

{-# INLINE _fromSVar #-}
_fromSVar :: Int -> Int -> m ()
_fromSVar = undefined

{-# INLINE _fromProducer #-}
_fromProducer :: Int -> Int -> m ()
_fromProducer = undefined

{-# INLINE replicateM #-}
replicateM :: Monad m => Int -> Int -> m ()
replicateM size start = drainGeneration UF.replicateM (size, return start)

{-# INLINE repeatM #-}
repeatM :: Monad m => Int -> Int -> m ()
repeatM size start = drainGeneration (UF.take size UF.repeatM) (return start)

{-# INLINE iterateM #-}
iterateM :: Monad m => Int -> Int -> m ()
iterateM size start =
    drainGeneration (UF.take size (UF.iterateM return)) (return start)

{-# INLINE fromIndicesM #-}
fromIndicesM :: Monad m => Int -> Int -> m ()
fromIndicesM size start =
    drainGeneration (UF.take size (UF.fromIndicesM return)) start

{-# INLINE enumerateFromThenIntegral #-}
enumerateFromThenIntegral :: Monad m => Int -> Int -> m ()
enumerateFromThenIntegral size start =
    drainGeneration (UF.take size UF.enumerateFromThenIntegral) (start, 1)

{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: Monad m => Int -> Int -> m ()
enumerateFromToIntegral size start =
    drainGeneration
    ( UF.second
      (size + start)
      UF.enumerateFromToIntegral
    ) start

{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: Monad m => Int -> Int -> m ()
enumerateFromIntegral size start =
    drainGeneration (UF.take size UF.enumerateFromIntegral) start

{-# INLINE enumerateFromStepNum #-}
enumerateFromStepNum :: Monad m => Int -> Int -> m ()
enumerateFromStepNum size start =
    drainGeneration (UF.take size (UF.enumerateFromThenNum)) (start, 1)

{-# INLINE enumerateFromNum #-}
enumerateFromNum :: Monad m => Int -> Int -> m ()
enumerateFromNum size start = drainGeneration (UF.take size UF.enumerateFromNum) start

{-# INLINE enumerateFromToFractional #-}
enumerateFromToFractional :: Monad m => Int -> Int -> m ()
enumerateFromToFractional size start =
    let intToDouble x = (fromInteger (fromIntegral x)) :: Double
     in drainGeneration
            ( UF.second
              (intToDouble $ start + size)
              UF.enumerateFromToFractional
            )
            (intToDouble start)

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# INLINE postscan #-}
postscan :: Monad m => Int -> Int -> m ()
postscan size start =
    drainTransformationDefault (size + start) (UF.postscan FL.sum) start

{-# INLINE map #-}
map :: Monad m => Int -> Int -> m ()
map size start = drainTransformationDefault (size + start) (UF.map (+1)) start

{-# INLINE mapM #-}
mapM :: Monad m => Int -> Int -> m ()
mapM size start =
    drainTransformationDefault (size + start) (UF.mapM (return . (+) 1)) start

{-# INLINE mapM2 #-}
mapM2 :: Monad m => Int -> Int -> m ()
mapM2 size start =
    drainTransformationDefault
        size
        (UF.mapM2 (\a b -> return $ a + b))
        start

-------------------------------------------------------------------------------
-- Stream filtering
-------------------------------------------------------------------------------

{-# INLINE takeWhileM #-}
takeWhileM :: Monad m => Int -> Int -> m ()
takeWhileM size start =
    drainTransformationDefault
        size
        (UF.takeWhileM (\b -> return (b <= size + start)))
        start

{-# INLINE takeWhile #-}
takeWhile :: Monad m => Int -> Int -> m ()
takeWhile size start =
    drainTransformationDefault
        size
        (UF.takeWhile (\b -> b <= size + start))
        start

{-# INLINE take #-}
take :: Monad m => Int -> Int -> m ()
take size start = drainTransformationDefault (size + start) (UF.take size) start

{-# INLINE filter #-}
filter :: Monad m => Int -> Int -> m ()
filter size start =
    drainTransformationDefault (size + start) (UF.filter (\_ -> True)) start

{-# INLINE filterM #-}
filterM :: Monad m => Int -> Int -> m ()
filterM size start =
    drainTransformationDefault
        (size + start)
        (UF.filterM (\_ -> (return True)))
        start

{-# INLINE _dropOne #-}
_dropOne :: Monad m => Int -> Int -> m ()
_dropOne size start =
    drainTransformationDefault (size + start) (UF.drop 1) start

{-# INLINE dropAll #-}
dropAll :: Monad m => Int -> Int -> m ()
dropAll size start =
    drainTransformationDefault (size + start) (UF.drop (size + 1)) start

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: Monad m => Int -> Int -> m ()
dropWhileTrue size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhileM (\_ -> return True))
        start

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: Monad m => Int -> Int -> m ()
dropWhileFalse size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhileM (\_ -> return False))
        start

{-# INLINE dropWhileMTrue #-}
dropWhileMTrue :: Monad m => Int -> Int -> m ()
dropWhileMTrue size start =
    drainTransformationDefault
        size
        (UF.dropWhileM (\_ -> return True))
        start

{-# INLINE dropWhileMFalse #-}
dropWhileMFalse :: Monad m => Int -> Int -> m ()
dropWhileMFalse size start =
    drainTransformationDefault
        size
        (UF.dropWhileM (\_ -> return False))
        start

-------------------------------------------------------------------------------
-- Stream combination
-------------------------------------------------------------------------------

{-# INLINE zipWith #-}
zipWith :: Monad m => Int -> Int -> m ()
zipWith size start =
    drainProductDefault (size + start) (UF.zipWith (+)) start

{-# INLINE zipWithM #-}
zipWithM :: Monad m => Int -> Int -> m ()
zipWithM size start =
    drainProductDefault
        (size + start)
        (UF.zipWithM (\a b -> return $ a + b))
        start

{-# INLINE teeZipWith #-}
teeZipWith :: Monad m => Int -> Int -> m ()
teeZipWith size start =
    drainProductDefault (size + start) (UF.zipWith (+)) start

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

nthRoot :: Double -> Int -> Int
nthRoot n value = round (fromIntegral value**(1/n))

{-# INLINE toNullAp #-}
toNullAp :: Monad m => Int -> Int -> m ()
toNullAp value start =
    let end = start + nthRoot 2 value
        s = source end
    -- in UF.fold ((+) <$> s <*> s) FL.drain start
    in UF.fold FL.drain ((+) `fmap` s `UF.crossApply` s) start

{-# INLINE _apDiscardFst #-}
_apDiscardFst :: Int -> Int -> m ()
_apDiscardFst = undefined

{-# INLINE _apDiscardSnd #-}
_apDiscardSnd :: Int -> Int -> m ()
_apDiscardSnd = undefined

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

{-# INLINE concatMapM #-}
concatMapM :: Monad m => Int -> Int -> m ()
concatMapM value start =
    val `seq` drainGeneration (UF.concatMapM unfoldInGen unfoldOut) start

    where

    val = nthRoot 2 value
    unfoldInGen i = return (UF.second (i + val) UF.enumerateFromToIntegral)
    unfoldOut = UF.second (start + val) UF.enumerateFromToIntegral

{-# INLINE toNull #-}
toNull :: Monad m => Int -> Int -> m ()
toNull value start =
    let end = start + nthRoot 2 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
            return (x + y)
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
                UF.fromPure (x + y)
     in UF.fold FL.drain u start


{-# INLINE toNull3 #-}
toNull3 :: Monad m => Int -> Int -> m ()
toNull3 value start =
    let end = start + nthRoot 3 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
            z <- src
            return (x + y + z)
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
                UF.fromPure (x + y)
     in UF.fold FL.drain u start

{-# INLINE toList #-}
toList :: Monad m => Int -> Int -> m [Int]
toList value start = do
    let end = start + nthRoot 2 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
            return (x + y)
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
                UF.fromPure (x + y)
     in UF.fold FL.toList u start

{-# INLINE toListSome #-}
toListSome :: Monad m => Int -> Int -> m [Int]
toListSome value start = do
    let end = start + nthRoot 2 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
            return (x + y)
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
                UF.fromPure (x + y)
     in UF.fold FL.toList (UF.take 1000 u) start

{-# INLINE filterAllOut #-}
filterAllOut :: Monad m => Int -> Int -> m ()
filterAllOut value start = do
    let end = start + nthRoot 2 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
            let s = x + y
             in if s < 0
                then UF.fromPure s
                else UF.nilM (return . const ())
     in UF.fold FL.drain u start

{-# INLINE filterAllIn #-}
filterAllIn :: Monad m => Int -> Int -> m ()
filterAllIn value start = do
    let end = start + nthRoot 2 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
            let s = x + y
             in if s > 0
                then UF.fromPure s
                else UF.nilM (return . const ())
     in UF.fold FL.drain u start

{-# INLINE filterSome #-}
filterSome :: Monad m => Int -> Int -> m ()
filterSome value start = do
    let end = start + nthRoot 2 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
            let s = x + y
             in if s > 1100000
                then UF.fromPure s
                else UF.nilM (return . const ())
     in UF.fold FL.drain u start

{-# INLINE breakAfterSome #-}
breakAfterSome :: Int -> Int -> IO ()
breakAfterSome value start =
    let end = start + nthRoot 2 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
            let s = x + y
             in if s > 1100000
                then error "break"
                else UF.fromPure s
     in do
        (_ :: Either ErrorCall ()) <- try $ UF.fold FL.drain u start
        return ()

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

-- n * (n + 1) / 2 == linearCount
concatCount :: Int -> Int
concatCount linearCount =
    round (((1 + 8 * fromIntegral linearCount)**(1/2::Double) - 1) / 2)

{-# INLINE many #-}
many :: Monad m => Int -> Int -> m ()
many linearCount start = do
    let end = start + concatCount linearCount
    UF.fold FL.drain (UF.many (source end) (source end)) start

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Unfold"

o_1_space_transformation_input :: Int -> [Benchmark]
o_1_space_transformation_input size =
    [ bgroup
          "transformation/input"
          [ benchIO "lmap" $ lmap size
          , benchIO "lmapM" $ lmapM size
          , benchIO "both" $ both size
          , benchIO "first" $ first size
          , benchIO "second" $ second size
          , benchIO "discardFirst" $ discardFirst size
          , benchIO "discardSecond" $ discardSecond size
          , benchIO "swap" $ swap size
          ]
    ]

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation size =
    [ bgroup
          "generation"
          [ benchIO "fromStream" $ fromStream size
          , benchIO "fromStreamK" $ fromStreamK size
          , benchIO "fromStreamD" $ fromStreamD size
          -- Very small benchmarks, reporting in ns
          -- , benchIO "nilM" $ nilM size
          , benchIO "consM" $ consM size
          -- , benchIO "functionM" $ functionM size
          -- , benchIO "function" $ function size
          -- , benchIO "identity" $ identity size
          -- , benchIO "const" $ const size
          , benchIO "unfoldrM" $ unfoldrM size
          , benchIO "fromList" $ fromList size
          , benchIO "fromListM" $ fromListM size
          -- Unimplemented
          -- , benchIO "fromSVar" $ fromSVar size
          -- , benchIO "fromProducer" $ fromProducer size
          , benchIO "replicateM" $ replicateM size
          , benchIO "repeatM" $ repeatM size
          , benchIO "iterateM" $ iterateM size
          , benchIO "fromIndicesM" $ fromIndicesM size
          , benchIO "enumerateFromThenIntegral" $ enumerateFromThenIntegral size
          , benchIO "enumerateFromToIntegral" $ enumerateFromToIntegral size
          , benchIO "enumerateFromIntegral" $ enumerateFromIntegral size
          , benchIO "enumerateFromStepNum" $ enumerateFromStepNum size
          , benchIO "enumerateFromNum" $ enumerateFromNum size
          , benchIO "enumerateFromToFractional" $ enumerateFromToFractional size
          ]
    ]

o_1_space_transformation :: Int -> [Benchmark]
o_1_space_transformation size =
    [ bgroup
          "transformation"
          [ benchIO "map" $ map size
          , benchIO "mapM" $ mapM size
          , benchIO "mapM2" $ mapM2 size
          , benchIO "postscan" $ postscan size
          ]
    ]

o_1_space_filtering :: Int -> [Benchmark]
o_1_space_filtering size =
    [ bgroup
          "filtering"
          [ benchIO "takeWhileM" $ takeWhileM size
          , benchIO "takeWhile" $ takeWhile size
          , benchIO "take" $ take size
          , benchIO "filter" $ filter size
          , benchIO "filterM" $ filterM size
          -- Very small benchmark, reporting in ns
          -- , benchIO "dropOne" $ dropOne size
          , benchIO "dropAll" $ dropAll size
          , benchIO "dropWhileTrue" $ dropWhileTrue size
          , benchIO "dropWhileFalse" $ dropWhileFalse size
          , benchIO "dropWhileMTrue" $ dropWhileMTrue size
          , benchIO "dropWhileMFalse" $ dropWhileMFalse size
          ]
    ]

o_1_space_zip :: Int -> [Benchmark]
o_1_space_zip size =
    [ bgroup
          "zip"
          [ benchIO "zipWithM" $ zipWithM size
          , benchIO "zipWith" $ zipWith size
          , benchIO "teeZipWith" $ teeZipWith size
          ]
    ]

lf :: Word8
lf = fromIntegral (ord '\n')

-- | Split on line feed.
foldManySepBy :: Handle -> IO Int
foldManySepBy =
    let u = UF.foldMany (FL.takeEndBy_ (== lf) FL.drain) FH.reader
     in UF.fold FL.length u

o_1_space_nested :: BenchEnv -> Int -> [Benchmark]
o_1_space_nested env size =
    [ bgroup
          "nested"
          [ benchIO "(<*>) (sqrt n x sqrt n)" $ toNullAp size
          -- Unimplemented
          -- , benchIO "apDiscardFst" $ apDiscardFst size
          -- , benchIO "apDiscardSnd" $ apDiscardSnd size

          , benchIO "concatMapM (sqrt n x sqrt n)" $ concatMapM size
          , benchIO "(>>=) (sqrt n x sqrt n)" $ toNull size
          , benchIO "(>>=) (cubert n x cubert n x cubert n)" $ toNull3 size
          , benchIO "breakAfterSome" $ breakAfterSome size
          , benchIO "filterAllOut" $ filterAllOut size
          , benchIO "filterAllIn" $ filterAllIn size
          , benchIO "filterSome" $ filterSome size

          , benchIO "many" $ many size
          , mkBench "foldMany (Fold.takeEndBy_ (== lf) Fold.drain)" env
            $ \inh _ -> foldManySepBy inh
          ]
    ]

o_n_space_nested :: Int -> [Benchmark]
o_n_space_nested size =
    [ bgroup
          "nested"
          [ benchIO "toList" $ toList size
          , benchIO "toListSome" $ toListSome size
          ]
    ]

-------------------------------------------------------------------------------
-- Unfold Exception Benchmarks
-------------------------------------------------------------------------------
-- | Send the file contents to /dev/null with exception handling
readWriteOnExceptionUnfold :: Handle -> Handle -> IO ()
readWriteOnExceptionUnfold inh devNull =
    let readEx = UF.onException (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'readWriteOnExceptionUnfold
-- inspect $ 'readWriteOnExceptionUnfold `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null with exception handling
readWriteHandleExceptionUnfold :: Handle -> Handle -> IO ()
readWriteHandleExceptionUnfold inh devNull =
    let handler (_e :: SomeException) = hClose inh >> return 10
        readEx = UF.handle (UF.functionM handler) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
-- hasNoTypeClasses started failing in GHC 9.4.4
-- inspect $ hasNoTypeClasses 'readWriteHandleExceptionUnfold
-- inspect $ 'readWriteHandleExceptionUnfold `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null with exception handling
readWriteFinally_Unfold :: Handle -> Handle -> IO ()
readWriteFinally_Unfold inh devNull =
    let readEx = UF.finally_ (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'readWriteFinally_Unfold [''MonadCatch]
#else
inspect $ hasNoTypeClasses 'readWriteFinally_Unfold
#endif
-- inspect $ 'readWriteFinallyUnfold `hasNoType` ''Step
#endif

readWriteFinallyUnfold :: Handle -> Handle -> IO ()
readWriteFinallyUnfold inh devNull =
    let readEx = UF.finally (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

-- | Send the file contents to /dev/null with exception handling
readWriteBracket_Unfold :: Handle -> Handle -> IO ()
readWriteBracket_Unfold inh devNull =
    let readEx = UF.bracket_ return (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'readWriteBracket_Unfold [''MonadCatch]
#else
inspect $ hasNoTypeClasses 'readWriteBracket_Unfold
#endif
-- inspect $ 'readWriteBracketUnfold `hasNoType` ''Step
#endif

readWriteBracketUnfold :: Handle -> Handle -> IO ()
readWriteBracketUnfold inh devNull =
    let readEx = UF.bracket return (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

o_1_space_copy_read_exceptions :: BenchEnv -> [Benchmark]
o_1_space_copy_read_exceptions env =
    [ bgroup "exceptions"
       [ mkBenchSmall "UF.onException" env $ \inh _ ->
           readWriteOnExceptionUnfold inh (nullH env)
       , mkBenchSmall "UF.handle" env $ \inh _ ->
           readWriteHandleExceptionUnfold inh (nullH env)
       , mkBenchSmall "UF.finally_" env $ \inh _ ->
           readWriteFinally_Unfold inh (nullH env)
       , mkBenchSmall "UF.finally" env $ \inh _ ->
           readWriteFinallyUnfold inh (nullH env)
       , mkBenchSmall "UF.bracket_" env $ \inh _ ->
           readWriteBracket_Unfold inh (nullH env)
       , mkBenchSmall "UF.bracket" env $ \inh _ ->
           readWriteBracketUnfold inh (nullH env)
        ]
    ]


-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
    env <- mkHandleBenchEnv
    runWithCLIOpts defaultStreamSize (allBenchmarks env)

    where

    allBenchmarks env size =
        [ bgroup (o_1_space_prefix moduleName)
            $ Prelude.concat
                  [ o_1_space_transformation_input size
                  , o_1_space_generation size
                  , o_1_space_transformation size
                  , o_1_space_filtering size
                  , o_1_space_zip size
                  , o_1_space_nested env size
                  , o_1_space_copy_read_exceptions env
                  ]
        , bgroup (o_n_space_prefix moduleName)
            $ Prelude.concat [o_n_space_nested size]
        ]
