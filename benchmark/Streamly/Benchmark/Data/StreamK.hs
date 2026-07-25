-- |
-- Module      : Streamly.Benchmark.Data.StreamK
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Main (main) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.Maybe (isJust)
import Streamly.Internal.Data.Stream (Stream, Step)
import Streamly.Internal.Data.StreamK (StreamK)
import System.Random (randomRIO)
import Test.Tasty.Bench (bench, nf, nfIO, bgroup, Benchmark)

import qualified Data.List as List
import qualified Prelude as P
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.StreamK as StreamK
import Streamly.Internal.Data.SVar.Type (State)

import GHC.Classes (IP)
import GHC.Stack (SrcLoc, CallStack)
import Prelude hiding
    ( Foldable(..), mapM_, last, map, concatMap, zipWith, init
    , iterate, repeat, replicate
    )
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

{-# INLINE withDrain #-}
withDrain :: (Int -> StreamK IO a) -> Int -> IO ()
withDrain f n = StreamK.drain (f n)

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: Int -> Int -> StreamK m Int
sourceUnfoldr streamLen n = StreamK.unfoldr step n
    where
    step cnt =
        if cnt > n + streamLen
        then Nothing
        else Just (cnt, cnt + 1)

{-# ANN unfoldr (PermitPatternMatches []) #-}
{-# ANN unfoldr (PermitConstructions []) #-}
{-# ANN unfoldr (PermitTypeClasses []) #-}
{-# NOINLINE unfoldr #-}
unfoldr :: Int -> Int -> IO ()
unfoldr streamLen = withDrain (sourceUnfoldr streamLen)

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> StreamK m Int
sourceUnfoldrM streamLen n = StreamK.unfoldrMWith StreamK.consM step n
    where
    step cnt =
        if cnt > n + streamLen
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# ANN unfoldrMWith (PermitPatternMatches []) #-}
{-# ANN unfoldrMWith (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN unfoldrMWith (PermitTypeClasses []) #-}
{-# NOINLINE unfoldrMWith #-}
unfoldrMWith :: Int -> Int -> IO ()
unfoldrMWith streamLen = withDrain (sourceUnfoldrM streamLen)

{-# INLINE withStream #-}
withStream :: Int -> (StreamK IO Int -> IO b) -> Int -> IO b
withStream value f = f . sourceUnfoldrM value

{-# ANN repeat (PermitPatternMatches [''State]) #-}
{-# ANN repeat (PermitConstructions [''(),''State,''Maybe,''Bool]) #-}
{-# ANN repeat (PermitTypeClasses []) #-}
{-# NOINLINE repeat #-}
repeat :: Int -> Int -> IO ()
repeat streamLen = withDrain $ StreamK.take streamLen . StreamK.repeat

{-# ANN repeatM (PermitPatternMatches [''State]) #-}
{-# ANN repeatM (PermitConstructions [''(),''State,''Maybe,''Bool]) #-}
{-# ANN repeatM (PermitTypeClasses [''Monad]) #-}
{-# NOINLINE repeatM #-}
repeatM :: Int -> Int -> IO ()
repeatM streamLen =
    withDrain $ StreamK.take streamLen . StreamK.repeatM . return

{-# ANN replicate (PermitPatternMatches []) #-}
{-# ANN replicate (PermitConstructions [''(),''State,''Maybe,''Bool]) #-}
{-# ANN replicate (PermitTypeClasses []) #-}
{-# NOINLINE replicate #-}
replicate :: Int -> Int -> IO ()
replicate streamLen = withDrain (StreamK.replicate streamLen)

{-# ANN replicateMWith (PermitPatternMatches []) #-}
{-# ANN replicateMWith (PermitConstructions [''(),''State,''Maybe,''Bool]) #-}
{-# ANN replicateMWith (PermitTypeClasses []) #-}
{-# NOINLINE replicateMWith #-}
replicateMWith :: Int -> Int -> IO ()
replicateMWith streamLen =
    withDrain $ StreamK.replicateMWith StreamK.consM streamLen . return

{-# ANN iterate (PermitPatternMatches [''State]) #-}
{-# ANN iterate (PermitConstructions [''Int,''(),''State,''Maybe,''Bool]) #-}
{-# ANN iterate (PermitTypeClasses []) #-}
{-# NOINLINE iterate #-}
iterate :: Int -> Int -> IO ()
iterate streamLen = withDrain $ StreamK.take streamLen . StreamK.iterate (+1)

{-# ANN iterateM (PermitPatternMatches [''State,''Int]) #-}
{-# ANN iterateM (PermitConstructions [''(),''State,''Maybe,''Bool,''Int]) #-}
{-# ANN iterateM (PermitTypeClasses []) #-}
{-# NOINLINE iterateM #-}
iterateM :: Int -> Int -> IO ()
iterateM streamLen =
    withDrain
        $ StreamK.take streamLen . StreamK.iterateM (return . (+1)) . return

{-# ANN fromFoldable (PermitPatternMatches []) #-}
{-# ANN fromFoldable (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN fromFoldable (PermitTypeClasses []) #-}
{-# NOINLINE fromFoldable #-}
fromFoldable :: Int -> Int -> IO ()
fromFoldable streamLen =
    withDrain $ \n -> StreamK.fromFoldable [n..n+streamLen]

{- HLINT ignore "Fuse foldr/fmap" -}
{-# ANN fromFoldableM (PermitPatternMatches []) #-}
{-# ANN fromFoldableM (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN fromFoldableM (PermitTypeClasses []) #-}
{-# NOINLINE fromFoldableM #-}
fromFoldableM :: Int -> Int -> IO ()
fromFoldableM streamLen =
    withDrain $ \n ->
    List.foldr StreamK.consM StreamK.nil (P.fmap return [n..n+streamLen])

{-# INLINE concatMapFoldableSerial #-}
concatMapFoldableSerial :: Int -> Int -> StreamK m Int
concatMapFoldableSerial streamLen n =
    P.foldr (StreamK.append . StreamK.fromPure) StreamK.nil [n..n+streamLen]

{-# INLINE concatMapFoldableSerialM #-}
concatMapFoldableSerialM :: Monad m => Int -> Int -> StreamK m Int
concatMapFoldableSerialM streamLen n =
    P.foldr
        (StreamK.append . StreamK.fromEffect . return)
        StreamK.nil [n..n+streamLen]

{-# ANN append_Foldable (PermitPatternMatches []) #-}
{-# ANN append_Foldable (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN append_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE append_Foldable #-}
append_Foldable :: Int -> Int -> IO ()
append_Foldable streamLen = withDrain (concatMapFoldableSerial streamLen)

{-# ANN append_FoldableM (PermitPatternMatches []) #-}
{-# ANN append_FoldableM (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN append_FoldableM (PermitTypeClasses []) #-}
{-# NOINLINE append_FoldableM #-}
append_FoldableM :: Int -> Int -> IO ()
append_FoldableM streamLen = withDrain (concatMapFoldableSerialM streamLen)

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# ANN mapM_ (PermitPatternMatches []) #-}
{-# ANN mapM_ (PermitConstructions [''(),''State,''Maybe,''Bool,''Int]) #-}
{-# ANN mapM_ (PermitTypeClasses []) #-}
{-# NOINLINE mapM_ #-}
mapM_ :: Int -> Int -> IO ()
mapM_ streamLen = withStream streamLen (StreamK.mapM_ (\_ -> return ()))

{-# ANN uncons (PermitPatternMatches [''Maybe,''(,)]) #-}
{-# ANN uncons (PermitConstructions [''State,''Maybe,''Bool,''(,),''Int]) #-}
{-# ANN uncons (PermitTypeClasses []) #-}
{-# NOINLINE uncons #-}
uncons :: Int -> Int -> IO ()
uncons streamLen = withStream streamLen go
    where
    go s = do
        r <- StreamK.uncons s
        case r of
            Nothing -> return ()
            Just (_, t) -> go t

{-# ANN init (PermitPatternMatches [''Maybe,''(,)]) #-}
{-# ANN init (PermitConstructions [''(),''State,''Maybe,''Bool,''Int,''(,)]) #-}
{-# ANN init (PermitTypeClasses []) #-}
{-# NOINLINE init #-}
init :: Int -> Int -> IO ()
init streamLen = withStream streamLen go
    where
    go s = do
        t <- StreamK.init s
        P.mapM_ StreamK.drain t

{-# ANN tail_Iterated (PermitPatternMatches [''Maybe]) #-}
{-# ANN tail_Iterated (PermitConstructions [''State,''Maybe,''Bool,''Int]) #-}
{-# ANN tail_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE tail_Iterated #-}
tail_Iterated :: Int -> Int -> IO ()
tail_Iterated streamLen = withStream streamLen go
    where go s = StreamK.tail s >>= P.mapM_ go

{-# ANN tail_Null_Iterated (PermitPatternMatches [''Bool,''Maybe]) #-}
{-# ANN tail_Null_Iterated (PermitConstructions
    [''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN tail_Null_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE tail_Null_Iterated #-}
tail_Null_Iterated :: Int -> Int -> IO ()
tail_Null_Iterated streamLen = withStream streamLen go
    where
    go s = do
        r <- StreamK.null s
        when (not r) $ StreamK.tail s >>= P.mapM_ go

{-# ANN tail_Head_Iterated (PermitPatternMatches [''Maybe]) #-}
{-# ANN tail_Head_Iterated (PermitConstructions
    [''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN tail_Head_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE tail_Head_Iterated #-}
tail_Head_Iterated :: Int -> Int -> IO ()
tail_Head_Iterated streamLen = withStream streamLen go
    where
    go s = do
        h <- StreamK.head s
        when (isJust h) $ StreamK.tail s >>= P.mapM_ go

{-# ANN toList (PermitPatternMatches []) #-}
{-# ANN toList (PermitConstructions [''[],''State,''Maybe,''Bool,''Int]) #-}
{-# ANN toList (PermitTypeClasses []) #-}
{-# NOINLINE toList #-}
toList :: Int -> Int -> IO [Int]
toList streamLen = withStream streamLen StreamK.toList

{-# ANN foldl' (PermitPatternMatches [''Int]) #-}
{-# ANN foldl' (PermitConstructions [''State,''Maybe,''Bool,''Int]) #-}
{-# ANN foldl' (PermitTypeClasses []) #-}
{-# NOINLINE foldl' #-}
foldl' :: Int -> Int -> IO Int
foldl' streamLen = withStream streamLen (StreamK.foldl' (+) 0)

{-# ANN foldlM' (PermitPatternMatches [''Int]) #-}
{-# ANN foldlM' (PermitConstructions [''State,''Maybe,''Bool,''Int]) #-}
{-# ANN foldlM' (PermitTypeClasses []) #-}
{-# NOINLINE foldlM' #-}
foldlM' :: Int -> Int -> IO Int
foldlM' streamLen =
    withStream streamLen (StreamK.foldlM' (\b a -> return (b + a)) (return 0))

{-# ANN last (PermitPatternMatches []) #-}
{-# ANN last (PermitConstructions [''Maybe,''State,''Bool,''Int]) #-}
{-# ANN last (PermitTypeClasses []) #-}
{-# NOINLINE last #-}
last :: Int -> Int -> IO (Maybe Int)
last streamLen = withStream streamLen StreamK.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE composeN #-}
composeN
    :: Monad m
    => Int -> (StreamK m Int -> StreamK m Int) -> StreamK m Int -> m ()
composeN n f =
    case n of
        1 -> StreamK.drain . f
        2 -> StreamK.drain . f . f
        3 -> StreamK.drain . f . f . f
        4 -> StreamK.drain . f . f . f . f
        _ -> undefined

{-# INLINE scanl' #-}
scanl' :: Int -> Int -> Int -> IO ()
scanl' n streamLen = withStream streamLen (composeN n (StreamK.scanl' (+) 0))

{-# INLINE map #-}
map :: Int -> Int -> Int -> IO ()
map n streamLen = withStream streamLen (composeN n (StreamK.map (+ 1)))

{-# INLINE fmapN #-}
fmapN :: Int -> Int -> Int -> IO ()
fmapN n streamLen = withStream streamLen (composeN n (P.fmap (+ 1)))

{-# INLINE mapMWith #-}
mapMWith :: Int -> Int -> Int -> IO ()
mapMWith n streamLen =
    withStream streamLen (composeN n (StreamK.mapMWith StreamK.consM return))

{-# INLINE mapMSerial #-}
mapMSerial :: Int -> Int -> Int -> IO ()
mapMSerial n streamLen =
    withStream streamLen (composeN n (StreamK.mapMSerial return))

{-# INLINE filter_Even #-}
filter_Even :: Int -> Int -> Int -> IO ()
filter_Even n streamLen =
    withStream streamLen (composeN n (StreamK.filter even))

{-# INLINE filter_AllOut #-}
filter_AllOut :: Int -> Int -> Int -> IO ()
filter_AllOut n streamLen =
    withStream streamLen (composeN n (StreamK.filter (> streamLen)))

{-# INLINE filter_AllIn #-}
filter_AllIn :: Int -> Int -> Int -> IO ()
filter_AllIn n streamLen =
    withStream streamLen (composeN n (StreamK.filter (<= streamLen)))

{-# INLINE _takeOne #-}
_takeOne :: Monad m => Int -> StreamK m Int -> m ()
_takeOne n = composeN n $ StreamK.take 1

{-# INLINE take_All #-}
take_All :: Int -> Int -> Int -> IO ()
take_All n streamLen =
    withStream streamLen (composeN n (StreamK.take streamLen))

{-# INLINE takeWhile_True #-}
takeWhile_True :: Int -> Int -> Int -> IO ()
takeWhile_True n streamLen =
    withStream streamLen (composeN n (StreamK.takeWhile (<= streamLen)))

{-# INLINE drop_One #-}
drop_One :: Int -> Int -> Int -> IO ()
drop_One n streamLen = withStream streamLen (composeN n (StreamK.drop 1))

{-# INLINE drop_All #-}
drop_All :: Int -> Int -> Int -> IO ()
drop_All n streamLen =
    withStream streamLen (composeN n (StreamK.drop streamLen))

{-# INLINE dropWhile_True #-}
dropWhile_True :: Int -> Int -> Int -> IO ()
dropWhile_True n streamLen =
    withStream streamLen (composeN n (StreamK.dropWhile (<= streamLen)))

{-# INLINE dropWhile_False #-}
dropWhile_False :: Int -> Int -> Int -> IO ()
dropWhile_False n streamLen =
    withStream streamLen (composeN n (StreamK.dropWhile (<= 1)))

{-# INLINE foldrS #-}
foldrS :: Int -> Int -> Int -> IO ()
foldrS n streamLen =
    withStream streamLen (composeN n (StreamK.foldrS StreamK.cons StreamK.nil))

{-# INLINE foldlS #-}
foldlS :: Int -> Int -> Int -> IO ()
foldlS n streamLen =
    withStream streamLen
        (composeN n (StreamK.foldlS (flip StreamK.cons) StreamK.nil))

{-# INLINE intersperse #-}
intersperse :: Int -> Int -> Int -> Int -> IO ()
intersperse bound n streamLen =
    withStream streamLen (composeN n (StreamK.intersperse bound))

{-# INLINE intersperse_Pure #-}
intersperse_Pure :: Int -> Int -> Int -> Int -> IO ()
intersperse_Pure bound n streamLen =
    composeN n (StreamK.intersperse bound) . sourceUnfoldr streamLen

-------------------------------------------------------------------------------
-- Composed transformation wrappers
-------------------------------------------------------------------------------

{-# ANN scanl'_x1 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN scanl'_x1 (PermitConstructions [''(),''State,''Maybe,''Bool,''Int]) #-}
{-# ANN scanl'_x1 (PermitTypeClasses []) #-}
{-# NOINLINE scanl'_x1 #-}
scanl'_x1 :: Int -> Int -> IO ()
scanl'_x1 streamLen = scanl' 1 streamLen

{-# ANN scanl'_x4 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN scanl'_x4 (PermitConstructions [''(),''State,''Maybe,''Bool,''Int]) #-}
{-# ANN scanl'_x4 (PermitTypeClasses []) #-}
{-# NOINLINE scanl'_x4 #-}
scanl'_x4 :: Int -> Int -> IO ()
scanl'_x4 streamLen = scanl' 4 streamLen

{-# ANN map_x1 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN map_x1 (PermitConstructions [''(),''State,''Maybe,''Bool,''Int]) #-}
{-# ANN map_x1 (PermitTypeClasses []) #-}
{-# NOINLINE map_x1 #-}
map_x1 :: Int -> Int -> IO ()
map_x1 streamLen = map 1 streamLen

{-# ANN map_x4 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN map_x4 (PermitConstructions [''(),''State,''Maybe,''Bool,''Int]) #-}
{-# ANN map_x4 (PermitTypeClasses []) #-}
{-# NOINLINE map_x4 #-}
map_x4 :: Int -> Int -> IO ()
map_x4 streamLen = map 4 streamLen

{-# ANN fmap_x1 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN fmap_x1 (PermitConstructions [''Int,''State,''Maybe,''(),''Bool]) #-}
{-# ANN fmap_x1 (PermitTypeClasses []) #-}
{-# NOINLINE fmap_x1 #-}
fmap_x1 :: Int -> Int -> IO ()
fmap_x1 streamLen = fmapN 1 streamLen

{-# ANN fmap_x4 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN fmap_x4 (PermitConstructions [''Int,''State,''Maybe,''(),''Bool]) #-}
{-# ANN fmap_x4 (PermitTypeClasses []) #-}
{-# NOINLINE fmap_x4 #-}
fmap_x4 :: Int -> Int -> IO ()
fmap_x4 streamLen = fmapN 4 streamLen

{-# ANN mapMWith_x1 (PermitPatternMatches [''State]) #-}
{-# ANN mapMWith_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN mapMWith_x1 (PermitTypeClasses []) #-}
{-# NOINLINE mapMWith_x1 #-}
mapMWith_x1 :: Int -> Int -> IO ()
mapMWith_x1 streamLen = mapMWith 1 streamLen

{-# ANN mapMWith_x4 (PermitPatternMatches [''State]) #-}
{-# ANN mapMWith_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN mapMWith_x4 (PermitTypeClasses []) #-}
{-# NOINLINE mapMWith_x4 #-}
mapMWith_x4 :: Int -> Int -> IO ()
mapMWith_x4 streamLen = mapMWith 4 streamLen

{-# ANN mapMSerial_x1 (PermitPatternMatches [''State]) #-}
{-# ANN mapMSerial_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN mapMSerial_x1 (PermitTypeClasses []) #-}
{-# NOINLINE mapMSerial_x1 #-}
mapMSerial_x1 :: Int -> Int -> IO ()
mapMSerial_x1 streamLen = mapMSerial 1 streamLen

{-# ANN mapMSerial_x4 (PermitPatternMatches [''State]) #-}
{-# ANN mapMSerial_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN mapMSerial_x4 (PermitTypeClasses []) #-}
{-# NOINLINE mapMSerial_x4 #-}
mapMSerial_x4 :: Int -> Int -> IO ()
mapMSerial_x4 streamLen = mapMSerial 4 streamLen

{-# ANN filter_Even_x1 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_Even_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_Even_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filter_Even_x1 #-}
filter_Even_x1 :: Int -> Int -> IO ()
filter_Even_x1 streamLen = filter_Even 1 streamLen

{-# ANN filter_Even_x4 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_Even_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_Even_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filter_Even_x4 #-}
filter_Even_x4 :: Int -> Int -> IO ()
filter_Even_x4 streamLen = filter_Even 4 streamLen

{-# ANN filter_AllOut_x1 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_AllOut_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_AllOut_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filter_AllOut_x1 #-}
filter_AllOut_x1 :: Int -> Int -> IO ()
filter_AllOut_x1 streamLen = filter_AllOut 1 streamLen

{-# ANN filter_AllOut_x4 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_AllOut_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_AllOut_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filter_AllOut_x4 #-}
filter_AllOut_x4 :: Int -> Int -> IO ()
filter_AllOut_x4 streamLen = filter_AllOut 4 streamLen

{-# ANN filter_AllIn_x1 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_AllIn_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_AllIn_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filter_AllIn_x1 #-}
filter_AllIn_x1 :: Int -> Int -> IO ()
filter_AllIn_x1 streamLen = filter_AllIn 1 streamLen

{-# ANN filter_AllIn_x4 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_AllIn_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_AllIn_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filter_AllIn_x4 #-}
filter_AllIn_x4 :: Int -> Int -> IO ()
filter_AllIn_x4 streamLen = filter_AllIn 4 streamLen

{-# ANN take_All_x1 (PermitPatternMatches [''State]) #-}
{-# ANN take_All_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN take_All_x1 (PermitTypeClasses []) #-}
{-# NOINLINE take_All_x1 #-}
take_All_x1 :: Int -> Int -> IO ()
take_All_x1 streamLen = take_All 1 streamLen

{-# ANN take_All_x4 (PermitPatternMatches [''State]) #-}
{-# ANN take_All_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN take_All_x4 (PermitTypeClasses []) #-}
{-# NOINLINE take_All_x4 #-}
take_All_x4 :: Int -> Int -> IO ()
take_All_x4 streamLen = take_All 4 streamLen

{-# ANN takeWhile_True_x1 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN takeWhile_True_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN takeWhile_True_x1 (PermitTypeClasses []) #-}
{-# NOINLINE takeWhile_True_x1 #-}
takeWhile_True_x1 :: Int -> Int -> IO ()
takeWhile_True_x1 streamLen = takeWhile_True 1 streamLen

{-# ANN takeWhile_True_x4 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN takeWhile_True_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN takeWhile_True_x4 (PermitTypeClasses []) #-}
{-# NOINLINE takeWhile_True_x4 #-}
takeWhile_True_x4 :: Int -> Int -> IO ()
takeWhile_True_x4 streamLen = takeWhile_True 4 streamLen

{-# ANN drop_One_x1 (PermitPatternMatches [''State]) #-}
{-# ANN drop_One_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN drop_One_x1 (PermitTypeClasses []) #-}
{-# NOINLINE drop_One_x1 #-}
drop_One_x1 :: Int -> Int -> IO ()
drop_One_x1 streamLen = drop_One 1 streamLen

{-# ANN drop_One_x4 (PermitPatternMatches [''State]) #-}
{-# ANN drop_One_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN drop_One_x4 (PermitTypeClasses []) #-}
{-# NOINLINE drop_One_x4 #-}
drop_One_x4 :: Int -> Int -> IO ()
drop_One_x4 streamLen = drop_One 4 streamLen

{-# ANN drop_All_x1 (PermitPatternMatches [''State]) #-}
{-# ANN drop_All_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN drop_All_x1 (PermitTypeClasses []) #-}
{-# NOINLINE drop_All_x1 #-}
drop_All_x1 :: Int -> Int -> IO ()
drop_All_x1 streamLen = drop_All 1 streamLen

{-# ANN drop_All_x4 (PermitPatternMatches [''State]) #-}
{-# ANN drop_All_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN drop_All_x4 (PermitTypeClasses []) #-}
{-# NOINLINE drop_All_x4 #-}
drop_All_x4 :: Int -> Int -> IO ()
drop_All_x4 streamLen = drop_All 4 streamLen

{-# ANN dropWhile_True_x1 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN dropWhile_True_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN dropWhile_True_x1 (PermitTypeClasses []) #-}
{-# NOINLINE dropWhile_True_x1 #-}
dropWhile_True_x1 :: Int -> Int -> IO ()
dropWhile_True_x1 streamLen = dropWhile_True 1 streamLen

{-# ANN dropWhile_True_x4 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN dropWhile_True_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN dropWhile_True_x4 (PermitTypeClasses []) #-}
{-# NOINLINE dropWhile_True_x4 #-}
dropWhile_True_x4 :: Int -> Int -> IO ()
dropWhile_True_x4 streamLen = dropWhile_True 4 streamLen

{-# ANN dropWhile_False_x1 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN dropWhile_False_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN dropWhile_False_x1 (PermitTypeClasses []) #-}
{-# NOINLINE dropWhile_False_x1 #-}
dropWhile_False_x1 :: Int -> Int -> IO ()
dropWhile_False_x1 streamLen = dropWhile_False 1 streamLen

{-# ANN dropWhile_False_x4 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN dropWhile_False_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN dropWhile_False_x4 (PermitTypeClasses []) #-}
{-# NOINLINE dropWhile_False_x4 #-}
dropWhile_False_x4 :: Int -> Int -> IO ()
dropWhile_False_x4 streamLen = dropWhile_False 4 streamLen

{-# ANN foldrS_x1 (PermitPatternMatches []) #-}
{-# ANN foldrS_x1 (PermitConstructions [''(),''State,''Maybe,''Bool,''Int]) #-}
{-# ANN foldrS_x1 (PermitTypeClasses []) #-}
{-# NOINLINE foldrS_x1 #-}
foldrS_x1 :: Int -> Int -> IO ()
foldrS_x1 streamLen = foldrS 1 streamLen

{-# ANN foldlS_x1 (PermitPatternMatches [''State]) #-}
{-# ANN foldlS_x1 (PermitConstructions [''(),''State,''Maybe,''Bool,''Int]) #-}
{-# ANN foldlS_x1 (PermitTypeClasses []) #-}
{-# NOINLINE foldlS_x1 #-}
foldlS_x1 :: Int -> Int -> IO ()
foldlS_x1 streamLen = foldlS 1 streamLen

{-# ANN intersperse_x1 (PermitPatternMatches [''State]) #-}
{-# ANN intersperse_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN intersperse_x1 (PermitTypeClasses []) #-}
{-# NOINLINE intersperse_x1 #-}
intersperse_x1 :: Int -> Int -> Int -> IO ()
intersperse_x1 bound streamLen = intersperse bound 1 streamLen

{-# ANN intersperse_x4 (PermitPatternMatches [''State]) #-}
{-# ANN intersperse_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN intersperse_x4 (PermitTypeClasses []) #-}
{-# NOINLINE intersperse_x4 #-}
intersperse_x4 :: Int -> Int -> Int -> IO ()
intersperse_x4 bound streamLen = intersperse bound 4 streamLen

{-# ANN intersperse_Pure_x1 (PermitPatternMatches [''State]) #-}
{-# ANN intersperse_Pure_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN intersperse_Pure_x1 (PermitTypeClasses []) #-}
{-# NOINLINE intersperse_Pure_x1 #-}
intersperse_Pure_x1 :: Int -> Int -> Int -> IO ()
intersperse_Pure_x1 bound streamLen = intersperse_Pure bound 1 streamLen

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

{-# INLINE iterateSource #-}
iterateSource
    :: Monad m
    => Int -> (StreamK m Int -> StreamK m Int) -> Int -> Int -> StreamK m Int
iterateSource iterStreamLen g i n = f i (sourceUnfoldrM iterStreamLen n)
    where
        f (0 :: Int) m = g m
        f x m = g (f (x P.- 1) m)

-- this is quadratic
{-# ANN scanl'_Iterated (PermitPatternMatches [''State,''Int]) #-}
{-# ANN scanl'_Iterated (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN scanl'_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE scanl'_Iterated #-}
scanl'_Iterated :: Int -> Int -> Int -> IO ()
scanl'_Iterated iterStreamLen maxIters =
    withDrain
        $ iterateSource
            iterStreamLen (StreamK.scanl' (+) 0) (maxIters `div` 10)

-- this is quadratic
{-# ANN dropWhile_False_Iterated (PermitPatternMatches [''State,''Int]) #-}
{-# ANN dropWhile_False_Iterated (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN dropWhile_False_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE dropWhile_False_Iterated #-}
dropWhile_False_Iterated :: Int -> Int -> Int -> Int -> IO ()
dropWhile_False_Iterated streamLen iterStreamLen maxIters =
    withDrain
        $ iterateSource
            iterStreamLen
            (StreamK.dropWhile (> streamLen))
            (maxIters `div` 10)

{-# ANN mapMWith_Iterated (PermitPatternMatches [''State]) #-}
{-# ANN mapMWith_Iterated (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN mapMWith_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE mapMWith_Iterated #-}
mapMWith_Iterated :: Int -> Int -> Int -> IO ()
mapMWith_Iterated iterStreamLen maxIters =
    withDrain
        $ iterateSource
            iterStreamLen (StreamK.mapMWith StreamK.consM return) maxIters

{-# ANN filter_Even_Iterated (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_Even_Iterated (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_Even_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE filter_Even_Iterated #-}
filter_Even_Iterated :: Int -> Int -> Int -> IO ()
filter_Even_Iterated iterStreamLen maxIters =
    withDrain $ iterateSource iterStreamLen (StreamK.filter even) maxIters

{-# ANN take_All_Iterated (PermitPatternMatches [''State]) #-}
{-# ANN take_All_Iterated (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN take_All_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE take_All_Iterated #-}
take_All_Iterated :: Int -> Int -> Int -> Int -> IO ()
take_All_Iterated streamLen iterStreamLen maxIters =
    withDrain $ iterateSource iterStreamLen (StreamK.take streamLen) maxIters

{-# ANN drop_One_Iterated (PermitPatternMatches [''State]) #-}
{-# ANN drop_One_Iterated (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN drop_One_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE drop_One_Iterated #-}
drop_One_Iterated :: Int -> Int -> Int -> IO ()
drop_One_Iterated iterStreamLen maxIters =
    withDrain $ iterateSource iterStreamLen (StreamK.drop 1) maxIters

{-# ANN dropWhile_True_Iterated (PermitPatternMatches [''State,''Int]) #-}
{-# ANN dropWhile_True_Iterated (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN dropWhile_True_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE dropWhile_True_Iterated #-}
dropWhile_True_Iterated :: Int -> Int -> Int -> Int -> IO ()
dropWhile_True_Iterated streamLen iterStreamLen maxIters =
    withDrain
        $ iterateSource
            iterStreamLen (StreamK.dropWhile (<= streamLen)) maxIters

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

{-# ANN zipWith (PermitPatternMatches [''State,''Int]) #-}
{-# ANN zipWith (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''(,), ''Int]) #-}
{-# ANN zipWith (PermitTypeClasses []) #-}
{-# NOINLINE zipWith #-}
zipWith :: Int -> Int -> IO ()
zipWith streamLen = withDrain $ \n ->
    let src = sourceUnfoldrM streamLen n
    in StreamK.zipWith (,) src src

{-# ANN zipWithM (PermitPatternMatches [''State,''Int]) #-}
{-# ANN zipWithM (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''(,), ''Int]) #-}
{-# ANN zipWithM (PermitTypeClasses []) #-}
{-# NOINLINE zipWithM #-}
zipWithM :: Int -> Int -> IO ()
zipWithM streamLen = withDrain $ \n ->
    let src = sourceUnfoldrM streamLen n
    in StreamK.zipWithM (curry return) src src

-------------------------------------------------------------------------------
-- Sorting
-------------------------------------------------------------------------------

{-# INLINE sortByK #-}
sortByK :: (Int -> Int -> Ordering) -> StreamK m Int -> StreamK m Int
sortByK f = StreamK.mergeMapWith (StreamK.mergeBy f) StreamK.fromPure

{-# INLINE sortBy #-}
sortBy :: (Int -> Int -> Ordering) -> Int -> Int -> IO ()
sortBy cmp streamLen = withDrain $ sortByK cmp . sourceUnfoldrM streamLen

{-# ANN sortBy_Randomized (PermitPatternMatches [''Int,''State]) #-}
{-# ANN sortBy_Randomized (PermitConstructions
    [''Int, ''State, ''Maybe, ''(), ''Bool]) #-}
{-# ANN sortBy_Randomized (PermitTypeClasses []) #-}
{-# NOINLINE sortBy_Randomized #-}
sortBy_Randomized :: Int -> Int -> IO ()
sortBy_Randomized streamLen =
    withDrain
        $ sortByK compare
            . StreamK.map (\x -> if even x then x + 2 else x)
            . sourceUnfoldrM streamLen

-------------------------------------------------------------------------------
-- Joining
-------------------------------------------------------------------------------

{-# ANN interleave (PermitPatternMatches []) #-}
{-# ANN interleave (PermitConstructions [''(),''State,''Maybe,''Bool,''Int]) #-}
{-# ANN interleave (PermitTypeClasses []) #-}
{-# NOINLINE interleave #-}
interleave :: Int -> Int -> IO ()
interleave value =
    withDrain $ \n ->
    StreamK.interleave
        (sourceUnfoldrM (value `div` 2) n)
        (sourceUnfoldrM (value `div` 2) (n + 1))

{-# INLINE concatMapWith #-}
concatMapWith
    :: (StreamK IO Int -> StreamK IO Int -> StreamK IO Int)
    -> Int
    -> Int
    -> Int
    -> IO ()
concatMapWith op outer inner =
    withDrain $ \n ->
    StreamK.concatMapWith op
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

{-# INLINE concatMapWithD #-}
concatMapWithD
    :: (Stream IO Int -> Stream IO Int -> Stream IO Int)
    -> Int
    -> Int
    -> Int
    -> IO ()
concatMapWithD op outer inner =
    withDrain $ \n ->
    StreamK.concatMapWith op1
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

    where

    op1 s1 s2 =
        StreamK.fromStream $ op (StreamK.toStream s1) (StreamK.toStream s2)

{-# INLINE mergeMapWith #-}
mergeMapWith
    :: (StreamK IO Int -> StreamK IO Int -> StreamK IO Int)
    -> Int
    -> Int
    -> Int
    -> IO ()
mergeMapWith op outer inner =
    withDrain $ \n ->
    StreamK.mergeMapWith op
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

{-# INLINE mergeMapWithD #-}
mergeMapWithD
    :: (Stream IO Int -> Stream IO Int -> Stream IO Int)
    -> Int
    -> Int
    -> Int
    -> IO ()
mergeMapWithD op outer inner =
    withDrain $ \n ->
    StreamK.mergeMapWith op1
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

    where

    op1 s1 s2 =
        StreamK.fromStream $ op (StreamK.toStream s1) (StreamK.toStream s2)

-------------------------------------------------------------------------------
-- Merging
-------------------------------------------------------------------------------

{-# INLINE mergeWith #-}
mergeWith ::
    (  (Int -> Int -> Ordering)
    -> StreamK IO Int
    -> StreamK IO Int
    -> StreamK IO Int
    )
    -> (Int -> Int -> Ordering)
    -> Int -> Int -> IO ()
mergeWith g cmp count =
    withDrain $ \n ->
    g cmp
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

{-# INLINE mergeWithM #-}
mergeWithM ::
    (  (Int -> Int -> IO Ordering)
    -> StreamK IO Int
    -> StreamK IO Int
    -> StreamK IO Int
    )
    -> (Int -> Int -> Ordering)
    -> Int -> Int -> IO ()
mergeWithM g cmp count =
    withDrain $ \n ->
    g (\a b -> return $ cmp a b)
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

{-# INLINE mergeBy #-}
mergeBy :: (Int -> Int -> Ordering) -> Int -> Int -> IO ()
mergeBy = mergeWith StreamK.mergeBy

{-# INLINE mergeByM #-}
mergeByM :: (Int -> Int -> Ordering) -> Int -> Int -> IO ()
mergeByM = mergeWithM StreamK.mergeByM

-------------------------------------------------------------------------------
-- Join and merge wrappers
-------------------------------------------------------------------------------

{-# ANN concatMapWith_Append (PermitPatternMatches [''Int,''State]) #-}
{-# ANN concatMapWith_Append (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN concatMapWith_Append (PermitTypeClasses []) #-}
{-# NOINLINE concatMapWith_Append #-}
concatMapWith_Append :: Int -> Int -> Int -> IO ()
concatMapWith_Append = concatMapWith StreamK.append

{-# ANN concatMapWith_Interleave (PermitPatternMatches [''Int,''State]) #-}
{-# ANN concatMapWith_Interleave (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN concatMapWith_Interleave (PermitTypeClasses []) #-}
{-# NOINLINE concatMapWith_Interleave #-}
concatMapWith_Interleave :: Int -> Int -> Int -> IO ()
concatMapWith_Interleave = concatMapWith StreamK.interleave

{-# ANN concatMapWith_D_Interleave (PermitPatternMatches
    [''Stream, ''Producer.InterleaveState, ''Step, ''Int, ''State]) #-}
{-# ANN concatMapWith_D_Interleave (PermitConstructions
    [ ''Step, ''Producer.InterleaveState, ''Stream, ''(), ''State, ''Maybe
    , ''Bool, ''Int
    ]) #-}
{-# ANN concatMapWith_D_Interleave (PermitTypeClasses []) #-}
{-# NOINLINE concatMapWith_D_Interleave #-}
concatMapWith_D_Interleave :: Int -> Int -> Int -> IO ()
concatMapWith_D_Interleave = concatMapWithD Stream.interleave

{-# ANN concatMapWith_D_RoundRobin (PermitPatternMatches
    [''Stream, ''Stream.InterleaveState, ''Step, ''Int, ''State]) #-}
{-# ANN concatMapWith_D_RoundRobin (PermitConstructions
    [ ''Step, ''Stream.InterleaveState, ''Stream, ''(), ''State, ''Maybe
    , ''Bool, ''Int
    ]) #-}
{-# ANN concatMapWith_D_RoundRobin (PermitTypeClasses []) #-}
{-# NOINLINE concatMapWith_D_RoundRobin #-}
concatMapWith_D_RoundRobin :: Int -> Int -> Int -> IO ()
concatMapWith_D_RoundRobin = concatMapWithD Stream.roundRobin

{-# ANN mergeMapWith_Interleave (PermitPatternMatches [''Int,''State]) #-}
{-# ANN mergeMapWith_Interleave (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN mergeMapWith_Interleave (PermitTypeClasses []) #-}
{-# NOINLINE mergeMapWith_Interleave #-}
mergeMapWith_Interleave :: Int -> Int -> Int -> IO ()
mergeMapWith_Interleave = mergeMapWith StreamK.interleave

{-# ANN mergeMapWith_MergeBy_Compare (PermitPatternMatches [''State,''Int]) #-}
{-# ANN mergeMapWith_MergeBy_Compare (PermitConstructions
    [''Int, ''State, ''Maybe, ''(), ''Bool]) #-}
{-# ANN mergeMapWith_MergeBy_Compare (PermitTypeClasses []) #-}
{-# NOINLINE mergeMapWith_MergeBy_Compare #-}
mergeMapWith_MergeBy_Compare :: Int -> Int -> Int -> IO ()
mergeMapWith_MergeBy_Compare = mergeMapWith (StreamK.mergeBy compare)

{-# ANN mergeMapWith_MergeBy_FlipCompare (PermitPatternMatches
    [''State, ''Int]) #-}
{-# ANN mergeMapWith_MergeBy_FlipCompare (PermitConstructions
    [''Int, ''State, ''Maybe, ''(), ''Bool]) #-}
{-# ANN mergeMapWith_MergeBy_FlipCompare (PermitTypeClasses []) #-}
{-# NOINLINE mergeMapWith_MergeBy_FlipCompare #-}
mergeMapWith_MergeBy_FlipCompare :: Int -> Int -> Int -> IO ()
mergeMapWith_MergeBy_FlipCompare =
    mergeMapWith (StreamK.mergeBy (flip compare))

{-# ANN mergeMapWith_ZipWith (PermitPatternMatches [''State,''Int]) #-}
{-# ANN mergeMapWith_ZipWith (PermitConstructions
    [''State, ''Maybe, ''(), ''Bool, ''Int]) #-}
{-# ANN mergeMapWith_ZipWith (PermitTypeClasses []) #-}
{-# NOINLINE mergeMapWith_ZipWith #-}
mergeMapWith_ZipWith :: Int -> Int -> Int -> IO ()
mergeMapWith_ZipWith = mergeMapWith (StreamK.zipWith (+))

{-# ANN mergeMapWith_D_Interleave (PermitPatternMatches
    [''Stream, ''Producer.InterleaveState, ''Step, ''State, ''Int]) #-}
{-# ANN mergeMapWith_D_Interleave (PermitConstructions
    [ ''Step, ''Producer.InterleaveState, ''Stream, ''(), ''State, ''Maybe
    , ''Bool, ''Int
    ]) #-}
{-# ANN mergeMapWith_D_Interleave (PermitTypeClasses []) #-}
{-# NOINLINE mergeMapWith_D_Interleave #-}
mergeMapWith_D_Interleave :: Int -> Int -> Int -> IO ()
mergeMapWith_D_Interleave = mergeMapWithD Stream.interleave

{-# ANN mergeMapWith_D_RoundRobin (PermitPatternMatches
    [''Stream, ''Stream.InterleaveState, ''Step, ''State, ''Int]) #-}
{-# ANN mergeMapWith_D_RoundRobin (PermitConstructions
    [ ''Step, ''Stream.InterleaveState, ''Stream, ''(), ''State, ''Maybe
    , ''Bool, ''Int
    ]) #-}
{-# ANN mergeMapWith_D_RoundRobin (PermitTypeClasses []) #-}
{-# NOINLINE mergeMapWith_D_RoundRobin #-}
mergeMapWith_D_RoundRobin :: Int -> Int -> Int -> IO ()
mergeMapWith_D_RoundRobin = mergeMapWithD Stream.roundRobin

{-# ANN mergeMapWith_D_MergeBy_Compare (PermitPatternMatches
    [''(,,,), ''Maybe, ''(), ''Int, ''Step, ''Stream, ''State]) #-}
{-# ANN mergeMapWith_D_MergeBy_Compare (PermitConstructions
    [''(,,,), ''Maybe, ''Step, ''Stream, ''(), ''State, ''Bool, ''Int]) #-}
{-# ANN mergeMapWith_D_MergeBy_Compare (PermitTypeClasses []) #-}
{-# NOINLINE mergeMapWith_D_MergeBy_Compare #-}
mergeMapWith_D_MergeBy_Compare :: Int -> Int -> Int -> IO ()
mergeMapWith_D_MergeBy_Compare = mergeMapWithD (Stream.mergeBy compare)

{-# ANN mergeMapWith_D_MergeBy_FlipCompare (PermitPatternMatches
    [''(,,,), ''Maybe, ''(), ''Int, ''Step, ''Stream, ''State]) #-}
{-# ANN mergeMapWith_D_MergeBy_FlipCompare (PermitConstructions
    [''(,,,), ''Maybe, ''Step, ''Stream, ''(), ''State, ''Bool, ''Int]) #-}
{-# ANN mergeMapWith_D_MergeBy_FlipCompare (PermitTypeClasses []) #-}
{-# NOINLINE mergeMapWith_D_MergeBy_FlipCompare #-}
mergeMapWith_D_MergeBy_FlipCompare :: Int -> Int -> Int -> IO ()
mergeMapWith_D_MergeBy_FlipCompare =
    mergeMapWithD (Stream.mergeBy (flip compare))

{-# ANN mergeBy_Compare (PermitPatternMatches [''Int]) #-}
{-# ANN mergeBy_Compare (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN mergeBy_Compare (PermitTypeClasses []) #-}
{-# NOINLINE mergeBy_Compare #-}
mergeBy_Compare :: Int -> Int -> IO ()
mergeBy_Compare = mergeBy compare

{-# ANN mergeBy_FlipCompare (PermitPatternMatches [''Int]) #-}
{-# ANN mergeBy_FlipCompare (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN mergeBy_FlipCompare (PermitTypeClasses []) #-}
{-# NOINLINE mergeBy_FlipCompare #-}
mergeBy_FlipCompare :: Int -> Int -> IO ()
mergeBy_FlipCompare = mergeBy (flip compare)

{-# ANN mergeByM_Compare (PermitPatternMatches [''State,''Ordering,''Int]) #-}
{-# ANN mergeByM_Compare (PermitConstructions
    [''State, ''Maybe, ''(), ''Bool, ''Int]) #-}
{-# ANN mergeByM_Compare (PermitTypeClasses []) #-}
{-# NOINLINE mergeByM_Compare #-}
mergeByM_Compare :: Int -> Int -> IO ()
mergeByM_Compare = mergeByM compare

{-# ANN mergeByM_FlipCompare (PermitPatternMatches
    [''State, ''Ordering, ''Int]) #-}
{-# ANN mergeByM_FlipCompare (PermitConstructions
    [''State, ''Maybe, ''(), ''Bool, ''Int]) #-}
{-# ANN mergeByM_FlipCompare (PermitTypeClasses []) #-}
{-# NOINLINE mergeByM_FlipCompare #-}
mergeByM_FlipCompare :: Int -> Int -> IO ()
mergeByM_FlipCompare = mergeByM (flip compare)

{-# ANN sortBy_Compare (PermitPatternMatches [''State,''Ordering,''Int]) #-}
{-# ANN sortBy_Compare (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN sortBy_Compare (PermitTypeClasses []) #-}
{-# NOINLINE sortBy_Compare #-}
sortBy_Compare :: Int -> Int -> IO ()
sortBy_Compare = sortBy compare

{-# ANN sortBy_FlipCompare (PermitPatternMatches [''State,''Ordering,''Int]) #-}
{-# ANN sortBy_FlipCompare (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN sortBy_FlipCompare (PermitTypeClasses []) #-}
{-# NOINLINE sortBy_FlipCompare #-}
sortBy_FlipCompare :: Int -> Int -> IO ()
sortBy_FlipCompare = sortBy (flip compare)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mergeBy_Compare
inspect $ 'mergeBy_Compare `hasNoType` ''SPEC

inspect $ hasNoTypeClasses 'mergeByM_Compare
inspect $ 'mergeByM_Compare `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Mixed Composition
-------------------------------------------------------------------------------

{-# INLINE scanl'_map #-}
scanl'_map :: Int -> Int -> Int -> IO ()
scanl'_map n streamLen =
    withStream streamLen
        (composeN n (StreamK.map (subtract 1) . StreamK.scanl' (+) 0))

{-# INLINE drop_map #-}
drop_map :: Int -> Int -> Int -> IO ()
drop_map n streamLen =
    withStream streamLen
        (composeN n (StreamK.map (subtract 1) . StreamK.drop 1))

{-# INLINE drop_scanl' #-}
drop_scanl' :: Int -> Int -> Int -> IO ()
drop_scanl' n streamLen =
    withStream streamLen (composeN n (StreamK.scanl' (+) 0 . StreamK.drop 1))

{-# INLINE take_drop #-}
take_drop :: Int -> Int -> Int -> IO ()
take_drop n streamLen =
    withStream streamLen (composeN n (StreamK.drop 1 . StreamK.take streamLen))

{-# INLINE take_scanl' #-}
take_scanl' :: Int -> Int -> Int -> IO ()
take_scanl' n streamLen =
    withStream streamLen
        (composeN n (StreamK.scanl' (+) 0 . StreamK.take streamLen))

{-# INLINE take_map #-}
take_map :: Int -> Int -> Int -> IO ()
take_map n streamLen =
    withStream streamLen
        (composeN n (StreamK.map (subtract 1) . StreamK.take streamLen))

{-# INLINE filter_drop #-}
filter_drop :: Int -> Int -> Int -> IO ()
filter_drop n streamLen =
    withStream streamLen
        (composeN n (StreamK.drop 1 . StreamK.filter (<= streamLen)))

{-# INLINE filter_take #-}
filter_take :: Int -> Int -> Int -> IO ()
filter_take n streamLen =
    withStream streamLen
        (composeN n (StreamK.take streamLen . StreamK.filter (<= streamLen)))

{-# INLINE filter_scanl' #-}
filter_scanl' :: Int -> Int -> Int -> IO ()
filter_scanl' n streamLen =
    withStream streamLen
        (composeN n (StreamK.scanl' (+) 0 . StreamK.filter (<= maxBound)))

{-# INLINE filter_map #-}
filter_map :: Int -> Int -> Int -> IO ()
filter_map n streamLen =
    withStream streamLen
        (composeN n (StreamK.map (subtract 1) . StreamK.filter (<= streamLen)))

{-# ANN scanl'_map_x1 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN scanl'_map_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN scanl'_map_x1 (PermitTypeClasses []) #-}
{-# NOINLINE scanl'_map_x1 #-}
scanl'_map_x1 :: Int -> Int -> IO ()
scanl'_map_x1 streamLen = scanl'_map 1 streamLen

{-# ANN scanl'_map_x2 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN scanl'_map_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN scanl'_map_x2 (PermitTypeClasses []) #-}
{-# NOINLINE scanl'_map_x2 #-}
scanl'_map_x2 :: Int -> Int -> IO ()
scanl'_map_x2 streamLen = scanl'_map 2 streamLen

{-# ANN scanl'_map_x4 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN scanl'_map_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN scanl'_map_x4 (PermitTypeClasses []) #-}
{-# NOINLINE scanl'_map_x4 #-}
scanl'_map_x4 :: Int -> Int -> IO ()
scanl'_map_x4 streamLen = scanl'_map 4 streamLen

{-# ANN drop_map_x1 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN drop_map_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN drop_map_x1 (PermitTypeClasses []) #-}
{-# NOINLINE drop_map_x1 #-}
drop_map_x1 :: Int -> Int -> IO ()
drop_map_x1 streamLen = drop_map 1 streamLen

{-# ANN drop_map_x2 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN drop_map_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN drop_map_x2 (PermitTypeClasses []) #-}
{-# NOINLINE drop_map_x2 #-}
drop_map_x2 :: Int -> Int -> IO ()
drop_map_x2 streamLen = drop_map 2 streamLen

{-# ANN drop_map_x4 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN drop_map_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN drop_map_x4 (PermitTypeClasses []) #-}
{-# NOINLINE drop_map_x4 #-}
drop_map_x4 :: Int -> Int -> IO ()
drop_map_x4 streamLen = drop_map 4 streamLen

{-# ANN drop_scanl'_x1 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN drop_scanl'_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN drop_scanl'_x1 (PermitTypeClasses []) #-}
{-# NOINLINE drop_scanl'_x1 #-}
drop_scanl'_x1 :: Int -> Int -> IO ()
drop_scanl'_x1 streamLen = drop_scanl' 1 streamLen

{-# ANN drop_scanl'_x2 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN drop_scanl'_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN drop_scanl'_x2 (PermitTypeClasses []) #-}
{-# NOINLINE drop_scanl'_x2 #-}
drop_scanl'_x2 :: Int -> Int -> IO ()
drop_scanl'_x2 streamLen = drop_scanl' 2 streamLen

{-# ANN drop_scanl'_x4 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN drop_scanl'_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN drop_scanl'_x4 (PermitTypeClasses []) #-}
{-# NOINLINE drop_scanl'_x4 #-}
drop_scanl'_x4 :: Int -> Int -> IO ()
drop_scanl'_x4 streamLen = drop_scanl' 4 streamLen

{-# ANN take_drop_x1 (PermitPatternMatches [''State]) #-}
{-# ANN take_drop_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN take_drop_x1 (PermitTypeClasses []) #-}
{-# NOINLINE take_drop_x1 #-}
take_drop_x1 :: Int -> Int -> IO ()
take_drop_x1 streamLen = take_drop 1 streamLen

{-# ANN take_drop_x2 (PermitPatternMatches [''State]) #-}
{-# ANN take_drop_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN take_drop_x2 (PermitTypeClasses []) #-}
{-# NOINLINE take_drop_x2 #-}
take_drop_x2 :: Int -> Int -> IO ()
take_drop_x2 streamLen = take_drop 2 streamLen

{-# ANN take_drop_x4 (PermitPatternMatches [''State]) #-}
{-# ANN take_drop_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN take_drop_x4 (PermitTypeClasses []) #-}
{-# NOINLINE take_drop_x4 #-}
take_drop_x4 :: Int -> Int -> IO ()
take_drop_x4 streamLen = take_drop 4 streamLen

{-# ANN take_scanl'_x1 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN take_scanl'_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN take_scanl'_x1 (PermitTypeClasses []) #-}
{-# NOINLINE take_scanl'_x1 #-}
take_scanl'_x1 :: Int -> Int -> IO ()
take_scanl'_x1 streamLen = take_scanl' 1 streamLen

{-# ANN take_scanl'_x2 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN take_scanl'_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN take_scanl'_x2 (PermitTypeClasses []) #-}
{-# NOINLINE take_scanl'_x2 #-}
take_scanl'_x2 :: Int -> Int -> IO ()
take_scanl'_x2 streamLen = take_scanl' 2 streamLen

{-# ANN take_scanl'_x4 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN take_scanl'_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN take_scanl'_x4 (PermitTypeClasses []) #-}
{-# NOINLINE take_scanl'_x4 #-}
take_scanl'_x4 :: Int -> Int -> IO ()
take_scanl'_x4 streamLen = take_scanl' 4 streamLen

{-# ANN take_map_x1 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN take_map_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN take_map_x1 (PermitTypeClasses []) #-}
{-# NOINLINE take_map_x1 #-}
take_map_x1 :: Int -> Int -> IO ()
take_map_x1 streamLen = take_map 1 streamLen

{-# ANN take_map_x2 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN take_map_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN take_map_x2 (PermitTypeClasses []) #-}
{-# NOINLINE take_map_x2 #-}
take_map_x2 :: Int -> Int -> IO ()
take_map_x2 streamLen = take_map 2 streamLen

{-# ANN take_map_x4 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN take_map_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN take_map_x4 (PermitTypeClasses []) #-}
{-# NOINLINE take_map_x4 #-}
take_map_x4 :: Int -> Int -> IO ()
take_map_x4 streamLen = take_map 4 streamLen

{-# ANN filter_drop_x1 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_drop_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_drop_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filter_drop_x1 #-}
filter_drop_x1 :: Int -> Int -> IO ()
filter_drop_x1 streamLen = filter_drop 1 streamLen

{-# ANN filter_drop_x2 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_drop_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_drop_x2 (PermitTypeClasses []) #-}
{-# NOINLINE filter_drop_x2 #-}
filter_drop_x2 :: Int -> Int -> IO ()
filter_drop_x2 streamLen = filter_drop 2 streamLen

{-# ANN filter_drop_x4 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_drop_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_drop_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filter_drop_x4 #-}
filter_drop_x4 :: Int -> Int -> IO ()
filter_drop_x4 streamLen = filter_drop 4 streamLen

{-# ANN filter_take_x1 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_take_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_take_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filter_take_x1 #-}
filter_take_x1 :: Int -> Int -> IO ()
filter_take_x1 streamLen = filter_take 1 streamLen

{-# ANN filter_take_x2 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_take_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_take_x2 (PermitTypeClasses []) #-}
{-# NOINLINE filter_take_x2 #-}
filter_take_x2 :: Int -> Int -> IO ()
filter_take_x2 streamLen = filter_take 2 streamLen

{-# ANN filter_take_x4 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_take_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_take_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filter_take_x4 #-}
filter_take_x4 :: Int -> Int -> IO ()
filter_take_x4 streamLen = filter_take 4 streamLen

{-# ANN filter_scanl'_x1 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_scanl'_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_scanl'_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filter_scanl'_x1 #-}
filter_scanl'_x1 :: Int -> Int -> IO ()
filter_scanl'_x1 streamLen = filter_scanl' 1 streamLen

{-# ANN filter_scanl'_x2 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_scanl'_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_scanl'_x2 (PermitTypeClasses []) #-}
{-# NOINLINE filter_scanl'_x2 #-}
filter_scanl'_x2 :: Int -> Int -> IO ()
filter_scanl'_x2 streamLen = filter_scanl' 2 streamLen

{-# ANN filter_scanl'_x4 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN filter_scanl'_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_scanl'_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filter_scanl'_x4 #-}
filter_scanl'_x4 :: Int -> Int -> IO ()
filter_scanl'_x4 streamLen = filter_scanl' 4 streamLen

{-# ANN filter_map_x1 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN filter_map_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_map_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filter_map_x1 #-}
filter_map_x1 :: Int -> Int -> IO ()
filter_map_x1 streamLen = filter_map 1 streamLen

{-# ANN filter_map_x2 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN filter_map_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_map_x2 (PermitTypeClasses []) #-}
{-# NOINLINE filter_map_x2 #-}
filter_map_x2 :: Int -> Int -> IO ()
filter_map_x2 streamLen = filter_map 2 streamLen

{-# ANN filter_map_x4 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN filter_map_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN filter_map_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filter_map_x4 #-}
filter_map_x4 :: Int -> Int -> IO ()
filter_map_x4 streamLen = filter_map 4 streamLen

-------------------------------------------------------------------------------
-- ConcatMap
-------------------------------------------------------------------------------

-- concatMap unfoldrMWith/unfoldrMWith

{-# ANN concatMap (PermitPatternMatches [''Int,''State]) #-}
{-# ANN concatMap (PermitConstructions [''(),''State,''Maybe,''Bool,''Int]) #-}
{-# ANN concatMap (PermitTypeClasses []) #-}
{-# NOINLINE concatMap #-}
concatMap :: Int -> Int -> Int -> IO ()
concatMap outer inner =
    withDrain $ \n ->
    StreamK.concatMap
        (\_ -> sourceUnfoldrM inner n)
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap
#endif

-- concatMap unfoldr/unfoldr

{-# ANN concatMap_Pure (PermitPatternMatches [''Int,''State]) #-}
{-# ANN concatMap_Pure (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN concatMap_Pure (PermitTypeClasses []) #-}
{-# NOINLINE concatMap_Pure #-}
concatMap_Pure :: Int -> Int -> Int -> IO ()
concatMap_Pure outer inner =
    withDrain $ \n ->
    StreamK.concatMap
        (\_ -> sourceUnfoldr inner n)
        (sourceUnfoldr outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap_Pure
#endif

-- concatMap replicate/unfoldrMWith

{-# ANN concatMap_Replicate (PermitPatternMatches [''State]) #-}
{-# ANN concatMap_Replicate (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN concatMap_Replicate (PermitTypeClasses []) #-}
{-# NOINLINE concatMap_Replicate #-}
concatMap_Replicate :: Int -> Int -> Int -> IO ()
concatMap_Replicate outer inner =
    withDrain $ \n ->
    StreamK.concatMap (StreamK.replicate inner) (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap_Replicate
#endif

-- concatMapWith

{-# INLINE sourceConcatMapId #-}
sourceConcatMapId :: Monad m
    => Int -> Int -> StreamK m (StreamK m Int)
sourceConcatMapId val n =
    StreamK.fromFoldable $ P.fmap (StreamK.fromEffect . return) [n..n+val]

{-# ANN concatMapWith_Streams (PermitPatternMatches [''State]) #-}
{-# ANN concatMapWith_Streams (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN concatMapWith_Streams (PermitTypeClasses []) #-}
{-# NOINLINE concatMapWith_Streams #-}
concatMapWith_Streams :: Int -> Int -> IO ()
concatMapWith_Streams streamLen =
    withDrain
        $ StreamK.concatMapWith StreamK.append id
            . sourceConcatMapId streamLen

-------------------------------------------------------------------------------
-- Nested Composition
-------------------------------------------------------------------------------

instance Monad m => Applicative (StreamK.StreamK m) where
    {-# INLINE pure #-}
    pure = StreamK.fromPure

    {-# INLINE (<*>) #-}
    (<*>) = StreamK.crossApply

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

    {-# INLINE (*>) #-}
    (*>) = StreamK.crossApplySnd

    {-# INLINE (<*) #-}
    (<*) = StreamK.crossApplyFst

-- NOTE: even though concatMap for StreamD is 3x faster compared to StreamK,
-- the monad instance of StreamD is slower than StreamK after foldr/build
-- fusion.
instance Monad m => Monad (StreamK.StreamK m) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = flip StreamK.concatMap

{-# ANN ap_ApplicativeInstance_x2 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN ap_ApplicativeInstance_x2 (PermitConstructions
    [''State, ''Maybe, ''(), ''Bool, ''Int]) #-}
{-# ANN ap_ApplicativeInstance_x2 (PermitTypeClasses []) #-}
{-# NOINLINE ap_ApplicativeInstance_x2 #-}
ap_ApplicativeInstance_x2 :: Int -> Int -> IO ()
ap_ApplicativeInstance_x2 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in (+) <$> s <*> s

{-# ANN ap_ApplicativeInstance_Pure_x2 (PermitPatternMatches [''State]) #-}
{-# ANN ap_ApplicativeInstance_Pure_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN ap_ApplicativeInstance_Pure_x2 (PermitTypeClasses []) #-}
{-# NOINLINE ap_ApplicativeInstance_Pure_x2 #-}
ap_ApplicativeInstance_Pure_x2 :: Int -> Int -> IO ()
ap_ApplicativeInstance_Pure_x2 streamLen = withDrain $ \n ->
    let s = sourceUnfoldr streamLen n
    in (+) <$> s <*> s

{-# ANN bind_MonadInstance_x2 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN bind_MonadInstance_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN bind_MonadInstance_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_x2 #-}
bind_MonadInstance_x2 :: Int -> Int -> IO ()
bind_MonadInstance_x2 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in do { x <- s; y <- s; return $ x + y }

{-# ANN bind_MonadInstance_Pure_x2 (PermitPatternMatches [''State]) #-}
{-# ANN bind_MonadInstance_Pure_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN bind_MonadInstance_Pure_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_Pure_x2 #-}
bind_MonadInstance_Pure_x2 :: Int -> Int -> IO ()
bind_MonadInstance_Pure_x2 streamLen = withDrain $ \n ->
    let s = sourceUnfoldr streamLen n
    in do { x <- s; y <- s; return $ x + y }

{-# ANN concatFor_x1 (PermitPatternMatches [''State,''Int]) #-}
{-# ANN concatFor_x1 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN concatFor_x1 (PermitTypeClasses []) #-}
{-# NOINLINE concatFor_x1 #-}
concatFor_x1 :: Int -> Int -> IO ()
concatFor_x1 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatFor s $ \x -> StreamK.fromPure $ x + 1

{-# ANN concatFor_x2 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN concatFor_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN concatFor_x2 (PermitTypeClasses []) #-}
{-# NOINLINE concatFor_x2 #-}
concatFor_x2 :: Int -> Int -> IO ()
concatFor_x2 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            StreamK.fromPure $ x + y

{-# ANN concatForM_x2 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN concatForM_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN concatForM_x2 (PermitTypeClasses []) #-}
{-# NOINLINE concatForM_x2 #-}
concatForM_x2 :: Int -> Int -> IO ()
concatForM_x2 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatForM s $ \x ->
        pure $ StreamK.concatForM s $ \y ->
            pure $ StreamK.fromPure $ x + y

{-# ANN bind_MonadInstance_x3 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN bind_MonadInstance_x3 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN bind_MonadInstance_x3 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_x3 #-}
bind_MonadInstance_x3 :: Int -> Int -> IO ()
bind_MonadInstance_x3 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in do { x <- s; y <- s; z <- s; return $ x + y + z }

{-# ANN bind_MonadInstance_Pure_x3 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN bind_MonadInstance_Pure_x3 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN bind_MonadInstance_Pure_x3 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_Pure_x3 #-}
bind_MonadInstance_Pure_x3 :: Int -> Int -> IO ()
bind_MonadInstance_Pure_x3 streamLen = withDrain $ \n ->
    let s = sourceUnfoldr streamLen n
    in do { x <- s; y <- s; z <- s; return $ x + y + z }

{-# ANN concatFor_x3 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN concatFor_x3 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN concatFor_x3 (PermitTypeClasses []) #-}
{-# NOINLINE concatFor_x3 #-}
concatFor_x3 :: Int -> Int -> IO ()
concatFor_x3 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            StreamK.concatFor s $ \z ->
                StreamK.fromPure $ x + y + z

{-# ANN concatForM_x3 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN concatForM_x3 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN concatForM_x3 (PermitTypeClasses []) #-}
{-# NOINLINE concatForM_x3 #-}
concatForM_x3 :: Int -> Int -> IO ()
concatForM_x3 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatForM s $ \x ->
        pure $ StreamK.concatForM s $ \y ->
            pure $ StreamK.concatForM s $ \z ->
                pure $ StreamK.fromPure $ x + y + z

{-# ANN concatFor_x4 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN concatFor_x4 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN concatFor_x4 (PermitTypeClasses []) #-}
{-# NOINLINE concatFor_x4 #-}
concatFor_x4 :: Int -> Int -> IO ()
concatFor_x4 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            StreamK.concatFor s $ \z ->
                StreamK.concatFor s $ \w ->
                    StreamK.fromPure $ x + y + z + w

{-# ANN concatFor_x5 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN concatFor_x5 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN concatFor_x5 (PermitTypeClasses []) #-}
{-# NOINLINE concatFor_x5 #-}
concatFor_x5 :: Int -> Int -> IO ()
concatFor_x5 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            StreamK.concatFor s $ \z ->
                StreamK.concatFor s $ \w ->
                    StreamK.concatFor s $ \u ->
                        StreamK.fromPure $ x + y + z + w + u

{-# ANN bind_MonadInstance_FilterAllOut_x2 (PermitPatternMatches
    [''Int, ''State]) #-}
{-# ANN bind_MonadInstance_FilterAllOut_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN bind_MonadInstance_FilterAllOut_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_FilterAllOut_x2 #-}
bind_MonadInstance_FilterAllOut_x2 :: Int -> Int -> IO ()
bind_MonadInstance_FilterAllOut_x2 streamLen = withDrain $ \n ->
    let str = sourceUnfoldrM streamLen n
    in do
        x <- str
        y <- str
        let s = x + y
        if s < 0 then return s else StreamK.nil

{-# ANN bind_MonadInstance_FilterAllOut_Pure_x2 (PermitPatternMatches
    [''Int, ''State]) #-}
{-# ANN bind_MonadInstance_FilterAllOut_Pure_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN bind_MonadInstance_FilterAllOut_Pure_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_FilterAllOut_Pure_x2 #-}
bind_MonadInstance_FilterAllOut_Pure_x2 :: Int -> Int -> IO ()
bind_MonadInstance_FilterAllOut_Pure_x2 streamLen = withDrain $ \n ->
    let str = sourceUnfoldr streamLen n
    in do
        x <- str
        y <- str
        let s = x + y
        if s < 0 then return s else StreamK.nil

{-# ANN concatFor_FilterAllOut_x2 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN concatFor_FilterAllOut_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN concatFor_FilterAllOut_x2 (PermitTypeClasses []) #-}
{-# NOINLINE concatFor_FilterAllOut_x2 #-}
concatFor_FilterAllOut_x2 :: Int -> Int -> IO ()
concatFor_FilterAllOut_x2 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            let s1 = x + y
             in if s1 < 0 then StreamK.fromPure s1 else StreamK.nil

{-# ANN bind_MonadInstance_FilterAllIn_x2 (PermitPatternMatches
    [''Int, ''State]) #-}
{-# ANN bind_MonadInstance_FilterAllIn_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN bind_MonadInstance_FilterAllIn_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_FilterAllIn_x2 #-}
bind_MonadInstance_FilterAllIn_x2 :: Int -> Int -> IO ()
bind_MonadInstance_FilterAllIn_x2 streamLen = withDrain $ \n ->
    let str = sourceUnfoldrM streamLen n
    in do
        x <- str
        y <- str
        let s = x + y
        if s > 0 then return s else StreamK.nil

{-# ANN bind_MonadInstance_FilterAllIn_Pure_x2 (PermitPatternMatches
    [''Int, ''State]) #-}
{-# ANN bind_MonadInstance_FilterAllIn_Pure_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN bind_MonadInstance_FilterAllIn_Pure_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_FilterAllIn_Pure_x2 #-}
bind_MonadInstance_FilterAllIn_Pure_x2 :: Int -> Int -> IO ()
bind_MonadInstance_FilterAllIn_Pure_x2 streamLen = withDrain $ \n ->
    let str = sourceUnfoldr streamLen n
    in do
        x <- str
        y <- str
        let s = x + y
        if s > 0 then return s else StreamK.nil

{-# ANN concatFor_FilterAllIn_x2 (PermitPatternMatches [''Int,''State]) #-}
{-# ANN concatFor_FilterAllIn_x2 (PermitConstructions
    [''(), ''State, ''Maybe, ''Bool, ''Int]) #-}
{-# ANN concatFor_FilterAllIn_x2 (PermitTypeClasses []) #-}
{-# NOINLINE concatFor_FilterAllIn_x2 #-}
concatFor_FilterAllIn_x2 :: Int -> Int -> IO ()
concatFor_FilterAllIn_x2 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            let s1 = x + y
             in if s1 > 0 then StreamK.fromPure s1 else StreamK.nil

-------------------------------------------------------------------------------
-- Nested Composition Pure lists
-------------------------------------------------------------------------------

-- There are several list benchmarks here for comparison with lists. It is easy
-- and convenient to see the comparisons when they are here, otherwise we'll
-- have to add a separate module for list benchmarks with the same names and
-- then add a comparison in bench.sh.

{-# INLINE unfoldrList #-}
unfoldrList :: Int -> Int -> [Int]
unfoldrList maxval n = List.unfoldr step n
    where
    step cnt =
        if cnt > n + maxval
        then Nothing
        else Just (cnt, cnt + 1)


{-# INLINE withList #-}
withList :: Int -> ([Int] -> IO b) -> Int -> IO b
withList value f = f . unfoldrList value

{-# ANN last_List (PermitPatternMatches []) #-}
{-# ANN last_List (PermitConstructions [''Int,''SrcLoc,''CallStack]) #-}
{-# ANN last_List (PermitTypeClasses [''IP]) #-}
{-# NOINLINE last_List #-}
last_List :: Int -> Int -> IO [Int]
last_List streamLen = withList streamLen (return . (\xs -> [List.last xs]))

{-# ANN ap_ApplicativeInstance_List_x2 (PermitPatternMatches [''[]]) #-}
{-# ANN ap_ApplicativeInstance_List_x2 (PermitConstructions [''[],''Int]) #-}
{-# ANN ap_ApplicativeInstance_List_x2 (PermitTypeClasses []) #-}
{-# NOINLINE ap_ApplicativeInstance_List_x2 #-}
ap_ApplicativeInstance_List_x2 :: Int -> Int -> IO [Int]
ap_ApplicativeInstance_List_x2 streamLen =
    withList streamLen $ \s -> return $ (+) <$> s <*> s

{-# ANN bind_MonadInstance_List_x2 (PermitPatternMatches [''[]]) #-}
{-# ANN bind_MonadInstance_List_x2 (PermitConstructions [''[],''Int]) #-}
{-# ANN bind_MonadInstance_List_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_List_x2 #-}
bind_MonadInstance_List_x2 :: Int -> Int -> IO [Int]
bind_MonadInstance_List_x2 streamLen = withList streamLen $ \s -> return $ do
    x <- s
    y <- s
    return $ x + y

{-# ANN bind_MonadInstance_List_x3 (PermitPatternMatches [''Int,''[]]) #-}
{-# ANN bind_MonadInstance_List_x3 (PermitConstructions [''[],''Int]) #-}
{-# ANN bind_MonadInstance_List_x3 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_List_x3 #-}
bind_MonadInstance_List_x3 :: Int -> Int -> IO [Int]
bind_MonadInstance_List_x3 streamLen = withList streamLen $ \s -> return $ do
    x <- s
    y <- s
    z <- s
    return $ x + y + z

{-# ANN bind_MonadInstance_FilterAllIn_List_x2 (PermitPatternMatches
    [''Int, ''[]]) #-}
{-# ANN bind_MonadInstance_FilterAllIn_List_x2 (PermitConstructions
    [''[], ''Int]) #-}
{-# ANN bind_MonadInstance_FilterAllIn_List_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_FilterAllIn_List_x2 #-}
bind_MonadInstance_FilterAllIn_List_x2 :: Int -> Int -> IO [Int]
bind_MonadInstance_FilterAllIn_List_x2 streamLen =
    withList streamLen $ \s -> return $ do
    x <- s
    y <- s
    let t = x + y
    if t > 0 then return t else []

{-# ANN bind_MonadInstance_FilterAllOut_List_x2 (PermitPatternMatches
    [''Int, ''[]]) #-}
{-# ANN bind_MonadInstance_FilterAllOut_List_x2 (PermitConstructions
    [''[], ''Int]) #-}
{-# ANN bind_MonadInstance_FilterAllOut_List_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_FilterAllOut_List_x2 #-}
bind_MonadInstance_FilterAllOut_List_x2 :: Int -> Int -> IO [Int]
bind_MonadInstance_FilterAllOut_List_x2 streamLen =
    withList streamLen $ \s -> return $ do
    x <- s
    y <- s
    let t = x + y
    if t < 0 then return t else []

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.StreamK"

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1, 1 :: Int) >>= f

-- Note: Name each benchmark (and its IO action) after the exported function it
-- benchmarks, using the format functionName_dimension1_dimension2..., where
-- the dimensions are optional variants/type specializations. Keep extra info
-- in parenthetical notes in the description.
benchmarks :: Int -> Int -> Int -> [(SpaceComplexity, Benchmark)]
benchmarks streamLen iterStreamLen maxIters =
    let streamLen2  = round (P.fromIntegral streamLen**(1/2::P.Double))
        streamLen3  = round (P.fromIntegral streamLen**(1/3::P.Double))
        streamLen4  = round (P.fromIntegral streamLen**(1/4::P.Double))
        streamLen5  = round (P.fromIntegral streamLen**(1/5::P.Double))
        streamLen16 = round (P.fromIntegral streamLen**(1/16::P.Double))
        halfLen     = streamLen `div` 2
    in
    -- O(1) space
      [ (SpaceO_1, benchIO "unfoldr" $ unfoldr streamLen)
      , (SpaceO_1, benchIO "unfoldrMWith (consM)" $ unfoldrMWith streamLen)
      , (SpaceO_1, benchIO "repeat" $ repeat streamLen)
      , (SpaceO_1, benchIO "repeatM" $ repeatM streamLen)
      , (SpaceO_1, benchIO "replicate" $ replicate streamLen)
      , (SpaceO_1, benchIO "replicateMWith (consM)" $ replicateMWith streamLen)
      , (SpaceO_1, benchIO "iterate" $ iterate streamLen)
      , (SpaceO_1, benchIO "iterateM" $ iterateM streamLen)

      , (SpaceO_1, benchIO "fromFoldable" $ fromFoldable streamLen)
      , (SpaceO_1, benchIO "fromFoldableM" $ fromFoldableM streamLen)

      -- appends
      , (SpaceO_1, benchIO "append_Foldable (fromPure)"
            $ append_Foldable streamLen)
      , (SpaceO_1, benchIO "append_FoldableM (fromEffect)"
            $ append_FoldableM streamLen)

      , (SpaceO_1, benchIO "mapM_" $ mapM_ streamLen)
      , (SpaceO_1, benchIO "uncons" $ uncons streamLen)
      , (SpaceO_1, benchIO "init" $ init streamLen)
      , (SpaceO_1, benchIO "foldl'" $ foldl' streamLen)
      , (SpaceO_1, benchIO "foldlM'" $ foldlM' streamLen)
      , (SpaceO_1, benchIO "last" $ last streamLen)

      , (SpaceO_1, benchIO "ap_ApplicativeInstance_x2 (<*>)"
            $ ap_ApplicativeInstance_x2 streamLen2)
      , (SpaceO_1, benchIO "ap_ApplicativeInstance_Pure_x2 (<*>)"
            $ ap_ApplicativeInstance_Pure_x2 streamLen2)

      , (SpaceO_1, benchIO "bind_MonadInstance_x2"
            $ bind_MonadInstance_x2 streamLen2)
      , (SpaceO_1, benchIO "bind_MonadInstance_x3"
            $ bind_MonadInstance_x3 streamLen3)
      , (SpaceO_1, benchIO "bind_MonadInstance_FilterAllIn_x2"
            $ bind_MonadInstance_FilterAllIn_x2 streamLen2)
      , (SpaceO_1, benchIO "bind_MonadInstance_FilterAllOut_x2"
            $ bind_MonadInstance_FilterAllOut_x2 streamLen2)
      , (SpaceO_1, benchIO "bind_MonadInstance_Pure_x2"
            $ bind_MonadInstance_Pure_x2 streamLen2)
      , (SpaceO_1, benchIO "bind_MonadInstance_Pure_x3"
            $ bind_MonadInstance_Pure_x3 streamLen3)
      , (SpaceO_1, benchIO "bind_MonadInstance_FilterAllIn_Pure_x2"
            $ bind_MonadInstance_FilterAllIn_Pure_x2 streamLen2)
      , (SpaceO_1, benchIO "bind_MonadInstance_FilterAllOut_Pure_x2"
            $ bind_MonadInstance_FilterAllOut_Pure_x2 streamLen2)

      , (SpaceO_1, benchIO "concatFor_x1" $ concatFor_x1 streamLen)
      , (SpaceO_1, benchIO "concatFor_x2" $ concatFor_x2 streamLen2)
      , (SpaceO_1, benchIO "concatFor_x3" $ concatFor_x3 streamLen3)
      , (SpaceO_1, benchIO "concatFor_x4" $ concatFor_x4 streamLen4)
      , (SpaceO_1, benchIO "concatFor_x5" $ concatFor_x5 streamLen5)
      , (SpaceO_1, benchIO "concatForM_x2" $ concatForM_x2 streamLen2)
      , (SpaceO_1, benchIO "concatForM_x3" $ concatForM_x3 streamLen3)
      , (SpaceO_1, benchIO "concatFor_FilterAllIn_x2"
            $ concatFor_FilterAllIn_x2 streamLen2)
      , (SpaceO_1, benchIO "concatFor_FilterAllOut_x2"
            $ concatFor_FilterAllOut_x2 streamLen2)

      , (SpaceO_1, benchIO "foldrS_x1" $ foldrS_x1 streamLen)
      , (SpaceO_1, benchIO "scanl'_x1" $ scanl'_x1 streamLen)
      , (SpaceO_1, benchIO "map_x1" $ map_x1 streamLen)
      , (SpaceO_1, benchIO "fmap_x1" $ fmap_x1 streamLen)
      , (SpaceO_1, benchIO "mapMWith_x1 (consM)" $ mapMWith_x1 streamLen)
      , (SpaceO_1, benchIO "mapMSerial_x1" $ mapMSerial_x1 streamLen)

      , (SpaceO_1, benchIO "scanl'_x4" $ scanl'_x4 streamLen)
      , (SpaceO_1, benchIO "map_x4" $ map_x4 streamLen)
      , (SpaceO_1, benchIO "fmap_x4" $ fmap_x4 streamLen)
      , (SpaceO_1, benchIO "mapMWith_x4 (consM)" $ mapMWith_x4 streamLen)
      , (SpaceO_1, benchIO "mapMSerial_x4" $ mapMSerial_x4 streamLen)

      , (SpaceO_1, benchIO "concatMap_Pure (outer=Max, inner=1)"
            $ concatMap_Pure streamLen 1)
      , (SpaceO_1, benchIO "concatMap_Pure (outer=inner=sqrt Max)"
            $ concatMap_Pure streamLen2 streamLen2)
      , (SpaceO_1, benchIO "concatMap_Pure (outer=1, inner=Max)"
            $ concatMap_Pure 1 streamLen)

      , (SpaceO_1, benchIO "concatMap (outer=Max, inner=1)"
            $ concatMap streamLen 1)
      , (SpaceO_1, benchIO "concatMap (outer=inner=sqrt Max)"
            $ concatMap streamLen2 streamLen2)
      , (SpaceO_1, benchIO "concatMap (outer=1, inner=Max)"
            $ concatMap 1 streamLen)

      , (SpaceO_1, benchIO "concatMap_Replicate (outer=inner=sqrt Max)"
            $ concatMap_Replicate streamLen2 streamLen2)

      -- This is for comparison with append_Foldable
      , (SpaceO_1, benchIO "concatMapWith_Streams (append, fromFoldable)"
            $ concatMapWith_Streams streamLen)

      , (SpaceO_1, benchIO "concatMapWith_Append (outer=Max, inner=1)"
            $ concatMapWith_Append streamLen 1)
      , (SpaceO_1, benchIO "concatMapWith_Append (outer=inner=sqrt Max)"
            $ concatMapWith_Append streamLen2 streamLen2)
      , (SpaceO_1, benchIO "concatMapWith_Append (outer=1, inner=Max)"
            $ concatMapWith_Append 1 streamLen)

      -- interleave with concatMapWith is O(1)
      , (SpaceO_1, benchIO "concatMapWith_Interleave (outer=Max, inner=1)"
            $ concatMapWith_Interleave streamLen 1)
      , (SpaceO_1, benchIO "concatMapWith_Interleave (outer=inner=sqrt Max)"
            $ concatMapWith_Interleave streamLen2 streamLen2)
      , (SpaceO_1, benchIO "concatMapWith_Interleave (outer=1, inner=Max)"
            $ concatMapWith_Interleave 1 streamLen)

      , (SpaceO_1, benchIO "filter_Even_x1" $ filter_Even_x1 streamLen)
      , (SpaceO_1, benchIO "filter_AllOut_x1" $ filter_AllOut_x1 streamLen)
      , (SpaceO_1, benchIO "filter_AllIn_x1" $ filter_AllIn_x1 streamLen)
      , (SpaceO_1, benchIO "take_All_x1" $ take_All_x1 streamLen)
      , (SpaceO_1, benchIO "takeWhile_True_x1" $ takeWhile_True_x1 streamLen)
      , (SpaceO_1, benchIO "drop_One_x1" $ drop_One_x1 streamLen)
      , (SpaceO_1, benchIO "drop_All_x1" $ drop_All_x1 streamLen)
      , (SpaceO_1, benchIO "dropWhile_True_x1" $ dropWhile_True_x1 streamLen)
      , (SpaceO_1, benchIO "dropWhile_False_x1" $ dropWhile_False_x1 streamLen)

      , (SpaceO_1, benchIO "filter_Even_x4" $ filter_Even_x4 streamLen)
      , (SpaceO_1, benchIO "filter_AllOut_x4" $ filter_AllOut_x4 streamLen)
      , (SpaceO_1, benchIO "filter_AllIn_x4" $ filter_AllIn_x4 streamLen)
      , (SpaceO_1, benchIO "take_All_x4" $ take_All_x4 streamLen)
      , (SpaceO_1, benchIO "takeWhile_True_x4" $ takeWhile_True_x4 streamLen)
      , (SpaceO_1, benchIO "drop_One_x4" $ drop_One_x4 streamLen)
      , (SpaceO_1, benchIO "drop_All_x4" $ drop_All_x4 streamLen)
      , (SpaceO_1, benchIO "dropWhile_True_x4" $ dropWhile_True_x4 streamLen)
      , (SpaceO_1, benchIO "dropWhile_False_x4" $ dropWhile_False_x4 streamLen)

      , (SpaceO_1, benchIO "interleave" $ interleave streamLen)

      , (SpaceO_1, benchIO "mergeBy_Compare" $ mergeBy_Compare halfLen)
      , (SpaceO_1, benchIO "mergeByM_Compare" $ mergeByM_Compare halfLen)
      , (SpaceO_1, benchIO "mergeBy_FlipCompare" $ mergeBy_FlipCompare halfLen)
      , (SpaceO_1, benchIO "mergeByM_FlipCompare"
            $ mergeByM_FlipCompare halfLen)

      , (SpaceO_1, benchIO "zipWith" $ zipWith streamLen)
      , (SpaceO_1, benchIO "zipWithM" $ zipWithM streamLen)

      -- join 2 streams using concatMapWith
      , (SpaceO_1, benchIO "concatMapWith_Interleave (2 streams)"
            $ concatMapWith_Interleave 2 halfLen)
      , (SpaceO_1, benchIO "concatMapWith_D_Interleave (2 streams)"
            $ concatMapWith_D_Interleave 2 halfLen)
      , (SpaceO_1, benchIO "concatMapWith_D_RoundRobin (2 streams)"
            $ concatMapWith_D_RoundRobin 2 halfLen)

      -- join 2 streams using mergeMapWith
      , (SpaceO_1, benchIO "mergeMapWith_Interleave (2 streams)"
            $ mergeMapWith_Interleave 2 halfLen)
      , (SpaceO_1, benchIO "mergeMapWith_D_Interleave (2 streams)"
            $ mergeMapWith_D_Interleave 2 halfLen)
      , (SpaceO_1, benchIO "mergeMapWith_D_RoundRobin (2 streams)"
            $ mergeMapWith_D_RoundRobin 2 halfLen)

      , (SpaceO_1, benchIO "mergeMapWith_MergeBy_Compare (2 streams)"
            $ mergeMapWith_MergeBy_Compare 2 halfLen)
      , (SpaceO_1, benchIO "mergeMapWith_MergeBy_FlipCompare (2 streams)"
            $ mergeMapWith_MergeBy_FlipCompare 2 halfLen)
      , (SpaceO_1, benchIO "mergeMapWith_D_MergeBy_Compare (2 streams)"
            $ mergeMapWith_D_MergeBy_Compare 2 halfLen)
      , (SpaceO_1, benchIO "mergeMapWith_D_MergeBy_FlipCompare (2 streams)"
            $ mergeMapWith_D_MergeBy_FlipCompare 2 halfLen)

      , (SpaceO_1, benchIO "mergeMapWith_ZipWith (2 streams)"
            $ mergeMapWith_ZipWith 2 halfLen)

      , (SpaceO_1, benchIO "scanl'_map_x1" $ scanl'_map_x1 streamLen)
      , (SpaceO_1, benchIO "drop_map_x1" $ drop_map_x1 streamLen)
      , (SpaceO_1, benchIO "drop_scanl'_x1" $ drop_scanl'_x1 streamLen)
      , (SpaceO_1, benchIO "take_drop_x1" $ take_drop_x1 streamLen)
      , (SpaceO_1, benchIO "take_scanl'_x1" $ take_scanl'_x1 streamLen)
      , (SpaceO_1, benchIO "take_map_x1" $ take_map_x1 streamLen)
      , (SpaceO_1, benchIO "filter_drop_x1" $ filter_drop_x1 streamLen)
      , (SpaceO_1, benchIO "filter_take_x1" $ filter_take_x1 streamLen)
      , (SpaceO_1, benchIO "filter_scanl'_x1" $ filter_scanl'_x1 streamLen)
      , (SpaceO_1, benchIO "filter_map_x1" $ filter_map_x1 streamLen)

      , (SpaceO_1, benchIO "scanl'_map_x2" $ scanl'_map_x2 streamLen)
      , (SpaceO_1, benchIO "drop_map_x2" $ drop_map_x2 streamLen)
      , (SpaceO_1, benchIO "drop_scanl'_x2" $ drop_scanl'_x2 streamLen)
      , (SpaceO_1, benchIO "take_drop_x2" $ take_drop_x2 streamLen)
      , (SpaceO_1, benchIO "take_scanl'_x2" $ take_scanl'_x2 streamLen)
      , (SpaceO_1, benchIO "take_map_x2" $ take_map_x2 streamLen)
      , (SpaceO_1, benchIO "filter_drop_x2" $ filter_drop_x2 streamLen)
      , (SpaceO_1, benchIO "filter_take_x2" $ filter_take_x2 streamLen)
      , (SpaceO_1, benchIO "filter_scanl'_x2" $ filter_scanl'_x2 streamLen)
      , (SpaceO_1, benchIO "filter_map_x2" $ filter_map_x2 streamLen)

      , (SpaceO_1, benchIO "scanl'_map_x4" $ scanl'_map_x4 streamLen)
      , (SpaceO_1, benchIO "drop_map_x4" $ drop_map_x4 streamLen)
      , (SpaceO_1, benchIO "drop_scanl'_x4" $ drop_scanl'_x4 streamLen)
      , (SpaceO_1, benchIO "take_drop_x4" $ take_drop_x4 streamLen)
      , (SpaceO_1, benchIO "take_scanl'_x4" $ take_scanl'_x4 streamLen)
      , (SpaceO_1, benchIO "take_map_x4" $ take_map_x4 streamLen)
      , (SpaceO_1, benchIO "filter_drop_x4" $ filter_drop_x4 streamLen)
      , (SpaceO_1, benchIO "filter_take_x4" $ filter_take_x4 streamLen)
      , (SpaceO_1, benchIO "filter_scanl'_x4" $ filter_scanl'_x4 streamLen)
      , (SpaceO_1, benchIO "filter_map_x4" $ filter_map_x4 streamLen)

      , (SpaceO_1, benchIO "last_List" $ last_List streamLen)
      , (SpaceO_1, benchIO "ap_ApplicativeInstance_List_x2"
            $ ap_ApplicativeInstance_List_x2 streamLen2)
      , (SpaceO_1, benchIO "bind_MonadInstance_List_x2"
            $ bind_MonadInstance_List_x2 streamLen2)
      , (SpaceO_1, benchIO "bind_MonadInstance_List_x3"
            $ bind_MonadInstance_List_x3 streamLen3)
      , (SpaceO_1, benchIO "bind_MonadInstance_FilterAllIn_List_x2"
            $ bind_MonadInstance_FilterAllIn_List_x2 streamLen2)
      , (SpaceO_1, benchIO "bind_MonadInstance_FilterAllOut_List_x2"
            $ bind_MonadInstance_FilterAllOut_List_x2 streamLen2)

      -- O(n) heap
      , (HeapO_n, benchIO "foldlS_x1" $ foldlS_x1 streamLen)

      , (HeapO_n, benchIO "mergeMapWith_Interleave (outer=Max, inner=1)"
            $ mergeMapWith_Interleave streamLen 1)
      , (HeapO_n, benchIO "mergeMapWith_Interleave (outer=inner=sqrt Max)"
            $ mergeMapWith_Interleave streamLen2 streamLen2)
      , (HeapO_n, benchIO "mergeMapWith_Interleave (outer=1, inner=Max)"
            $ mergeMapWith_Interleave 1 streamLen)

      , (HeapO_n, benchIO "mergeMapWith_D_Interleave (outer=inner=sqrt Max)"
            $ mergeMapWith_D_Interleave streamLen2 streamLen2)
      , (HeapO_n, benchIO "mergeMapWith_D_RoundRobin (outer=inner=sqrt Max)"
            $ mergeMapWith_D_RoundRobin streamLen2 streamLen2)

      , (HeapO_n, benchIO "mergeMapWith_MergeBy_Compare (outer=Max, inner=1)"
            $ mergeMapWith_MergeBy_Compare streamLen 1)
      , (HeapO_n, benchIO
            "mergeMapWith_MergeBy_Compare (outer=inner=sqrt Max)"
            $ mergeMapWith_MergeBy_Compare streamLen2 streamLen2)
      , (HeapO_n, benchIO "mergeMapWith_MergeBy_Compare (outer=1, inner=Max)"
            $ mergeMapWith_MergeBy_Compare 1 streamLen)

      , (HeapO_n, benchIO
            "mergeMapWith_MergeBy_FlipCompare (outer=Max, inner=1)"
            $ mergeMapWith_MergeBy_FlipCompare streamLen 1)
      , (HeapO_n, benchIO
            "mergeMapWith_MergeBy_FlipCompare (outer=inner=sqrt Max)"
            $ mergeMapWith_MergeBy_FlipCompare streamLen2 streamLen2)
      , (HeapO_n, benchIO
            "mergeMapWith_MergeBy_FlipCompare (outer=1, inner=Max)"
            $ mergeMapWith_MergeBy_FlipCompare 1 streamLen)

      , (HeapO_n, benchIO "mergeMapWith_ZipWith (outer=Max, inner=1)"
            $ mergeMapWith_ZipWith streamLen 1)
      , (HeapO_n, benchIO "mergeMapWith_ZipWith (outer=inner=sqrt Max)"
            $ mergeMapWith_ZipWith streamLen2 streamLen2)

      {- HLINT ignore "Use sort" -}
      , (HeapO_n, benchIO "sortBy_Compare" $ sortBy_Compare streamLen)
      , (HeapO_n, benchIO "sortBy_FlipCompare" $ sortBy_FlipCompare streamLen)
      , (HeapO_n, benchIO "sortBy_Randomized" $ sortBy_Randomized streamLen)
      , (HeapO_n, bench "List.sortBy (compare)"
            $ nf (\x -> List.sortBy compare [1..x]) streamLen)
      , (HeapO_n, bench "List.sortBy (flip compare)"
            $ nf (\x -> List.sortBy (flip compare) [1..x]) streamLen)
      , (HeapO_n, bench "List.sortBy (compare, randomized)"
            $ nf (\x -> List.sortBy compare
                    (List.map (\n -> if even n then n + 2 else n) [1..x])
                 )
                 streamLen)

      -- O(n) stack
      , (StackO_n, benchIO "tail_Iterated" $ tail_Iterated streamLen)
      , (StackO_n, benchIO "tail_Null_Iterated" $ tail_Null_Iterated streamLen)
      , (StackO_n, benchIO "tail_Head_Iterated" $ tail_Head_Iterated streamLen)

      -- XXX why do these need so much stack
      , (StackO_n, benchIO "intersperse_x1"
            $ intersperse_x1 streamLen streamLen2)
      , (StackO_n, benchIO "intersperse_Pure_x1"
            $ intersperse_Pure_x1 streamLen streamLen2)
      , (StackO_n, benchIO "intersperse_x4"
            $ intersperse_x4 streamLen streamLen16)

      , (StackO_n, benchIO "mapMWith_Iterated"
            $ mapMWith_Iterated iterStreamLen maxIters)
      , (StackO_n, benchIO "scanl'_Iterated (maxIters div 10)"
            $ scanl'_Iterated iterStreamLen maxIters)
      , (StackO_n, benchIO "filter_Even_Iterated"
            $ filter_Even_Iterated iterStreamLen maxIters)
      , (StackO_n, benchIO "take_All_Iterated"
            $ take_All_Iterated streamLen iterStreamLen maxIters)
      , (StackO_n, benchIO "drop_One_Iterated"
            $ drop_One_Iterated iterStreamLen maxIters)
      , (StackO_n, benchIO "dropWhile_False_Iterated (maxIters div 10)"
            $ dropWhile_False_Iterated streamLen iterStreamLen maxIters)
      , (StackO_n, benchIO "dropWhile_True_Iterated"
            $ dropWhile_True_Iterated streamLen iterStreamLen maxIters)

      -- O(n) space
      , (SpaceO_n, benchIO "toList" $ toList streamLen)

      -- concatMapWith using StreamD versions of interleave operations are
      -- all quadratic, we just measure the sqrtVal benchmark for comparison.
      , (SpaceO_n, benchIO "concatMapWith_D_Interleave (outer=inner=sqrt Max)"
            $ concatMapWith_D_Interleave streamLen2 streamLen2)
      , (SpaceO_n, benchIO "concatMapWith_D_RoundRobin (outer=inner=sqrt Max)"
            $ concatMapWith_D_RoundRobin streamLen2 streamLen2)
      ]


main :: IO ()
main = do
    runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks streamLen =
        let !iterStreamLen = 10
            !maxIters = streamLen `div` iterStreamLen
            allBenches = benchmarks streamLen iterStreamLen maxIters
            get x = P.map snd $ filter ((==) x . fst) allBenches
            o1 = get SpaceO_1
            o_n_heap = get HeapO_n
            o_n_stack = get StackO_n
            o_n_space = get SpaceO_n
        in
        [ bgroup (o_1_space_prefix moduleName) o1
        , bgroup (o_n_stack_prefix moduleName) o_n_stack
        , bgroup (o_n_heap_prefix moduleName) o_n_heap
        , bgroup (o_n_space_prefix moduleName) o_n_space
        ]
