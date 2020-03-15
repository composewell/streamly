{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Parse
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Parsers.

module Streamly.Internal.Data.Parse
    (
      Parse (..)

    -- * Parsers
    , any
    , all

    , takeWhile
    , takeExact
    , takeAtLeast

    , endOn
    , endAfter
    , endBefore

    , finishBy
    , zipWith
    )
where

import Control.Exception (assert)
import Prelude
       hiding (any, all, takeWhile, zipWith)

import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Parse.Types (Parse(..), Step(..))
import Streamly.Internal.Data.Fold.Types (Fold(..))

import Streamly.Internal.Data.Strict

-------------------------------------------------------------------------------
-- Terminating folds
-------------------------------------------------------------------------------
--
-- XXX any/all should be terminating folds when we have a terminating fold
-- type.
--
-- >>> S.parse (PR.any (== 0)) $ S.fromList [1,0,1]
-- > Right True
--
{-# INLINABLE any #-}
any :: Monad m => (a -> Bool) -> Parse m a Bool
any predicate = Parse step initial (\x -> return $ Right (0,x))
    where
    initial = return False
    step s a = return $
        if s
        then Halt True
        else
            if predicate a
            then Halt True
            else Keep 0 False

-- >>> S.parse (PR.any (== 0)) $ S.fromList [1,0,1]
-- > Right False
--
{-# INLINABLE all #-}
all :: Monad m => (a -> Bool) -> Parse m a Bool
all predicate = Parse step initial (\x -> return $ Right (0,x))
    where
    initial = return True
    step s a = return $
        if s
        then
            if predicate a
            then Keep 0 True
            else Halt False
        else Halt False

-------------------------------------------------------------------------------
-- Taking elements
-------------------------------------------------------------------------------
--
-- | Stops after taking exactly @n@ input elements.
--
-- * Stops - after @n@ elements.
-- * Fails - if the stream ends before it can collect @n@ elements.
--
-- >>> S.parse (PR.takeExact 4 FL.toList) $ S.fromList [1,0,1]
-- > Left "takeExact: Expecting exactly 4 elements, got 3"
--
-- /Internal/
--
{-# INLINABLE takeExact #-}
takeExact :: Monad m => Int -> Fold m a b -> Parse m a b
takeExact n (Fold fstep finitial fextract) = Parse step initial extract

    where

    initial = (Tuple' 0) <$> finitial

    step (Tuple' i r) a = do
        res <- fstep r a
        let i1 = i + 1
            s1 = Tuple' i1 res
        return $ if i1 < n then Hold s1 else Halt s1

    extract (Tuple' i r) = fmap f (fextract r)

        where

        err =
               "takeExact: Expecting exactly " ++ show n
            ++ " elements, got " ++ show i

        f x =
            if n == i
            then Right (0, x)
            else Left err

-- | Take at least @n@ input elements, but can collect more.
--
-- * Stops - never.
-- * Fails - if the stream end before producing @n@ elements.
--
-- >>> S.parse (PR.takeAtLeast 4 FL.toList) $ S.fromList [1,0,1]
-- > Left "takeAtLeast: Expecting at least 4 elements, got only 3"
--
-- >>> S.parse (PR.takeAtLeast 4 FL.toList) $ S.fromList [1,0,1,0,1]
-- > Right [1,0,1,0,1]
--
-- /Internal/
--
{-# INLINABLE takeAtLeast #-}
takeAtLeast :: Monad m => Int -> Fold m a b -> Parse m a b
takeAtLeast n (Fold fstep finitial fextract) = Parse step initial extract

    where

    initial = (Tuple' 0) <$> finitial

    step (Tuple' i r) a = do
        res <- fstep r a
        let i1 = i + 1
            s1 = Tuple' i1 res
        return $ Hold s1

    extract (Tuple' i r) = fmap f (fextract r)

        where

        err =
              "takeAtLeast: Expecting at least " ++ show n
           ++ " elements, got only " ++ show i

        f x =
            if i >= n
            then Right (0, x)
            else Left err

-- | Take until the predicate fails. Does not take the failing element.
--
-- * Stops - when the predicate fails.
-- * Fails - never.
--
-- >>> S.parse (PR.takeWhile (== 0) FL.toList) $ S.fromList [0,0,1,0,1]
-- > Right [0,0]
--
-- /Internal/
--
{-# INLINABLE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Fold m a b -> Parse m a b
takeWhile predicate (Fold fstep finitial fextract) =
    Parse step initial extract

    where

    initial = finitial
    step s a = do
        if predicate a
        then Keep 0 <$> fstep s a
        else return $ Halt s
    extract s = do
        b <- fextract s
        return $ Right (1, b)

-- | Keep taking elements until the predicate succeeds. Drop the succeeding
-- element.
--
-- * Stops - when the predicate succeeds.
-- * Fails - never.
--
-- >>> S.parse (PR.endOn (== 1) FL.toList) $ S.fromList [0,0,1,0,1]
-- > Right [0,0]
--
-- >>> S.toList $ S.parseChunks (PR.endOn (== 1) FL.toList) $ S.fromList [0,0,1,0,1]
-- > [[0,0],[0],[]]
--
-- /Internal/
--
{-# INLINABLE endOn #-}
endOn :: Monad m => (a -> Bool) -> Fold m a b -> Parse m a b
endOn predicate (Fold fstep finitial fextract) =
    Parse step initial extract

    where

    initial = finitial
    step s a = do
        if not (predicate a)
        then Keep 0 <$> fstep s a
        else return $ Halt s
    extract s = do
        b <- fextract s
        return $ Right (0, b)

-- | Keep taking elements until the predicate succeeds. Take the succeeding
-- element as well.
--
-- * Stops - when the predicate succeeds.
-- * Fails - never.
--
-- /Unimplemented/
--
{-# INLINABLE endAfter #-}
endAfter ::
    -- Monad m =>
    (a -> Bool) -> Fold m a b -> Parse m a b
endAfter = undefined

-- | Keep taking elements until the predicate succeeds. Return the succeeding
-- element back to the input.
--
-- * Stops - when the predicate succeeds.
-- * Fails - never.
--
-- /Unimplemented/
--
{-# INLINABLE endBefore #-}
endBefore ::
    -- Monad m =>
    (a -> Bool) -> Fold m a b -> Parse m a b
endBefore = undefined

-- | Distribute the input to a parse and a fold until the parse succeeds or
-- fails. The parse can be used to decide the termination of the fold.
--
-- /Unimplemented/
--
finishBy :: Parse m a x -> Fold m a y -> Parse m a (x, y)
finishBy = undefined

-------------------------------------------------------------------------------
-- Zipping Parses
-------------------------------------------------------------------------------
--

{-# ANN type ParOne Fuse #-}
data ParOne s a = ParStream [a] s | ParBuf [a] s [a]

-- Note: strictness annotation is important for fusing the constructors
{-# ANN type ParState Fuse #-}
data ParState sL sR a =
    ParPair !(Either sL (ParOne sL a)) !(Either sR (ParOne sR a))

-- XXX we can write a much simpler zipWith if the types do not have Back or
-- Hold. Perhaps we can write a simpler partial function assuming that and
-- statically assert the assumptions with the help of the compiler.
--
-- zipWithMin -- end when either stream ends
-- zipWithFst -- end when the first stream ends
--
-- XXX requires -funfolding-use-threshold=200 to fuse
--
-- | @zipWith f p1 p2@ distributes its input to both @p1@ and @p2@ and combines
-- their output using @f@.
--
-- /Internal/
--
{-# INLINE zipWith #-}
zipWith :: Monad m
    => (a -> b -> c) -> Parse m x a -> Parse m x b -> Parse m x c
zipWith zf (Parse stepL initialL extractL) (Parse stepR initialR extractR) =
    Parse step initial extract

    where

    {-# INLINE_LATE initial #-}
    initial = do
        sL <- initialL
        sR <- initialR
        return $ ParPair (Right (ParStream [] sL))
                         (Right (ParStream [] sR))

    {-# INLINE_LATE step #-}
    step (ParPair (Right (ParStream bufL sL))
                  (Right (ParStream bufR sR))) x = do
        l <- useStream bufL stepL sL x
        r <- useStream bufR stepR sR x
        return $ Keep 0 $ ParPair l r

    {-# INLINE useStream #-}
    useStream buf stp st x = do
        r <- stp st x
        let buf1 = x:buf
        return $ case r of
            Hold s -> Right $ ParStream buf1 s
            Keep n s ->
                 assert (n <= length buf1)
                        (Right $ (ParStream (Prelude.take n buf1) s))
            Back n s ->
                let (src0, buf2) = splitAt n buf1
                    src  = Prelude.reverse src0
                 in assert (n <= length buf1)
                           (Right (ParBuf buf2 s src))
            Halt s -> Left s

    {-# INLINE extractBoth #-}
    extractBoth sL sR = do
        rL <- extractL sL
        case rL of
            Left err -> return $ Left err
            Right (_, x) -> do
                rR <- extractR sR
                return $ case rR of
                    Left err -> Left err
                    Right (_, y) -> Right (0, zf x y)

    {-# INLINE_LATE extract #-}
    extract st =
        let (s1, s2) = case st of
                ParPair (Right (ParStream _ sL))
                        (Right (ParStream _ sR)) -> (sL, sR)
                ParPair (Left sL) (Left sR) -> (sL, sR)
        in extractBoth s1 s2
