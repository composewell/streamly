{-# LANGUAGE CPP                       #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Streamly.Internal.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Parsers.

module Streamly.Internal.Data.Parser
    (
      Parser (..)

    -- * Combinators
    , fromFold

    -- * Folds
    , any
    , all

    , sepBy
    , sepByMax
    , sepWithSuffix
    , wordBy

    -- * Parsers
    , takeWhile
    , takeEQ
    , takeGE

    , sepWithPrefix
    -- , sepWithInfix
    , groupBy

    -- Parallel parsers
    , teeWith
    , teeWithFst
    , teeWithMin
    )
where

import Prelude
       hiding (any, all, takeWhile)

import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Parser.Tee (teeWith, teeWithFst, teeWithMin)
import Streamly.Internal.Data.Parser.Types (Parser(..), Step(..))

import Streamly.Internal.Data.Strict

-------------------------------------------------------------------------------
-- Upgrade folds to parses
-------------------------------------------------------------------------------
--
-- | The resulting parse never terminates and never errors out.
--
{-# INLINE fromFold #-}
fromFold :: Monad m => Fold m a b -> Parser m a b
fromFold (Fold fstep finitial fextract) = Parser step finitial fextract

    where

    step s a = Yield 0 <$> fstep s a

-------------------------------------------------------------------------------
-- Terminating but not failing folds
-------------------------------------------------------------------------------
--
-- |
-- >>> S.parse (PR.any (== 0)) $ S.fromList [1,0,1]
-- > Right True
--
{-# INLINABLE any #-}
any :: Monad m => (a -> Bool) -> Parser m a Bool
any predicate = Parser step initial return

    where

    initial = return False

    step s a = return $
        if s
        then Stop 0 True
        else
            if predicate a
            then Stop 0 True
            else Yield 0 False

-- |
-- >>> S.parse (PR.all (== 0)) $ S.fromList [1,0,1]
-- > Right False
--
{-# INLINABLE all #-}
all :: Monad m => (a -> Bool) -> Parser m a Bool
all predicate = Parser step initial return

    where

    initial = return True

    step s a = return $
        if s
        then
            if predicate a
            then Yield 0 True
            else Stop 0 False
        else Stop 0 False

-------------------------------------------------------------------------------
-- Taking elements
-------------------------------------------------------------------------------
--
-- XXX can we use a "cmp" operation in a common implementation?
--
-- | Stops after taking exactly @n@ input elements.
--
-- * Stops - after @n@ elements.
-- * Fails - if the stream ends before it can collect @n@ elements.
--
-- >>> S.parse (PR.takeExact 4 FL.toList) $ S.fromList [1,0,1]
-- > Left "takeEQ: Expecting exactly 4 elements, got 3"
--
-- /Internal/
--
{-# INLINE takeEQ #-}
takeEQ :: Monad m => Int -> Fold m a b -> Parser m a b
takeEQ n (Fold fstep finitial fextract) = Parser step initial extract

    where

    initial = Tuple' 0 <$> finitial

    step (Tuple' i r) a = do
        res <- fstep r a
        let i1 = i + 1
            s1 = Tuple' i1 res
        if i1 < n then return (Skip 0 s1) else Stop 0 <$> fextract res

    extract (Tuple' i r) =
        if n == i
        then fextract r
        else error err

        where

        err =
               "takeEQ: Expecting exactly " ++ show n
            ++ " elements, got " ++ show i

-- | Take at least @n@ input elements, but can collect more.
--
-- * Stops - never.
-- * Fails - if the stream end before producing @n@ elements.
--
-- >>> S.parse (PR.takeGE 4 FL.toList) $ S.fromList [1,0,1]
-- > Left "takeGE: Expecting at least 4 elements, got only 3"
--
-- >>> S.parse (PR.takeGE 4 FL.toList) $ S.fromList [1,0,1,0,1]
-- > Right [1,0,1,0,1]
--
-- /Internal/
--
{-# INLINE takeGE #-}
takeGE :: Monad m => Int -> Fold m a b -> Parser m a b
takeGE n (Fold fstep finitial fextract) = Parser step initial extract

    where

    initial = Tuple' 0 <$> finitial

    step (Tuple' i r) a = do
        res <- fstep r a
        let i1 = i + 1
            s1 = Tuple' i1 res
        return $
            if i1 < n
            then Skip 0 s1
            else Yield 0 s1

    extract (Tuple' i r) = fmap f (fextract r)

        where

        err =
              "takeGE: Expecting at least " ++ show n
           ++ " elements, got only " ++ show i

        f x =
            if i >= n
            then x
            else error err

-- | Take until the predicate fails. The element on which the predicate fails
-- is returned back to the input stream.
--
-- * Stops - when the predicate fails.
-- * Fails - never.
--
-- >>> S.parse (PR.takeWhile (== 0) FL.toList) $ S.fromList [0,0,1,0,1]
-- > Right [0,0]
--
-- @
-- breakOn p = takeWhile (not p)
-- @
--
-- /Internal/
--
{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
takeWhile predicate (Fold fstep finitial fextract) =
    Parser step initial fextract

    where

    initial = finitial

    step s a =
        if predicate a
        then Yield 0 <$> fstep s a
        else Stop 1 <$> fextract s

-- | Keep taking elements until the predicate succeeds. Drop the succeeding
-- element.
--
-- * Stops - when the predicate succeeds.
-- * Fails - never.
--
-- >>> S.parse (PR.sepBy (== 1) FL.toList) $ S.fromList [0,0,1,0,1]
-- > Right [0,0]
--
-- >>> S.toList $ S.splitParse (PR.sepBy (== 1) FL.toList) $ S.fromList [0,0,1,0,1]
-- > [[0,0],[0],[]]
--
-- S.splitOn pred f = S.splitParse (PR.sepBy pred f)
--
-- /Internal/
--
{-# INLINABLE sepBy #-}
sepBy :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
sepBy predicate (Fold fstep finitial fextract) =
    Parser step initial fextract

    where

    initial = finitial
    step s a =
        if not (predicate a)
        then Yield 0 <$> fstep s a
        else Stop 0 <$> fextract s

-- | Keep taking elements until the predicate succeeds. Take the succeeding
-- element as well.
--
-- * Stops - when the predicate succeeds.
-- * Fails - never.
--
-- S.splitWithSuffix pred f = S.splitParse (PR.sepWithSuffix pred f)
--
-- /Unimplemented/
--
{-# INLINABLE sepWithSuffix #-}
sepWithSuffix ::
    -- Monad m =>
    (a -> Bool) -> Fold m a b -> Parser m a b
sepWithSuffix = undefined

-- | Keep taking elements until the predicate succeeds. Return the succeeding
-- element back to the input.
--
-- * Stops - when the predicate succeeds.
-- * Fails - never.
--
-- S.splitWithPrefix pred f = S.splitParse (PR.sepWithPrefix pred f)
--
-- /Unimplemented/
--
{-# INLINABLE sepWithPrefix #-}
sepWithPrefix ::
    -- Monad m =>
    (a -> Bool) -> Fold m a b -> Parser m a b
sepWithPrefix = undefined

-- | Split using a condition or a count whichever occurs first. This is a
-- hybrid of 'splitOn' and 'take'. The element on which the condition succeeds
-- is dropped.
--
-- /Internal/
--
{-# INLINABLE sepByMax #-}
sepByMax :: Monad m
    => (a -> Bool) -> Int -> Fold m a b -> Parser m a b
sepByMax predicate count (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial = Tuple' 0 <$> finitial
    step (Tuple' i r) a = do
        res <- fstep r a
        let i1 = i + 1
            s1 = Tuple' i1 res
        if not (predicate a) && i1 < count
        then return $ Yield 0 s1
        else do
            b <- fextract res
            return $ Stop 0 b
    extract (Tuple' _ r) = fextract r

-- | Like 'splitOn' after stripping leading, trailing, and repeated separators.
-- Therefore, @".a..b."@ with '.' as the separator would be parsed as
-- @["a","b"]@.  In other words, its like parsing words from whitespace
-- separated text.
--
-- * Stops - when it finds a word separator after a non-word element
-- * Fails - never.
--
-- S.wordsBy pred f = S.splitParse (PR.wordBy pred f)
--
-- /Unimplemented/
--
{-# INLINABLE wordBy #-}
wordBy ::
    -- Monad m =>
    (a -> Bool) -> Fold m a b -> Parser m a b
wordBy = undefined

-- | @group cmp f $ S.fromList [a,b,c,...]@ assigns the element @a@ to the
-- first group, if @a \`cmp` b@ is 'True' then @b@ is also assigned to the same
-- group.  If @a \`cmp` c@ is 'True' then @c@ is also assigned to the same
-- group and so on. When the comparison fails a new group is started. Each
-- group is folded using the fold @f@ and the result of the fold is emitted in
-- the output stream.
--
-- * Stops - when the group ends.
-- * Fails - never.
--
-- S.groupsBy cmp f = S.splitParse (PR.groupBy cmp f)
--
-- /Unimplemented/
--
{-# INLINABLE groupBy #-}
groupBy ::
    -- Monad m =>
    (a -> a -> Bool) -> Fold m a b -> Parser m a b
groupBy = undefined
