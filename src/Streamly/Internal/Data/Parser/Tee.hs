{-# LANGUAGE CPP                       #-}

-- Somehow uni-pattern match using "let" produces better optimized code
-- compared to using cases and using explicit "error" messages in unreachable
-- cases.  There seem to be no way to silence individual warning so we use this
-- global option for the file.
--
-- Disabling this has the potential to mask off some legit warnings, therefore
-- we have segregated code that uses uni-pattern matches in this file.
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
#endif

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Parser.Tee
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Parsers.

module Streamly.Internal.Data.Parser.Tee
    (
    -- Parallel parsers
      teeWith
    , teeWithFst
    , teeWithMin
    )
where

import Control.Exception (assert)
import Prelude
       hiding (any, all, takeWhile)

import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Parser.Types (Parser(..), Step(..))

-------------------------------------------------------------------------------
-- Distributing Parsers
-------------------------------------------------------------------------------

data StepState s a = StepState s | StepResult a

-- | State of the pair of parsers in a tee composition
-- Note: strictness annotation is important for fusing the constructors
{-# ANN type TeeState Fuse #-}
data TeeState sL sR x a b =
    TeePair !([x], StepState sL a, [x], [x]) !([x], StepState sR b, [x], [x])

data Res = Yld Int | Stp Int | Skp

-- XXX: With the current "Step" semantics, it is hard to write, and not sure
-- how useful, an efficient teeWith that returns a correct unused input count.
--
-- | @teeWith f p1 p2@ distributes its input to both @p1@ and @p2@ until both
-- of them succeed or fail and combines their output using @f@. The parser
-- succeeds if both the parsers succeed.
--
-- /Internal/
--
{-# INLINE teeWith #-}
teeWith :: Monad m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
teeWith zf (Parser stepL initialL extractL) (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    {-# INLINE_LATE initial #-}
    initial = do
        sL <- initialL
        sR <- initialR
        return $ TeePair ([], StepState sL, [], []) ([], StepState sR, [], [])

    {-# INLINE consume #-}
    consume buf inp1 inp2 stp st y = do
        let (x, inp11, inp21) =
                case inp1 of
                    [] -> (y, [], [])
                    z : [] -> (z, reverse (x:inp2), [])
                    z : zs -> (z, zs, x:inp2)
        r <- stp st x
        let buf1 = x:buf
        return (buf1, r, inp11, inp21)

    -- consume one input item and return the next state of the fold
    {-# INLINE useStream #-}
    useStream buf inp1 inp2 stp st y = do
        (buf1, r, inp11, inp21) <- consume buf inp1 inp2 stp st y
        case r of
            Yield n s ->
                let state = (Prelude.take n buf1, StepState s, inp11, inp21)
                 in assert (n <= length buf1) (return (state, Yld n))
            Stop n b ->
                let state = (Prelude.take n buf1, StepResult b, inp11, inp21)
                 in assert (n <= length buf1) (return (state, Stp n))
            -- Skip 0 s -> (buf1, Right s, inp11, inp21)
            Skip n s ->
                let (src0, buf2) = splitAt n buf1
                    src  = Prelude.reverse src0
                    state = (buf2, StepState s, src ++ inp11, inp21)
                 in assert (n <= length buf1) (return (state, Skp))

    {-# INLINE_LATE step #-}
    step (TeePair (bufL, StepState sL, inpL1, inpL2)
                  (bufR, StepState sR, inpR1, inpR2)) x = do
        (l,stL) <- useStream bufL inpL1 inpL2 stepL sL x
        (r,stR) <- useStream bufR inpR1 inpR2 stepR sR x
        let next = TeePair l r
        return $ case (stL,stR) of
            (Yld n1, Yld n2) -> Yield (min n1 n2) next
            (Yld n1, Stp n2) -> Yield (min n1 n2) next
            (Stp n1, Yld n2) -> Yield (min n1 n2) next
            (Stp n1, Stp n2) ->
                -- Uni-pattern match results in better optimized code compared
                -- to a case match.
                let (_, StepResult rL, _, _) = l
                    (_, StepResult rR, _, _) = r
                 in Stop (min n1 n2) (zf rL rR)
            _ -> Skip 0 next

    step (TeePair (bufL, StepState sL, inpL1, inpL2)
                r@(_, StepResult rR, _, _)) x = do
        (l,stL) <- useStream bufL inpL1 inpL2 stepL sL x
        let next = TeePair l r
        -- XXX If the unused count of this stream is lower than the unused
        -- count of the stopped stream, only then this will be correct. We need
        -- to fix the other case. We need to keep incrementing the unused count
        -- of the stopped stream and take the min of the two.
        return $ case stL of
            Yld n -> Yield n next
            Stp n ->
                let (_, StepResult rL, _, _) = l
                 in Stop n (zf rL rR)
            Skp -> Skip 0 next

    step (TeePair l@(_, StepResult rL, _, _)
                    (bufR, StepState sR, inpR1, inpR2)) x = do
        (r, stR) <- useStream bufR inpR1 inpR2 stepR sR x
        let next = TeePair l r
        -- XXX If the unused count of this stream is lower than the unused
        -- count of the stopped stream, only then this will be correct. We need
        -- to fix the other case. We need to keep incrementing the unused count
        -- of the stopped stream and take the min of the two.
        return $ case stR of
            Yld n -> Yield n next
            Stp n ->
                let (_, StepResult rR, _, _) = r
                 in Stop n (zf rL rR)
            Skp -> Skip 0 next

    step _ _ = undefined

    {-# INLINE extractBoth #-}
    extractBoth sL sR = do
        rL <- extractL sL
        rR <- extractR sR
        return (zf rL rR)

    {-# INLINE_LATE extract #-}
    extract st =
        case st of
                TeePair (_, StepState sL, _, _) (_, StepState sR, _, _) ->
                    extractBoth sL sR
                TeePair (_, StepState sL, _, _) (_, StepResult rR, _, _) -> do
                    rL <- extractL sL
                    return $ zf rL rR
                TeePair (_, StepResult  rL, _, _) (_, StepState sR, _, _) -> do
                    rR <- extractR sR
                    return $ zf rL rR
                TeePair (_, StepResult rL, _, _) (_, StepResult rR, _, _) ->
                    return $ zf rL rR

-- | Like 'teeWith' but ends parsing and zips the results, if available,
-- whenever the first parser ends.
--
-- /Internal/
--
{-# INLINE teeWithFst #-}
teeWithFst :: Monad m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
teeWithFst zf (Parser stepL initialL extractL)
              (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    {-# INLINE_LATE initial #-}
    initial = do
        sL <- initialL
        sR <- initialR
        return $ TeePair ([], StepState sL, [], []) ([], StepState sR, [], [])

    {-# INLINE consume #-}
    consume buf inp1 inp2 stp st y = do
        let (x, inp11, inp21) =
                case inp1 of
                    [] -> (y, [], [])
                    z : [] -> (z, reverse (x:inp2), [])
                    z : zs -> (z, zs, x:inp2)
        r <- stp st x
        let buf1 = x:buf
        return (buf1, r, inp11, inp21)

    -- consume one input item and return the next state of the fold
    {-# INLINE useStream #-}
    useStream buf inp1 inp2 stp st y = do
        (buf1, r, inp11, inp21) <- consume buf inp1 inp2 stp st y
        case r of
            Yield n s ->
                let state = (Prelude.take n buf1, StepState s, inp11, inp21)
                 in assert (n <= length buf1) (return (state, Yld n))
            Stop n b ->
                let state = (Prelude.take n buf1, StepResult b, inp11, inp21)
                 in assert (n <= length buf1) (return (state, Stp n))
            -- Skip 0 s -> (buf1, Right s, inp11, inp21)
            Skip n s ->
                let (src0, buf2) = splitAt n buf1
                    src  = Prelude.reverse src0
                    state = (buf2, StepState s, src ++ inp11, inp21)
                 in assert (n <= length buf1) (return (state, Skp))

    {-# INLINE_LATE step #-}
    step (TeePair (bufL, StepState sL, inpL1, inpL2)
                  (bufR, StepState sR, inpR1, inpR2)) x = do
        (l,stL) <- useStream bufL inpL1 inpL2 stepL sL x
        (r,stR) <- useStream bufR inpR1 inpR2 stepR sR x
        let next = TeePair l r
        case (stL,stR) of
            -- XXX what if the first parser returns an unused count which is
            -- more than the second parser's unused count? It does not make
            -- sense for the second parser to consume more than the first
            -- parser. We reset the input cursor based on the first parser.
            -- Error out if the second one has consumed more then the first?
            (Stp n1, Stp _) ->
                -- Uni-pattern match results in better optimized code compared
                -- to a case match.
                let (_, StepResult rL, _, _) = l
                    (_, StepResult rR, _, _) = r
                 in return $ Stop n1 (zf rL rR)
            (Stp n1, Yld _) ->
                let (_, StepResult rL, _, _) = l
                    (_, StepState  ssR, _, _) = r
                 in do
                    rR <- extractR ssR
                    return $ Stop n1 (zf rL rR)
            (Yld n1, Yld n2) -> return $ Yield (min n1 n2) next
            (Yld n1, Stp n2) -> return $ Yield (min n1 n2) next
            _ -> return $ Skip 0 next

    step (TeePair (bufL, StepState sL, inpL1, inpL2)
                r@(_, StepResult rR, _, _)) x = do
        (l,stL) <- useStream bufL inpL1 inpL2 stepL sL x
        let next = TeePair l r
        -- XXX If the unused count of this stream is lower than the unused
        -- count of the stopped stream, only then this will be correct. We need
        -- to fix the other case. We need to keep incrementing the unused count
        -- of the stopped stream and take the min of the two.
        return $ case stL of
            Yld n -> Yield n next
            Stp n ->
                let (_, StepResult rL, _, _) = l
                 in Stop n (zf rL rR)
            Skp -> Skip 0 next

    step _ _ = undefined

    {-# INLINE extractBoth #-}
    extractBoth sL sR = do
        rL <- extractL sL
        rR <- extractR sR
        return (zf rL rR)

    {-# INLINE_LATE extract #-}
    extract st =
        case st of
                TeePair (_, StepState sL, _, _) (_, StepState sR, _, _) ->
                    extractBoth sL sR
                TeePair (_, StepState sL, _, _) (_, StepResult rR, _, _) -> do
                    rL <- extractL sL
                    return $ zf rL rR
                _ -> error "unreachable"

-- | Like 'teeWith' but ends parsing and zips the results, if available,
-- whenever any of the parsers ends or fails.
--
-- /Unimplemented/
--
{-# INLINE teeWithMin #-}
teeWithMin ::
    -- Monad m =>
    (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
teeWithMin = undefined
