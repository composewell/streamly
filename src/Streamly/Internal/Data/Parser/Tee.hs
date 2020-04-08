{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    -- Parallel zipped
      teeWith
    , teeWithFst
    , teeWithMin

    -- Parallel alternatives
    , shortest
    , longest
    )
where

import Control.Exception (assert)
import Control.Monad.Catch (MonadCatch, try)
import Prelude
       hiding (any, all, takeWhile)

import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Parser.Types (Parser(..), Step(..), ParseError)

-------------------------------------------------------------------------------
-- Distribute input to two parsers and collect both results
-------------------------------------------------------------------------------

data StepState s a = StepState s | StepResult a

-- XXX Use a Zipper structure for buffering?
--
-- | State of the pair of parsers in a tee composition
-- Note: strictness annotation is important for fusing the constructors
{-# ANN type TeeState Fuse #-}
data TeeState sL sR x a b =
-- @TeePair (past buffer, parser state, future-buffer1, future-buffer2) ...@
    TeePair !([x], StepState sL a, [x], [x]) !([x], StepState sR b, [x], [x])

data Res = Yld Int | Stp Int | Skp | Err String

-- XXX: With the current "Step" semantics, it is hard to write, and not sure
-- how useful, an efficient teeWith that returns a correct unused input count.
--
-- XXX Teeing a parser with a Fold could be more useful and simpler to
-- implement. A fold never fails or backtracks so we do not need to buffer the
-- input for the fold. It can be useful in, for example, maintaining the line
-- and column number position to report for errors. We can always have the
-- line/column fold running in parallel with the main parser, whenever an error
-- occurs we can zip the error with the context fold.
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
            Error err -> return (undefined, Err err)

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
            (Err err, _) -> Error err
            (_, Err err) -> Error err
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
            Err err -> Error err

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
            Err err -> Error err

    step _ _ = undefined

    {-# INLINE_LATE extract #-}
    extract st =
        case st of
            TeePair (_, StepState sL, _, _) (_, StepState sR, _, _) -> do
                rL <- extractL sL
                rR <- extractR sR
                return $ zf rL rR
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
            Error err -> return (undefined, Err err)

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
            (Err err, _) -> return $ Error err
            (_, Err err) -> return $ Error err
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
            Err err -> Error err

    step _ _ = undefined

    {-# INLINE_LATE extract #-}
    extract st =
        case st of
            TeePair (_, StepState sL, _, _) (_, StepState sR, _, _) -> do
                rL <- extractL sL
                rR <- extractR sR
                return $ zf rL rR
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

-------------------------------------------------------------------------------
-- Distribute input to two parsers and choose one result
-------------------------------------------------------------------------------

-- | Shortest alternative. Apply both parsers in parallel but choose the result
-- from the one which consumed least input i.e. take the shortest succeeding
-- parse.
--
-- /Internal/
--
{-# INLINE shortest #-}
shortest :: Monad m => Parser m x a -> Parser m x a -> Parser m x a
shortest (Parser stepL initialL extractL) (Parser stepR initialR _) =
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
            Error err -> return (undefined, Err err)

    -- XXX Even if a parse finished earlier it may not be shortest if the other
    -- parser finishes later but returns a lot of unconsumed input. Our current
    -- criterion of shortest is whichever parse decided to stop earlier.
    {-# INLINE_LATE step #-}
    step (TeePair (bufL, StepState sL, inpL1, inpL2)
                  (bufR, StepState sR, inpR1, inpR2)) x = do
        (l,stL) <- useStream bufL inpL1 inpL2 stepL sL x
        (r,stR) <- useStream bufR inpR1 inpR2 stepR sR x
        let next = TeePair l r
        return $ case (stL,stR) of
            (Stp n1, _) ->
                let (_, StepResult rL, _, _) = l
                 in Stop n1 rL
            (_, Stp n2) ->
                let (_, StepResult rR, _, _) = r
                 in Stop n2 rR
            (Yld n1, Yld n2) -> Yield (min n1 n2) next
            (Err err, _) -> Error err
            (_, Err err) -> Error err
            _ -> Skip 0 next

    step _ _ = undefined

    {-# INLINE_LATE extract #-}
    extract st =
        case st of
            TeePair (_, StepState sL, _, _) _ -> extractL sL
            _ -> error "unreachable"

-- | Longest alternative. Apply both parsers in parallel but choose the result
-- from the one which consumed more input i.e. take the longest succeeding
-- parse.
--
-- /Internal/
--
{-# INLINE longest #-}
longest :: MonadCatch m => Parser m x a -> Parser m x a -> Parser m x a
longest (Parser stepL initialL extractL) (Parser stepR initialR extractR) =
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
            Error err -> return (undefined, Err err)

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
                let (_, StepResult rL, _, _) = l
                    (_, StepResult rR, _, _) = r
                 in Stop (max n1 n2) (if n1 >= n2 then rL else rR)
            (Err err, _) -> Error err
            (_, Err err) -> Error err
            _ -> Skip 0 next

    -- XXX the parser that finishes last may not be the longest because it may
    -- return a lot of unused input which makes it shorter. Our current
    -- criterion of deciding longest is based on whoever decides to finish
    -- last and not whoever consumed more input.
    --
    -- To actually know who made more progress we need to keep an account of
    -- how many items are unconsumed since the last yield.
    --
    step (TeePair (bufL, StepState sL, inpL1, inpL2)
                r@(_, StepResult _, _, _)) x = do
        (l,stL) <- useStream bufL inpL1 inpL2 stepL sL x
        let next = TeePair l r
        return $ case stL of
            Yld n -> Yield n next
            Stp n ->
                let (_, StepResult rL, _, _) = l
                 in Stop n rL
            Skp -> Skip 0 next
            Err err -> Error err

    step (TeePair l@(_, StepResult _, _, _)
                    (bufR, StepState sR, inpR1, inpR2)) x = do
        (r, stR) <- useStream bufR inpR1 inpR2 stepR sR x
        let next = TeePair l r
        return $ case stR of
            Yld n -> Yield n next
            Stp n ->
                let (_, StepResult rR, _, _) = r
                 in Stop n rR
            Skp -> Skip 0 next
            Err err -> Error err

    step _ _ = undefined

    {-# INLINE_LATE extract #-}
    extract st =
        -- XXX When results are partial we may not be able to precisely compare
        -- which parser has made more progress till now.  One way to do that is
        -- to figure out the actually consumed input up to the last yield.
        --
        case st of
            TeePair (_, StepState sL, _, _) (_, StepState sR, _, _) -> do
                r <- try $ extractL sL
                case r of
                    Left (_ :: ParseError) -> extractR sR
                    Right b -> return b
            TeePair (_, StepState sL, _, _) (_, StepResult rR, _, _) -> do
                r <- try $ extractL sL
                case r of
                    Left (_ :: ParseError) -> return rR
                    Right b -> return b
            TeePair (_, StepResult rL, _, _) (_, StepState sR, _, _) -> do
                r <- try $ extractR sR
                case r of
                    Left (_ :: ParseError) -> return rL
                    Right b -> return b
            TeePair (_, StepResult _, _, _) (_, StepResult _, _, _) ->
                error "unreachable"
