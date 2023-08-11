{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Parser.ParserD.Tee
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Parallel parsers. Distributing the input to multiple parsers at the same
-- time.
--
-- For simplicity, we are using code where a particular state is unreachable
-- but it is not prevented by types.  Somehow uni-pattern match using "let"
-- produces better optimized code compared to using @case@ match and using
-- explicit error messages in unreachable cases.
--
-- There seem to be no way to silence individual warnings so we use a global
-- incomplete uni-pattern match warning suppression option for the file.
-- Disabling the warning for other code as well  has the potential to mask off
-- some legit warnings, therefore, we have segregated only the code that uses
-- uni-pattern matches in this module.

module Streamly.Internal.Data.Parser.Tee
    (
    {-
    -- Parallel zipped
      teeWith
    , teeWithFst
    , teeWithMin

    -- Parallel alternatives
    , shortest
    , longest
    -}
    )
where

{-
import Control.Exception (assert)
import Control.Monad.Catch (MonadCatch, try)
import Prelude
       hiding (any, all, takeWhile)

import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Parser.ParserD.Type
       (Initial(..), Parser(..), Step(..), ParseError)

-------------------------------------------------------------------------------
-- Distribute input to two parsers and collect both results
-------------------------------------------------------------------------------

-- When the input stream is distributed to two parsers, both the parsers can
-- backtrack independently. Therefore, we need separate buffer state for each
-- parser.
--
-- ParserK
--
-- We can keep the state of each parser in the zipper and pass around that
-- zipper to the parsers. Each parser can consume from the zipper and then pass
-- around the zipper to the other parser.
--
-- ParserD
--
-- In the approach we have taken here, the driver pushes one element at a time
-- to the tee and each of the parsers in the tee may buffer it independently
-- for backtracking. So they do not need to depend on the original stream
-- source for individual parser backtracking. Problem arises when both the
-- parsers backtrack and they do not need any input from the driver rather they
-- must consume from their buffers. For such situation we may need a
-- "Continue" style driver command from the tee so that the driver runs
-- the tee without providing it any input. Or we may need a local driver loop
-- until new input is to be demanded from the input stream.
--
-- When the tee errors out or stops, the tee driver may have to backtrack by
-- the specified amount (or the tee must return the leftover input). Therefore,
-- the tee driver also has to buffer, this leads to triple buffering.
--
-- When the tee stops we need to determine the backtracking amount from the
-- leftover of both the parsers. Since both the parsers may have consumed
-- different lengths of the stream we consider the maximum of the two as
-- consumed.
--
  -- XXX We can use Initial instead of StepState
{-# ANN type StepState Fuse #-}
data StepState s a = StepState s | StepResult a

-- | State of the pair of parsers in a tee composition
-- Note: strictness annotation is important for fusing the constructors
{-# ANN type TeeState Fuse #-}
data TeeState sL sR x a b =
-- @TeePair (past buffer, parser state, future-buffer1, future-buffer2) ...@
    TeePair !([x], StepState sL a, [x], [x]) !([x], StepState sR b, [x], [x])

{-# ANN type Res Fuse #-}
data Res = Yld Int | Stp Int | Skp | Err String

-- | See 'Streamly.Internal.Data.Parser.teeWith'.
--
-- /Broken/
--
{-# INLINE teeWith #-}
teeWith :: Monad m
    => (a -> b -> c) -> Parser x m a -> Parser x m b -> Parser x m c
teeWith zf (Parser stepL initialL extractL) (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    {-# INLINE_LATE initial #-}
    initial = do
        resL <- initialL
        resR <- initialR
        return $ case resL of
            IPartial sl ->
                case resR of
                     IPartial sr -> IPartial $ TeePair ([], StepState sl, [], [])
                                                       ([], StepState sr, [], [])
                     IDone br -> IPartial $ TeePair ([], StepState sl, [], [])
                                                    ([], StepResult br, [], [])
                     IError err -> IError err
            IDone bl ->
                case resR of
                     IPartial sr ->
                         IPartial $ TeePair ([], StepResult bl, [], [])
                                            ([], StepState sr, [], [])
                     IDone br -> IDone $ zf bl br
                     IError err -> IError err
            IError err -> IError err

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

    -- XXX This is currently broken, even though both the parsers need to
    -- consume from their buffers after backtracking the driver would still be
    -- pushing more input to the buffers.
    --
    -- consume one input item and return the next state of the fold
    {-# INLINE useStream #-}
    useStream buf inp1 inp2 stp st y = do
        (buf1, r, inp11, inp21) <- consume buf inp1 inp2 stp st y
        case r of
            Partial 0 s ->
                let state = ([], StepState s, inp11, inp21)
                 in return (state, Yld 0)
            Partial n s ->
                let src0 = Prelude.take n buf1
                    src  = Prelude.reverse src0
                    state = ([], StepState s, src ++ inp11, inp21)
                 in assert (n <= length buf1) (return (state, Yld n))
            Done n b ->
                let state = (Prelude.take n buf1, StepResult b, inp11, inp21)
                 in assert (n <= length buf1) (return (state, Stp n))
            -- Continue 0 s -> (buf1, Right s, inp11, inp21)
            Continue n s ->
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
            (Yld n1, Yld n2) -> Partial (min n1 n2) next
            (Yld n1, Stp n2) -> Partial (min n1 n2) next
            (Stp n1, Yld n2) -> Partial (min n1 n2) next
            (Stp n1, Stp n2) ->
                -- Uni-pattern match results in better optimized code compared
                -- to a case match.
                let (_, StepResult rL, _, _) = l
                    (_, StepResult rR, _, _) = r
                 in Done (min n1 n2) (zf rL rR)
            (Err err, _) -> Error err
            (_, Err err) -> Error err
            _ -> Continue 0 next

    step (TeePair (bufL, StepState sL, inpL1, inpL2)
                r@(_, StepResult rR, _, _)) x = do
        (l,stL) <- useStream bufL inpL1 inpL2 stepL sL x
        let next = TeePair l r
        -- XXX If the unused count of this stream is lower than the unused
        -- count of the stopped stream, only then this will be correct. We need
        -- to fix the other case. We need to keep incrementing the unused count
        -- of the stopped stream and take the min of the two.
        return $ case stL of
            Yld n -> Partial n next
            Stp n ->
                let (_, StepResult rL, _, _) = l
                 in Done n (zf rL rR)
            Skp -> Continue 0 next
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
            Yld n -> Partial n next
            Stp n ->
                let (_, StepResult rR, _, _) = r
                 in Done n (zf rL rR)
            Skp -> Continue 0 next
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

-- | See 'Streamly.Internal.Data.Parser.teeWithFst'.
--
-- /Broken/
--
{-# INLINE teeWithFst #-}
teeWithFst :: Monad m
    => (a -> b -> c) -> Parser x m a -> Parser x m b -> Parser x m c
teeWithFst zf (Parser stepL initialL extractL)
              (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    {-# INLINE_LATE initial #-}
    initial = do
        resL <- initialL
        resR <- initialR
        case resL of
            IPartial sl ->
                return $ case resR of
                     IPartial sr -> IPartial $ TeePair ([], StepState sl, [], [])
                                                       ([], StepState sr, [], [])
                     IDone br -> IPartial $ TeePair ([], StepState sl, [], [])
                                                    ([], StepResult br, [], [])
                     IError err -> IError err
            IDone bl ->
                case resR of
                     IPartial sr -> IDone . zf bl <$> extractR sr
                     IDone br -> return $ IDone $ zf bl br
                     IError err -> return $ IError err
            IError err -> return $ IError err

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
            Partial 0 s ->
                let state = ([], StepState s, inp11, inp21)
                 in return (state, Yld 0)
            Partial n _ -> return (undefined, Yld n) -- Not implemented
            Done n b ->
                let state = (Prelude.take n buf1, StepResult b, inp11, inp21)
                 in assert (n <= length buf1) (return (state, Stp n))
            -- Continue 0 s -> (buf1, Right s, inp11, inp21)
            Continue n s ->
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
                 in return $ Done n1 (zf rL rR)
            (Stp n1, Yld _) ->
                let (_, StepResult rL, _, _) = l
                    (_, StepState  ssR, _, _) = r
                 in do
                    rR <- extractR ssR
                    return $ Done n1 (zf rL rR)
            (Yld n1, Yld n2) -> return $ Partial (min n1 n2) next
            (Yld n1, Stp n2) -> return $ Partial (min n1 n2) next
            (Err err, _) -> return $ Error err
            (_, Err err) -> return $ Error err
            _ -> return $ Continue 0 next

    step (TeePair (bufL, StepState sL, inpL1, inpL2)
                r@(_, StepResult rR, _, _)) x = do
        (l,stL) <- useStream bufL inpL1 inpL2 stepL sL x
        let next = TeePair l r
        -- XXX If the unused count of this stream is lower than the unused
        -- count of the stopped stream, only then this will be correct. We need
        -- to fix the other case. We need to keep incrementing the unused count
        -- of the stopped stream and take the min of the two.
        return $ case stL of
            Yld n -> Partial n next
            Stp n ->
                let (_, StepResult rL, _, _) = l
                 in Done n (zf rL rR)
            Skp -> Continue 0 next
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

-- | See 'Streamly.Internal.Data.Parser.teeWithMin'.
--
-- /Unimplemented/
--
{-# INLINE teeWithMin #-}
teeWithMin ::
    -- Monad m =>
    (a -> b -> c) -> Parser x m a -> Parser x m b -> Parser x m c
teeWithMin = undefined

-------------------------------------------------------------------------------
-- Distribute input to two parsers and choose one result
-------------------------------------------------------------------------------

-- | See 'Streamly.Internal.Data.Parser.shortest'.
--
-- /Broken/
--
{-# INLINE shortest #-}
shortest :: Monad m => Parser x m a -> Parser x m a -> Parser x m a
shortest (Parser stepL initialL extractL) (Parser stepR initialR _) =
    Parser step initial extract

    where

    {-# INLINE_LATE initial #-}
    initial = do
        resL <- initialL
        resR <- initialR
        return $ case resL of
            IPartial sl ->
                case resR of
                     IPartial sr -> IPartial $ TeePair ([], StepState sl, [], [])
                                                       ([], StepState sr, [], [])
                     IDone br -> IDone br
                     IError err -> IError err
            IDone bl -> IDone bl
            IError errL ->
                case resR of
                     IPartial _ -> IError errL
                     IDone br -> IDone br
                     IError errR -> IError errR

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
            Partial 0 s ->
                let state = ([], StepState s, inp11, inp21)
                 in return (state, Yld 0)
            Partial n _ -> return (undefined, Yld n) -- Not implemented
            Done n b ->
                let state = (Prelude.take n buf1, StepResult b, inp11, inp21)
                 in assert (n <= length buf1) (return (state, Stp n))
            -- Continue 0 s -> (buf1, Right s, inp11, inp21)
            Continue n s ->
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
                 in Done n1 rL
            (_, Stp n2) ->
                let (_, StepResult rR, _, _) = r
                 in Done n2 rR
            (Yld n1, Yld n2) -> Partial (min n1 n2) next
            (Err err, _) -> Error err
            (_, Err err) -> Error err
            _ -> Continue 0 next

    step _ _ = undefined

    {-# INLINE_LATE extract #-}
    extract st =
        case st of
            TeePair (_, StepState sL, _, _) _ -> extractL sL
            _ -> error "unreachable"

-- | See 'Streamly.Internal.Data.Parser.longest'.
--
-- /Broken/
--
{-# INLINE longest #-}
longest :: MonadCatch m => Parser x m a -> Parser x m a -> Parser x m a
longest (Parser stepL initialL extractL) (Parser stepR initialR extractR) =
    Parser step initial extract

    where


    {-# INLINE_LATE initial #-}
    initial = do
        resL <- initialL
        resR <- initialR
        return $ case resL of
            IPartial sl ->
                case resR of
                     IPartial sr -> IPartial $ TeePair ([], StepState sl, [], [])
                                                       ([], StepState sr, [], [])
                     IDone br -> IPartial $ TeePair ([], StepState sl, [], [])
                                                    ([], StepResult br, [], [])
                     IError _ ->
                         IPartial $ TeePair ([], StepState sl, [], [])
                                            ([], StepResult undefined, [], [])
            IDone bl ->
                case resR of
                     IPartial sr ->
                         IPartial $ TeePair ([], StepResult bl, [], [])
                                            ([], StepState sr, [], [])
                     IDone _ -> IDone bl
                     IError _ -> IDone bl
            IError _ ->
                case resR of
                     IPartial sr ->
                         IPartial $ TeePair ([], StepResult undefined, [], [])
                                            ([], StepState sr, [], [])
                     IDone br -> IDone br
                     IError err -> IError err

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
            Partial 0 s ->
                let state = ([], StepState s, inp11, inp21)
                 in return (state, Yld 0)
            Partial n _ -> return (undefined, Yld n) -- Not implemented
            Done n b ->
                let state = (Prelude.take n buf1, StepResult b, inp11, inp21)
                 in assert (n <= length buf1) (return (state, Stp n))
            -- Continue 0 s -> (buf1, Right s, inp11, inp21)
            Continue n s ->
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
            (Yld n1, Yld n2) -> Partial (min n1 n2) next
            (Yld n1, Stp n2) -> Partial (min n1 n2) next
            (Stp n1, Yld n2) -> Partial (min n1 n2) next
            (Stp n1, Stp n2) ->
                let (_, StepResult rL, _, _) = l
                    (_, StepResult rR, _, _) = r
                 in Done (max n1 n2) (if n1 >= n2 then rL else rR)
            (Err err, _) -> Error err
            (_, Err err) -> Error err
            _ -> Continue 0 next

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
            Yld n -> Partial n next
            Stp n ->
                let (_, StepResult rL, _, _) = l
                 in Done n rL
            Skp -> Continue 0 next
            Err err -> Error err

    step (TeePair l@(_, StepResult _, _, _)
                    (bufR, StepState sR, inpR1, inpR2)) x = do
        (r, stR) <- useStream bufR inpR1 inpR2 stepR sR x
        let next = TeePair l r
        return $ case stR of
            Yld n -> Partial n next
            Stp n ->
                let (_, StepResult rR, _, _) = r
                 in Done n rR
            Skp -> Continue 0 next
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
-}
