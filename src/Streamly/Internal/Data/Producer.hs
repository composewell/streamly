-- |
-- Module      : Streamly.Internal.Data.Producer
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A 'Producer' is an 'Unfold' with an 'extract' function added to to extract
-- the state. It is more powerful but less general than an Unfold.
--
-- A 'Producer' represents steps of a loop generating a sequence of elements.
-- While unfolds are closed representation of imperative loops with some opaque
-- internal state, producers are open loops with the state being accesible to
-- the user.
--
-- Unlike an unfold, which runs a loop till completion, a producer can be
-- stopped in the middle, its state can be extracted, examined, changed, and
-- then it can be resumed later from the stopped state.
--
-- A producer can be used in places where a CPS stream would otherwise be
-- needed, because the state of the loop can be passed around. However, it can
-- be much more efficient than CPS because it allows stream fusion and
-- unecessary function calls can be avoided.

module Streamly.Internal.Data.Producer
    ( Unfold (..)

    -- * Converting
    , simplify

    -- * Unfolds
    , nil
    , nilM
    , unfoldrM
    , fromStreamD
    , fromList

    -- * Combinators
    , NestedLoop (..)
    , concat
    , parse
    , parseMany
    , parseManyD
    )
where

#include "inline.hs"

import Control.Exception (assert)
import Control.Monad.Catch (MonadThrow, throwM)
import GHC.Exts (SpecConstrAnnotation(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Parser.ParserD (ParseError(..), Step(..))
import Streamly.Internal.Data.Producer.Source (Source)
import Streamly.Internal.Data.Stream.StreamD.Step (Step(..))
import Streamly.Internal.Data.Stream.StreamD.Type (Stream(..))
import Streamly.Internal.Data.SVar (defState)
import Streamly.Internal.Data.Unfold.Types (Unfold(..))

import qualified Streamly.Internal.Data.Parser.ParserD as ParserD
import qualified Streamly.Internal.Data.Parser.ParserK.Types as ParserK
import qualified Streamly.Internal.Data.Producer.Source as Source

import Streamly.Internal.Data.Producer.Type
import Prelude hiding (concat)

-- XXX We should write unfolds as producers where possible and define
-- unfolds using "simplify".
--
-------------------------------------------------------------------------------
-- Converting to unfolds
-------------------------------------------------------------------------------

-- | Simplify a producer to an unfold.
--
-- /Internal/
{-# INLINE simplify #-}
simplify :: Producer m a b -> Unfold m a b
simplify (Producer step inject _) = Unfold step inject

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

-- | Convert a StreamD stream into a producer.
--
-- /Internal/
{-# INLINE_NORMAL fromStreamD #-}
fromStreamD :: Monad m => Producer m (Stream m a) a
fromStreamD = Producer step return return

    where

    {-# INLINE_LATE step #-}
    step (UnStream step1 state1) = do
        r <- step1 defState state1
        return $ case r of
            Yield x s -> Yield x (Stream step1 s)
            Skip s    -> Skip (Stream step1 s)
            Stop      -> Stop

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- XXX This should probably be moved to the Source module?
--
-- GHC parser does not accept {-# ANN type [] NoSpecConstr #-}, so we need
-- to make a newtype.
{-# ANN type List NoSpecConstr #-}
newtype List a = List {getList :: [a]}

{-# INLINE_NORMAL parseD #-}
parseD
    :: MonadThrow m
    => ParserD.Parser m a b
    -> Producer m (Source s a) a
    -> Source s a
    -> m (b, Source s a)
parseD
    (ParserD.Parser pstep initial extract)
    (Producer ustep uinject uextract)
    seed = do

    state <- uinject seed
    initial >>= go SPEC state (List [])

    where

    -- XXX currently we are using a dumb list based approach for backtracking
    -- buffer. This can be replaced by a sliding/ring buffer using Data.Array.
    -- That will allow us more efficient random back and forth movement.
    {-# INLINE go #-}
    go !_ st buf !pst = do
        r <- ustep st
        case r of
            Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    Partial 0 pst1 -> go SPEC s (List []) pst1
                    Partial n pst1 -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let src0 = Prelude.take n (x:getList buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s (List []) (List src) pst1
                    Continue 0 pst1 -> go SPEC s (List (x:getList buf)) pst1
                    Continue n pst1 -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let (src0, buf1) = splitAt n (x:getList buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s (List buf1) (List src) pst1
                    Done n b -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let src0 = Prelude.take n (x:getList buf)
                            src  = Prelude.reverse src0
                        s1 <- uextract s
                        return (b, Source.unread src s1)
                    Error err -> throwM $ ParseError err
            Skip s -> go SPEC s buf pst
            Stop   -> do
                b <- extract pst
                -- XXX we should return the remaining buffer
                return (b, Source.source Nothing)

    gobuf !_ s buf (List []) !pst = go SPEC s buf pst
    gobuf !_ s buf (List (x:xs)) !pst = do
        pRes <- pstep pst x
        case pRes of
            Partial 0 pst1 ->
                gobuf SPEC s (List []) (List xs) pst1
            Partial n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC s (List []) (List src) pst1
            Continue 0 pst1 ->
                gobuf SPEC s (List (x:getList buf)) (List xs) pst1
            Continue n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let (src0, buf1) = splitAt n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC s (List buf1) (List src) pst1
            Done n b -> do
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0
                s1 <- uextract s
                return (b, Source.unread src s1)
            Error err -> throwM $ ParseError err

-- | Parse a resumable unfold returning the parsed value and the state of the
-- unfold which can be resumed later to unfold the remaining values.
--
-- /Internal/
{-# INLINE [3] parse #-}
parse
    :: MonadThrow m
    => ParserK.Parser m a b
    -> Producer m (Source s a) a
    -> Source s a
    -> m (b, Source s a)
parse = parseD . ParserK.fromParserK

-------------------------------------------------------------------------------
-- Nested parsing
-------------------------------------------------------------------------------

{-# INLINE parseManyD #-}
parseManyD :: MonadThrow m =>
       ParserD.Parser m a b
    -> Producer m (Source x a) a
    -> Producer m (Source x a) b
parseManyD parser reader = Producer step return return

    where

    {-# INLINE_LATE step #-}
    step src = do
        if Source.isEmpty src
        then return Stop
        else do
            (b, s1) <- parseD parser reader src
            return $ Yield b s1

-- | Apply a parser repeatedly on an unfold to generate an unfold of parsed
-- values.
--
-- /Internal/
{-# INLINE parseMany #-}
parseMany :: MonadThrow m =>
       ParserK.Parser m a b
    -> Producer m (Source x a) a
    -> Producer m (Source x a) b
parseMany parser = parseManyD (ParserK.fromParserK parser)
