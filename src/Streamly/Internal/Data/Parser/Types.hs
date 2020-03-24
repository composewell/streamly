{-# LANGUAGE ExistentialQuantification          #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Streamly.Parser.Types
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Streaming and backtracking parsers.
--
-- The 'Parser' type or a parsing fold is a generalization of the 'Fold' type.
-- The 'Fold' type always succeeds on each input. Therefore it does not need to
-- buffer the input. In contrast, a 'Parser' may fail and backtrack to replay
-- the input again to explore another branch of the parser. Therefore, it needs
-- to buffer the input.
--
-- The 'Parser' type is an extension of the 'Fold' type.  The 'Step' functor
-- for the 'Parser' type has been specifically designed for stream parsing
-- applications, assuming the input to be a seuqentially read stream and
-- potentially buffering a contiguous segment of the stream based on the
-- parser's need.
--
-- To provide richer communication with the input stream, the 'Step' functor of
-- a 'Parser' provides more powerful commands to manipulate the input stream
-- after each step invocation. It allows the fold driver to:
--
-- 1. Buffer the input until explicitly asked to drop the input
-- 2. Drop the input stream beyond a "point of no return" in the history
-- because the input till that point has been consumed irrevocably.
-- 3. Go back in the buffered stream upto the point of no return.
--
-- Based on application requirements it should be possible to design even a
-- richer interface to manipulate the input stream/buffer. For example, we
-- could randomly seek into the stream in the forward or reverse directions or
-- we can even seek to the end or from the end or seek from the beginning.

-- We can distribute and scan/parse a stream using both folds and parsers and
-- merge the resulting streams using different merge strategies (e.g.
-- interleaving or serial).

module Streamly.Internal.Data.Parser.Types
    (
      Step (..)
    , Parser (..)
    , splitWith
    )
where

import Fusion.Plugin.Types (Fuse(..))

-- | The return type of a 'Parser' step.
--
-- A parser is driven by a parse driver one step at a time, at any time the
-- driver may @extract@ the result of the parser. The parser may ask the driver
-- to backtrack at any point, therefore, the driver holds the input up to a
-- point of no return in a backtracking buffer.  The buffer grows or shrinks
-- based on the return values of the parser step execution.
--
-- When a parser step is executed it generates a new state of the parse result
-- along with a command to the driver. The command tells the driver whether to
-- keep the input stream for backtracking or drop it, and how much to keep. The
-- constructors of 'Step' represent the commands to the driver.
--
-- /Internal/
--
{-# ANN type Step Fuse #-}
data Step s =
      Yield Int s
      -- ^ @Yield offset state@ indicates that the parser has yielded a new
      -- result which is a point of no return. The result can be extracted
      -- using @extract@. The driver drops the buffer except @offset@ elements
      -- before the current position in stream. The rule is that if a parser
      -- has yielded at least once it cannot return a failure result.

    | Skip Int s
    -- ^ @Skip offset state@ indicates that the parser has consumed the current
    -- input but no new result has been generated. A new @state@ is generated.
    -- However, if we use @extract@ on @state@ it will generate a result from
    -- the previous @Yield@.  When @offset@ is non-zero it is a backward offset
    -- from the current position in the stream from which the driver will feed
    -- the next input to the parser. The offset cannot be beyond the latest
    -- point of no return created by @Yield@.

    | Stop Int s
    -- ^ @Stop offset state@ asks the driver to stop driving the parser because
    -- it has reached a fixed point and further input will not change the
    -- result.  @offset@ is the count of unused elements which includes the
    -- element on which 'Stop' occurred.  Once a fold stops, driving it further
    -- may produce undefined behavior.

-- | A parsing fold is represented as @Parser step initial extract@. Before we
-- drive a parser we call the @initial@ action to retrieve the initial state of
-- the fold. The driver invokes @step@ with the state returned by the previous
-- step and the next input element. It results into a new state and a command
-- to the driver represented by 'Step' type. At any point of time the driver
-- can call @extract@ to inspect the result of the fold. It may result in an
-- error or an output value.
--
data Parser m a b =
    forall s. Parser (s -> a -> m (Step s)) (m s) (s -> m (Either String b))

-- | Maps a function over the output of the fold.
--
instance Functor m => Functor (Parser m a) where
    {-# INLINE fmap #-}
    fmap f (Parser step initial extract) =
        Parser step initial (fmap3 f extract)

        where

        fmap3 g = fmap (fmap (fmap g))

{-# INLINE wrap #-}
wrap :: Monad m => b -> Parser m a b
wrap b = Parser (\_ _ -> pure $ Stop 0 ())  -- step
                (pure ())                   -- initial
                (\_ -> pure $ Right b)      -- extract

{-# ANN type SeqParseState Fuse #-}
data SeqParseState sl f sr =
    SeqParseL sl | SeqParseR f sr | SeqParseLErr String

-- XXX implement interleaved variant, parse using the first parser and then
-- using the second parser alternately.

-- | Apply two parsers sequentially to an input stream. The input is provided
-- to the first parser, when it is done the remaining input is provided to the
-- second parser. If both the parsers succeed there outputs are combined using
-- the supplied function. The operation fails if any of the parsers fail.
--
-- This undoes an "append" of two streams, it splits the streams.
--
{-# INLINE splitWith #-}
splitWith :: Monad m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
splitWith func (Parser stepL initialL extractL)
               (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    initial = SeqParseL <$> initialL

    -- Note: For the composed parse to terminate, the left parser has to be
    -- a terminating parser returning a Stop at some point.
    step (SeqParseL st) a = do
        r <- stepL st a
        case r of
            Yield _ s -> return $ Skip 0 (SeqParseL s)
            Skip n s -> return $ Skip n (SeqParseL s)
            Stop n s -> do
                res <- extractL s
                case res of
                    Left err -> return $ Stop n (SeqParseLErr err)
                    Right x -> Skip n <$> (SeqParseR (func x) <$> initialR)

    step (SeqParseR f st) a = do
        r <- stepR st a
        return $ case r of
            Yield n s -> Yield n (SeqParseR f s)
            Skip n s -> Skip n (SeqParseR f s)
            Stop n s -> Stop n (SeqParseR f s)

    step (SeqParseLErr _) _ = error "step called in SeqParseLErr"

    -- XXX bimap to add "<*>: Right parse failed" to the error message
    extract (SeqParseR f s) = fmap (fmap f) (extractR s)
    extract (SeqParseL _) = return $ Left "<*>: Incomplete left parse"
    extract (SeqParseLErr err) =
        return $ Left $ "<*>: Left parse failed\n" ++ err

-- | 'Applicative' form of 'splitWith'.
instance Monad m => Applicative (Parser m a) where
    {-# INLINE pure #-}
    pure = wrap

    {-# INLINE (<*>) #-}
    (<*>) = splitWith id
