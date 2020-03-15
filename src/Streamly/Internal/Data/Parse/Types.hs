{-# LANGUAGE ExistentialQuantification          #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Streamly.Parse.Types
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Streaming and backtracking parsers.
--
-- A parser is a generalization of a classical left fold. A fold never fails,
-- it always produces a result, it has a default initial value. A parser may
-- terminate or fail even without producing any result.

-- Differences between a terminating fold and a parser:
--
-- \* A Parser is a superset of terminating fold with failure and backtracking
--   added.
-- \* A fold may be upgraded to a parser but vice versa is not true.
-- \* A fold is not a partial function, as it may never fail.
-- \* A fold's Step result is simpler as it does not backtrack.
-- \* A fold's result type is simpler as it does not fail and it does not return
--   unused input count.
--
-- Parse and Zip (zipWith) is possible only if both the parses are unskipping
-- or are guaranteed to have outputs in sync. Or else we will have to keep the
-- input buffered and may have to end the zip when one of them finishes. This
-- is simple if the type guarantees no skip e.g. in case of classical left
-- folds.
--
-- Terminating folds can have Sequential and Distribute and zip style
-- applicatives but cannot have Alternative.
--
-- Parsers can have Sequential Applicative and Alternative but not Distribute
-- and zip style applicative.
--
-- We can distribute and scan/parse a stream using both folds and parsers and
-- merge the resulting streams using different merge strategies (e.g.
-- interleaving or serial).

module Streamly.Internal.Data.Parse.Types
    (
      Step (..)
    , Parse (..)
    )
where

-- | The return type of a fold step. A fold is driven by a fold driver one step
-- at a time, at any time the driver may @extract@ and examine the result of
-- the fold. The fold may backtrack at any point, therefore, the driver holds
-- the input in a backtracking buffer. The buffer grows or shrinks based on the
-- return values of the fold step execution.
--
-- When a fold step is executed it generates a new state of the fold along with
-- a command to the fold driver. The command tells the fold driver whether to
-- keep the input for future backtracking or drop it. The constructors of
-- 'Step' represent the commands to the driver.
--
-- /Internal/
--
data Step s =
      Hold s     -- ^ Hold the current input in the buffer, the input is
                 -- accepted tentatively, but the fold may later fail with the
                 -- input remaining unused.
    | Keep Int s -- ^ Keep @n@ most recent inputs, including the input of this
                 -- step invocation, and discard the rest.  This indicates a
                 -- commit of an intermediate result of the fold. A fold cannot
                 -- fail beyond this commit point. If the fold terminates it
                 -- will return at least the result up to this commit and the
                 -- count of unused elements after that.
    | Back Int s -- ^ Go back in the buffer by @n@ items, the count @n@
                 -- includes the input element of this step as well. The fold
                 -- driver backtracks and starts driving the fold with past
                 -- inputs from the requested point in the buffer. Note that
                 -- the driver cannot go back beyond the last commit point
                 -- created by 'Keep'.
    | Halt s     -- ^ Halt because the result of the fold will not change after
                 -- this point.  Once a fold halts, driving it further may
                 -- produce undefined behavior. An @extract@ after the 'Halt'
                 -- would return a count of unused elements which includes the
                 -- element on which 'Halt' occurred.

-- Note: Instead of returning the unused count as part of extract, we can
-- return it via Halt command. However, some folds may not ever halt, but they
-- may still have unused input after the last commit point.

-- | A parsing fold is represented as @Parse step initial extract@. Before we
-- drive a fold we call the @initial@ action to retrieve the initial state of
-- the fold. The driver invokes @step@ with the previous state and an input
-- element. It results into a new state and a command to the driver represented
-- by 'Step' type. At any point of time the driver can call @extract@ to
-- inspect the result of the fold. It may result in an error or an output value
-- along with the count (zero or more) of last unused elements.
--
data Parse m a b =
    forall s. Parse (s -> a -> m (Step s))
                    (m s)
                    (s -> m (Either String (Int, b)))

-- | Maps a function over the output of the fold.
--
instance Monad m => Functor (Parse m a) where
    {-# INLINE fmap #-}
    fmap f (Parse step initial extract) = Parse step initial (fmap4 f extract)

        where

        fmap4 g = fmap (fmap (fmap (fmap g)))

    {-# INLINE (<$) #-}
    (<$) b = \_ -> pure b

{-# INLINE wrap #-}
wrap :: Monad m => b -> Parse m a b
wrap b = Parse (\_ _ -> pure $ Halt ())    -- step
               (pure ())                   -- initial
               (\_ -> pure $ Right (0, b)) -- extract

data SeqParseState sl f sr =
    SeqParseL sl | SeqParseR f sr | SeqParseLErr String

-- XXX implement interleaved variant, parse using the first parser and then
-- using the second parser alternately.
--
-- | Apply two parsing folds sequentially to an input. The input is provided to
-- the first parser, if the parser fails then whole composition fails. If the
-- parser succeeds, the remaining input is supplied to the second parser. If
-- the second parser succeeds the composed parser succeeds otherwise it fails.
--
-- This is the opposite of the "append" operation on streams.
--
instance Monad m => Applicative (Parse m a) where
    {-# INLINE pure #-}
    pure = wrap

    {-# INLINE (<*>) #-}
    (Parse stepL initialL extractL) <*> (Parse stepR initialR extractR) =
        Parse step initial extract

        where

        initial = SeqParseL <$> initialL

        -- Note: For the composed parse to terminate, the left parser has to be
        -- a terminating parser returning a Halt at some point.
        step (SeqParseL st) a = do
            r <- stepL st a
            case r of
                Keep n s -> return $ Keep n (SeqParseL s)
                Hold s   -> return $ Hold (SeqParseL s)
                Back n s -> return $ Back n (SeqParseL s)
                Halt s   -> do
                    res <- extractL s
                    case res of
                        Left err -> return $ Halt (SeqParseLErr err)
                        Right (n, f) -> Back n <$> (SeqParseR f <$> initialR)

        step (SeqParseR f st) a = do
            r <- stepR st a
            return $ case r of
                Keep n s -> Keep n (SeqParseR f s)
                Hold s   -> Hold (SeqParseR f s)
                Back n s -> Back n (SeqParseR f s)
                Halt s   -> Halt (SeqParseR f s)

        step s@(SeqParseLErr _) _ = return $ Halt s

        -- XXX bimap to add "<*>: Right parse failed" to the error message
        extract (SeqParseR f s) = fmap (fmap (fmap f)) (extractR s)
        extract (SeqParseL _) = return $ Left "<*>: Incomplete left parse"
        extract (SeqParseLErr err) =
            return $ Left $ "<*>: Left parse failed\n" ++ err
