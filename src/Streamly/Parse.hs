{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Streamly.Parse
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- A 'Sink' is a 'Fold' is a 'Parse'. A deserializer is a parser. A protocol
-- deserializer is usually a non-backtracking parser i.e we do not need the
-- alternative instance, we know definitely how to parse the following
-- structure. Choice is usually represented by a sum flag in the serialized
-- structure indicating a choice of parse, based on the flag we can choose a
-- different parser using the demux primitive.

-- Some notes on parsers:
--
-- Applicative parsers: The simplest parsers, if the next parse never depends
-- on a pervious parse then we can use purely applicative form.
--
-- Alternative parsers (Backtracking): When a parse fails after consuming some
-- input, and we need to restart the next parse from the beginning i.e. we need
-- to give it the same input that the failed parse consumed then we need
-- backtracking. If we are using the Alternative instance then we are using
-- backtracking.
--
-- Monadic Parsers: If a next parse may depend on the previous parse results
-- then we need monadic parsing. For example, in a protocol deserialization we
-- get the length in the header field to parse a field then we need monadic
-- parser. Monadic parser may need some backtracking even without using the
-- Alternative instance e.g. consider the "takeWhile" combinator. In an
-- applicative parser we can use "span" instead and break the input into two
-- folds independent of each other. However, in a monadic parser we may need to
-- use the output of the first parse for the next parse, in that case we need
-- to use "takeWhile" instead of "span". "takeWhile" would not consume the last
-- element and therefore we will have to return that to the input, to be
-- consumed by the next parse. This is a limited form of backtracking that is
-- required even when we have a successful parse. This requires an additional
-- constructor in the parse result type.

module Streamly.Parse
    (
      Parse (..)

    -- * Combinators
    , parse
    , groups
--    , line

    -- * Transformation
    , ltake
    , ltakeWhile

    -- * Creating
    , drain
    , any
    , all

    -- * Conversion
    , fromFold
    )
where

import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, replicate, concatMap, mconcat, foldMap, unzip)

import Control.Applicative (liftA2)
import Streamly.Foldr.Types (Foldr(..))
import Streamly.Fold.Types (Fold(..), Pair'(..))
import Streamly.Parse.Types (Parse(..), Status(..), fromResult)
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK (IsStream(..))

import qualified Streamly.Fold as F
import qualified Streamly.Streams.StreamD as D
import qualified Streamly.Streams.StreamK as K
import qualified Streamly.Streams.Prelude as P

-- | As soon as a parse fails or succeeds, it no longer accepts any more input.
-- However, as long as a parse remains partial it continues accepting input.
{-# INLINE parse #-}
parse :: Monad m => Parse m a b -> SerialT m a -> m b
parse (Parse step begin done) = P.parselMx' step begin done

{-# INLINABLE drain #-}
drain :: Monad m => Parse m a ()
drain = fromFold F.drain
{-
drain = Parse step initial done
    where
    initial = return $ Partial ()
    step _ _ = return $ Partial ()
    done = return
-}

{-# INLINABLE any #-}
any :: Monad m => (a -> Bool) -> Parse m a Bool
any predicate = Parse step initial done
    where
    initial = return $ Partial False
    step x a = return $
        if x
        then Success x
        else
            if predicate a
            then Success True
            else Partial False
    done = return

{-# INLINABLE all #-}
all :: Monad m => (a -> Bool) -> Parse m a Bool
all predicate = Parse step initial done
    where
    initial = return $ Partial True
    step x a = return $
        if x
        then
            if predicate a
            then Partial True
            else Success False
        else Success x
    done = return

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

-- XXX if the stream ends before the take completes then the driver needs to
-- interpret the partial result as a failure.
--
-- | Takes exactly n elements from the input. The parse succeeds when n
-- elements are consumed. The parse fails if the nested parse fails. Otherwise
-- the parse remains partial.
{-# INLINABLE ltake #-}
ltake :: Monad m => Int -> Parse m a b -> Parse m a b
ltake n (Parse step initial done) = Parse step' initial' done'
    where
    initial' = fmap (fmap $ Pair' 0) initial
    done' (Pair' _ r) = done r
    step' (Pair' i r) a = do
        res <- step r a
        let i' = i + 1
            p = Pair' i' (fromResult res)
        return $
            if i' < n
            then Partial p
            else Success p

-- XXX consider using (Status x) as the argument to the step function so that
-- we can remember and make decisions based on the previous Status of the
-- result. Otherwise remembering that becomes very cumbersome.
--
-- The parse step does not remember the state. The responsibility of the driver
-- is to never call the step again once it succeeds or fails. If the step is
-- called again the result may be unpredictable. For example, it may succeed
-- after failing or may return partial after succeeding.

-- | take while the predicate remains true. Takes elements from the input as
-- long as the predicate succeeds. The parse succeeds when the predicate fails.
-- The parse fails if the nested parse fails. Otherwise the parse remains
-- partial.
{-# INLINABLE ltakeWhile #-}
ltakeWhile :: Monad m => (a -> Bool) -> Parse m a b -> Parse m a b
ltakeWhile predicate (Parse step initial done) = Parse step' initial done
    where
    step' r a = do
        if predicate a
        then step r a
        -- Note, if the "step" had failed earlier we would have returned a
        -- failure, if the driver ignored the failure and called the parse
        -- again we return Success here after returning failure earlier. We do
        -- not remember the state. If we want to do that then we will have to
        -- use a Constructor around "r".
        --
        -- XXX we need to return the unsed value a here.
        else return $ Success r

-- XXX we can take a Fold as an argument and turn that into a parse?
-- This can be an upgrade of a Fold into a parse using a combinator

{-
{-# INLINABLE line #-}
line :: Monad m => Fold m Char a -> Parse m Char a
line (Fold step initial done) = Parse step' initial' done
    where
    initial' = fmap Partial initial
    step' acc a = fmap (if a == '\n' then Success else Partial) $ step acc a

{-# INLINABLE take #-}
take :: Monad m => Int -> Fold m a b -> Parse m a b
take n (Fold step initial done) = Parse step' initial' done'
    where
    initial' = fmap (Partial . Pair' 0) initial
    done' (Pair' _ r) = done r
    step' (Pair' i r) a = do
        res <- step r a
        let i' = i + 1
            p = Pair' i' res
        return $
            if i' < n
            then Partial p
            else Success p
-}

------------------------------------------------------------------------------
-- Upgrade to a parser
------------------------------------------------------------------------------

-- | Convert a 'Fold' to a 'Parse'. When you want to compose folds and
-- parsers together, upgrade a fold to a parser before composing.
--
-- Note that a fold would turn into a parse that always remains partial i.e.
-- never returns success and therefore would end up consuming the whole stream.
fromFold :: Monad m => Fold m a b -> Parse m a b
fromFold (Fold step initial done) = Parse step' initial' done
    where
    initial' = fmap Partial initial
    step' b x = fmap Partial (step b x)

{-
{-# INLINABLE newline #-}
newline :: Monad m => Parse m Char Bool
newline = Parse step initial done
    where
    initial = return $ Partial False
    step _ a = return $
        if a == '\n'
        then Success True
        else Partial False
    done = return

-- Termination combinators can be used to arrive at arbitrarily general parses.
-- For example, one can split the stream using the "sum" fold whenever the
-- running sum exceeds 100. It can also be used to split on a pattern.

-- XXX we can use this to implement the above combinators if the performance
-- looks good.
finishWith :: Parse m a x -> Parse m a y -> Parse m a y
finishWith (Parse stepL initialL doneL) (Parse stepR initialR doneR) =
    Parse step initial done
    where
    initial = do
        resL <- initialL
        resR <- initialR
        return $ case (resL, resR) of
            (Success _, _) -> Success $ Pair' resL resR
            (_, _)           -> Partial $ Pair' resL resR

    step _ a = return $
        if a == '\n'
        then Success True
        else Partial False
    done = return
-}

-- This is the most general grouping/splitting function.
-- foldGroupWith is a grouping dual of foldMapWith. It takes a Parse as an
-- argument and applies it repeatedly on the stream.
--
-- Note that it can only split the stream but cannot do any transformations
-- e.g. if it splits based on a pattern, it cannot remove the pattern, it can
-- only mark where the pattern ends and split the stream at that point.  This
-- can in fact be expressed in terms of a combination of scanl and groupBy.
--
-- |
-- >>> S.toList $ PR.groups (PR.take 2 $ PR.fromFold FL.sum) $ S.fromList [1..10]
-- > [3,7,11,15,19]
--
-- >>> S.toList $ PR.groups (PR.line FL.toList) $ S.fromList "hello\nworld"
-- > ["hello\n","world"]
--
groups
    :: (IsStream t, Monad m)
    => Parse m a b
    -> t m a
    -> t m b
groups f m = D.fromStreamD $ D.chained f (D.toStreamD m)
