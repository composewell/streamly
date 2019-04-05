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
--

module Streamly.Parse
    (
      Parse (..)

    -- * Applying Parses
    , parse
    , parseGroup

    -- * Parses
    , drain
    , any
    , all

    -- * Combinators for Folds
    , fromFold
    , line
    , take
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
import Streamly.Foldl.Types (Foldl(..), Pair'(..))
import Streamly.Parse.Types (Parse(..), Status(..))
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK (IsStream(..))

import qualified Streamly.Streams.StreamD as D
import qualified Streamly.Streams.StreamK as K
import qualified Streamly.Streams.Prelude as P

{-# INLINE parse #-}
parse :: Monad m => Parse m a b -> SerialT m a -> m b
parse (Parse step begin done) = P.parselMx' step begin done

{-# INLINABLE drain #-}
drain :: Monad m => Parse m a ()
drain = Parse step initial done
    where
    initial = return $ Partial ()
    step _ _ = return $ Partial ()
    done = return

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

------------------------------------------------------------------------------
-- Upgrade to a parser
------------------------------------------------------------------------------

-- | Convert a 'Foldl' to a 'Parse'. When you want to compose folds and
-- parsers together, upgrade a fold to a parser before composing.
--
-- Note that a fold would turn into a parse that always remains partial i.e.
-- never returns success and therefore would end up consuming the whole stream.
fromFold :: Monad m => Foldl m a b -> Parse m a b
fromFold (Foldl step initial done) = Parse step' initial' done
    where
    initial' = fmap Partial initial
    step' b x = fmap Partial (step b x)

-- XXX we can use additional state to detect if a parse if being called even
-- after it returned a Success result. However, we do not do that because of
-- unnecessary performance overhead. The responsiblity is with the caller to
-- not call the parse after Success.
--
-- XXX we can take a Fold as an argument and turn that into a parse?
-- This can be an upgrade of a Fold into a parse using a combinator
{-# INLINABLE line #-}
line :: Monad m => Foldl m Char a -> Parse m Char a
line (Foldl step initial done) = Parse step' initial' done
    where
    initial' = fmap Partial initial
    step' acc a = fmap (if a == '\n' then Success else Partial) $ step acc a

{-# INLINABLE take #-}
take :: Monad m => Int -> Foldl m a b -> Parse m a b
take n (Foldl step initial done) = Parse step' initial' done'
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

-- XXX should it be parseGroups instead?
--
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
-- >>> S.toList $ S.foldGroupWith (PR.count 2 $ FL.sum) $ S.fromList [1..10]
-- > [3,7,11,15,19]
--
-- >>> S.toList $ S.foldGroupWith (PR.line FL.toList) $ S.fromList "hello\nworld"
-- > ["hello\n","world"]
--
parseGroup
    :: (IsStream t, Monad m)
    => (forall n. Monad n => Parse n a b)
    -> t m a
    -> t m b
parseGroup f m = D.fromStreamD $ D.foldGroup f (D.toStreamD m)
