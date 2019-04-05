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
    , parse
    , drain
    , any
    , all
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
