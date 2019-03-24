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
import Streamly.Parse.Types (Parse(..), Result(..))
import Streamly.Streams.Serial (SerialT)
import qualified Streamly.Streams.Prelude as P

{-# INLINE parse #-}
parse :: Monad m => Parse m a b -> SerialT m a -> m b
parse (Parse step begin done) = P.parselMx' step begin done

{-# INLINABLE drain #-}
drain :: Monad m => Parse m a ()
drain = Parse step initial done
    where
    initial = return $ More ()
    step _ _ = return $ More ()
    done = return

{-# INLINABLE any #-}
any :: Monad m => (a -> Bool) -> Parse m a Bool
any predicate = Parse step initial done
    where
    initial = return $ More False
    step x a = return $
        if x
        then Done x
        else
            if predicate a
            then Done True
            else More False
    done = return

{-# INLINABLE all #-}
all :: Monad m => (a -> Bool) -> Parse m a Bool
all predicate = Parse step initial done
    where
    initial = return $ More True
    step x a = return $
        if x
        then
            if predicate a
            then More True
            else Done False
        else Done x
    done = return

