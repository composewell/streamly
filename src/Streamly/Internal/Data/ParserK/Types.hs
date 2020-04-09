{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "inline.hs"

-- |
-- Module      : Streamly.ParserK.Types
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- CPS style implementation of parsers.

module Streamly.Internal.Data.ParserK.Types
    (
      Parser (..)
    )
where

import Control.Monad (MonadPlus(..), ap)
import Control.Applicative (Alternative(..))
import Streamly.Internal.Data.Zipper (Zipper)
import qualified Streamly.Internal.Data.Zipper as Z

newtype Parser m a b =
    MkParser { runParser :: forall r.
        Zipper m a -> ((Zipper m a, Either String b) -> m r) -> m r }

-- | Maps a function over the output of the fold.
--
instance Functor m => Functor (Parser m a) where
    {-# INLINE fmap #-}
    fmap f parser =
        MkParser $ \inp yieldk -> runParser parser inp (yieldk . fmap (fmap f))

{-# INLINE yield #-}
yield :: b -> Parser m a b
yield b = MkParser (\_ yieldk -> yieldk (Z.nil, Right b))

-------------------------------------------------------------------------------
-- Sequential applicative
-------------------------------------------------------------------------------
--
-- | 'Applicative' form of 'splitWith'.
instance Monad m => Applicative (Parser m a) where
    {-# INLINE pure #-}
    pure = yield

    {-# INLINE (<*>) #-}
    (<*>) = ap

{-# INLINE die #-}
die :: String -> Parser m a b
die err = MkParser (\z yieldk -> yieldk (z, Left err))

instance Monad m => Monad (Parser m a) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    m >>= k = MkParser $ \inp yieldk ->
        let yield1 (z, b) = case b of
                Right x -> runParser (k x) z yieldk
                Left err -> runParser (die err) z yieldk
        in runParser m inp yield1

-- | 'Alternative' instance using 'alt'.
--
-- The "some" and "many" operations of alternative collect results in a pure
-- list which is not scalable and streaming. Instead use the "splitParse"
-- operation to apply a 'Parser' repeatedly on a stream in a scalable manner.
--
instance Monad m => Alternative (Parser m a) where
    {-# INLINE empty #-}
    empty = die "empty"

    {-# INLINE (<|>) #-}
    m1 <|> m2 = MkParser $ \inp yieldk ->
        let yield1 (z, b) = case b of
                Right _ -> yieldk (Z.release z, b)
                Left _ -> runParser m2 (Z.restore z) yieldk
        in runParser m1 (Z.checkpoint inp) yield1
    -- XXX INLINE some and many?

-- | 'mzero' is same as 'empty', it aborts the parser. 'mplus' is same as
-- '<|>', it selects the first succeeding parser.
--
-- /Internal/
--
instance Monad m => MonadPlus (Parser m a) where
    {-# INLINE mzero #-}
    mzero = die "mzero"

    {-# INLINE mplus #-}
    mplus = (<|>)
