{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-} -- XXX
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Streamly.List
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- The 'List' type in this module is just a newtype wrapper around the stream
-- type @SerialT Identity@. It provides better performance compared to the
-- standard Haskell lists and can be used almost as a drop-in replacement. The
-- type @List a@ is equivalent to @[a]@ with the following correspondences:
--
-- * constructor @[]@ corresponds to 'Nil'
-- * constructor @:@ corresponds to 'Cons'
--
-- If you use 'OverloadedLists', 'OverloadedStrings' and 'MonadComprehensions'
-- extensions then the standard list, string and list comprehension syntax can
-- be used. 'toList' and 'fromList' from the 'IsList' typeclass can be used to
-- convert to and from standard lists.  Conversion to stream types is free.
-- Stream combinators can be used on lists by converting them to streams, such
-- as:
--
-- @
-- fromSerial $ S.map (+ 1) $ toSerial (1 \`Cons\` Nil)
-- @
--
-- This module provides combinators that work directly on the 'List' type and
-- therefore no conversion is needed.
--
-- See <docs/streamly-vs-lists.md> for more details and
-- <test/PureStreams.hs> for comprehensive usage examples.
--
module Streamly.List
    ( List
    , pattern Nil
    , pattern Cons
    , fromSerial
    , toSerial
    )
where

import Control.Arrow (second)
import Control.DeepSeq (NFData(..), NFData1(..))
import Data.Functor.Identity (Identity, runIdentity)
import GHC.Exts (IsList(..), IsString(..))
import Text.Read (Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec,
                  readListPrecDefault)

import Streamly.Streams.Serial (SerialT)
import qualified Streamly.Streams.Prelude as P
import qualified Streamly.Streams.StreamK as K

-- We implement list as a newtype instead of a type synonym to make type
-- inference easier when using -XOverloadedLists and -XOverloadedStrings. When
-- using a stream type the programmer needs to specify the Monad otherwise the
-- type remains ambiguous.
--
-- | A better replacement for the standard lists in 'base'.
--
-- @since 0.5.3
newtype List a = List { unList :: SerialT Identity a }
    deriving (Eq, Ord, IsList, NFData, NFData1
             , Semigroup, Monoid, Functor, Applicative, Monad)

instance Show a => Show (List a) where {
    showsPrec p dl = showParen (p > 10) $
        showString "fromList " . shows (toList dl) };

instance Read a => Read (List a) where {
    readPrec = parens $ prec 10 $ do {
        Ident "fromList" <- lexP;
        dl <- readPrec;
        return (fromList dl) };
    readListPrec = readListPrecDefault };

instance (a ~ Char) => IsString (List a) where
    {-# INLINE fromString #-}
    fromString = List . P.fromList

------------------------------------------------------------------------------
-- Patterns
------------------------------------------------------------------------------

-- | An empty list constructor and pattern that matches an empty 'List'.
-- Corresponds to '[]' for Haskell lists.
--
-- @since 0.5.3
pattern Nil :: List a
pattern Nil <- (runIdentity . K.null . unList -> True) where
    Nil = List K.nil

-- | A list constructor and pattern that deconstructs a 'List' into its head
-- and tail. Corresponds to ':' for Haskell lists.
--
-- @since 0.5.3
pattern Cons :: a -> List a -> List a
pattern Cons x xs <-
    (fmap (second List) . runIdentity . K.uncons . unList
        -> Just (x, xs)) where
            Cons x xs = List $ K.cons x (unList xs)

------------------------------------------------------------------------------
-- Conversion to and from stream
------------------------------------------------------------------------------

-- We do not expose the newtype wrapper as that would require the show
-- instance to show it which makes show and read more complicated.

-- | Convert a 'Serial' stream to a 'List'.
--
-- @since 0.5.3
fromSerial :: SerialT Identity a -> List a
fromSerial = List

-- | Convert a 'List' to a 'Serial' stream.
--
-- @since 0.5.3
toSerial :: List a -> SerialT Identity a
toSerial = unList
