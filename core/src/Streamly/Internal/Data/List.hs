{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Streamly.Internal.Data.List
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Lists are just a special case of monadic streams. The stream type @Stream
-- Identity a@ can be used as a replacement for @[a]@.  The 'List' type in this
-- module is just a newtype wrapper around @Stream Identity@ for better type
-- inference when using the 'OverloadedLists' GHC extension. @List a@ provides
-- better performance compared to @[a]@. Standard list, string and list
-- comprehension syntax can be used with the 'List' type by enabling
-- 'OverloadedLists', 'OverloadedStrings' and 'MonadComprehensions' GHC
-- extensions.  There would be a slight difference in the 'Show' and 'Read'
-- strings of streamly list as compared to regular lists.
--
-- Conversion to stream types is free, any stream combinator can be used on
-- lists by converting them to streams.  However, for convenience, this module
-- provides combinators that work directly on the 'List' type.
--
--
-- @
-- List $ S.map (+ 1) $ toStream (1 \`Cons\` Nil)
-- @
--
-- To convert a 'List' to regular lists, you can use any of the following:
--
-- * @toList . toStream@ and @toStream . fromList@
-- * 'Data.Foldable.toList' from "Data.Foldable"
-- * 'GHC.Exts.toList' and 'GHC.Exts.fromList' from 'IsList' in "GHC.Exts"
--
-- If you have made use of 'Nil' and 'Cons' constructors in the code and you
-- want to replace streamly lists with standard lists, all you need to do is
-- import these definitions:
--
-- @
-- type List = []
-- pattern Nil <- [] where Nil = []
-- pattern Cons x xs = x : xs
-- infixr 5 `Cons`
-- {-\# COMPLETE Cons, Nil #-}
-- @
--
-- See <src/docs/streamly-vs-lists.md> for more details and
-- <src/test/PureStreams.hs> for comprehensive usage examples.
--
module Streamly.Internal.Data.List
    (
    List (.., Nil, Cons)

    -- XXX we may want to use rebindable syntax for variants instead of using
    -- different types (applicative do and apWith).
    , ZipList (..)
    , fromZipList
    , toZipList
    )
where

import Control.Arrow (second)
import Control.DeepSeq (NFData(..))
import Control.DeepSeq (NFData1(..))
import Data.Functor.Identity (Identity, runIdentity)
import GHC.Exts (IsList(..), IsString(..))
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Stream.Zip (ZipSerialM, ZipStream(..))

import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Stream.Type as Stream

-- We implement list as a newtype instead of a type synonym to make type
-- inference easier when using -XOverloadedLists and -XOverloadedStrings. When
-- using a stream type the programmer needs to specify the Monad otherwise the
-- type remains ambiguous.
--
-- XXX once we separate consM from IsStream or remove the MonadIO and
-- MonadBaseControlIO dependency from it, then we can make this an instance of
-- IsStream and use the regular polymorphic functions on Lists as well. Once
-- that happens we can change the Show and Read instances as well to use "1 >:
-- 2 >: nil" etc. or should we use a separate constructor indicating the "List"
-- type ":>" for better inference?
--
-- | @List a@ is a replacement for @[a]@.
--
-- /Pre-release/
newtype List a = List { toStream :: Stream Identity a }
    deriving
    ( Show, Read, Eq, Ord, NFData , NFData1
    , Semigroup, Monoid, Functor, Foldable
    , Applicative, Traversable, Monad)

instance (a ~ Char) => IsString (List a) where
    {-# INLINE fromString #-}
    fromString = List . fromList

-- GHC versions 8.0 and below cannot derive IsList
instance IsList (List a) where
    type (Item (List a)) = a
    {-# INLINE fromList #-}
    fromList = List . fromList
    {-# INLINE toList #-}
    toList = toList . toStream

------------------------------------------------------------------------------
-- Patterns
------------------------------------------------------------------------------

-- Note: When using the OverloadedLists extension we should be able to pattern
-- match using the regular list contructors. OverloadedLists uses 'toList' to
-- perform the pattern match, it should not be too bad as it works lazily in
-- the Identity monad. We need these patterns only when not using that
-- extension.
--
-- | An empty list constructor and pattern that matches an empty 'List'.
-- Corresponds to '[]' for Haskell lists.
--
-- @since 0.6.0
pattern Nil :: List a
pattern Nil <- (runIdentity . K.null . Stream.toStreamK . toStream -> True) where
    Nil = List $ Stream.fromStreamK K.nil

infixr 5 `Cons`

-- | A list constructor and pattern that deconstructs a 'List' into its head
-- and tail. Corresponds to ':' for Haskell lists.
--
-- @since 0.6.0
pattern Cons :: a -> List a -> List a
pattern Cons x xs <-
    (fmap (second (List . Stream.fromStreamK))
        . runIdentity . K.uncons . Stream.toStreamK . toStream
            -> Just (x, xs)
    )

    where

    Cons x xs = List $ Stream.cons x (toStream xs)

{-# COMPLETE Nil, Cons #-}

------------------------------------------------------------------------------
-- ZipList
------------------------------------------------------------------------------

-- | Just like 'List' except that it has a zipping 'Applicative' instance
-- and no 'Monad' instance.
--
-- @since 0.6.0
newtype ZipList a = ZipList { toZipSerial :: ZipSerialM Identity a }
    deriving
    ( Show, Read, Eq, Ord, NFData , NFData1
    , Semigroup, Monoid, Functor, Foldable
    , Applicative, Traversable
    )

instance (a ~ Char) => IsString (ZipList a) where
    {-# INLINE fromString #-}
    fromString = ZipList . fromList

-- GHC versions 8.0 and below cannot derive IsList
instance IsList (ZipList a) where
    type (Item (ZipList a)) = a
    {-# INLINE fromList #-}
    fromList = ZipList . fromList
    {-# INLINE toList #-}
    toList = toList . toZipSerial

-- | Convert a 'ZipList' to a regular 'List'
--
-- @since 0.6.0
fromZipList :: ZipList a -> List a
fromZipList (ZipList zs) = List $ getZipStream zs

-- | Convert a regular 'List' to a 'ZipList'
--
-- @since 0.6.0
toZipList :: List a -> ZipList a
toZipList = ZipList . ZipStream . toStream
