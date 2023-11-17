{-# LANGUAGE TemplateHaskell #-}

-- This is required as all the instances in this module are orphan instances.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Streamly.Internal.Data.Serialize
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--

module Streamly.Internal.Data.Serialize
    ( module Streamly.Internal.Data.Serialize.Type
    , module Streamly.Internal.Data.Unbox.TH
    , module Streamly.Internal.Data.Serialize.TH
    , module Streamly.Internal.Data.Serialize.TH.RecHeader
    , module Streamly.Internal.Data.Serialize.TH.Common
    , module Streamly.Internal.Data.Serialize.TH.Bottom
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Proxy (Proxy(..))
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.Unbox (MutableByteArray(..))
import GHC.Exts (Int(..), sizeofByteArray#, unsafeCoerce#)
import GHC.Word (Word8)

#if __GLASGOW_HASKELL__ >= 900
import GHC.Num.Integer (Integer(..))
#else
import GHC.Integer.GMP.Internals (Integer(..), BigNat(..))
#endif

import Streamly.Internal.Data.Serialize.Type
import Streamly.Internal.Data.Unbox.TH
import Streamly.Internal.Data.Serialize.TH
import Streamly.Internal.Data.Serialize.TH.RecHeader
import Streamly.Internal.Data.Serialize.TH.Common
import Streamly.Internal.Data.Serialize.TH.Bottom

--------------------------------------------------------------------------------
-- Common instances
--------------------------------------------------------------------------------

-- Note
-- ====
--
-- Even a non-functional change such as changing the order of constructors will
-- change the instance derivation.
--
-- This will not pose a problem if both, encode, and decode are done by the same
-- version of the application. There *might* be a problem if version that
-- encodes differs from the version that decodes.
--
-- We need to add some compatibility tests using different versions of
-- dependencies.
--
-- Although such chages for the most basic types won't happen we need to detect
-- if it ever happens.
--
-- Should we worry about these kind of changes and this kind of compatibility?
-- This is a problem for all types of derivations that depend on the order of
-- constructors, for example, Enum.

-- Note on Windows build
-- =====================
--
-- On Windows, having template haskell splices here fail the build with the
-- following error:
--
-- @
-- addLibrarySearchPath: C:\...  (Win32 error 3): The system cannot find the path specified.
-- @
--
-- The error might be irrelavant but having these splices triggers it. We should
-- either fix the problem or avoid the use to template haskell splices in this
-- file.
--
-- Similar issue: https://github.com/haskell/cabal/issues/4741

-- $(Serialize.deriveSerialize ''Maybe)
instance Serialize a => Serialize (Maybe a) where

    {-# INLINE size #-}
    size acc x =
        case x of
            Nothing -> (acc + 1)
            Just field0 -> (size (acc + 1)) field0

    {-# INLINE deserialize #-}
    deserialize initialOffset arr endOffset = do
        (i0, tag) <- ((deserialize initialOffset) arr) endOffset
        case tag :: Word8 of
            0 -> pure (i0, Nothing)
            1 -> do (i1, a0) <- ((deserialize i0) arr) endOffset
                    pure (i1, Just a0)
            _ -> error "Found invalid tag while peeking (Maybe a)"

    {-# INLINE serialize #-}
    serialize initialOffset arr val =
        case val of
            Nothing -> do
                i0 <- ((serialize initialOffset) arr) (0 :: Word8)
                pure i0
            Just field0 -> do
                i0 <- ((serialize initialOffset) arr) (1 :: Word8)
                i1 <- ((serialize i0) arr) field0
                pure i1

-- $(Serialize.deriveSerialize ''Either)
instance (Serialize a, Serialize b) => Serialize (Either a b) where

    {-# INLINE size #-}
    size acc x =
        case x of
            Left field0 -> (size (acc + 1)) field0
            Right field0 -> (size (acc + 1)) field0

    {-# INLINE deserialize #-}
    deserialize initialOffset arr endOffset = do
        (i0, tag) <- ((deserialize initialOffset) arr) endOffset
        case tag :: Word8 of
            0 -> do (i1, a0) <- ((deserialize i0) arr) endOffset
                    pure (i1, Left a0)
            1 -> do (i1, a0) <- ((deserialize i0) arr) endOffset
                    pure (i1, Right a0)
            _ -> error "Found invalid tag while peeking (Either a b)"

    {-# INLINE serialize #-}
    serialize initialOffset arr val =
        case val of
            Left field0 -> do
                i0 <- ((serialize initialOffset) arr) (0 :: Word8)
                i1 <- ((serialize i0) arr) field0
                pure i1
            Right field0 -> do
                i0 <- ((serialize initialOffset) arr) (1 :: Word8)
                i1 <- ((serialize i0) arr) field0
                pure i1

instance Serialize (Proxy a) where

    {-# INLINE size #-}
    size acc _ = (acc + 1)

    {-# INLINE deserialize #-}
    deserialize initialOffset _ _ = pure ((initialOffset + 1), Proxy)

    {-# INLINE serialize #-}
    serialize initialOffset _ _ = pure (initialOffset + 1)

--------------------------------------------------------------------------------
-- Integer
--------------------------------------------------------------------------------

data LiftedInteger
    = LIS Int
    | LIP (Array Word)
    | LIN (Array Word)

-- $(Serialize.deriveSerialize ''LiftedInteger)
instance Serialize LiftedInteger where

    {-# INLINE size #-}
    size acc x =
        case x of
            LIS field0 -> (size (acc + 1)) field0
            LIP field0 -> (size (acc + 1)) field0
            LIN field0 -> (size (acc + 1)) field0

    {-# INLINE deserialize #-}
    deserialize initialOffset arr endOffset = do
        (i0, tag) <- ((deserialize initialOffset) arr) endOffset
        case tag :: Word8 of
            0 -> do (i1, a0) <- ((deserialize i0) arr) endOffset
                    pure (i1, LIS a0)
            1 -> do (i1, a0) <- ((deserialize i0) arr) endOffset
                    pure (i1, LIP a0)
            2 -> do (i1, a0) <- ((deserialize i0) arr) endOffset
                    pure (i1, LIN a0)
            _ -> error "Found invalid tag while peeking (LiftedInteger)"

    {-# INLINE serialize #-}
    serialize initialOffset arr val =
        case val of
            LIS field0 -> do
                i0 <- ((serialize initialOffset) arr) (0 :: Word8)
                i1 <- ((serialize i0) arr) field0
                pure i1
            LIP field0 -> do
                i0 <- ((serialize initialOffset) arr) (1 :: Word8)
                i1 <- ((serialize i0) arr) field0
                pure i1
            LIN field0 -> do
                i0 <- ((serialize initialOffset) arr) (2 :: Word8)
                i1 <- ((serialize i0) arr) field0
                pure i1

#if __GLASGOW_HASKELL__ >= 900

{-# INLINE liftInteger #-}
liftInteger :: Integer -> LiftedInteger
liftInteger (IS x) = LIS (I# x)
liftInteger (IP x) =
    LIP (Array (MutableByteArray (unsafeCoerce# x)) 0 (I# (sizeofByteArray# x)))
liftInteger (IN x) =
    LIN (Array (MutableByteArray (unsafeCoerce# x)) 0 (I# (sizeofByteArray# x)))

{-# INLINE unliftInteger #-}
unliftInteger :: LiftedInteger -> Integer
unliftInteger (LIS (I# x)) = IS x
unliftInteger (LIP (Array (MutableByteArray x) _ _)) = IP (unsafeCoerce# x)
unliftInteger (LIN (Array (MutableByteArray x) _ _)) = IN (unsafeCoerce# x)

#else

{-# INLINE liftInteger #-}
liftInteger :: Integer -> LiftedInteger
liftInteger (S# x) = LIS (I# x)
liftInteger (Jp# (BN# x)) =
    LIP (Array (MutableByteArray (unsafeCoerce# x)) 0 (I# (sizeofByteArray# x)))
liftInteger (Jn# (BN# x)) =
    LIN (Array (MutableByteArray (unsafeCoerce# x)) 0 (I# (sizeofByteArray# x)))

{-# INLINE unliftInteger #-}
unliftInteger :: LiftedInteger -> Integer
unliftInteger (LIS (I# x)) = S# x
unliftInteger (LIP (Array (MutableByteArray x) _ _)) =
    Jp# (BN# (unsafeCoerce# x))
unliftInteger (LIN (Array (MutableByteArray x) _ _)) =
    Jn# (BN# (unsafeCoerce# x))

#endif

instance Serialize Integer where
    {-# INLINE size #-}
    size i a = size i (liftInteger a)

    {-# INLINE deserialize #-}
    deserialize off arr end = fmap unliftInteger <$> deserialize off arr end

    {-# INLINE serialize #-}
    serialize off arr val = serialize off arr (liftInteger val)
