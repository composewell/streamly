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
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

{-
import Data.Proxy (Proxy)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.Unbox (MutableByteArray(..))
import GHC.Exts (Int(..), sizeofByteArray#, unsafeCoerce#)

import qualified Streamly.Internal.Data.Serialize.TH as Serialize

#if __GLASGOW_HASKELL__ >= 900
import GHC.Num.Integer (Integer(..))
#else
import GHC.Integer.GMP.Internals (Integer(..), BigNat(..))
#endif
-}

import Streamly.Internal.Data.Serialize.Type

{-

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

$(Serialize.deriveSerialize ''Maybe)
$(Serialize.deriveSerialize ''Either)
$(Serialize.deriveSerializeWith
      Serialize.defaultConfig
      [d|instance Serialize (Proxy a)|])

--------------------------------------------------------------------------------
-- Integer
--------------------------------------------------------------------------------

data LiftedInteger
    = LIS Int
    | LIP (Array Word)
    | LIN (Array Word)

$(Serialize.deriveSerialize ''LiftedInteger)

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
-}
