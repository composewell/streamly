-- |
-- Module      : Streamly.Internal.System.IOVec
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Low level IO routines interfacing the operating system.
--

module Streamly.Internal.System.IOVec
    ( IOVec(..)
    , c_writev
    , c_safe_writev
#if !defined(mingw32_HOST_OS)
{-
    , groupIOVecsOf
    , groupIOVecsOfMut
-}
#endif
    )
where

#include "inline.hs"

#if !defined(mingw32_HOST_OS)
{-
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Ptr (castPtr)
import Streamly.Internal.Data.MutArray (length)
import Streamly.Internal.Data.SVar.Type (adaptState)
import Streamly.Internal.Data.MutArray (Array(..))

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray.Type as MArray
import qualified Streamly.Internal.Data.Stream as D
-}
#endif

import Streamly.Internal.System.IOVec.Type

import Prelude hiding (length)

#if !defined(mingw32_HOST_OS)
{-
data GatherState s arr
    = GatherInitial s
    | GatherBuffering s arr Int
    | GatherYielding arr (GatherState s arr)
    | GatherFinish

-- | @groupIOVecsOf maxBytes maxEntries@ groups arrays in the incoming stream
-- to create a stream of 'IOVec' arrays with a maximum of @maxBytes@ bytes in
-- each array and a maximum of @maxEntries@ entries in each array.
--
-- @since 0.7.0
{-# INLINE_NORMAL groupIOVecsOfMut #-}
groupIOVecsOfMut :: MonadIO m
    => Int -> Int -> D.Stream m (Array a) -> D.Stream m (Array IOVec)
groupIOVecsOfMut n maxIOVLen (D.Stream step state) =
    D.Stream step' (GatherInitial state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (GatherInitial st) = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.MutArray.Type.groupIOVecsOf: the size of "
                 ++ "groups [" ++ show n ++ "] must be a natural number"
        when (maxIOVLen <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.MutArray.Type.groupIOVecsOf: the number of "
                 ++ "IOVec entries [" ++ show n ++ "] must be a natural number"
        r <- step (adaptState gst) st
        case r of
            D.Yield arr s -> do
                let p = arrStart arr
                    len = MArray.byteLength arr
                iov <- liftIO $ MArray.newArray maxIOVLen
                iov' <- liftIO $ MArray.snocUnsafe iov (IOVec (castPtr p)
                                                (fromIntegral len))
                if len >= n
                then return $ D.Skip (GatherYielding iov' (GatherInitial s))
                else return $ D.Skip (GatherBuffering s iov' len)
            D.Skip s -> return $ D.Skip (GatherInitial s)
            D.Stop -> return D.Stop

    step' gst (GatherBuffering st iov len) = do
        r <- step (adaptState gst) st
        case r of
            D.Yield arr s -> do
                let p = arrStart arr
                    alen = MArray.byteLength arr
                    len' = len + alen
                if len' > n || length iov >= maxIOVLen
                then do
                    iov' <- liftIO $ MArray.newArray maxIOVLen
                    iov'' <- liftIO $ MArray.snocUnsafe iov' (IOVec (castPtr p)
                                                      (fromIntegral alen))
                    return $ D.Skip (GatherYielding iov
                                        (GatherBuffering s iov'' alen))
                else do
                    iov' <- liftIO $ MArray.snocUnsafe iov (IOVec (castPtr p)
                                                    (fromIntegral alen))
                    return $ D.Skip (GatherBuffering s iov' len')
            D.Skip s -> return $ D.Skip (GatherBuffering s iov len)
            D.Stop -> return $ D.Skip (GatherYielding iov GatherFinish)

    step' _ GatherFinish = return D.Stop

    step' _ (GatherYielding iov next) = return $ D.Yield iov next

-- | @groupIOVecsOf maxBytes maxEntries@ groups arrays in the incoming stream
-- to create a stream of 'IOVec' arrays with a maximum of @maxBytes@ bytes in
-- each array and a maximum of @maxEntries@ entries in each array.
--
-- @since 0.7.0
{-# INLINE_NORMAL groupIOVecsOf #-}
groupIOVecsOf :: MonadIO m
    => Int
    -> Int
    -> D.Stream m (Array.Array a) -> D.Stream m (Array.Array IOVec)
groupIOVecsOf n maxIOVLen str =
    D.map Array.unsafeFreeze
        $ groupIOVecsOfMut n maxIOVLen
        $ D.map Array.unsafeThaw str
-}
#endif
