-- |
-- Module      : Streamly.Internal.Syscall.Common
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC

module Streamly.Internal.Syscall.Common
    ( retry
    , ioeAppendLocation
    , modifyIOErrorByType
    , modifyIOErrorString
    ) where

import System.IO.Error
    ( ioeGetLocation,
      ioeSetErrorString,
      ioeSetLocation,
      modifyIOError )

-------------------------------------------------------------------------------
-- Retry IO Action
-------------------------------------------------------------------------------

-- | Retry an IO action, modifying the argument on each Nothing result.
retry :: (a -> a) -> (a -> IO (Maybe b)) -> a -> IO b
retry modify f = go
    where
    go x = do
        r <- f x
        case r of
            Just v  -> return v
            Nothing -> go (modify x)

-------------------------------------------------------------------------------
-- IO Errors
-------------------------------------------------------------------------------

ioeMapLocation :: (String -> String) -> IOError -> IOError
ioeMapLocation f e =
    ioeSetLocation e (f (ioeGetLocation e))

ioeAppendLocation :: String -> IOError -> IOError
ioeAppendLocation loc =
    ioeMapLocation $ \oldLoc ->
        loc <> if null oldLoc then "" else ":" <> oldLoc

modifyIOErrorByType
  :: (IOError -> Bool)
  -> (IOError -> IOError)
  -> IO a
  -> IO a
modifyIOErrorByType check f =
    modifyIOError (\e -> if check e then f e else e)

modifyIOErrorString :: (IOError -> Bool) -> String -> IO a -> IO a
modifyIOErrorString errType str =
    modifyIOErrorByType errType (`ioeSetErrorString` str)
