module Streamly.Internal.FileSystem.Utils where

openFileWith
  :: Path     -- ^ file to open
  -> IOMode   -- ^ mode in which to open the file
  -> Bool     -- ^ open the file in non-blocking mode?
              --   This has no effect on regular files and block devices:
              --   they are always opened in blocking mode.
              --   See 'fdIsNonBlocking' for more discussion.
  -> (FD -> IODeviceType -> IO r) -- ^ @act1@: An action to perform
                    -- on the file descriptor with the masking state
                    -- restored and an exception handler that closes
                    -- the file on exception.
  -> ((forall x. IO x -> IO x) -> r -> IO s)
                    -- ^ @act2@: An action to perform with async exceptions
                    -- masked and no exception handler.
  -> IO s
openFileWith filepath iomode non_blocking act1 act2 =
  withFilePath filepath $ \ f ->
    let
      oflags1 = case iomode of
                  ReadMode      -> read_flags
                  WriteMode     -> write_flags
                  ReadWriteMode -> rw_flags
                  AppendMode    -> append_flags

#if defined(mingw32_HOST_OS)
      binary_flags = o_BINARY
#else
      binary_flags = 0
#endif

      oflags2 = oflags1 .|. binary_flags

      oflags | non_blocking = oflags2 .|. nonblock_flags
             | otherwise    = oflags2
    in do
      -- We want to be sure all the arguments to c_interruptible_open
      -- are fully evaluated *before* it slips under a mask (assuming we're
      -- not already under a user-imposed mask).
      oflags' <- evaluate oflags
      -- NB. always use a safe open(), because we don't know whether open()
      -- will be fast or not.  It can be slow on NFS and FUSE filesystems,
      -- for example.

      mask $ \restore -> do
        fileno <- throwErrnoIfMinus1Retry "openFile" $
                c_interruptible_open f oflags' 0o666

        (fD,fd_type) <- mkFD fileno iomode Nothing{-no stat-}
                                False{-not a socket-}
                                non_blocking `onException` c_close fileno

        -- we want to truncate() if this is an open in WriteMode, but only
        -- if the target is a RegularFile.  ftruncate() fails on special files
        -- like /dev/null.

        when (iomode == WriteMode && fd_type == RegularFile) $
          setSize fD 0 `onException` close fD

        carry <- restore (act1 fD fd_type) `onException` close fD

        act2 restore carry
