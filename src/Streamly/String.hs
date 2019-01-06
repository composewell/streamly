-- |
-- Module      : Streamly.String
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- The 'String' type in this module is just a synonym for the type @List Char@.
-- It provides better performance compared to the standard Haskell @String@
-- type and can be used almost as a drop-in replacement, especially when used
-- with @OverloadedStrings@ extension, with little differences.
--
-- See "Streamly.List", <src/docs/streamly-vs-lists.md> for more details and
-- <src/test/PureStreams.hs> for comprehensive usage examples.
--
--
module Streamly.String
    (
      String
    )
where

import Streamly.List (List)
import Prelude hiding (String)

type String = List Char

{-
-- XXX we can have our own Handle type and use our own routines to create
-- Handles. We do not need any of the bagagge of GHC Handles, we just need a
-- way to read and write bytes, with no buffering or no encoding/decoding.
--
-------------------------------------------------------------------------------
-- Encoding/Decoding Characters
-------------------------------------------------------------------------------

-- Handles are always binary mode and perform no encoding or decoding on their
-- own. Encoding/decoding is an explicit transformation on a stream.
--
-- We read byte streams from file handles. Byte stream can be decoded into a
-- Char stream using an appropriate decoder. The Char streams are the
-- equivalent of Haskell Strings. We can use usual text/string processing
-- routines on these streams. Processed streams can finally be encoded and then
-- written to a handle.

-- decodeWith :: TextEncoding -> t m Word8 -> t m Char
decodeUtf8 :: t m Word8 -> t m Char

-------------------------------------------------------------------------------
-- Character IO
-------------------------------------------------------------------------------

-- XXX we should not need these at all. We should be processing
{-
hGetChar
hPutChar
hPutStr
hPrint = liftIO . IO.hPrint

-- Standard input/output
getChar
putChar
putStr
-}

-------------------------------------------------------------------------------
-- Line IO
-------------------------------------------------------------------------------

-- lines :: t m Char -> t m (t m Char)
-- unlines :: t m (t m Char) -> t m Char
-- linesBy :: NewLine -> t m Char -> t m (t m Char)
-- unlinesBy :: NewLine -> t m (t m Char) -> t m Char

-- XXX define fromHandle/fromHandleLn in Streamly.String
-- Also define encode/decodeUtf8 there
-- As well as decode/encode for auto decode and encode based on environment
--
-- XXX this should go in Streamly.String, this will require decoding to be
-- performed. We will need a fromHandleChar as a result of decoding.
-- perhaps a separate streamly-string package that will include normalized
-- strings as well.
--
-- fromHandleLn :: (IsStream t, MonadIO m) => IO.Handle -> t m (t m Char)
-- fromHandleLn h = go

-- XXX automatically use LineBuffering when we are using line based routines.
-- The buffering can be implemented in the stream itself instead of the handle?
-- implementing in the stream means we can use the same thing for everything
-- e.g. even on a socket.
--
hPutLine =
hGetLine =

{-
--
-- | Read lines from an IO Handle into a stream of Strings.
--
-- @since 0.1.0
{-# DEPRECATED fromHandle "Please use \"map lines fromHandleChar\" instead." #-}
hGetLines :: (IsStream t, MonadIO m) => IO.Handle -> t m String
hGetLines h = go

-- |
-- @
-- toHandle h = S.mapM_ $ hPutStrLn h
-- @
--
-- Write a stream of Strings to an IO Handle.
--
-- @since 0.1.0
hPutLines :: MonadIO m => IO.Handle -> SerialT m String -> m ()
hPutLines h m = go m

-- stdin/stdout
putLine =
getLine =

-- getLinesN
-- putLinesN

-- hGetLineBy
-- hGetLinesBy
--
-- hPutLineBy
-- hPutLinesBy
-- -}
-}
