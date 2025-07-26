{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Module      : Streamly.Internal.Unicode.Utf8
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

-- XXX We can move this to stream-core/streamly-unicode-core, and provide an
-- additional module in streamly-unicode for case conversions (because it
-- depends on unicode-data). Or just keep all of it in streamly-unicode
-- which will have a dependency on unicode-data.

module Streamly.Internal.Unicode.Utf8
    (
    -- * Type
      Text
    , TextB
    , Render(..)
    , text
    , build
    , force
    , force'
    , byteLength
    , toArray

    -- * Construction
    , snoc
    , singleton
    , cons
    , pack
    , concat
    , empty

    -- * Elimination
    , length
    , unsnoc
    , head
    , last
    , tail
    , init
    , null
    , uncons
    , read
    , unpack

    -- * Internals
    , readEncoding
    , readEncodingRev

    -- * IO
    , print
    , printLn
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad ((>=>))
import Data.Word (Word8)
import Streamly.Data.MutArray (MutArray)
import Streamly.Data.Array (Array)
import System.IO.Unsafe (unsafePerformIO)
import Streamly.Data.Stream (Stream)
import Language.Haskell.TH (appE)
import Language.Haskell.TH.Quote (QuasiQuoter)
import Streamly.Internal.Unicode.String (strWith)
import System.IO (stdout)

import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Prelude hiding
    (concat, print, read, length, head, last, tail, init, null)

--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------

-- | A space efficient, packed, unboxed Unicode container.
newtype Text = Text (Array Word8)

{-# INLINE toArray #-}
toArray :: Text -> Array Word8
toArray (Text arr) = arr

--------------------------------------------------------------------------------
-- Builder
--------------------------------------------------------------------------------

data TextB = TextB Int (MutArray Word8 -> IO (MutArray Word8))

byteLength :: TextB -> Int
byteLength (TextB i _) = i

build :: Array Word8 -> TextB
build t =
    TextB (Array.byteLength t) (`MutArray.unsafeSplice` (Array.unsafeThaw t))

force :: TextB -> Text
force (TextB i f) =
    unsafePerformIO
        $ MutArray.emptyOf i >>= fmap (Text . Array.unsafeFreeze) . f

force' :: TextB -> Text
force' (TextB i f) =
    unsafePerformIO
        $ MutArray.emptyOf' i >>= fmap (Text . Array.unsafeFreeze) . f

--------------------------------------------------------------------------------
-- Class
--------------------------------------------------------------------------------

class Render a where
    render :: a -> TextB

--------------------------------------------------------------------------------
-- Decoding Utils
--------------------------------------------------------------------------------

data Encoding = Encoding
    { eSize :: Int
    , eChar :: Char
    }

-- NOTE: Most slice based functions can be implemented using this.
{-# INLINE readEncoding #-}
readEncoding :: {- Monad m => -} Text -> Stream m Encoding
readEncoding = undefined

{-# INLINE readEncodingRev #-}
readEncodingRev :: {- Monad m => -} Text -> Stream m Encoding
readEncodingRev = undefined

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

{-# INLINEABLE pack #-}
pack :: String -> TextB
pack =
    build
        . unsafePerformIO
        . Stream.fold Array.create
        . Unicode.encodeUtf8'
        . Stream.fromList

{-# INLINEABLE append #-}
append :: TextB -> TextB -> TextB
append (TextB i f) (TextB j g) = TextB (i + j) (f >=> g)

{-# INLINE ord #-}
ord :: Char -> Array Word8
ord =
    unsafePerformIO
        . Stream.fold (Array.createOf 4)
        . Stream.unfold Unicode.readCharUtf8'

{-# INLINEABLE singleton #-}
singleton :: Char -> TextB
singleton = build . ord

{-# INLINEABLE empty #-}
empty :: TextB
empty = TextB 0 pure

{-# INLINEABLE cons #-}
cons :: Char -> TextB -> TextB
cons c s = append (singleton c) s

{-# INLINEABLE snoc #-}
snoc :: TextB -> Char -> TextB
snoc s c = append s (singleton c)

{-# INLINEABLE concat #-}
concat :: [TextB] -> TextB
concat = foldr mappend mempty

--------------------------------------------------------------------------------
-- Elimination
--------------------------------------------------------------------------------

{-# INLINE read #-}
read :: Monad m => Text -> Stream m Char
read = Unicode.decodeUtf8 . Array.read . toArray

{-# INLINEABLE unpack #-}
unpack :: Text -> String
unpack = unsafePerformIO . Stream.fold Fold.toList . read

--------------------------------------------------------------------------------
-- Elimination
--------------------------------------------------------------------------------

{-# INLINE uncons #-}
uncons :: Text -> Maybe (Char, Text)
uncons txt@(Text arr) = unsafePerformIO $ do
    let blen = Array.byteLength arr
    val <- Stream.fold Fold.one $ readEncoding txt
    pure $ case val of
        Just (Encoding {..}) ->
            Just ( eChar
                 , Text (Array.unsafeSliceOffLen eSize (blen - eSize) arr)
                 )
        Nothing -> Nothing

{-# INLINE unsnoc #-}
unsnoc :: Text -> Maybe (Text, Char)
unsnoc txt@(Text arr) = unsafePerformIO $ do
    let blen = Array.byteLength arr
    val <- Stream.fold Fold.one $ readEncodingRev txt
    pure $ case val of
        Just (Encoding {..}) ->
            Just ( Text (Array.unsafeSliceOffLen 0 (blen - eSize) arr)
                 , eChar
                 )
        Nothing -> Nothing

{-# INLINEABLE head #-}
head :: Text -> Maybe Char
head = fmap fst . uncons

{-# INLINEABLE last #-}
last :: Text -> Maybe Char
last = fmap snd . unsnoc

{-# INLINEABLE tail #-}
tail :: Text -> Maybe Text
tail = fmap snd . uncons

{-# INLINEABLE init #-}
init :: Text -> Maybe Text
init = fmap fst . unsnoc

{-# INLINEABLE null #-}
null :: Text -> Bool
null (Text arr) = Array.length arr == 0

{-# INLINEABLE length #-}
length :: Text -> Int
length = unsafePerformIO . Stream.fold Fold.length . read

--------------------------------------------------------------------------------
-- Text Instances
--------------------------------------------------------------------------------

instance Semigroup TextB where
    (<>) = append

instance Monoid TextB where
    mempty = empty
    mconcat = foldr mappend mempty

--------------------------------------------------------------------------------
-- Render Instances
--------------------------------------------------------------------------------

instance Render TextB where
    render = id

instance Render Text where
    render (Text arr) = build arr

instance Render String where
    render = pack

instance Render Int where
    render = pack . show

--------------------------------------------------------------------------------
-- QuasiQuoter
--------------------------------------------------------------------------------

text :: QuasiQuoter
text = strWith [|mconcat|] (appE [|render|]) (appE [|render|])

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

print :: Render a => a -> IO ()
print = Handle.putChunk stdout . toArray . force' . render

printLn :: Render a => a -> IO ()
printLn =
    Handle.putChunk stdout . toArray . force'. (<> singleton '\n') . render
