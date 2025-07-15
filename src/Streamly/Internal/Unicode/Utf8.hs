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
    , Display(..)
    , text

    -- * Combinators
    , length
    , snoc
    , unsnoc
    , head
    , last
    , tail
    , init
    , null
    , byteLength
    , singleton
    , empty
    , cons
    , uncons
    , toArray
    , read
    , create
    , fromStream
    , pack
    , unpack
    , concatM
    , concat
    , rightSizeM
    , rightSize

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

import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.MutArray (MutArray)
import Streamly.Data.Array (Array)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Data.Stream (Stream)
import Streamly.Data.Fold (Fold)
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
newtype Text = Text (MutArray Word8)

instance Semigroup Text where
    (<>) = append

instance Monoid Text where
    mempty = Text (Array.unsafeThaw Array.empty)
    mconcat = concat

{-# INLINE toArray #-}
toArray :: Text -> Array Word8
toArray t =
    unsafePerformIO $ do
        (Text arr) <- rightSizeM t
        pure $ Array.unsafeFreeze arr

{-# INLINE toMutArray #-}
toMutArray :: Text -> MutArray Word8
toMutArray (Text arr) = arr

--------------------------------------------------------------------------------
-- Class
--------------------------------------------------------------------------------

class Display a where
    display :: a -> Text

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
-- Streaming
--------------------------------------------------------------------------------

{-# INLINE read #-}
read :: Monad m => Text -> Stream m Char
read = Unicode.decodeUtf8 . Array.read . toArray

{-# INLINE ordM #-}
ordM :: MonadIO m => Char -> m (MutArray Word8)
ordM = Stream.fold (MutArray.createOf 4) . Stream.unfold Unicode.readCharUtf8'

{-# INLINEABLE appendM #-}
appendM :: MonadIO m => Text -> Text -> m Text
appendM (Text a) (Text b) = Text <$> MutArray.spliceExp a b

{-# INLINEABLE append #-}
append :: Text -> Text -> Text
append a b = unsafePerformIO $ appendM a b

{-# INLINE create #-}
create :: MonadIO m => Fold m Char Text
create =
    Fold.foldlM' MutArray.spliceExp (MutArray.emptyOf MutArray.blockSize)
        & Fold.lmapM ordM
        & fmap Text

{-# INLINE fromStream #-}
fromStream :: MonadIO m => Stream m Char -> m Text
fromStream = fmap Text . Stream.fold MutArray.create . Unicode.encodeUtf8'

--------------------------------------------------------------------------------
-- Creation and elimination
--------------------------------------------------------------------------------

{-# INLINEABLE pack #-}
pack :: String -> Text
pack = unsafePerformIO . fromStream . Stream.fromList

{-# INLINEABLE unpack #-}
unpack :: Text -> String
unpack = unsafePerformIO . Stream.fold Fold.toList . read

{-# INLINEABLE singleton #-}
singleton :: Char -> Text
singleton = unsafePerformIO . fmap Text . ordM

{-# INLINEABLE empty #-}
empty :: Text
empty = Text (Array.unsafeThaw Array.empty)

{-# INLINEABLE rightSizeM #-}
rightSizeM :: MonadIO m => Text -> m Text
rightSizeM (Text arr) = Text <$> MutArray.rightSize arr

{-# INLINEABLE rightSize #-}
rightSize :: Text -> Text
rightSize = unsafePerformIO . rightSizeM

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Display Text where
    display = id

instance Display String where
    display = pack

instance Display Int where
    display = pack . show

--------------------------------------------------------------------------------
-- QuasiQuoter
--------------------------------------------------------------------------------

text :: QuasiQuoter
text = strWith [|mconcat|] (appE [|pack|]) (appE [|display|])

--------------------------------------------------------------------------------
-- Basic interface
--------------------------------------------------------------------------------

{-# INLINEABLE cons #-}
cons :: Char -> Text -> Text
cons c s = append (singleton c) s

{-# INLINEABLE snoc #-}
snoc :: Text -> Char -> Text
snoc s c = append s (singleton c)

{-# INLINE uncons #-}
uncons :: Text -> Maybe (Char, Text)
uncons txt@(Text arr) = unsafePerformIO $ do
    let blen = MutArray.byteLength arr
    val <- Stream.fold Fold.one $ readEncoding txt
    pure $ case val of
        Just (Encoding {..}) ->
            Just ( eChar
                 , Text (MutArray.unsafeSliceOffLen eSize (blen - eSize) arr)
                 )
        Nothing -> Nothing

{-# INLINE unsnoc #-}
unsnoc :: Text -> Maybe (Text, Char)
unsnoc txt@(Text arr) = unsafePerformIO $ do
    let blen = MutArray.byteLength arr
    val <- Stream.fold Fold.one $ readEncodingRev txt
    pure $ case val of
        Just (Encoding {..}) ->
            Just ( Text (MutArray.unsafeSliceOffLen 0 (blen - eSize) arr)
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
null (Text arr) = MutArray.length arr == 0

{-# INLINEABLE length #-}
length :: Text -> Int
length = unsafePerformIO . Stream.fold Fold.length . read

{-# INLINEABLE byteLength #-}
byteLength :: Text -> Int
byteLength (Text arr) = MutArray.length arr

--------------------------------------------------------------------------------
-- Appending
--------------------------------------------------------------------------------

{-# INLINEABLE concatM #-}
concatM :: MonadIO m => Stream m Text -> m Text
concatM = fmap Text . MutArray.fromChunksRealloced . fmap toMutArray

{-# INLINEABLE concat #-}
concat :: [Text] -> Text
concat = unsafePerformIO . concatM . Stream.fromList

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

print :: Display a => a -> IO ()
print = Handle.putChunk stdout . toArray . display

printLn :: Display a => a -> IO ()
printLn = Handle.putChunk stdout . toArray . (<> pack "\n")  . display
