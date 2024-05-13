{-# LANGUAGE CPP #-}

module Streamly.Internal.Unicode.UTF8
    (module Streamly.Internal.Unicode.UTF8) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Stream (Stream)
import System.IO.Unsafe (unsafePerformIO)
import Data.Functor.Identity (Identity(..))
import GHC.Exts (Addr#)

import qualified Data.List as L
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.MutArray as MArray
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.StreamK as StreamK
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Unicode.Stream as Unicode
import qualified Unicode.Char as Char

import qualified Streamly.Internal.Data.StreamK as StreamK (concatMap)
import qualified Streamly.Internal.Data.Array as Array
    (unsafeFreeze, unsafeThaw, empty, getSliceUnsafe, fromChunks, fromByteStr#)
import qualified Streamly.Internal.Data.MutArray as MArray
    (spliceExp, blockSize, spliceUnsafe)
import qualified Streamly.Internal.Unicode.Stream as Unicode
    ( readCharUtf8', decodeUtf8Indexed, decodeUtf8ReverseIndexed
    , CodingFailureMode(..), fromStr#
    )
import qualified Unicode.Internal.Unfold as UUnfold (toList)
import qualified Streamly.Internal.Data.Stream as Stream (splitOnSeqWith)

import Prelude as P hiding (read, length, all, dropWhile)

--------------------------------------------------------------------------------
-- CPP
--------------------------------------------------------------------------------

#define INLINEABLE(x) {-# INLINEABLE x #-};x
#define UNIMPLEMENTED(x) x = error "The functions x is unimplemented"

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | A space efficient, packed, unboxed Unicode container.
newtype UTF8 =
    UTF8 { unUTF8 :: Array Word8 }

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

{-# INLINE read #-}
read :: Monad m => UTF8 -> Stream m Char
read (UTF8 arr) = Unicode.decodeUtf8' $ Array.read arr

{-# INLINE ordM #-}
ordM :: MonadIO m => Char -> m (Array Word8)
ordM =
    Stream.fold (Array.createOf 4)
        . Stream.unfold Unicode.readCharUtf8'

INLINEABLE(appendM) :: MonadIO m => UTF8 -> UTF8 -> m UTF8
appendM (UTF8 a) (UTF8 b) =
    fmap (UTF8 . Array.unsafeFreeze)
        $ MArray.spliceExp (Array.unsafeThaw a) (Array.unsafeThaw b)

INLINEABLE(append) :: UTF8 -> UTF8 -> UTF8
append a b = unsafePerformIO $ appendM a b

{-# INLINE createOf #-}
createOf :: MonadIO m => Int -> Fold m Char UTF8
createOf i =
    Fold.foldlM' MArray.spliceExp (MArray.emptyOf i)
        & Fold.lmapM (fmap Array.unsafeThaw . ordM)
        & fmap (UTF8 . Array.unsafeFreeze)

{-# INLINE create #-}
create :: MonadIO m => Fold m Char UTF8
create = createOf MArray.blockSize

INLINEABLE(fromStream) :: MonadIO m => Stream m Char -> m UTF8
fromStream strm =
    Unicode.encodeUtf8' strm
        & Stream.fold Array.create
        & fmap UTF8

INLINEABLE(fromStreamOf) :: MonadIO m => Int -> Stream m Char -> m UTF8
fromStreamOf n strm =
    Unicode.encodeUtf8' strm
        & Stream.fold (Array.createOf n)
        & fmap UTF8

--------------------------------------------------------------------------------
-- Creation and elimination
--------------------------------------------------------------------------------

INLINEABLE(pack) :: String -> UTF8
pack s = unsafePerformIO $ fromStreamOf (L.length s) $ Stream.fromList s

INLINEABLE(unpack) :: UTF8 -> String
unpack text = unsafePerformIO $ Stream.fold Fold.toList $ read text

INLINEABLE(singleton) :: Char -> UTF8
singleton c = UTF8 $ unsafePerformIO $ ordM c

INLINEABLE(empty) :: UTF8
empty = UTF8 Array.empty

--------------------------------------------------------------------------------
-- Basic interface
--------------------------------------------------------------------------------

INLINEABLE(cons) :: Char -> UTF8 -> UTF8
cons c s = append (singleton c) s

INLINEABLE(snoc) :: UTF8 -> Char -> UTF8
snoc s c = append s (singleton c)

INLINEABLE(uncons) :: UTF8 -> Maybe (Char, UTF8)
uncons (UTF8 arr) = unsafePerformIO $ do
    res <-
        Array.read arr
            & Unicode.decodeUtf8Indexed Unicode.ErrorOnCodingFailure
            & Stream.fold Fold.one
    case res of
        Just ((_, i), c) ->
            let txt = UTF8 $ Array.getSliceUnsafe i (Array.length arr - i) arr
             in pure $ Just (c, txt)
        Nothing -> pure Nothing

INLINEABLE(unsnoc) :: UTF8 -> Maybe (Char, UTF8)
unsnoc (UTF8 arr) = unsafePerformIO $ do
    res <-
        Array.readRev arr
            & Unicode.decodeUtf8ReverseIndexed Unicode.ErrorOnCodingFailure
            & Stream.fold Fold.one
    case res of
        Just ((_, i), c) ->
            let txt = UTF8 $ Array.getSliceUnsafe 0 (Array.length arr - i) arr
             in pure $ Just (c, txt)
        Nothing -> pure Nothing

INLINEABLE(head) :: UTF8 -> Maybe Char
head = fmap fst . uncons

INLINEABLE(last) :: UTF8 -> Maybe Char
last = fmap fst . unsnoc

INLINEABLE(tail) :: UTF8 -> Maybe UTF8
tail = fmap snd . uncons

INLINEABLE(init) :: UTF8 -> Maybe UTF8
init = fmap snd . unsnoc

INLINEABLE(null) :: UTF8 -> Bool
null (UTF8 arr) = Array.length arr == 0

INLINEABLE(length) :: UTF8 -> Int
length text = unsafePerformIO $ Stream.fold Fold.length $ read text

INLINEABLE(byteLength) :: UTF8 -> Int
byteLength (UTF8 arr) = Array.length arr

INLINEABLE(compareLength) :: UTF8 -> Int -> Ordering
compareLength text i = unsafePerformIO $ do
    res <-
        read text
            & Stream.scan Fold.length
            & Stream.fold (Fold.takeEndBy (<= i + 1) Fold.latest)
    pure
        $ case res of
              Nothing -> LT
              Just j -> compare j i

--------------------------------------------------------------------------------
-- Transformation
--------------------------------------------------------------------------------

INLINEABLE(map) :: (Char -> Char) -> UTF8 -> UTF8
map f text = unsafePerformIO $ do
    read text
        & fmap f
        & Stream.fold create

INLINEABLE(intercalate) :: UTF8 -> [UTF8] -> UTF8
intercalate seed inp =
    Stream.intercalate
        (Unfold.function id)
        (unUTF8 seed)
        (fmap unUTF8 (Stream.fromList inp))
        & Array.fromChunks
        & fmap UTF8
        & unsafePerformIO

INLINEABLE(intersperse) :: Char -> UTF8 -> UTF8
intersperse c text =
    read text
        & Stream.intersperse c
        & Stream.fold create
        & unsafePerformIO

-- XXX This is a naive implementation to get things to work.
INLINEABLE(transpose) :: [UTF8] -> [UTF8]
transpose ts = P.map pack (L.transpose (P.map unpack ts))

INLINEABLE(reverse) :: UTF8 -> UTF8
reverse (UTF8 arr) =
    Array.readRev arr
        & Unicode.decodeUtf8ReverseIndexed Unicode.ErrorOnCodingFailure
        & fmap fst
        & Stream.fold copyFold
        & fmap (UTF8 . Array.unsafeFreeze)
        & unsafePerformIO

    where

    copyFold =
        Fold.foldrM'
            (\(o, l) b ->
                 MArray.spliceUnsafe b
                     (Array.unsafeThaw (Array.getSliceUnsafe o l arr)))
            (MArray.emptyOf (Array.length arr))

-- Use Boyer–Moore for strict pattern replacement?
-- Check perf and decide.
INLINEABLE(replace)
    :: UTF8 -- ^ needle to search for. If this string is empty, an
            -- error will occur.
    -> UTF8 -- ^ replacement to replace needle with.
    -> UTF8 -- ^ haystack in which to search.
    -> UTF8
UNIMPLEMENTED(replace)
{-
replace (UTF8 needle) (UTF8 replacement) (UTF8 haystack) =
    Stream.splitOnSeq needle replicate (Array.read haystack)
        & fromStream & unsafePerformIO
-}

--------------------------------------------------------------------------------
-- Case conversion
--------------------------------------------------------------------------------

INLINEABLE(toCaseFold) :: UTF8 -> UTF8
toCaseFold text =
    read text
        & fmap (UUnfold.toList Char.caseFoldMapping)
        & Stream.unfoldMany Unfold.fromList
        & fromStream & unsafePerformIO

INLINEABLE(toLower) :: UTF8 -> UTF8
toLower text =
    read text
        & fmap (UUnfold.toList Char.lowerCaseMapping)
        & Stream.unfoldMany Unfold.fromList
        & fromStream & unsafePerformIO

INLINEABLE(toUpper) :: UTF8 -> UTF8
toUpper text =
    read text
        & fmap (UUnfold.toList Char.upperCaseMapping)
        & Stream.unfoldMany Unfold.fromList
        & fromStream & unsafePerformIO

INLINEABLE(toTitle) :: UTF8 -> UTF8
toTitle text =
    read text
        & fmap (UUnfold.toList Char.titleCaseMapping)
        & Stream.unfoldMany Unfold.fromList
        & fromStream & unsafePerformIO

--------------------------------------------------------------------------------
-- Justification
--------------------------------------------------------------------------------

INLINEABLE(justifyLeft) :: Int -> Char -> UTF8 -> UTF8
justifyLeft i c text@(UTF8 arr) = unsafePerformIO $ do
    len <- Stream.fold Fold.length $ read text
    (UTF8 padding) <- fromStream $ Stream.fromPure c
    new0 <- MArray.emptyOf (Array.length arr + (Array.length padding) * i)
    if i > len
    then do
        new1 <-
            Stream.replicate (i - len) (Array.unsafeThaw padding)
                & Stream.fold (Fold.foldrM' MArray.spliceUnsafe (pure new0))
        new2 <- MArray.spliceUnsafe new1 (Array.unsafeThaw arr)
        pure $ UTF8 $ Array.unsafeFreeze new2
    else pure text

INLINEABLE(justifyRight) :: Int -> Char -> UTF8 -> UTF8
justifyRight i c text@(UTF8 arr) = unsafePerformIO $ do
    len <- Stream.fold Fold.length $ read text
    (UTF8 padding) <- fromStream $ Stream.fromPure c
    new0 <- MArray.emptyOf (Array.length arr + (Array.length padding) * i)
    if i > len
    then do
        new1 <- MArray.spliceUnsafe new0 (Array.unsafeThaw arr)
        new2 <-
            Stream.replicate (i - len) (Array.unsafeThaw padding)
                & Stream.fold (Fold.foldrM' MArray.spliceUnsafe (pure new1))
        pure $ UTF8 $ Array.unsafeFreeze new2
    else pure text

INLINEABLE(center) :: Int -> Char -> UTF8 -> UTF8
center i c text@(UTF8 arr) = unsafePerformIO $ do
    len <- Stream.fold Fold.length $ read text
    (UTF8 padding) <- fromStreamOf 4 (Stream.fromPure c)
    new0 <- MArray.emptyOf (Array.length arr + 4 * i)
    let paddingLenL = (i - len) `div` 2
        paddingLenR = i - len - paddingLenL
    if i > len
    then do
        new1 <-
            Stream.replicate paddingLenL (Array.unsafeThaw padding)
                & Stream.fold (Fold.foldrM' MArray.spliceUnsafe (pure new0))
        new2 <- MArray.spliceUnsafe new1 (Array.unsafeThaw arr)
        new3 <-
            Stream.replicate paddingLenR (Array.unsafeThaw padding)
                & Stream.fold (Fold.foldrM' MArray.spliceUnsafe (pure new2))
        pure $ UTF8 $ Array.unsafeFreeze new3
    else pure text

--------------------------------------------------------------------------------
-- Folds
--------------------------------------------------------------------------------

INLINEABLE(fold) :: MonadIO m => Fold m Char b -> UTF8 -> m b
fold f text = Stream.fold f $ read text

INLINEABLE(concat) :: [UTF8] -> UTF8
concat =
    unsafePerformIO
        . fmap UTF8 . Array.fromChunks . fmap unUTF8 . Stream.fromList

INLINEABLE(concatMap) :: (Char -> UTF8) -> UTF8 -> UTF8
concatMap f0 text =
    read text
        & StreamK.fromStream
        & StreamK.concatMap f
        & StreamK.toStream
        & fromStream
        & unsafePerformIO
    where
    f c = StreamK.fromStream $ read (f0 c)

INLINEABLE(any) :: (Char -> Bool) -> UTF8 -> Bool
any f inp =
    read inp
        & Stream.fold (Fold.any f)
        & unsafePerformIO

INLINEABLE(all) :: (Char -> Bool) -> UTF8 -> Bool
all f inp =
    read inp
        & Stream.fold (Fold.all f)
        & unsafePerformIO

INLINEABLE(maximum) :: UTF8 -> Maybe Char
maximum inp =
    read inp
        & Stream.fold Fold.maximum
        & unsafePerformIO

INLINEABLE(minumum) :: UTF8 -> Maybe Char
minumum inp =
    read inp
        & Stream.fold Fold.minimum
        & unsafePerformIO

INLINEABLE(isAscii) :: UTF8 -> Bool
isAscii = all Char.isAscii

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Scans
--------------------------------------------------------------------------------

INLINEABLE(scan) :: Fold Identity Char Char -> UTF8 -> UTF8
scan fld text =
    read text
        & Stream.scan fld
        & Stream.fold (Fold.morphInner (Identity . unsafePerformIO) create)
        & runIdentity

INLINEABLE(mapAccum) :: Fold Identity Char (a, Char) -> UTF8 -> (Maybe a, UTF8)
mapAccum fld text =
    read text
        & Stream.scan fld
        & Stream.fold
              (Fold.tee
                   (Fold.lmap fst Fold.latest)
                   (Fold.lmap snd
                        (Fold.morphInner (Identity . unsafePerformIO) create)))
        & runIdentity

--------------------------------------------------------------------------------
-- Unfolding
--------------------------------------------------------------------------------

INLINEABLE(replicate) :: Int -> UTF8 -> UTF8
replicate i (UTF8 arr) =
    Stream.replicate i (Array.unsafeThaw arr)
        & Stream.fold
              (Fold.foldrM' MArray.spliceExp (MArray.emptyOf MArray.blockSize))
        & fmap (UTF8 . Array.unsafeFreeze)
        & unsafePerformIO

INLINEABLE(unfoldr) :: (s -> Maybe (Char, s)) -> s -> UTF8
unfoldr f s =
    Stream.unfoldr f s
        & fromStream & unsafePerformIO

INLINEABLE(unfoldrN) :: Int -> (s -> Maybe (Char, s)) -> s -> UTF8
unfoldrN i f s =
    Stream.unfoldr f s
        & fromStreamOf (4 * i) & unsafePerformIO

--------------------------------------------------------------------------------
-- Breaking
--------------------------------------------------------------------------------

INLINEABLE(take) :: Int -> UTF8 -> UTF8
take i text@(UTF8 arr) = unsafePerformIO $ do
    res <-
        Unicode.decodeUtf8Indexed Unicode.ErrorOnCodingFailure (Array.read arr)
            & Stream.take i
            & Stream.fold Fold.latest
    pure
        $ case res of
              Nothing -> text
              Just ((off, len), _) ->
                  UTF8 (Array.getSliceUnsafe 0 (off + len) arr)

INLINEABLE(takeEnd) :: Int -> UTF8 -> UTF8
takeEnd i text@(UTF8 arr) = unsafePerformIO $ do
    res <-
        Array.readRev arr
            & Unicode.decodeUtf8ReverseIndexed Unicode.ErrorOnCodingFailure
            & Stream.take i
            & Stream.fold Fold.latest
    let arrLen = Array.length arr
    pure
        $ case res of
              Nothing -> text
              Just ((off, len), _) ->
                  UTF8 (Array.getSliceUnsafe (arrLen - (off + len)) arrLen arr)

INLINEABLE(drop) :: Int -> UTF8 -> UTF8
drop i text@(UTF8 arr) = unsafePerformIO $ do
    res <-
        Unicode.decodeUtf8Indexed Unicode.ErrorOnCodingFailure (Array.read arr)
            & Stream.drop i
            & Stream.fold Fold.latest
    pure
        $ case res of
              Nothing -> text
              Just ((off, len), _) ->
                  UTF8 (Array.getSliceUnsafe (off + len) len arr)

INLINEABLE(dropEnd) :: Int -> UTF8 -> UTF8
dropEnd i text@(UTF8 arr) = unsafePerformIO $ do
    res <-
        Array.readRev arr
            & Unicode.decodeUtf8ReverseIndexed Unicode.ErrorOnCodingFailure
            & Stream.drop i
            & Stream.fold Fold.latest
    let arrLen = Array.length arr
    pure
        $ case res of
              Nothing -> text
              Just ((off, len), _) ->
                  UTF8 (Array.getSliceUnsafe 0 (arrLen - (off + len)) arr)

INLINEABLE(takeWhile) :: (Char -> Bool) -> UTF8 -> UTF8
takeWhile p text@(UTF8 arr) = unsafePerformIO $ do
    res <-
        Unicode.decodeUtf8Indexed Unicode.ErrorOnCodingFailure (Array.read arr)
            & Stream.takeWhile (p . snd)
            & Stream.fold Fold.latest
    pure
        $ case res of
              Nothing -> text
              Just ((off, len), _) ->
                  UTF8 (Array.getSliceUnsafe 0 (off + len) arr)

INLINEABLE(takeWhileEnd) :: (Char -> Bool) -> UTF8 -> UTF8
takeWhileEnd p text@(UTF8 arr) = unsafePerformIO $ do
    res <-
        Array.readRev arr
            & Unicode.decodeUtf8ReverseIndexed Unicode.ErrorOnCodingFailure
            & Stream.takeWhile (p . snd)
            & Stream.fold Fold.latest
    let arrLen = Array.length arr
    pure
        $ case res of
              Nothing -> text
              Just ((off, len), _) ->
                  UTF8 (Array.getSliceUnsafe (arrLen - (off + len)) arrLen  arr)

INLINEABLE(dropWhile) :: (Char -> Bool) -> UTF8 -> UTF8
dropWhile p text@(UTF8 arr) = unsafePerformIO $ do
    res <-
        Unicode.decodeUtf8Indexed Unicode.ErrorOnCodingFailure (Array.read arr)
            & Stream.dropWhile (p . snd)
            & Stream.fold Fold.latest
    pure
        $ case res of
              Nothing -> text
              Just ((off, len), _) ->
                  UTF8 (Array.getSliceUnsafe (off + len) len arr)

INLINEABLE(dropWhileEnd) :: (Char -> Bool) -> UTF8 -> UTF8
dropWhileEnd p text@(UTF8 arr) = unsafePerformIO $ do
    res <-
        Array.readRev arr
            & Unicode.decodeUtf8ReverseIndexed Unicode.ErrorOnCodingFailure
            & Stream.dropWhile (p . snd)
            & Stream.fold Fold.latest
    let arrLen = Array.length arr
    pure
        $ case res of
              Nothing -> text
              Just ((off, len), _) ->
                  UTF8 (Array.getSliceUnsafe 0  (arrLen - (off + len))  arr)

INLINEABLE(dropAround) :: (Char -> Bool) -> UTF8 -> UTF8
dropAround p text@(UTF8 arr) = unsafePerformIO $ do
    beg <-
        Unicode.decodeUtf8Indexed Unicode.ErrorOnCodingFailure (Array.read arr)
            & Stream.dropWhile (p . snd)
            & Stream.fold Fold.latest
    end <-
        Array.readRev arr
            & Unicode.decodeUtf8ReverseIndexed Unicode.ErrorOnCodingFailure
            & Stream.dropWhile (p . snd)
            & Stream.fold Fold.latest
    let arrLen = Array.length arr
    pure
        $ case (beg, end) of
              (Just ((boff, _), _), Just ((eoff, _), _)) ->
                  UTF8 (Array.getSliceUnsafe boff eoff arr)
              (Just ((boff, _), _), Nothing) ->
                  UTF8 (Array.getSliceUnsafe boff arrLen arr)
              (Nothing, Just ((eoff, _), _)) ->
                  UTF8 (Array.getSliceUnsafe 0 eoff arr)
              (Nothing, Nothing) -> text

INLINEABLE(strip) :: UTF8 -> UTF8
strip = dropAround Char.isWhiteSpace

INLINEABLE(stripStart) :: UTF8 -> UTF8
stripStart = dropWhile Char.isWhiteSpace

INLINEABLE(stripEnd) :: UTF8 -> UTF8
stripEnd = dropWhileEnd Char.isWhiteSpace

INLINEABLE(splitAt) :: Int -> UTF8 -> (UTF8, UTF8)
splitAt i str@(UTF8 arr) = unsafePerformIO $ do
    res <-
        Unicode.decodeUtf8Indexed Unicode.ErrorOnCodingFailure (Array.read arr)
            & Stream.take i
            & Stream.fold Fold.latest
    pure
        $ case res of
              Nothing -> (empty, str)
              Just ((off, _), _) ->
                  ( UTF8 (Array.getSliceUnsafe 0 off arr)
                  , UTF8 (Array.getSliceUnsafe off (Array.length arr - off) arr)
                  )

INLINEABLE(breakOn) :: UTF8 -> UTF8 -> (UTF8, UTF8)
UNIMPLEMENTED(breakOn)

INLINEABLE(breakOnEnd) :: UTF8 -> UTF8 -> (UTF8, UTF8)
UNIMPLEMENTED(breakOnEnd)

INLINEABLE(break) :: (Char -> Bool) -> UTF8 -> (UTF8, UTF8)
break p str@(UTF8 arr) = unsafePerformIO $ do
    res <-
        Unicode.decodeUtf8Indexed Unicode.ErrorOnCodingFailure (Array.read arr)
            & Stream.fold (Fold.find (p . snd))
    pure
        $ case res of
              Nothing -> (empty, str)
              Just ((off, _), _) ->
                  ( UTF8 (Array.getSliceUnsafe 0 off arr)
                  , UTF8 (Array.getSliceUnsafe off (Array.length arr - off) arr)
                  )

INLINEABLE(span) :: (Char -> Bool) -> UTF8 -> (UTF8, UTF8)
span p str@(UTF8 arr) = unsafePerformIO $ do
    res <-
        Unicode.decodeUtf8Indexed Unicode.ErrorOnCodingFailure (Array.read arr)
            & Stream.takeWhile (p . snd)
            & Stream.fold Fold.latest
    pure
        $ case res of
              Nothing -> (empty, str)
              Just ((off, _), _) ->
                  ( UTF8 (Array.getSliceUnsafe 0 off arr)
                  , UTF8 (Array.getSliceUnsafe off (Array.length arr - off) arr)
                  )

INLINEABLE(spanM) :: Monad m => (Char -> m Bool) -> UTF8 -> m (UTF8, UTF8)
spanM p str@(UTF8 arr) = do
    res <-
        Unicode.decodeUtf8Indexed Unicode.ErrorOnCodingFailure (Array.read arr)
            & Stream.takeWhileM (p . snd)
            & Stream.fold Fold.latest
    pure
        $ case res of
              Nothing -> (empty, str)
              Just ((off, _), _) ->
                  ( UTF8 (Array.getSliceUnsafe 0 off arr)
                  , UTF8 (Array.getSliceUnsafe off (Array.length arr - off) arr)
                  )

INLINEABLE(spanEndM) :: Monad m => (Char -> m Bool) -> UTF8 -> m (UTF8, UTF8)
spanEndM p str@(UTF8 arr) = do
    res <-
        Array.read arr
            & Unicode.decodeUtf8ReverseIndexed Unicode.ErrorOnCodingFailure
            & Stream.takeWhileM (p . snd)
            & Stream.fold Fold.latest
    let arrLen = Array.length arr
    pure
        $ case res of
              Nothing -> (empty, str)
              Just ((off, _), _) ->
                  ( UTF8 (Array.getSliceUnsafe 0 (arrLen - off) arr)
                  , UTF8 (Array.getSliceUnsafe (arrLen - off) arrLen arr)
                  )

INLINEABLE(group) :: UTF8 -> [UTF8]
group (UTF8 arr) =
      Array.read arr
            & Unicode.decodeUtf8ReverseIndexed Unicode.ErrorOnCodingFailure
            & Stream.parseMany (Parser.groupBy eq fld)
            & Stream.catRights -- There shouldn't be any parse error here!
            & fmap UTF8
            & Stream.toList
            & unsafePerformIO

    where
    eq a b = snd a == snd b
    fld = slicer <$> Fold.tee Fold.one Fold.latest
    slicer (Just ((os, _), _), Just ((oe, le), _)) =
        Array.getSliceUnsafe os (oe + le) arr
    slicer _ = Array.empty

INLINEABLE(groupBy) :: (Char -> Char -> Bool) -> UTF8 -> [UTF8]
groupBy p (UTF8 arr) =
      Array.read arr
            & Unicode.decodeUtf8ReverseIndexed Unicode.ErrorOnCodingFailure
            & Stream.parseMany (Parser.groupBy eq fld)
            & Stream.catRights -- There shouldn't be any parse error here!
            & fmap UTF8
            & Stream.toList
            & unsafePerformIO

    where
    eq a b = p (snd a) (snd b)
    fld = slicer <$> Fold.tee Fold.one Fold.latest
    slicer (Just ((os, _), _), Just ((oe, le), _)) =
        Array.getSliceUnsafe os (oe + le) arr
    slicer _ = Array.empty

INLINEABLE(inits) :: UTF8 -> [UTF8]
inits (UTF8 arr) =
      Array.read arr
            & Unicode.decodeUtf8ReverseIndexed Unicode.ErrorOnCodingFailure
            & fmap slicer
            & fmap UTF8
            & Stream.toList
            & fmap (empty:)
            & unsafePerformIO
    where
    slicer ((o, l), _) = Array.getSliceUnsafe 0 (o + l) arr

INLINEABLE(tails) :: UTF8 -> [UTF8]
tails (UTF8 arr) =
      Array.read arr
            & Unicode.decodeUtf8ReverseIndexed Unicode.ErrorOnCodingFailure
            & fmap slicer
            & fmap UTF8
            & Stream.fold Fold.toListRev
            & fmap (P.reverse . (empty:))
            & unsafePerformIO
    where
    slicer ((o, _), _) = Array.getSliceUnsafe o (Array.length arr) arr

--------------------------------------------------------------------------------
-- Breaking into many substrings
--------------------------------------------------------------------------------

INLINEABLE(splitOn) :: UTF8 -> UTF8 -> [UTF8]
splitOn (UTF8 pat) (UTF8 arr) =
      Array.read arr
            & Stream.zipWith (,) Stream.enumerate
            & Stream.splitOnSeqWith pat snd fld
            & fmap UTF8
            & Stream.toList
            & unsafePerformIO
    where
    fld = slicer <$> Fold.tee Fold.one Fold.latest
    slicer (Just (s, _), Just (l, _)) =
        Array.getSliceUnsafe s  (l + 1) arr
    slicer _ = Array.empty

INLINEABLE(split) :: (Char -> Bool) -> UTF8 -> [UTF8]
split p (UTF8 arr) =
      Array.read arr
            & Unicode.decodeUtf8ReverseIndexed Unicode.ErrorOnCodingFailure
            & Stream.splitOn (p . snd) fld
            & fmap UTF8
            & Stream.toList
            & unsafePerformIO
    where
    fld = slicer <$> Fold.tee Fold.one Fold.latest
    slicer (Just ((os, _), _), Just ((oe, le), _)) =
        Array.getSliceUnsafe os (oe + le) arr
    slicer _ = Array.empty

INLINEABLE(chunksOf) :: Int -> UTF8 -> [UTF8]
chunksOf i (UTF8 arr) =
      Array.read arr
            & Unicode.decodeUtf8ReverseIndexed Unicode.ErrorOnCodingFailure
            & Stream.groupsOf i fld
            & fmap UTF8
            & Stream.toList
            & unsafePerformIO
    where
    fld = slicer <$> Fold.tee Fold.one Fold.latest
    slicer (Just ((os, _), _), Just ((oe, le), _)) =
        Array.getSliceUnsafe os (oe + le) arr
    slicer _ = Array.empty

--------------------------------------------------------------------------------
-- Breaking into lines and words
--------------------------------------------------------------------------------

-- XXX isLineSeperator?
INLINEABLE(lines) :: UTF8 -> [UTF8]
lines = split (== '\n')

INLINEABLE(words) :: UTF8 -> [UTF8]
words = split Char.isWhiteSpace

INLINEABLE(unlines) :: [UTF8] -> UTF8
unlines = intercalate (singleton '\n')

INLINEABLE(unwords) :: [UTF8] -> UTF8
unwords = intercalate (singleton ' ')

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

INLINEABLE(isPrefixOf) :: UTF8 -> UTF8 -> Bool
UNIMPLEMENTED(isPrefixOf)

INLINEABLE(isSuffixOf) :: UTF8 -> UTF8 -> Bool
UNIMPLEMENTED(isSuffixOf)

INLINEABLE(isInfixOf) :: UTF8 -> UTF8 -> Bool
UNIMPLEMENTED(isInfixOf)

--------------------------------------------------------------------------------
-- View Patterns
--------------------------------------------------------------------------------

INLINEABLE(stripPrefix) :: UTF8 -> UTF8 -> Bool
UNIMPLEMENTED(stripPrefix)

INLINEABLE(stripSuffix) :: UTF8 -> UTF8 -> Bool
UNIMPLEMENTED(stripSuffix)

INLINEABLE(commonPrefixes) :: UTF8 -> UTF8 -> Maybe (UTF8, UTF8, UTF8)
UNIMPLEMENTED(commonPrefixes)

--------------------------------------------------------------------------------
-- Searching
--------------------------------------------------------------------------------

INLINEABLE(filter) :: (Char -> Bool) -> UTF8 -> UTF8
filter p inp =
    read inp
        & Stream.filter p
        & Stream.fold (createOf (byteLength inp))
        & unsafePerformIO

INLINEABLE(breakOnAll)
    :: UTF8 -- ^ needle to search for
    -> UTF8 -- ^ haystack in which to search
    -> [(UTF8, UTF8)]
UNIMPLEMENTED(breakOnAll)

INLINEABLE(find) :: (Char -> Bool) -> UTF8 -> Maybe Char
find p inp =
    read inp
        & Stream.fold (Fold.find p)
        & unsafePerformIO

INLINEABLE(elem) :: Char -> UTF8 -> Bool
elem c inp =
    read inp
        & Stream.fold (Fold.elem c)
        & unsafePerformIO

INLINEABLE(partition) :: (Char -> Bool) -> UTF8 -> (UTF8, UTF8)
partition p inp =
    read inp
        & Stream.filter p
        & Stream.fold (Fold.tee (Fold.filter p create) (Fold.filter p create))
        & unsafePerformIO

--------------------------------------------------------------------------------
-- Indexing
--------------------------------------------------------------------------------

INLINEABLE(index) ::  UTF8 -> Int -> Maybe Char
index inp i =
    read inp
        & Stream.fold (Fold.index i)
        & unsafePerformIO

INLINEABLE(findIndex) ::  (Char -> Bool) -> UTF8 -> Maybe Int
findIndex p inp =
    read inp
        & Stream.fold (Fold.findIndex p)
        & unsafePerformIO

--------------------------------------------------------------------------------
-- Zipping
--------------------------------------------------------------------------------

INLINEABLE(zip) :: UTF8 -> UTF8 -> [(Char, Char)]
zip l r =
    Stream.zipWith (,) (read l) (read r)
        & Stream.toList
        & unsafePerformIO

--------------------------------------------------------------------------------
-- Low level operations
--------------------------------------------------------------------------------

INLINEABLE(copy) :: UTF8 -> UTF8
copy (UTF8 arr) = unsafePerformIO $ do
    new <- MArray.emptyOf (Array.length arr)
    new1 <- MArray.spliceUnsafe new (Array.unsafeThaw arr)
    pure $ UTF8 $ Array.unsafeFreeze new1

INLINEABLE(unpackCString#) :: Addr# -> UTF8
unpackCString# addr =
    Unicode.fromStr# addr
        & fromStream
        & unsafePerformIO

INLINEABLE(unpackCStringAscii#) :: Addr# -> UTF8
unpackCStringAscii# addr = UTF8 (Array.fromByteStr# addr)

INLINEABLE(measureOff) :: Int -> UTF8 -> UTF8
UNIMPLEMENTED(measureOff)
