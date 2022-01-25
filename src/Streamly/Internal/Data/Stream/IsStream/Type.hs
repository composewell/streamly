{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.Type
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.Stream.IsStream.Type
    (
    -- * IsStream Type Class
      IsStream (..)
    , K.Stream (..)
    , Streaming

    -- * Type Conversion
    , fromStreamS
    , toStreamS
    , fromStreamD
    , toStreamD
    , adapt
    , toConsK

    -- * Building a stream
    , mkStream
    , foldStreamShared
    , foldStream

    -- * Stream Types
    , SerialT
    , Serial
    , fromSerial

    , WSerialT
    , WSerial
    , fromWSerial

    , AsyncT
    , Async
    , fromAsync

    , WAsyncT
    , WAsync
    , fromWAsync

    , AheadT
    , Ahead
    , fromAhead

    , ParallelT
    , Parallel
    , fromParallel

    , ZipSerialM
    , ZipSerial
    , fromZipSerial

    , ZipAsyncM
    , ZipAsync
    , fromZipAsync

    -- * Construction
    , cons
    , (.:)
    , nil
    , nilM
    , fromPure
    , fromEffect
    , repeat

    -- * Bind/Concat
    , bindWith
    , concatMapWith

    -- * Fold Utilities
    , concatFoldableWith
    , concatMapFoldableWith
    , concatForFoldableWith

    -- * Running Effects
    , drain

    -- * Conversion operations
    , fromList
    , toList

    -- * Fold operations
    , foldrM
    , foldrMx
    , foldr

    , foldlx'
    , foldlMx'
    , foldl'
    , fold

    -- * Zip style operations
    , eqBy
    , cmpBy

    -- * Deprecated
    , interleaving
    , zipping
    , zippingAsync
    )
where

import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Fold.Type (Fold (..))
import Streamly.Internal.Data.Stream.Serial
    (SerialT(..), Serial, WSerialT(..), WSerial)
import Streamly.Internal.Data.Stream.Async
    (AsyncT(..), Async, WAsyncT(..), WAsync)
import Streamly.Internal.Data.Stream.Ahead (AheadT(..), Ahead)
import Streamly.Internal.Data.Stream.Parallel (ParallelT(..), Parallel)
import Streamly.Internal.Data.Stream.Zip
    (ZipSerialM(..), ZipSerial, ZipAsyncM(..), ZipAsync)
import Streamly.Internal.Data.SVar.Type (State, adaptState)

import qualified Prelude
import qualified Streamly.Internal.Data.Stream.Ahead as Ahead
import qualified Streamly.Internal.Data.Stream.Async as Async
import qualified Streamly.Internal.Data.Stream.Parallel as Parallel
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
#ifdef USE_STREAMK_ONLY
import qualified Streamly.Internal.Data.Stream.StreamK as S
import qualified Streamly.Internal.Data.Stream.StreamK.Type as S
#else
import qualified Streamly.Internal.Data.Stream.StreamD.Type as S
#endif
import qualified Streamly.Internal.Data.Stream.Zip as Zip

import Prelude hiding (foldr, repeat)

#include "inline.hs"
#define USE_IS_STREAM
#include "PreludeCommon.hs"

------------------------------------------------------------------------------
-- Types that can behave as a Stream
------------------------------------------------------------------------------

infixr 5 `consM`
infixr 5 |:

-- XXX Use a different SVar based on the stream type. But we need to make sure
-- that we do not lose performance due to polymorphism.
--
-- | Class of types that can represent a stream of elements of some type 'a' in
-- some monad 'm'.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
class
#if __GLASGOW_HASKELL__ >= 806
    ( forall m a. MonadAsync m => Semigroup (t m a)
    , forall m a. MonadAsync m => Monoid (t m a)
    , forall m. Monad m => Functor (t m)
    , forall m. MonadAsync m => Applicative (t m)
    ) =>
#endif
      IsStream t where
    toStream :: t m a -> K.Stream m a
    fromStream :: K.Stream m a -> t m a
    -- | Constructs a stream by adding a monadic action at the head of an
    -- existing stream. For example:
    --
    -- @
    -- > toList $ getLine \`consM` getLine \`consM` nil
    -- hello
    -- world
    -- ["hello","world"]
    -- @
    --
    -- /Concurrent (do not use 'fromParallel' to construct infinite streams)/
    --
    -- @since 0.2.0
    consM :: MonadAsync m => m a -> t m a -> t m a
    -- | Operator equivalent of 'consM'. We can read it as "@parallel colon@"
    -- to remember that @|@ comes before ':'.
    --
    -- @
    -- > toList $ getLine |: getLine |: nil
    -- hello
    -- world
    -- ["hello","world"]
    -- @
    --
    -- @
    -- let delay = threadDelay 1000000 >> print 1
    -- drain $ fromSerial  $ delay |: delay |: delay |: nil
    -- drain $ fromParallel $ delay |: delay |: delay |: nil
    -- @
    --
    -- /Concurrent (do not use 'fromParallel' to construct infinite streams)/
    --
    -- @since 0.2.0
    (|:) :: MonadAsync m => m a -> t m a -> t m a
    -- We can define (|:) just as 'consM' but it is defined explicitly for each
    -- type because we want to use SPECIALIZE pragma on the definition.

-- | Same as 'IsStream'.
--
-- @since 0.1.0
{-# DEPRECATED Streaming "Please use IsStream instead." #-}
type Streaming = IsStream

-------------------------------------------------------------------------------
-- Type adapting combinators
-------------------------------------------------------------------------------

-- XXX Move/reset the State here by reconstructing the stream with cleared
-- state. Can we make sure we do not do that when t1 = t2? If we do this then
-- we do not need to do that explicitly using svarStyle.  It would act as
-- unShare when the stream type is the same.
--
-- | Adapt any specific stream type to any other specific stream type.
--
-- /Since: 0.1.0 ("Streamly")/
--
-- @since 0.8.0
adapt :: (IsStream t1, IsStream t2) => t1 m a -> t2 m a
adapt = fromStream . toStream

{-# INLINE fromStreamD #-}
fromStreamD :: (IS_STREAM Monad m) => D.Stream m a -> STREAM m a
fromStreamD = FROM_STREAM . D.toStreamK

-- | Adapt a polymorphic consM operation to a StreamK cons operation
{-# INLINE toConsK #-}
toConsK :: IsStream t =>
    (m a -> t m a -> t m a) -> m a -> K.Stream m a -> K.Stream m a
toConsK cns x xs = toStream $ x `cns` fromStream xs

------------------------------------------------------------------------------
-- Building a stream
------------------------------------------------------------------------------

-- XXX The State is always parameterized by "Stream" which means State is not
-- different for different stream types. So we have to manually make sure that
-- when converting from one stream to another we migrate the state correctly.
-- This can be fixed if we use a different SVar type for different streams.
-- Currently we always use "SVar Stream" and therefore a different State type
-- parameterized by that stream.
--
-- XXX Since t is coercible we should be able to coerce k
-- mkStream k = fromStream $ MkStream $ coerce k
--
-- | Build a stream from an 'SVar', a stop continuation, a singleton stream
-- continuation and a yield continuation.
{-# INLINE_EARLY mkStream #-}
mkStream :: IsStream t
    => (forall r. State K.Stream m a
        -> (a -> t m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r)
    -> t m a
mkStream k = fromStream $ K.MkStream $ \st yld sng stp ->
    let yieldk a r = yld a (toStream r)
     in k st yieldk sng stp

{-# RULES "mkStream from stream" mkStream = mkStreamFromStream #-}
mkStreamFromStream :: IsStream t
    => (forall r. State K.Stream m a
        -> (a -> K.Stream m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r)
    -> t m a
mkStreamFromStream k = fromStream $ K.MkStream k

{-# RULES "mkStream stream" mkStream = mkStreamStream #-}
mkStreamStream
    :: (forall r. State K.Stream m a
        -> (a -> K.Stream m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r)
    -> K.Stream m a
mkStreamStream = K.MkStream

------------------------------------------------------------------------------
-- Folding a stream
------------------------------------------------------------------------------

-- | Fold a stream by providing an SVar, a stop continuation, a singleton
-- continuation and a yield continuation. The stream would share the current
-- SVar passed via the State.
{-# INLINE_EARLY foldStreamShared #-}
foldStreamShared
    :: IsStream t
    => State K.Stream m a
    -> (a -> t m a -> m r)
    -> (a -> m r)
    -> m r
    -> t m a
    -> m r
foldStreamShared st yld sng stp m =
    let yieldk a x = yld a (fromStream x)
        K.MkStream k = toStream m
     in k st yieldk sng stp

-- XXX write a similar rule for foldStream as well?
{-# RULES "foldStreamShared from stream"
   foldStreamShared = foldStreamSharedStream #-}
foldStreamSharedStream
    :: State K.Stream m a
    -> (a -> K.Stream m a -> m r)
    -> (a -> m r)
    -> m r
    -> K.Stream m a
    -> m r
foldStreamSharedStream st yld sng stp m =
    let K.MkStream k = m
     in k st yld sng stp

-- | Fold a stream by providing a State, stop continuation, a singleton
-- continuation and a yield continuation. The stream will not use the SVar
-- passed via State.
{-# INLINE foldStream #-}
foldStream
    :: IsStream t
    => State K.Stream m a
    -> (a -> t m a -> m r)
    -> (a -> m r)
    -> m r
    -> t m a
    -> m r
foldStream st yld sng stp m =
    let yieldk a x = yld a (fromStream x)
        K.MkStream k = toStream m
     in k (adaptState st) yieldk sng stp

-------------------------------------------------------------------------------
-- Serial
-------------------------------------------------------------------------------

-- | Fix the type of a polymorphic stream as 'SerialT'.
--
-- /Since: 0.1.0 ("Streamly")/
--
-- @since 0.8.0
fromSerial :: IsStream t => SerialT m a -> t m a
fromSerial = adapt

instance IsStream SerialT where
    toStream = getSerialT
    fromStream = SerialT

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> SerialT IO a -> SerialT IO a #-}
    consM = Serial.consM

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> SerialT IO a -> SerialT IO a #-}
    (|:) = Serial.consM

-- | Fix the type of a polymorphic stream as 'WSerialT'.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
fromWSerial :: IsStream t => WSerialT m a -> t m a
fromWSerial = adapt

-- | Same as 'fromWSerial'.
--
-- @since 0.1.0
{-# DEPRECATED interleaving "Please use fromWSerial instead." #-}
interleaving :: IsStream t => WSerialT m a -> t m a
interleaving = fromWSerial

instance IsStream WSerialT where
    toStream = getWSerialT
    fromStream = WSerialT

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> WSerialT IO a -> WSerialT IO a #-}
    consM :: Monad m => m a -> WSerialT m a -> WSerialT m a
    consM = Serial.consMWSerial

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> WSerialT IO a -> WSerialT IO a #-}
    (|:) :: Monad m => m a -> WSerialT m a -> WSerialT m a
    (|:) = Serial.consMWSerial

-------------------------------------------------------------------------------
-- Async
-------------------------------------------------------------------------------

-- | Fix the type of a polymorphic stream as 'AsyncT'.
--
-- /Since: 0.1.0 ("Streamly")/
--
-- @since 0.8.0
fromAsync :: IsStream t => AsyncT m a -> t m a
fromAsync = adapt

instance IsStream AsyncT where
    toStream = getAsyncT
    fromStream = AsyncT

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> AsyncT IO a -> AsyncT IO a #-}
    consM = Async.consMAsync

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> AsyncT IO a -> AsyncT IO a #-}
    (|:) = Async.consMAsync

-- | Fix the type of a polymorphic stream as 'WAsyncT'.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
fromWAsync :: IsStream t => WAsyncT m a -> t m a
fromWAsync = adapt

instance IsStream WAsyncT where
    toStream = getWAsyncT
    fromStream = WAsyncT

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> WAsyncT IO a -> WAsyncT IO a #-}
    consM = Async.consMWAsync

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> WAsyncT IO a -> WAsyncT IO a #-}
    (|:) = Async.consMWAsync

-------------------------------------------------------------------------------
-- Ahead
-------------------------------------------------------------------------------

-- | Fix the type of a polymorphic stream as 'AheadT'.
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
fromAhead :: IsStream t => AheadT m a -> t m a
fromAhead = adapt

instance IsStream AheadT where
    toStream = getAheadT
    fromStream = AheadT

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> AheadT IO a -> AheadT IO a #-}
    consM = Ahead.consM

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> AheadT IO a -> AheadT IO a #-}
    (|:) = Ahead.consM

-------------------------------------------------------------------------------
-- Parallel
-------------------------------------------------------------------------------

-- | Fix the type of a polymorphic stream as 'ParallelT'.
--
-- /Since: 0.1.0 ("Streamly")/
--
-- @since 0.8.0
fromParallel :: IsStream t => ParallelT m a -> t m a
fromParallel = adapt

instance IsStream ParallelT where
    toStream = getParallelT
    fromStream = ParallelT

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> ParallelT IO a -> ParallelT IO a #-}
    consM = Parallel.consM

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> ParallelT IO a -> ParallelT IO a #-}
    (|:) = Parallel.consM

-------------------------------------------------------------------------------
-- Zip
-------------------------------------------------------------------------------

-- | Fix the type of a polymorphic stream as 'ZipSerialM'.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
fromZipSerial :: IsStream t => ZipSerialM m a -> t m a
fromZipSerial = adapt

-- | Same as 'fromZipSerial'.
--
-- @since 0.1.0
{-# DEPRECATED zipping "Please use fromZipSerial instead." #-}
zipping :: IsStream t => ZipSerialM m a -> t m a
zipping = fromZipSerial

instance IsStream ZipSerialM where
    toStream = getZipSerialM
    fromStream = ZipSerialM

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> ZipSerialM IO a -> ZipSerialM IO a #-}
    consM :: Monad m => m a -> ZipSerialM m a -> ZipSerialM m a
    consM = Zip.consMZip

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> ZipSerialM IO a -> ZipSerialM IO a #-}
    (|:) :: Monad m => m a -> ZipSerialM m a -> ZipSerialM m a
    (|:) = Zip.consMZip

-- | Fix the type of a polymorphic stream as 'ZipAsyncM'.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
fromZipAsync :: IsStream t => ZipAsyncM m a -> t m a
fromZipAsync = adapt

-- | Same as 'fromZipAsync'.
--
-- @since 0.1.0
{-# DEPRECATED zippingAsync "Please use fromZipAsync instead." #-}
zippingAsync :: IsStream t => ZipAsyncM m a -> t m a
zippingAsync = fromZipAsync

instance IsStream ZipAsyncM where
    toStream = getZipAsyncM
    fromStream = ZipAsyncM

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> ZipAsyncM IO a -> ZipAsyncM IO a #-}
    consM :: Monad m => m a -> ZipAsyncM m a -> ZipAsyncM m a
    consM = Zip.consMZipAsync

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> ZipAsyncM IO a -> ZipAsyncM IO a #-}
    (|:) :: Monad m => m a -> ZipAsyncM m a -> ZipAsyncM m a
    (|:) = Zip.consMZipAsync

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

infixr 5 `cons`

-- | Construct a stream by adding a pure value at the head of an existing
-- stream. For serial streams this is the same as @(return a) \`consM` r@ but
-- more efficient. For concurrent streams this is not concurrent whereas
-- 'consM' is concurrent. For example:
--
-- @
-- > toList $ 1 \`cons` 2 \`cons` 3 \`cons` nil
-- [1,2,3]
-- @
--
-- @since 0.1.0
{-# INLINE_NORMAL cons #-}
cons :: IsStream t => a -> t m a -> t m a
cons a r = fromStream $ K.cons a (toStream r)

infixr 5 .:

-- | Operator equivalent of 'cons'.
--
-- @
-- > toList $ 1 .: 2 .: 3 .: nil
-- [1,2,3]
-- @
--
-- @since 0.1.1
{-# INLINE (.:) #-}
(.:) :: IsStream t => a -> t m a -> t m a
(.:) = cons

{-# INLINE_NORMAL nil #-}
nil :: IsStream t => t m a
nil = fromStream K.nil

{-# INLINE_NORMAL nilM #-}
nilM :: (IsStream t, Monad m) => m b -> t m a
nilM = fromStream . K.nilM

{-# INLINE_NORMAL fromPure #-}
fromPure :: IsStream t => a -> t m a
fromPure = fromStream . K.fromPure

{-# INLINE_NORMAL fromEffect #-}
fromEffect :: (Monad m, IsStream t) => m a -> t m a
fromEffect = fromStream . K.fromEffect

{-# INLINE repeat #-}
repeat :: IsStream t => a -> t m a
repeat = fromStream . K.repeat

-------------------------------------------------------------------------------
-- Bind/Concat
-------------------------------------------------------------------------------

{-# INLINE bindWith #-}
bindWith
    :: IsStream t
    => (t m b -> t m b -> t m b)
    -> t m a
    -> (a -> t m b)
    -> t m b
bindWith par m1 f =
    fromStream
        $ K.bindWith
            (\s1 s2 -> toStream $ par (fromStream s1) (fromStream s2))
            (toStream m1)
            (toStream . f)

-- | @concatMapWith mixer generator stream@ is a two dimensional looping
-- combinator.  The @generator@ function is used to generate streams from the
-- elements in the input @stream@ and the @mixer@ function is used to merge
-- those streams.
--
-- Note we can merge streams concurrently by using a concurrent merge function.
--
-- /Since: 0.7.0/
--
-- /Since: 0.8.0 (signature change)/
{-# INLINE concatMapWith #-}
concatMapWith
    :: IsStream t
    => (t m b -> t m b -> t m b)
    -> (a -> t m b)
    -> t m a
    -> t m b
concatMapWith par f xs = bindWith par xs f

-- | A variant of 'foldMap' that allows you to map a monadic streaming action
-- on a 'Foldable' container and then fold it using the specified stream merge
-- operation.
--
-- @concatMapFoldableWith 'async' return [1..3]@
--
-- Equivalent to:
--
-- @
-- concatMapFoldableWith f g = Prelude.foldr (f . g) S.nil
-- concatMapFoldableWith f g xs = S.concatMapWith f g (S.fromFoldable xs)
-- @
--
-- /Since: 0.8.0 (Renamed foldMapWith to concatMapFoldableWith)/
--
-- /Since: 0.1.0 ("Streamly")/
{-# INLINE concatMapFoldableWith #-}
concatMapFoldableWith :: (IsStream t, Foldable f)
    => (t m b -> t m b -> t m b) -> (a -> t m b) -> f a -> t m b
concatMapFoldableWith f g = Prelude.foldr (f . g) nil

-- | Like 'concatMapFoldableWith' but with the last two arguments reversed i.e. the
-- monadic streaming function is the last argument.
--
-- Equivalent to:
--
-- @
-- concatForFoldableWith f xs g = Prelude.foldr (f . g) S.nil xs
-- concatForFoldableWith f = flip (S.concatMapFoldableWith f)
-- @
--
-- /Since: 0.8.0 (Renamed forEachWith to concatForFoldableWith)/
--
-- /Since: 0.1.0 ("Streamly")/
{-# INLINE concatForFoldableWith #-}
concatForFoldableWith :: (IsStream t, Foldable f)
    => (t m b -> t m b -> t m b) -> f a -> (a -> t m b) -> t m b
concatForFoldableWith f = flip (concatMapFoldableWith f)

-- | A variant of 'Data.Foldable.fold' that allows you to fold a 'Foldable'
-- container of streams using the specified stream sum operation.
--
-- @concatFoldableWith 'async' $ map return [1..3]@
--
-- Equivalent to:
--
-- @
-- concatFoldableWith f = Prelude.foldr f S.nil
-- concatFoldableWith f = S.concatMapFoldableWith f id
-- @
--
-- /Since: 0.8.0 (Renamed foldWith to concatFoldableWith)/
--
-- /Since: 0.1.0 ("Streamly")/
{-# INLINE concatFoldableWith #-}
concatFoldableWith :: (IsStream t, Foldable f)
    => (t m a -> t m a -> t m a) -> f (t m a) -> t m a
concatFoldableWith f = concatMapFoldableWith f id
