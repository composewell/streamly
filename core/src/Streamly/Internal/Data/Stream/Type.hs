{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.Type
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.Type
    (
      Stream
    , fromStreamK
    , toStreamK
    , fromStreamD
    , toStreamD

    -- * Construction
    , cons
    , consM
    , (.:)
    , nil
    , nilM
    , fromPure
    , fromEffect
    , append

    -- * Bind/Concat
    , bindWith
    , concatMapWith

    -- * Fold Utilities
    , concatFoldableWith
    , concatMapFoldableWith
    , concatForFoldableWith

    -- * Fold operations
    , foldrM
    , foldrMx
    , foldr

    , foldlx'
    , foldlMx'
    , foldl'

    -- * Conversion operations
    , fromList
    , toList
    -- * Zip style operations
    , eqBy
    , cmpBy
    )
where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData(..), NFData1(..))
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Foldable (Foldable(foldl'), fold)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Endo(..))
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import GHC.Exts (IsList(..), IsString(..), oneShot)
import Streamly.Internal.BaseCompat ((#.))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Text.Read
       ( Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec
       , readListPrecDefault)

import qualified Streamly.Internal.Data.Stream.Common as P
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Prelude hiding (repeat)

#include "Instances.hs"
#include "inline.hs"

-- $setup
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Stream as Stream

------------------------------------------------------------------------------
-- Stream
------------------------------------------------------------------------------

-- | Semigroup instance appends two streams:
--
-- > (<>) = Stream.append
--
-- Monad bind maps a stream generator function on the stream and flattens the
-- resulting stream:
--
-- > (>>=) = flip . Stream.concatMapWith Stream.append
--
-- A 'Monad' bind behaves like a @for@ loop:
--
-- >>> :{
-- Stream.fold Fold.toList $ do
--      x <- Stream.unfold Unfold.fromList [1,2] -- foreach x in stream
--      return x
-- :}
-- [1,2]
--
-- Nested monad binds behave like nested @for@ loops:
--
-- >>> :{
-- Stream.fold Fold.toList $ do
--     x <- Stream.unfold Unfold.fromList [1,2] -- foreach x in stream
--     y <- Stream.unfold Unfold.fromList [3,4] -- foreach y in stream
--     return (x, y)
-- :}
-- [(1,3),(1,4),(2,3),(2,4)]
--
-- @since 0.9.0
newtype Stream m a = Stream (K.Stream m a)
    -- XXX when deriving do we inherit an INLINE?
    deriving (Semigroup, Monoid, MonadTrans)

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

{-# INLINE fromStreamK #-}
fromStreamK :: K.Stream m a -> Stream m a
fromStreamK = Stream

{-# INLINE toStreamK #-}
toStreamK :: Stream m a -> K.Stream m a
toStreamK (Stream k) = k

{-# INLINE toStreamD #-}
toStreamD :: Applicative m => Stream m a -> D.Stream m a
toStreamD = D.fromStreamK . toStreamK

{-# INLINE fromStreamD #-}
fromStreamD :: Monad m => D.Stream m a -> Stream m a
fromStreamD = fromStreamK . D.toStreamK

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (Stream m) where
    return = pure

    -- Benchmarks better with StreamD bind and pure:
    -- toList, filterAllout, *>, *<, >> (~2x)
    --
    -- pure = Stream . D.fromStreamD . D.fromPure
    -- m >>= f = D.fromStreamD $ D.concatMap (D.toStreamD . f) (D.toStreamD m)

    -- Benchmarks better with CPS bind and pure:
    -- Prime sieve (25x)
    -- n binds, breakAfterSome, filterAllIn, state transformer (~2x)
    --
    {-# INLINE (>>=) #-}
    (>>=) (Stream m) f = Stream $ K.bindWith K.serial m (toStreamK . f)

    {-# INLINE (>>) #-}
    (>>)  = (*>)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

{-# INLINE apSerial #-}
apSerial :: Monad m => Stream m (a -> b) -> Stream m a -> Stream m b
apSerial (Stream m1) (Stream m2) =
    Stream $ D.toStreamK $ D.fromStreamK m1 <*> D.fromStreamK m2

{-# INLINE apSequence #-}
apSequence :: Monad m => Stream m a -> Stream m b -> Stream m b
apSequence (Stream m1) (Stream m2) =
    Stream $ D.toStreamK $ D.fromStreamK m1 *> D.fromStreamK m2

{-# INLINE apDiscardSnd #-}
apDiscardSnd :: Monad m => Stream m a -> Stream m b -> Stream m a
apDiscardSnd (Stream m1) (Stream m2) =
    Stream $ D.toStreamK $ D.fromStreamK m1 <* D.fromStreamK m2

-- Note: we need to define all the typeclass operations because we want to
-- INLINE them.
instance Monad m => Applicative (Stream m) where
    {-# INLINE pure #-}
    pure = Stream . K.fromPure

    {-# INLINE (<*>) #-}
    (<*>) = apSerial
    -- (<*>) = K.apSerial

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

    {-# INLINE (*>) #-}
    (*>)  = apSequence
    -- (*>)  = K.apSerialDiscardFst

    {-# INLINE (<*) #-}
    (<*) = apDiscardSnd
    -- (<*)  = K.apSerialDiscardSnd

MONAD_COMMON_INSTANCES(Stream,)
LIST_INSTANCES(Stream)
NFDATA1_INSTANCE(Stream)
FOLDABLE_INSTANCE(Stream)
TRAVERSABLE_INSTANCE(Stream)

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

infixr 5 `cons`

-- | Construct a stream by adding a pure value at the head of an existing
-- stream. Same as the following but more efficient:
--
-- For example:
--
-- >> s = 1 `Stream.cons` 2 `Stream.cons` 3 `Stream.cons` Stream.nil
-- >> Stream.fold Fold.toList s
-- >[1,2,3]
--
-- >>> cons x xs = return x `Stream.consM` xs
--
-- /Pre-release/
--
{-# INLINE_NORMAL cons #-}
cons ::  a -> Stream m a -> Stream m a
cons x = fromStreamK . K.cons x . toStreamK

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
-- > consM x xs = fromEffect x `append` xs
--
-- /Pre-release/
--
{-# INLINE consM #-}
{-# SPECIALIZE consM :: IO a -> Stream IO a -> Stream IO a #-}
consM :: Monad m => m a -> Stream m a -> Stream m a
consM m = fromStreamK . K.consM m . toStreamK

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
(.:) ::  a -> Stream m a -> Stream m a
(.:) = cons

{-# INLINE_NORMAL nil #-}
nil ::  Stream m a
nil = Stream K.nil

{-# INLINE_NORMAL nilM #-}
nilM :: (Monad m) => m b -> Stream m a
nilM = Stream . K.nilM

{-# INLINE_NORMAL fromPure #-}
fromPure :: a -> Stream m a
fromPure = Stream . K.fromPure

{-# INLINE_NORMAL fromEffect #-}
fromEffect :: Monad m => m a -> Stream m a
fromEffect = Stream . K.fromEffect

-------------------------------------------------------------------------------
-- Bind/Concat
-------------------------------------------------------------------------------

{-# INLINE bindWith #-}
bindWith
    :: (Stream m b -> Stream m b -> Stream m b)
    -> Stream m a
    -> (a -> Stream m b)
    -> Stream m b
bindWith par m1 f =
    Stream
        $ K.bindWith
            (\s1 s2 -> toStreamK $ par (Stream s1) (Stream s2))
            (toStreamK m1)
            (toStreamK . f)

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
    :: (Stream m b -> Stream m b -> Stream m b)
    -> (a -> Stream m b)
    -> Stream m a
    -> Stream m b
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
-- concatMapFoldableWith f g = Prelude.foldr (f . g) K.nil
-- concatMapFoldableWith f g xs = K.concatMapWith f g (K.fromFoldable xs)
-- @
--
-- /Since: 0.8.0 (Renamed foldMapWith to concatMapFoldableWith)/
--
-- /Since: 0.1.0 ("Streamly")/
{-# INLINE concatMapFoldableWith #-}
concatMapFoldableWith :: Foldable f
    => (Stream m b -> Stream m b -> Stream m b)
    -> (a -> Stream m b)
    -> f a
    -> Stream m b
concatMapFoldableWith f g = Prelude.foldr (f . g) nil

-- | Like 'concatMapFoldableWith' but with the last two arguments reversed i.e. the
-- monadic streaming function is the last argument.
--
-- Equivalent to:
--
-- @
-- concatForFoldableWith f xs g = Prelude.foldr (f . g) K.nil xs
-- concatForFoldableWith f = flip (K.concatMapFoldableWith f)
-- @
--
-- /Since: 0.8.0 (Renamed forEachWith to concatForFoldableWith)/
--
-- /Since: 0.1.0 ("Streamly")/
{-# INLINE concatForFoldableWith #-}
concatForFoldableWith :: Foldable f
    => (Stream m b -> Stream m b -> Stream m b)
    -> f a
    -> (a -> Stream m b)
    -> Stream m b
concatForFoldableWith f = flip (concatMapFoldableWith f)

-- | A variant of 'Data.Foldable.fold' that allows you to fold a 'Foldable'
-- container of streams using the specified stream sum operation.
--
-- @concatFoldableWith 'async' $ map return [1..3]@
--
-- Equivalent to:
--
-- @
-- concatFoldableWith f = Prelude.foldr f K.nil
-- concatFoldableWith f = K.concatMapFoldableWith f id
-- @
--
-- /Since: 0.8.0 (Renamed foldWith to concatFoldableWith)/
--
-- /Since: 0.1.0 ("Streamly")/
{-# INLINE concatFoldableWith #-}
concatFoldableWith :: Foldable f
    => (Stream m a -> Stream m a -> Stream m a)
    -> f (Stream m a)
    -> Stream m a
concatFoldableWith f = concatMapFoldableWith f id

------------------------------------------------------------------------------
-- Folds
------------------------------------------------------------------------------

{-# INLINE foldrM #-}
foldrM ::  (a -> m b -> m b) -> m b -> Stream m a -> m b
foldrM step acc m = K.foldrM step acc $ toStreamK m

{-# INLINE foldrMx #-}
foldrMx :: (Monad m)
    => (a -> m x -> m x) -> m x -> (m x -> m b) -> Stream m a -> m b
foldrMx step final project m = D.foldrMx step final project $ toStreamD m

-- | Like 'foldlx'', but with a monadic step function.
--
-- @since 0.7.0
{-# INLINE foldlMx' #-}
foldlMx' ::
    (Monad m)
    => (x -> a -> m x) -> m x -> (x -> m b) -> Stream m a -> m b
foldlMx' step begin done m = P.foldlMx' step begin done $ toStreamK m

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
--
-- @since 0.7.0
{-# INLINE foldlx' #-}
foldlx' ::
    (Monad m) => (x -> a -> x) -> x -> (x -> b) -> Stream m a -> m b
foldlx' step begin done m = K.foldlx' step begin done $ toStreamK m

------------------------------------------------------------------------------
-- Comparison
------------------------------------------------------------------------------

-- | Compare two streams for equality
--
-- @since 0.5.3
{-# INLINE eqBy #-}
eqBy :: (Monad m) =>
    (a -> b -> Bool) -> Stream m a -> Stream m b -> m Bool
eqBy f m1 m2 = D.eqBy f (toStreamD m1) (toStreamD m2)

-- | Compare two streams
--
-- @since 0.5.3
{-# INLINE cmpBy #-}
cmpBy
    :: (Monad m)
    => (a -> b -> Ordering) -> Stream m a -> Stream m b -> m Ordering
cmpBy f m1 m2 = D.cmpBy f (toStreamD m1) (toStreamD m2)

------------------------------------------------------------------------------
-- Combining
------------------------------------------------------------------------------

infixr 6 `append`

-- | Appends two streams sequentially, yielding all elements from the first
-- stream, and then all elements from the second stream.
--
-- >>> s1 = Stream.unfold Unfold.fromList [1,2]
-- >>> s2 = Stream.unfold Unfold.fromList [3,4]
-- >>> Stream.fold Fold.toList $ s1 `Stream.append` s2
-- [1,2,3,4]
--
-- This operation can be used to fold an infinite lazy container of streams.
--
-- /Pre-release/
--
{-# INLINE append #-}
append :: Stream m a -> Stream m a -> Stream m a
append = (<>)
