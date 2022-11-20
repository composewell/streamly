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
    -- * Stream Type
      Stream

    -- * Type Conversion
    , fromStreamK
    , toStreamK
    , fromStreamD
    , toStreamD
    , Streamly.Internal.Data.Stream.Type.fromList

    -- * Construction
    , cons
    , consM
    , nil
    , nilM
    , fromPure
    , fromEffect

    -- * Applicative
    , crossApply
    , crossApplySnd
    , crossApplyFst

    -- * Bind/Concat
    , bindWith
    , concatMapWith

    -- * Double folds
    , eqBy
    , cmpBy
    )
where

#include "inline.hs"

import Control.Applicative (liftA2)
import Data.Foldable (Foldable(foldl'), fold)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Endo(..))
import GHC.Exts (IsList(..), IsString(..), oneShot)
import Streamly.Internal.BaseCompat ((#.))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Text.Read
       ( Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec
       , readListPrecDefault)

import qualified Streamly.Internal.Data.Stream.Common as P
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

-- $setup
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Stream as Stream

------------------------------------------------------------------------------
-- Stream
------------------------------------------------------------------------------

-- | Semigroup instance appends two streams:
--
-- >>> (<>) = Stream.append
--
newtype Stream m a = Stream (K.Stream m a)
    -- XXX when deriving do we inherit an INLINE?
    deriving (Semigroup, Monoid)

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

{-# INLINE_EARLY fromStreamK #-}
fromStreamK :: K.Stream m a -> Stream m a
fromStreamK = Stream

{-# INLINE_EARLY toStreamK #-}
toStreamK :: Stream m a -> K.Stream m a
toStreamK (Stream k) = k

{-# INLINE_EARLY fromStreamD #-}
fromStreamD :: Monad m => D.Stream m a -> Stream m a
fromStreamD = fromStreamK . D.toStreamK

{-# INLINE_EARLY toStreamD #-}
toStreamD :: Applicative m => Stream m a -> D.Stream m a
toStreamD = D.fromStreamK . toStreamK

------------------------------------------------------------------------------
-- Generation
------------------------------------------------------------------------------

-- |
-- >>> fromList = Prelude.foldr Stream.cons Stream.nil
--
-- Construct a stream from a list of pure values. This is more efficient than
-- 'fromFoldable'.
--
{-# INLINE fromList #-}
fromList :: Monad m => [a] -> Stream m a
fromList = fromStreamK . P.fromList

------------------------------------------------------------------------------
-- Comparison
------------------------------------------------------------------------------

-- | Compare two streams for equality
--
{-# INLINE eqBy #-}
eqBy :: Monad m =>
    (a -> b -> Bool) -> Stream m a -> Stream m b -> m Bool
eqBy f m1 m2 = D.eqBy f (toStreamD m1) (toStreamD m2)

-- | Compare two streams
--
{-# INLINE cmpBy #-}
cmpBy
    :: Monad m
    => (a -> b -> Ordering) -> Stream m a -> Stream m b -> m Ordering
cmpBy f m1 m2 = D.cmpBy f (toStreamD m1) (toStreamD m2)

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance Monad m => Functor (Stream m) where
    {-# INLINE fmap #-}
    -- IMPORTANT: do not use eta reduction.
    fmap f m = fromStreamD $ D.mapM (return . f) $ toStreamD m

    {-# INLINE (<$) #-}
    (<$) = fmap . const

------------------------------------------------------------------------------
-- Lists
------------------------------------------------------------------------------

-- Serial streams can act like regular lists using the Identity monad

-- XXX Show instance is 10x slower compared to read, we can do much better.
-- The list show instance itself is really slow.

-- XXX The default definitions of "<" in the Ord instance etc. do not perform
-- well, because they do not get inlined. Need to add INLINE in Ord class in
-- base?

instance IsList (Stream Identity a) where
    type (Item (Stream Identity a)) = a

    {-# INLINE fromList #-}
    fromList xs = Stream $ P.fromList xs

    {-# INLINE toList #-}
    toList (Stream xs) = runIdentity $ P.toList xs

instance Eq a => Eq (Stream Identity a) where
    {-# INLINE (==) #-}
    (==) (Stream xs) (Stream ys) = runIdentity $ P.eqBy (==) xs ys

instance Ord a => Ord (Stream Identity a) where
    {-# INLINE compare #-}
    compare (Stream xs) (Stream ys) = runIdentity $ P.cmpBy compare xs ys

    {-# INLINE (<) #-}
    x < y =
        case compare x y of
            LT -> True
            _ -> False

    {-# INLINE (<=) #-}
    x <= y =
        case compare x y of
            GT -> False
            _ -> True

    {-# INLINE (>) #-}
    x > y =
        case compare x y of
            GT -> True
            _ -> False

    {-# INLINE (>=) #-}
    x >= y =
        case compare x y of
            LT -> False
            _ -> True

    {-# INLINE max #-}
    max x y = if x <= y then y else x

    {-# INLINE min #-}
    min x y = if x <= y then x else y

instance Show a => Show (Stream Identity a) where
    showsPrec p dl = showParen (p > 10) $
        showString "fromList " . shows (toList dl)

instance Read a => Read (Stream Identity a) where
    readPrec = parens $ prec 10 $ do
        Ident "fromList" <- lexP
        Streamly.Internal.Data.Stream.Type.fromList <$> readPrec

    readListPrec = readListPrecDefault

instance (a ~ Char) => IsString (Stream Identity a) where
    {-# INLINE fromString #-}
    fromString xs = Stream $ P.fromList xs

-------------------------------------------------------------------------------
-- Foldable
-------------------------------------------------------------------------------

-- The default Foldable instance has several issues:
-- 1) several definitions do not have INLINE on them, so we provide
--    re-implementations with INLINE pragmas.
-- 2) the definitions of sum/product/maximum/minimum are inefficient as they
--    use right folds, they cannot run in constant memory. We provide
--    implementations using strict left folds here.

instance (Foldable m, Monad m) => Foldable (Stream m) where

    {-# INLINE foldMap #-}
    foldMap f (Stream xs) = fold $ P.foldr (mappend . f) mempty xs

    {-# INLINE foldr #-}
    foldr f z t = appEndo (foldMap (Endo #. f) t) z

    {-# INLINE foldl' #-}
    foldl' f z0 xs = foldr f' id xs z0
        where f' x k = oneShot $ \z -> k $! f z x

    {-# INLINE length #-}
    length = foldl' (\n _ -> n + 1) 0

    {-# INLINE elem #-}
    elem = any . (==)

    {-# INLINE maximum #-}
    maximum =
          fromMaybe (errorWithoutStackTrace "maximum: empty stream")
        . toMaybe
        . foldl' getMax Nothing'

        where

        getMax Nothing' x = Just' x
        getMax (Just' mx) x = Just' $! max mx x

    {-# INLINE minimum #-}
    minimum =
          fromMaybe (errorWithoutStackTrace "minimum: empty stream")
        . toMaybe
        . foldl' getMin Nothing'

        where

        getMin Nothing' x = Just' x
        getMin (Just' mn) x = Just' $! min mn x

    {-# INLINE sum #-}
    sum = foldl' (+) 0

    {-# INLINE product #-}
    product = foldl' (*) 1

-------------------------------------------------------------------------------
-- Traversable
-------------------------------------------------------------------------------

instance Traversable (Stream Identity) where
    {-# INLINE traverse #-}
    traverse f (Stream xs) =
        fmap Stream $ runIdentity $ P.foldr consA (pure mempty) xs

        where

        consA x ys = liftA2 K.cons (f x) ys

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

infixr 5 `cons`

-- | A right associative prepend operation to add a pure value at the head of
-- an existing stream::
--
-- >>> s = 1 `Stream.cons` 2 `Stream.cons` 3 `Stream.cons` Stream.nil
-- >>> Stream.fold Fold.toList s
-- [1,2,3]
--
-- It can be used efficiently with 'Prelude.foldr':
--
-- >>> fromFoldable = Prelude.foldr Stream.cons Stream.nil
--
-- Same as the following but more efficient:
--
-- >>> cons x xs = return x `Stream.consM` xs
--
-- /Not fused/
--
{-# INLINE_NORMAL cons #-}
cons ::  a -> Stream m a -> Stream m a
cons x = fromStreamK . K.cons x . toStreamK

infixr 5 `consM`

-- | A right associative prepend operation to add an effectful value at the
-- head of an existing stream::
--
-- >>> s = putStrLn "hello" `consM` putStrLn "world" `consM` Stream.nil
-- >>> Stream.fold Fold.drain s
-- hello
-- world
--
-- It can be used efficiently with 'Prelude.foldr':
--
-- >>> fromFoldableM = Prelude.foldr Stream.consM Stream.nil
--
-- Same as the following but more efficient:
--
-- >>> consM x xs = Stream.fromEffect x `Stream.append` xs
--
-- /Not fused/
--
{-# INLINE consM #-}
{-# SPECIALIZE consM :: IO a -> Stream IO a -> Stream IO a #-}
consM :: Monad m => m a -> Stream m a -> Stream m a
consM m = fromStreamK . K.consM m . toStreamK

-- | A stream that terminates without producing any output or side effect.
--
-- >>> Stream.fold Fold.toList Stream.nil
-- []
--
{-# INLINE_NORMAL nil #-}
nil ::  Stream m a
nil = fromStreamK K.nil

-- | A stream that terminates without producing any output, but produces a side
-- effect.
--
-- >>> Stream.fold Fold.toList (Stream.nilM (print "nil"))
-- "nil"
-- []
--
-- /Pre-release/
{-# INLINE_NORMAL nilM #-}
nilM :: Monad m => m b -> Stream m a
nilM = fromStreamK . K.nilM

-- | Create a singleton stream from a pure value.
--
-- >>> fromPure a = a `cons` Stream.nil
-- >>> fromPure = pure
-- >>> fromPure = fromEffect . pure
--
{-# INLINE_NORMAL fromPure #-}
fromPure :: a -> Stream m a
fromPure = fromStreamK . K.fromPure

-- | Create a singleton stream from a monadic action.
--
-- >>> fromEffect m = m `consM` Stream.nil
-- >>> fromEffect = Stream.sequence . Stream.fromPure
--
-- >>> Stream.fold Fold.drain $ Stream.fromEffect (putStrLn "hello")
-- hello
--
{-# INLINE_NORMAL fromEffect #-}
fromEffect :: Monad m => m a -> Stream m a
fromEffect = fromStreamK . K.fromEffect

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

{-# INLINE crossApply #-}
crossApply :: Monad m => Stream m (a -> b) -> Stream m a -> Stream m b
crossApply m1 m2 =
    fromStreamD $ D.crossApply (toStreamD m1) (toStreamD m2)

{-# INLINE crossApplySnd #-}
crossApplySnd :: Monad m => Stream m a -> Stream m b -> Stream m b
crossApplySnd m1 m2 =
    fromStreamD $ D.crossApplySnd (toStreamD m1) (toStreamD m2)

{-# INLINE crossApplyFst #-}
crossApplyFst :: Monad m => Stream m a -> Stream m b -> Stream m a
crossApplyFst m1 m2 =
    fromStreamD $ D.crossApplyFst (toStreamD m1) (toStreamD m2)

-------------------------------------------------------------------------------
-- Bind/Concat
-------------------------------------------------------------------------------

-- |
--
-- /CPS/
{-# INLINE bindWith #-}
bindWith
    :: (Stream m b -> Stream m b -> Stream m b)
    -> Stream m a
    -> (a -> Stream m b)
    -> Stream m b
bindWith par m1 f =
    fromStreamK
        $ K.bindWith
            (\s1 s2 -> toStreamK $ par (fromStreamK s1) (fromStreamK s2))
            (toStreamK m1)
            (toStreamK . f)

-- | @concatMapWith mixer generator stream@ is a two dimensional looping
-- combinator.  The @generator@ function is used to generate streams from the
-- elements in the input @stream@ and the @mixer@ function is used to merge
-- those streams.
--
-- /CPS/
{-# INLINE concatMapWith #-}
concatMapWith
    :: (Stream m b -> Stream m b -> Stream m b)
    -> (a -> Stream m b)
    -> Stream m a
    -> Stream m b
concatMapWith par f xs = bindWith par xs f
