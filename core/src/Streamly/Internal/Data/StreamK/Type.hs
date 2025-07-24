{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
-- Must come after TypeFamilies, otherwise it is re-enabled.
-- MonoLocalBinds enabled by TypeFamilies causes perf regressions in general.
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Streamly.Internal.Data.StreamK.Type
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
-- Continuation passing style (CPS) stream implementation. The symbol 'K' below
-- denotes a function as well as a Kontinuation.
--
module Streamly.Internal.Data.StreamK.Type
    (
    -- * StreamK type
      Stream
    , StreamK (..)

    -- * Nested type wrapper
    , Nested(..)
    , FairNested(..) -- experimental, do not release, associativity issues

    -- * foldr/build Fusion
    , mkStream
    , foldStream
    , foldStreamShared
    , foldrM
    , foldrS
    , foldrSShared
    , foldrSM
    , build
    , buildS
    , buildM
    , buildSM
    , augmentS
    , augmentSM
    , unShare

    -- * Construction
    -- ** Primitives
    , fromStopK
    , fromYieldK
    , consK
    , cons
    , (.:)
    , consM
    , consMBy
    , nil
    , nilM

    -- ** Unfolding
    , unfoldr
    , unfoldrMWith
    , unfoldrM

    -- ** From Values
    , fromEffect
    , fromPure
    , repeat
    , repeatMWith
    , replicateMWith

    -- ** From Indices
    , fromIndicesMWith

    -- ** Iteration
    , iterateMWith

    -- ** From Containers
    , fromFoldable
    , fromFoldableM
    , Streamly.Internal.Data.StreamK.Type.fromList

    -- ** Cyclic
    , mfix

    -- * Elimination
    -- ** Primitives
    , uncons

    -- ** Strict Left Folds
    , Streamly.Internal.Data.StreamK.Type.foldl'
    , foldlx'
    , foldlMx'
    , foldlM'

    -- ** Lazy Right Folds
    , Streamly.Internal.Data.StreamK.Type.foldr

    -- ** Specific Folds
    , drain
    , null
    , headNonEmpty
    , tail
    , tailNonEmpty
    , init
    , initNonEmpty

    -- * Mapping
    , map
    , mapMWith
    , mapMSerial
    , mapMAccum

    -- * Combining Two Streams
    -- ** Appending
    , conjoin
    , append

    -- ** Interleave
    , interleave
    , interleaveEndBy'
    , interleaveSepBy

    -- ** Cross Product
    , crossApplyWith
    , crossApply
    , crossApplySnd
    , crossApplyFst
    , crossWith
    , cross

    -- * Concat
    , before
    , concatEffect
    , concatMapEffect
    , concatMapWith
    , bindWith
    , concatMap
    , bfsConcatMap
    , fairConcatMap
    , concatMapMAccum
    , concatIterateWith
    , concatIterateLeftsWith
    , concatIterateScanWith

    -- * Merge
    , mergeMapWith
    , mergeIterateWith

    -- * Buffered Operations
    , foldlS
    , reverse

    -- * Deprecated
    , interleaveFst
    , interleaveMin
    , CrossStreamK
    , mkCross
    , unCross
    )
where

#include "inline.hs"

import Control.Applicative (Alternative(..))
import Control.Monad ((>=>), ap, MonadPlus(..))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Class (MonadTrans(lift))
#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (Foldable(foldl'), fold, foldr)
import Data.Function (fix)
import Data.Functor.Identity (Identity(..))
#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type)
#endif
import Data.Maybe (fromMaybe)
import Data.Semigroup (Endo(..))
import GHC.Exts (IsList(..), IsString(..), oneShot, inline)
import Streamly.Internal.BaseCompat ((#.))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.SVar.Type (State, adaptState, defState)
import Text.Read
       ( Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec
       , readListPrecDefault)

import qualified Control.Monad.Fail as Fail
import qualified Prelude

import Prelude hiding
    (map, mapM, concatMap, foldr, repeat, null, reverse, tail, init)

#include "DocTestDataStreamK.hs"

------------------------------------------------------------------------------
-- Basic stream type
------------------------------------------------------------------------------

-- It uses stop, singleton and yield continuations equivalent to the following
-- direct style type:
--
-- @
-- data StreamK m a = Stop | Singleton a | Yield a (StreamK m a)
-- @
--
-- To facilitate parallel composition we maintain a local state in an 'SVar'
-- that is shared across and is used for synchronization of the streams being
-- composed.
--
-- The singleton case can be expressed in terms of stop and yield but we have
-- it as a separate case to optimize composition operations for streams with
-- single element.  We build singleton streams in the implementation of 'pure'
-- for Applicative and Monad, and in 'lift' for MonadTrans.

-- XXX can we replace it with a direct style type? With foldr/build fusion.
-- StreamK (m (Maybe (a, StreamK m a)))
-- XXX remove the State param.

-- | Continuation Passing Style (CPS) version of "Streamly.Data.Stream.Stream".
-- Unlike "Streamly.Data.Stream.Stream", 'StreamK' can be composed recursively
-- without affecting performance.
--
-- Semigroup instance appends two streams:
--
-- >>> (<>) = Stream.append
--
{-# DEPRECATED Stream "Please use StreamK instead." #-}
type Stream = StreamK

newtype StreamK m a =
    MkStream (forall r.
               State StreamK m a         -- state
            -> (a -> StreamK m a -> m r) -- yield
            -> (a -> m r)               -- singleton
            -> m r                      -- stop
            -> m r
            )

mkStream
    :: (forall r. State StreamK m a
        -> (a -> StreamK m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r)
    -> StreamK m a
mkStream = MkStream

-- | A terminal function that has no continuation to follow.
#if __GLASGOW_HASKELL__ >= 810
type StopK :: (Type -> Type) -> Type
#endif
type StopK m = forall r. m r -> m r

-- | A monadic continuation, it is a function that yields a value of type "a"
-- and calls the argument (a -> m r) as a continuation with that value. We can
-- also think of it as a callback with a handler (a -> m r).  Category
-- theorists call it a codensity type, a special type of right kan extension.
#if __GLASGOW_HASKELL__ >= 810
type YieldK :: (Type -> Type) -> Type -> Type
#endif
type YieldK m a = forall r. (a -> m r) -> m r

_wrapM :: Monad m => m a -> YieldK m a
_wrapM m = (m >>=)

-- | Make an empty stream from a stop function.
fromStopK :: StopK m -> StreamK m a
fromStopK k = mkStream $ \_ _ _ stp -> k stp

-- | Make a singleton stream from a callback function. The callback function
-- calls the one-shot yield continuation to yield an element.
fromYieldK :: YieldK m a -> StreamK m a
fromYieldK k = mkStream $ \_ _ sng _ -> k sng

-- | Add a yield function at the head of the stream.
consK :: YieldK m a -> StreamK m a -> StreamK m a
consK k r = mkStream $ \_ yld _ _ -> k (`yld` r)

-- XXX Build a stream from a repeating callback function.

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

infixr 5 `cons`

-- faster than consM because there is no bind.

-- | A right associative prepend operation to add a pure value at the head of
-- an existing stream:
--
-- >>> s = 1 `StreamK.cons` 2 `StreamK.cons` 3 `StreamK.cons` StreamK.nil
-- >>> Stream.fold Fold.toList (StreamK.toStream s)
-- [1,2,3]
--
-- Unlike "Streamly.Data.Stream" cons StreamK cons can be used
-- recursively:
--
-- >>> repeat x = let xs = StreamK.cons x xs in xs
-- >>> fromFoldable = Prelude.foldr StreamK.cons StreamK.nil
--
-- cons is same as the following but more efficient:
--
-- >>> cons x xs = return x `StreamK.consM` xs
--
{-# INLINE_NORMAL cons #-}
cons :: a -> StreamK m a -> StreamK m a
cons a r = mkStream $ \_ yield _ _ -> yield a r

infixr 5 .:

-- | Operator equivalent of 'cons'.
--
-- @
-- > toList $ 1 .: 2 .: 3 .: nil
-- [1,2,3]
-- @
--
{-# INLINE (.:) #-}
(.:) :: a -> StreamK m a -> StreamK m a
(.:) = cons

-- | A stream that terminates without producing any output or side effect.
--
-- >>> Stream.fold Fold.toList (StreamK.toStream StreamK.nil)
-- []
--
{-# INLINE_NORMAL nil #-}
nil :: StreamK m a
nil = mkStream $ \_ _ _ stp -> stp

-- | A stream that terminates without producing any output, but produces a side
-- effect.
--
-- >>> Stream.fold Fold.toList (StreamK.toStream (StreamK.nilM (print "nil")))
-- "nil"
-- []
--
-- /Pre-release/
{-# INLINE_NORMAL nilM #-}
nilM :: Applicative m => m b -> StreamK m a
nilM m = mkStream $ \_ _ _ stp -> m *> stp

-- Create a singleton stream from a pure value.
--
-- >>> fromPure a = a `StreamK.cons` StreamK.nil
-- >>> fromPure = pure
-- >>> fromPure = StreamK.fromEffect . pure
--
{-# INLINE_NORMAL fromPure #-}
fromPure :: a -> StreamK m a
fromPure a = mkStream $ \_ _ single _ -> single a

-- Create a singleton stream from a monadic action.
--
-- >>> fromEffect m = m `StreamK.consM` StreamK.nil
--
-- >>> Stream.fold Fold.drain $ StreamK.toStream $ StreamK.fromEffect (putStrLn "hello")
-- hello
--
{-# INLINE_NORMAL fromEffect #-}
fromEffect :: Monad m => m a -> StreamK m a
fromEffect m = mkStream $ \_ _ single _ -> m >>= single

infixr 5 `consM`

-- NOTE: specializing the function outside the instance definition seems to
-- improve performance quite a bit at times, even if we have the same
-- SPECIALIZE in the instance definition.

-- | A right associative prepend operation to add an effectful value at the
-- head of an existing stream::
--
-- >>> s = putStrLn "hello" `StreamK.consM` putStrLn "world" `StreamK.consM` StreamK.nil
-- >>> Stream.fold Fold.drain (StreamK.toStream s)
-- hello
-- world
--
-- It can be used efficiently with 'Prelude.foldr':
--
-- >>> fromFoldableM = Prelude.foldr StreamK.consM StreamK.nil
--
-- Same as the following but more efficient:
--
-- >>> consM x xs = StreamK.fromEffect x `StreamK.append` xs
--
{-# INLINE consM #-}
{-# SPECIALIZE consM :: IO a -> StreamK IO a -> StreamK IO a #-}
consM :: Monad m => m a -> StreamK m a -> StreamK m a
consM m r = MkStream $ \_ yld _ _ -> m >>= (`yld` r)

-- XXX specialize to IO?
{-# INLINE consMBy #-}
consMBy :: Monad m =>
    (StreamK m a -> StreamK m a -> StreamK m a) -> m a -> StreamK m a -> StreamK m a
consMBy f m r = fromEffect m `f` r

------------------------------------------------------------------------------
-- Folding a stream
------------------------------------------------------------------------------

-- | Fold a stream by providing an SVar, a stop continuation, a singleton
-- continuation and a yield continuation. The stream would share the current
-- SVar passed via the State.
{-# INLINE_EARLY foldStreamShared #-}
foldStreamShared
    :: State StreamK m a
    -> (a -> StreamK m a -> m r)
    -> (a -> m r)
    -> m r
    -> StreamK m a
    -> m r
foldStreamShared s yield single stop (MkStream k) = k s yield single stop

-- | Fold a stream by providing a State, stop continuation, a singleton
-- continuation and a yield continuation. The stream will not use the SVar
-- passed via State.
{-# INLINE foldStream #-}
foldStream
    :: State StreamK m a
    -> (a -> StreamK m a -> m r)
    -> (a -> m r)
    -> m r
    -> StreamK m a
    -> m r
foldStream s yield single stop (MkStream k) =
    k (adaptState s) yield single stop

-------------------------------------------------------------------------------
-- foldr/build fusion
-------------------------------------------------------------------------------

-- XXX perhaps we can just use foldrSM/buildM everywhere as they are more
-- general and cover foldrS/buildS as well.

-- | The function 'f' decides how to reconstruct the stream. We could
-- reconstruct using a shared state (SVar) or without sharing the state.
--
{-# INLINE foldrSWith #-}
foldrSWith ::
    (forall r. State StreamK m b
        -> (b -> StreamK m b -> m r)
        -> (b -> m r)
        -> m r
        -> StreamK m b
        -> m r)
    -> (a -> StreamK m b -> StreamK m b)
    -> StreamK m b
    -> StreamK m a
    -> StreamK m b
foldrSWith f step final m = go m
    where
    go m1 = mkStream $ \st yld sng stp ->
        let run x = f st yld sng stp x
            stop = run final
            single a = run $ step a final
            yieldk a r = run $ step a (go r)
         -- XXX if type a and b are the same we do not need adaptState, can we
         -- save some perf with that?
         -- XXX since we are using adaptState anyway here we can use
         -- foldStreamShared instead, will that save some perf?
         in foldStream (adaptState st) yieldk single stop m1

-- XXX we can use rewrite rules just for foldrSWith, if the function f is the
-- same we can rewrite it.

-- | Fold sharing the SVar state within the reconstructed stream
{-# INLINE_NORMAL foldrSShared #-}
foldrSShared ::
       (a -> StreamK m b -> StreamK m b)
    -> StreamK m b
    -> StreamK m a
    -> StreamK m b
foldrSShared = foldrSWith foldStreamShared

-- XXX consM is a typeclass method, therefore rewritten already. Instead maybe
-- we can make consM polymorphic using rewrite rules.
-- {-# RULES "foldrSShared/id"     foldrSShared consM nil = \x -> x #-}
{-# RULES "foldrSShared/nil"
    forall k z. foldrSShared k z nil = z #-}
{-# RULES "foldrSShared/single"
    forall k z x. foldrSShared k z (fromPure x) = k x z #-}
-- {-# RULES "foldrSShared/app" [1]
--     forall ys. foldrSShared consM ys = \xs -> xs `conjoin` ys #-}

-- | Right fold to a streaming monad.
--
-- > foldrS StreamK.cons StreamK.nil === id
--
-- 'foldrS' can be used to perform stateless stream to stream transformations
-- like map and filter in general. It can be coupled with a scan to perform
-- stateful transformations. However, note that the custom map and filter
-- routines can be much more efficient than this due to better stream fusion.
--
-- >>> input = StreamK.fromStream $ Stream.fromList [1..5]
-- >>> Stream.fold Fold.toList $ StreamK.toStream $ StreamK.foldrS StreamK.cons StreamK.nil input
-- [1,2,3,4,5]
--
-- Find if any element in the stream is 'True':
--
-- >>> step x xs = if odd x then StreamK.fromPure True else xs
-- >>> input = StreamK.fromStream (Stream.fromList (2:4:5:undefined)) :: StreamK IO Int
-- >>> Stream.fold Fold.toList $ StreamK.toStream $ StreamK.foldrS step (StreamK.fromPure False) input
-- [True]
--
-- Map (+2) on odd elements and filter out the even elements:
--
-- >>> step x xs = if odd x then (x + 2) `StreamK.cons` xs else xs
-- >>> input = StreamK.fromStream (Stream.fromList [1..5]) :: StreamK IO Int
-- >>> Stream.fold Fold.toList $ StreamK.toStream $ StreamK.foldrS step StreamK.nil input
-- [3,5,7]
--
-- /Pre-release/
{-# INLINE_NORMAL foldrS #-}
foldrS ::
       (a -> StreamK m b -> StreamK m b)
    -> StreamK m b
    -> StreamK m a
    -> StreamK m b
foldrS = foldrSWith foldStream

{-# RULES "foldrS/id"     foldrS cons nil = \x -> x #-}
{-# RULES "foldrS/nil"    forall k z.   foldrS k z nil  = z #-}
-- See notes in GHC.Base about this rule
-- {-# RULES "foldr/cons"
--  forall k z x xs. foldrS k z (x `cons` xs) = k x (foldrS k z xs) #-}
{-# RULES "foldrS/single" forall k z x. foldrS k z (fromPure x) = k x z #-}
-- {-# RULES "foldrS/app" [1]
--  forall ys. foldrS cons ys = \xs -> xs `conjoin` ys #-}

-------------------------------------------------------------------------------
-- foldrS with monadic cons i.e. consM
-------------------------------------------------------------------------------

{-# INLINE foldrSMWith #-}
foldrSMWith :: Monad m
    => (forall r. State StreamK m b
        -> (b -> StreamK m b -> m r)
        -> (b -> m r)
        -> m r
        -> StreamK m b
        -> m r)
    -> (m a -> StreamK m b -> StreamK m b)
    -> StreamK m b
    -> StreamK m a
    -> StreamK m b
foldrSMWith f step final m = go m
    where
    go m1 = mkStream $ \st yld sng stp ->
        let run x = f st yld sng stp x
            stop = run final
            single a = run $ step (return a) final
            yieldk a r = run $ step (return a) (go r)
         in foldStream (adaptState st) yieldk single stop m1

{-# INLINE_NORMAL foldrSM #-}
foldrSM :: Monad m
    => (m a -> StreamK m b -> StreamK m b)
    -> StreamK m b
    -> StreamK m a
    -> StreamK m b
foldrSM = foldrSMWith foldStream

-- {-# RULES "foldrSM/id"     foldrSM consM nil = \x -> x #-}
{-# RULES "foldrSM/nil"    forall k z.   foldrSM k z nil  = z #-}
{-# RULES "foldrSM/single" forall k z x. foldrSM k z (fromEffect x) = k x z #-}
-- {-# RULES "foldrSM/app" [1]
--  forall ys. foldrSM consM ys = \xs -> xs `conjoin` ys #-}

-- Like foldrSM but sharing the SVar state within the recostructed stream.
{-# INLINE_NORMAL foldrSMShared #-}
foldrSMShared :: Monad m
    => (m a -> StreamK m b -> StreamK m b)
    -> StreamK m b
    -> StreamK m a
    -> StreamK m b
foldrSMShared = foldrSMWith foldStreamShared

-- {-# RULES "foldrSM/id"     foldrSM consM nil = \x -> x #-}
{-# RULES "foldrSMShared/nil"
    forall k z. foldrSMShared k z nil = z #-}
{-# RULES "foldrSMShared/single"
    forall k z x. foldrSMShared k z (fromEffect x) = k x z #-}
-- {-# RULES "foldrSM/app" [1]
--  forall ys. foldrSM consM ys = \xs -> xs `conjoin` ys #-}

-------------------------------------------------------------------------------
-- build
-------------------------------------------------------------------------------

{-# INLINE_NORMAL build #-}
build :: forall m a. (forall b. (a -> b -> b) -> b -> b) -> StreamK m a
build g = g cons nil

{-# RULES "foldrM/build"
    forall k z (g :: forall b. (a -> b -> b) -> b -> b).
    foldrM k z (build g) = g k z #-}

{-# RULES "foldrS/build"
      forall k z (g :: forall b. (a -> b -> b) -> b -> b).
      foldrS k z (build g) = g k z #-}

{-# RULES "foldrS/cons/build"
      forall k z x (g :: forall b. (a -> b -> b) -> b -> b).
      foldrS k z (x `cons` build g) = k x (g k z) #-}

{-# RULES "foldrSShared/build"
      forall k z (g :: forall b. (a -> b -> b) -> b -> b).
      foldrSShared k z (build g) = g k z #-}

{-# RULES "foldrSShared/cons/build"
      forall k z x (g :: forall b. (a -> b -> b) -> b -> b).
      foldrSShared k z (x `cons` build g) = k x (g k z) #-}

-- build a stream by applying cons and nil to a build function
{-# INLINE_NORMAL buildS #-}
buildS ::
       ((a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a)
    -> StreamK m a
buildS g = g cons nil

{-# RULES "foldrS/buildS"
      forall k z
        (g :: (a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a).
      foldrS k z (buildS g) = g k z #-}

{-# RULES "foldrS/cons/buildS"
      forall k z x
        (g :: (a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a).
      foldrS k z (x `cons` buildS g) = k x (g k z) #-}

{-# RULES "foldrSShared/buildS"
      forall k z
        (g :: (a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a).
      foldrSShared k z (buildS g) = g k z #-}

{-# RULES "foldrSShared/cons/buildS"
      forall k z x
        (g :: (a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a).
      foldrSShared k z (x `cons` buildS g) = k x (g k z) #-}

-- build a stream by applying consM and nil to a build function
{-# INLINE_NORMAL buildSM #-}
buildSM :: Monad m
    => ((m a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a)
    -> StreamK m a
buildSM g = g consM nil

{-# RULES "foldrSM/buildSM"
     forall k z
        (g :: (m a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a).
     foldrSM k z (buildSM g) = g k z #-}

{-# RULES "foldrSMShared/buildSM"
     forall k z
        (g :: (m a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a).
     foldrSMShared k z (buildSM g) = g k z #-}

-- Disabled because this may not fire as consM is a class Op
{-
{-# RULES "foldrS/consM/buildSM"
      forall k z x (g :: (m a -> t m a -> t m a) -> t m a -> t m a)
    . foldrSM k z (x `consM` buildSM g)
    = k x (g k z)
#-}
-}

-- Build using monadic build functions (continuations) instead of
-- reconstructing a stream.
{-# INLINE_NORMAL buildM #-}
buildM :: Monad m
    => (forall r. (a -> StreamK m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r
       )
    -> StreamK m a
buildM g = mkStream $ \st yld sng stp ->
    g (\a r -> foldStream st yld sng stp (return a `consM` r)) sng stp

-- | Like 'buildM' but shares the SVar state across computations.
{-# INLINE_NORMAL sharedMWith #-}
sharedMWith :: Monad m
    => (m a -> StreamK m a -> StreamK m a)
    -> (forall r. (a -> StreamK m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r
       )
    -> StreamK m a
sharedMWith cns g = mkStream $ \st yld sng stp ->
    g (\a r -> foldStreamShared st yld sng stp (return a `cns` r)) sng stp

-------------------------------------------------------------------------------
-- augment
-------------------------------------------------------------------------------

{-# INLINE_NORMAL augmentS #-}
augmentS ::
       ((a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a)
    -> StreamK m a
    -> StreamK m a
augmentS g xs = g cons xs

{-# RULES "augmentS/nil"
    forall (g :: (a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a).
    augmentS g nil = buildS g
    #-}

{-# RULES "foldrS/augmentS"
    forall k z xs
        (g :: (a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a).
    foldrS k z (augmentS g xs) = g k (foldrS k z xs)
    #-}

{-# RULES "augmentS/buildS"
    forall (g :: (a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a)
           (h :: (a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a).
    augmentS g (buildS h) = buildS (\c n -> g c (h c n))
    #-}

{-# INLINE_NORMAL augmentSM #-}
augmentSM :: Monad m =>
       ((m a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a)
    -> StreamK m a -> StreamK m a
augmentSM g xs = g consM xs

{-# RULES "augmentSM/nil"
    forall
        (g :: (m a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a).
    augmentSM g nil = buildSM g
    #-}

{-# RULES "foldrSM/augmentSM"
    forall k z xs
        (g :: (m a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a).
    foldrSM k z (augmentSM g xs) = g k (foldrSM k z xs)
    #-}

{-# RULES "augmentSM/buildSM"
    forall
        (g :: (m a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a)
        (h :: (m a -> StreamK m a -> StreamK m a) -> StreamK m a -> StreamK m a).
    augmentSM g (buildSM h) = buildSM (\c n -> g c (h c n))
    #-}

-------------------------------------------------------------------------------
-- Experimental foldrM/buildM
-------------------------------------------------------------------------------

-- | Lazy right fold with a monadic step function.
{-# INLINE_NORMAL foldrM #-}
foldrM :: (a -> m b -> m b) -> m b -> StreamK m a -> m b
foldrM step acc m = go m
    where
    go m1 =
        let stop = acc
            single a = step a acc
            yieldk a r = step a (go r)
        in foldStream defState yieldk single stop m1

{-# INLINE_NORMAL foldrMKWith #-}
foldrMKWith
    :: (State StreamK m a
        -> (a -> StreamK m a -> m b)
        -> (a -> m b)
        -> m b
        -> StreamK m a
        -> m b)
    -> (a -> m b -> m b)
    -> m b
    -> ((a -> StreamK m a -> m b) -> (a -> m b) -> m b -> m b)
    -> m b
foldrMKWith f step acc = go
    where
    go k =
        let stop = acc
            single a = step a acc
            yieldk a r = step a (go (\yld sng stp -> f defState yld sng stp r))
        in k yieldk single stop

{-
{-# RULES "foldrM/buildS"
      forall k z (g :: (a -> t m a -> t m a) -> t m a -> t m a)
    . foldrM k z (buildS g)
    = g k z
#-}
-}
-- XXX in which case will foldrM/buildM fusion be useful?
{-# RULES "foldrM/buildM"
    forall step acc (g :: (forall r.
           (a -> StreamK m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r
       )).
    foldrM step acc (buildM g) = foldrMKWith foldStream step acc g
    #-}

{-
{-# RULES "foldrM/sharedM"
    forall step acc (g :: (forall r.
           (a -> StreamK m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r
       )).
    foldrM step acc (sharedM g) = foldrMKWith foldStreamShared step acc g
    #-}
-}

------------------------------------------------------------------------------
-- Left fold
------------------------------------------------------------------------------

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
--
-- Note that the accumulator is always evaluated including the initial value.
{-# INLINE foldlx' #-}
foldlx' :: forall m a b x. Monad m
    => (x -> a -> x) -> x -> (x -> b) -> StreamK m a -> m b
foldlx' step begin done =
    foldlMx' (\x a -> return (step x a)) (return begin) (return . done)

-- | Strict left associative fold.
{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> StreamK m a -> m b
foldl' step begin = foldlx' step begin id

-- | Like 'foldx', but with a monadic step function.
{-# INLINE foldlMx' #-}
foldlMx' :: Monad m
    => (x -> a -> m x) -> m x -> (x -> m b) -> StreamK m a -> m b
foldlMx' step begin done stream =
    -- Note: Unrolling improves the last benchmark significantly.
    let stop = begin >>= done
        single a = begin >>= \x -> step x a >>= done
        yieldk a r = begin >>= \x -> step x a >>= go r
     in foldStream defState yieldk single stop stream

    where

    go m1 !acc =
        let stop = done $! acc
            single a = step acc a >>= done
            yieldk a r = step acc a >>= go r
         in foldStream defState yieldk single stop m1

-- | Like 'foldl'' but with a monadic step function.
{-# INLINE foldlM' #-}
foldlM' :: Monad m => (b -> a -> m b) -> m b -> StreamK m a -> m b
foldlM' step begin = foldlMx' step begin return

------------------------------------------------------------------------------
-- Specialized folds
------------------------------------------------------------------------------

-- XXX use foldrM to implement folds where possible
-- XXX This (commented) definition of drain and mapM_ perform much better on
-- some benchmarks but worse on others. Need to investigate why, maybe there is
-- an optimization opportunity that we can exploit.
-- drain = foldrM (\_ xs -> return () >> xs) (return ())

--
-- > drain = foldl' (\_ _ -> ()) ()
-- > drain = mapM_ (\_ -> return ())
{-# INLINE drain #-}
drain :: Monad m => StreamK m a -> m ()
drain = foldrM (\_ xs -> xs) (return ())
{-
drain = go
    where
    go m1 =
        let stop = return ()
            single _ = return ()
            yieldk _ r = go r
         in foldStream defState yieldk single stop m1
-}

{-# INLINE null #-}
null :: Monad m => StreamK m a -> m Bool
-- null = foldrM (\_ _ -> return True) (return False)
null m =
    let stop      = return True
        single _  = return False
        yieldk _ _ = return False
    in foldStream defState yieldk single stop m

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

infixr 6 `append`

-- | Unlike the fused "Streamly.Data.Stream" append, StreamK append can be used
-- at scale, recursively, with linear performance:
--
-- >>> cycle xs = let ys = xs `StreamK.append` ys in ys
--
-- 'concatMapWith' 'append' (same as concatMap) flattens a stream of streams in a
-- depth-first manner i.e. it yields each stream fully and then the next and so
-- on. Given a stream of three streams:
--
-- @
-- 1. [1,2,3]
-- 2. [4,5,6]
-- 3. [7,8,9]
-- @
--
-- The resulting stream will be @[1,2,3,4,5,6,7,8,9]@.
--
-- Best used in a right associative manner.
--
{-# INLINE append #-}
append :: StreamK m a -> StreamK m a -> StreamK m a
-- XXX This doubles the time of toNullAp benchmark, may not be fusing properly
-- serial xs ys = augmentS (\c n -> foldrS c n xs) ys
append m1 m2 =
    mkStream $ \st yld sng stp ->
        let stop       = foldStream st yld sng stp m2
            single a   = yld a m2
            yieldk a r = yld a (go r)
        in foldStream st yieldk single stop m1

    where

    go m =
        mkStream $ \st yld sng stp ->
            let stop       = foldStream st yld sng stp m2
                single a   = yld a m2
                yieldk a r = yld a (go r)
            in foldStream st yieldk single stop m

-- join/merge/append streams depending on consM
{-# INLINE conjoin #-}
conjoin :: Monad m => StreamK m a -> StreamK m a -> StreamK m a
conjoin xs = augmentSM (\c n -> foldrSM c n xs)

instance Semigroup (StreamK m a) where
    (<>) = append

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance Monoid (StreamK m a) where
    mempty = nil
    mappend = (<>)

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

-- IMPORTANT: This is eta expanded on purpose. This should not be eta
-- reduced. This will cause a lot of regressions, probably because of some
-- rewrite rules. Ideally don't run hlint on this file.
{-# INLINE_LATE mapFB #-}
mapFB :: forall b m a.
       (b -> StreamK m b -> StreamK m b)
    -> (a -> b)
    -> a
    -> StreamK m b
    -> StreamK m b
mapFB c f = \x ys -> c (f x) ys

{-# RULES
"mapFB/mapFB" forall c f g. mapFB (mapFB c f) g = mapFB c (f . g)
"mapFB/id"    forall c.     mapFB c (\x -> x)   = c
    #-}

{-# INLINE map #-}
map :: (a -> b) -> StreamK m a -> StreamK m b
map f xs = buildS (\c n -> foldrS (mapFB c f) n xs)

-- XXX This definition might potentially be more efficient, but the cost in the
-- benchmark is dominated by unfoldrM cost so we cannot correctly determine
-- differences in the mapping cost. We should perhaps deduct the cost of
-- unfoldrM from the benchmarks and then compare.
{-
map f m = go m
    where
        go m1 =
            mkStream $ \st yld sng stp ->
            let single     = sng . f
                yieldk a r = yld (f a) (go r)
            in foldStream (adaptState st) yieldk single stp m1
-}

{-# INLINE_LATE mapMFB #-}
mapMFB :: Monad m => (m b -> t m b -> t m b) -> (a -> m b) -> m a -> t m b -> t m b
mapMFB c f x = c (x >>= f)

{-# RULES
    "mapMFB/mapMFB" forall c f g. mapMFB (mapMFB c f) g = mapMFB c (f >=> g)
    #-}
-- XXX These rules may never fire because pure/return type class rules will
-- fire first.
{-
"mapMFB/pure"    forall c.     mapMFB c (\x -> pure x)   = c
"mapMFB/return"  forall c.     mapMFB c (\x -> return x) = c
-}

-- This is experimental serial version supporting fusion.
--
-- XXX what if we do not want to fuse two concurrent mapMs?
-- XXX we can combine two concurrent mapM only if the SVar is of the same type
-- So for now we use it only for serial streams.
-- XXX fusion would be easier for monomoprhic stream types.
-- {-# RULES "mapM serial" mapM = mapMSerial #-}
{-# INLINE mapMSerial #-}
mapMSerial :: Monad m => (a -> m b) -> StreamK m a -> StreamK m b
mapMSerial f xs = buildSM (\c n -> foldrSMShared (mapMFB c f) n xs)

{-# INLINE mapMWith #-}
mapMWith ::
       (m b -> StreamK m b -> StreamK m b)
    -> (a -> m b)
    -> StreamK m a
    -> StreamK m b
mapMWith cns f = foldrSShared (\x xs -> f x `cns` xs) nil

{-
-- See note under map definition above.
mapMWith cns f = go
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single a  = f a >>= sng
            yieldk a r = foldStreamShared st yld sng stp $ f a `cns` go r
         in foldStream (adaptState st) yieldk single stp m1
-}

-- XXX in fact use the Stream type everywhere and only use polymorphism in the
-- high level modules/prelude.
instance Monad m => Functor (StreamK m) where
    fmap = map

-- $smapM_Notes
--
-- The stateful step function can be simplified to @(s -> a -> m b)@ to provide
-- a read-only environment. However, that would just be 'mapM'.
--
-- The initial action could be @m (s, Maybe b)@, and we can also add a final
-- action @s -> m (Maybe b)@. This can be used to get pre/post scan like
-- functionality and also to flush the state in the end like scanlMAfter'.
-- We can also use it along with a fusible version of bracket to get
-- scanlMAfter' like functionality. See issue #677.
--
-- This can be further generalized to a type similar to Fold/Parser, giving it
-- filtering and parsing capability as well (this is in fact equivalent to
-- parseMany):
--
-- smapM :: (s -> a -> m (Step s b)) -> m s -> t m a -> t m b
--

-- | A stateful map aka scan but with a slight difference.
--
-- This is similar to a scan except that instead of emitting the state it emits
-- a separate result. This is also similar to mapAccumL but does not return the
-- final value of the state.
--
-- Separation of state from the output makes it easier to think in terms of a
-- shared state, and also makes it easier to keep the state fully strict and
-- the output lazy.
--
-- /Unimplemented/
--
{-# INLINE mapMAccum #-}
mapMAccum :: -- Monad m =>
       (s -> a -> m (s, b))
    -> m s
    -> StreamK m a
    -> StreamK m b
mapMAccum _step _initial _stream = undefined
{-
    -- XXX implement this directly instead of using scanlM'
    -- Once we have postscanlM' with monadic initial we can use this code
    -- let r = postscanlM'
    --              (\(s, _) a -> step s a)
    --              (fmap (,undefined) initial)
    --              stream
    let r = postscanlM'
                (\(s, _) a -> step s a)
                (fmap (,undefined) initial)
                stream
     in map snd r
-}

-- | Like 'concatMapWith' but carries a state which can be used to share
-- information across multiple steps of concat.
--
-- @
-- concatSmapMWith combine f initial = concatMapWith combine id . smapM f initial
-- @
--
-- /Unimplemented/
--
{-# INLINE concatMapMAccum #-}
concatMapMAccum :: -- (Monad m) =>
       (StreamK m b -> StreamK m b -> StreamK m b)
    -> (s -> a -> m (s, StreamK m b))
    -> m s
    -> StreamK m a
    -> StreamK m b
concatMapMAccum combine f initial =
    concatMapWith combine id . mapMAccum f initial

------------------------------------------------------------------------------
-- Lists
------------------------------------------------------------------------------

-- Serial streams can act like regular lists using the Identity monad

-- XXX Show instance is 10x slower compared to read, we can do much better.
-- The list show instance itself is really slow.

-- XXX The default definitions of "<" in the Ord instance etc. do not perform
-- well, because they do not get inlined. Need to add INLINE in Ord class in
-- base?

instance IsList (StreamK Identity a) where
    type (Item (StreamK Identity a)) = a

    {-# INLINE fromList #-}
    fromList = fromFoldable

    {-# INLINE toList #-}
    toList = Data.Foldable.foldr (:) []

-- XXX Fix these
{-
instance Eq a => Eq (StreamK Identity a) where
    {-# INLINE (==) #-}
    (==) xs ys = runIdentity $ eqBy (==) xs ys

instance Ord a => Ord (StreamK Identity a) where
    {-# INLINE compare #-}
    compare xs ys = runIdentity $ cmpBy compare xs ys

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
-}

instance Show a => Show (StreamK Identity a) where
    showsPrec p dl = showParen (p > 10) $
        showString "fromList " . shows (toList dl)

instance Read a => Read (StreamK Identity a) where
    readPrec = parens $ prec 10 $ do
        Ident "fromList" <- lexP
        GHC.Exts.fromList <$> readPrec

    readListPrec = readListPrecDefault

instance (a ~ Char) => IsString (StreamK Identity a) where
    {-# INLINE fromString #-}
    fromString = GHC.Exts.fromList

-------------------------------------------------------------------------------
-- Foldable
-------------------------------------------------------------------------------

-- | Lazy right associative fold.
{-# INLINE foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> StreamK m a -> m b
foldr step acc = foldrM (\x xs -> xs >>= \b -> return (step x b)) (return acc)

-- The default Foldable instance has several issues:
-- 1) several definitions do not have INLINE on them, so we provide
--    re-implementations with INLINE pragmas.
-- 2) the definitions of sum/product/maximum/minimum are inefficient as they
--    use right folds, they cannot run in constant memory. We provide
--    implementations using strict left folds here.

instance (Foldable m, Monad m) => Foldable (StreamK m) where

    {-# INLINE foldMap #-}
    foldMap f =
          fold
        . Streamly.Internal.Data.StreamK.Type.foldr (mappend . f) mempty

    {-# INLINE foldr #-}
    foldr f z t = appEndo (foldMap (Endo #. f) t) z

    {-# INLINE foldl' #-}
    foldl' f z0 xs = Data.Foldable.foldr f' id xs z0
        where f' x k = oneShot $ \z -> k $! f z x

    {-# INLINE length #-}
    length = Data.Foldable.foldl' (\n _ -> n + 1) 0

    {-# INLINE elem #-}
    elem = any . (==)

    {-# INLINE maximum #-}
    maximum =
          fromMaybe (errorWithoutStackTrace "maximum: empty stream")
        . toMaybe
        . Data.Foldable.foldl' getMax Nothing'

        where

        getMax Nothing' x = Just' x
        getMax (Just' mx) x = Just' $! max mx x

    {-# INLINE minimum #-}
    minimum =
          fromMaybe (errorWithoutStackTrace "minimum: empty stream")
        . toMaybe
        . Data.Foldable.foldl' getMin Nothing'

        where

        getMin Nothing' x = Just' x
        getMin (Just' mn) x = Just' $! min mn x

    {-# INLINE sum #-}
    sum = Data.Foldable.foldl' (+) 0

    {-# INLINE product #-}
    product = Data.Foldable.foldl' (*) 1

-------------------------------------------------------------------------------
-- Traversable
-------------------------------------------------------------------------------

instance Traversable (StreamK Identity) where
    {-# INLINE traverse #-}
    traverse f xs =
        runIdentity
            $ Streamly.Internal.Data.StreamK.Type.foldr
                consA (pure mempty) xs

        where

        consA x ys = liftA2 cons (f x) ys

-------------------------------------------------------------------------------
-- Nesting
-------------------------------------------------------------------------------

-- | Detach a stream from an SVar
{-# INLINE unShare #-}
unShare :: StreamK m a -> StreamK m a
unShare x = mkStream $ \st yld sng stp ->
    foldStream st yld sng stp x

-- XXX the function stream and value stream can run in parallel
{-# INLINE crossApplyWith #-}
crossApplyWith ::
       (StreamK m b -> StreamK m b -> StreamK m b)
    -> StreamK m (a -> b)
    -> StreamK m a
    -> StreamK m b
crossApplyWith par fstream stream = go1 fstream

    where

    go1 m =
        mkStream $ \st yld sng stp ->
            let foldShared = foldStreamShared st yld sng stp
                single f   = foldShared $ unShare (go2 f stream)
                yieldk f r = foldShared $ unShare (go2 f stream) `par` go1 r
            in foldStream (adaptState st) yieldk single stp m

    go2 f m =
        mkStream $ \st yld sng stp ->
            let single a   = sng (f a)
                yieldk a r = yld (f a) (go2 f r)
            in foldStream (adaptState st) yieldk single stp m

-- | Apply a stream of functions to a stream of values and flatten the results.
--
-- Note that the second stream is evaluated multiple times.
--
-- Definition:
--
-- >>> crossApply = StreamK.crossApplyWith StreamK.append
-- >>> crossApply = Stream.crossWith id
--
{-# INLINE crossApply #-}
crossApply ::
       StreamK m (a -> b)
    -> StreamK m a
    -> StreamK m b
crossApply fstream stream = go1 fstream

    where

    go1 m =
        mkStream $ \st yld sng stp ->
            let foldShared = foldStreamShared st yld sng stp
                single f   = foldShared $ go3 f stream
                yieldk f r = foldShared $ go2 f r stream
            in foldStream (adaptState st) yieldk single stp m

    go2 f r1 m =
        mkStream $ \st yld sng stp ->
            let foldShared = foldStreamShared st yld sng stp
                stop = foldShared $ go1 r1
                single a   = yld (f a) (go1 r1)
                yieldk a r = yld (f a) (go2 f r1 r)
            in foldStream (adaptState st) yieldk single stop m

    go3 f m =
        mkStream $ \st yld sng stp ->
            let single a   = sng (f a)
                yieldk a r = yld (f a) (go3 f r)
            in foldStream (adaptState st) yieldk single stp m

{-# INLINE crossApplySnd #-}
crossApplySnd ::
       StreamK m a
    -> StreamK m b
    -> StreamK m b
crossApplySnd fstream stream = go1 fstream

    where

    go1 m =
        mkStream $ \st yld sng stp ->
            let foldShared = foldStreamShared st yld sng stp
                single _   = foldShared stream
                yieldk _ r = foldShared $ go2 r stream
            in foldStream (adaptState st) yieldk single stp m

    go2 r1 m =
        mkStream $ \st yld sng stp ->
            let foldShared = foldStreamShared st yld sng stp
                stop = foldShared $ go1 r1
                single a   = yld a (go1 r1)
                yieldk a r = yld a (go2 r1 r)
            in foldStream st yieldk single stop m

{-# INLINE crossApplyFst #-}
crossApplyFst ::
       StreamK m a
    -> StreamK m b
    -> StreamK m a
crossApplyFst fstream stream = go1 fstream

    where

    go1 m =
        mkStream $ \st yld sng stp ->
            let foldShared = foldStreamShared st yld sng stp
                single f   = foldShared $ go3 f stream
                yieldk f r = foldShared $ go2 f r stream
            in foldStream st yieldk single stp m

    go2 f r1 m =
        mkStream $ \st yld sng stp ->
            let foldShared = foldStreamShared st yld sng stp
                stop = foldShared $ go1 r1
                single _   = yld f (go1 r1)
                yieldk _ r = yld f (go2 f r1 r)
            in foldStream (adaptState st) yieldk single stop m

    go3 f m =
        mkStream $ \st yld sng stp ->
            let single _   = sng f
                yieldk _ r = yld f (go3 f r)
            in foldStream (adaptState st) yieldk single stp m

-- |
-- Definition:
--
-- >>> crossWith f m1 m2 = fmap f m1 `StreamK.crossApply` m2
--
-- Note that the second stream is evaluated multiple times.
--
{-# INLINE crossWith #-}
crossWith :: Monad m => (a -> b -> c) -> StreamK m a -> StreamK m b -> StreamK m c
crossWith f m1 m2 = fmap f m1 `crossApply` m2

-- | Given a @StreamK m a@ and @StreamK m b@ generate a stream with all possible
-- combinations of the tuple @(a, b)@.
--
-- Definition:
--
-- >>> cross = StreamK.crossWith (,)
--
-- The second stream is evaluated multiple times. If that is not desired it can
-- be cached in an 'Data.Array.Array' and then generated from the array before
-- calling this function. Caching may also improve performance if the stream is
-- expensive to evaluate.
--
-- See 'Streamly.Internal.Data.Unfold.cross' for a much faster fused
-- alternative.
--
-- Time: O(m x n)
--
-- /Pre-release/
{-# INLINE cross #-}
cross :: Monad m => StreamK m a -> StreamK m b -> StreamK m (a, b)
cross = crossWith (,)

-- XXX This is just concatMapWith with arguments flipped. We need to keep this
-- instead of using a concatMap style definition because the bind
-- implementation in Async and WAsync streams show significant perf degradation
-- if the argument order is changed.
{-# INLINE bindWith #-}
bindWith ::
       (StreamK m b -> StreamK m b -> StreamK m b)
    -> StreamK m a
    -> (a -> StreamK m b)
    -> StreamK m b
bindWith combine m1 f = go m1
{-
    -- There is a small improvement by unrolling the first iteration
    mkStream $ \st yld sng stp ->
        let foldShared = foldStreamShared st yld sng stp
            single a   = foldShared $ unShare (f a)
            yieldk a r = foldShared $ unShare (f a) `combine` go r
        in foldStreamShared (adaptState st) yieldk single stp m1
-}

    where

    go m =
        mkStream $ \st yld sng stp ->
            let foldShared = foldStreamShared st yld sng stp
                single a   = foldShared $ unShare (f a)
                yieldk a r = foldShared $ unShare (f a) `combine` go r
            in foldStreamShared (adaptState st) yieldk single stp m

-- XXX express in terms of foldrS?
-- XXX can we use a different stream type for the generated stream being
-- falttened so that we can combine them differently and keep the resulting
-- stream different?
-- XXX do we need specialize to IO?
-- XXX can we optimize when c and a are same, by removing the forall using
-- rewrite rules with type applications?

-- | Perform a 'concatMap' using a specified concat strategy. The first
-- argument specifies a merge or concat function that is used to merge the
-- streams generated by the map function.
--
-- For example, interleaving n streams in a left biased manner:
--
-- >>> lists = mk [[1,5],[2,6],[3,7],[4,8]]
-- >>> un $ StreamK.concatMapWith StreamK.interleave mk lists
-- [1,2,5,3,6,4,7,8]
--
-- For a fair interleaving example see 'bfsConcatMap' and 'mergeMapWith'.
--
{-# INLINE concatMapWith #-}
concatMapWith
    ::
       (StreamK m b -> StreamK m b -> StreamK m b)
    -> (a -> StreamK m b)
    -> StreamK m a
    -> StreamK m b
concatMapWith par f xs = bindWith par xs f

{-# INLINE concatMap #-}
concatMap :: (a -> StreamK m b) -> StreamK m a -> StreamK m b
concatMap = concatMapWith append

{-
-- Fused version.
-- XXX This fuses but when the stream is nil this performs poorly.
-- The filterAllOut benchmark degrades. Need to investigate and fix that.
{-# INLINE concatMap #-}
concatMap :: IsStream t => (a -> t m b) -> t m a -> t m b
concatMap f xs = buildS
    (\c n -> foldrS (\x b -> foldrS c b (f x)) n xs)

-- Stream polymorphic concatMap implementation
-- XXX need to use buildSM/foldrSMShared for parallel behavior
-- XXX unShare seems to degrade the fused performance
{-# INLINE_EARLY concatMap_ #-}
concatMap_ :: IsStream t => (a -> t m b) -> t m a -> t m b
concatMap_ f xs = buildS
     (\c n -> foldrSShared (\x b -> foldrSShared c b (unShare $ f x)) n xs)
-}

-- XXX Instead of using "mergeMapWith interleave" we can implement an N-way
-- interleaving CPS combinator which behaves like unfoldEachInterleave. Instead
-- of pairing up the streams we just need to go yielding one element from each
-- stream and storing the remaining streams and then keep doing rounds through
-- those in a round robin fashion. This would be much like wAsync.

-- | Combine streams in pairs using a binary combinator, the resulting streams
-- are then combined again in pairs recursively until we get to a single
-- combined stream. The composition would thus form a binary tree.
--
-- For example, 'mergeMapWith interleave' gives the following result:
--
-- >>> lists = mk [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
-- >>> un $ StreamK.mergeMapWith StreamK.interleave mk lists
-- [1,7,4,10,2,8,5,11,3,9,6,12]
--
-- The above example is equivalent to the following pairings:
--
-- >>> pair1 = mk [1,2,3] `StreamK.interleave` mk [4,5,6]
-- >>> pair2 = mk [7,8,9] `StreamK.interleave` mk [10,11,12]
-- >>> un $ pair1 `StreamK.interleave` pair2
-- [1,7,4,10,2,8,5,11,3,9,6,12]
--
-- If the number of streams being combined is not a power of 2, the binary tree
-- composed by mergeMapWith is not balanced, therefore, the output may not look
-- fairly interleaved, it will be biased towards the unpaired streams:
--
-- >>> lists = mk [[1,2,3],[4,5,6],[7,8,9]]
-- >>> un $ StreamK.mergeMapWith StreamK.interleave mk lists
-- [1,7,4,8,2,9,5,3,6]
--
-- An efficient merge sort can be implemented by using 'mergeBy' as the
-- combining function:
--
-- >>> combine = StreamK.mergeBy compare
-- >>> un $ StreamK.mergeMapWith combine StreamK.fromPure (mk [5,1,7,9,2])
-- [1,2,5,7,9]
--
-- /Caution: the stream of streams must be finite/
--
-- /Pre-release/
--
{-# INLINE mergeMapWith #-}
mergeMapWith
    ::
       (StreamK m b -> StreamK m b -> StreamK m b)
    -> (a -> StreamK m b)
    -> StreamK m a
    -> StreamK m b
mergeMapWith combine f str = go (leafPairs str)

    where

    go stream =
        mkStream $ \st yld sng stp ->
            let foldShared = foldStreamShared st yld sng stp
                single a   = foldShared $ unShare a
                yieldk a r = foldShared $ go1 a r
            in foldStream (adaptState st) yieldk single stp stream

    go1 a1 stream =
        mkStream $ \st yld sng stp ->
            let foldShared = foldStreamShared st yld sng stp
                stop = foldShared $ unShare a1
                single a = foldShared $ unShare a1 `combine` a
                yieldk a r =
                    foldShared $ go $ combine a1 a `cons` nonLeafPairs r
            in foldStream (adaptState st) yieldk single stop stream

    -- Exactly the same as "go" except that stop continuation extracts the
    -- stream.
    leafPairs stream =
        mkStream $ \st yld sng stp ->
            let foldShared = foldStreamShared st yld sng stp
                single a   = sng (f a)
                yieldk a r = foldShared $ leafPairs1 a r
            in foldStream (adaptState st) yieldk single stp stream

    leafPairs1 a1 stream =
        mkStream $ \st yld sng _ ->
            let stop = sng (f a1)
                single a = sng (f a1 `combine` f a)
                yieldk a r = yld (f a1 `combine` f a) $ leafPairs r
            in foldStream (adaptState st) yieldk single stop stream

    -- Exactly the same as "leafPairs" except that it does not map "f"
    nonLeafPairs stream =
        mkStream $ \st yld sng stp ->
            let foldShared = foldStreamShared st yld sng stp
                single a   = sng a
                yieldk a r = foldShared $ nonLeafPairs1 a r
            in foldStream (adaptState st) yieldk single stp stream

    nonLeafPairs1 a1 stream =
        mkStream $ \st yld sng _ ->
            let stop = sng a1
                single a = sng (a1 `combine` a)
                yieldk a r = yld (a1 `combine` a) $ nonLeafPairs r
            in foldStream (adaptState st) yieldk single stop stream

-- XXX check if bindInterleave has better perf like bindWith?

-- | While concatMap flattens a stream of streams in a depth first manner,
-- 'bfsConcatMap' flattens it in a breadth-first manner. It yields one
-- item from the first stream, then one item from the next stream and so on.
-- Given a stream of three streams:
--
-- @
-- 1. [1,2,3]
-- 2. [4,5,6]
-- 3. [7,8,9]
-- @
--
-- The resulting stream is @[1,4,7,2,5,8,3,6,9]@.
--
-- For example:
--
-- >>> stream = mk [[1,2,3],[4,5,6],[7,8,9]]
-- >>> un $ StreamK.bfsConcatMap mk stream
-- [1,4,7,2,5,8,3,6,9]
--
-- Compare with 'concatMapWith' 'interleave' which explores the depth
-- exponentially more compared to the breadth, such that each stream yields
-- twice as many items compared to the next stream.
--
-- See also the equivalent fused version 'Data.Stream.unfoldEachInterleave'.
--
{-# INLINE bfsConcatMap #-}
bfsConcatMap ::
       (a -> StreamK m b)
    -> StreamK m a
    -> StreamK m b
bfsConcatMap f m1 = go id m1

    where

    go xs m =
        mkStream $ \st yld sng stp ->
            let foldShared = foldStreamShared st yld sng stp
                stop       = foldStream st yld sng stp (goLoop id (xs []))
                single a   = foldShared $ goLast xs (f a)
                yieldk a r = foldShared $ goNext xs r (f a)
            in foldStream (adaptState st) yieldk single stop m

    -- generate first element from cur stream, and then put it back in the
    -- queue xs.
    goNext xs m cur =
        mkStream $ \st yld sng stp -> do
            let stop       = foldStream st yld sng stp (go xs m)
                single a   = yld a (go xs m)
                yieldk a r = yld a (go (xs . (r :)) m)
            foldStream st yieldk single stop cur

    goLast xs cur =
        mkStream $ \st yld sng stp -> do
            let stop       = foldStream st yld sng stp (goLoop id (xs []))
                single a   = yld a (goLoop id (xs []))
                yieldk a r = yld a (goLoop id ((xs . (r :)) []))
            foldStream st yieldk single stop cur

    -- Loop through all streams in the queue ys until they are over
    goLoop ys [] =
            -- We will do this optimization only after two iterations are
            -- over, if doing this earlier is helpful we can do it in
            -- goLast as well, before calling goLoop.
           let xs = ys []
            in case xs of
                    [] -> nil
                    (z:[]) -> z
                    (z1:z2:[]) -> interleave z1 z2
                    zs -> goLoop id zs
    goLoop ys (x:xs) =
        mkStream $ \st yld sng stp -> do
            let stop       = foldStream st yld sng stp (goLoop ys xs)
                single a   = yld a (goLoop ys xs)
                yieldk a r = yld a (goLoop (ys . (r :)) xs)
            foldStream st yieldk single stop x

-- | 'fairConcatMap' flattens a stream of streams in a diagonal manner.
-- Every previous stream yields one more item than the next. Therefore, the
-- depth and breadth of traversal is equally balanced.
-- Given a stream of three streams:
--
-- @
-- 1. [1,2,3]
-- 2. [4,5,6]
-- 3. [7,8,9]
-- @
--
-- The resulting stream is @[1,2,4,3,5,7,6,8,9]@.
--
-- This is useful when producing cross products of streams such that both the
-- dimensions are explored equally.
--
-- For example:
--
-- >>> stream = mk [[1,2,3],[4,5,6],[7,8,9]]
-- >>> un $ StreamK.fairConcatMap mk stream
-- [1,2,4,3,5,7,6,8,9]
--
-- Compare with 'concatMapWith interleave' which explores the depth
-- exponentially more compared to the breadth, such that each stream yields
-- twice as many items compared to the next stream.
--
{-# INLINE fairConcatMap #-}
fairConcatMap ::
       (a -> StreamK m b)
    -> StreamK m a
    -> StreamK m b
fairConcatMap f m1 = go id m1

    where

    go xs m =
        mkStream $ \st yld sng stp ->
            let foldShared = foldStreamShared st yld sng stp
                stop       = foldStream st yld sng stp (goLoop id (xs []))
                single a   = foldShared $ goLoop id (xs [f a])
                yieldk a r = foldShared $ goNext r id (xs [f a])
            in foldStream (adaptState st) yieldk single stop m

    goNext m ys [] = go ys m
    goNext m ys (x:xs) =
        mkStream $ \st yld sng stp -> do
            let stop       = foldStream st yld sng stp (goNext m ys xs)
                single a   = yld a (goNext m ys xs)
                yieldk a r = yld a (goNext m (ys . (r :)) xs)
            foldStream st yieldk single stop x

    -- Loop through all streams in the queue ys until they are over
    goLoop ys [] =
            -- We will do this optimization only after two iterations are
            -- over, if doing this earlier is helpful we can do it in
            -- goLast as well, before calling goLoop.
           let xs = ys []
            in case xs of
                    [] -> nil
                    (z:[]) -> z
                    (z1:z2:[]) -> interleave z1 z2
                    zs -> goLoop id zs
    goLoop ys (x:xs) =
        mkStream $ \st yld sng stp -> do
            let stop       = foldStream st yld sng stp (goLoop ys xs)
                single a   = yld a (goLoop ys xs)
                yieldk a r = yld a (goLoop (ys . (r :)) xs)
            foldStream st yieldk single stop x

{-
instance Monad m => Applicative (StreamK m) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = crossApply

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

    {-# INLINE (*>) #-}
    (*>) = crossApplySnd

    {-# INLINE (<*) #-}
    (<*) = crossApplyFst

-- NOTE: even though concatMap for StreamD is 3x faster compared to StreamK,
-- the monad instance of StreamD is slower than StreamK after foldr/build
-- fusion.
instance Monad m => Monad (StreamK m) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = flip concatMap
-}

{-
-- Like concatMap but generates stream using an unfold function. Similar to
-- unfoldMany but for StreamK.
concatUnfoldr :: IsStream t
    => (b -> t m (Maybe (a, b))) -> t m b -> t m a
concatUnfoldr = undefined
-}

------------------------------------------------------------------------------
-- concatIterate - Map and flatten Trees of Streams
------------------------------------------------------------------------------

-- | Yield an input element in the output stream, map a stream generator on it
-- and repeat the process on the resulting stream. Resulting streams are
-- flattened using the 'concatMapWith' combinator. This can be used for a depth
-- first style (DFS) traversal of a tree like structure.
--
-- Example, list a directory tree using DFS:
--
-- >>> f = StreamK.fromStream . either (Dir.readEitherPaths id) (const Stream.nil)
-- >>> input = StreamK.fromEffect (Left <$> Path.fromString ".")
-- >>> ls = StreamK.concatIterateWith StreamK.append f input
--
-- Note that 'iterateM' is a special case of 'concatIterateWith':
--
-- >>> iterateM f = StreamK.concatIterateWith StreamK.append (StreamK.fromEffect . f) . StreamK.fromEffect
--
-- /Pre-release/
--
{-# INLINE concatIterateWith #-}
concatIterateWith ::
       (StreamK m a -> StreamK m a -> StreamK m a)
    -> (a -> StreamK m a)
    -> StreamK m a
    -> StreamK m a
concatIterateWith combine f = iterateStream

    where

    iterateStream = concatMapWith combine generate

    generate x = x `cons` iterateStream (f x)

-- | Like 'concatIterateWith' but uses the pairwise flattening combinator
-- 'mergeMapWith' for flattening the resulting streams. This can be used for a
-- balanced traversal of a tree like structure.
--
-- Example, list a directory tree using balanced traversal:
--
-- >>> f = StreamK.fromStream . either (Dir.readEitherPaths id) (const Stream.nil)
-- >>> input = StreamK.fromEffect (Left <$> Path.fromString ".")
-- >>> ls = StreamK.mergeIterateWith StreamK.interleave f input
--
-- /Pre-release/
--
{-# INLINE mergeIterateWith #-}
mergeIterateWith ::
       (StreamK m a -> StreamK m a -> StreamK m a)
    -> (a -> StreamK m a)
    -> StreamK m a
    -> StreamK m a
mergeIterateWith combine f = iterateStream

    where

    iterateStream = mergeMapWith combine generate

    generate x = x `cons` iterateStream (f x)

------------------------------------------------------------------------------
-- Flattening Graphs
------------------------------------------------------------------------------

-- To traverse graphs we need a state to be carried around in the traversal.
-- For example, we can use a hashmap to store the visited status of nodes.

-- XXX rename to concateIterateAccum? Like mapMAccum

-- | Like 'iterateMap' but carries a state in the stream generation function.
-- This can be used to traverse graph like structures, we can remember the
-- visited nodes in the state to avoid cycles.
--
-- Note that a combination of 'iterateMap' and 'usingState' can also be used to
-- traverse graphs. However, this function provides a more localized state
-- instead of using a global state.
--
-- See also: 'mfix'
--
-- /Pre-release/
--
{-# INLINE concatIterateScanWith #-}
concatIterateScanWith
    :: Monad m
    => (StreamK m a -> StreamK m a -> StreamK m a)
    -> (b -> a -> m (b, StreamK m a))
    -> m b
    -> StreamK m a
    -> StreamK m a
concatIterateScanWith combine f initial stream =
    concatEffect $ do
        b <- initial
        iterateStream (b, stream)

    where

    iterateStream (b, s) = pure $ concatMapWith combine (generate b) s

    generate b a = a `cons` feedback b a

    feedback b a = concatEffect $ f b a >>= iterateStream

------------------------------------------------------------------------------
-- Either streams
------------------------------------------------------------------------------

-- Keep concating either streams as long as rights are generated, stop as soon
-- as a left is generated and concat the left stream.
--
-- See also: 'handle'
--
-- /Unimplemented/
--
{-
concatMapEitherWith
    :: (forall x. t m x -> t m x -> t m x)
    -> (a -> t m (Either (StreamK m b) b))
    -> StreamK m a
    -> StreamK m b
concatMapEitherWith = undefined
-}

-- XXX We should prefer using the Maybe stream returning signatures over this.
-- This API should perhaps be removed in favor of those.

-- | In an 'Either' stream iterate on 'Left's.  This is a special case of
-- 'concatIterateWith':
--
-- >>> concatIterateLeftsWith combine f = StreamK.concatIterateWith combine (either f (const StreamK.nil))
--
-- To traverse a directory tree:
--
-- >>> input = StreamK.fromEffect (Left <$> Path.fromString ".")
-- >>> ls = StreamK.concatIterateLeftsWith StreamK.append (StreamK.fromStream . Dir.readEither id) input
--
-- /Pre-release/
--
{-# INLINE concatIterateLeftsWith #-}
concatIterateLeftsWith
    :: (b ~ Either a c)
    => (StreamK m b -> StreamK m b -> StreamK m b)
    -> (a -> StreamK m b)
    -> StreamK m b
    -> StreamK m b
concatIterateLeftsWith combine f =
    concatIterateWith combine (either f (const nil))

------------------------------------------------------------------------------
-- Interleaving
------------------------------------------------------------------------------

infixr 6 `interleave`

-- We can have a variant of interleave where m elements yield from the first
-- stream and n elements yielding from the second stream. We can also have time
-- slicing variants of positional interleaving, e.g. run first stream for m
-- seconds and run the second stream for n seconds.
--
-- TBD; give an example to show second stream is half consumed.
--
-- a1,a2,a3,a4,a5,a6,a7,a8
-- b1,b2,b3,b4,b5,b6,b7,b8
-- c1,c2,c3,c4,c5,c6,c7,c8
-- d1,d2,d3,d4,d5,d6,d7,d8
-- e1,e2,e3,e4,e5,e6,e7,e8
-- f1,f2,f3,f4,f5,f6,f7,f8
-- g1,g2,g3,g4,g5,g6,g7,g8
-- h1,h2,h3,h4,h5,h6,h7,h8
--
-- Produces: (..)
--

-- | Interleave two streams fairly, yielding one item from each in a
-- round-robin fashion:
--
-- >>> un $ StreamK.interleave (mk [1,3,5]) (mk [2,4,6])
-- [1,2,3,4,5,6]
-- >>> un $ StreamK.interleave (mk [1,3]) (mk [2,4,6])
-- [1,2,3,4,6]
-- >>> un $ StreamK.interleave (mk []) (mk [2,4,6])
-- [2,4,6]
--
-- 'interleave' is right associative when used as an infix operator.
--
-- >>> un $ mk [1,2,3] `StreamK.interleave` mk [4,5,6] `StreamK.interleave` mk [7,8,9]
-- [1,4,2,7,3,5,8,6,9]
--
-- Because of right association, the first stream yields as many items as the
-- next two streams combined.
--
-- Be careful when refactoring code involving a chain of three or more
-- 'interleave' operations as it is not associative i.e. right associated code
-- may not produce the same result as left associated. This is a direct
-- consequence of the disbalance of scheduling in the previous example. If left
-- associated the above example would produce:
--
-- >>> un $ (mk [1,2,3] `StreamK.interleave` mk [4,5,6]) `StreamK.interleave` mk [7,8,9]
-- [1,7,4,8,2,9,5,3,6]
--
-- Note: Use concatMap based interleaving instead of the binary operator to
-- interleave more than two streams to avoid associativity issues.
--
-- 'concatMapWith' 'interleave' flattens a stream of streams using 'interleave'
-- in a right associative manner. Given a stream of three streams:
--
-- @
-- 1. [1,2,3]
-- 2. [4,5,6]
-- 3. [7,8,9]
-- @
--
-- The resulting sequence is @[1,4,2,7,3,5,8,6,9]@.
--
-- For this reason, the right associated flattening with 'interleave' can work
-- with infinite number of streams without opening too many streams at the same
-- time. Each stream is consumed twice as much as the next one; if we are
-- combining an infinite number of streams of size @n@ then at most @log n@
-- streams will be opened at any given time, because the first stream will
-- finish by the time the stream after @log n@ th stream is opened.
--
-- Compare with 'bfsConcatMap' and 'mergeMapWith' 'interleave'.
--
-- For interleaving many streams, the best way is to use 'bfsConcatMap'.
--
-- See also the fused version 'Streamly.Data.Stream.interleave'.
{-# INLINE interleave #-}
interleave :: StreamK m a -> StreamK m a -> StreamK m a
interleave m1 m2 = mkStream $ \st yld sng stp -> do
    let stop       = foldStream st yld sng stp m2
        single a   = yld a m2
        yieldk a r = yld a (interleave m2 r)
    foldStream st yieldk single stop m1

-- Examples:
--
-- >>> fromList = StreamK.fromStream . Stream.fromList
-- >>> toList = Stream.toList . StreamK.toStream
-- >>> f x y = toList $ StreamK.interleaveSepBy (fromList x) (fromList y)
--
-- -- This is broken.
-- >> f "..." "abc"
-- "a.b.c"

-- >>> f ".." "abc"
-- "a.b.c"
-- >>> f "." "abc"
-- "a.bc"
--
{-# INLINE interleaveSepBy #-}
interleaveSepBy :: StreamK m a -> StreamK m a -> StreamK m a
interleaveSepBy m2 m1 = mkStream $ \st yld sng stp -> do
    let yieldFirst a r = yld a (yieldSecond r m2)
     in foldStream st yieldFirst sng stp m1

    where

    yieldSecond s1 s2 = mkStream $ \st yld sng stp -> do
            let stop       = foldStream st yld sng stp s1
                single a   = yld a s1
                yieldk a r = yld a (interleave s1 r)
             in foldStream st yieldk single stop s2

infixr 6 `interleaveFst`

{-# DEPRECATED interleaveFst "Please use flip interleaveSepBy instead." #-}
{-# INLINE interleaveFst #-}
interleaveFst :: StreamK m a -> StreamK m a -> StreamK m a
interleaveFst = flip interleaveSepBy

-- |
--
-- Examples:
--
-- >>> fromList = StreamK.fromStream . Stream.fromList
-- >>> toList = Stream.toList . StreamK.toStream
-- >>> f x y = toList $ StreamK.interleaveEndBy' (fromList x) (fromList y)
-- >>> f "..." "abc"
-- "a.b.c."
-- >>> f "..." "ab"
-- "a.b."
--
-- Currently broken, generates an additional element at the end::
--
-- >> f ".." "abc"
-- "a.b."
--
{-# INLINE interleaveEndBy' #-}
interleaveEndBy' :: StreamK m a -> StreamK m a -> StreamK m a
interleaveEndBy' m2 m1 = mkStream $ \st yld _ stp -> do
    let stop       = stp
        -- "single a" is defined as "yld a (interleaveMin m2 nil)" instead of
        -- "sng a" to keep the behaviour consistent with the yield
        -- continuation.
        single a   = yld a (interleaveEndBy' nil m2)
        yieldk a r = yld a (interleaveEndBy' r m2)
    foldStream st yieldk single stop m1

infixr 6 `interleaveMin`

{-# DEPRECATED interleaveMin "Please use flip interleaveEndBy' instead." #-}
{-# INLINE interleaveMin #-}
interleaveMin :: StreamK m a -> StreamK m a -> StreamK m a
interleaveMin = flip interleaveEndBy'

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-- |
-- >>> :{
-- unfoldr step s =
--     case step s of
--         Nothing -> StreamK.nil
--         Just (a, b) -> a `StreamK.cons` unfoldr step b
-- :}
--
-- Build a stream by unfolding a /pure/ step function @step@ starting from a
-- seed @s@.  The step function returns the next element in the stream and the
-- next seed value. When it is done it returns 'Nothing' and the stream ends.
-- For example,
--
-- >>> :{
-- let f b =
--         if b > 2
--         then Nothing
--         else Just (b, b + 1)
-- in StreamK.toList $ StreamK.unfoldr f 0
-- :}
-- [0,1,2]
--
{-# INLINE unfoldr #-}
unfoldr :: (b -> Maybe (a, b)) -> b -> StreamK m a
unfoldr next s0 = build $ \yld stp ->
    let go s =
            case next s of
                Just (a, b) -> yld a (go b)
                Nothing -> stp
    in go s0

{-# INLINE unfoldrMWith #-}
unfoldrMWith :: Monad m =>
       (m a -> StreamK m a -> StreamK m a)
    -> (b -> m (Maybe (a, b)))
    -> b
    -> StreamK m a
unfoldrMWith cns step = go

    where

    go s = sharedMWith cns $ \yld _ stp -> do
                r <- step s
                case r of
                    Just (a, b) -> yld a (go b)
                    Nothing -> stp

-- | Build a stream by unfolding a /monadic/ step function starting from a
-- seed.  The step function returns the next element in the stream and the next
-- seed value. When it is done it returns 'Nothing' and the stream ends. For
-- example,
--
-- >>> :{
-- let f b =
--         if b > 2
--         then return Nothing
--         else return (Just (b, b + 1))
-- in StreamK.toList $ StreamK.unfoldrM f 0
-- :}
-- [0,1,2]
--
{-# INLINE unfoldrM #-}
unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> StreamK m a
unfoldrM = unfoldrMWith consM

-- | Generate an infinite stream by repeating a pure value.
--
-- >>> repeat x = let xs = StreamK.cons x xs in xs
--
-- /Pre-release/
{-# INLINE repeat #-}
repeat :: a -> StreamK m a
repeat x = let xs = cons x xs in xs

-- | Like 'repeatM' but takes a stream 'cons' operation to combine the actions
-- in a stream specific manner. A serial cons would repeat the values serially
-- while an async cons would repeat concurrently.
--
-- /Pre-release/
repeatMWith :: (m a -> t m a -> t m a) -> m a -> t m a
repeatMWith cns = go

    where

    go m = m `cns` go m

{-# INLINE replicateMWith #-}
replicateMWith :: (m a -> StreamK m a -> StreamK m a) -> Int -> m a -> StreamK m a
replicateMWith cns n m = go n

    where

    go cnt = if cnt <= 0 then nil else m `cns` go (cnt - 1)

{-# INLINE fromIndicesMWith #-}
fromIndicesMWith ::
    (m a -> StreamK m a -> StreamK m a) -> (Int -> m a) -> StreamK m a
fromIndicesMWith cns gen = go 0

    where

    go i = mkStream $ \st stp sng yld -> do
        foldStreamShared st stp sng yld (gen i `cns` go (i + 1))

{-# INLINE iterateMWith #-}
iterateMWith :: Monad m =>
    (m a -> StreamK m a -> StreamK m a) -> (a -> m a) -> m a -> StreamK m a
iterateMWith cns step = go

    where

    go s = mkStream $ \st stp sng yld -> do
        !next <- s
        foldStreamShared st stp sng yld (return next `cns` go (step next))

-- | head for non-empty streams, fails for empty stream case.
--
{-# INLINE headNonEmpty #-}
headNonEmpty :: Monad m => StreamK m a -> m a
headNonEmpty = foldrM (\x _ -> return x) (error "headNonEmpty: empty stream")

-- | init for non-empty streams, fails for empty stream case.
--
-- See also 'init' for a non-partial version of this function..
{-# INLINE initNonEmpty #-}
initNonEmpty :: Stream m a -> Stream m a
initNonEmpty = go0

    where

    go0 m = mkStream $ \st yld sng stp ->
        let stop = error "initNonEmpty: Empty Stream."
            single _ = stp
            yieldk a r = foldStream st yld sng stp (go1 a r)
         in foldStream st yieldk single stop m

    go1 a r = mkStream $ \st yld sng stp ->
        let stop = stp
            single _ = sng a
            yieldk a1 r1 = yld a (go1 a1 r1)
         in foldStream st yieldk single stop r

-- | tail for non-empty streams, fails for empty stream case.
--
-- See also 'tail' for a non-partial version of this function..
--
-- Note: this is same as "drop 1" with error on empty stream.
{-# INLINE tailNonEmpty #-}
tailNonEmpty :: StreamK m a -> StreamK m a
tailNonEmpty m = mkStream $ \st yld sng stp ->
    let stop      = error "tailNonEmpty: empty stream"
        single _  = stp
        yieldk _ r = foldStream st yld sng stp r
    in foldStream st yieldk single stop m

-- | We can define cyclic structures using @let@:
--
-- >>> :set -fno-warn-unrecognised-warning-flags
-- >>> :set -fno-warn-x-partial
-- >>> let (a, b) = ([1, b], head a) in (a, b)
-- ([1,1],1)
--
-- The function @fix@ defined as:
--
-- >>> fix f = let x = f x in x
--
-- ensures that the argument of a function and its output refer to the same
-- lazy value @x@ i.e.  the same location in memory.  Thus @x@ can be defined
-- in terms of itself, creating structures with cyclic references.
--
-- >>> f ~(a, b) = ([1, b], head a)
-- >>> fix f
-- ([1,1],1)
--
-- 'Control.Monad.mfix' is essentially the same as @fix@ but for monadic
-- values.
--
-- Using 'mfix' for streams we can construct a stream in which each element of
-- the stream is defined in a cyclic fashion. The argument of the function
-- being fixed represents the current element of the stream which is being
-- returned by the stream monad. Thus, we can use the argument to construct
-- itself.
--
-- In the following example, the argument @action@ of the function @f@
-- represents the tuple @(x,y)@ returned by it in a given iteration. We define
-- the first element of the tuple in terms of the second.
--
-- >>> import System.IO.Unsafe (unsafeInterleaveIO)
--
-- >>> :{
-- main = Stream.fold (Fold.drainMapM print) $ StreamK.toStream $ StreamK.mfix f
--     where
--     f action = StreamK.unNested $ do
--         let incr n act = fmap ((+n) . snd) $ unsafeInterleaveIO act
--         x <- StreamK.Nested $ StreamK.fromStream $ Stream.sequence $ Stream.fromList [incr 1 action, incr 2 action]
--         y <- StreamK.Nested $ StreamK.fromStream $ Stream.fromList [4,5]
--         return (x, y)
-- :}
--
-- Note: you cannot achieve this by just changing the order of the monad
-- statements because that would change the order in which the stream elements
-- are generated.
--
-- Note that the function @f@ must be lazy in its argument, that's why we use
-- 'unsafeInterleaveIO' on @action@ because IO monad is strict.
--
-- /Pre-release/
{-# INLINE mfix #-}
mfix :: Monad m => (m a -> StreamK m a) -> StreamK m a
mfix f = mkStream $ \st yld sng stp ->
    let single a  = foldStream st yld sng stp $ a `cons` ys
        yieldk a _ = foldStream st yld sng stp $ a `cons` ys
    in foldStream st yieldk single stp xs

    where

    -- fix the head element of the stream
    xs = fix  (f . headNonEmpty)

    -- now fix the tail recursively
    ys = mfix (tailNonEmpty . f)

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- |
-- >>> fromFoldable = Prelude.foldr StreamK.cons StreamK.nil
--
-- Construct a stream from a 'Foldable' containing pure values:
--
{-# INLINE fromFoldable #-}
fromFoldable :: Foldable f => f a -> StreamK m a
fromFoldable = Prelude.foldr cons nil

{-# INLINE fromFoldableM #-}
fromFoldableM :: (Foldable f, Monad m) => f (m a) -> StreamK m a
fromFoldableM = Prelude.foldr consM nil

{-# INLINE fromList #-}
fromList :: [a] -> StreamK m a
fromList = fromFoldable

-------------------------------------------------------------------------------
-- Deconstruction
-------------------------------------------------------------------------------

{-# INLINE uncons #-}
uncons :: Applicative m => StreamK m a -> m (Maybe (a, StreamK m a))
uncons m =
    let stop = pure Nothing
        single a = pure (Just (a, nil))
        yieldk a r = pure (Just (a, r))
    in foldStream defState yieldk single stop m

-- Note that this is not a StreamK -> StreamK because then we cannot handle the
-- empty stream case without making this a partial function.
--
-- See tailNonEmpty as well above.

-- | Same as:
--
-- >>> tail = fmap (fmap snd) . StreamK.uncons
--
{-# INLINE tail #-}
tail :: Applicative m => StreamK m a -> m (Maybe (StreamK m a))
tail =
    let stop      = pure Nothing
        single _  = pure $ Just nil
        yieldk _ r = pure $ Just r
    in foldStream defState yieldk single stop

-- Note that this is not a StreamK -> StreamK because then we cannot handle the
-- empty stream case without making this a partial function.
--
-- XXX How do we implement unsnoc? Make StreamK a monad and return the
-- remaining stream as a result value in the monad?

-- | Extract all but the last element of the stream, if any. This will end up
-- evaluating the last element as well to find out that it is last.
--
-- /Pre-release/
{-# INLINE init #-}
init :: Applicative m => StreamK m a -> m (Maybe (StreamK m a))
init = go1
    where
    go1 m1 = do
        (\case
            Nothing -> Nothing
            Just (h, t) -> Just $ go h t) <$> uncons m1
    go p m1 = mkStream $ \_ yld sng stp ->
        let single _ = sng p
            yieldk a x = yld p $ go a x
         in foldStream defState yieldk single stp m1

------------------------------------------------------------------------------
-- Reordering
------------------------------------------------------------------------------

-- | Lazy left fold to a stream.
{-# INLINE foldlS #-}
foldlS ::
    (StreamK m b -> a -> StreamK m b) -> StreamK m b -> StreamK m a -> StreamK m b
foldlS step = go
    where
    go acc rest = mkStream $ \st yld sng stp ->
        let run x = foldStream st yld sng stp x
            stop = run acc
            single a = run $ step acc a
            yieldk a r = run $ go (step acc a) r
         in foldStream (adaptState st) yieldk single stop rest

{-# INLINE reverse #-}
reverse :: StreamK m a -> StreamK m a
reverse = foldlS (flip cons) nil

------------------------------------------------------------------------------
-- Running effects
------------------------------------------------------------------------------

-- | Run an action before evaluating the stream.
{-# INLINE before #-}
before :: Monad m => m b -> StreamK m a -> StreamK m a
before action stream =
    mkStream $ \st yld sng stp ->
        action >> foldStreamShared st yld sng stp stream

{-# INLINE concatEffect #-}
concatEffect :: Monad m => m (StreamK m a) -> StreamK m a
concatEffect action =
    mkStream $ \st yld sng stp ->
        action >>= foldStreamShared st yld sng stp

{-# INLINE concatMapEffect #-}
concatMapEffect :: Monad m => (b -> StreamK m a) -> m b -> StreamK m a
concatMapEffect f action =
    mkStream $ \st yld sng stp ->
        action >>= foldStreamShared st yld sng stp . f

------------------------------------------------------------------------------
-- Stream with a cross product style monad instance
------------------------------------------------------------------------------

-- XXX add Alternative, MonadPlus - should we use interleave as the Semigroup
-- append operation in FairNested?

-- | 'Nested' is a list-transformer monad, it serves the same purpose as the
-- @ListT@ type from the @list-t@ package. It is similar to the standard
-- Haskell lists' monad instance. 'Nested' monad behaves like nested @for@ loops
-- implementing a computation based on a cross product over multiple streams.
--
-- >>> mk = StreamK.Nested . StreamK.fromStream . Stream.fromList
-- >>> un = Stream.toList . StreamK.toStream . StreamK.unNested
--
-- == Looping
--
-- In the following code the variable @x@ assumes values of the elements of the
-- stream one at a time and runs the code that follows; using that value. It is
-- equivalent to a @for@ loop:
--
-- >>> :{
-- un $ do
--     x <- mk [1,2,3] -- for each element in the stream
--     return x
-- :}
-- [1,2,3]
--
-- == Nested Looping
--
-- Multiple streams can be nested like nested @for@ loops. The result is a
-- cross product of the streams.
--
-- >>> :{
-- un $ do
--     x <- mk [1,2,3] -- outer loop, for each element in the stream
--     y <- mk [4,5,6] -- inner loop, for each element in the stream
--     return (x, y)
-- :}
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
--
-- Note that an infinite stream in an inner loop will block the outer streams
-- from moving to the next iteration.
--
-- == How it works?
--
-- The bind operation of the monad is flipped 'concatMapWith' 'append'. The
-- concatMap operation maps the lines involving y as a function of x over the
-- stream [1,2,3]. The streams generated so are combined using the 'append'
-- operation. If we desugar the above monad code using bind explicitly, it
-- becomes clear how it works:
--
-- >>> import Streamly.Internal.Data.StreamK (Nested(..))
-- >>> (Nested m) >>= f = Nested $ StreamK.concatMapWith StreamK.append (unNested . f) m
-- >>> un (mk [1,2,3] >>= (\x -> (mk [4,5,6] >>= \y -> return (x,y))))
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
--
-- You can achieve the looping and nested looping by directly using concatMap
-- but the monad and the \"do notation\" gives you better ergonomics.
--
-- == Interleaving of loop iterations
--
-- If we look at the cross product of [1,2,3], [4,5,6], the streams being
-- combined using 'append' are the @for@ loop iterations as follows:
--
-- @
-- (1,4) (1,5) (1,6) -- first iteration of the outer loop
-- (2,4) (2,5) (2,6) -- second iteration of the outer loop
-- (3,4) (3,5) (3,6) -- third iteration of the outer loop
-- @
--
-- The result is equivalent to sequentially appending all the iterations of the
-- nested @for@ loop:
--
-- @
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
-- @
--
-- == Logic Programming
--
-- 'Nested' also serves the purpose of 'LogicT' type from the 'logict' package.
-- The @MonadLogic@ operations can be implemented using the available stream
-- operations. For example, 'uncons' is @msplit@, 'interleave' corresponds to
-- the @interleave@ operation of MonadLogic, 'fairConcatMap' is a flipped
-- fair bind (@>>-@) operation. The 'FairNested' type provides a monad with fair
-- bind.
--
-- == Related Functionality
--
-- A custom type can be created using 'bfsConcatMap' as the monad bind
-- operation then the nested loops would get inverted - the innermost loop
-- becomes the outermost and vice versa.
--
-- See 'FairNested' if you want all the streams to get equal chance to execute
-- even if they are infinite.
newtype Nested m a = Nested {unNested :: StreamK m a}
        deriving (Functor, Semigroup, Monoid, Foldable)

{-# DEPRECATED CrossStreamK "Use Nested instead." #-}
type CrossStreamK = Nested

{-# DEPRECATED mkCross "Use Nested instead." #-}
-- | Wrap the 'StreamK' type in a 'Nested' newtype to enable cross
-- product style applicative and monad instances.
--
-- This is a type level operation with no runtime overhead.
{-# INLINE mkCross #-}
mkCross :: StreamK m a -> Nested m a
mkCross = Nested

-- | Unwrap the 'StreamK' type from 'CrossStreamK' newtype.
--
-- This is a type level operation with no runtime overhead.
{-# INLINE unCross #-}
unCross :: CrossStreamK m a -> StreamK m a
unCross = unNested

-- Pure (Identity monad) stream instances
deriving instance Traversable (Nested Identity)
deriving instance IsList (Nested Identity a)
deriving instance (a ~ Char) => IsString (Nested Identity a)
-- deriving instance Eq a => Eq (Nested Identity a)
-- deriving instance Ord a => Ord (Nested Identity a)

-- Do not use automatic derivation for this to show as "fromList" rather than
-- "fromList Identity".
instance Show a => Show (Nested Identity a) where
    {-# INLINE show #-}
    show (Nested xs) = show xs

instance Read a => Read (Nested Identity a) where
    {-# INLINE readPrec #-}
    readPrec = fmap Nested readPrec

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

-- Note: we need to define all the typeclass operations because we want to
-- INLINE them.
instance Monad m => Applicative (Nested m) where
    {-# INLINE pure #-}
    pure x = Nested (fromPure x)

    {-# INLINE (<*>) #-}
    (Nested s1) <*> (Nested s2) =
        Nested (crossApply s1 s2)

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

    {-# INLINE (*>) #-}
    (Nested s1) *> (Nested s2) =
        Nested (crossApplySnd s1 s2)

    {-# INLINE (<*) #-}
    (Nested s1) <* (Nested s2) =
        Nested (crossApplyFst s1 s2)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (Nested m) where
    return = pure

    -- Benchmarks better with CPS bind and pure:
    -- Prime sieve (25x)
    -- n binds, breakAfterSome, filterAllIn, state transformer (~2x)
    --
    {-# INLINE (>>=) #-}
    (>>=) (Nested m) f =
        Nested (bindWith append m (unNested . f))

    {-# INLINE (>>) #-}
    (>>) = (*>)

------------------------------------------------------------------------------
-- Alternative and MonadPlus
------------------------------------------------------------------------------

instance (Monad m) => Fail.MonadFail (Nested m) where
  fail _ = inline mempty

instance (Monad m, Functor m) => Alternative (Nested m) where
  empty = inline mempty
  (<|>) = inline mappend

instance (Monad m) => MonadPlus (Nested m) where
  mzero = inline mempty
  mplus = inline mappend

------------------------------------------------------------------------------
-- Transformers
------------------------------------------------------------------------------

instance (MonadIO m) => MonadIO (Nested m) where
    liftIO x = Nested (fromEffect $ liftIO x)

instance MonadTrans Nested where
    {-# INLINE lift #-}
    lift x = Nested (fromEffect x)

instance (MonadThrow m) => MonadThrow (Nested m) where
    throwM = lift . throwM

------------------------------------------------------------------------------
-- Stream with a fair cross product style monad instance
------------------------------------------------------------------------------

-- XXX We can fix the termination issues by adding a "skip" continuation in the
-- stream. Adding a "block" continuation can allow for blocking IO. Both of
-- these together will provide a co-operative scheduling. However, adding skip
-- will regress performance in heavy filtering cases. If that's important we
-- create another type StreamK' for skip continuation. That type can use
-- conversion from Stream type for everything except append and concatMap.

-- | 'FairNested' is like the 'Nested' type but explores the depth and breadth of
-- the cross product grid equally, so that each of the stream being crossed is
-- consumed equally. It can be used to nest infinite streams without starving
-- one due to the other.
--
-- >>> mk = StreamK.FairNested . StreamK.fromStream . Stream.fromList
-- >>> un = Stream.toList . StreamK.toStream . StreamK.unFairNested
--
-- == Looping
--
-- A single stream case is equivalent to 'Nested', it is a simple @for@ loop
-- over the stream:
--
-- >>> :{
-- un $ do
--     x <- mk [1,2] -- for each element in the stream
--     return x
-- :}
-- [1,2]
--
-- == Fair Nested Looping
--
-- Multiple streams nest like @for@ loops. The result is a cross product of the
-- streams. However, the ordering of the results of the cross product is such
-- that each stream gets consumed equally. In other words, inner iterations of
-- a nested loop get the same priority as the outer iterations. Inner
-- iterations do not finish completely before the outer iterations start.
--
-- >>> :{
-- un $ do
--     x <- mk [1,2,3] -- outer, for each element in the stream
--     y <- mk [4,5,6] -- inner, for each element in the stream
--     -- Perform the following actions for each x, for each y
--     return (x, y)
-- :}
-- [(1,4),(1,5),(2,4),(1,6),(2,5),(3,4),(2,6),(3,5),(3,6)]
--
-- == Nesting Infinite Streams
--
-- Example with infinite streams. Print all pairs in the cross product with sum
-- less than a specified number.
--
-- >>> :{
-- Stream.toList
--  $ Stream.takeWhile (\(x,y) -> x + y < 6)
--  $ StreamK.toStream $ StreamK.unFairNested
--  $ do
--     x <- mk [1..] -- infinite stream
--     y <- mk [1..] -- infinite stream
--     return (x, y)
-- :}
-- [(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(1,4),(2,3),(3,2),(4,1)]
--
-- == How it works?
--
-- 'FairNested' uses flipped 'fairConcatMap' as the monad bind operation.
-- If we look at the cross product of [1,2,3], [4,5,6], the streams being
-- combined using 'concatMapDigaonal' are the sequential loop iterations:
--
-- @
-- (1,4) (1,5) (1,6) -- first iteration of the outer loop
-- (2,4) (2,5) (2,6) -- second iteration of the outer loop
-- (3,4) (3,5) (3,6) -- third iteration of the outer loop
-- @
--
-- The result is a triangular or diagonal traversal of these iterations:
--
-- @
-- [(1,4),(1,5),(2,4),(1,6),(2,5),(3,4),(2,6),(3,5),(3,6)]
-- @
--
-- == Associativity Issues
--
-- WARNING! The FairNested monad breaks the associativity law intentionally for
-- usefulness, it is associative only up to permutation equivalence. In this
-- monad the association order of statements might make a difference to the
-- ordering of the results because of changing the way in which streams are
-- scheduled. The same issues arise when you use the 'interleave' operation
-- directly, association order matters - however, here it can be more subtle as
-- the programmer may not see it directly.
--
-- >>> un (mk [1,2] >>= (\x -> mk [x, x + 1] >>= (\y -> mk [y, y + 2])))
-- [1,3,2,2,4,4,3,5]
-- >>> un ((mk [1,2] >>= (\x -> mk [x, x + 1])) >>= (\y -> mk [y, y + 2]))
-- [1,3,2,4,2,4,3,5]
--
-- This type is designed to be used for use cases where ordering of results
-- does not matter, we want to explore different streams to find specific
-- results, but the order in which we find or present the results may not be
-- important. Re-association of statements in this monad may change how different
-- branches are scheduled, which may change the scheduling priority of some
-- streams over others, this may end up starving some branches - in the worst
-- case some branches may be fully starved by some infinite branches producing
-- nothing - resulting in a non-terminating program.
--
-- == Non-Termination Cases
--
-- If an infinite stream that does not produce a value at all is interleaved
-- with another stream then the entire computation gets stuck forever because
-- the interleave operation schedules the second stream only after the first
-- stream yields a value. This can lead to non-terminating programs, an example
-- is provided below.
--
-- >>> :{
-- toS = StreamK.toStream . StreamK.unFairNested
-- odds x = mk (if x then [1,3..] else [2,4..])
-- filterEven x = if even x then pure x else StreamK.FairNested StreamK.nil
-- :}
--
-- When writing code with do notation, keep in mind that when we bind a
-- variable to a monadic value, all the following code that depends on this
-- variable is associated together and connected to it via a monad bind.
-- Consider the following code:
--
-- >>> :{
-- evens = toS $ do
--     r <- mk [True,False]
--     -- The next two statements depending on the variable r are associated
--     -- together and bound to the previous line using a monad bind.
--     x <- odds r
--     filterEven x
-- :}
--
-- This code does not terminate because, when r is True, @odds@ and
-- @filterEven@ together constitute an infinite inner loop, coninuously working
-- but not yielding any value at all, this stream is interleaved with the outer
-- loop, therefore, the outer loop does not get a chance to move to the next
-- iteration.
--
-- But the following code works as expected:
--
-- >>> :{
-- evens = toS $ do
--     x <- mk [True,False] >>= odds
--     filterEven x
-- :}
--
-- >>> Stream.toList $ Stream.take 3 $ evens
-- [2,4,6]
--
-- This works because both the lists being interleaved continue to produce
-- values in the outer loop and the inner loop keeps filtering them.
--
-- Care should be taken how you write your program, keep in mind the scheduling
-- implications. To avoid such scheduling problems in the serial FairNested type
-- use the concurrent version i.e. FairParallel described in
-- 'Streamly.Data.Stream.MkType' module. Due to concurrent evaluation each
-- branch will make progress even if one is an infinite loop producing nothing.
--
-- == Related Operations
--
-- We can create a custom type with 'concatMapWith' 'interleave' as the monad
-- bind operation then the inner loop iterations get exponentially more
-- priority over the outer iterations of the nested loop. This is not fully
-- fair, it is biased - this is exactly how the logic-t and list-t
-- implementation of fair bind works.

newtype FairNested m a = FairNested {unFairNested :: StreamK m a}
        deriving (Functor, Foldable)

-- Pure (Identity monad) stream instances
deriving instance Traversable (FairNested Identity)
deriving instance IsList (FairNested Identity a)
deriving instance (a ~ Char) => IsString (FairNested Identity a)
-- deriving instance Eq a => Eq (FairNested Identity a)
-- deriving instance Ord a => Ord (FairNested Identity a)

-- Do not use automatic derivation for this to show as "fromList" rather than
-- "fromList Identity".
instance Show a => Show (FairNested Identity a) where
    {-# INLINE show #-}
    show (FairNested xs) = show xs

instance Read a => Read (FairNested Identity a) where
    {-# INLINE readPrec #-}
    readPrec = fmap FairNested readPrec

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

-- Note: we need to define all the typeclass operations because we want to
-- INLINE them.
instance Monad m => Applicative (FairNested m) where
    {-# INLINE pure #-}
    pure x = FairNested (fromPure x)

    -- XXX implement more efficient version of these
    (<*>) = ap
    {-
    {-# INLINE (<*>) #-}
    (FairNested s1) <*> (FairNested s2) =
        FairNested (crossApply s1 s2)

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

    {-# INLINE (*>) #-}
    (FairNested s1) *> (FairNested s2) =
        FairNested (crossApplySnd s1 s2)

    {-# INLINE (<*) #-}
    (FairNested s1) <* (FairNested s2) =
        FairNested (crossApplyFst s1 s2)
    -}

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (FairNested m) where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) (FairNested m) f = FairNested (fairConcatMap (unFairNested . f) m)

    -- {-# INLINE (>>) #-}
    -- (>>) = (*>)

------------------------------------------------------------------------------
-- Transformers
------------------------------------------------------------------------------

instance (MonadIO m) => MonadIO (FairNested m) where
    liftIO x = FairNested (fromEffect $ liftIO x)

instance MonadTrans FairNested where
    {-# INLINE lift #-}
    lift x = FairNested (fromEffect x)

instance (MonadThrow m) => MonadThrow (FairNested m) where
    throwM = lift . throwM
