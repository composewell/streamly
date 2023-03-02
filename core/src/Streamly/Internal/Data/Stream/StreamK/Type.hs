{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Streamly.Internal.Data.Stream.StreamK.Type
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
module Streamly.Internal.Data.Stream.StreamK.Type
    (
    -- * StreamK type
      Stream
    , StreamK (..)

    -- * CrossStreamK type wrapper
    , CrossStreamK
    , unCross
    , mkCross

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

    -- ** Cyclic
    , mfix

    -- * Elimination
    -- ** Primitives
    , uncons

    -- ** Strict Left Folds
    , Streamly.Internal.Data.Stream.StreamK.Type.foldl'
    , foldlx'

    -- ** Lazy Right Folds
    , Streamly.Internal.Data.Stream.StreamK.Type.foldr

    -- ** Specific Folds
    , drain
    , null
    , tail
    , init

    -- * Mapping
    , map
    , mapMWith
    , mapMSerial

    -- * Combining Two Streams
    -- ** Appending
    , conjoin
    , append

    -- ** Interleave
    , interleave
    , interleaveFst
    , interleaveMin

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
    , concatMap
    , bindWith
    , concatIterateWith
    , concatIterateLeftsWith
    , concatIterateScanWith

    -- * Merge
    , mergeMapWith
    , mergeIterateWith

    -- * Buffered Operations
    , foldlS
    , reverse
    )
where

#include "inline.hs"

-- import Control.Applicative (liftA2)
import Control.Monad ((>=>))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (Foldable(foldl'), fold, foldr)
import Data.Function (fix)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup (Endo(..))
import GHC.Exts (IsList(..), IsString(..), oneShot)
import Streamly.Internal.BaseCompat ((#.))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.SVar.Type (State, adaptState, defState)
import Text.Read
       ( Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec
       , readListPrecDefault)

import qualified Prelude

import Prelude hiding
    (map, mapM, concatMap, foldr, repeat, null, reverse, tail, init)

-- $setup
-- >>> import Data.Function (fix, (&))
-- >>> import Data.Semigroup (cycle1)
-- >>> import Streamly.Internal.Data.Stream.StreamK (CrossStreamK(..))
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Stream.StreamK as StreamK
-- >>> import qualified Streamly.Internal.Data.Stream.StreamK as StreamK
-- >>> import qualified Streamly.Internal.FileSystem.Dir as Dir

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
type StopK m = forall r. m r -> m r

-- | A monadic continuation, it is a function that yields a value of type "a"
-- and calls the argument (a -> m r) as a continuation with that value. We can
-- also think of it as a callback with a handler (a -> m r).  Category
-- theorists call it a codensity type, a special type of right kan extension.
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
-- an existing stream::
--
-- >>> s = 1 `StreamK.cons` 2 `StreamK.cons` 3 `StreamK.cons` StreamK.nil
-- >>> Stream.fold Fold.toList (StreamK.toStream s)
-- [1,2,3]
--
-- It can be used efficiently with 'Prelude.foldr':
--
-- >>> fromFoldable = Prelude.foldr StreamK.cons StreamK.nil
--
-- Same as the following but more efficient:
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
-- @since 0.1.1
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

-- | Create a singleton stream from a pure value.
--
-- >>> fromPure a = a `cons` StreamK.nil
-- >>> fromPure = pure
-- >>> fromPure = StreamK.fromEffect . pure
--
{-# INLINE_NORMAL fromPure #-}
fromPure :: a -> StreamK m a
fromPure a = mkStream $ \_ _ single _ -> single a

-- | Create a singleton stream from a monadic action.
--
-- >>> fromEffect m = m `consM` StreamK.nil
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
-- >>> s = putStrLn "hello" `consM` putStrLn "world" `consM` StreamK.nil
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
foldlx' step begin done m = get $ go m begin
    where
    {-# NOINLINE get #-}
    get :: StreamK m x -> m b
    get m1 =
        -- XXX we are not strictly evaluating the accumulator here. Is this
        -- okay?
        let single = return . done
        -- XXX this is foldSingleton. why foldStreamShared?
         in foldStreamShared undefined undefined single undefined m1

    -- Note, this can be implemented by making a recursive call to "go",
    -- however that is more expensive because of unnecessary recursion
    -- that cannot be tail call optimized. Unfolding recursion explicitly via
    -- continuations is much more efficient.
    go :: StreamK m a -> x -> StreamK m x
    go m1 !acc = mkStream $ \_ yld sng _ ->
        let stop = sng acc
            single a = sng $ step acc a
            -- XXX this is foldNonEmptyStream
            yieldk a r = foldStream defState yld sng undefined $
                go r (step acc a)
        in foldStream defState yieldk single stop m1

-- | Strict left associative fold.
{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> StreamK m a -> m b
foldl' step begin = foldlx' step begin id

------------------------------------------------------------------------------
-- Specialized folds
------------------------------------------------------------------------------

-- XXX use foldrM to implement folds where possible
-- XXX This (commented) definition of drain and mapM_ perform much better on
-- some benchmarks but worse on others. Need to investigate why, may there is
-- an optimization opportunity that we can exploit.
-- drain = foldrM (\_ xs -> return () >> xs) (return ())

-- |
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

-- | Appends two streams sequentially, yielding all elements from the first
-- stream, and then all elements from the second stream.
--
-- >>> s1 = StreamK.fromStream $ Stream.fromList [1,2]
-- >>> s2 = StreamK.fromStream $ Stream.fromList [3,4]
-- >>> Stream.fold Fold.toList $ StreamK.toStream $ s1 `StreamK.append` s2
-- [1,2,3,4]
--
-- This has O(n) append performance where @n@ is the number of streams. It can
-- be used to efficiently fold an infinite lazy container of streams using
-- 'concatMapWith' et. al.
--
{-# INLINE append #-}
append :: StreamK m a -> StreamK m a -> StreamK m a
-- XXX This doubles the time of toNullAp benchmark, may not be fusing properly
-- serial xs ys = augmentS (\c n -> foldrS c n xs) ys
append m1 m2 = go m1
    where
    go m = mkStream $ \st yld sng stp ->
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
        fromList <$> readPrec

    readListPrec = readListPrecDefault

instance (a ~ Char) => IsString (StreamK Identity a) where
    {-# INLINE fromString #-}
    fromString = fromList

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
        . Streamly.Internal.Data.Stream.StreamK.Type.foldr (mappend . f) mempty

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
            $ Streamly.Internal.Data.Stream.StreamK.Type.foldr
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
bindWith par m1 f = go m1
    where
        go m =
            mkStream $ \st yld sng stp ->
                let foldShared = foldStreamShared st yld sng stp
                    single a   = foldShared $ unShare (f a)
                    yieldk a r = foldShared $ unShare (f a) `par` go r
                in foldStream (adaptState st) yieldk single stp m

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

-- | Combine streams in pairs using a binary combinator, the resulting streams
-- are then combined again in pairs recursively until we get to a single
-- combined stream. The composition would thus form a binary tree.
--
-- For example, you can sort a stream using merge sort like this:
--
-- >>> s = StreamK.fromStream $ Stream.fromList [5,1,7,9,2]
-- >>> generate = StreamK.fromPure
-- >>> combine = StreamK.mergeBy compare
-- >>> Stream.fold Fold.toList $ StreamK.toStream $ StreamK.mergeMapWith combine generate s
-- [1,2,5,7,9]
--
-- Note that if the stream length is not a power of 2, the binary tree composed
-- by mergeMapWith would not be balanced, which may or may not be important
-- depending on what you are trying to achieve.
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
-- >>> f = StreamK.fromStream . either Dir.readEitherPaths (const Stream.nil)
-- >>> input = StreamK.fromPure (Left ".")
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
-- >>> f = StreamK.fromStream . either Dir.readEitherPaths (const Stream.nil)
-- >>> input = StreamK.fromPure (Left ".")
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
-- >>> input = StreamK.fromPure (Left ".")
-- >>> ls = StreamK.concatIterateLeftsWith StreamK.append (StreamK.fromStream . Dir.readEither) input
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

-- Additionally we can have m elements yield from the first stream and n
-- elements yielding from the second stream. We can also have time slicing
-- variants of positional interleaving, e.g. run first stream for m seconds and
-- run the second stream for n seconds.

-- | Interleaves two streams, yielding one element from each stream
-- alternately.  When one stream stops the rest of the other stream is used in
-- the output stream.
--
-- When joining many streams in a left associative manner earlier streams will
-- get exponential priority than the ones joining later. Because of exponential
-- weighting it can be used with 'concatMapWith' even on a large number of
-- streams.
--
{-# INLINE interleave #-}
interleave :: StreamK m a -> StreamK m a -> StreamK m a
interleave m1 m2 = mkStream $ \st yld sng stp -> do
    let stop       = foldStream st yld sng stp m2
        single a   = yld a m2
        yieldk a r = yld a (interleave m2 r)
    foldStream st yieldk single stop m1

infixr 6 `interleaveFst`

-- | Like `interleave` but stops interleaving as soon as the first stream stops.
--
{-# INLINE interleaveFst #-}
interleaveFst :: StreamK m a -> StreamK m a -> StreamK m a
interleaveFst m1 m2 = mkStream $ \st yld sng stp -> do
    let yieldFirst a r = yld a (yieldSecond r m2)
     in foldStream st yieldFirst sng stp m1

    where

    yieldSecond s1 s2 = mkStream $ \st yld sng stp -> do
            let stop       = foldStream st yld sng stp s1
                single a   = yld a s1
                yieldk a r = yld a (interleave s1 r)
             in foldStream st yieldk single stop s2

infixr 6 `interleaveMin`

-- | Like `interleave` but stops interleaving as soon as any of the two streams
-- stops.
--
{-# INLINE interleaveMin #-}
interleaveMin :: StreamK m a -> StreamK m a -> StreamK m a
interleaveMin m1 m2 = mkStream $ \st yld _ stp -> do
    let stop       = stp
        -- "single a" is defined as "yld a (interleaveMin m2 nil)" instead of
        -- "sng a" to keep the behaviour consistent with the yield
        -- continuation.
        single a   = yld a (interleaveMin m2 nil)
        yieldk a r = yld a (interleaveMin m2 r)
    foldStream st yieldk single stop m1

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

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

{-# INLINE unfoldrM #-}
unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> StreamK m a
unfoldrM = unfoldrMWith consM

-- | Generate an infinite stream by repeating a pure value.
--
-- /Pre-release/
{-# INLINE repeat #-}
repeat :: a -> StreamK m a
repeat a = let x = cons a x in x

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

{-# INLINE headPartial #-}
headPartial :: Monad m => StreamK m a -> m a
headPartial = foldrM (\x _ -> return x) (error "head of nil")

{-# INLINE tailPartial #-}
tailPartial :: StreamK m a -> StreamK m a
tailPartial m = mkStream $ \st yld sng stp ->
    let stop      = error "tail of nil"
        single _  = stp
        yieldk _ r = foldStream st yld sng stp r
    in foldStream st yieldk single stop m

-- | We can define cyclic structures using @let@:
--
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
--     f action = StreamK.unCross $ do
--         let incr n act = fmap ((+n) . snd) $ unsafeInterleaveIO act
--         x <- StreamK.mkCross $ StreamK.fromStream $ Stream.sequence $ Stream.fromList [incr 1 action, incr 2 action]
--         y <- StreamK.mkCross $ StreamK.fromStream $ Stream.fromList [4,5]
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
    xs = fix  (f . headPartial)

    -- now fix the tail recursively
    ys = mfix (tailPartial . f)

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- |
-- @
-- fromFoldable = 'Prelude.foldr' 'cons' 'nil'
-- @
--
-- Construct a stream from a 'Foldable' containing pure values:
--
-- @since 0.2.0
{-# INLINE fromFoldable #-}
fromFoldable :: Foldable f => f a -> StreamK m a
fromFoldable = Prelude.foldr cons nil

{-# INLINE fromFoldableM #-}
fromFoldableM :: (Foldable f, Monad m) => f (m a) -> StreamK m a
fromFoldableM = Prelude.foldr consM nil

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

{-# INLINE tail #-}
tail :: Applicative m => StreamK m a -> m (Maybe (StreamK m a))
tail =
    let stop      = pure Nothing
        single _  = pure $ Just nil
        yieldk _ r = pure $ Just r
    in foldStream defState yieldk single stop

-- | Extract all but the last element of the stream, if any.
--
-- Note: This will end up buffering the entire stream.
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

-- | concat . fromEffect
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

-- | A newtype wrapper for the 'StreamK' type adding a cross product style
-- monad instance.
--
-- A 'Monad' bind behaves like a @for@ loop:
--
-- >>> :{
-- Stream.fold Fold.toList $ StreamK.toStream $ StreamK.unCross $ do
--     x <- StreamK.mkCross $ StreamK.fromStream $ Stream.fromList [1,2]
--     -- Perform the following actions for each x in the stream
--     return x
-- :}
-- [1,2]
--
-- Nested monad binds behave like nested @for@ loops:
--
-- >>> :{
-- Stream.fold Fold.toList $ StreamK.toStream $ StreamK.unCross $ do
--     x <- StreamK.mkCross $ StreamK.fromStream $ Stream.fromList [1,2]
--     y <- StreamK.mkCross $ StreamK.fromStream $ Stream.fromList [3,4]
--     -- Perform the following actions for each x, for each y
--     return (x, y)
-- :}
-- [(1,3),(1,4),(2,3),(2,4)]
--
newtype CrossStreamK m a = CrossStreamK {unCrossStreamK :: StreamK m a}
        deriving (Functor, Semigroup, Monoid, Foldable)

-- | Wrap the 'StreamK' type in a 'CrossStreamK' newtype to enable cross
-- product style applicative and monad instances.
--
-- This is a type level operation with no runtime overhead.
{-# INLINE mkCross #-}
mkCross :: StreamK m a -> CrossStreamK m a
mkCross = CrossStreamK

-- | Unwrap the 'StreamK' type from 'CrossStreamK' newtype.
--
-- This is a type level operation with no runtime overhead.
{-# INLINE unCross #-}
unCross :: CrossStreamK m a -> StreamK m a
unCross = unCrossStreamK

-- Pure (Identity monad) stream instances
deriving instance Traversable (CrossStreamK Identity)
deriving instance IsList (CrossStreamK Identity a)
deriving instance (a ~ Char) => IsString (CrossStreamK Identity a)
-- deriving instance Eq a => Eq (CrossStreamK Identity a)
-- deriving instance Ord a => Ord (CrossStreamK Identity a)

-- Do not use automatic derivation for this to show as "fromList" rather than
-- "fromList Identity".
instance Show a => Show (CrossStreamK Identity a) where
    {-# INLINE show #-}
    show (CrossStreamK xs) = show xs

instance Read a => Read (CrossStreamK Identity a) where
    {-# INLINE readPrec #-}
    readPrec = fmap CrossStreamK readPrec

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

-- Note: we need to define all the typeclass operations because we want to
-- INLINE them.
instance Monad m => Applicative (CrossStreamK m) where
    {-# INLINE pure #-}
    pure x = CrossStreamK (fromPure x)

    {-# INLINE (<*>) #-}
    (CrossStreamK s1) <*> (CrossStreamK s2) =
        CrossStreamK (crossApply s1 s2)

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

    {-# INLINE (*>) #-}
    (CrossStreamK s1) *> (CrossStreamK s2) =
        CrossStreamK (crossApplySnd s1 s2)

    {-# INLINE (<*) #-}
    (CrossStreamK s1) <* (CrossStreamK s2) =
        CrossStreamK (crossApplyFst s1 s2)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (CrossStreamK m) where
    return = pure

    -- Benchmarks better with CPS bind and pure:
    -- Prime sieve (25x)
    -- n binds, breakAfterSome, filterAllIn, state transformer (~2x)
    --
    {-# INLINE (>>=) #-}
    (>>=) (CrossStreamK m) f =
        CrossStreamK (bindWith append m (unCrossStreamK . f))

    {-# INLINE (>>) #-}
    (>>) = (*>)

------------------------------------------------------------------------------
-- Transformers
------------------------------------------------------------------------------

instance (MonadIO m) => MonadIO (CrossStreamK m) where
    liftIO x = CrossStreamK (fromEffect $ liftIO x)

instance MonadTrans CrossStreamK where
    {-# INLINE lift #-}
    lift x = CrossStreamK (fromEffect x)

instance (MonadThrow m) => MonadThrow (CrossStreamK m) where
    throwM = lift . throwM
