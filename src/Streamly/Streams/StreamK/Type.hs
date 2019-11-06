{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

#include "../inline.hs"

-- |
-- Module      : Streamly.Streams.StreamK.Type
-- Copyright   : (c) 2017 Harendra Kumar
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
module Streamly.Streams.StreamK.Type
    (
    -- * A class for streams
      IsStream (..)
    , adapt

    -- * The stream type
    , Stream ()

    -- * Construction
    , mkStream
    , fromStopK
    , fromYieldK
    , consK

    -- * Elimination
    , foldStream
    , foldStreamShared
    , foldStreamSVar

    -- * foldr/build
    , foldrM
    , foldrS
    , foldrSM
    , build
    , buildS
    , buildM
    , buildSM
    , sharedM
    , augmentS
    , augmentSM

    -- instances
    , cons
    , (.:)
    , consMStream
    , consMBy
    , yieldM
    , yield

    , nil
    , nilM
    , conjoin
    , serial
    , map
    , mapM
    , mapMSerial
    , unShare
    , concatMapBy
    , concatMap
    , bindWith

    , Streaming   -- deprecated
    )
where

import Control.Monad (void, ap, (>=>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
#if __GLASGOW_HASKELL__ >= 800
import Data.Kind (Type)
#endif
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import Prelude hiding (map, mapM, concatMap, foldr)

import Streamly.Internal.Data.SVar

------------------------------------------------------------------------------
-- Basic stream type
------------------------------------------------------------------------------

-- | The type @Stream m a@ represents a monadic stream of values of type 'a'
-- constructed using actions in monad 'm'. It uses stop, singleton and yield
-- continuations equivalent to the following direct style type:
--
-- @
-- data Stream m a = Stop | Singleton a | Yield a (Stream m a)
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
--
-- XXX remove the Stream type parameter from State as it is always constant.
-- We can remove it from SVar as well
--
newtype Stream m a =
    MkStream (forall r.
               State Stream m a         -- state
            -> (a -> Stream m a -> m r) -- yield
            -> (a -> m r)               -- singleton
            -> m r                      -- stop
            -> m r
            )

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
-- @since 0.2.0
class IsStream t where
    toStream :: t m a -> Stream m a
    fromStream :: Stream m a -> t m a
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
    -- /Concurrent (do not use 'parallely' to construct infinite streams)/
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
    -- drain $ serially  $ delay |: delay |: delay |: nil
    -- drain $ parallely $ delay |: delay |: delay |: nil
    -- @
    --
    -- /Concurrent (do not use 'parallely' to construct infinite streams)/
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
-- @since 0.1.0
adapt :: (IsStream t1, IsStream t2) => t1 m a -> t2 m a
adapt = fromStream . toStream

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
    => (forall r. State Stream m a
        -> (a -> t m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r)
    -> t m a
mkStream k = fromStream $ MkStream $ \st yld sng stp ->
    let yieldk a r = yld a (toStream r)
     in k st yieldk sng stp

{-# RULES "mkStream from stream" mkStream = mkStreamFromStream #-}
mkStreamFromStream :: IsStream t
    => (forall r. State Stream m a
        -> (a -> Stream m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r)
    -> t m a
mkStreamFromStream k = fromStream $ MkStream k

{-# RULES "mkStream stream" mkStream = mkStreamStream #-}
mkStreamStream
    :: (forall r. State Stream m a
        -> (a -> Stream m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r)
    -> Stream m a
mkStreamStream = MkStream

-- | A terminal function that has no continuation to follow.
type StopK m = forall r. m r -> m r

-- | A monadic continuation, it is a function that yields a value of type "a"
-- and calls the argument (a -> m r) as a continuation with that value. We can
-- also think of it as a callback with a handler (a -> m r).  Category
-- theorists call it a codensity type, a special type of right kan extension.
type YieldK m a = forall r. (a -> m r) -> m r

_wrapM :: Monad m => m a -> YieldK m a
_wrapM m = \k -> m >>= k

-- | Make an empty stream from a stop function.
fromStopK :: IsStream t => StopK m -> t m a
fromStopK k = mkStream $ \_ _ _ stp -> k stp

-- | Make a singleton stream from a yield function.
fromYieldK :: IsStream t => YieldK m a -> t m a
fromYieldK k = mkStream $ \_ _ sng _ -> k sng

-- | Add a yield function at the head of the stream.
consK :: IsStream t => YieldK m a -> t m a -> t m a
consK k r = mkStream $ \_ yld _ _ -> k (\x -> yld x r)

-- XXX Build a stream from a repeating callback function.

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

infixr 5 `cons`

-- faster than consM because there is no bind.
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
cons a r = mkStream $ \_ yld _ _ -> yld a r

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

-- | An empty stream.
--
-- @
-- > toList nil
-- []
-- @
--
-- @since 0.1.0
{-# INLINE_NORMAL nil #-}
nil :: IsStream t => t m a
nil = mkStream $ \_ _ _ stp -> stp

-- | An empty stream producing a side effect.
--
-- @
-- > toList (nilM (print "nil"))
-- "nil"
-- []
-- @
--
-- /Internal/
{-# INLINE_NORMAL nilM #-}
nilM :: (IsStream t, Monad m) => m b -> t m a
nilM m = mkStream $ \_ _ _ stp -> m >> stp

{-# INLINE_NORMAL yield #-}
yield :: IsStream t => a -> t m a
yield a = mkStream $ \_ _ single _ -> single a

{-# INLINE_NORMAL yieldM #-}
yieldM :: (Monad m, IsStream t) => m a -> t m a
yieldM m = fromStream $ mkStream $ \_ _ single _ -> m >>= single

-- XXX specialize to IO?
{-# INLINE consMBy #-}
consMBy :: (IsStream t, MonadAsync m) => (t m a -> t m a -> t m a)
    -> m a -> t m a -> t m a
consMBy f m r = (fromStream $ yieldM m) `f` r

------------------------------------------------------------------------------
-- Folding a stream
------------------------------------------------------------------------------

-- | Fold a stream by providing an SVar, a stop continuation, a singleton
-- continuation and a yield continuation. The stream would share the current
-- SVar passed via the State.
{-# INLINE_EARLY foldStreamShared #-}
foldStreamShared
    :: IsStream t
    => State Stream m a
    -> (a -> t m a -> m r)
    -> (a -> m r)
    -> m r
    -> t m a
    -> m r
foldStreamShared st yld sng stp m =
    let yieldk a x = yld a (fromStream x)
        MkStream k = toStream m
     in k st yieldk sng stp

-- XXX write a similar rule for foldStream as well?
{-# RULES "foldStreamShared from stream"
   foldStreamShared = foldStreamSharedStream #-}
foldStreamSharedStream
    :: State Stream m a
    -> (a -> Stream m a -> m r)
    -> (a -> m r)
    -> m r
    -> Stream m a
    -> m r
foldStreamSharedStream st yld sng stp m =
    let MkStream k = toStream m
     in k st yld sng stp

-- | Fold a stream by providing a State, stop continuation, a singleton
-- continuation and a yield continuation. The stream will not use the SVar
-- passed via State.
{-# INLINE foldStream #-}
foldStream
    :: IsStream t
    => State Stream m a
    -> (a -> t m a -> m r)
    -> (a -> m r)
    -> m r
    -> t m a
    -> m r
foldStream st yld sng stp m =
    let yieldk a x = yld a (fromStream x)
        MkStream k = toStream m
     in k (adaptState st) yieldk sng stp

-- Run the stream using a run function associated with the SVar that runs the
-- streams with a captured snapshot of the monadic state.
{-# INLINE foldStreamSVar #-}
foldStreamSVar
    :: (IsStream t, MonadIO m)
    => SVar Stream m a
    -> State Stream m a          -- state
    -> (a -> t m a -> m r)       -- yield
    -> (a -> m r)                -- singleton
    -> m r                       -- stop
    -> t m a
    -> m ()
foldStreamSVar sv st yld sng stp m =
    let mrun = runInIO $ svarMrun sv
    in void $ liftIO $ mrun $ foldStreamShared st yld sng stp m

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- NOTE: specializing the function outside the instance definition seems to
-- improve performance quite a bit at times, even if we have the same
-- SPECIALIZE in the instance definition.
{-# INLINE consMStream #-}
{-# SPECIALIZE consMStream :: IO a -> Stream IO a -> Stream IO a #-}
consMStream :: (Monad m) => m a -> Stream m a -> Stream m a
consMStream m r = MkStream $ \_ yld _ _ -> m >>= \a -> yld a r

-------------------------------------------------------------------------------
-- IsStream Stream
-------------------------------------------------------------------------------

instance IsStream Stream where
    toStream = id
    fromStream = id

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> Stream IO a -> Stream IO a #-}
    consM :: Monad m => m a -> Stream m a -> Stream m a
    consM = consMStream

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> Stream IO a -> Stream IO a #-}
    (|:) :: Monad m => m a -> Stream m a -> Stream m a
    (|:) = consMStream

-------------------------------------------------------------------------------
-- foldr/build fusion
-------------------------------------------------------------------------------

-- XXX perhaps we can just use foldrSM/buildM everywhere as they are more
-- general and cover foldrS/buildS as well.

-- | The function 'f' decides how to reconstruct the stream. We could
-- reconstruct using a shared state (SVar) or without sharing the state.
--
{-# INLINE foldrSWith #-}
foldrSWith :: IsStream t
    => (forall r. State Stream m b
        -> (b -> t m b -> m r)
        -> (b -> m r)
        -> m r
        -> t m b
        -> m r)
    -> (a -> t m b -> t m b) -> t m b -> t m a -> t m b
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
foldrSShared :: IsStream t => (a -> t m b -> t m b) -> t m b -> t m a -> t m b
foldrSShared = foldrSWith foldStreamShared

-- XXX consM is a typeclass method, therefore rewritten already. Instead maybe
-- we can make consM polymorphic using rewrite rules.
-- {-# RULES "foldrSShared/id"     foldrSShared consM nil = \x -> x #-}
{-# RULES "foldrSShared/nil"
    forall k z. foldrSShared k z nil = z #-}
{-# RULES "foldrSShared/single"
    forall k z x. foldrSShared k z (yield x) = k x z #-}
-- {-# RULES "foldrSShared/app" [1]
--     forall ys. foldrSShared consM ys = \xs -> xs `conjoin` ys #-}

-- | Lazy right associative fold to a stream.
{-# INLINE_NORMAL foldrS #-}
foldrS :: IsStream t => (a -> t m b -> t m b) -> t m b -> t m a -> t m b
foldrS = foldrSWith foldStream

{-# RULES "foldrS/id"     foldrS cons nil = \x -> x #-}
{-# RULES "foldrS/nil"    forall k z.   foldrS k z nil  = z #-}
-- See notes in GHC.Base about this rule
-- {-# RULES "foldr/cons"
--  forall k z x xs. foldrS k z (x `cons` xs) = k x (foldrS k z xs) #-}
{-# RULES "foldrS/single" forall k z x. foldrS k z (yield x) = k x z #-}
-- {-# RULES "foldrS/app" [1]
--  forall ys. foldrS cons ys = \xs -> xs `conjoin` ys #-}

-------------------------------------------------------------------------------
-- foldrS with monadic cons i.e. consM
-------------------------------------------------------------------------------

{-# INLINE foldrSMWith #-}
foldrSMWith :: (IsStream t, Monad m)
    => (forall r. State Stream m b
        -> (b -> t m b -> m r)
        -> (b -> m r)
        -> m r
        -> t m b
        -> m r)
    -> (m a -> t m b -> t m b) -> t m b -> t m a -> t m b
foldrSMWith f step final m = go m
    where
    go m1 = mkStream $ \st yld sng stp ->
        let run x = f st yld sng stp x
            stop = run final
            single a = run $ step (return a) final
            yieldk a r = run $ step (return a) (go r)
         in foldStream (adaptState st) yieldk single stop m1

{-# INLINE_NORMAL foldrSM #-}
foldrSM :: (IsStream t, Monad m)
    => (m a -> t m b -> t m b) -> t m b -> t m a -> t m b
foldrSM = foldrSMWith foldStream

-- {-# RULES "foldrSM/id"     foldrSM consM nil = \x -> x #-}
{-# RULES "foldrSM/nil"    forall k z.   foldrSM k z nil  = z #-}
{-# RULES "foldrSM/single" forall k z x. foldrSM k z (yieldM x) = k x z #-}
-- {-# RULES "foldrSM/app" [1]
--  forall ys. foldrSM consM ys = \xs -> xs `conjoin` ys #-}

-- Like foldrSM but sharing the SVar state within the recostructed stream.
{-# INLINE_NORMAL foldrSMShared #-}
foldrSMShared :: (IsStream t, Monad m)
    => (m a -> t m b -> t m b) -> t m b -> t m a -> t m b
foldrSMShared = foldrSMWith foldStreamShared

-- {-# RULES "foldrSM/id"     foldrSM consM nil = \x -> x #-}
{-# RULES "foldrSMShared/nil"
    forall k z. foldrSMShared k z nil = z #-}
{-# RULES "foldrSMShared/single"
    forall k z x. foldrSMShared k z (yieldM x) = k x z #-}
-- {-# RULES "foldrSM/app" [1]
--  forall ys. foldrSM consM ys = \xs -> xs `conjoin` ys #-}

-------------------------------------------------------------------------------
-- build
-------------------------------------------------------------------------------

{-# INLINE_NORMAL build #-}
build :: IsStream t => forall a. (forall b. (a -> b -> b) -> b -> b) -> t m a
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
buildS :: IsStream t => ((a -> t m a -> t m a) -> t m a -> t m a) -> t m a
buildS g = g cons nil

{-# RULES "foldrS/buildS"
      forall k z (g :: (a -> t m a -> t m a) -> t m a -> t m a).
      foldrS k z (buildS g) = g k z #-}

{-# RULES "foldrS/cons/buildS"
      forall k z x (g :: (a -> t m a -> t m a) -> t m a -> t m a).
      foldrS k z (x `cons` buildS g) = k x (g k z) #-}

{-# RULES "foldrSShared/buildS"
      forall k z (g :: (a -> t m a -> t m a) -> t m a -> t m a).
      foldrSShared k z (buildS g) = g k z #-}

{-# RULES "foldrSShared/cons/buildS"
      forall k z x (g :: (a -> t m a -> t m a) -> t m a -> t m a).
      foldrSShared k z (x `cons` buildS g) = k x (g k z) #-}

-- build a stream by applying consM and nil to a build function
{-# INLINE_NORMAL buildSM #-}
buildSM :: (IsStream t, MonadAsync m)
    => ((m a -> t m a -> t m a) -> t m a -> t m a) -> t m a
buildSM g = g consM nil

{-# RULES "foldrSM/buildSM"
     forall k z (g :: (m a -> t m a -> t m a) -> t m a -> t m a).
     foldrSM k z (buildSM g) = g k z #-}

{-# RULES "foldrSMShared/buildSM"
     forall k z (g :: (m a -> t m a -> t m a) -> t m a -> t m a).
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
buildM :: (IsStream t, MonadAsync m)
    => (forall r. (a -> t m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r
       )
    -> t m a
buildM g = mkStream $ \st yld sng stp ->
    g (\a r -> foldStream st yld sng stp (return a `consM` r)) sng stp

-- | Like 'buildM' but shares the SVar state across computations.
{-# INLINE_NORMAL sharedM #-}
sharedM :: (IsStream t, MonadAsync m)
    => (forall r. (a -> t m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r
       )
    -> t m a
sharedM g = mkStream $ \st yld sng stp ->
    g (\a r -> foldStreamShared st yld sng stp (return a `consM` r)) sng stp

-------------------------------------------------------------------------------
-- augment
-------------------------------------------------------------------------------

{-# INLINE_NORMAL augmentS #-}
augmentS :: IsStream t
    => ((a -> t m a -> t m a) -> t m a -> t m a) -> t m a -> t m a
augmentS g xs = g cons xs

{-# RULES "augmentS/nil"
    forall (g :: (a -> t m a -> t m a) -> t m a -> t m a).
    augmentS g nil = buildS g
    #-}

{-# RULES "foldrS/augmentS"
    forall k z xs (g :: (a -> t m a -> t m a) -> t m a -> t m a).
    foldrS k z (augmentS g xs) = g k (foldrS k z xs)
    #-}

{-# RULES "augmentS/buildS"
    forall (g :: (a -> t m a -> t m a) -> t m a -> t m a)
           (h :: (a -> t m a -> t m a) -> t m a -> t m a).
    augmentS g (buildS h) = buildS (\c n -> g c (h c n))
    #-}

{-# INLINE_NORMAL augmentSM #-}
augmentSM :: (IsStream t, MonadAsync m)
    => ((m a -> t m a -> t m a) -> t m a -> t m a) -> t m a -> t m a
augmentSM g xs = g consM xs

{-# RULES "augmentSM/nil"
    forall (g :: (m a -> t m a -> t m a) -> t m a -> t m a).
    augmentSM g nil = buildSM g
    #-}

{-# RULES "foldrSM/augmentSM"
    forall k z xs (g :: (m a -> t m a -> t m a) -> t m a -> t m a).
    foldrSM k z (augmentSM g xs) = g k (foldrSM k z xs)
    #-}

{-# RULES "augmentSM/buildSM"
    forall (g :: (m a -> t m a -> t m a) -> t m a -> t m a)
           (h :: (m a -> t m a -> t m a) -> t m a -> t m a).
    augmentSM g (buildSM h) = buildSM (\c n -> g c (h c n))
    #-}

-------------------------------------------------------------------------------
-- Experimental foldrM/buildM
-------------------------------------------------------------------------------

-- | Lazy right fold with a monadic step function.
{-# INLINE_NORMAL foldrM #-}
foldrM :: IsStream t => (a -> m b -> m b) -> m b -> t m a -> m b
foldrM step acc m = go m
    where
    go m1 =
        let stop = acc
            single a = step a acc
            yieldk a r = step a (go r)
        in foldStream defState yieldk single stop m1

{-# INLINE_NORMAL foldrMKWith #-}
foldrMKWith
    :: (State Stream m a
        -> (a -> t m a -> m b)
        -> (a -> m b)
        -> m b
        -> t m a
        -> m b)
    -> (a -> m b -> m b)
    -> m b
    -> ((a -> t m a -> m b) -> (a -> m b) -> m b -> m b)
    -> m b
foldrMKWith f step acc g = go g
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
           (a -> t m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r
       )).
    foldrM step acc (buildM g) = foldrMKWith foldStream step acc g
    #-}

{-# RULES "foldrM/sharedM"
    forall step acc (g :: (forall r.
           (a -> t m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r
       )).
    foldrM step acc (sharedM g) = foldrMKWith foldStreamShared step acc g
    #-}

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'SerialT'.
-- Appends two streams sequentially, yielding all elements from the first
-- stream, and then all elements from the second stream.
--
-- @since 0.2.0
{-# INLINE serial #-}
serial :: IsStream t => t m a -> t m a -> t m a
serial xs ys = augmentS (\c n -> foldrS c n xs) ys
{-
serial m1 m2 = go m1
    where
    go m = mkStream $ \st yld sng stp ->
               let stop       = foldStream st yld sng stp m2
                   single a   = yld a m2
                   yieldk a r = yld a (go r)
               in foldStream st yieldk single stop m
-}

-- join/merge/append streams depending on consM
{-# INLINE conjoin #-}
conjoin :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
conjoin xs ys = augmentSM (\c n -> foldrSM c n xs) ys

instance Semigroup (Stream m a) where
    (<>) = serial

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance Monoid (Stream m a) where
    mempty = nil
    mappend = (<>)

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 800
#define Type *
#endif
-- Note eta expanded
{-# INLINE_LATE mapFB #-}
mapFB :: forall (t :: (Type -> Type) -> Type -> Type) b m a.
    (b -> t m b -> t m b) -> (a -> b) -> a -> t m b -> t m b
mapFB c f = \x ys -> c (f x) ys
#undef Type

{-# RULES
"mapFB/mapFB" forall c f g. mapFB (mapFB c f) g = mapFB c (f . g)
"mapFB/id"    forall c.     mapFB c (\x -> x)   = c
    #-}

{-# INLINE map #-}
map :: IsStream t => (a -> b) -> t m a -> t m b
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
mapMFB c f = \x ys -> c (x >>= f) ys

{-# RULES
    "mapMFB/mapMFB" forall c f g. mapMFB (mapMFB c f) g = mapMFB c (f >=> g)
    #-}
-- XXX These rules may never fire because pure/return type class rules will
-- fire first.
{-
"mapMFB/pure"    forall c.     mapMFB c (\x -> pure x)   = c
"mapMFB/return"  forall c.     mapMFB c (\x -> return x) = c
-}

-- Be careful when modifying this, this uses a consM (|:) deliberately to allow
-- other stream types to overload it.
{-# INLINE mapM #-}
mapM :: (IsStream t, MonadAsync m) => (a -> m b) -> t m a -> t m b
mapM f = foldrSShared (\x xs -> f x `consM` xs) nil
-- See note under map definition above.
{-
mapM f m = go m
    where
    go m1 = mkStream $ \st yld sng stp ->
        let single a  = f a >>= sng
            yieldk a r = foldStreamShared st yld sng stp $ f a |: go r
         in foldStream (adaptState st) yieldk single stp m1
         -}

-- This is experimental serial version supporting fusion.
--
-- XXX what if we do not want to fuse two concurrent mapMs?
-- XXX we can combine two concurrent mapM only if the SVar is of the same type
-- So for now we use it only for serial streams.
-- XXX fusion would be easier for monomoprhic stream types.
-- {-# RULES "mapM serial" mapM = mapMSerial #-}
{-# INLINE mapMSerial #-}
mapMSerial :: MonadAsync m => (a -> m b) -> Stream m a -> Stream m b
mapMSerial f xs = buildSM (\c n -> foldrSMShared (mapMFB c f) n xs)

-- XXX in fact use the Stream type everywhere and only use polymorphism in the
-- high level modules/prelude.
instance Monad m => Functor (Stream m) where
    fmap = map

-------------------------------------------------------------------------------
-- Transformers
-------------------------------------------------------------------------------

instance MonadTrans Stream where
    lift = yieldM

-------------------------------------------------------------------------------
-- Nesting
-------------------------------------------------------------------------------

-- | Detach a stream from an SVar
{-# INLINE unShare #-}
unShare :: IsStream t => t m a -> t m a
unShare x = mkStream $ \st yld sng stp ->
    foldStream st yld sng stp x

-- XXX This is just concatMapBy with arguments flipped. We need to keep this
-- instead of using a concatMap style definition because the bind
-- implementation in Async and WAsync streams show significant perf degradation
-- if the argument order is changed.
{-# INLINE bindWith #-}
bindWith
    :: IsStream t
    => (forall c. t m c -> t m c -> t m c)
    -> t m a
    -> (a -> t m b)
    -> t m b
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
-- streams generated by the map function. For example, the concat function
-- could be 'serial', 'parallel', 'async', 'ahead' or any other zip or merge
-- function.
--
-- @since 0.7.0
{-# INLINE concatMapBy #-}
concatMapBy
    :: IsStream t
    => (forall c. t m c -> t m c -> t m c)
    -> (a -> t m b)
    -> t m a
    -> t m b
concatMapBy par f xs = bindWith par xs f

{-# INLINE concatMap #-}
concatMap :: IsStream t => (a -> t m b) -> t m a -> t m b
concatMap f m = fromStream $
    concatMapBy serial
        (\a -> adapt $ toStream $ f a)
        (adapt $ toStream m)

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

instance Monad m => Applicative (Stream m) where
    {-# INLINE pure #-}
    pure = yield
    {-# INLINE (<*>) #-}
    (<*>) = ap

-- NOTE: even though concatMap for StreamD is 3x faster compared to StreamK,
-- the monad instance of StreamD is slower than StreamK after foldr/build
-- fusion.
instance Monad m => Monad (Stream m) where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    (>>=) = flip concatMap

{-
-- ConcatMap recursively on itself using a merge strategy.
concatLoopBy :: IsStream t
    => (forall c. t m c -> t m c -> t m c)
    -> (a -> t m a) -> t m a
concatLoopBy = undefined

-- This is mfix. Put another way, concatMap recursively on the output of a
-- stream. Compare with iterate.
concatLoop :: IsStream t => (a -> t m a) -> t m a
concatLoop = concatLoopBy serial

instance MonadFix (Stream m) where
    mfix = concatLoop

-- The SVar implementation is something similar to concatFeedBack, so maybe
-- there is an opportunity to share the implementation here. In an SVar we run
-- a part of an action (a stream), it yields an output and the rest of the
-- stream, we yield the output and queue back the rest of the stream for
-- further evaluation. Also see unfoldrA.
--
-- There could be multiple variants of this combinator, for example use a
-- specific way of concating i.e. concatLoopBy. Which also includes combinators
-- with different stop behaviors. For example if the Left values are errors we
-- can stop the whole composition on errors or on specific errors.
--
-- We can also flip the serial/ahead append behavior e.g. instead of processing
-- the Left output after the original stream we can reverse the order.
--
-- | Concat map on the 'Left' output of a stream and merge it back into the
-- stream. The right output is yielded in the output stream.
concatFeedBack :: IsStream t
    => (b -> t m (Either a b)) -> t m (Either a b) -> t m a
concatFeedBack = undefined

-- Compare this with unfoldr. Start with a seed stream and generate a stream
-- with a value and new seeds. The new seeds are fed back to generate a seed
-- stream and so on. This is a stream level unfoldr.
concatUnfoldr :: IsStream t
    => (b -> t m (Maybe (a, b))) -> t m (Maybe (a, b)) -> t m a
concatUnfoldr = undefined
-}
