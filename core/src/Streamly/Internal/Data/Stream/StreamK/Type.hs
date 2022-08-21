{-# LANGUAGE UndecidableInstances #-}

#include "inline.hs"

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
    -- * The stream type
      Stream (..)
    , toStreamK
    , fromStreamK

    -- * foldr/build
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

    -- * Construction
    , fromStopK
    , fromYieldK
    , consK
    , cons
    , (.:)
    , consM
    , consMBy
    , nil
    , nilM

    -- * Generation
    , fromEffect
    , fromPure
    , unfoldr
    , unfoldrMWith
    , repeat
    , repeatMWith
    , replicateMWith
    , fromIndicesMWith
    , iterateMWith
    , fromFoldable
    , fromFoldableM
    , mfix

    -- * Elimination
    , uncons
    , foldl'
    , foldlx'
    , drain
    , null
    , tail
    , init

    -- * Transformation
    , conjoin
    , serial
    , map
    , mapMWith
    , mapMSerial
    , unShare
    , concatMapWith
    , concatMap
    , bindWith
    , concatPairsWith
    , apWith
    , apSerial
    , apSerialDiscardFst
    , apSerialDiscardSnd
    , foldlS
    , reverse

    -- * Reader
    , withLocal
    )
where

import Control.Monad (ap, (>=>))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Function (fix)
import Streamly.Internal.Data.SVar.Type (State, adaptState, defState)
import Prelude hiding
    (map, mapM, concatMap, foldr, repeat, null, reverse, tail, init)
import qualified Prelude

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

-- XXX remove the Stream type parameter from State as it is always constant.
-- We can remove it from SVar as well

newtype Stream m a =
    MkStream (forall r.
               State Stream m a         -- state
            -> (a -> Stream m a -> m r) -- yield
            -> (a -> m r)               -- singleton
            -> m r                      -- stop
            -> m r
            )

{-# INLINE fromStreamK #-}
fromStreamK :: Stream m a -> Stream m a
fromStreamK = id

{-# INLINE toStreamK #-}
toStreamK :: Stream m a -> Stream m a
toStreamK = id

mkStream
    :: (forall r. State Stream m a
        -> (a -> Stream m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r)
    -> Stream m a
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
fromStopK :: StopK m -> Stream m a
fromStopK k = mkStream $ \_ _ _ stp -> k stp

-- | Make a singleton stream from a callback function. The callback function
-- calls the one-shot yield continuation to yield an element.
fromYieldK :: YieldK m a -> Stream m a
fromYieldK k = mkStream $ \_ _ sng _ -> k sng

-- | Add a yield function at the head of the stream.
consK :: YieldK m a -> Stream m a -> Stream m a
consK k r = mkStream $ \_ yld _ _ -> k (`yld` r)

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
cons :: a -> Stream m a -> Stream m a
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
(.:) :: a -> Stream m a -> Stream m a
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
nil :: Stream m a
nil = mkStream $ \_ _ _ stp -> stp

-- | An empty stream producing a side effect.
--
-- @
-- > toList (nilM (print "nil"))
-- "nil"
-- []
-- @
--
-- /Pre-release/
{-# INLINE_NORMAL nilM #-}
nilM :: Applicative m => m b -> Stream m a
nilM m = mkStream $ \_ _ _ stp -> m *> stp

{-# INLINE_NORMAL fromPure #-}
fromPure :: a -> Stream m a
fromPure a = mkStream $ \_ _ single _ -> single a

{-# INLINE_NORMAL fromEffect #-}
fromEffect :: Monad m => m a -> Stream m a
fromEffect m = mkStream $ \_ _ single _ -> m >>= single

infixr 5 `consM`

-- NOTE: specializing the function outside the instance definition seems to
-- improve performance quite a bit at times, even if we have the same
-- SPECIALIZE in the instance definition.
{-# INLINE consM #-}
{-# SPECIALIZE consM :: IO a -> Stream IO a -> Stream IO a #-}
consM :: Monad m => m a -> Stream m a -> Stream m a
consM m r = MkStream $ \_ yld _ _ -> m >>= (`yld` r)

-- XXX specialize to IO?
{-# INLINE consMBy #-}
consMBy :: Monad m =>
    (Stream m a -> Stream m a -> Stream m a) -> m a -> Stream m a -> Stream m a
consMBy f m r = fromEffect m `f` r

------------------------------------------------------------------------------
-- Folding a stream
------------------------------------------------------------------------------

-- | Fold a stream by providing an SVar, a stop continuation, a singleton
-- continuation and a yield continuation. The stream would share the current
-- SVar passed via the State.
{-# INLINE_EARLY foldStreamShared #-}
foldStreamShared
    :: State Stream m a
    -> (a -> Stream m a -> m r)
    -> (a -> m r)
    -> m r
    -> Stream m a
    -> m r
foldStreamShared s yield single stop (MkStream k) = k s yield single stop

-- | Fold a stream by providing a State, stop continuation, a singleton
-- continuation and a yield continuation. The stream will not use the SVar
-- passed via State.
{-# INLINE foldStream #-}
foldStream
    :: State Stream m a
    -> (a -> Stream m a -> m r)
    -> (a -> m r)
    -> m r
    -> Stream m a
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
    (forall r. State Stream m b
        -> (b -> Stream m b -> m r)
        -> (b -> m r)
        -> m r
        -> Stream m b
        -> m r)
    -> (a -> Stream m b -> Stream m b)
    -> Stream m b
    -> Stream m a
    -> Stream m b
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
       (a -> Stream m b -> Stream m b)
    -> Stream m b
    -> Stream m a
    -> Stream m b
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

-- | Lazy right associative fold to a stream.
{-# INLINE_NORMAL foldrS #-}
foldrS ::
       (a -> Stream m b -> Stream m b)
    -> Stream m b
    -> Stream m a
    -> Stream m b
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
    => (forall r. State Stream m b
        -> (b -> Stream m b -> m r)
        -> (b -> m r)
        -> m r
        -> Stream m b
        -> m r)
    -> (m a -> Stream m b -> Stream m b)
    -> Stream m b
    -> Stream m a
    -> Stream m b
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
    => (m a -> Stream m b -> Stream m b)
    -> Stream m b
    -> Stream m a
    -> Stream m b
foldrSM = foldrSMWith foldStream

-- {-# RULES "foldrSM/id"     foldrSM consM nil = \x -> x #-}
{-# RULES "foldrSM/nil"    forall k z.   foldrSM k z nil  = z #-}
{-# RULES "foldrSM/single" forall k z x. foldrSM k z (fromEffect x) = k x z #-}
-- {-# RULES "foldrSM/app" [1]
--  forall ys. foldrSM consM ys = \xs -> xs `conjoin` ys #-}

-- Like foldrSM but sharing the SVar state within the recostructed stream.
{-# INLINE_NORMAL foldrSMShared #-}
foldrSMShared :: Monad m
    => (m a -> Stream m b -> Stream m b)
    -> Stream m b
    -> Stream m a
    -> Stream m b
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
build :: forall m a. (forall b. (a -> b -> b) -> b -> b) -> Stream m a
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
       ((a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a)
    -> Stream m a
buildS g = g cons nil

{-# RULES "foldrS/buildS"
      forall k z
        (g :: (a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a).
      foldrS k z (buildS g) = g k z #-}

{-# RULES "foldrS/cons/buildS"
      forall k z x
        (g :: (a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a).
      foldrS k z (x `cons` buildS g) = k x (g k z) #-}

{-# RULES "foldrSShared/buildS"
      forall k z
        (g :: (a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a).
      foldrSShared k z (buildS g) = g k z #-}

{-# RULES "foldrSShared/cons/buildS"
      forall k z x
        (g :: (a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a).
      foldrSShared k z (x `cons` buildS g) = k x (g k z) #-}

-- build a stream by applying consM and nil to a build function
{-# INLINE_NORMAL buildSM #-}
buildSM :: Monad m
    => ((m a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a)
    -> Stream m a
buildSM g = g consM nil

{-# RULES "foldrSM/buildSM"
     forall k z
        (g :: (m a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a).
     foldrSM k z (buildSM g) = g k z #-}

{-# RULES "foldrSMShared/buildSM"
     forall k z
        (g :: (m a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a).
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
    => (forall r. (a -> Stream m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r
       )
    -> Stream m a
buildM g = mkStream $ \st yld sng stp ->
    g (\a r -> foldStream st yld sng stp (return a `consM` r)) sng stp

-- | Like 'buildM' but shares the SVar state across computations.
{-# INLINE_NORMAL sharedMWith #-}
sharedMWith :: Monad m
    => (m a -> Stream m a -> Stream m a)
    -> (forall r. (a -> Stream m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r
       )
    -> Stream m a
sharedMWith cns g = mkStream $ \st yld sng stp ->
    g (\a r -> foldStreamShared st yld sng stp (return a `cns` r)) sng stp

-------------------------------------------------------------------------------
-- augment
-------------------------------------------------------------------------------

{-# INLINE_NORMAL augmentS #-}
augmentS ::
       ((a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a)
    -> Stream m a
    -> Stream m a
augmentS g xs = g cons xs

{-# RULES "augmentS/nil"
    forall (g :: (a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a).
    augmentS g nil = buildS g
    #-}

{-# RULES "foldrS/augmentS"
    forall k z xs
        (g :: (a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a).
    foldrS k z (augmentS g xs) = g k (foldrS k z xs)
    #-}

{-# RULES "augmentS/buildS"
    forall (g :: (a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a)
           (h :: (a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a).
    augmentS g (buildS h) = buildS (\c n -> g c (h c n))
    #-}

{-# INLINE_NORMAL augmentSM #-}
augmentSM :: Monad m =>
       ((m a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a)
    -> Stream m a -> Stream m a
augmentSM g xs = g consM xs

{-# RULES "augmentSM/nil"
    forall
        (g :: (m a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a).
    augmentSM g nil = buildSM g
    #-}

{-# RULES "foldrSM/augmentSM"
    forall k z xs
        (g :: (m a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a).
    foldrSM k z (augmentSM g xs) = g k (foldrSM k z xs)
    #-}

{-# RULES "augmentSM/buildSM"
    forall
        (g :: (m a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a)
        (h :: (m a -> Stream m a -> Stream m a) -> Stream m a -> Stream m a).
    augmentSM g (buildSM h) = buildSM (\c n -> g c (h c n))
    #-}

-------------------------------------------------------------------------------
-- Experimental foldrM/buildM
-------------------------------------------------------------------------------

-- | Lazy right fold with a monadic step function.
{-# INLINE_NORMAL foldrM #-}
foldrM :: (a -> m b -> m b) -> m b -> Stream m a -> m b
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
        -> (a -> Stream m a -> m b)
        -> (a -> m b)
        -> m b
        -> Stream m a
        -> m b)
    -> (a -> m b -> m b)
    -> m b
    -> ((a -> Stream m a -> m b) -> (a -> m b) -> m b -> m b)
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
           (a -> Stream m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r
       )).
    foldrM step acc (buildM g) = foldrMKWith foldStream step acc g
    #-}

{-
{-# RULES "foldrM/sharedM"
    forall step acc (g :: (forall r.
           (a -> Stream m a -> m r)
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
    => (x -> a -> x) -> x -> (x -> b) -> Stream m a -> m b
foldlx' step begin done m = get $ go m begin
    where
    {-# NOINLINE get #-}
    get :: Stream m x -> m b
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
    go :: Stream m a -> x -> Stream m x
    go m1 !acc = mkStream $ \_ yld sng _ ->
        let stop = sng acc
            single a = sng $ step acc a
            -- XXX this is foldNonEmptyStream
            yieldk a r = foldStream defState yld sng undefined $
                go r (step acc a)
        in foldStream defState yieldk single stop m1

-- | Strict left associative fold.
{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> m b
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
drain :: Monad m => Stream m a -> m ()
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
null :: Monad m => Stream m a -> m Bool
-- null = foldrM (\_ _ -> return True) (return False)
null m =
    let stop      = return True
        single _  = return False
        yieldk _ _ = return False
    in foldStream defState yieldk single stop m

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

infixr 6 `serial`

-- | Appends two streams sequentially, yielding all elements from the first
-- stream, and then all elements from the second stream.
--
{-# INLINE serial #-}
serial :: Stream m a -> Stream m a -> Stream m a
-- XXX This doubles the time of toNullAp benchmark, may not be fusing properly
-- serial xs ys = augmentS (\c n -> foldrS c n xs) ys
serial m1 m2 = go m1
    where
    go m = mkStream $ \st yld sng stp ->
               let stop       = foldStream st yld sng stp m2
                   single a   = yld a m2
                   yieldk a r = yld a (go r)
               in foldStream st yieldk single stop m

-- join/merge/append streams depending on consM
{-# INLINE conjoin #-}
conjoin :: Monad m => Stream m a -> Stream m a -> Stream m a
conjoin xs = augmentSM (\c n -> foldrSM c n xs)

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

-- IMPORTANT: This is eta expanded on purpose. This should not be eta
-- reduced. This will cause a lot of regressions, probably because of some
-- rewrite rules. Ideally don't run hlint on this file.
{-# INLINE_LATE mapFB #-}
mapFB :: forall b m a.
       (b -> Stream m b -> Stream m b)
    -> (a -> b)
    -> a
    -> Stream m b
    -> Stream m b
mapFB c f = \x ys -> c (f x) ys

{-# RULES
"mapFB/mapFB" forall c f g. mapFB (mapFB c f) g = mapFB c (f . g)
"mapFB/id"    forall c.     mapFB c (\x -> x)   = c
    #-}

{-# INLINE map #-}
map :: (a -> b) -> Stream m a -> Stream m b
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
mapMSerial :: Monad m => (a -> m b) -> Stream m a -> Stream m b
mapMSerial f xs = buildSM (\c n -> foldrSMShared (mapMFB c f) n xs)

{-# INLINE mapMWith #-}
mapMWith ::
       (m b -> Stream m b -> Stream m b)
    -> (a -> m b)
    -> Stream m a
    -> Stream m b
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
instance Monad m => Functor (Stream m) where
    fmap = map

-------------------------------------------------------------------------------
-- Transformers
-------------------------------------------------------------------------------

instance MonadTrans Stream where
    {-# INLINE lift #-}
    lift = fromEffect

-------------------------------------------------------------------------------
-- Nesting
-------------------------------------------------------------------------------

-- | Detach a stream from an SVar
{-# INLINE unShare #-}
unShare :: Stream m a -> Stream m a
unShare x = mkStream $ \st yld sng stp ->
    foldStream st yld sng stp x

-- XXX the function stream and value stream can run in parallel
{-# INLINE apWith #-}
apWith ::
       (Stream m b -> Stream m b -> Stream m b)
    -> Stream m (a -> b)
    -> Stream m a
    -> Stream m b
apWith par fstream stream = go1 fstream

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

{-# INLINE apSerial #-}
apSerial ::
       Stream m (a -> b)
    -> Stream m a
    -> Stream m b
apSerial fstream stream = go1 fstream

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

{-# INLINE apSerialDiscardFst #-}
apSerialDiscardFst ::
       Stream m a
    -> Stream m b
    -> Stream m b
apSerialDiscardFst fstream stream = go1 fstream

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

{-# INLINE apSerialDiscardSnd #-}
apSerialDiscardSnd ::
       Stream m a
    -> Stream m b
    -> Stream m a
apSerialDiscardSnd fstream stream = go1 fstream

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

-- XXX This is just concatMapWith with arguments flipped. We need to keep this
-- instead of using a concatMap style definition because the bind
-- implementation in Async and WAsync streams show significant perf degradation
-- if the argument order is changed.
{-# INLINE bindWith #-}
bindWith ::
       (Stream m b -> Stream m b -> Stream m b)
    -> Stream m a
    -> (a -> Stream m b)
    -> Stream m b
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
{-# INLINE concatMapWith #-}
concatMapWith
    ::
       (Stream m b -> Stream m b -> Stream m b)
    -> (a -> Stream m b)
    -> Stream m a
    -> Stream m b
concatMapWith par f xs = bindWith par xs f

{-# INLINE concatMap #-}
concatMap :: (a -> Stream m b) -> Stream m a -> Stream m b
concatMap = concatMapWith serial

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

-- | See 'Streamly.Internal.Data.Stream.concatPairsWith' for
-- documentation.
--
{-# INLINE concatPairsWith #-}
concatPairsWith
    ::
       (Stream m b -> Stream m b -> Stream m b)
    -> (a -> Stream m b)
    -> Stream m a
    -> Stream m b
concatPairsWith combine f str = go (leafPairs str)

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

instance Monad m => Applicative (Stream m) where
    {-# INLINE pure #-}
    pure = fromPure
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
-- Like concatMap but generates stream using an unfold function. Similar to
-- unfoldMany but for StreamK.
concatUnfoldr :: IsStream t
    => (b -> t m (Maybe (a, b))) -> t m b -> t m a
concatUnfoldr = undefined
-}

------------------------------------------------------------------------------
-- MonadReader
------------------------------------------------------------------------------

{-# INLINABLE withLocal #-}
withLocal :: MonadReader r m => (r -> r) -> Stream m a -> Stream m a
withLocal f m =
    mkStream $ \st yld sng stp ->
        let single = local f . sng
            yieldk a r = local f $ yld a (withLocal f r)
        in foldStream st yieldk single (local f stp) m

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

{-# INLINE unfoldr #-}
unfoldr :: (b -> Maybe (a, b)) -> b -> Stream m a
unfoldr next s0 = build $ \yld stp ->
    let go s =
            case next s of
                Just (a, b) -> yld a (go b)
                Nothing -> stp
    in go s0

{-# INLINE unfoldrMWith #-}
unfoldrMWith :: Monad m =>
       (m a -> Stream m a -> Stream m a)
    -> (b -> m (Maybe (a, b)))
    -> b
    -> Stream m a
unfoldrMWith cns step = go

    where

    go s = sharedMWith cns $ \yld _ stp -> do
                r <- step s
                case r of
                    Just (a, b) -> yld a (go b)
                    Nothing -> stp

-- | Generate an infinite stream by repeating a pure value.
--
-- /Pre-release/
{-# INLINE repeat #-}
repeat :: a -> Stream m a
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
replicateMWith :: (m a -> Stream m a -> Stream m a) -> Int -> m a -> Stream m a
replicateMWith cns n m = go n

    where

    go cnt = if cnt <= 0 then nil else m `cns` go (cnt - 1)

{-# INLINE fromIndicesMWith #-}
fromIndicesMWith ::
    (m a -> Stream m a -> Stream m a) -> (Int -> m a) -> Stream m a
fromIndicesMWith cns gen = go 0

    where

    go i = mkStream $ \st stp sng yld -> do
        foldStreamShared st stp sng yld (gen i `cns` go (i + 1))

{-# INLINE iterateMWith #-}
iterateMWith :: Monad m =>
    (m a -> Stream m a -> Stream m a) -> (a -> m a) -> m a -> Stream m a
iterateMWith cns step = go

    where

    go s = mkStream $ \st stp sng yld -> do
        !next <- s
        foldStreamShared st stp sng yld (return next `cns` go (step next))

{-# INLINE headPartial #-}
headPartial :: Monad m => Stream m a -> m a
headPartial = foldrM (\x _ -> return x) (error "head of nil")

{-# INLINE tailPartial #-}
tailPartial :: Stream m a -> Stream m a
tailPartial m = mkStream $ \st yld sng stp ->
    let stop      = error "tail of nil"
        single _  = stp
        yieldk _ r = foldStream st yld sng stp r
    in foldStream st yieldk single stop m

{-# INLINE mfix #-}
mfix :: Monad m => (m a -> Stream m a) -> Stream m a
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
fromFoldable :: Foldable f => f a -> Stream m a
fromFoldable = Prelude.foldr cons nil

{-# INLINE fromFoldableM #-}
fromFoldableM :: (Foldable f, Monad m) => f (m a) -> Stream m a
fromFoldableM = Prelude.foldr consM nil

-------------------------------------------------------------------------------
-- Deconstruction
-------------------------------------------------------------------------------

{-# INLINE uncons #-}
uncons :: Applicative m => Stream m a -> m (Maybe (a, Stream m a))
uncons m =
    let stop = pure Nothing
        single a = pure (Just (a, nil))
        yieldk a r = pure (Just (a, r))
    in foldStream defState yieldk single stop m

{-# INLINE tail #-}
tail :: Applicative m => Stream m a -> m (Maybe (Stream m a))
tail =
    let stop      = pure Nothing
        single _  = pure $ Just nil
        yieldk _ r = pure $ Just r
    in foldStream defState yieldk single stop

{-# INLINE init #-}
init :: Applicative m => Stream m a -> m (Maybe (Stream m a))
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
    (Stream m b -> a -> Stream m b) -> Stream m b -> Stream m a -> Stream m b
foldlS step = go
    where
    go acc rest = mkStream $ \st yld sng stp ->
        let run x = foldStream st yld sng stp x
            stop = run acc
            single a = run $ step acc a
            yieldk a r = run $ go (step acc a) r
         in foldStream (adaptState st) yieldk single stop rest

{-# INLINE reverse #-}
reverse :: Stream m a -> Stream m a
reverse = foldlS (flip cons) nil
