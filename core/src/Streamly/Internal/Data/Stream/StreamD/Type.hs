{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Type
-- Copyright   : (c) 2018 Composewell Technologies
--               (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

-- The stream type is inspired by the vector package.  A few functions in this
-- module have been originally adapted from the vector package (c) Roman
-- Leshchinskiy. See the notes in specific functions.

module Streamly.Internal.Data.Stream.StreamD.Type
    (
    -- * The stream type
      Step (..)
    -- XXX UnStream is exported to avoid a performance issue in some
    -- combinators if we use the pattern synonym "Stream".
    , Stream (Stream, UnStream)

    -- * CrossStream type wrapper
    , CrossStream
    , unCross
    , mkCross

    -- * Conversion to StreamK
    , fromStreamK
    , toStreamK

    -- * From Unfold
    , unfold

    -- * Construction
    -- ** Primitives
    , nilM
    , consM

    -- ** From Values
    , fromPure
    , fromEffect

    -- ** From Containers
    , Streamly.Internal.Data.Stream.StreamD.Type.fromList

    -- * Elimination
    -- ** Primitives
    , uncons

    -- ** Strict Left Folds
    , Streamly.Internal.Data.Stream.StreamD.Type.fold
    , foldBreak
    , foldAddLazy
    , foldAdd
    , foldEither

    , Streamly.Internal.Data.Stream.StreamD.Type.foldl'
    , foldlM'
    , foldlx'
    , foldlMx'

    -- ** Lazy Right Folds
    , foldrM
    , foldrMx
    , Streamly.Internal.Data.Stream.StreamD.Type.foldr
    , foldrS

    -- ** Specific Folds
    , drain
    , Streamly.Internal.Data.Stream.StreamD.Type.toList

    -- * Mapping
    , map
    , mapM

    -- * Stateful Filters
    , take
    , takeWhile
    , takeWhileM
    , takeEndBy
    , takeEndByM

    -- * Combining Two Streams
    -- ** Zipping
    , zipWithM
    , zipWith

    -- ** Cross Product
    , crossApply
    , crossApplyFst
    , crossApplySnd
    , crossWith
    , cross

    -- * Unfold Many
    , ConcatMapUState (..)
    , unfoldMany

    -- * Concat
    , concatEffect
    , concatMap
    , concatMapM
    , concat

    -- * Unfold Iterate
    , unfoldIterateDfs
    , unfoldIterateBfs
    , unfoldIterateBfsRev

    -- * Concat Iterate
    , concatIterateScan
    , concatIterateDfs
    , concatIterateBfs
    , concatIterateBfsRev

    -- * Fold Many
    , FoldMany (..) -- for inspection testing
    , FoldManyPost (..)
    , foldMany
    , foldManyPost
    , groupsOf
    , refoldMany

    -- * Fold Iterate
    , reduceIterateBfs
    , foldIterateBfs

    -- * Multi-stream folds
    , eqBy
    , cmpBy
    )
where

#include "inline.hs"

import Control.Applicative (liftA2)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (Foldable(foldl'), fold, foldr)
import Data.Functor (($>))
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup (Endo(..))
import Fusion.Plugin.Types (Fuse(..))
import GHC.Base (build)
import GHC.Exts (IsList(..), IsString(..), oneShot)
import GHC.Types (SPEC(..))
import Prelude hiding (map, mapM, take, concatMap, takeWhile, zipWith, concat)
import Text.Read
       ( Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec
       , readListPrecDefault)

import Streamly.Internal.BaseCompat ((#.))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.Refold (Refold(..))
import Streamly.Internal.Data.Stream.StreamD.Step (Step (..))
import Streamly.Internal.Data.SVar.Type (State, adaptState, defState)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import qualified Streamly.Internal.Data.Fold.Type as FL hiding (foldr)
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
#ifdef USE_UNFOLDS_EVERYWHERE
import qualified Streamly.Internal.Data.Unfold.Type as Unfold
#endif

#include "DocTestDataStream.hs"

------------------------------------------------------------------------------
-- The direct style stream type
------------------------------------------------------------------------------

-- gst = global state

-- | A stream consists of a step function that generates the next step given a
-- current state, and the current state.
data Stream m a =
    forall s. UnStream (State K.StreamK m a -> s -> m (Step s a)) s

-- XXX This causes perf trouble when pattern matching with "Stream"  in a
-- recursive way, e.g. in uncons, foldBreak, concatMap. We need to get rid of
-- this.
unShare :: Stream m a -> Stream m a
unShare (UnStream step state) = UnStream step' state
    where step' gst = step (adaptState gst)

pattern Stream :: (State K.StreamK m a -> s -> m (Step s a)) -> s -> Stream m a
pattern Stream step state <- (unShare -> UnStream step state)
    where Stream = UnStream

{-# COMPLETE Stream #-}

------------------------------------------------------------------------------
-- Primitives
------------------------------------------------------------------------------

-- | A stream that terminates without producing any output, but produces a side
-- effect.
--
-- >>> Stream.fold Fold.toList (Stream.nilM (print "nil"))
-- "nil"
-- []
--
-- /Pre-release/
{-# INLINE_NORMAL nilM #-}
nilM :: Applicative m => m b -> Stream m a
nilM m = Stream (\_ _ -> m $> Stop) ()

-- | Like 'cons' but fuses an effect instead of a pure value.
{-# INLINE_NORMAL consM #-}
consM :: Applicative m => m a -> Stream m a -> Stream m a
consM m (Stream step state) = Stream step1 Nothing

    where

    {-# INLINE_LATE step1 #-}
    step1 _ Nothing = (`Yield` Just state) <$> m
    step1 gst (Just st) = do
          (\case
            Yield a s -> Yield a (Just s)
            Skip  s   -> Skip (Just s)
            Stop      -> Stop) <$> step gst st

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns @Just (a, ma)@, where @a@ is
-- the head of the stream and @ma@ its tail.
--
-- Properties:
--
-- >>> Nothing <- Stream.uncons Stream.nil
-- >>> Just ("a", t) <- Stream.uncons (Stream.cons "a" Stream.nil)
--
-- This can be used to consume the stream in an imperative manner one element
-- at a time, as it just breaks down the stream into individual elements and we
-- can loop over them as we deem fit. For example, this can be used to convert
-- a streamly stream into other stream types.
--
-- All the folds in this module can be expressed in terms of 'uncons', however,
-- this is generally less efficient than specific folds because it takes apart
-- the stream one element at a time, therefore, does not take adavantage of
-- stream fusion.
--
-- 'foldBreak' is a more general way of consuming a stream piecemeal.
--
-- >>> :{
-- uncons xs = do
--     r <- Stream.foldBreak Fold.one xs
--     return $ case r of
--         (Nothing, _) -> Nothing
--         (Just h, t) -> Just (h, t)
-- :}
--
{-# INLINE_NORMAL uncons #-}
uncons :: Monad m => Stream m a -> m (Maybe (a, Stream m a))
uncons (UnStream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield x s -> return $ Just (x, Stream step s)
            Skip  s   -> go SPEC s
            Stop      -> return Nothing

------------------------------------------------------------------------------
-- From 'Unfold'
------------------------------------------------------------------------------

data UnfoldState s = UnfoldNothing | UnfoldJust s

-- | Convert an 'Unfold' into a stream by supplying it an input seed.
--
-- >>> s = Stream.unfold Unfold.replicateM (3, putStrLn "hello")
-- >>> Stream.fold Fold.drain s
-- hello
-- hello
-- hello
--
{-# INLINE_NORMAL unfold #-}
unfold :: Applicative m => Unfold m a b -> a -> Stream m b
unfold (Unfold ustep inject) seed = Stream step UnfoldNothing

    where

    {-# INLINE_LATE step #-}
    step _ UnfoldNothing = Skip . UnfoldJust <$> inject seed
    step _ (UnfoldJust st) = do
        (\case
            Yield x s -> Yield x (UnfoldJust s)
            Skip s    -> Skip (UnfoldJust s)
            Stop      -> Stop) <$> ustep st

------------------------------------------------------------------------------
-- From Values
------------------------------------------------------------------------------

-- | Create a singleton stream from a pure value.
--
-- >>> fromPure a = a `Stream.cons` Stream.nil
-- >>> fromPure = pure
-- >>> fromPure = Stream.fromEffect . pure
--
{-# INLINE_NORMAL fromPure #-}
fromPure :: Applicative m => a -> Stream m a
fromPure x = Stream (\_ s -> pure $ step undefined s) True
  where
    {-# INLINE_LATE step #-}
    step _ True  = Yield x False
    step _ False = Stop

-- | Create a singleton stream from a monadic action.
--
-- >>> fromEffect m = m `Stream.consM` Stream.nil
-- >>> fromEffect = Stream.sequence . Stream.fromPure
--
-- >>> Stream.fold Fold.drain $ Stream.fromEffect (putStrLn "hello")
-- hello
--
{-# INLINE_NORMAL fromEffect #-}
fromEffect :: Applicative m => m a -> Stream m a
fromEffect m = Stream step True

    where

    {-# INLINE_LATE step #-}
    step _ True  = (`Yield` False) <$> m
    step _ False = pure Stop

------------------------------------------------------------------------------
-- From Containers
------------------------------------------------------------------------------

-- Adapted from the vector package.

-- | Construct a stream from a list of pure values.
{-# INLINE_LATE fromList #-}
fromList :: Applicative m => [a] -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
fromList = unfold Unfold.fromList
#else
fromList = Stream step
  where
    {-# INLINE_LATE step #-}
    step _ (x:xs) = pure $ Yield x xs
    step _ []     = pure Stop
#endif

------------------------------------------------------------------------------
-- Conversions From/To
------------------------------------------------------------------------------

-- | Convert a CPS encoded StreamK to direct style step encoded StreamD
{-# INLINE_LATE fromStreamK #-}
fromStreamK :: Applicative m => K.StreamK m a -> Stream m a
fromStreamK = Stream step
    where
    step gst m1 =
        let stop       = pure Stop
            single a   = pure $ Yield a K.nil
            yieldk a r = pure $ Yield a r
         in K.foldStreamShared gst yieldk single stop m1

-- | Convert a direct style step encoded StreamD to a CPS encoded StreamK
{-# INLINE_LATE toStreamK #-}
toStreamK :: Monad m => Stream m a -> K.StreamK m a
toStreamK (Stream step state) = go state
    where
    go st = K.MkStream $ \gst yld _ stp ->
      let go' ss = do
           r <- step gst ss
           case r of
               Yield x s -> yld x (go s)
               Skip  s   -> go' s
               Stop      -> stp
      in go' st

#ifndef DISABLE_FUSION
{-# RULES "fromStreamK/toStreamK fusion"
    forall s. toStreamK (fromStreamK s) = s #-}
{-# RULES "toStreamK/fromStreamK fusion"
    forall s. fromStreamK (toStreamK s) = s #-}
#endif

------------------------------------------------------------------------------
-- Running a 'Fold'
------------------------------------------------------------------------------

-- | Fold resulting in either breaking the stream or continuation of the fold.
-- Instead of supplying the input stream in one go we can run the fold multiple
-- times, each time supplying the next segment of the input stream. If the fold
-- has not yet finished it returns a fold that can be run again otherwise it
-- returns the fold result and the residual stream.
--
-- /Internal/
{-# INLINE_NORMAL foldEither #-}
foldEither :: Monad m =>
    Fold m a b -> Stream m a -> m (Either (Fold m a b) (b, Stream m a))
foldEither (Fold fstep begin done) (UnStream step state) = do
    res <- begin
    case res of
        FL.Partial fs -> go SPEC fs state
        FL.Done fb -> return $! Right (fb, Stream step state)

    where

    {-# INLINE go #-}
    go !_ !fs st = do
        r <- step defState st
        case r of
            Yield x s -> do
                res <- fstep fs x
                case res of
                    FL.Done b -> return $! Right (b, Stream step s)
                    FL.Partial fs1 -> go SPEC fs1 s
            Skip s -> go SPEC fs s
            Stop -> return $! Left (Fold fstep (return $ FL.Partial fs) done)

-- | Like 'fold' but also returns the remaining stream. The resulting stream
-- would be 'Stream.nil' if the stream finished before the fold.
--
{-# INLINE_NORMAL foldBreak #-}
foldBreak :: Monad m => Fold m a b -> Stream m a -> m (b, Stream m a)
foldBreak fld strm = do
    r <- foldEither fld strm
    case r of
        Right res -> return res
        Left (Fold _ initial extract) -> do
            res <- initial
            case res of
                FL.Done _ -> error "foldBreak: unreachable state"
                FL.Partial s -> do
                    b <- extract s
                    return (b, nil)

    where

    nil = Stream (\_ _ -> return Stop) ()

-- >>> fold f = Fold.extractM . Stream.foldAddLazy f
-- >>> fold f = Stream.fold Fold.one . Stream.foldManyPost f
-- >>> fold f = Fold.extractM <=< Stream.foldAdd f

-- | Fold a stream using the supplied left 'Fold' and reducing the resulting
-- expression strictly at each step. The behavior is similar to 'foldl''. A
-- 'Fold' can terminate early without consuming the full stream. See the
-- documentation of individual 'Fold's for termination behavior.
--
-- Definitions:
--
-- >>> fold f = fmap fst . Stream.foldBreak f
-- >>> fold f = Stream.parse (Parser.fromFold f)
--
-- Example:
--
-- >>> Stream.fold Fold.sum (Stream.enumerateFromTo 1 100)
-- 5050
--
{-# INLINE_NORMAL fold #-}
fold :: Monad m => Fold m a b -> Stream m a -> m b
fold fld strm = do
    (b, _) <- foldBreak fld strm
    return b

-- | Append a stream to a fold lazily to build an accumulator incrementally.
--
-- Example, to continue folding a list of streams on the same sum fold:
--
-- >>> streams = [Stream.fromList [1..5], Stream.fromList [6..10]]
-- >>> f = Prelude.foldl Stream.foldAddLazy Fold.sum streams
-- >>> Stream.fold f Stream.nil
-- 55
--
{-# INLINE_NORMAL foldAddLazy #-}
foldAddLazy :: Monad m => Fold m a b -> Stream m a -> Fold m a b
foldAddLazy (Fold fstep finitial fextract) (Stream sstep state) =
    Fold fstep initial fextract

    where

    initial = do
        res <- finitial
        case res of
            FL.Partial fs -> go SPEC fs state
            FL.Done fb -> return $ FL.Done fb

    {-# INLINE go #-}
    go !_ !fs st = do
        r <- sstep defState st
        case r of
            Yield x s -> do
                res <- fstep fs x
                case res of
                    FL.Done b -> return $ FL.Done b
                    FL.Partial fs1 -> go SPEC fs1 s
            Skip s -> go SPEC fs s
            Stop -> return $ FL.Partial fs

-- >>> foldAdd f = Stream.foldAddLazy f >=> Fold.reduce

-- |
-- >>> foldAdd = flip Fold.addStream
--
foldAdd :: Monad m => Fold m a b -> Stream m a -> m (Fold m a b)
foldAdd f =
    Streamly.Internal.Data.Stream.StreamD.Type.fold (FL.duplicate f)

------------------------------------------------------------------------------
-- Right Folds
------------------------------------------------------------------------------

-- Adapted from the vector package.
--
-- XXX Use of SPEC constructor in folds causes 2x performance degradation in
-- one shot operations, but helps immensely in operations composed of multiple
-- combinators or the same combinator many times. There seems to be an
-- opportunity to optimize here, can we get both, better perf for single ops
-- as well as composed ops? Without SPEC, all single operation benchmarks
-- become 2x faster.

-- The way we want a left fold to be strict, dually we want the right fold to
-- be lazy.  The correct signature of the fold function to keep it lazy must be
-- (a -> m b -> m b) instead of (a -> b -> m b). We were using the latter
-- earlier, which is incorrect. In the latter signature we have to feed the
-- value to the fold function after evaluating the monadic action, depending on
-- the bind behavior of the monad, the action may get evaluated immediately
-- introducing unnecessary strictness to the fold. If the implementation is
-- lazy the following example, must work:
--
-- S.foldrM (\x t -> if x then return t else return False) (return True)
--  (S.fromList [False,undefined] :: Stream IO Bool)

-- | Right associative/lazy pull fold. @foldrM build final stream@ constructs
-- an output structure using the step function @build@. @build@ is invoked with
-- the next input element and the remaining (lazy) tail of the output
-- structure. It builds a lazy output expression using the two. When the "tail
-- structure" in the output expression is evaluated it calls @build@ again thus
-- lazily consuming the input @stream@ until either the output expression built
-- by @build@ is free of the "tail" or the input is exhausted in which case
-- @final@ is used as the terminating case for the output structure. For more
-- details see the description in the previous section.
--
-- Example, determine if any element is 'odd' in a stream:
--
-- >>> s = Stream.fromList (2:4:5:undefined)
-- >>> step x xs = if odd x then return True else xs
-- >>> Stream.foldrM step (return False) s
-- True
--
{-# INLINE_NORMAL foldrM #-}
foldrM :: Monad m => (a -> m b -> m b) -> m b -> Stream m a -> m b
foldrM f z (Stream step state) = go SPEC state
  where
    {-# INLINE_LATE go #-}
    go !_ st = do
          r <- step defState st
          case r of
            Yield x s -> f x (go SPEC s)
            Skip s    -> go SPEC s
            Stop      -> z

{-# INLINE_NORMAL foldrMx #-}
foldrMx :: Monad m
    => (a -> m x -> m x) -> m x -> (m x -> m b) -> Stream m a -> m b
foldrMx fstep final convert (Stream step state) = convert $ go SPEC state
  where
    {-# INLINE_LATE go #-}
    go !_ st = do
          r <- step defState st
          case r of
            Yield x s -> fstep x (go SPEC s)
            Skip s    -> go SPEC s
            Stop      -> final

-- XXX Should we make all argument strict wherever we use SPEC?

-- Note that foldr works on pure values, therefore it becomes necessarily
-- strict when the monad m is strict. In that case it cannot terminate early,
-- it would evaluate all of its input.  Though, this should work fine with lazy
-- monads. For example, if "any" is implemented using "foldr" instead of
-- "foldrM" it performs the same with Identity monad but performs 1000x slower
-- with IO monad.

-- | Right fold, lazy for lazy monads and pure streams, and strict for strict
-- monads.
--
-- Please avoid using this routine in strict monads like IO unless you need a
-- strict right fold. This is provided only for use in lazy monads (e.g.
-- Identity) or pure streams. Note that with this signature it is not possible
-- to implement a lazy foldr when the monad @m@ is strict. In that case it
-- would be strict in its accumulator and therefore would necessarily consume
-- all its input.
--
-- >>> foldr f z = Stream.foldrM (\a b -> f a <$> b) (return z)
--
-- Note: This is similar to Fold.foldr' (the right fold via left fold), but
-- could be more efficient.
--
{-# INLINE_NORMAL foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> Stream m a -> m b
foldr f z = foldrM (liftA2 f . return) (return z)

-- this performs horribly, should not be used
{-# INLINE_NORMAL foldrS #-}
foldrS
    :: Monad m
    => (a -> Stream m b -> Stream m b)
    -> Stream m b
    -> Stream m a
    -> Stream m b
foldrS f final (Stream step state) = go SPEC state
  where
    {-# INLINE_LATE go #-}
    go !_ st = concatEffect $ fmap g $ step defState st

    g r =
        case r of
          Yield x s -> f x (go SPEC s)
          Skip s    -> go SPEC s
          Stop      -> final

------------------------------------------------------------------------------
-- Left Folds
------------------------------------------------------------------------------

-- XXX run begin action only if the stream is not empty.
{-# INLINE_NORMAL foldlMx' #-}
foldlMx' :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Stream m a -> m b
foldlMx' fstep begin done (Stream step state) =
    begin >>= \x -> go SPEC x state
  where
    -- XXX !acc?
    {-# INLINE_LATE go #-}
    go !_ acc st = acc `seq` do
        r <- step defState st
        case r of
            Yield x s -> do
                acc' <- fstep acc x
                go SPEC acc' s
            Skip s -> go SPEC acc s
            Stop   -> done acc

{-# INLINE foldlx' #-}
foldlx' :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream m a -> m b
foldlx' fstep begin done =
    foldlMx' (\b a -> return (fstep b a)) (return begin) (return . done)

-- Adapted from the vector package.
-- XXX implement in terms of foldlMx'?
{-# INLINE_NORMAL foldlM' #-}
foldlM' :: Monad m => (b -> a -> m b) -> m b -> Stream m a -> m b
foldlM' fstep mbegin (Stream step state) = do
    begin <- mbegin
    go SPEC begin state
  where
    {-# INLINE_LATE go #-}
    go !_ acc st = acc `seq` do
        r <- step defState st
        case r of
            Yield x s -> do
                acc' <- fstep acc x
                go SPEC acc' s
            Skip s -> go SPEC acc s
            Stop   -> return acc

{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> m b
foldl' fstep begin = foldlM' (\b a -> return (fstep b a)) (return begin)

------------------------------------------------------------------------------
-- Special folds
------------------------------------------------------------------------------

-- >>> drain = mapM_ (\_ -> return ())

-- |
-- Definitions:
--
-- >>> drain = Stream.fold Fold.drain
-- >>> drain = Stream.foldrM (\_ xs -> xs) (return ())
--
-- Run a stream, discarding the results.
--
{-# INLINE_LATE drain #-}
drain :: Monad m => Stream m a -> m ()
-- drain = foldrM (\_ xs -> xs) (return ())
drain (Stream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield _ s -> go SPEC s
            Skip s    -> go SPEC s
            Stop      -> return ()

------------------------------------------------------------------------------
-- To Containers
------------------------------------------------------------------------------

-- This toList impl is faster (30% on streaming-benchmarks) than the
-- corresponding left fold. The left fold retains an additional argument in the
-- recursive loop.
--
-- Core for the right fold loop:
--
-- main_$s$wgo1
--   = \ sc_s3e6 sc1_s3e5 ->
--       case ># sc1_s3e5 100000# of {
--         __DEFAULT ->
--           case main_$s$wgo1 sc_s3e6 (+# sc1_s3e5 1#) of
--
-- Core for the left fold loop:
--
--  main_$s$wgo1
--   = \ sc_s3oT sc1_s3oS sc2_s3oR ->
--       case sc2_s3oR of fs2_a2lw { __DEFAULT ->
--       case ># sc1_s3oS 100000# of {
--         __DEFAULT ->
--           let { wild_a2og = I# sc1_s3oS } in
--           main_$s$wgo1
--             sc_s3oT (+# sc1_s3oS 1#) (\ x_X9 -> fs2_a2lw (: wild_a2og x_X9));

-- |
-- Definitions:
--
-- >>> toList = Stream.foldr (:) []
-- >>> toList = Stream.fold Fold.toList
--
-- Convert a stream into a list in the underlying monad. The list can be
-- consumed lazily in a lazy monad (e.g. 'Identity'). In a strict monad (e.g.
-- IO) the whole list is generated and buffered before it can be consumed.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Data.Array" instead.
--
-- Note that this could a bit more efficient compared to @Stream.fold
-- Fold.toList@, and it can fuse with pure list consumers.
--
{-# INLINE_NORMAL toList #-}
toList :: Monad m => Stream m a -> m [a]
toList = Streamly.Internal.Data.Stream.StreamD.Type.foldr (:) []

-- Use foldr/build fusion to fuse with list consumers
-- This can be useful when using the IsList instance
{-# INLINE_LATE toListFB #-}
toListFB :: (a -> b -> b) -> b -> Stream Identity a -> b
toListFB c n (Stream step state) = go state
  where
    go st = case runIdentity (step defState st) of
             Yield x s -> x `c` go s
             Skip s    -> go s
             Stop      -> n

{-# RULES "toList Identity" Streamly.Internal.Data.Stream.StreamD.Type.toList = toListId #-}
{-# INLINE_EARLY toListId #-}
toListId :: Stream Identity a -> Identity [a]
toListId s = Identity $ build (\c n -> toListFB c n s)

------------------------------------------------------------------------------
-- Multi-stream folds
------------------------------------------------------------------------------

-- Adapted from the vector package.

-- | Compare two streams for equality
{-# INLINE_NORMAL eqBy #-}
eqBy :: Monad m => (a -> b -> Bool) -> Stream m a -> Stream m b -> m Bool
eqBy eq (Stream step1 t1) (Stream step2 t2) = eq_loop0 SPEC t1 t2
  where
    eq_loop0 !_ s1 s2 = do
      r <- step1 defState s1
      case r of
        Yield x s1' -> eq_loop1 SPEC x s1' s2
        Skip    s1' -> eq_loop0 SPEC   s1' s2
        Stop        -> eq_null s2

    eq_loop1 !_ x s1 s2 = do
      r <- step2 defState s2
      case r of
        Yield y s2'
          | eq x y    -> eq_loop0 SPEC   s1 s2'
          | otherwise -> return False
        Skip    s2'   -> eq_loop1 SPEC x s1 s2'
        Stop          -> return False

    eq_null s2 = do
      r <- step2 defState s2
      case r of
        Yield _ _ -> return False
        Skip s2'  -> eq_null s2'
        Stop      -> return True

-- Adapted from the vector package.

-- | Compare two streams lexicographically.
{-# INLINE_NORMAL cmpBy #-}
cmpBy
    :: Monad m
    => (a -> b -> Ordering) -> Stream m a -> Stream m b -> m Ordering
cmpBy cmp (Stream step1 t1) (Stream step2 t2) = cmp_loop0 SPEC t1 t2
  where
    cmp_loop0 !_ s1 s2 = do
      r <- step1 defState s1
      case r of
        Yield x s1' -> cmp_loop1 SPEC x s1' s2
        Skip    s1' -> cmp_loop0 SPEC   s1' s2
        Stop        -> cmp_null s2

    cmp_loop1 !_ x s1 s2 = do
      r <- step2 defState s2
      case r of
        Yield y s2' -> case x `cmp` y of
                         EQ -> cmp_loop0 SPEC s1 s2'
                         c  -> return c
        Skip    s2' -> cmp_loop1 SPEC x s1 s2'
        Stop        -> return GT

    cmp_null s2 = do
      r <- step2 defState s2
      case r of
        Yield _ _ -> return LT
        Skip s2'  -> cmp_null s2'
        Stop      -> return EQ

------------------------------------------------------------------------------
-- Transformations
------------------------------------------------------------------------------

-- Adapted from the vector package.

-- |
-- >>> mapM f = Stream.sequence . fmap f
--
-- Apply a monadic function to each element of the stream and replace it with
-- the output of the resulting action.
--
-- >>> s = Stream.fromList ["a", "b", "c"]
-- >>> Stream.fold Fold.drain $ Stream.mapM putStr s
-- abc
--
{-# INLINE_NORMAL mapM #-}
mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
mapM f (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> f x >>= \a -> return $ Yield a s
            Skip s    -> return $ Skip s
            Stop      -> return Stop

{-# INLINE map #-}
map :: Monad m => (a -> b) -> Stream m a -> Stream m b
map f = mapM (return . f)

-- (Functor m) based implementation of fmap does not fuse well in
-- streaming-benchmarks. XXX need to investigate why.
instance Monad m => Functor (Stream m) where
    {-# INLINE fmap #-}
    fmap = map

    {-# INLINE (<$) #-}
    (<$) = fmap . const

------------------------------------------------------------------------------
-- Lists
------------------------------------------------------------------------------

-- XXX Show instance is 10x slower compared to read, we can do much better.
-- The list show instance itself is really slow.

-- XXX The default definitions of "<" in the Ord instance etc. do not perform
-- well, because they do not get inlined. Need to add INLINE in Ord class in
-- base?

instance IsList (Stream Identity a) where
    type (Item (Stream Identity a)) = a

    {-# INLINE fromList #-}
    fromList = Streamly.Internal.Data.Stream.StreamD.Type.fromList

    {-# INLINE toList #-}
    toList = runIdentity . Streamly.Internal.Data.Stream.StreamD.Type.toList

instance Eq a => Eq (Stream Identity a) where
    {-# INLINE (==) #-}
    (==) xs ys = runIdentity $ eqBy (==) xs ys

instance Ord a => Ord (Stream Identity a) where
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

instance Show a => Show (Stream Identity a) where
    showsPrec p dl = showParen (p > 10) $
        showString "fromList " . shows (GHC.Exts.toList dl)

instance Read a => Read (Stream Identity a) where
    readPrec = parens $ prec 10 $ do
        Ident "fromList" <- lexP
        Streamly.Internal.Data.Stream.StreamD.Type.fromList <$> readPrec

    readListPrec = readListPrecDefault

instance (a ~ Char) => IsString (Stream Identity a) where
    {-# INLINE fromString #-}
    fromString = Streamly.Internal.Data.Stream.StreamD.Type.fromList

-------------------------------------------------------------------------------
-- Foldable
-------------------------------------------------------------------------------

-- The default Foldable instance has several issues:
-- 1) several definitions do not have INLINE on them, so we provide
--    re-implementations with INLINE pragmas.
-- 2) the definitions of sum/product/maximum/minimum are inefficient as they
--    use right folds, they cannot run in constant memory. We provide
--    implementations using strict left folds here.

-- There is no Traversable instance because, there is no scalable cons for
-- StreamD, use toList and fromList instead.

instance (Foldable m, Monad m) => Foldable (Stream m) where

    {-# INLINE foldMap #-}
    foldMap f =
        Data.Foldable.fold
            . Streamly.Internal.Data.Stream.StreamD.Type.foldr (mappend . f) mempty

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
-- Filtering
-------------------------------------------------------------------------------

-- Adapted from the vector package.

-- | Take first 'n' elements from the stream and discard the rest.
--
{-# INLINE_NORMAL take #-}
take :: Applicative m => Int -> Stream m a -> Stream m a
take n (Stream step state) = n `seq` Stream step' (state, 0)

    where

    {-# INLINE_LATE step' #-}
    step' gst (st, i) | i < n = do
        (\case
            Yield x s -> Yield x (s, i + 1)
            Skip s    -> Skip (s, i)
            Stop      -> Stop) <$> step gst st
    step' _ (_, _) = pure Stop

-- Adapted from the vector package.

-- | Same as 'takeWhile' but with a monadic predicate.
--
{-# INLINE_NORMAL takeWhileM #-}
takeWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
-- takeWhileM p = scanMaybe (FL.takingEndByM_ (\x -> not <$> p x))
takeWhileM f (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- step gst st
        case r of
            Yield x s -> do
                b <- f x
                return $ if b then Yield x s else Stop
            Skip s -> return $ Skip s
            Stop   -> return Stop

-- | End the stream as soon as the predicate fails on an element.
--
{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
takeWhile f = takeWhileM (return . f)

-- Like takeWhile but with an inverted condition and also taking
-- the matching element.

{-# INLINE_NORMAL takeEndByM #-}
takeEndByM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
takeEndByM f (Stream step state) = Stream step' (Just state)
  where
    {-# INLINE_LATE step' #-}
    step' gst (Just st) = do
        r <- step gst st
        case r of
            Yield x s -> do
                b <- f x
                return $
                    if not b
                    then Yield x (Just s)
                    else Yield x Nothing
            Skip s -> return $ Skip (Just s)
            Stop   -> return Stop

    step' _ Nothing = return Stop

{-# INLINE takeEndBy #-}
takeEndBy :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
takeEndBy f = takeEndByM (return . f)

------------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------------

-- | Like 'zipWith' but using a monadic zipping function.
--
{-# INLINE_NORMAL zipWithM #-}
zipWithM :: Monad m
    => (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
zipWithM f (Stream stepa ta) (Stream stepb tb) = Stream step (ta, tb, Nothing)
  where
    {-# INLINE_LATE step #-}
    step gst (sa, sb, Nothing) = do
        r <- stepa (adaptState gst) sa
        return $
          case r of
            Yield x sa' -> Skip (sa', sb, Just x)
            Skip sa'    -> Skip (sa', sb, Nothing)
            Stop        -> Stop

    step gst (sa, sb, Just x) = do
        r <- stepb (adaptState gst) sb
        case r of
            Yield y sb' -> do
                z <- f x y
                return $ Yield z (sa, sb', Nothing)
            Skip sb' -> return $ Skip (sa, sb', Just x)
            Stop     -> return Stop

{-# RULES "zipWithM xs xs"
    forall f xs. zipWithM @Identity f xs xs = mapM (\x -> f x x) xs #-}

-- | Stream @a@ is evaluated first, followed by stream @b@, the resulting
-- elements @a@ and @b@ are then zipped using the supplied zip function and the
-- result @c@ is yielded to the consumer.
--
-- If stream @a@ or stream @b@ ends, the zipped stream ends. If stream @b@ ends
-- first, the element @a@ from previous evaluation of stream @a@ is discarded.
--
-- >>> s1 = Stream.fromList [1,2,3]
-- >>> s2 = Stream.fromList [4,5,6]
-- >>> Stream.fold Fold.toList $ Stream.zipWith (+) s1 s2
-- [5,7,9]
--
{-# INLINE zipWith #-}
zipWith :: Monad m => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWith f = zipWithM (\a b -> return (f a b))

------------------------------------------------------------------------------
-- Combine N Streams - concatAp
------------------------------------------------------------------------------

-- | Apply a stream of functions to a stream of values and flatten the results.
--
-- Note that the second stream is evaluated multiple times.
--
-- >>> crossApply = Stream.crossWith id
--
{-# INLINE_NORMAL crossApply #-}
crossApply :: Functor f => Stream f (a -> b) -> Stream f a -> Stream f b
crossApply (Stream stepa statea) (Stream stepb stateb) =
    Stream step' (Left statea)

    where

    {-# INLINE_LATE step' #-}
    step' gst (Left st) = fmap
        (\case
            Yield f s -> Skip (Right (f, s, stateb))
            Skip    s -> Skip (Left s)
            Stop      -> Stop)
        (stepa (adaptState gst) st)
    step' gst (Right (f, os, st)) = fmap
        (\case
            Yield a s -> Yield (f a) (Right (f, os, s))
            Skip s    -> Skip (Right (f,os, s))
            Stop      -> Skip (Left os))
        (stepb (adaptState gst) st)

{-# INLINE_NORMAL crossApplySnd #-}
crossApplySnd :: Functor f => Stream f a -> Stream f b -> Stream f b
crossApplySnd (Stream stepa statea) (Stream stepb stateb) =
    Stream step (Left statea)

    where

    {-# INLINE_LATE step #-}
    step gst (Left st) =
        fmap
            (\case
                 Yield _ s -> Skip (Right (s, stateb))
                 Skip s -> Skip (Left s)
                 Stop -> Stop)
            (stepa (adaptState gst) st)
    step gst (Right (ostate, st)) =
        fmap
            (\case
                 Yield b s -> Yield b (Right (ostate, s))
                 Skip s -> Skip (Right (ostate, s))
                 Stop -> Skip (Left ostate))
            (stepb gst st)

{-# INLINE_NORMAL crossApplyFst #-}
crossApplyFst :: Functor f => Stream f a -> Stream f b -> Stream f a
crossApplyFst (Stream stepa statea) (Stream stepb stateb) =
    Stream step (Left statea)

    where

    {-# INLINE_LATE step #-}
    step gst (Left st) =
        fmap
            (\case
                 Yield b s -> Skip (Right (s, stateb, b))
                 Skip s -> Skip (Left s)
                 Stop -> Stop)
            (stepa gst st)
    step gst (Right (ostate, st, b)) =
        fmap
            (\case
                 Yield _ s -> Yield b (Right (ostate, s, b))
                 Skip s -> Skip (Right (ostate, s, b))
                 Stop -> Skip (Left ostate))
            (stepb (adaptState gst) st)

{-
instance Applicative f => Applicative (Stream f) where
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
-}

-- |
-- Definition:
--
-- >>> crossWith f m1 m2 = fmap f m1 `Stream.crossApply` m2
--
-- Note that the second stream is evaluated multiple times.
--
{-# INLINE crossWith #-}
crossWith :: Monad m => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
crossWith f m1 m2 = fmap f m1 `crossApply` m2

-- | Given a @Stream m a@ and @Stream m b@ generate a stream with all possible
-- combinations of the tuple @(a, b)@.
--
-- Definition:
--
-- >>> cross = Stream.crossWith (,)
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
cross :: Monad m => Stream m a -> Stream m b -> Stream m (a, b)
cross = crossWith (,)

------------------------------------------------------------------------------
-- Combine N Streams - unfoldMany
------------------------------------------------------------------------------

{-# ANN type ConcatMapUState Fuse #-}
data ConcatMapUState o i =
      ConcatMapUOuter o
    | ConcatMapUInner o i

-- | @unfoldMany unfold stream@ uses @unfold@ to map the input stream elements
-- to streams and then flattens the generated streams into a single output
-- stream.

-- This is like 'concatMap' but uses an unfold with an explicit state to
-- generate the stream instead of a 'Stream' type generator. This allows better
-- optimization via fusion.  This can be many times more efficient than
-- 'concatMap'.

-- | Like 'concatMap' but uses an 'Unfold' for stream generation. Unlike
-- 'concatMap' this can fuse the 'Unfold' code with the inner loop and
-- therefore provide many times better performance.
--
{-# INLINE_NORMAL unfoldMany #-}
unfoldMany :: Monad m => Unfold m a b -> Stream m a -> Stream m b
unfoldMany (Unfold istep inject) (Stream ostep ost) =
    Stream step (ConcatMapUOuter ost)
  where
    {-# INLINE_LATE step #-}
    step gst (ConcatMapUOuter o) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- inject a
                i `seq` return (Skip (ConcatMapUInner o' i))
            Skip o' -> return $ Skip (ConcatMapUOuter o')
            Stop -> return Stop

    step _ (ConcatMapUInner o i) = do
        r <- istep i
        return $ case r of
            Yield x i' -> Yield x (ConcatMapUInner o i')
            Skip i'    -> Skip (ConcatMapUInner o i')
            Stop       -> Skip (ConcatMapUOuter o)

------------------------------------------------------------------------------
-- Combine N Streams - concatMap
------------------------------------------------------------------------------

-- Adapted from the vector package.

-- | Map a stream producing monadic function on each element of the stream
-- and then flatten the results into a single stream. Since the stream
-- generation function is monadic, unlike 'concatMap', it can produce an
-- effect at the beginning of each iteration of the inner loop.
--
-- See 'unfoldMany' for a fusible alternative.
--
{-# INLINE_NORMAL concatMapM #-}
concatMapM :: Monad m => (a -> m (Stream m b)) -> Stream m a -> Stream m b
concatMapM f (Stream step state) = Stream step' (Left state)
  where
    {-# INLINE_LATE step' #-}
    step' gst (Left st) = do
        r <- step (adaptState gst) st
        case r of
            Yield a s -> do
                b_stream <- f a
                return $ Skip (Right (b_stream, s))
            Skip s -> return $ Skip (Left s)
            Stop -> return Stop

    -- XXX flattenArrays is 5x faster than "concatMap fromArray". if somehow we
    -- can get inner_step to inline and fuse here we can perhaps get the same
    -- performance using "concatMap fromArray".
    --
    -- XXX using the pattern synonym "Stream" causes a major performance issue
    -- here even if the synonym does not include an adaptState call. Need to
    -- find out why. Is that something to be fixed in GHC?
    step' gst (Right (UnStream inner_step inner_st, st)) = do
        r <- inner_step (adaptState gst) inner_st
        case r of
            Yield b inner_s ->
                return $ Yield b (Right (Stream inner_step inner_s, st))
            Skip inner_s ->
                return $ Skip (Right (Stream inner_step inner_s, st))
            Stop -> return $ Skip (Left st)

-- | Map a stream producing function on each element of the stream and then
-- flatten the results into a single stream.
--
-- >>> concatMap f = Stream.concatMapM (return . f)
-- >>> concatMap f = Stream.concat . fmap f
-- >>> concatMap f = Stream.unfoldMany (Unfold.lmap f Unfold.fromStream)
--
-- See 'unfoldMany' for a fusible alternative.
--
{-# INLINE concatMap #-}
concatMap :: Monad m => (a -> Stream m b) -> Stream m a -> Stream m b
concatMap f = concatMapM (return . f)

-- | Flatten a stream of streams to a single stream.
--
-- >>> concat = Stream.concatMap id
--
-- /Pre-release/
{-# INLINE concat #-}
concat :: Monad m => Stream m (Stream m a) -> Stream m a
concat = concatMap id

-- XXX The idea behind this rule is to rewrite any calls to "concatMap
-- fromArray" automatically to flattenArrays which is much faster.  However, we
-- need an INLINE_EARLY on concatMap for this rule to fire. But if we use
-- INLINE_EARLY on concatMap or fromArray then direct uses of
-- "concatMap fromArray" (without the RULE) become much slower, this means
-- "concatMap f" in general would become slower. Need to find a solution to
-- this.
--
-- {-# RULES "concatMap Array.toStreamD"
--      concatMap Array.toStreamD = Array.flattenArray #-}

-- >>> concatEffect = Stream.concat . lift    -- requires (MonadTrans t)
-- >>> concatEffect = join . lift             -- requires (MonadTrans t, Monad (Stream m))

-- | Given a stream value in the underlying monad, lift and join the underlying
-- monad with the stream monad.
--
-- >>> concatEffect = Stream.concat . Stream.fromEffect
-- >>> concatEffect eff = Stream.concatMapM (\() -> eff) (Stream.fromPure ())
--
-- See also: 'concat', 'sequence'
--
{-# INLINE concatEffect #-}
concatEffect :: Monad m => m (Stream m a) -> Stream m a
concatEffect generator = concatMapM (\() -> generator) (fromPure ())

{-
-- NOTE: even though concatMap for StreamD is 4x faster compared to StreamK,
-- the monad instance does not seem to be significantly faster.
instance Monad m => Monad (Stream m) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = flip concatMap

    {-# INLINE (>>) #-}
    (>>) = (*>)
-}

------------------------------------------------------------------------------
-- Traversing a tree top down
------------------------------------------------------------------------------

-- Next stream is to be generated by the return value of the previous stream. A
-- general intuitive way of doing that could be to use an appending monad
-- instance for streams where the result of the previous stream is used to
-- generate the next one. In the first pass we can just emit the values in the
-- stream and keep building a buffered list/stream, once done we can then
-- process the buffered stream.

-- | Generate a stream from an initial state, scan and concat the stream,
-- generate a stream again from the final state of the previous scan and repeat
-- the process.
{-# INLINE_NORMAL concatIterateScan #-}
concatIterateScan :: Monad m =>
       (b -> a -> m b)
    -> (b -> m (Maybe (b, Stream m a)))
    -> b
    -> Stream m a
concatIterateScan scanner generate initial = Stream step (Left initial)

    where

    {-# INLINE_LATE step #-}
    step _ (Left acc) = do
        r <- generate acc
        case r of
            Nothing -> return Stop
            Just v -> return $ Skip (Right v)

    step gst (Right (st, UnStream inner_step inner_st)) = do
        r <- inner_step (adaptState gst) inner_st
        case r of
            Yield b inner_s -> do
                acc <- scanner st b
                return $ Yield b (Right (acc, Stream inner_step inner_s))
            Skip inner_s ->
                return $ Skip (Right (st, Stream inner_step inner_s))
            Stop -> return $ Skip (Left st)

-- Note: The iterate function returns a Maybe Stream instead of returning a nil
-- stream for indicating a leaf node. This is to optimize so that we do not
-- have to store any state. This makes the stored state proportional to the
-- number of non-leaf nodes rather than total number of nodes.

-- | Same as 'concatIterateBfs' except that the traversal of the last
-- element on a level is emitted first and then going backwards up to the first
-- element (reversed ordering). This may be slightly faster than
-- 'concatIterateBfs'.
--
{-# INLINE_NORMAL concatIterateBfsRev #-}
concatIterateBfsRev :: Monad m =>
       (a -> Maybe (Stream m a))
    -> Stream m a
    -> Stream m a
concatIterateBfsRev f stream = Stream step (stream, [])

    where

    {-# INLINE_LATE step #-}
    step gst (UnStream step1 st, xs) = do
        r <- step1 (adaptState gst) st
        case r of
            Yield a s -> do
                let xs1 =
                        case f a of
                            Nothing -> xs
                            Just x -> x:xs
                return $ Yield a (Stream step1 s, xs1)
            Skip s -> return $ Skip (Stream step1 s, xs)
            Stop ->
                case xs of
                    (y:ys) -> return $ Skip (y, ys)
                    [] -> return Stop

-- | Similar to 'concatIterateDfs' except that it traverses the stream in
-- breadth first style (BFS). First, all the elements in the input stream are
-- emitted, and then their traversals are emitted.
--
-- Example, list a directory tree using BFS:
--
-- >>> f = either (Just . Dir.readEitherPaths) (const Nothing)
-- >>> input = Stream.fromPure (Left ".")
-- >>> ls = Stream.concatIterateBfs f input
--
-- /Pre-release/
{-# INLINE_NORMAL concatIterateBfs #-}
concatIterateBfs :: Monad m =>
       (a -> Maybe (Stream m a))
    -> Stream m a
    -> Stream m a
concatIterateBfs f stream = Stream step (stream, [], [])

    where

    {-# INLINE_LATE step #-}
    step gst (UnStream step1 st, xs, ys) = do
        r <- step1 (adaptState gst) st
        case r of
            Yield a s -> do
                let ys1 =
                        case f a of
                            Nothing -> ys
                            Just y -> y:ys
                return $ Yield a (Stream step1 s, xs, ys1)
            Skip s -> return $ Skip (Stream step1 s, xs, ys)
            Stop ->
                case xs of
                    (x:xs1) -> return $ Skip (x, xs1, ys)
                    [] ->
                        case reverse ys of
                            (x:xs1) -> return $ Skip (x, xs1, [])
                            [] -> return Stop

-- | Traverse the stream in depth first style (DFS). Map each element in the
-- input stream to a stream and flatten, recursively map the resulting elements
-- as well to a stream and flatten until no more streams are generated.
--
-- Example, list a directory tree using DFS:
--
-- >>> f = either (Just . Dir.readEitherPaths) (const Nothing)
-- >>> input = Stream.fromPure (Left ".")
-- >>> ls = Stream.concatIterateDfs f input
--
-- This is equivalent to using @concatIterateWith StreamK.append@.
--
-- /Pre-release/
{-# INLINE_NORMAL concatIterateDfs #-}
concatIterateDfs :: Monad m =>
       (a -> Maybe (Stream m a))
    -> Stream m a
    -> Stream m a
concatIterateDfs f stream = Stream step (stream, [])

    where

    {-# INLINE_LATE step #-}
    step gst (UnStream step1 st, xs) = do
        r <- step1 (adaptState gst) st
        case r of
            Yield a s -> do
                let st1 =
                        case f a of
                            Nothing -> (Stream step1 s, xs)
                            Just x -> (x, Stream step1 s:xs)
                return $ Yield a st1
            Skip s -> return $ Skip (Stream step1 s, xs)
            Stop ->
                case xs of
                    (y:ys) -> return $ Skip (y, ys)
                    [] -> return Stop

{-# ANN type IterateUnfoldState Fuse #-}
data IterateUnfoldState o i =
      IterateUnfoldOuter o
    | IterateUnfoldInner o i [i]

-- | Same as @concatIterateDfs@ but more efficient due to stream fusion.
--
-- Example, list a directory tree using DFS:
--
-- >>> f = Unfold.either Dir.eitherReaderPaths Unfold.nil
-- >>> input = Stream.fromPure (Left ".")
-- >>> ls = Stream.unfoldIterateDfs f input
--
-- /Pre-release/
{-# INLINE_NORMAL unfoldIterateDfs #-}
unfoldIterateDfs :: Monad m =>
       Unfold m a a
    -> Stream m a
    -> Stream m a
unfoldIterateDfs (Unfold istep inject) (Stream ostep ost) =
    Stream step (IterateUnfoldOuter ost)

    where

    {-# INLINE_LATE step #-}
    step gst (IterateUnfoldOuter o) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a s -> do
                i <- inject a
                i `seq` return (Yield a (IterateUnfoldInner s i []))
            Skip s -> return $ Skip (IterateUnfoldOuter s)
            Stop -> return Stop

    step _ (IterateUnfoldInner o i ii) = do
        r <- istep i
        case r of
            Yield x s -> do
                i1 <- inject x
                i1 `seq` return $ Yield x (IterateUnfoldInner o i1 (s:ii))
            Skip s -> return $ Skip (IterateUnfoldInner o s ii)
            Stop ->
                case ii of
                    (y:ys) -> return $ Skip (IterateUnfoldInner o y ys)
                    [] -> return $ Skip (IterateUnfoldOuter o)

{-# ANN type IterateUnfoldBFSRevState Fuse #-}
data IterateUnfoldBFSRevState o i =
      IterateUnfoldBFSRevOuter o [i]
    | IterateUnfoldBFSRevInner i [i]

-- | Like 'unfoldIterateBfs' but processes the children in reverse order,
-- therefore, may be slightly faster.
--
-- /Pre-release/
{-# INLINE_NORMAL unfoldIterateBfsRev #-}
unfoldIterateBfsRev :: Monad m =>
       Unfold m a a
    -> Stream m a
    -> Stream m a
unfoldIterateBfsRev (Unfold istep inject) (Stream ostep ost) =
    Stream step (IterateUnfoldBFSRevOuter ost [])

    where

    {-# INLINE_LATE step #-}
    step gst (IterateUnfoldBFSRevOuter o ii) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a s -> do
                i <- inject a
                i `seq` return (Yield a (IterateUnfoldBFSRevOuter s (i:ii)))
            Skip s -> return $ Skip (IterateUnfoldBFSRevOuter s ii)
            Stop ->
                case ii of
                    (y:ys) -> return $ Skip (IterateUnfoldBFSRevInner y ys)
                    [] -> return Stop

    step _ (IterateUnfoldBFSRevInner i ii) = do
        r <- istep i
        case r of
            Yield x s -> do
                i1 <- inject x
                i1 `seq` return $ Yield x (IterateUnfoldBFSRevInner s (i1:ii))
            Skip s -> return $ Skip (IterateUnfoldBFSRevInner s ii)
            Stop ->
                case ii of
                    (y:ys) -> return $ Skip (IterateUnfoldBFSRevInner y ys)
                    [] -> return Stop

{-# ANN type IterateUnfoldBFSState Fuse #-}
data IterateUnfoldBFSState o i =
      IterateUnfoldBFSOuter o [i]
    | IterateUnfoldBFSInner i [i] [i]

-- | Like 'unfoldIterateDfs' but uses breadth first style traversal.
--
-- /Pre-release/
{-# INLINE_NORMAL unfoldIterateBfs #-}
unfoldIterateBfs :: Monad m =>
       Unfold m a a
    -> Stream m a
    -> Stream m a
unfoldIterateBfs (Unfold istep inject) (Stream ostep ost) =
    Stream step (IterateUnfoldBFSOuter ost [])

    where

    {-# INLINE_LATE step #-}
    step gst (IterateUnfoldBFSOuter o rii) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a s -> do
                i <- inject a
                i `seq` return (Yield a (IterateUnfoldBFSOuter s (i:rii)))
            Skip s -> return $ Skip (IterateUnfoldBFSOuter s rii)
            Stop ->
                case reverse rii of
                    (y:ys) -> return $ Skip (IterateUnfoldBFSInner y ys [])
                    [] -> return Stop

    step _ (IterateUnfoldBFSInner i ii rii) = do
        r <- istep i
        case r of
            Yield x s -> do
                i1 <- inject x
                i1 `seq` return $ Yield x (IterateUnfoldBFSInner s ii (i1:rii))
            Skip s -> return $ Skip (IterateUnfoldBFSInner s ii rii)
            Stop ->
                case ii of
                    (y:ys) -> return $ Skip (IterateUnfoldBFSInner y ys rii)
                    [] ->
                        case reverse rii of
                            (y:ys) -> return $ Skip (IterateUnfoldBFSInner y ys [])
                            [] -> return Stop

------------------------------------------------------------------------------
-- Folding a tree bottom up
------------------------------------------------------------------------------

-- | Binary BFS style reduce, folds a level entirely using the supplied fold
-- function, collecting the outputs as next level of the tree, then repeats the
-- same process on the next level. The last elements of a previously folded
-- level are folded first.
{-# INLINE_NORMAL reduceIterateBfs #-}
reduceIterateBfs :: Monad m =>
    (a -> a -> m a) -> Stream m a -> m (Maybe a)
reduceIterateBfs f (Stream step state) = go SPEC state [] Nothing

    where

    go _ st xs Nothing = do
        r <- step defState st
        case r of
            Yield x1 s -> go SPEC s xs (Just x1)
            Skip s -> go SPEC s xs Nothing
            Stop ->
                case xs of
                    [] -> return Nothing
                    _ -> goBuf SPEC xs []
    go _ st xs (Just x1) = do
        r2 <- step defState st
        case r2 of
            Yield x2 s -> do
                x <- f x1 x2
                go SPEC s (x:xs) Nothing
            Skip s -> go SPEC s xs (Just x1)
            Stop ->
                case xs of
                    [] -> return (Just x1)
                    _ -> goBuf SPEC (x1:xs) []

    goBuf _ [] ys = goBuf SPEC ys []
    goBuf _ [x1] ys = do
        case ys of
            [] -> return (Just x1)
            (x2:xs) -> do
                y <- f x1 x2
                goBuf SPEC xs [y]
    goBuf _ (x1:x2:xs) ys = do
        y <- f x1 x2
        goBuf SPEC xs (y:ys)

-- | N-Ary BFS style iterative fold, if the input stream finished before the
-- fold then it returns Left otherwise Right. If the fold returns Left we
-- terminate.
--
-- /Unimplemented/
foldIterateBfs ::
    Fold m a (Either a a) -> Stream m a -> m (Maybe a)
foldIterateBfs = undefined

------------------------------------------------------------------------------
-- Grouping/Splitting
------------------------------------------------------------------------------

-- s = stream state, fs = fold state
{-# ANN type FoldManyPost Fuse #-}
data FoldManyPost s fs b a
    = FoldManyPostStart s
    | FoldManyPostLoop s fs
    | FoldManyPostYield b (FoldManyPost s fs b a)
    | FoldManyPostDone

-- XXX Need a more intuitive name, and need to reconcile the names
-- foldMany/fold/parse/parseMany/parseManyPost etc.

-- XXX foldManyPost keeps the last fold always partial. if the last fold is
-- complete then another fold is applied on empty input. This is used for
-- applying folds like takeEndBy such that the last element is not the
-- separator (infix style). But that looks like a hack. We should remove this
-- and use a custom combinator for infix parsing.

-- | Like 'foldMany' but evaluates the fold even if the fold did not receive
-- any input, therefore, always results in a non-empty output even on an empty
-- stream (default result of the fold).
--
-- Example, empty stream:
--
-- >>> f = Fold.take 2 Fold.sum
-- >>> fmany = Stream.fold Fold.toList . Stream.foldManyPost f
-- >>> fmany $ Stream.fromList []
-- [0]
--
-- Example, last fold empty:
--
-- >>> fmany $ Stream.fromList [1..4]
-- [3,7,0]
--
-- Example, last fold non-empty:
--
-- >>> fmany $ Stream.fromList [1..5]
-- [3,7,5]
--
-- Note that using a closed fold e.g. @Fold.take 0@, would result in an
-- infinite stream without consuming the input.
--
-- /Pre-release/
--
{-# INLINE_NORMAL foldManyPost #-}
foldManyPost :: Monad m => Fold m a b -> Stream m a -> Stream m b
foldManyPost (Fold fstep initial extract) (Stream step state) =
    Stream step' (FoldManyPostStart state)

    where

    {-# INLINE consume #-}
    consume x s fs = do
        res <- fstep fs x
        return
            $ Skip
            $ case res of
                  FL.Done b -> FoldManyPostYield b (FoldManyPostStart s)
                  FL.Partial ps -> FoldManyPostLoop s ps

    {-# INLINE_LATE step' #-}
    step' _ (FoldManyPostStart st) = do
        r <- initial
        return
            $ Skip
            $ case r of
                  FL.Done b -> FoldManyPostYield b (FoldManyPostStart st)
                  FL.Partial fs -> FoldManyPostLoop st fs
    step' gst (FoldManyPostLoop st fs) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> consume x s fs
            Skip s -> return $ Skip (FoldManyPostLoop s fs)
            Stop -> do
                b <- extract fs
                return $ Skip (FoldManyPostYield b FoldManyPostDone)
    step' _ (FoldManyPostYield b next) = return $ Yield b next
    step' _ FoldManyPostDone = return Stop

{-# ANN type FoldMany Fuse #-}
data FoldMany s fs b a
    = FoldManyStart s
    | FoldManyFirst fs s
    | FoldManyLoop s fs
    | FoldManyYield b (FoldMany s fs b a)
    | FoldManyDone

-- XXX Nested foldMany does not fuse.

-- | Apply a 'Fold' repeatedly on a stream and emit the results in the output
-- stream.
--
-- Definition:
--
-- >>> foldMany f = Stream.parseMany (Parser.fromFold f)
--
-- Example, empty stream:
--
-- >>> f = Fold.take 2 Fold.sum
-- >>> fmany = Stream.fold Fold.toList . Stream.foldMany f
-- >>> fmany $ Stream.fromList []
-- []
--
-- Example, last fold empty:
--
-- >>> fmany $ Stream.fromList [1..4]
-- [3,7]
--
-- Example, last fold non-empty:
--
-- >>> fmany $ Stream.fromList [1..5]
-- [3,7,5]
--
-- Note that using a closed fold e.g. @Fold.take 0@, would result in an
-- infinite stream on a non-empty input stream.
--
{-# INLINE_NORMAL foldMany #-}
foldMany :: Monad m => Fold m a b -> Stream m a -> Stream m b
foldMany (Fold fstep initial extract) (Stream step state) =
    Stream step' (FoldManyStart state)

    where

    {-# INLINE consume #-}
    consume x s fs = do
        res <- fstep fs x
        return
            $ Skip
            $ case res of
                  FL.Done b -> FoldManyYield b (FoldManyStart s)
                  FL.Partial ps -> FoldManyLoop s ps

    {-# INLINE_LATE step' #-}
    step' _ (FoldManyStart st) = do
        r <- initial
        return
            $ Skip
            $ case r of
                  FL.Done b -> FoldManyYield b (FoldManyStart st)
                  FL.Partial fs -> FoldManyFirst fs st
    step' gst (FoldManyFirst fs st) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> consume x s fs
            Skip s -> return $ Skip (FoldManyFirst fs s)
            Stop -> return Stop
    step' gst (FoldManyLoop st fs) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> consume x s fs
            Skip s -> return $ Skip (FoldManyLoop s fs)
            Stop -> do
                b <- extract fs
                return $ Skip (FoldManyYield b FoldManyDone)
    step' _ (FoldManyYield b next) = return $ Yield b next
    step' _ FoldManyDone = return Stop

-- | Group the input stream into groups of @n@ elements each and then fold each
-- group using the provided fold function.
--
-- @groupsOf n f = foldMany (FL.take n f)@
--
-- >>> Stream.toList $ Stream.groupsOf 2 Fold.sum (Stream.enumerateFromTo 1 10)
-- [3,7,11,15,19]
--
-- This can be considered as an n-fold version of 'take' where we apply
-- 'take' repeatedly on the leftover stream until the stream exhausts.
--
{-# INLINE groupsOf #-}
groupsOf :: Monad m => Int -> Fold m a b -> Stream m a -> Stream m b
groupsOf n f = foldMany (FL.take n f)

-- Keep the argument order consistent with refoldIterateM.

-- | Like 'foldMany' but for the 'Refold' type.  The supplied action is used as
-- the initial value for each refold.
--
-- /Internal/
{-# INLINE_NORMAL refoldMany #-}
refoldMany :: Monad m => Refold m x a b -> m x -> Stream m a -> Stream m b
refoldMany (Refold fstep inject extract) action (Stream step state) =
    Stream step' (FoldManyStart state)

    where

    {-# INLINE consume #-}
    consume x s fs = do
        res <- fstep fs x
        return
            $ Skip
            $ case res of
                  FL.Done b -> FoldManyYield b (FoldManyStart s)
                  FL.Partial ps -> FoldManyLoop s ps

    {-# INLINE_LATE step' #-}
    step' _ (FoldManyStart st) = do
        r <- action >>= inject
        return
            $ Skip
            $ case r of
                  FL.Done b -> FoldManyYield b (FoldManyStart st)
                  FL.Partial fs -> FoldManyFirst fs st
    step' gst (FoldManyFirst fs st) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> consume x s fs
            Skip s -> return $ Skip (FoldManyFirst fs s)
            Stop -> return Stop
    step' gst (FoldManyLoop st fs) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> consume x s fs
            Skip s -> return $ Skip (FoldManyLoop s fs)
            Stop -> do
                b <- extract fs
                return $ Skip (FoldManyYield b FoldManyDone)
    step' _ (FoldManyYield b next) = return $ Yield b next
    step' _ FoldManyDone = return Stop

------------------------------------------------------------------------------
-- Stream with a cross product style monad instance
------------------------------------------------------------------------------

-- XXX CrossStream performs better than the CrossStreamK when nesting two
-- loops, however, CrossStreamK seems to be better for more than two nestings,
-- need to do more perf investigation.

-- | A newtype wrapper for the 'Stream' type with a cross product style monad
-- instance.
--
-- A 'Monad' bind behaves like a @for@ loop:
--
-- >>> :{
-- Stream.fold Fold.toList $ Stream.unCross $ do
--     x <- Stream.mkCross $ Stream.fromList [1,2]
--     -- Perform the following actions for each x in the stream
--     return x
-- :}
-- [1,2]
--
-- Nested monad binds behave like nested @for@ loops:
--
-- >>> :{
-- Stream.fold Fold.toList $ Stream.unCross $ do
--     x <- Stream.mkCross $ Stream.fromList [1,2]
--     y <- Stream.mkCross $ Stream.fromList [3,4]
--     -- Perform the following actions for each x, for each y
--     return (x, y)
-- :}
-- [(1,3),(1,4),(2,3),(2,4)]
--
newtype CrossStream m a = CrossStream {unCrossStream :: Stream m a}
        deriving (Functor, Foldable)

{-# INLINE mkCross #-}
mkCross :: Stream m a -> CrossStream m a
mkCross = CrossStream

{-# INLINE unCross #-}
unCross :: CrossStream m a -> Stream m a
unCross = unCrossStream

-- Pure (Identity monad) stream instances
deriving instance IsList (CrossStream Identity a)
deriving instance (a ~ Char) => IsString (CrossStream Identity a)
deriving instance Eq a => Eq (CrossStream Identity a)
deriving instance Ord a => Ord (CrossStream Identity a)

-- Do not use automatic derivation for this to show as "fromList" rather than
-- "fromList Identity".
instance Show a => Show (CrossStream Identity a) where
    {-# INLINE show #-}
    show (CrossStream xs) = show xs

instance Read a => Read (CrossStream Identity a) where
    {-# INLINE readPrec #-}
    readPrec = fmap CrossStream readPrec

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

-- Note: we need to define all the typeclass operations because we want to
-- INLINE them.
instance Monad m => Applicative (CrossStream m) where
    {-# INLINE pure #-}
    pure x = CrossStream (fromPure x)

    {-# INLINE (<*>) #-}
    (CrossStream s1) <*> (CrossStream s2) =
        CrossStream (crossApply s1 s2)

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

    {-# INLINE (*>) #-}
    (CrossStream s1) *> (CrossStream s2) =
        CrossStream (crossApplySnd s1 s2)

    {-# INLINE (<*) #-}
    (CrossStream s1) <* (CrossStream s2) =
        CrossStream (crossApplyFst s1 s2)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (CrossStream m) where
    return = pure

    -- Benchmarks better with StreamD bind and pure:
    -- toList, filterAllout, *>, *<, >> (~2x)
    --

    -- Benchmarks better with CPS bind and pure:
    -- Prime sieve (25x)
    -- n binds, breakAfterSome, filterAllIn, state transformer (~2x)
    --
    {-# INLINE (>>=) #-}
    (>>=) (CrossStream m) f = CrossStream (concatMap (unCrossStream . f) m)

    {-# INLINE (>>) #-}
    (>>) = (*>)

------------------------------------------------------------------------------
-- Transformers
------------------------------------------------------------------------------

instance (MonadIO m) => MonadIO (CrossStream m) where
    liftIO x = CrossStream (fromEffect $ liftIO x)

instance MonadTrans CrossStream where
    {-# INLINE lift #-}
    lift x = CrossStream (fromEffect x)

instance (MonadThrow m) => MonadThrow (CrossStream m) where
    throwM = lift . throwM
