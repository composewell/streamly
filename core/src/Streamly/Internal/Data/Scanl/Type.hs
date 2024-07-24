{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Scanl.Type
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Scanl vs Pipe:
--
-- A scanl is a simpler version of pipes. A scan always produces an output and
-- may or may not consume an input. It can consume at most one input on one
-- output. Whereas a pipe may consume input even without producing anything or
-- it can consume multiple inputs on a single output. Scans are simpler
-- abstractions to think about and easier for the compiler to optimize.
--
-- Returning a stream on "extract":
--
-- Make the extract function return a Step and call extract until Done or
-- alternatively if a fold wants to return multiple values during finalization
-- then we can make the fold output itself a list or stream (on each step).
--
-- Maybe the extract function draining the buffer should be represented by a
-- pipe rather than a scan? It makes the scan behave like a pipe in the
-- finalization case.
--
-- Scan type:
--
-- We can represent the scan as:
--
-- step ::
--  Partial s
--  Done b
-- extract :: s -> b
-- final :: s -> b
--
-- This type allows the accumulator to be returned even if there is no input,
-- using final. This can implement "scanl" as well as "scanl1".
--
-- We can call extract any time, means that it can always produce a valid
-- value. If the input is not last the driver can call "extract", if it is last
-- then it can call "final".
--
-- This does not allow "id" to be implemented for Category instance. Because it
-- requires an output even if there is no input.
--
-- How about the following type?
--
-- step ::
--  Partial s b
--  Done b
-- final :: ()
--
-- This cannot produce output without an input. It can implement scanl1 but not
-- scanl. This can allow category instance, because "id" can be implemented.
-- But this cannot compose with Foldl type, as "final" does not return a value,
-- so the fold cannot return a value.
--
-- How about the following type?
--
-- step ::
--  Partial s b
--  Done b
-- final :: s -> b
--
-- In this case we may not be able to avoid duplicate output. If the fold has
-- already consumed an input, Partial would have returned an output on the last
-- input, then we decide to stop the fold and use "final" on it, which will
-- again produce possibly the same output.
--
module Streamly.Internal.Data.Scanl.Type
    (
      module Streamly.Internal.Data.Fold.Step

    -- * Fold Type
    , Scanl (..)

    -- * Constructors
    , scanl'
    , scanlM'
    , scanl1'
    , scanl1M'
    , scant'
    , scantM'
    , scanr'
    , scanrM'

    -- * Folds
    , fromPure
    , fromEffect
    , fromRefold
    -- , fromScan
    , drain
    , toList
    , toStreamK
    , toStreamKRev
    , genericLength
    , length

    -- * Combinators

    -- ** Mapping output
    , rmapM

    -- ** Mapping Input
    , lmap
    , lmapM
    , postscan

    -- ** Filtering
    , catMaybes
    , scanMaybe
    , filter
    , filtering
    , filterM
    , catLefts
    , catRights
    , catEithers

    -- ** Trimming
    , take
    , taking
    , takeEndBy_
    , takeEndBy
    , dropping

    -- ** Sequential application
    -- , splitWith -- rename to "append"
    -- , split_

    -- ** Repeated Application (Splitting)
    , ManyState
    , many
    , manyPost
    , groupsOf
    , refoldMany
    , refoldMany1

    -- ** Nested Application
    -- , concatMap
    -- , duplicate
    , refold

    -- ** Parallel Distribution
    , teeWith
    , teeWithFst
    , teeWithMin

    -- ** Parallel Alternative
    , shortest
    , longest

    -- * Running A Fold
    , extractM
    , reduce
    , snoc
    , addOne
    , snocM
    , snocl
    , snoclM
    , close
    , isClosed

    -- * Transforming inner monad
    , morphInner
    , generalizeInner
    )
where

#include "inline.hs"

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Monad ((>=>), void)
import Data.Bifunctor (Bifunctor(..))
import Data.Either (fromLeft, fromRight, isLeft, isRight)
import Data.Functor.Identity (Identity(..))
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.Refold.Type (Refold(..))
-- import Streamly.Internal.Data.Scan (Scan(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

--import qualified Streamly.Internal.Data.Stream.Step as Stream
import qualified Streamly.Internal.Data.StreamK.Type as K

import Prelude hiding (Foldable(..), concatMap, filter, map, take)

-- Entire module is exported, do not import selectively
import Streamly.Internal.Data.Fold.Step

#include "DocTestDataFold.hs"

------------------------------------------------------------------------------
-- The Fold type
------------------------------------------------------------------------------

-- An fold is akin to a writer. It is the streaming equivalent of a writer.
-- The type @b@ is the accumulator of the writer. That's the reason the
-- default folds in various modules are called "write".

-- An alternative to using an "extract" function is to use "Partial s b" style
-- partial value so that we always emit the output value and there is no need
-- to extract. Then extract can be used for cleanup purposes. But in this case
-- in some cases we may need a "Continue" constructor where an output value is
-- not available, this was implicit earlier. Also, "b" should be lazy here so
-- that we do not always compute it even if we do not need it.
--
-- Partial s b  --> extract :: s -> b
-- Continue     --> extract :: s -> Maybe b
--
-- But keeping 'b' lazy does not let the fold optimize well. It leads to
-- significant regressions in the key-value folds.
--
-- The "final" function complicates combinators that take other folds as
-- argument because we need to call their finalizers at right places. An
-- alternative to reduce this complexity where it is not required is to use a
-- separate type for bracketed folds but then we need to manage the complexity
-- of two different fold types.

-- The "final" function could be (s -> m (Step s b)), like in parsers so that
-- it can be called in a loop to drain the fold.

-- | The type @Fold m a b@ represents a consumer of an input stream of values
-- of type @a@ and returning a final value of type @b@ in 'Monad' @m@. The
-- constructor of a fold is @Fold step initial extract final@.
--
-- The fold uses an internal state of type @s@. The initial value of the state
-- @s@ is created by @initial@. This function is called once and only once
-- before the fold starts consuming input. Any resource allocation can be done
-- in this function.
--
-- The @step@ function is called on each input, it consumes an input and
-- returns the next intermediate state (see 'Step') or the final result @b@ if
-- the fold terminates.
--
-- If the fold is used as a scan, the @extract@ function is used by the scan
-- driver to map the current state @s@ of the fold to the fold result. Thus
-- @extract@ can be called multiple times. In some folds, where scanning does
-- not make sense, this function is left unimplemented; such folds cannot be
-- used as scans.
--
-- Before a fold terminates, @final@ is called once and only once (unless the
-- fold terminated in @initial@ itself). Any resources allocated by @initial@
-- can be released in @final@. In folds that do not require any cleanup
-- @extract@ and @final@ are typically the same.
--
-- When implementing fold combinators, care should be taken to cleanup any
-- state of the argument folds held by the fold by calling the respective
-- @final@ at all exit points of the fold. Also, @final@ should not be called
-- more than once. Note that if a fold terminates by 'Done' constructor, there
-- is no state to cleanup.
--
-- NOTE: The constructor is not yet released, smart constructors are provided
-- to create folds.
--
data Scanl m a b =
  -- XXX Since we have scans now, we can remove the extract function.
  -- XXX initial can be made pure, like in streams, we can add effects by using
  -- bracket like operations.
  -- | @Fold@ @step@ @initial@ @extract@ @final@
  forall s. Scanl (s -> a -> m (Step s b)) (m (Step s b)) (s -> m b) (s -> m b)

------------------------------------------------------------------------------
-- Mapping on the output
------------------------------------------------------------------------------

-- | Map a monadic function on the output of a fold.
--
{-# INLINE rmapM #-}
rmapM :: Monad m => (b -> m c) -> Scanl m a b -> Scanl m a c
rmapM f (Scanl step initial extract final) =
    Scanl step1 initial1 (extract >=> f) (final >=> f)

    where

    initial1 = initial >>= mapMStep f
    step1 s a = step s a >>= mapMStep f

------------------------------------------------------------------------------
-- Left fold constructors
------------------------------------------------------------------------------

-- | Make a fold from a left fold style pure step function and initial value of
-- the accumulator.
--
-- If your 'Fold' returns only 'Partial' (i.e. never returns a 'Done') then you
-- can use @foldl'*@ constructors.
--
-- A fold with an extract function can be expressed using fmap:
--
-- @
-- mkfoldlx :: Monad m => (s -> a -> s) -> s -> (s -> b) -> Scanl m a b
-- mkfoldlx step initial extract = fmap extract (foldl' step initial)
-- @
--
{-# INLINE scanl' #-}
scanl' :: Monad m => (b -> a -> b) -> b -> Scanl m a b
scanl' step initial =
    Scanl
        (\s a -> return $ Partial $ step s a)
        (return (Partial initial))
        return
        return

-- | Make a fold from a left fold style monadic step function and initial value
-- of the accumulator.
--
-- A fold with an extract function can be expressed using rmapM:
--
-- @
-- mkFoldlxM :: Functor m => (s -> a -> m s) -> m s -> (s -> m b) -> Scanl m a b
-- mkFoldlxM step initial extract = rmapM extract (foldlM' step initial)
-- @
--
{-# INLINE scanlM' #-}
scanlM' :: Monad m => (b -> a -> m b) -> m b -> Scanl m a b
scanlM' step initial =
    Scanl (\s a -> Partial <$> step s a) (Partial <$> initial) return return

-- | Maps a function on the output of the fold (the type @b@).
instance Functor m => Functor (Scanl m a) where
    {-# INLINE fmap #-}
    fmap f (Scanl step1 initial1 extract final) =
        Scanl step initial (fmap2 f extract) (fmap2 f final)

        where

        initial = fmap2 f initial1
        step s b = fmap2 f (step1 s b)
        fmap2 g = fmap (fmap g)

-- | Make a strict left fold, for non-empty streams, using first element as the
-- starting value. Returns Nothing if the stream is empty.
--
-- /Pre-release/
{-# INLINE scanl1' #-}
scanl1' :: Monad m => (a -> a -> a) -> Scanl m a (Maybe a)
scanl1' step = fmap toMaybe $ scanl' step1 Nothing'

    where

    step1 Nothing' a = Just' a
    step1 (Just' x) a = Just' $ step x a

-- | Like 'foldl1\'' but with a monadic step function.
--
-- /Pre-release/
{-# INLINE scanl1M' #-}
scanl1M' :: Monad m => (a -> a -> m a) -> Scanl m a (Maybe a)
scanl1M'  step = fmap toMaybe $ scanlM' step1 (return Nothing')

    where

    step1 Nothing' a = return $ Just' a
    step1 (Just' x) a = Just' <$> step x a

{-
data FromScan s b = FromScanInit !s | FromScanGo !s !b

-- XXX we can attach a scan on the last fold e.g. "runScan s last". Or run a
-- scan on a fold that supplies a default value?
--
-- If we are pushing a value to a scan and the scan stops we will lose the
-- input. Only those scans that do not use the Stop constructor can be used as
-- folds or with folds? The Stop constructor makes them suitable to be composed
-- with pull based streams, push based folds cannot work with that. Do we need
-- two types of scans then, scans for streams and scans for folds? ScanR and
-- ScanL?

-- | This does not work correctly yet. We lose the last input.
--
{-# INLINE fromScan #-}
fromScan :: Monad m => Scan m a b -> Scanl m a (Maybe b)
fromScan (Scan consume initial) =
    Fold fstep (return $ Partial (FromScanInit initial)) fextract fextract

    where

    fstep (FromScanInit ss) a = do
        r <- consume ss a
        return $ case r of
            Stream.Yield b s -> Partial (FromScanGo s b)
            Stream.Skip s -> Partial (FromScanInit s)
            -- XXX We have lost the input here.
            -- XXX Need to change folds to always return Done on the next input
            Stream.Stop -> Done Nothing
    fstep (FromScanGo ss acc) a = do
        r <- consume ss a
        return $ case r of
            Stream.Yield b s -> Partial (FromScanGo s b)
            Stream.Skip s -> Partial (FromScanGo s acc)
            -- XXX We have lost the input here.
            Stream.Stop -> Done (Just acc)

    fextract (FromScanInit _) = return Nothing
    fextract (FromScanGo _ acc) = return (Just acc)
-}

------------------------------------------------------------------------------
-- Right fold constructors
------------------------------------------------------------------------------

-- | Make a fold using a right fold style step function and a terminal value.
-- It performs a strict right fold via a left fold using function composition.
-- Note that a strict right fold can only be useful for constructing strict
-- structures in memory. For reductions this will be very inefficient.
--
-- Definitions:
--
-- >>> foldr' f z = fmap (flip appEndo z) $ Fold.foldMap (Endo . f)
-- >>> foldr' f z = fmap ($ z) $ Fold.foldl' (\g x -> g . f x) id
--
-- Example:
--
-- >>> Stream.fold (Fold.foldr' (:) []) $ Stream.enumerateFromTo 1 5
-- [1,2,3,4,5]
--
{-# INLINE scanr' #-}
scanr' :: Monad m => (a -> b -> b) -> b -> Scanl m a b
scanr' f z = fmap ($ z) $ scanl' (\g x -> g . f x) id

-- XXX we have not seen any use of this yet, not releasing until we have a use
-- case.

-- | Like foldr' but with a monadic step function.
--
-- Example:
--
-- >>> toList = Fold.foldrM' (\a xs -> return $ a : xs) (return [])
--
-- See also: 'Streamly.Internal.Data.Stream.foldrM'
--
-- /Pre-release/
{-# INLINE scanrM' #-}
scanrM' :: Monad m => (a -> b -> m b) -> m b -> Scanl m a b
scanrM' g z =
    rmapM (z >>=) $ scanlM' (\f x -> return $ g x >=> f) (return return)

------------------------------------------------------------------------------
-- General fold constructors
------------------------------------------------------------------------------

-- XXX If the Step yield gives the result each time along with the state then
-- we can make the type of this as
--
-- mkFold :: Monad m => (s -> a -> Step s b) -> Step s b -> Scanl m a b
--
-- Then similar to foldl' and foldr we can just fmap extract on it to extend
-- it to the version where an 'extract' function is required. Or do we even
-- need that?
--
-- Until we investigate this we are not releasing these.
--
-- XXX The above text would apply to
-- Streamly.Internal.Data.Parser.ParserD.Type.parser

-- | Make a terminating fold using a pure step function, a pure initial state
-- and a pure state extraction function.
--
-- /Pre-release/
--
{-# INLINE scant' #-}
scant' :: Monad m => (s -> a -> Step s b) -> Step s b -> (s -> b) -> Scanl m a b
scant' step initial extract =
    Scanl
        (\s a -> return $ step s a)
        (return initial)
        (return . extract)
        (return . extract)

-- | Make a terminating fold with an effectful step function and initial state,
-- and a state extraction function.
--
-- >>> foldtM' = Fold.Fold
--
--  We can just use 'Fold' but it is provided for completeness.
--
-- /Pre-release/
--
{-# INLINE scantM' #-}
scantM' :: (s -> a -> m (Step s b)) -> m (Step s b) -> (s -> m b) -> Scanl m a b
scantM' step initial extract = Scanl step initial extract extract

------------------------------------------------------------------------------
-- Refold
------------------------------------------------------------------------------

-- This is similar to how we run an Unfold to generate a Stream. A Fold is like
-- a Stream and a Fold2 is like an Unfold.
--
-- | Make a fold from a consumer.
--
-- /Internal/
fromRefold :: Refold m c a b -> c -> Scanl m a b
fromRefold (Refold step inject extract) c =
    Scanl step (inject c) extract extract

------------------------------------------------------------------------------
-- Basic Folds
------------------------------------------------------------------------------

-- | A fold that drains all its input, running the effects and discarding the
-- results.
--
-- >>> drain = Fold.drainMapM (const (return ()))
-- >>> drain = Fold.foldl' (\_ _ -> ()) ()
--
{-# INLINE drain #-}
drain :: Monad m => Scanl m a ()
drain = scanl' (\_ _ -> ()) ()

-- | Folds the input stream to a list.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Data.Array"
-- instead.
--
-- >>> toList = Fold.foldr' (:) []
--
{-# INLINE toList #-}
toList :: Monad m => Scanl m a [a]
toList = scanr' (:) []

-- | Buffers the input stream to a pure stream in the reverse order of the
-- input.
--
-- >>> toStreamKRev = Foldable.foldl' (flip StreamK.cons) StreamK.nil
--
-- This is more efficient than 'toStreamK'. toStreamK has exactly the same
-- performance as reversing the stream after toStreamKRev.
--
-- /Pre-release/

--  xn : ... : x2 : x1 : []
{-# INLINE toStreamKRev #-}
toStreamKRev :: Monad m => Scanl m a (K.StreamK n a)
toStreamKRev = scanl' (flip K.cons) K.nil

-- | A fold that buffers its input to a pure stream.
--
-- >>> toStreamK = foldr StreamK.cons StreamK.nil
-- >>> toStreamK = fmap StreamK.reverse Fold.toStreamKRev
--
-- /Internal/
{-# INLINE toStreamK #-}
toStreamK :: Monad m => Scanl m a (K.StreamK n a)
toStreamK = scanr' K.cons K.nil

-- | Like 'length', except with a more general 'Num' return value
--
-- Definition:
--
-- >>> genericLength = fmap getSum $ Fold.foldMap (Sum . const  1)
-- >>> genericLength = Fold.foldl' (\n _ -> n + 1) 0
--
-- /Pre-release/
{-# INLINE genericLength #-}
genericLength :: (Monad m, Num b) => Scanl m a b
genericLength = scanl' (\n _ -> n + 1) 0

-- | Determine the length of the input stream.
--
-- Definition:
--
-- >>> length = Fold.genericLength
-- >>> length = fmap getSum $ Fold.foldMap (Sum . const  1)
--
{-# INLINE length #-}
length :: Monad m => Scanl m a Int
length = genericLength

------------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------------

-- XXX These are singleton folds that are closed for input. The correspondence
-- to a nil stream would be a nil fold that returns "Done" in "initial" i.e. it
-- does not produce any accumulator value. However, we do not have a
-- representation of an empty value in folds, because the Done constructor
-- always produces a value (Done b). We can potentially use "Partial s b" and
-- "Done" to make the type correspond to the stream type. That may be possible
-- if we introduce the "Skip" constructor as well because after the last
-- "Partial s b" we have to emit a "Skip to Done" state to keep cranking the
-- fold until it is done.
--
-- There is also the asymmetry between folds and streams because folds have an
-- "initial" to initialize the fold without any input. A similar concept is
-- possible in streams as well to stop the stream. That would be a "closing"
-- operation for the stream which can be called even without consuming any item
-- from the stream or when we are done consuming.
--
-- However, the initial action in folds creates a discrepancy with the CPS
-- folds, and the same may be the case if we have a stop/cleanup operation in
-- streams.

-- | Make a fold that yields the supplied value without consuming any further
-- input.
--
-- /Pre-release/
--
{-# INLINE fromPure #-}
fromPure :: Applicative m => b -> Scanl m a b
fromPure b = Scanl undefined (pure $ Done b) pure pure

-- | Make a fold that yields the result of the supplied effectful action
-- without consuming any further input.
--
-- /Pre-release/
--
{-# INLINE fromEffect #-}
fromEffect :: Applicative m => m b -> Scanl m a b
fromEffect b = Scanl undefined (Done <$> b) pure pure

{-
{-# ANN type SeqFoldState Fuse #-}
data SeqFoldState sl f sr = SeqFoldL !sl | SeqFoldR !f !sr

-- | Sequential fold application. Apply two folds sequentially to an input
-- stream.  The input is provided to the first fold, when it is done - the
-- remaining input is provided to the second fold. When the second fold is done
-- or if the input stream is over, the outputs of the two folds are combined
-- using the supplied function.
--
-- Example:
--
-- >>> header = Fold.take 8 Fold.toList
-- >>> line = Fold.takeEndBy (== '\n') Fold.toList
-- >>> f = Fold.splitWith (,) header line
-- >>> Stream.fold f $ Stream.fromList "header: hello\n"
-- ("header: ","hello\n")
--
-- Note: This is dual to appending streams using 'Data.Stream.append'.
--
-- Note: this implementation allows for stream fusion but has quadratic time
-- complexity, because each composition adds a new branch that each subsequent
-- fold's input element has to traverse, therefore, it cannot scale to a large
-- number of compositions. After around 100 compositions the performance starts
-- dipping rapidly compared to a CPS style implementation.
--
-- For larger number of compositions you can convert the fold to a parser and
-- use ParserK.
--
-- /Time: O(n^2) where n is the number of compositions./
--
{-# INLINE splitWith #-}
splitWith :: Monad m =>
    (a -> b -> c) -> Fold m x a -> Fold m x b -> Fold m x c
splitWith func
    (Fold stepL initialL _ finalL)
    (Fold stepR initialR _ finalR) =
    Scanl step initial extract final

    where

    {-# INLINE runR #-}
    runR action f = bimap (SeqFoldR f) f <$> action

    {-# INLINE runL #-}
    runL action = do
        resL <- action
        chainStepM (return . SeqFoldL) (runR initialR . func) resL

    initial = runL initialL

    step (SeqFoldL st) a = runL (stepL st a)
    step (SeqFoldR f st) a = runR (stepR st a) f

    -- XXX splitWith should not be used for scanning
    -- It would rarely make sense and resource tracking and cleanup would be
    -- expensive. especially when multiple splitWith are chained.
    extract _ = error "splitWith: cannot be used for scanning"

    final (SeqFoldR f sR) = fmap f (finalR sR)
    final (SeqFoldL sL) = do
        rL <- finalL sL
        res <- initialR
        fmap (func rL)
            $ case res of
                Partial sR -> finalR sR
                Done rR -> return rR

{-# ANN type SeqFoldState_ Fuse #-}
data SeqFoldState_ sl sr = SeqFoldL_ !sl | SeqFoldR_ !sr

-- | Same as applicative '*>'. Run two folds serially one after the other
-- discarding the result of the first.
--
-- This was written in the hope that it might be faster than implementing it
-- using splitWith, but the current benchmarks show that it has the same
-- performance. So do not expose it unless some benchmark shows benefit.
--
{-# INLINE split_ #-}
split_ :: Monad m => Fold m x a -> Fold m x b -> Fold m x b
split_ (Fold stepL initialL _ finalL) (Fold stepR initialR _ finalR) =
    Scanl step initial extract final

    where

    initial = do
        resL <- initialL
        case resL of
            Partial sl -> return $ Partial $ SeqFoldL_ sl
            Done _ -> do
                resR <- initialR
                return $ first SeqFoldR_ resR

    step (SeqFoldL_ st) a = do
        r <- stepL st a
        case r of
            Partial s -> return $ Partial (SeqFoldL_ s)
            Done _ -> do
                resR <- initialR
                return $ first SeqFoldR_ resR
    step (SeqFoldR_ st) a = do
        resR <- stepR st a
        return $ first SeqFoldR_ resR

    -- XXX split_ should not be used for scanning
    -- See splitWith for more details.
    extract _ = error "split_: cannot be used for scanning"

    final (SeqFoldR_ sR) = finalR sR
    final (SeqFoldL_ sL) = do
        _ <- finalL sL
        res <- initialR
        case res of
            Partial sR -> finalR sR
            Done rR -> return rR

-- | 'Applicative' form of 'splitWith'. Split the input serially over two
-- folds. Note that this fuses but performance degrades quadratically with
-- respect to the number of compositions. It should be good to use for less
-- than 8 compositions.
instance Monad m => Applicative (Fold m a) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = splitWith id

    {-# INLINE (*>) #-}
    (*>) = split_

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)
-}

{-# ANN type TeeState Fuse #-}
data TeeState sL sR bL bR
    = TeeBoth !sL !sR
    | TeeLeft !bR !sL
    | TeeRight !bL !sR

-- | @teeWith k f1 f2@ distributes its input to both @f1@ and @f2@ until both
-- of them terminate and combines their output using @k@.
--
-- Definition:
--
-- >>> teeWith k f1 f2 = fmap (uncurry k) (Fold.tee f1 f2)
--
-- Example:
--
-- >>> avg = Fold.teeWith (/) Fold.sum (fmap fromIntegral Fold.length)
-- >>> Stream.fold avg $ Stream.fromList [1.0..100.0]
-- 50.5
--
-- For applicative composition using this combinator see
-- "Streamly.Data.Fold.Tee".
--
-- See also: "Streamly.Data.Fold.Tee"
--
-- Note that nested applications of teeWith do not fuse.
--
{-# INLINE teeWith #-}
teeWith :: Monad m => (a -> b -> c) -> Scanl m x a -> Scanl m x b -> Scanl m x c
teeWith f
    (Scanl stepL initialL extractL finalL)
    (Scanl stepR initialR extractR finalR) =
    Scanl step initial extract final

    where

    {-# INLINE runBoth #-}
    runBoth actionL actionR = do
        resL <- actionL
        resR <- actionR
        return
            $ case resL of
                  Partial sl ->
                      Partial
                          $ case resR of
                                Partial sr -> TeeBoth sl sr
                                Done br -> TeeLeft br sl
                  Done bl -> bimap (TeeRight bl) (f bl) resR

    initial = runBoth initialL initialR

    step (TeeBoth sL sR) a = runBoth (stepL sL a) (stepR sR a)
    step (TeeLeft bR sL) a = bimap (TeeLeft bR) (`f` bR) <$> stepL sL a
    step (TeeRight bL sR) a = bimap (TeeRight bL) (f bL) <$> stepR sR a

    extract (TeeBoth sL sR) = f <$> extractL sL <*> extractR sR
    extract (TeeLeft bR sL) = (`f` bR) <$> extractL sL
    extract (TeeRight bL sR) = f bL <$> extractR sR

    final (TeeBoth sL sR) = f <$> finalL sL <*> finalR sR
    final (TeeLeft bR sL) = (`f` bR) <$> finalL sL
    final (TeeRight bL sR) = f bL <$> finalR sR

{-# ANN type TeeFstState Fuse #-}
data TeeFstState sL sR b
    = TeeFstBoth !sL !sR
    | TeeFstLeft !b !sL

-- | Like 'teeWith' but terminates as soon as the first fold terminates.
--
-- /Pre-release/
--
{-# INLINE teeWithFst #-}
teeWithFst :: Monad m =>
    (b -> c -> d) -> Scanl m a b -> Scanl m a c -> Scanl m a d
teeWithFst f
    (Scanl stepL initialL extractL finalL)
    (Scanl stepR initialR extractR finalR) =
    Scanl step initial extract final

    where

    {-# INLINE runBoth #-}
    runBoth actionL actionR = do
        resL <- actionL
        resR <- actionR

        case resL of
            Partial sl ->
                return
                    $ Partial
                    $ case resR of
                        Partial sr -> TeeFstBoth sl sr
                        Done br -> TeeFstLeft br sl
            Done bl -> do
                Done . f bl <$>
                    case resR of
                        Partial sr -> finalR sr
                        Done br -> return br

    initial = runBoth initialL initialR

    step (TeeFstBoth sL sR) a = runBoth (stepL sL a) (stepR sR a)
    step (TeeFstLeft bR sL) a = bimap (TeeFstLeft bR) (`f` bR) <$> stepL sL a

    extract (TeeFstBoth sL sR) = f <$> extractL sL <*> extractR sR
    extract (TeeFstLeft bR sL) = (`f` bR) <$> extractL sL

    final (TeeFstBoth sL sR) = f <$> finalL sL <*> finalR sR
    final (TeeFstLeft bR sL) = (`f` bR) <$> finalL sL

-- | Like 'teeWith' but terminates as soon as any one of the two folds
-- terminates.
--
-- /Pre-release/
--
{-# INLINE teeWithMin #-}
teeWithMin :: Monad m =>
    (b -> c -> d) -> Scanl m a b -> Scanl m a c -> Scanl m a d
teeWithMin f
    (Scanl stepL initialL extractL finalL)
    (Scanl stepR initialR extractR finalR) =
    Scanl step initial extract final

    where

    {-# INLINE runBoth #-}
    runBoth actionL actionR = do
        resL <- actionL
        resR <- actionR
        case resL of
            Partial sl -> do
                case resR of
                    Partial sr -> return $ Partial $ Tuple' sl sr
                    Done br -> Done . (`f` br) <$> finalL sl

            Done bl -> do
                Done . f bl <$>
                    case resR of
                        Partial sr -> finalR sr
                        Done br -> return br

    initial = runBoth initialL initialR

    step (Tuple' sL sR) a = runBoth (stepL sL a) (stepR sR a)

    extract (Tuple' sL sR) = f <$> extractL sL <*> extractR sR

    final (Tuple' sL sR) = f <$> finalL sL <*> finalR sR

instance Monad m => Applicative (Scanl m a) where
    {-# INLINE pure #-}
    pure = fromPure

    (<*>) = teeWith id

-- | Shortest alternative. Apply both folds in parallel but choose the result
-- from the one which consumed least input i.e. take the shortest succeeding
-- fold.
--
-- If both the folds finish at the same time or if the result is extracted
-- before any of the folds could finish then the left one is taken.
--
-- /Pre-release/
--
{-# INLINE shortest #-}
shortest :: Monad m => Scanl m x a -> Scanl m x b -> Scanl m x (Either a b)
shortest (Scanl stepL initialL extractL finalL) (Scanl stepR initialR _ finalR) =
    Scanl step initial extract final

    where

    {-# INLINE runBoth #-}
    runBoth actionL actionR = do
        resL <- actionL
        resR <- actionR
        case resL of
            Partial sL ->
                case resR of
                    Partial sR -> return $ Partial $ Tuple' sL sR
                    Done bR -> finalL sL >> return (Done (Right bR))
            Done bL -> do
                case resR of
                    Partial sR -> void (finalR sR)
                    Done _ -> return ()
                return (Done (Left bL))

    initial = runBoth initialL initialR

    step (Tuple' sL sR) a = runBoth (stepL sL a) (stepR sR a)

    extract (Tuple' sL _) = Left <$> extractL sL

    final (Tuple' sL sR) = Left <$> finalL sL <* finalR sR

{-# ANN type LongestState Fuse #-}
data LongestState sL sR
    = LongestBoth !sL !sR
    | LongestLeft !sL
    | LongestRight !sR

-- | Longest alternative. Apply both folds in parallel but choose the result
-- from the one which consumed more input i.e. take the longest succeeding
-- fold.
--
-- If both the folds finish at the same time or if the result is extracted
-- before any of the folds could finish then the left one is taken.
--
-- /Pre-release/
--
{-# INLINE longest #-}
longest :: Monad m => Scanl m x a -> Scanl m x b -> Scanl m x (Either a b)
longest
    (Scanl stepL initialL _ finalL)
    (Scanl stepR initialR _ finalR) =
    Scanl step initial extract final

    where

    {-# INLINE runBoth #-}
    runBoth actionL actionR = do
        resL <- actionL
        resR <- actionR
        return $
            case resL of
                Partial sL ->
                    Partial $
                        case resR of
                            Partial sR -> LongestBoth sL sR
                            Done _ -> LongestLeft sL
                Done bL -> bimap LongestRight (const (Left bL)) resR

    initial = runBoth initialL initialR

    step (LongestBoth sL sR) a = runBoth (stepL sL a) (stepR sR a)
    step (LongestLeft sL) a = bimap LongestLeft Left <$> stepL sL a
    step (LongestRight sR) a = bimap LongestRight Right <$> stepR sR a

    -- XXX Scan with this may not make sense as we cannot determine the longest
    -- until one of them have exhausted.
    extract _ = error $ "longest: scan is not allowed as longest cannot be "
        ++ "determined until one fold has exhausted."

    final (LongestLeft sL) = Left <$> finalL sL
    final (LongestRight sR) = Right <$> finalR sR
    final (LongestBoth sL sR) = Left <$> finalL sL <* finalR sR

{-
data ConcatMapState m sa a b c
    = B !sa (sa -> m b)
    | forall s. C (s -> a -> m (Step s c)) !s (s -> m c) (s -> m c)

-- | Map a 'Fold' returning function on the result of a 'Fold' and run the
-- returned fold. This is akin to an n-ary version of 'splitWith' where the
-- next fold for splitting the input is decided dynamically using the previous
-- result. This operation can be used to express data dependencies
-- between fold operations.
--
-- Let's say the first element in the stream is a count of the following
-- elements that we have to add, then:
--
-- >>> import Data.Maybe (fromJust)
-- >>> count = fmap fromJust Fold.one
-- >>> total n = Fold.take n Fold.sum
-- >>> Stream.fold (Fold.concatMap total count) $ Stream.fromList [10,9..1]
-- 45
--
-- This does not fuse completely, see 'refold' for a fusible alternative.
--
-- /Time: O(n^2) where @n@ is the number of compositions./
--
-- See also: 'Streamly.Internal.Data.Stream.foldIterateM', 'refold'
--
{-# INLINE concatMap #-}
concatMap :: Monad m => (b -> Scanl m a c) -> Scanl m a b -> Scanl m a c
concatMap f (Fold stepa initiala _ finala) =
    Fold stepc initialc extractc finalc
  where
    initialc = do
        r <- initiala
        case r of
            Partial s -> return $ Partial (B s finala)
            Done b -> initInnerFold (f b)

    stepc (B s fin) a = do
        r <- stepa s a
        case r of
            Partial s1 -> return $ Partial (B s1 fin)
            Done b -> initInnerFold (f b)

    stepc (C stepInner s extractInner fin) a = do
        r <- stepInner s a
        return $ case r of
            Partial sc -> Partial (C stepInner sc extractInner fin)
            Done c -> Done c

    -- XXX Cannot use for scanning
    extractc _ = error "concatMap: cannot be used for scanning"

    initInnerFold (Scanl step i e fin) = do
        r <- i
        return $ case r of
            Partial s -> Partial (C step s e fin)
            Done c -> Done c

    initFinalize (Fold _ i _ fin) = do
        r <- i
        case r of
            Partial s -> fin s
            Done c -> return c

    finalc (B s fin) = do
        r <- fin s
        initFinalize (f r)
    finalc (C _ sInner _ fin) = fin sInner
-}

------------------------------------------------------------------------------
-- Mapping on input
------------------------------------------------------------------------------

-- | @lmap f fold@ maps the function @f@ on the input of the fold.
--
-- Definition:
--
-- >>> lmap = Fold.lmapM return
--
-- Example:
--
-- >>> sumSquared = Fold.lmap (\x -> x * x) Fold.sum
-- >>> Stream.fold sumSquared (Stream.enumerateFromTo 1 100)
-- 338350
--
{-# INLINE lmap #-}
lmap :: (a -> b) -> Scanl m b r -> Scanl m a r
lmap f (Scanl step begin done final) = Scanl step' begin done final
    where
    step' x a = step x (f a)

-- | @lmapM f fold@ maps the monadic function @f@ on the input of the fold.
--
{-# INLINE lmapM #-}
lmapM :: Monad m => (a -> m b) -> Scanl m b r -> Scanl m a r
lmapM f (Scanl step begin done final) = Scanl step' begin done final
    where
    step' x a = f a >>= step x

-- XXX This is concat or append

-- | Postscan the input of a 'Fold' to change it in a stateful manner using
-- another 'Fold'.
--
-- @postscan scanner collector@
--
-- /Pre-release/
{-# INLINE postscan #-}
postscan :: Monad m => Scanl m a b -> Scanl m b c -> Scanl m a c
postscan
    (Scanl stepL initialL extractL finalL)
    (Scanl stepR initialR extractR finalR) =
    Scanl step initial extract final

    where

    {-# INLINE runStep #-}
    runStep actionL sR = do
        rL <- actionL
        case rL of
            Done bL -> do
                rR <- stepR sR bL
                case rR of
                    Partial sR1 -> Done <$> finalR sR1
                    Done bR -> return $ Done bR
            Partial sL -> do
                !b <- extractL sL
                rR <- stepR sR b
                case rR of
                    Partial sR1 -> return $ Partial (sL, sR1)
                    Done bR -> finalL sL >> return (Done bR)

    initial = do
        rR <- initialR
        case rR of
            Partial sR -> do
                rL <- initialL
                case rL of
                    Done _ -> Done <$> finalR sR
                    Partial sL -> return $ Partial (sL, sR)
            Done b -> return $ Done b

    -- XXX should use Tuple'
    step (sL, sR) x = runStep (stepL sL x) sR

    extract = extractR . snd

    final (sL, sR) = finalL sL *> finalR sR

------------------------------------------------------------------------------
-- Filtering
------------------------------------------------------------------------------

-- | Modify a fold to receive a 'Maybe' input, the 'Just' values are unwrapped
-- and sent to the original fold, 'Nothing' values are discarded.
--
-- >>> catMaybes = Fold.mapMaybe id
-- >>> catMaybes = Fold.filter isJust . Fold.lmap fromJust
--
{-# INLINE_NORMAL catMaybes #-}
catMaybes :: Monad m => Scanl m a b -> Scanl m (Maybe a) b
catMaybes (Scanl step initial extract final) =
    Scanl step1 initial extract final

    where

    step1 s a =
        case a of
            Nothing -> return $ Partial s
            Just x -> step s x

-- | Use a 'Maybe' returning fold as a filtering scan.
--
-- >>> scanMaybe p f = Fold.postscan p (Fold.catMaybes f)
--
-- /Pre-release/
{-# INLINE scanMaybe #-}
scanMaybe :: Monad m => Scanl m a (Maybe b) -> Scanl m b c -> Scanl m a c
scanMaybe f1 f2 = postscan f1 (catMaybes f2)

-- | A scanning fold for filtering elements based on a predicate.
--
{-# INLINE filtering #-}
filtering :: Monad m => (a -> Bool) -> Scanl m a (Maybe a)
filtering f = scanl' step Nothing

    where

    step _ a = if f a then Just a else Nothing

-- | Include only those elements that pass a predicate.
--
-- >>> Stream.fold (Fold.filter (> 5) Fold.sum) $ Stream.fromList [1..10]
-- 40
--
-- >>> filter p = Fold.scanMaybe (Fold.filtering p)
-- >>> filter p = Fold.filterM (return . p)
-- >>> filter p = Fold.mapMaybe (\x -> if p x then Just x else Nothing)
--
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Scanl m a r -> Scanl m a r
-- filter p = scanMaybe (filtering p)
filter f (Scanl step begin extract final) = Scanl step' begin extract final
    where
    step' x a = if f a then step x a else return $ Partial x

-- | Like 'filter' but with a monadic predicate.
--
-- >>> f p x = p x >>= \r -> return $ if r then Just x else Nothing
-- >>> filterM p = Fold.mapMaybeM (f p)
--
{-# INLINE filterM #-}
filterM :: Monad m => (a -> m Bool) -> Scanl m a r -> Scanl m a r
filterM f (Scanl step begin extract final) = Scanl step' begin extract final
    where
    step' x a = do
      use <- f a
      if use then step x a else return $ Partial x

------------------------------------------------------------------------------
-- Either streams
------------------------------------------------------------------------------

-- | Discard 'Right's and unwrap 'Left's in an 'Either' stream.
--
-- /Pre-release/
--
{-# INLINE catLefts #-}
catLefts :: (Monad m) => Scanl m a c -> Scanl m (Either a b) c
catLefts = filter isLeft . lmap (fromLeft undefined)

-- | Discard 'Left's and unwrap 'Right's in an 'Either' stream.
--
-- /Pre-release/
--
{-# INLINE catRights #-}
catRights :: (Monad m) => Scanl m b c -> Scanl m (Either a b) c
catRights = filter isRight . lmap (fromRight undefined)

-- | Remove the either wrapper and flatten both lefts and as well as rights in
-- the output stream.
--
-- Definition:
--
-- >>> catEithers = Fold.lmap (either id id)
--
-- /Pre-release/
--
{-# INLINE catEithers #-}
catEithers :: Scanl m a b -> Scanl m (Either a a) b
catEithers = lmap (either id id)

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- Required to fuse "take" with "many" in "groupsOf", for ghc-9.x
{-# ANN type Tuple'Fused Fuse #-}
data Tuple'Fused a b = Tuple'Fused !a !b deriving Show

{-# INLINE taking #-}
taking :: Monad m => Int -> Scanl m a (Maybe a)
taking n = scant' step initial extract

    where

    initial =
        if n <= 0
        then Done Nothing
        else Partial (Tuple'Fused n Nothing)

    step (Tuple'Fused i _) a =
        if i > 1
        then Partial (Tuple'Fused (i - 1) (Just a))
        else Done (Just a)

    extract (Tuple'Fused _ r) = r

{-# INLINE dropping #-}
dropping :: Monad m => Int -> Scanl m a (Maybe a)
dropping n = scant' step initial extract

    where

    initial = Partial (Tuple'Fused n Nothing)

    step (Tuple'Fused i _) a =
        if i > 0
        then Partial (Tuple'Fused (i - 1) Nothing)
        else Partial (Tuple'Fused i (Just a))

    extract (Tuple'Fused _ r) = r

-- | Take at most @n@ input elements and fold them using the supplied fold. A
-- negative count is treated as 0.
--
-- >>> Stream.fold (Fold.take 2 Fold.toList) $ Stream.fromList [1..10]
-- [1,2]
--
{-# INLINE take #-}
take :: Monad m => Int -> Scanl m a b -> Scanl m a b
-- take n = scanMaybe (taking n)
take n (Scanl fstep finitial fextract ffinal) = Scanl step initial extract final

    where

    {-# INLINE next #-}
    next i res =
        case res of
            Partial s -> do
                let i1 = i + 1
                    s1 = Tuple'Fused i1 s
                if i1 < n
                then return $ Partial s1
                else Done <$> ffinal s
            Done b -> return $ Done b

    initial = finitial >>= next (-1)

    step (Tuple'Fused i r) a = fstep r a >>= next i

    extract (Tuple'Fused _ r) = fextract r

    final (Tuple'Fused _ r) = ffinal r

-- Note: Keep this consistent with S.splitOn. In fact we should eliminate
-- S.splitOn in favor of the fold.
--
-- XXX Use Fold.many instead once it is fixed.
-- > Stream.splitOnSuffix p f = Stream.foldMany (Fold.takeEndBy_ p f)

-- | Like 'takeEndBy' but drops the element on which the predicate succeeds.
--
-- Example:
--
-- >>> input = Stream.fromList "hello\nthere\n"
-- >>> line = Fold.takeEndBy_ (== '\n') Fold.toList
-- >>> Stream.fold line input
-- "hello"
--
-- >>> Stream.fold Fold.toList $ Stream.foldMany line input
-- ["hello","there"]
--
{-# INLINE takeEndBy_ #-}
takeEndBy_ :: Monad m => (a -> Bool) -> Scanl m a b -> Scanl m a b
-- takeEndBy_ predicate = scanMaybe (takingEndBy_ predicate)
takeEndBy_ predicate (Scanl fstep finitial fextract ffinal) =
    Scanl step finitial fextract ffinal

    where

    step s a =
        if not (predicate a)
        then fstep s a
        else Done <$> ffinal s

-- Note:
-- > Stream.splitWithSuffix p f = Stream.foldMany (Fold.takeEndBy p f)

-- | Take the input, stop when the predicate succeeds taking the succeeding
-- element as well.
--
-- Example:
--
-- >>> input = Stream.fromList "hello\nthere\n"
-- >>> line = Fold.takeEndBy (== '\n') Fold.toList
-- >>> Stream.fold line input
-- "hello\n"
--
-- >>> Stream.fold Fold.toList $ Stream.foldMany line input
-- ["hello\n","there\n"]
--
{-# INLINE takeEndBy #-}
takeEndBy :: Monad m => (a -> Bool) -> Scanl m a b -> Scanl m a b
-- takeEndBy predicate = scanMaybe (takingEndBy predicate)
takeEndBy predicate (Scanl fstep finitial fextract ffinal) =
    Scanl step finitial fextract ffinal

    where

    step s a = do
        res <- fstep s a
        if not (predicate a)
        then return res
        else do
            case res of
                Partial s1 -> Done <$> ffinal s1
                Done b -> return $ Done b

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------

-- Similar to the comonad "duplicate" operation.

{-
-- | 'duplicate' provides the ability to run a fold in parts.  The duplicated
-- fold consumes the input and returns the same fold as output instead of
-- returning the final result, the returned fold can be run later to consume
-- more input.
--
-- 'duplicate' essentially appends a stream to the fold without finishing the
-- fold.  Compare with 'snoc' which appends a singleton value to the fold.
--
-- /Pre-release/
{-# INLINE duplicate #-}
duplicate :: Monad m => Scanl m a b -> Scanl m a (Scanl m a b)
duplicate (Fold step1 initial1 extract1 final1) =
    Scanl step initial extract final

    where

    initial = second fromPure <$> initial1

    step s a = second fromPure <$> step1 s a

    -- Scanning may be problematic due to multiple finalizations.
    extract = error "duplicate: scanning may be problematic"

    final s = pure $ Fold step1 (pure $ Partial s) extract1 final1
-}

-- If there were a finalize/flushing action in the stream type that would be
-- equivalent to running initialize in Fold. But we do not have a flushing
-- action in streams.

-- | Evaluate the initialization effect of a fold. If we are building the fold
-- by chaining lazy actions in fold init this would reduce the actions to a
-- strict accumulator value.
--
-- /Pre-release/
{-# INLINE reduce #-}
reduce :: Monad m => Scanl m a b -> m (Scanl m a b)
reduce (Scanl step initial extract final) = do
    i <- initial
    return $ Scanl step (return i) extract final

-- This is the dual of Stream @cons@.

-- | Append an effect to the fold lazily, in other words run a single
-- step of the fold.
--
-- /Pre-release/
{-# INLINE snoclM #-}
snoclM :: Monad m => Scanl m a b -> m a -> Scanl m a b
snoclM (Scanl fstep finitial fextract ffinal) action =
    Scanl fstep initial fextract ffinal

    where

    initial = do
        res <- finitial
        case res of
            Partial fs -> action >>= fstep fs
            Done b -> return $ Done b

-- | Append a singleton value to the fold lazily, in other words run a single
-- step of the fold.
--
-- Definition:
--
-- >>> snocl f = Fold.snoclM f . return
--
-- Example:
--
-- >>> import qualified Data.Foldable as Foldable
-- >>> Fold.extractM $ Foldable.foldl Fold.snocl Fold.toList [1..3]
-- [1,2,3]
--
-- /Pre-release/
{-# INLINE snocl #-}
snocl :: Monad m => Scanl m a b -> a -> Scanl m a b
-- snocl f = snoclM f . return
snocl (Scanl fstep finitial fextract ffinal) a =
    Scanl fstep initial fextract ffinal

    where

    initial = do
        res <- finitial
        case res of
            Partial fs -> fstep fs a
            Done b -> return $ Done b

-- | Append a singleton value to the fold in other words run a single step of
-- the fold.
--
-- Definition:
--
-- >>> snocM f = Fold.reduce . Fold.snoclM f
--
-- /Pre-release/
{-# INLINE snocM #-}
snocM :: Monad m => Scanl m a b -> m a -> m (Scanl m a b)
snocM (Scanl step initial extract final) action = do
    res <- initial
    r <- case res of
          Partial fs -> action >>= step fs
          Done _ -> return res
    return $ Scanl step (return r) extract final

-- Definitions:
--
-- >>> snoc f = Fold.reduce . Fold.snocl f
-- >>> snoc f = Fold.snocM f . return

-- | Append a singleton value to the fold, in other words run a single step of
-- the fold.
--
-- Example:
--
-- >>> import qualified Data.Foldable as Foldable
-- >>> Foldable.foldlM Fold.snoc Fold.toList [1..3] >>= Fold.drive Stream.nil
-- [1,2,3]
--
-- /Pre-release/
{-# INLINE snoc #-}
snoc :: Monad m => Scanl m a b -> a -> m (Scanl m a b)
snoc (Scanl step initial extract final) a = do
    res <- initial
    r <- case res of
          Partial fs -> step fs a
          Done _ -> return res
    return $ Scanl step (return r) extract final

-- | Append a singleton value to the fold.
--
-- See examples under 'addStream'.
--
-- /Pre-release/
{-# INLINE addOne #-}
addOne :: Monad m => a -> Scanl m a b -> m (Scanl m a b)
addOne = flip snoc

-- Similar to the comonad "extract" operation.
-- XXX rename to extract. We can use "extr" for the fold extract function.

-- | Extract the accumulated result of the fold.
--
-- Definition:
--
-- >>> extractM = Fold.drive Stream.nil
--
-- Example:
--
-- >>> Fold.extractM Fold.toList
-- []
--
-- /Pre-release/
{-# INLINE extractM #-}
extractM :: Monad m => Scanl m a b -> m b
extractM (Scanl _ initial extract _) = do
    res <- initial
    case res of
          Partial fs -> extract fs
          Done b -> return b

-- | Close a fold so that it does not accept any more input.
{-# INLINE close #-}
close :: Monad m => Scanl m a b -> Scanl m a b
close (Scanl _ initial1 _ final1) =
    Scanl undefined initial undefined undefined

    where

    initial = do
        res <- initial1
        case res of
              Partial s -> Done <$> final1 s
              Done b -> return $ Done b

-- Corresponds to the null check for streams.

-- | Check if the fold has terminated and can take no more input.
--
-- /Pre-release/
{-# INLINE isClosed #-}
isClosed :: Monad m => Scanl m a b -> m Bool
isClosed (Scanl _ initial _ _) = do
    res <- initial
    return $ case res of
          Partial _ -> False
          Done _ -> True

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- All the grouping transformation that we apply to a stream can also be
-- applied to a fold input stream. groupBy et al can be written as terminating
-- folds and then we can apply "many" to use those repeatedly on a stream.

{-# ANN type ManyState Fuse #-}
data ManyState s1 s2
    = ManyFirst !s1 !s2
    | ManyLoop !s1 !s2

-- | Collect zero or more applications of a fold.  @many first second@ applies
-- the @first@ fold repeatedly on the input stream and accumulates it's results
-- using the @second@ fold.
--
-- >>> two = Fold.take 2 Fold.toList
-- >>> twos = Fold.many two Fold.toList
-- >>> Stream.fold twos $ Stream.fromList [1..10]
-- [[1,2],[3,4],[5,6],[7,8],[9,10]]
--
-- Stops when @second@ fold stops.
--
-- See also: 'Data.Stream.concatMap', 'Data.Stream.foldMany'
--
{-# INLINE many #-}
many :: Monad m => Scanl m a b -> Scanl m b c -> Scanl m a c
many
    (Scanl sstep sinitial sextract sfinal)
    (Scanl cstep cinitial cextract cfinal) =
    Scanl step initial extract final

    where

    -- cs = collect state
    -- ss = split state
    -- cres = collect state result
    -- sres = split state result
    -- cb = collect done
    -- sb = split done

    -- Caution! There is mutual recursion here, inlining the right functions is
    -- important.

    {-# INLINE split #-}
    split f cs sres =
        case sres of
            Partial ss -> return $ Partial $ f ss cs
            Done sb -> cstep cs sb >>= collect

    collect cres =
        case cres of
            Partial cs -> sinitial >>= split ManyFirst cs
            Done cb -> return $ Done cb

    -- A fold may terminate even without accepting a single input.  So we run
    -- the split fold's initial action even if no input is received.  However,
    -- this means that if no input was ever received by "step" we discard the
    -- fold's initial result which could have generated an effect. However,
    -- note that if "sinitial" results in Done we do collect its output even
    -- though the fold may not have received any input. XXX Is this
    -- inconsistent?
    initial = cinitial >>= collect

    {-# INLINE step_ #-}
    step_ ss cs a = sstep ss a >>= split ManyLoop cs

    {-# INLINE step #-}
    step (ManyFirst ss cs) a = step_ ss cs a
    step (ManyLoop ss cs) a = step_ ss cs a

    -- Do not extract the split fold if no item was consumed.
    extract (ManyFirst _ cs) = cextract cs
    extract (ManyLoop ss cs) = do
        cres <- sextract ss >>= cstep cs
        case cres of
            Partial s -> cextract s
            Done b -> return b

    final (ManyFirst ss cs) = sfinal ss *> cfinal cs
    final (ManyLoop ss cs) = do
        cres <- sfinal ss >>= cstep cs
        case cres of
            Partial s -> cfinal s
            Done b -> return b

-- | Like many, but the "first" fold emits an output at the end even if no
-- input is received.
--
-- /Internal/
--
-- See also: 'Data.Stream.concatMap', 'Data.Stream.foldMany'
--
{-# INLINE manyPost #-}
manyPost :: Monad m => Scanl m a b -> Scanl m b c -> Scanl m a c
manyPost
    (Scanl sstep sinitial sextract sfinal)
    (Scanl cstep cinitial cextract cfinal) =
    Scanl step initial extract final

    where

    -- cs = collect state
    -- ss = split state
    -- cres = collect state result
    -- sres = split state result
    -- cb = collect done
    -- sb = split done

    -- Caution! There is mutual recursion here, inlining the right functions is
    -- important.

    {-# INLINE split #-}
    split cs sres =
        case sres of
            Partial ss1 -> return $ Partial $ Tuple' ss1 cs
            Done sb -> cstep cs sb >>= collect

    collect cres =
        case cres of
            Partial cs -> sinitial >>= split cs
            Done cb -> return $ Done cb

    initial = cinitial >>= collect

    {-# INLINE step #-}
    step (Tuple' ss cs) a = sstep ss a >>= split cs

    extract (Tuple' ss cs) = do
        cres <- sextract ss >>= cstep cs
        case cres of
            Partial s -> cextract s
            Done b -> return b

    final (Tuple' ss cs) = do
        cres <- sfinal ss >>= cstep cs
        case cres of
            Partial s -> cfinal s
            Done b -> return b

-- | @groupsOf n split collect@ repeatedly applies the @split@ fold to chunks
-- of @n@ items in the input stream and supplies the result to the @collect@
-- fold.
--
-- Definition:
--
-- >>> groupsOf n split = Fold.many (Fold.take n split)
--
-- Example:
--
-- >>> twos = Fold.groupsOf 2 Fold.toList Fold.toList
-- >>> Stream.fold twos $ Stream.fromList [1..10]
-- [[1,2],[3,4],[5,6],[7,8],[9,10]]
--
-- Stops when @collect@ stops.
--
{-# INLINE groupsOf #-}
groupsOf :: Monad m => Int -> Scanl m a b -> Scanl m b c -> Scanl m a c
groupsOf n split = many (take n split)

------------------------------------------------------------------------------
-- Refold and Fold Combinators
------------------------------------------------------------------------------

-- | Like 'many' but uses a 'Refold' for collecting.
--
{-# INLINE refoldMany #-}
refoldMany :: Monad m => Scanl m a b -> Refold m x b c -> Refold m x a c
refoldMany
    (Scanl sstep sinitial sextract _sfinal)
    -- XXX We will need a "final" in refold as well
    (Refold cstep cinject cextract) =
    Refold step inject extract

    where

    -- cs = collect state
    -- ss = split state
    -- cres = collect state result
    -- sres = split state result
    -- cb = collect done
    -- sb = split done

    -- Caution! There is mutual recursion here, inlining the right functions is
    -- important.

    {-# INLINE split #-}
    split cs f sres =
        case sres of
            Partial ss -> return $ Partial $ Tuple' cs (f ss)
            Done sb -> cstep cs sb >>= collect

    collect cres =
        case cres of
            Partial cs -> sinitial >>= split cs Left
            Done cb -> return $ Done cb

    inject x = cinject x >>= collect

    {-# INLINE step_ #-}
    step_ ss cs a = sstep ss a >>= split cs Right

    {-# INLINE step #-}
    step (Tuple' cs (Left ss)) a = step_ ss cs a
    step (Tuple' cs (Right ss)) a = step_ ss cs a

    -- Do not extract the split fold if no item was consumed.
    extract (Tuple' cs (Left _)) = cextract cs
    extract (Tuple' cs (Right ss )) = do
        cres <- sextract ss >>= cstep cs
        case cres of
            Partial s -> cextract s
            Done b -> return b

{-# ANN type ConsumeManyState Fuse #-}
data ConsumeManyState x cs ss = ConsumeMany x cs (Either ss ss)

-- | Like 'many' but uses a 'Refold' for splitting.
--
-- /Internal/
{-# INLINE refoldMany1 #-}
refoldMany1 :: Monad m => Refold m x a b -> Scanl m b c -> Refold m x a c
refoldMany1
    (Refold sstep sinject sextract)
    (Scanl cstep cinitial cextract _cfinal) =
    Refold step inject extract

    where

    -- cs = collect state
    -- ss = split state
    -- cres = collect state result
    -- sres = split state result
    -- cb = collect done
    -- sb = split done

    -- Caution! There is mutual recursion here, inlining the right functions is
    -- important.

    {-# INLINE split #-}
    split x cs f sres =
        case sres of
            Partial ss -> return $ Partial $ ConsumeMany x cs (f ss)
            Done sb -> cstep cs sb >>= collect x

    collect x cres =
        case cres of
            Partial cs -> sinject x >>= split x cs Left
            Done cb -> return $ Done cb

    inject x = cinitial >>= collect x

    {-# INLINE step_ #-}
    step_ x ss cs a = sstep ss a >>= split x cs Right

    {-# INLINE step #-}
    step (ConsumeMany x cs (Left ss)) a = step_ x ss cs a
    step (ConsumeMany x cs (Right ss)) a = step_ x ss cs a

    -- Do not extract the split fold if no item was consumed.
    extract (ConsumeMany _ cs (Left _)) = cextract cs
    extract (ConsumeMany _ cs (Right ss )) = do
        cres <- sextract ss >>= cstep cs
        case cres of
            Partial s -> cextract s
            Done b -> return b

-- | Extract the output of a fold and refold it using a 'Refold'.
--
-- A fusible alternative to 'concatMap'.
--
-- /Internal/
{-# INLINE refold #-}
refold :: Monad m => Refold m b a c -> Scanl m a b -> Scanl m a c
refold (Refold step inject extract) f =
    Scanl step (extractM f >>= inject) extract extract

------------------------------------------------------------------------------
-- morphInner
------------------------------------------------------------------------------

-- | Change the underlying monad of a fold. Also known as hoist.
--
-- /Pre-release/
morphInner :: (forall x. m x -> n x) -> Scanl m a b -> Scanl n a b
morphInner f (Scanl step initial extract final) =
    Scanl (\x a -> f $ step x a) (f initial) (f . extract) (f . final)

-- | Adapt a pure fold to any monad.
--
-- >>> generalizeInner = Fold.morphInner (return . runIdentity)
--
-- /Pre-release/
generalizeInner :: Monad m => Scanl Identity a b -> Scanl m a b
generalizeInner = morphInner (return . runIdentity)
