-- |
-- Module      : Streamly.Internal.Data.Fold.Types
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- = Stream Consumers
--
-- We can classify stream consumers in the following categories in order of
-- increasing complexity and power:
--
-- == Accumulators
--
-- These are the simplest folds that never fail and never terminate, they
-- accumulate the input values forever and always remain @partial@ and
-- @complete@ at the same time. It means that we can keep adding more input to
-- them or at any time retrieve a consistent result. A
-- 'Streamly.Internal.Data.Fold.sum' operation is an example of an accumulator.
--
-- We can distribute an input stream to two or more accumulators using a @tee@
-- style composition.  Accumulators cannot be applied on a stream one after the
-- other, which we call a @split@ style composition, as the first one itself
-- will never terminate, therefore, the next one will never get to run.
--
-- == Splitters
--
-- Splitters are accumulators that can terminate. When applied on a stream
-- splitters consume part of the stream, thereby, splitting it.  Splitters can
-- be used in a @split@ style composition where one splitter can be applied
-- after the other on an input stream. We can apply a splitter repeatedly on an
-- input stream splitting and consuming it in fragments.  Splitters never fail,
-- therefore, they do not need backtracking, but they can lookahead and return
-- unconsumed input. The 'Streamly.Internal.Data.Parser.take' operation is an
-- example of a splitter. It terminates after consuming @n@ items. Coupled with
-- an accumulator it can be used to split the stream into chunks of fixed size.
--
-- Consider the example of @takeWhile@ operation, it needs to inspect an
-- element for termination decision. However, it does not consume the element
-- on which it terminates. To implement @takeWhile@ a splitter will have to
-- implement a way to return unconsumed input to the driver.
--
-- == Parsers
--
-- Parsers are splitters that can fail and backtrack. Parsers can be composed
-- using an @alternative@ style composition where they can backtrack and apply
-- another parser if one parser fails. 'Streamly.Internal.Data.Parser.satisfy'
-- is a simple example of a parser, it would succeed if the condition is
-- satisfied and it would fail otherwise, on failure an alternative parser can
-- be used on the same input.
--
-- = Types for Stream Consumers
--
-- We use the 'Fold' type to implement the Accumulator and Splitter
-- functionality.  Parsers are represented by the
-- 'Streamly.Internal.Data.Parser.Parser' type.  This is a sweet spot to
-- balance ease of use, type safety and performance.  Using separate
-- Accumulator and Splitter types would encode more information in types but it
-- would make ease of use, implementation, maintenance effort worse. Combining
-- Accumulator, Splitter and Parser into a single
-- 'Streamly.Internal.Data.Parser.Parser' type would make ease of use even
-- better but type safety and performance worse.
--
-- One of the design requirements that we have placed for better ease of use
-- and code reuse is that 'Streamly.Internal.Data.Parser.Parser' type should be
-- a strict superset of the 'Fold' type i.e. it can do everything that a 'Fold'
-- can do and more. Therefore, folds can be easily upgraded to parsers and we
-- can use parser combinators on folds as well when needed.
--
-- = Fold Design
--
-- A fold is represented by a collection of "initial", "step" and "extract"
-- functions. The "initial" action generates the initial state of the fold. The
-- state is internal to the fold and maintains the accumulated output. The
-- "step" function is invoked using the current state and the next input value
-- and results in a @Partial@ or @>Done@. A @Partial@ returns the next intermediate
-- state of the fold, a @>Done@ indicates that the fold has terminated and
-- returns the final value of the accumulator.
--
-- Every @Partial@ indicates that a new accumulated output is available.  The
-- accumulated output can be extracted from the state at any point using
-- "extract". "extract" can never fail. A fold returns a valid output even
-- without any input i.e. even if you call "extract" on "initial" state it
-- provides an output. This is not true for parsers.
--
-- In general, "extract" is used in two cases:
--
-- * When the fold is used as a scan @extract@ is called on the intermediate
-- state every time it is yielded by the fold, the resulting value is yielded
-- as a stream.
-- * When the fold is used as a regular fold, @extract@ is called once when
-- we are done feeding input to the fold.
--
-- = Alternate Designs
--
-- An alternate and simpler design would be to return the intermediate output
-- via @Partial@ along with the state, instead of using "extract" on the yielded
-- state and remove the extract function altogether.
--
-- This may even facilitate more efficient implementation.  Extract from the
-- intermediate state after each yield may be more costly compared to the fold
-- step itself yielding the output. The fold may have more efficient ways to
-- retrieve the output rather than stuffing it in the state and using extract
-- on the state.
--
-- However, removing extract altogether may lead to less optimal code in some
-- cases because the driver of the fold needs to thread around the intermediate
-- output to return it if the stream stops before the fold could @>Done@.  When
-- using this approach, the @splitParse (FL.take filesize)@ benchmark shows a
-- 2x worse performance even after ensuring everything fuses.  So we keep the
-- "extract" approach to ensure better perf in all cases.
--
-- But we could still yield both state and the output in @Partial@, the output
-- can be used for the scan use case, instead of using extract. Extract would
-- then be used only for the case when the stream stops before the fold
-- completes.

module Streamly.Internal.Data.Fold.Types
    ( Step (..)
    , Fold (..)
    , Fold2 (..)
    , simplify
    , toListRevF  -- experimental
    -- $toListRevF
    , lmap
    , lmapM
    , lfilter
    , lfilterM
    , lcatMaybes
    , ltake
    , ltakeWhile

    , teeWith
    , teeWithFst
    , teeWithMin

    , splitWith
    , many

    , lsessionsOf
    , lchunksOf
    , lchunksOf2

    , duplicate
    , initialize
    , runStep

    , GenericRunState(..)
    )
where

import Control.Applicative (liftA2)
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, modifyMVar_)
import Control.Exception (SomeException(..), catch, mask)
import Control.Monad (void)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (control)
import Data.Bifunctor (Bifunctor(..))
import Data.Maybe (isJust, fromJust)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..), Tuple5'(..))
-- import Streamly.Internal.Data.Either.Strict (Either'(..))
import Streamly.Internal.Data.SVar (MonadAsync)

------------------------------------------------------------------------------
-- Monadic left folds
------------------------------------------------------------------------------

-- | This is the intermediate state in the workflow of a @Fold@. @Partial@
-- represents a new intermediate value available to be extracted. @Done@
-- represents that the fold has been terminated and no won't be processed
-- further.
data Step s b
    = Partial !s
    | Done !b

-- | A bifunctor instance on 'Step'. @first@ maps on the value held by 'Partial'
-- and @second@ maps on the reult held by 'Done'.
instance Bifunctor Step where
    {-# INLINE bimap #-}
    bimap f _ (Partial a) = Partial (f a)
    bimap _ g (Done b) = Done (g b)

    {-# INLINE first #-}
    first f (Partial a) = Partial (f a)
    first _ (Done x) = Done x

    {-# INLINE second #-}
    second _ (Partial x) = Partial x
    second f (Done a) = Done (f a)

-- | Maps the function over the result held by 'Done'.
-- @
-- fmap = 'second'
-- @
instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap = second

-- | Represents a left fold over an input stream consisting of values of type
-- @a@ to a single value of type @b@ in 'Monad' @m@.
--
-- The fold uses an intermediate state @s@ as accumulator. The @step@ function
-- processes the state and returns a 'Step'. When the fold is done
-- the final result of the fold is extracted from the intermediate state
-- using the @extract@ function.
--
-- @since 0.7.0

data Fold m a b =
  -- | @Fold @ @ step @ @ initial @ @ extract@
  forall s. Fold (s -> a -> m (Step s b)) (m s) (s -> m b)

-- | Experimental type to provide a side input to the fold for generating the
-- initial state. For example, if we have to fold chunks of a stream and write
-- each chunk to a different file, then we can generate the file name using a
-- monadic action. This is a generalized version of 'Fold'.
--
data Fold2 m c a b =
  -- | @Fold @ @ step @ @ inject @ @ extract@
  forall s. Fold2 (s -> a -> m s) (c -> m s) (s -> m b)

-- | Convert more general type 'Fold2' into a simpler type 'Fold'
simplify :: Functor m => Fold2 m c a b -> c -> Fold m a b
simplify (Fold2 step inject extract) c = Fold step1 (inject c) extract
    where
    step1 x a = Partial <$> step x a

-- | Maps a function on the output of the fold (the type @b@).
instance Functor m => Functor (Fold m a) where
    {-# INLINE fmap #-}
    fmap f (Fold step start done) = Fold step1 start done'
        where
        done' x = fmap f $! done x
        step1 x a = fmap (second f) $! step x a

-- {-# ANN type Step Fuse #-}
-- data SeqFoldState sl f sr = SeqFoldL !sl | SeqFoldR !f !sr
data SeqFoldState sl f sr = SeqFoldL sl | SeqFoldR f sr

-- | Sequential fold application. Apply two folds sequentially to an input
-- stream.  The input is provided to the first fold, when it is done the
-- remaining input is provided to the second fold. When the second fold is
-- done, the outputs of the two folds are combined using the supplied function.
--
-- Note: This is a folding dual of appending streams using
-- 'Streamly.Prelude.serial', it splits the streams using two folds and zips
-- the results. This has the same caveats as ParseD's @splitWith@
--
-- /Internal/
--
{-# INLINE splitWith #-}
splitWith :: Monad m =>
    (a -> b -> c) -> Fold m x a -> Fold m x b -> Fold m x c
splitWith func (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    initial = SeqFoldL <$> initialL

    step (SeqFoldL st) a = do
        r <- stepL st a
        case r of
            Partial s -> return $ Partial (SeqFoldL s)
            Done b -> Partial <$> (SeqFoldR (func b) <$> initialR)
    step (SeqFoldR f st) a = do
        r <- stepR st a
        return
          $ case r of
                Partial s -> Partial (SeqFoldR f s)
                Done b -> Done (f b)

    extract (SeqFoldR f sR) = fmap f (extractR sR)
    extract (SeqFoldL sL) = do
        rL <- extractL sL
        sR <- initialR
        rR <- extractR sR
        return $ func rL rR

-- | The fold resulting from '<*>' distributes its input to both the argument
-- folds and combines their output using the supplied function.
instance Monad m => Applicative (Fold m a) where
    {-# INLINE pure #-}
    pure b = Fold (\() _ -> pure (Partial ())) (pure ()) (\() -> pure b)

    {-# INLINE (<*>) #-}
    (<*>) = teeWith ($)

data GenericRunState fl fr bl br
    = RunBoth !fl !fr
    | RunLeft !fl !br
    | RunRight !bl !fr

-- | @teeWith f f1 f2@ distributes its input to both @f1@ and @f2@ until both
-- of them terminate and combines their output using @f@.
--
-- /Internal/
--
{-# INLINE teeWith #-}
teeWith :: Monad m => (b -> c -> d) -> Fold m a b -> Fold m a c -> Fold m a d
teeWith f (Fold stepL beginL doneL) (Fold stepR beginR doneR) =
    Fold step begin done

    where

    begin = do
        fl <- beginL
        fr <- beginR
        return $ RunBoth fl fr

    step (RunBoth fl fr) a = do
        sfl <- stepL fl a
        sfr <- stepR fr a
        return
            $ case sfl of
                  Partial fl1 ->
                      Partial
                          $ case sfr of
                                Partial fr1 -> RunBoth fl1 fr1
                                Done br -> RunLeft fl1 br
                  Done bl ->
                      case sfr of
                          Partial fr1 -> Partial $ RunRight bl fr1
                          Done br -> Done $ f bl br
    step (RunLeft fl br) a = do
        sfl <- stepL fl a
        return
            $ case sfl of
                  Partial fl1 -> Partial $ RunLeft fl1 br
                  Done bl -> Done $ f bl br
    step (RunRight bl fr) a = do
        sfr <- stepR fr a
        return
            $ case sfr of
                  Partial fr1 -> Partial $ RunRight bl fr1
                  Done br -> Done $ f bl br

    done (RunBoth fl fr) = do
        bl <- doneL fl
        br <- doneR fr
        return $ f bl br
    done (RunLeft fl br) = do
        bl <- doneL fl
        return $ f bl br
    done (RunRight bl fr) = do
        br <- doneR fr
        return $ f bl br

-- | Like 'teeWith' but terminates when the first fold terminates.
--
-- /Unimplemented/
--
{-# INLINE teeWithFst #-}
teeWithFst :: (b -> c -> d) -> Fold m a b -> Fold m a c -> Fold m a d
teeWithFst = undefined

-- | Like 'teeWith' but terminates when any fold terminates.
--
-- /Unimplemented/
--
{-# INLINE teeWithMin #-}
teeWithMin :: (b -> c -> d) -> Fold m a b -> Fold m a c -> Fold m a d
teeWithMin = undefined

-- | Combines the outputs of the folds (the type @b@) using their 'Semigroup'
-- instances.
instance (Semigroup b, Monad m) => Semigroup (Fold m a b) where
    {-# INLINE (<>) #-}
    (<>) = liftA2 (<>)

-- | Combines the outputs of the folds (the type @b@) using their 'Monoid'
-- instances.
instance (Semigroup b, Monoid b, Monad m) => Monoid (Fold m a b) where
    {-# INLINE mempty #-}
    mempty = pure mempty

    {-# INLINE mappend #-}
    mappend = (<>)

-- | Combines the fold outputs (type @b@) using their 'Num' instances.
instance (Monad m, Num b) => Num (Fold m a b) where
    {-# INLINE fromInteger #-}
    fromInteger = pure . fromInteger

    {-# INLINE negate #-}
    negate = fmap negate

    {-# INLINE abs #-}
    abs = fmap abs

    {-# INLINE signum #-}
    signum = fmap signum

    {-# INLINE (+) #-}
    (+) = liftA2 (+)

    {-# INLINE (*) #-}
    (*) = liftA2 (*)

    {-# INLINE (-) #-}
    (-) = liftA2 (-)

-- | Combines the fold outputs (type @b@) using their 'Fractional' instances.
instance (Monad m, Fractional b) => Fractional (Fold m a b) where
    {-# INLINE fromRational #-}
    fromRational = pure . fromRational

    {-# INLINE recip #-}
    recip = fmap recip

    {-# INLINE (/) #-}
    (/) = liftA2 (/)

-- | Combines the fold outputs using their 'Floating' instances.
instance (Monad m, Floating b) => Floating (Fold m a b) where
    {-# INLINE pi #-}
    pi = pure pi

    {-# INLINE exp #-}
    exp = fmap exp

    {-# INLINE sqrt #-}
    sqrt = fmap sqrt

    {-# INLINE log #-}
    log = fmap log

    {-# INLINE sin #-}
    sin = fmap sin

    {-# INLINE tan #-}
    tan = fmap tan

    {-# INLINE cos #-}
    cos = fmap cos

    {-# INLINE asin #-}
    asin = fmap asin

    {-# INLINE atan #-}
    atan = fmap atan

    {-# INLINE acos #-}
    acos = fmap acos

    {-# INLINE sinh #-}
    sinh = fmap sinh

    {-# INLINE tanh #-}
    tanh = fmap tanh

    {-# INLINE cosh #-}
    cosh = fmap cosh

    {-# INLINE asinh #-}
    asinh = fmap asinh

    {-# INLINE atanh #-}
    atanh = fmap atanh

    {-# INLINE acosh #-}
    acosh = fmap acosh

    {-# INLINE (**) #-}
    (**) = liftA2 (**)

    {-# INLINE logBase #-}
    logBase = liftA2 logBase

------------------------------------------------------------------------------
-- Internal APIs
------------------------------------------------------------------------------

-- $toListRevF
-- This is more efficient than 'Streamly.Internal.Data.Fold.toList'. toList is
-- exactly the same as reversing the list after 'toListRevF'.

-- | Buffers the input stream to a list in the reverse order of the input.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Array" instead.
--
-- @since 0.7.0

--  xn : ... : x2 : x1 : []
{-# INLINABLE toListRevF #-}
toListRevF :: Monad m => Fold m a [a]
toListRevF = Fold (\xs x -> return $ Partial $ x:xs) (return []) return

-- | @(lmap f fold)@ maps the function @f@ on the input of the fold.
--
-- >>> S.fold (FL.lmap (\x -> x * x) FL.sum) (S.enumerateFromTo 1 100)
-- 338350
--
-- @since 0.7.0
{-# INLINABLE lmap #-}
lmap :: (a -> b) -> Fold m b r -> Fold m a r
lmap f (Fold step begin done) = Fold step' begin done
  where
    step' x a = step x (f a)

-- | @(lmapM f fold)@ maps the monadic function @f@ on the input of the fold.
--
-- @since 0.7.0
{-# INLINABLE lmapM #-}
lmapM :: Monad m => (a -> m b) -> Fold m b r -> Fold m a r
lmapM f (Fold step begin done) = Fold step' begin done
  where
    step' x a = f a >>= step x

------------------------------------------------------------------------------
-- Filtering
------------------------------------------------------------------------------

-- | Include only those elements that pass a predicate.
--
-- >>> S.fold (lfilter (> 5) FL.sum) [1..10]
-- 40
--
-- @since 0.7.0
{-# INLINABLE lfilter #-}
lfilter :: Monad m => (a -> Bool) -> Fold m a r -> Fold m a r
lfilter f (Fold step begin done) = Fold step' begin done
  where
    step' x a = if f a then step x a else return $ Partial x

-- | Like 'lfilter' but with a monadic predicate.
--
-- @since 0.7.0
{-# INLINABLE lfilterM #-}
lfilterM :: Monad m => (a -> m Bool) -> Fold m a r -> Fold m a r
lfilterM f (Fold step begin done) = Fold step' begin done
  where
    step' x a = do
      use <- f a
      if use then step x a else return $ Partial x

-- | Transform a fold from a pure input to a 'Maybe' input, consuming only
-- 'Just' values.
{-# INLINE lcatMaybes #-}
lcatMaybes :: Monad m => Fold m a b -> Fold m (Maybe a) b
lcatMaybes = lfilter isJust . lmap fromJust

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- All the grouping transformation that we apply to a stream can also be
-- applied to a fold input stream. groupBy et al can be written as terminating
-- folds and then we can apply "many" to use those repeatedly on a stream.

-- | Collect zero or more applications of a fold.  @many collect split@ applies
-- the @split@ fold repeatedly on the input stream and accumulates zero or more
-- fold results using @collect@.
--
-- /Internal/
--
-- /See also: Streamly.Prelude.concatMap, Streamly.Prelude.foldMany/
--
{-# INLINE many #-}
many :: Monad m => Fold m b c -> Fold m a b -> Fold m a c
many (Fold fstep finitial fextract) (Fold step1 initial1 extract1) =
    Fold step initial extract

    where

    initial = do
        ps <- initial1
        fs <- finitial
        pure (Tuple' ps fs)

    {-# INLINE step #-}
    step (Tuple' st fs) a = do
        r <- step1 st a
        case r of
            Partial s -> return $ Partial (Tuple' s fs)
            Done b -> do
                s <- initial1
                fs1 <- fstep fs b
                return
                    $ case fs1 of
                          Partial s1 -> Partial (Tuple' s s1)
                          Done b1 -> Done b1

    extract (Tuple' s fs) = do
        b <- extract1 s
        acc <- fstep fs b
        case acc of
            Partial s1 -> fextract s1
            Done x -> return x

-- If i <= 0 then ltake consumes an input element silently.
-- | Take first @n@ elements from the stream and discard the rest.
--
-- @since 0.7.0
{-# INLINABLE ltake #-}
ltake :: Monad m => Int -> Fold m a b -> Fold m a b
ltake n (Fold step initial done) = Fold step' initial' done'
    where
    initial' = fmap (Tuple' 0) initial
    step' (Tuple' i r) a =
        if i < n
        then do
            res <- step r a
            case res of
                Partial ps -> do
                    let i1 = i + 1
                        s1 = Tuple' i1 ps
                    if i1 < n
                    then return $ Partial s1
                    else Done <$> done ps
                Done d -> return $ Done d
        -- This discards an element.
        else Done <$> done r
    done' (Tuple' _ r) = done r

-- | Takes elements from the input as long as the predicate succeeds.
--
-- @since 0.7.0
{-# INLINABLE ltakeWhile #-}
ltakeWhile :: Monad m => (a -> Bool) -> Fold m a b -> Fold m a b
ltakeWhile predicate (Fold step initial done) = Fold step' initial done
    where
    step' s a =
        if predicate a
        then step s a
        else Done <$> done s

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------
--
-- | Modify the fold such that when the fold is done, instead of returning the
-- accumulator, it returns a fold. The returned fold starts from where we left
-- i.e. it uses the last accumulator value as the initial value of the
-- accumulator. Thus we can resume the fold later and feed it more input.
--
-- >> do
-- >    more <- S.fold (FL.duplicate FL.sum) (S.enumerateFromTo 1 10)
-- >    evenMore <- S.fold (FL.duplicate more) (S.enumerateFromTo 11 20)
-- >    S.fold evenMore (S.enumerateFromTo 21 30)
-- > 465
--
-- @since 0.7.0
{-# INLINABLE duplicate #-}
duplicate ::
    --Applicative m =>
    Fold m a b -> Fold m a (Fold m a b)
duplicate _ = undefined

-- | Run the initialization effect of a fold. The returned fold would use the
-- value returned by this effect as its initial value.
--
{-# INLINABLE initialize #-}
initialize :: Monad m => Fold m a b -> m (Fold m a b)
initialize (Fold step initial extract) = do
    i <- initial
    return $ Fold step (return i) extract

-- | Run one step of a fold and store the accumulator as an initial value in
-- the returned fold.
{-# INLINABLE runStep #-}
runStep :: Monad m => Fold m a b -> a -> m (Fold m a b)
runStep (Fold step initial extract) a = do
    i <- initial
    res <- step i a
    return $ case res of
        Partial s -> Fold step (return s) extract
        Done b ->
            let step1 _ _ = return $ Done b
                extract1 _ = return b
             in Fold step1 (return i) extract1

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- XXX These can be expressed using foldChunks repeatedly on the input of a
-- fold.

-- XXX This does not work well when i is 0
-- | For every n input items, apply the first fold and supply the result to the
-- next fold.
--
{-# INLINE lchunksOf #-}
lchunksOf :: Monad m => Int -> Fold m a b -> Fold m b c -> Fold m a c
lchunksOf n (Fold step1 initial1 extract1) (Fold step2 initial2 extract2) =
    Fold step' initial' extract'

    where

    initial' = Tuple3' 0 <$> initial1 <*> initial2

    {-# INLINE splitter #-}
    splitter i a r1 r2 = do
        res <- step1 r1 a
        case res of
            Partial ss -> return $ Partial $ Tuple3' (i + 1) ss r2
            Done bs -> do
                acc2 <- step2 r2 bs
                case acc2 of
                    Partial sc -> do
                        i1 <- initial1
                        return $ Partial $ Tuple3' 0 i1 sc
                    Done bc -> return $ Done bc

    step' (Tuple3' i r1 r2) a =
        if i < n
        then splitter i a r1 r2
        else do
            res <- extract1 r1
            acc2 <- step2 r2 res
            case acc2 of
                Partial sc -> do
                    i1 <- initial1
                    splitter 0 a i1 sc
                Done bc -> return $ Done bc

    extract' (Tuple3' _ r1 r2) = do
        res <- extract1 r1
        acc2 <- step2 r2 res
        case acc2 of
            Partial sc -> extract2 sc
            Done bs -> return bs

{-# INLINE lchunksOf2 #-}
lchunksOf2 :: Monad m => Int -> Fold m a b -> Fold2 m x b c -> Fold2 m x a c
lchunksOf2 n (Fold step1 initial1 extract1) (Fold2 step2 inject2 extract2) =
    Fold2 step' inject' extract'

    where

    inject' x = Tuple3' 0 <$> initial1 <*> inject2 x

    {-# INLINE splitter #-}
    splitter i a r1 r2 = do
        res <- step1 r1 a
        case res of
            Partial ss -> return $ Tuple3' (i + 1) ss r2
            Done bs -> do
                sc <- step2 r2 bs
                i1 <- initial1
                return $ Tuple3' 0 i1 sc

    step' (Tuple3' i r1 r2) a =
        if i < n
        then splitter i a r1 r2
        else do
            res <- extract1 r1
            acc2 <- step2 r2 res
            i1 <- initial1
            splitter 0 a i1 acc2

    extract' (Tuple3' _ r1 r2) = do
        res <- extract1 r1
        acc2 <- step2 r2 res
        extract2 acc2

-- XXX We need to think about a more better solution. Using a timer to update a
-- flag gives wrong results on a termination condition.
-- XXX This does not even handle an MVar exception
-- | Group the input stream into windows of n second each and then fold each
-- group using the provided fold function.
--
-- For example, we can copy and distribute a stream to multiple folds where
-- each fold can group the input differently e.g. by one second, one minute and
-- one hour windows respectively and fold each resulting stream of folds.
--
-- @
--
-- -----Fold m a b----|-Fold n a c-|-Fold n a c-|-...-|----Fold m a c
--
-- @
{-# INLINE lsessionsOf #-}
lsessionsOf :: MonadAsync m => Double -> Fold m a b -> Fold m b c -> Fold m a c
lsessionsOf n (Fold stepS initialS extractS) (Fold stepC initialC extractC) =
    Fold step initial extract

    where

    timerThread mv =
        liftIO
            $ do
                threadDelay (round $ n * 1000000)
                modifyMVar_ mv (return . fmap not)

    handleChildException ::
        MVar (Either SomeException a) -> SomeException -> IO ()
    handleChildException mv e = do
        er <- takeMVar mv
        let er1 = case er of
                    Left _ -> er
                    Right _ -> Left e
        putMVar mv er1

    -- XXX MVar may be expensive we need a cheaper synch mechanism here
    initial = do
        fs <- initialS
        fc <- initialC
        mv <- liftIO $ newMVar (Right False)
        t <-
            control $ \run ->
                mask $ \restore -> do
                    tid <- forkIO $ catch (restore $ void $ run $ timerThread mv)
                                          (handleChildException mv)
                    run (return tid)
        return $ Tuple5' True t mv fs fc

    {-# INLINE splitter #-}
    splitter !p !t !mv !a !fs !fc = do
        sfs <- stepS fs a
        case sfs of
            Partial fs1 -> return $ Partial $ Tuple5' p t mv fs1 fc
            Done bs -> do
                sfc <- stepC fc bs
                case sfc of
                    Partial fc1 -> do
                        fs1 <- initialS
                        return $ Partial $ Tuple5' p t mv fs1 fc1
                    Done bc -> do
                        liftIO $ killThread t
                        return $ Done bc

    step (Tuple5' p t mv fs fc) a = do
        mp0 <- liftIO $ takeMVar mv
        case mp0 of
            Right p0 ->
                if p0 /= p
                then splitter p t mv a fs fc
                else do
                    bs <- extractS fs
                    sfc <- stepC fc bs
                    case sfc of
                        Partial fc1 -> do
                           fs1 <- initialS
                           splitter (not p) t mv a fs1 fc1
                        Done bc -> do
                           liftIO $ killThread t
                           return $ Done bc
            Left _ -> do
                -- XXX throwM e
                liftIO $ killThread t
                bc <- extractC fc
                return $ Done bc

    extract (Tuple5' _ t mv _ fc) = do
        er <- liftIO $ takeMVar mv
        liftIO $ killThread t
        case er of
            Right _ -> extractC fc
            Left e -> throwM e
