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
-- and results in a @Partial@ or @Done@. A @Partial@ returns the next intermediate
-- state of the fold, a @Done@ indicates that the fold has terminated and
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
-- output to return it if the stream stops before the fold could @Done@.  When
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
    , liftStep
    , liftExtract
    , liftInitial
    , liftInitialM
    , partialM
    , doneM
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
    , lsessionsOf
    , lchunksOf
    , lchunksOf2

    , duplicate
    , initialize
    , runStep
    )
where

import Data.Bifunctor
import Control.Applicative (liftA2)
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception (SomeException(..), catch, mask)
import Control.Monad (void)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (control)
import Data.Maybe (isJust, fromJust)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))
import Streamly.Internal.Data.SVar (MonadAsync)

------------------------------------------------------------------------------
-- Monadic left folds
------------------------------------------------------------------------------

-- {-# ANN type Step Fuse #-}
data Step s b = Partial !s | Done !b

instance Bifunctor Step where
    {-# INLINE bimap #-}
    bimap f _ (Partial a) = Partial (f a)
    bimap _ g (Done b) = Done (g b)

    {-# INLINE first #-}
    first f (Partial a) = Partial (f a)
    first _ (Done x) = Done x

    {-# INLINE second #-}
    second f (Done a) = Done (f a)
    second _ (Partial x) = Partial x

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap = second

-- | Represents a left fold over an input stream of values of type @a@ to a
-- single value of type @b@ in 'Monad' @m@.
--
-- The fold uses an intermediate state @s@ as accumulator. The @step@ function
-- updates the state and returns the new state. When the fold is done
-- the final result of the fold is extracted from the intermediate state
-- using the @extract@ function.
--
-- @since 0.7.0

data Fold m a b =
  -- | @Fold @ @ step @ @ initial @ @ extract@
  forall s. Fold (s -> a -> m (Step s b)) (m s) (s -> m b)

{-# INLINE liftStep #-}
liftStep :: Monad m => (s -> a -> m (Step s b)) -> Step s b -> a -> m (Step s b)
liftStep step (Partial s) a = step s a
liftStep _ x _ = return x

{-# INLINE liftExtract #-}
liftExtract :: Monad m => (s -> m b) -> Step s b -> m b
liftExtract _ (Done b) = return b
liftExtract done (Partial s) = done s

{-# INLINE liftInitial #-}
liftInitial :: s -> Step s b
liftInitial = Partial

{-# INLINE liftInitialM #-}
liftInitialM :: Monad m => m s -> m (Step s b)
liftInitialM = fmap Partial

{-# INLINE partialM #-}
partialM :: Monad m => s -> m (Step s b)
partialM = return . Partial

{-# INLINE doneM #-}
doneM :: Monad m => b -> m (Step s b)
doneM = return . Done

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
simplify (Fold2 step inject extract) c =
    Fold (\x a -> Partial <$> step x a) (inject c) extract

-- | Maps a function on the output of the fold (the type @b@).
instance Monad m => Functor (Fold m a) where
    {-# INLINE fmap #-}
    fmap f (Fold step start done) = Fold step' start done'
        where
        step' x a = do
            res <- step x a
            case res of
                Partial s -> partialM s
                Done b -> doneM (f b)
        done' x = fmap f $! done x

-- | The fold resulting from '<*>' distributes its input to both the argument
-- folds and combines their output using the supplied function.
instance Monad m => Applicative (Fold m a) where
    {-# INLINE pure #-}
    pure b = Fold (\() _ -> pure $ Done b) (pure ()) (\() -> pure b)
    {-# INLINE (<*>) #-}
    (Fold stepL beginL doneL) <*> (Fold stepR beginR doneR) =
        let combine (Done dL) (Done dR) = Done $ dL dR
            combine sl sr = Partial $ Tuple' sl sr
            step (Tuple' xL xR) a =
                combine <$> liftStep stepL xL a <*> liftStep stepR xR a
            begin = Tuple' <$> liftInitialM beginL <*> liftInitialM beginR
            done (Tuple' xL xR) = liftExtract doneL xL <*> liftExtract doneR xR
         in Fold step begin done


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
toListRevF = Fold (\xs x -> partialM $ x:xs) (return []) return

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
    step' x a = if f a then step x a else partialM x

-- | Like 'lfilter' but with a monadic predicate.
--
-- @since 0.7.0
{-# INLINABLE lfilterM #-}
lfilterM :: Monad m => (a -> m Bool) -> Fold m a r -> Fold m a r
lfilterM f (Fold step begin done) = Fold step' begin done
  where
    step' x a = do
      use <- f a
      if use then step x a else partialM x

-- | Transform a fold from a pure input to a 'Maybe' input, consuming only
-- 'Just' values.
{-# INLINE lcatMaybes #-}
lcatMaybes :: Monad m => Fold m a b -> Fold m (Maybe a) b
lcatMaybes = lfilter isJust . lmap fromJust

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- XXX These should become terminating folds.
--
-- | Take first @n@ elements from the stream and discard the rest.
--
-- @since 0.7.0
{-# INLINE ltake #-}
ltake :: Monad m => Int -> Fold m a b -> Fold m a b
ltake n (Fold step initial done) = Fold step' initial' done'
    where
    initial' = fmap (Tuple' 0) initial
    step' (Tuple' i r) a =
        if i < n
        then do
            res <- step r a
            case res of
                Partial s -> partialM $ Tuple' (i + 1) s
                Done b -> doneM b
        else Done <$> done r
    done' (Tuple' _ r) = done r

-- | Takes elements from the input as long as the predicate succeeds.
--
-- @since 0.7.0
{-# INLINABLE ltakeWhile #-}
ltakeWhile :: Monad m => (a -> Bool) -> Fold m a b -> Fold m a b
ltakeWhile predicate (Fold step initial done) = Fold step' initial done
    where
    step' r a =
        if predicate a
        then step r a
        else Done <$> done r

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
-- XXX Is this correct?
{-# INLINABLE duplicate #-}
duplicate :: Monad m => Fold m a b -> Fold m a (Fold m a b)
duplicate (Fold step begin done) =
    Fold step' begin (\x -> pure (Fold step (pure x) done))
    where
      step' x a = do
          res <- step x a
          case res of
              Partial s -> pure $ Partial s
              Done _ -> pure $ Done $ Fold step (pure x) done

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
    r <- step i a
    case r of
        Partial s -> return $ Fold step (return s) extract
        Done b -> return $ Fold (\_ _ -> doneM b) (return i) (\_ -> return b)


------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- XXX These can be expressed using foldChunks repeatedly on the input of a
-- fold.

-- | For every n input items, apply the first fold and supply the result to the
-- next fold.
--
{-# INLINE lchunksOf #-}
lchunksOf :: Monad m => Int -> Fold m a b -> Fold m b c -> Fold m a c
lchunksOf n (Fold step1 initial1 extract1) (Fold step2 initial2 extract2) =
    Fold step' initial' extract'

    where

    initial' = Tuple3' 0 <$> liftInitialM initial1 <*> liftInitialM initial2
    step' (Tuple3' i r1 r2) a =
        if i < n
        then do
            res <- liftStep step1 r1 a
            partialM $ Tuple3' (i + 1) res r2
        else do
            res <- liftExtract extract1 r1
            acc2 <- liftStep step2 r2 res
            case acc2 of
                Done b -> doneM b
                Partial _ -> do
                    i1 <- initial1
                    acc1 <- step1 i1 a
                    partialM $ Tuple3' 1 acc1 acc2
    extract' (Tuple3' _ r1 r2) = do
        res <- liftExtract extract1 r1
        acc2 <- liftStep step2 r2 res
        liftExtract extract2 acc2

{-# INLINE lchunksOf2 #-}
lchunksOf2 :: Monad m => Int -> Fold m a b -> Fold2 m x b c -> Fold2 m x a c
lchunksOf2 n (Fold step1 initial1 extract1) (Fold2 step2 inject2 extract2) =
    Fold2 step' inject' extract'

    where

    inject' x = Tuple3' 0 <$> liftInitialM initial1 <*> inject2 x
    step' (Tuple3' i r1 r2) a =
        if i < n
        then do
            res <- liftStep step1 r1 a
            return $ Tuple3' (i + 1) res r2
        else do
            res <- liftExtract extract1 r1
            acc2 <- step2 r2 res

            i1 <- initial1
            acc1 <- step1 i1 a
            return $ Tuple3' 1 acc1 acc2
    extract' (Tuple3' _ r1 r2) = do
        res <- liftExtract extract1 r1
        acc2 <- step2 r2 res
        extract2 acc2

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
-- XXX Should we check for mv2 at each step?
{-# INLINE lsessionsOf #-}
lsessionsOf :: MonadAsync m => Double -> Fold m a b -> Fold m b c -> Fold m a c
lsessionsOf n (Fold step1 initial1 extract1) (Fold step2 initial2 extract2) =
    Fold step' initial' extract'

    where

    -- XXX MVar may be expensive we need a cheaper synch mechanism here
    initial' = do
        i1 <- liftInitialM initial1
        i2 <- liftInitialM initial2
        mv1 <- liftIO $ newMVar i1
        mv2 <- liftIO $ newMVar (Right i2)
        t <- control $ \run ->
            mask $ \restore -> do
                tid <- forkIO $ catch (restore $ void $ run (timerThread mv1 mv2))
                                      (handleChildException mv2)
                run (return tid)
        return $ Tuple3' t (Partial mv1) mv2
    step' acc@(Tuple3' t (Partial mv1) mv2) a = do
            r1 <- liftIO $ takeMVar mv1
            res <- liftStep step1 r1 a
            liftIO $ putMVar mv1 res
            case res of
                Partial _ -> partialM acc
                Done _ -> partialM $ Tuple3' t (Done mv1) mv2
    step' acc@(Tuple3' _ (Done _) _) _ = partialM acc
    extract' (Tuple3' tid _ mv2) = do
        r2 <- liftIO $ takeMVar mv2
        liftIO $ killThread tid
        case r2 of
            Left e -> throwM e
            Right x -> liftExtract extract2 x

    timerThread mv1 mv2 = do
        liftIO $ threadDelay (round $ n * 1000000)

        r1 <- liftIO $ takeMVar mv1
        i1 <- liftInitialM initial1
        liftIO $ putMVar mv1 i1

        res1 <- liftExtract extract1 r1
        r2 <- liftIO $ takeMVar mv2
        case r2 of
            Left _ -> liftIO $ putMVar mv2 r2
            Right x -> do
                res <- liftStep step2 x res1
                case res of
                    Partial _ -> do
                        liftIO $ putMVar mv2 $ Right res
                        timerThread mv1 mv2
                    Done _ -> liftIO $ putMVar mv2 $ Right res

    handleChildException ::
        MVar (Either SomeException a) -> SomeException -> IO ()
    handleChildException mv2 e = do
        r2 <- takeMVar mv2
        let r = case r2 of
                    Left _ -> r2
                    Right _ -> Left e
        putMVar mv2 r
