{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Pipe.Types
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Pipe.Types
    ( Step (..)
    , Pipe (..)
    , PipeState (..)
    {-
    , zipWith
    , tee
    -}
    , map
    , compose
    )
where

import Control.Arrow (Arrow(..))
import Data.Maybe (isJust)
import Data.Semigroup (Semigroup(..))
import Prelude hiding (zipWith, map, unzip, null)
import Streamly.Strict (Tuple'(..))

import qualified Prelude
import qualified Control.Category as Cat

------------------------------------------------------------------------------
-- Notes
------------------------------------------------------------------------------

-- A scan is a much simpler version of pipes. A scan always produces an output
-- on an input whereas a pipe does not necessarily produce an output on an
-- input, it might consume multiple inputs before producing an output. That way
-- it can implement filtering. Similarly, it can produce more than one output
-- on an single input.
--
-- Therefore when two pipes are composed in parallel formation, one may run
-- slower or faster than the other. If all of them are being fed from the same
-- source, we may have to buffer the input to match the speeds. In case of
-- scans we do not have that problem.
--
-- We can upgrade a stream or a fold into a pipe. However, streams are more
-- efficient for generation and folds are more efficient for consumption.
--
-- For pure transformation we can have a 'Scan' type. A Scan would be more
-- efficient in zipping whereas pipes are useful for merging and zipping where
-- we know buffering can occur. A Scan type can be upgraded to a pipe.
--
------------------------------------------------------------------------------
-- Pipes
------------------------------------------------------------------------------

-- | The result yielded by running a single consume or produce step of the
-- pipe.
data Step s a =
      Yield a s    -- Yield value 'a' with the next state 's'
    | Continue s   -- Yields no value, the next state is 's'
    | Stop         -- The pipe is closed for input or output

instance Functor (Step s) where
    fmap f step =
        case step of
            Yield x s -> Yield (f x) s
            Continue s -> Continue s
            Stop -> Stop

-- An explicit either type for better readability of the code
data PipeState s1 s2 = Consume !s1 | Produce !s2

isProduce :: PipeState s1 s2 -> Bool
isProduce s =
    case s of
        Produce _ -> True
        Consume _ -> False

-- XXX we can possibly use a single state type s instead of s1/s2. That would
-- be simpler but weakly typed.
--
-- | A 'Pipe' represents a stateful transformation over an input stream of
-- values of type @a@ to outputs of type @b@ in 'Monad' @m@. The 'Pipe'
-- consists of an initial consumption/producer state 's', a consume function,
-- produce function and a finalize function to indicate that we no longer want
-- to feed any more input to the pipe.
--
data Pipe m a b =
  forall s1 s2. Pipe
    (PipeState s1 s2)                           -- initial state
    (s1 -> a -> m (Step (PipeState s1 s2) b))   -- consume
    (s2      -> m (Step (PipeState s1 s2) b))   -- produce
    (PipeState s1 s2 -> PipeState s1 s2)        -- finalize

-- | Maps a function on the output of the pipe.
instance Monad m => Functor (Pipe m a) where
    {-# INLINE_NORMAL fmap #-}
    fmap f (Pipe initial consume produce finalize) =
        Pipe initial consume' produce' finalize
        where
        {-# INLINE_LATE consume' #-}
        consume' st a = consume st a >>= return . fmap f
        {-# INLINE_LATE produce' #-}
        produce' st   = produce st   >>= return . fmap f

{-
-- XXX move this to a separate module
data Deque a = Deque [a] [a]

{-# INLINE null #-}
null :: Deque a -> Bool
null (Deque [] []) = True
null _ = False

{-# INLINE snoc #-}
snoc :: a -> Deque a -> Deque a
snoc a (Deque snocList consList) = Deque (a : snocList) consList

{-# INLINE uncons #-}
uncons :: Deque a -> Maybe (a, Deque a)
uncons (Deque snocList consList) =
  case consList of
    h : t -> Just (h, Deque snocList t)
    _ ->
      case Prelude.reverse snocList of
        h : t -> Just (h, Deque [] t)
        _ -> Nothing

-- | The composed pipe distributes the input to both the constituent pipes and
-- zips the output of the two using a supplied zipping function.
--
-- @since 0.7.0
{-# INLINE_NORMAL zipWith #-}
zipWith :: Monad m => (a -> b -> c) -> Pipe m i a -> Pipe m i b -> Pipe m i c
zipWith f (Pipe consumeL produceL stateL) (Pipe consumeR produceR stateR) =
                    Pipe consume produce state
        where

        -- Left state means we need to consume input from the source. A Right
        -- state means we either have buffered input or we are in generation
        -- mode so we do not need input from source in either case.
        --
        state = Tuple' (Consume stateL, Nothing, Nothing)
                       (Consume stateR, Nothing, Nothing)

        -- XXX for heavy buffering we need to have the (ring) buffer in pinned
        -- memory using the Storable instance.
        {-# INLINE_LATE consume #-}
        consume (Tuple' (sL, resL, lq) (sR, resR, rq)) a = do
            s1 <- drive sL resL lq consumeL produceL a
            s2 <- drive sR resR rq consumeR produceR a
            case (s1,s2) of
                (Just s1', Just s2') -> yieldOutput s1' s2'
                _ -> return Stop

            where

            {-# INLINE drive #-}
            drive st res queue fConsume fProduce val = do
                case res of
                    Nothing -> goConsume st queue val fConsume fProduce
                    Just x -> return $
                        case queue of
                            Nothing -> Just (st, Just x, Just $ (Deque [val] []))
                            Just q  -> Just (st, Just x, Just $ snoc val q)

            {-# INLINE goConsume #-}
            goConsume stt queue val fConsume stp2 = do
                case stt of
                    Consume st -> do
                        case queue of
                            Nothing -> do
                                r <- fConsume st val
                                return $ case r of
                                    Yield x s  -> Just (s, Just x, Nothing)
                                    Continue s -> Just (s, Nothing, Nothing)
                                    Stop -> Nothing
                            Just queue' ->
                                case uncons queue' of
                                    Just (v, q) -> do
                                        r <- fConsume st v
                                        let q' = snoc val q
                                        return $ case r of
                                            Yield x s  -> Just (s, Just x, Just q')
                                            Continue s -> Just (s, Nothing, Just q')
                                            Stop -> Nothing
                                    Nothing -> undefined -- never occurs
                    Produce st -> do
                        r <- stp2 st
                        return $ case r of
                            Yield x s  -> Just (s, Just x, queue)
                            Continue s -> Just (s, Nothing, queue)
                            Stop -> Nothing

        {-# INLINE_LATE produce #-}
        produce (Tuple' (sL, resL, lq) (sR, resR, rq)) = do
            s1 <- drive sL resL lq consumeL produceL
            s2 <- drive sR resR rq consumeR produceR
            case (s1,s2) of
                (Just s1', Just s2') -> yieldOutput s1' s2'
                _ -> return Stop

            where

            {-# INLINE drive #-}
            drive stt res q fConsume fProduce = do
                case res of
                    Nothing -> goProduce stt q fConsume fProduce
                    Just x -> return $ Just (stt, Just x, q)

            {-# INLINE goProduce #-}
            goProduce stt queue fConsume fProduce = do
                case stt of
                    Consume st -> do
                        case queue of
                            -- See yieldOutput. We enter produce mode only when
                            -- each pipe is either in Produce state or the
                            -- queue is non-empty. So this case cannot occur.
                            Nothing -> undefined
                            Just queue' ->
                                case uncons queue' of
                                    Just (v, q) -> do
                                        r <- fConsume st v
                                        -- We provide a guarantee that if the
                                        -- queue is "Just" it is always
                                        -- non-empty. yieldOutput and goConsume
                                        -- depend on it.
                                        let q' = if null q
                                                 then Nothing
                                                 else Just q
                                        return $ case r of
                                            Yield x s  -> Just (s, Just x, q')
                                            Continue s -> Just (s, Nothing, q')
                                            Stop -> Nothing
                                    Nothing -> return $ Just (stt, Nothing, Nothing)
                    Produce st -> do
                        r <- fProduce st
                        return $ case r of
                            Yield x s  -> Just (s, Just x, queue)
                            Continue s -> Just (s, Nothing, queue)
                            Stop -> Nothing

        {-# INLINE yieldOutput #-}
        yieldOutput s1@(sL', resL', lq') s2@(sR', resR', rq') = return $
            -- switch to produce mode if we do not need input
            if (isProduce sL' || isJust lq') && (isProduce sR' || isJust rq')
            then
                case (resL', resR') of
                    (Just xL, Just xR) ->
                        Yield (f xL xR) (Produce (Tuple' (clear s1) (clear s2)))
                    _ -> Continue (Produce (Tuple' s1 s2))
            else
                case (resL', resR') of
                    (Just xL, Just xR) ->
                        Yield (f xL xR) (Consume (Tuple' (clear s1) (clear s2)))
                    _ -> Continue (Consume (Tuple' s1 s2))
            where clear (s, _, q) = (s, Nothing, q)

instance Monad m => Applicative (Pipe m a) where
    {-# INLINE pure #-}
    pure b = Pipe (\_ _ -> pure $ Yield b (Consume ())) undefined ()

    (<*>) = zipWith id

-- XXX It is also possible to compose in a way so as to append the pipes after
-- distributing the input to them, but that will require full buffering of the
-- input.

data TeeConsume sL sR =
      TCBoth !sL !sR
    | TCLeft !sL
    | TCRight !sR

data TeeProduce a s sLc sLp sRp sRc =
      TPLeft a s !sRc
    | TPRight !sLc !sRp
    | TPSwitchRightOnly a !sRc
    | TPRightOnly !sRp
    | TPLeftOnly !sLp

-- | The composed pipe distributes the input to both the constituent pipes and
-- merges the outputs of the two.
--
-- @since 0.7.0
{-# INLINE_NORMAL tee #-}
tee :: Monad m => Pipe m a b -> Pipe m a b -> Pipe m a b
tee (Pipe consumeL produceL stateL) (Pipe consumeR produceR stateR) =
        Pipe consume produce state
    where

    state = TCBoth stateL stateR

    -- At the start both pipes are in Consume mode.
    --
    -- We start with the left pipe.  If either of the pipes goes in produce
    -- mode then the tee goes to produce mode. After one pipe finishes
    -- producing all outputs for a given input only then we move on to the next
    -- pipe.
    consume (TCBoth sL sR) a = do
        r <- consumeL sL a
        return $ case r of
            Yield x s  -> Yield x  (Produce (TPLeft a s sR))
            Continue s -> Continue (Produce (TPLeft a s sR))
            Stop       -> Continue (Produce (TPSwitchRightOnly a sR))

    -- Right pipe has stopped, only the left pipe is running
    consume (TCLeft sL) a = do
        r <- consumeL sL a
        return $ case r of
            Yield x (Consume s)  -> Yield x  (Consume (TCLeft s))
            Yield x (Produce s)  -> Yield x  (Produce (TPLeftOnly s))
            Continue (Consume s) -> Continue (Consume (TCLeft s))
            Continue (Produce s) -> Continue (Produce (TPLeftOnly s))
            Stop                 -> Stop

    consume (TCRight sR) a = do
        r <- consumeR sR a
        return $ case r of
            Yield x (Consume s)  -> Yield x  (Consume (TCRight s))
            Yield x (Produce s)  -> Yield x  (Produce (TPRightOnly s))
            Continue (Consume s) -> Continue (Consume (TCRight s))
            Continue (Produce s) -> Continue (Produce (TPRightOnly s))
            Stop                 -> Stop

    -- Left pipe went to produce mode and right pipe is waiting for its turn.
    produce (TPLeft a (Produce sL) sR) = do
        r <- produceL sL
        return $ case r of
            Yield x s  -> Yield x  (Produce (TPLeft a s sR))
            Continue s -> Continue (Produce (TPLeft a s sR))
            Stop       -> Continue (Produce (TPSwitchRightOnly a sR))

    -- Left pipe is done consuming an input, both pipes are again in consume
    -- mode and its Right pipe's turn to consume the buffered input now.
    produce (TPLeft a (Consume sL) sR) = do
        r <- consumeR sR a
        return $ case r of
            Yield x  (Consume s) -> Yield x  (Consume (TCBoth  sL s))
            Yield x  (Produce s) -> Yield x  (Produce (TPRight sL s))
            Continue (Consume s) -> Continue (Consume (TCBoth  sL s))
            Continue (Produce s) -> Continue (Produce (TPRight sL s))
            Stop                 -> Continue (Consume (TCLeft  sL))

    -- Left pipe has stopped, we have to continue with just the right pipe.
    produce (TPSwitchRightOnly a sR) = do
        r <- consumeR sR a
        return $ case r of
            Yield x  (Consume s) -> Yield x  (Consume (TCRight s))
            Yield x  (Produce s) -> Yield x  (Produce (TPRightOnly s))
            Continue (Consume s) -> Continue (Consume (TCRight s))
            Continue (Produce s) -> Continue (Produce (TPRightOnly s))
            Stop                 -> Stop

    -- Left pipe has consumed and produced, right pipe has consumed and is now
    -- in produce mode.
    produce (TPRight sL sR) = do
        r <- produceR sR
        return $ case r of
            Yield x  (Consume s) -> Yield x  (Consume (TCBoth sL s))
            Yield x  (Produce s) -> Yield x  (Produce (TPRight sL s))
            Continue (Consume s) -> Continue (Consume (TCBoth sL s))
            Continue (Produce s) -> Continue (Produce (TPRight sL s))
            Stop                 -> Continue (Consume (TCLeft sL))

    -- Left pipe has stopped and right pipe is in produce mode.
    produce (TPRightOnly sR) = do
        r <- produceR sR
        return $ case r of
            Yield x  (Consume s) -> Yield x  (Consume (TCRight s))
            Yield x  (Produce s) -> Yield x  (Produce (TPRightOnly s))
            Continue (Consume s) -> Continue (Consume (TCRight s))
            Continue (Produce s) -> Continue (Produce (TPRightOnly s))
            Stop                 -> Stop

    -- Right pipe has stopped and left pipe is in produce mode.
    produce (TPLeftOnly sL) = do
        r <- produceL sL
        return $ case r of
            Yield x  (Consume s) -> Yield x  (Consume (TCLeft s))
            Yield x  (Produce s) -> Yield x  (Produce (TPLeftOnly s))
            Continue (Consume s) -> Continue (Consume (TCLeft s))
            Continue (Produce s) -> Continue (Produce (TPLeftOnly s))
            Stop                 -> Stop

instance Monad m => Semigroup (Pipe m a b) where
    {-# INLINE (<>) #-}
    (<>) = tee
-}

-- | Lift a pure function to a 'Pipe'.
--
-- @since 0.7.0
{-# INLINE map #-}
map :: Monad m => (a -> b) -> Pipe m a b
map f = Pipe (Consume ()) consume undefined id
    where
    consume _ a = return $ Yield (f a) (Consume ())

{-
-- | A hollow or identity 'Pipe' passes through everything that comes in.
--
-- @since 0.7.0
{-# INLINE id #-}
id :: Monad m => Pipe m a a
id = map Prelude.id
-}

-- First 's' is for "State", second L/R are for left/right, third c/p are for
-- consume/produce.
data ComposeConsume sLc sRc = ComposeConsumeCC !sLc !sRc

data ComposeProduce x sLc sRc sLp sRp =
      ComposeProduceCPx x !sLc !sRp
    | ComposeProduceCPxFinal x !sLc !sRp
    | ComposeProduceCCx x !sLc !sRc
    | ComposeProduceCCxFinal x !sLc !sRc
    | ComposeProducePP !sLp !sRp
    | ComposeProduceCP !sLc !sRp
    | ComposeProducePC !sLp !sRc

-- | Compose two pipes such that the output of the second pipe is attached to
-- the input of the first pipe.
--
-- @since 0.7.0
{-# INLINE_NORMAL compose #-}
compose :: Monad m => Pipe m b c -> Pipe m a b -> Pipe m a c
compose (Pipe initialL consumeL produceL finalizeL)
        (Pipe initialR consumeR produceR finalizeR) =
    Pipe state consume produce finalize

    where

    mkState (s1, s2) = case (s1, s2) of
        (Consume stateL, Consume stateR) ->
            Consume (ComposeConsumeCC stateL stateR)
        _ -> undefined

    state = mkState (initialL, initialR)

    -- flush the internal state and finalize the streams.
    -- finalize cur@(Consume (ComposeConsumeCC sL sR)) =
    finalize cur =
        case cur of
            (Consume (ComposeConsumeCC l r)) -> finalizeState (Consume l) (Consume r)
            (Produce (ComposeProducePP l r)) -> finalizeState (Produce l) (Produce r)
            (Produce (ComposeProduceCP l r)) -> finalizeState (Consume l) (Produce r)
            (Produce (ComposeProducePC l r)) -> finalizeState (Produce l) (Consume r)
            (Produce (ComposeProduceCPx x l r)) -> (Produce (ComposeProduceCPxFinal x l r))
            (Produce (ComposeProduceCCx x l r)) -> (Produce (ComposeProduceCCxFinal x l r))
            (Produce (ComposeProduceCPxFinal _ _ _)) -> undefined
            (Produce (ComposeProduceCCxFinal _ _ _)) -> undefined

    finalizeState l r =
        case (finalizeL l, finalizeR r) of
            (Consume sL', Consume sR') -> (Consume (ComposeConsumeCC sL' sR'))
            (Consume sL', Produce sR') -> (Produce (ComposeProduceCP sL' sR'))
            (Produce sL', Consume sR') -> (Produce (ComposeProducePC sL' sR'))
            (Produce sL', Produce sR') -> (Produce (ComposeProducePP sL' sR'))

    -- Both pipes are in Consume state.
    consume (ComposeConsumeCC sL sR) a = do
        r <- consumeR sR a
        return $ case r of
            Yield x  (Consume s) -> Continue (Produce $ ComposeProduceCCx x sL s)
            Yield x  (Produce s) -> Continue (Produce $ ComposeProduceCPx x sL s)
            Continue (Consume s) -> Continue (Consume $ ComposeConsumeCC sL s)
            Continue (Produce s) -> Continue (Produce $ ComposeProduceCP sL s)
            Stop                 -> Stop

    -- left consume, right produce
    produce (ComposeProduceCPx a sL sR) = do
        r <- consumeL sL a
        return $ case r of
            Yield x  (Consume s) -> Yield x  (Produce $ ComposeProduceCP s sR)
            Yield x  (Produce s) -> Yield x  (Produce $ ComposeProducePP s sR)
            Continue (Consume s) -> Continue (Produce $ ComposeProduceCP s sR)
            Continue (Produce s) -> Continue (Produce $ ComposeProducePP s sR)
            Stop                 -> Stop

    produce (ComposeProduceCPxFinal a sL sR) = do
        r <- consumeL sL a
        return $ case r of
            Yield x  (Consume s) -> Yield x  (finalize $ Produce $ ComposeProduceCP s sR)
            Yield x  (Produce s) -> Yield x  (finalize $ Produce $ ComposeProducePP s sR)
            Continue (Consume s) -> Continue (finalize $ Produce $ ComposeProduceCP s sR)
            Continue (Produce s) -> Continue (finalize $ Produce $ ComposeProducePP s sR)
            Stop                 -> Stop

    -- left consume, right consume
    produce (ComposeProduceCCx a sL sR) = do
        r <- consumeL sL a
        return $ case r of
            Yield x  (Consume s) -> Yield x  (Consume $ ComposeConsumeCC s sR)
            Yield x  (Produce s) -> Yield x  (Produce $ ComposeProducePC s sR)
            Continue (Consume s) -> Continue (Consume $ ComposeConsumeCC s sR)
            Continue (Produce s) -> Continue (Produce $ ComposeProducePC s sR)
            Stop                 -> Stop

    produce (ComposeProduceCCxFinal a sL sR) = do
        r <- consumeL sL a
        return $ case r of
            Yield x  (Consume s) -> Yield x  (finalize $ Consume $ ComposeConsumeCC s sR)
            Yield x  (Produce s) -> Yield x  (finalize $ Produce $ ComposeProducePC s sR)
            Continue (Consume s) -> Continue (finalize $ Consume $ ComposeConsumeCC s sR)
            Continue (Produce s) -> Continue (finalize $ Produce $ ComposeProducePC s sR)
            Stop                 -> Stop

    -- left consume, right produce
    produce (ComposeProduceCP sL sR) = do
        r <- produceR sR
        return $ case r of
            Yield x  (Consume s) -> Continue (Produce $ ComposeProduceCCx x sL s)
            Yield x  (Produce s) -> Continue (Produce $ ComposeProduceCPx x sL s)
            Continue (Consume s) -> Continue (Consume $ ComposeConsumeCC sL s)
            Continue (Produce s) -> Continue (Produce $ ComposeProduceCP sL s)
            Stop                 -> Stop

    -- left produce, right produce
    produce (ComposeProducePP sL sR) = do
        r <- produceL sL
        return $ case r of
            Yield x  (Consume s) -> Yield x  (Produce $ ComposeProduceCP s sR)
            Yield x  (Produce s) -> Yield x  (Produce $ ComposeProducePP s sR)
            Continue (Consume s) -> Continue (Produce $ ComposeProduceCP s sR)
            Continue (Produce s) -> Continue (Produce $ ComposeProducePP s sR)
            Stop                 -> Stop

    -- left produce, right consume
    produce (ComposeProducePC sL sR) = do
        r <- produceL sL
        return $ case r of
            Yield x  (Consume s) -> Yield x  (Consume $ ComposeConsumeCC s sR)
            Yield x  (Produce s) -> Yield x  (Produce $ ComposeProducePC s sR)
            Continue (Consume s) -> Continue (Consume $ ComposeConsumeCC s sR)
            Continue (Produce s) -> Continue (Produce $ ComposeProducePC s sR)
            Stop                 -> Stop

instance Monad m => Cat.Category (Pipe m) where
    {-# INLINE id #-}
    id = map Prelude.id

    {-# INLINE (.) #-}
    (.) = compose

{-
unzip :: Pipe m a x -> Pipe m b y -> Pipe m (a, b) (x, y)
unzip = undefined

instance Monad m => Arrow (Pipe m) where
    {-# INLINE arr #-}
    arr = map

    {-# INLINE (***) #-}
    (***) = unzip

    {-# INLINE (&&&) #-}
    (&&&) = zipWith (,)
    -}
