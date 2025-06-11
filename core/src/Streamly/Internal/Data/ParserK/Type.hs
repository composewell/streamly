-- |
-- Module      : Streamly.Internal.Data.Parser.ParserK.Type
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- CPS style implementation of parsers.
--
-- The CPS representation allows linear performance for Applicative, sequence,
-- Monad, Alternative, and choice operations compared to the quadratic
-- complexity of the corresponding direct style operations. However, direct
-- style operations allow fusion with ~10x better performance than CPS.
--
-- The direct style representation does not allow for recursive definitions of
-- "some" and "many" whereas CPS allows that.
--
-- 'Applicative' and 'Control.Applicative.Alternative' type class based
-- combinators from the
-- <http://hackage.haskell.org/package/parser-combinators parser-combinators>
-- package can also be used with the 'ParserK' type.

module Streamly.Internal.Data.ParserK.Type
    (
      Step (..)
    , Input (..)
    , ParseResult (..)
    , ParserK (..)
    , parserK
    , toParser -- XXX unParserK, unK, unPK
    , fromPure
    , fromEffect
    , die

    , parserDone

    -- * Deprecated
    , adapt
    )
where

#include "ArrayMacros.h"
#include "assert.hs"
#include "deprecation.h"
#include "inline.hs"

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.Trans.Class (MonadTrans(lift))
import GHC.Types (SPEC(..))

import qualified Control.Monad.Fail as Fail
import qualified Streamly.Internal.Data.Parser.Type as ParserD

-------------------------------------------------------------------------------
-- Developer Notes
-------------------------------------------------------------------------------

-- MonadReader cannot be implemented using continuations for ParserK
--
-- "local" (and hence "MonadReader") cannot be implemented for ParserK because
-- there is no way to override all continuations.
--
-- We can implement `MonadReader` for ParserK via ParserD:
--
-- @
-- instance (Show r, MonadReader r m) => MonadReader r (Parser a m) where
--     {-# INLINE ask #-}
--     ask = Parser.fromEffect ask
--     {-# INLINE local #-}
--     local f (Parser step initial extract) =
--         Parser
--             ((local f .) . step)
--             (local f initial)
--             (local f . extract)
--
-- instance (Show r, MonadReader r m) => MonadReader r (ParserK a m) where
--     {-# INLINE ask #-}
--     ask = ParserK.fromEffect ask
--     {-# INLINE local #-}
--     local f parser = ParserK.adapt $ local f $ ParserK.toParser parser
-- @

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- Note: We cannot use an Array directly as input because we need to identify
-- the end of input case using None. We cannot do that using nil Array as nil
-- Arrays can be encountered in normal input as well.
--
-- We could specialize the ParserK type to use an Array directly, that provides
-- some performance improvement. The best advantage of that is when we consume
-- one element at a time from the array. If we really want that perf
-- improvement we can use a special ParserK type with the following Input.
--
-- data Input a = None | Chunk {-# UNPACK #-} !(Array a)
--
-- XXX Rename Chunk to Some.
data Input a = None | Chunk a

-- Note: Step should ideally be called StepResult and StepParser should be just
-- Step, but then it will not be consistent with Parser/Stream.

-- Using "Input" in runParser is not necessary but it avoids making
-- one more function call to get the input. This could be helpful
-- for cases where we process just one element per call.

-- | A parsing function that parses a single input object.
type StepParser a m r = Input a -> m (Step a m r)

-- | The intermediate result of running a parser step. The parser driver may
-- (1) stop with a final result ('Done') with no more inputs to be accepted,
-- (2) generate an intermediate result ('Partial') and accept more inputs, (3)
-- generate no result but wait for more input ('Continue'), (4) or fail with an
-- error ('Error').
--
-- The Int is a count by which the current stream position should be adjusted
-- before calling the next parsing step.
--
-- See the documentation of 'Streamly.Data.Parser.Step' for more details, this
-- has the same semantics.
--
-- /Pre-release/
--
data Step a m r =
      Done !Int r
    | Partial !Int (StepParser a m r)
    | Continue !Int (StepParser a m r)
    | Error !Int String

instance Functor m => Functor (Step a m) where
    fmap f (Done n r) = Done n (f r)
    fmap f (Partial n k) = Partial n (fmap (fmap f) . k)
    fmap f (Continue n k) = Continue n (fmap (fmap f) . k)
    fmap _ (Error n e) = Error n e

-- Note: Passing position index separately instead of passing it with the
-- result causes huge regression in expression parsing becnhmarks.

-- | The parser's result.
--
-- Int is the position index into the current input array. Could be negative.
-- Cannot be beyond the input array max bound.
--
-- /Pre-release/
--
data ParseResult b =
      Success !Int !b      -- Position index, result
    | Failure !Int !String -- Position index, error

-- | Map a function over 'Success'.
instance Functor ParseResult where
    fmap f (Success n b) = Success n (f b)
    fmap _ (Failure n e) = Failure n e

-- XXX Change the type to the shape (a -> m r -> m r) -> (m r -> m r) -> m r
--
-- The parse continuation would be: Array a -> m (Step a m r) -> m (Step a m r)
-- The extract continuation would be: m (Step a m r) -> m (Step a m r)
--
-- Use Step itself in place of ParseResult.

-- | A continuation passing style parser representation. A parser is a
-- continuation of 'Step's, each step passes a state and a parse result to the
-- next 'Step'. The resulting 'Step' may carry a continuation that consumes
-- input 'a' and results in another 'Step'. Essentially, the continuation may
-- either consume input without a result or return a result with no further
-- input to be consumed.
--
-- The first argument of runParser is a continuation to be invoked after the
-- parser is done, it is of the following shape:
--
-- >> ParseResult b -> Int -> StepParser a m r
--
-- First argument of the continuation is the 'ParseResult'. The current stream
-- position is carried as part of the 'Success' or 'Failure' constructors of
-- 'ParseResult'. The second argument of the continuation is a count of the
-- elements used in the current alterantive of an alternative composition, if
-- the alternative fails we need to backtrack by this amount before invoking
-- the next alternative.
--
-- The second argument of runParser is the incoming stream position adjustment.
-- The parser needs to adjust the current position of the stream by this amount
-- before consuming any input. A positive value means move forward by that much
-- in the stream and a negative value means backward. See the 'Step' and
-- 'Streamly.Data.Parser.Step' documentation for more details.
--
-- The third argument is the incoming cumulative used element count for the
-- current alternative, same as described for the continuation above.
--
newtype ParserK a m b = MkParser
    { runParser :: forall r.
           -- Do not eta reduce the applications of this continuation.
           -- Continuation to be invoked after the parser is done
           (ParseResult b -> Int -> StepParser a m r)
           -- XXX Maintain and pass the original position in the stream. that
           -- way we can also report better errors. Use a Context structure for
           -- passing the state.
           --
           -- stream position adjustment before the parser starts.
        -> Int
           -- initial used count for the current alternative.
        -> Int
            -- final parse result, when the last continuation is done.
        -> StepParser a m r
    }

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

-- XXX rewrite this using ParserD, expose rmapM from ParserD.

-- | Map a function on the result i.e. on @b@ in @Parser a m b@.
instance Functor m => Functor (ParserK a m) where
    {-# INLINE fmap #-}
    fmap f parser = MkParser $ \k pos used inp ->
        let k1 res = k (fmap f res)
         in runParser parser k1 pos used inp

-------------------------------------------------------------------------------
-- Sequential applicative
-------------------------------------------------------------------------------

-- This is the dual of stream "fromPure".

-- | A parser that always yields a pure value without consuming any input.
--
-- /Pre-release/
--
{-# INLINE fromPure #-}
fromPure :: b -> ParserK a m b
fromPure b = MkParser $ \k pos used inp -> k (Success pos b) used inp

-- | See 'Streamly.Internal.Data.Parser.fromEffect'.
--
-- /Pre-release/
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> ParserK a m b
fromEffect eff =
    MkParser $ \k pos used inp -> eff >>= \b -> k (Success pos b) used inp

-- | @f \<$> p1 \<*> p2@ applies parsers p1 and p2 sequentially to an input
-- stream. The first parser runs and processes the input, the remaining input
-- is then passed to the second parser. If both parsers succeed, their outputs
-- are applied to the function @f@. If either parser fails, the operation
-- fails.
--
instance Monad m => Applicative (ParserK a m) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = ap

    {-# INLINE (*>) #-}
    p1 *> p2 = MkParser $ \k pos used input ->
        let k1 (Success pos1 _) u inp = runParser p2 k pos1 u inp
            k1 (Failure pos1 e) u inp = k (Failure pos1 e) u inp
        in runParser p1 k1 pos used input

    {-# INLINE (<*) #-}
    p1 <* p2 = MkParser $ \k pos used input ->
        let k1 (Success pos1 b) u1 inp =
                let k2 (Success pos2 _) u2 inp2 = k (Success pos2 b) u2 inp2
                    k2 (Failure pos2 e) u2 inp2 = k (Failure pos2 e) u2 inp2
                in runParser p2 k2 pos1 u1 inp
            k1 (Failure pos1 e) u1 inp = k (Failure pos1 e) u1 inp
        in runParser p1 k1 pos used input

    {-# INLINE liftA2 #-}
    liftA2 f p = (<*>) (fmap f p)

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

-- This is the dual of "nil".
--
-- | A parser that always fails with an error message without consuming
-- any input.
--
-- /Pre-release/
--
{-# INLINE die #-}
die :: String -> ParserK a m b
die err = MkParser (\k pos used inp -> k (Failure pos err) used inp)

-- | Monad composition can be used for lookbehind parsers, we can dynamically
-- compose new parsers based on the results of the previously parsed values.
instance Monad m => Monad (ParserK a m) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    p >>= f = MkParser $ \k pos used input ->
        let k1 (Success pos1 b) u1 inp = runParser (f b) k pos1 u1 inp
            k1 (Failure pos1 e) u1 inp = k (Failure pos1 e) u1 inp
         in runParser p k1 pos used input

    {-# INLINE (>>) #-}
    (>>) = (*>)

#if !(MIN_VERSION_base(4,13,0))
    -- This is redefined instead of just being Fail.fail to be
    -- compatible with base 4.8.
    {-# INLINE fail #-}
    fail = die
#endif
instance Monad m => Fail.MonadFail (ParserK a m) where
    {-# INLINE fail #-}
    fail = die

instance MonadIO m => MonadIO (ParserK a m) where
    {-# INLINE liftIO #-}
    liftIO = fromEffect . liftIO

-------------------------------------------------------------------------------
-- Alternative
-------------------------------------------------------------------------------

-- | @p1 \<|> p2@ passes the input to parser p1, if it succeeds, the result is
-- returned. However, if p1 fails, the parser driver backtracks and tries the
-- same input on the alternative parser p2, returning the result if it
-- succeeds.
--
instance Monad m => Alternative (ParserK a m) where
    {-# INLINE empty #-}
    empty = die "empty"

    {-# INLINE (<|>) #-}
    p1 <|> p2 = MkParser $ \k pos _ input ->
        let
            k1 (Failure pos1 _) used inp = runParser p2 k (pos1 - used) 0 inp
            k1 success _ inp = k success 0 inp
        in runParser p1 k1 pos 0 input

    -- some and many are implemented here instead of using default definitions
    -- so that we can use INLINE on them. It gives 50% performance improvement.

    {-# INLINE many #-}
    many v = many_v

        where

        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v

    {-# INLINE some #-}
    some v = some_v

        where

        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v

-- | 'mzero' is same as 'empty', it aborts the parser. 'mplus' is same as
-- '<|>', it selects the first succeeding parser.
--
instance Monad m => MonadPlus (ParserK a m) where
    {-# INLINE mzero #-}
    mzero = die "mzero"

    {-# INLINE mplus #-}
    mplus = (<|>)

{-
instance MonadTrans (ParserK a) where
    {-# INLINE lift #-}
    lift = fromEffect
-}

--------------------------------------------------------------------------------
-- Make a ParserK from Parser
--------------------------------------------------------------------------------

{-# INLINE adaptWith #-}
adaptWith
    :: forall m a s b r. (Monad m)
    => (s -> a -> m (ParserD.Step s b))
    -> m (ParserD.Initial s b)
    -> (s -> m (ParserD.Step s b))
    -> (ParseResult b -> Int -> Input a -> m (Step a m r))
    -> Int
    -> Int
    -> Input a
    -> m (Step a m r)
adaptWith pstep initial extract cont !relPos !usedCount !input = do
    res <- initial
    case res of
        ParserD.IPartial pst -> do
            if relPos == 0
            then
                case input of
                    -- In element parser case chunk is just one element
                    Chunk element -> parseContChunk usedCount pst element
                    None -> parseContNothing usedCount pst
            -- XXX Previous code was using Continue in this case
            else
                -- We consumed previous input, need to fetch the next
                -- input from the driver.
                pure $ Partial relPos (parseCont usedCount pst)
        ParserD.IDone b -> cont (Success relPos b) usedCount input
        ParserD.IError err -> cont (Failure relPos err) usedCount input

    where

    -- XXX We can maintain an absolute position instead of relative that will
    -- help in reporting of error location in the stream.
    {-# NOINLINE parseContChunk #-}
    parseContChunk !count !state x = do
         go SPEC state

        where

        go !_ !pst = do
            r <- pstep pst x
            case r of
                -- Done, call the next continuation
                ParserD.SDone 1 b ->
                    cont (Success 1 b) (count + 1) (Chunk x)
                ParserD.SDone 0 b ->
                    cont (Success 0 b) count (Chunk x)
                ParserD.SDone m b -> -- n > 1
                    let n = 1 - m
                     in cont (Success (1 - n) b) (count + 1 - n) (Chunk x)

                -- Not done yet, return the parseCont continuation
                ParserD.SPartial 1 pst1 ->
                    pure $ Partial 1 (parseCont (count + 1) pst1)
                ParserD.SPartial 0 pst1 ->
                    -- XXX if we recurse we are not dropping backtrack buffer
                    -- on partial.
                    -- XXX recurse or call the driver?
                    go SPEC pst1
                ParserD.SPartial m pst1 -> -- n > 0
                    let n = 1 - m
                     in pure $ Partial (1 - n) (parseCont (count + 1 - n) pst1)
                ParserD.SContinue 1 pst1 ->
                    pure $ Continue 1 (parseCont (count + 1) pst1)
                ParserD.SContinue 0 pst1 ->
                    -- XXX recurse or call the driver?
                    go SPEC pst1
                ParserD.SContinue m pst1 -> -- n > 0
                    let n = 1 - m
                     in pure $ Continue (1 - n) (parseCont (count + 1 - n) pst1)

                -- Error case
                ParserD.Error err ->
                    cont (Failure 0 err) count (Chunk x)

    {-# NOINLINE parseContNothing #-}
    parseContNothing !count !pst = do
        r <- extract pst
        case r of
            ParserD.SDone n b ->
                assert (n <= 0)
                    (cont (Success n b) (count + n) None)
            ParserD.SContinue n pst1 ->
                assert (n <= 0)
                    (return $ Continue n (parseCont (count + n) pst1))
            ParserD.Error err ->
                -- XXX It is called only when there is no input chunk. So using
                -- 0 as the position is correct?
                cont (Failure 0 err) count None
            ParserD.SPartial _ _ -> error "Bug: adaptWith Partial unreachable"

    -- XXX Maybe we can use two separate continuations instead of using
    -- Just/Nothing cases here. That may help in avoiding the parseContJust
    -- function call.
    {-# INLINE parseCont #-}
    parseCont !cnt !pst (Chunk element) = parseContChunk cnt pst element
    parseCont !cnt !pst None = parseContNothing cnt pst

-- | Convert a 'Parser' to 'ParserK'.
--
-- /Pre-release/
--
{-# INLINE_LATE parserK #-}
parserK, adapt :: Monad m => ParserD.Parser a m b -> ParserK a m b
parserK (ParserD.Parser step initial extract) =
    MkParser $ adaptWith step initial extract

RENAME(adapt,parserK)

-------------------------------------------------------------------------------
-- Convert CPS style 'Parser' to direct style 'D.Parser'
-------------------------------------------------------------------------------

-- | A continuation to extract the result when a CPS parser is done.
{-# INLINE parserDone #-}
parserDone :: Applicative m =>
    ParseResult b -> Int -> Input a -> m (Step a m b)
parserDone (Success n b) _ _ =
    -- trace ("parserDone Success n: " ++ show n) $
        assert(n <= 1) `seq` pure (Done n b)
parserDone (Failure n e) _ _ =
    -- trace ("parserDone Failure n: " ++ show n) $
        assert(n <= 1) `seq` pure (Error n e)

-- XXX Note that this works only for single element parsers and not for Array
-- input parsers. The asserts will fail for array parsers.

-- | Convert a CPS style 'ParserK' to a direct style 'ParserD.Parser'.
--
-- /Pre-release/
--
{-# INLINE_LATE toParser #-}
toParser :: Monad m => ParserK a m b -> ParserD.Parser a m b
toParser parser = ParserD.Parser step initial extract

    where

    initial = pure (ParserD.IPartial (runParser parser parserDone 0 0))

    step cont a = do
        r <- cont (Chunk a)
        return $ case r of
            Done n b -> assert (n <= 1) (ParserD.SDone n b)
            Error _ e -> ParserD.Error e
            Partial n cont1 -> assert (n <= 1) (ParserD.SPartial n cont1)
            Continue n cont1 -> assert (n <= 1) (ParserD.SContinue n cont1)

    extract cont = do
        r <- cont None
        case r of
            Done n b ->  assert (n <= 0) (return $ ParserD.SDone n b)
            Error _ e -> return $ ParserD.Error e
            Partial _ cont1 -> extract cont1
            Continue n cont1 ->
                assert (n <= 0) (return $ ParserD.SContinue n cont1)

{-# RULES "fromParser/toParser fusion" [2]
    forall s. toParser (parserK s) = s #-}
{-# RULES "toParser/fromParser fusion" [2]
    forall s. parserK (toParser s) = s #-}
