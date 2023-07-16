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

module Streamly.Internal.Data.Parser.ParserK.Type
    (
      Step (..)
    , Input (..)
    , ParseResult (..)
    , ParserK (..)
    , fromParser
    -- , toParser
    , fromPure
    , fromEffect
    , die
    )
where

#include "assert.hs"
#include "inline.hs"

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.Trans.Class (MonadTrans(lift))
import GHC.Types (SPEC(..))

import qualified Control.Monad.Fail as Fail
import qualified Streamly.Internal.Data.Parser.ParserD.Type as ParserD

data Input a = None | Single a

-- | The intermediate result of running a parser step. The parser driver may
-- stop with a final result, pause with a continuation to resume, or fail with
-- an error.
--
-- See ParserD docs. This is the same as the ParserD Step except that it uses a
-- continuation in Partial and Continue constructors instead of a state in case
-- of ParserD.
--
-- /Pre-release/
--
data Step a m r =
    -- The Int is the relative stream position to move to.
    -- 0 means try the current element again
    -- 1 means try the next element
    -- -1 backtrack by 1 element
      Done !Int r
      -- XXX we can use a "resume" and a "stop" continuations instead of Maybe.
      -- measure if that works any better.
      -- a -> m (Step a m r), m (Step a m r)
    | Partial !Int (Input a -> m (Step a m r))
    | Continue !Int (Input a -> m (Step a m r))
    | Error String

instance Functor m => Functor (Step a m) where
    fmap f (Done n r) = Done n (f r)
    fmap f (Partial n k) = Partial n (fmap (fmap f) . k)
    fmap f (Continue n k) = Continue n (fmap (fmap f) . k)
    fmap _ (Error e) = Error e

-- Note: Passing position index separately instead of passing it with the
-- result causes huge regression in expression parsing becnhmarks.

-- | The parser's result.
--
-- Int is the relative stream position to move to. Could be negative.
--
-- /Pre-release/
--
data ParseResult b =
      Success !Int !b     -- Relative stream position, result
    | Failure !String     -- Error

-- | Map a function over 'Success'.
instance Functor ParseResult where
    fmap f (Success n b) = Success n (f b)
    fmap _ (Failure e) = Failure e

-- XXX Change the type to the shape (a -> m r -> m r) -> (m r -> m r) -> m r
--
-- The parse continuation would be: a -> m (Step a m r) -> m (Step a m r)
-- The extract continuation would be: m (Step a m r) -> m (Step a m r)
--
-- Use Step itself in place of ParseResult.

-- | A continuation passing style parser representation. A continuation of
-- 'Step's, each step passes a state and a parse result to the next 'Step'. The
-- resulting 'Step' may carry a continuation that consumes input 'a' and
-- results in another 'Step'. Essentially, the continuation may either consume
-- input without a result or return a result with no further input to be
-- consumed.
--
newtype ParserK a m b = MkParser
    { runParser :: forall r.
           -- Using "Input" in runParser is not necessary but it avoids making
           -- one more function call to get the input. This could be helpful
           -- for cases where we process just one element per call.
           --
           -- Do not eta reduce the applications of this continuation.
           --
           -- The second argument is the used count. The current input position
           -- is carried as part of 'Success' constructor of 'ParseResult'.
           (ParseResult b -> Int -> Input a -> m (Step a m r))
           -- XXX Maintain and pass the original position in the stream. that
           -- way we can also report better errors. Use a Context structure for
           -- passing the state.

           -- Relative stream position. If
           -- negative then backtracking is required.
           -- The parser should use "Continue -n" in this case if it needs to
           -- backtrack. Negative value cannot be beyond the current
           -- backtrack buffer. Positive value cannot be beyond 1.
           -- Only single element forward tracking is possible now.
        -> Int
           -- Used elem count, a count of elements consumed by the parser. If
           -- an Alternative fails we need to backtrack by this amount.
        -> Int
        -> Input a
        -> m (Step a m r)
    }

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

-- XXX rewrite this using ParserD, expose rmapM from ParserD.
-- | Maps a function over the output of the parser.
--
instance Functor m => Functor (ParserK a m) where
    {-# INLINE fmap #-}
    fmap f parser = MkParser $ \k n st arr ->
        let k1 res = k (fmap f res)
         in runParser parser k1 n st arr

-------------------------------------------------------------------------------
-- Sequential applicative
-------------------------------------------------------------------------------

-- This is the dual of stream "fromPure".
--
-- | A parser that always yields a pure value without consuming any input.
--
-- /Pre-release/
--
{-# INLINE fromPure #-}
fromPure :: b -> ParserK a m b
fromPure b = MkParser $ \k n st arr -> k (Success n b) st arr

-- | See 'Streamly.Internal.Data.Parser.fromEffect'.
--
-- /Pre-release/
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> ParserK a m b
fromEffect eff =
    MkParser $ \k n st arr -> eff >>= \b -> k (Success n b) st arr

-- | 'Applicative' form of 'Streamly.Internal.Data.Parser.splitWith'. Note that
-- this operation does not fuse, use 'Streamly.Internal.Data.Parser.splitWith'
-- when fusion is important.
--
instance Monad m => Applicative (ParserK a m) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = ap

    {-# INLINE (*>) #-}
    p1 *> p2 = MkParser $ \k n st arr ->
        let k1 (Success n1 _) s input = runParser p2 k n1 s input
            k1 (Failure e) s input = k (Failure e) s input
        in runParser p1 k1 n st arr

    {-# INLINE (<*) #-}
    p1 <* p2 = MkParser $ \k n st arr ->
        let k1 (Success n1 b) s1 input =
                let k2 (Success n2 _) s2 input2  = k (Success n2 b) s2 input2
                    k2 (Failure e) s2 input2  = k (Failure e) s2 input2
                in runParser p2 k2 n1 s1 input
            k1 (Failure e) s1 input = k (Failure e) s1 input
        in runParser p1 k1 n st arr

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
die err = MkParser (\k _ st arr -> k (Failure err) st arr)

-- | Monad composition can be used for lookbehind parsers, we can make the
-- future parses depend on the previously parsed values.
--
-- If we have to parse "a9" or "9a" but not "99" or "aa" we can use the
-- following parser:
--
-- @
-- backtracking :: MonadCatch m => PR.Parser Char m String
-- backtracking =
--     sequence [PR.satisfy isDigit, PR.satisfy isAlpha]
--     '<|>'
--     sequence [PR.satisfy isAlpha, PR.satisfy isDigit]
-- @
--
-- We know that if the first parse resulted in a digit at the first place then
-- the second parse is going to fail.  However, we waste that information and
-- parse the first character again in the second parse only to know that it is
-- not an alphabetic char.  By using lookbehind in a 'Monad' composition we can
-- avoid redundant work:
--
-- @
-- data DigitOrAlpha = Digit Char | Alpha Char
--
-- lookbehind :: MonadCatch m => PR.Parser Char m String
-- lookbehind = do
--     x1 \<-    Digit '<$>' PR.satisfy isDigit
--          '<|>' Alpha '<$>' PR.satisfy isAlpha
--
--     -- Note: the parse depends on what we parsed already
--     x2 <- case x1 of
--         Digit _ -> PR.satisfy isAlpha
--         Alpha _ -> PR.satisfy isDigit
--
--     return $ case x1 of
--         Digit x -> [x,x2]
--         Alpha x -> [x,x2]
-- @
--
-- See also 'Streamly.Internal.Data.Parser.concatMap'. This monad instance
-- does not fuse, use 'Streamly.Internal.Data.Parser.concatMap' when you need
-- fusion.
--
instance Monad m => Monad (ParserK a m) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    p >>= f = MkParser $ \k n st arr ->
        let k1 (Success n1 b) s1 inp = runParser (f b) k n1 s1 inp
            k1 (Failure e) s1 inp = k (Failure e) s1 inp
         in runParser p k1 n st arr

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

-- | 'Alternative' form of 'Streamly.Internal.Data.Parser.alt'. Backtrack and
-- run the second parser if the first one fails.
--
-- The "some" and "many" operations of alternative accumulate results in a pure
-- list which is not scalable and streaming. Instead use
-- 'Streamly.Internal.Data.Parser.some' and
-- 'Streamly.Internal.Data.Parser.many' for fusible operations with composable
-- accumulation of results.
--
-- See also 'Streamly.Internal.Data.Parser.alt'. This 'Alternative' instance
-- does not fuse, use 'Streamly.Internal.Data.Parser.alt' when you need
-- fusion.
--
instance Monad m => Alternative (ParserK a m) where
    {-# INLINE empty #-}
    empty = die "empty"

    {-# INLINE (<|>) #-}
    p1 <|> p2 = MkParser $ \k n _ arr ->
        let
            k1 (Failure _) used input = runParser p2 k (- used) 0 input
            k1 success _ input = k success 0 input
        in runParser p1 k1 n 0 arr

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

-------------------------------------------------------------------------------
-- Convert ParserD to ParserK
-------------------------------------------------------------------------------

{-# INLINE parseDToK #-}
parseDToK
    :: forall m a s b r. (Monad m)
    => (s -> a -> m (ParserD.Step s b))
    -> m (ParserD.Initial s b)
    -> (s -> m (ParserD.Step s b))
    -> (ParseResult b -> Int -> Input a -> m (Step a m r))
    -> Int
    -> Int
    -> Input a
    -> m (Step a m r)
parseDToK pstep initial extract cont !relPos !usedCount !input = do
    res <- initial
    case res of
        ParserD.IPartial pst -> do
            if relPos == 0
            then
                case input of
                    Single x -> parseContSingle usedCount pst x
                    None -> parseContNothing usedCount pst
            else pure $ Partial relPos (parseCont usedCount pst)
        ParserD.IDone b -> cont (Success 0 b) usedCount input
        ParserD.IError err -> cont (Failure err) usedCount input

    where

    -- XXX We can maintain an absolute position instead of relative that will
    -- help in reporting of error location in the stream.
    {-# NOINLINE parseContSingle #-}
    parseContSingle !count !state !x =
        go SPEC state

        where

        go !_ !pst = do
            pRes <- pstep pst x
            case pRes of
                ParserD.Done 0 b ->
                    cont (Success 1 b) (count + 1) (Single x)
                ParserD.Done 1 b ->
                    cont (Success 0 b) count (Single x)
                ParserD.Done n b ->
                    cont (Success (1 - n) b) (count + 1 - n) (Single x)
                ParserD.Partial 0 pst1 ->
                    pure $ Partial 1 (parseCont (count + 1) pst1)
                ParserD.Partial 1 pst1 ->
                    go SPEC pst1
                ParserD.Partial n pst1 ->
                    pure $ Partial (1 - n) (parseCont (count + 1 - n) pst1)
                ParserD.Continue 0 pst1 ->
                    pure $ Continue 1 (parseCont (count + 1) pst1)
                ParserD.Continue 1 pst1 ->
                    go SPEC pst1
                ParserD.Continue n pst1 ->
                    pure $ Continue (1 - n) (parseCont (count + 1 - n) pst1)
                ParserD.Error err ->
                    cont (Failure err) count (Single x)

    {-# NOINLINE parseContNothing #-}
    parseContNothing !count !pst = do
        r <- extract pst
        case r of
            -- IMPORTANT: the n here is from the byte stream parser, that means
            -- it is the backtrack element count and not the relative stream
            -- position.
            ParserD.Done n b ->
                assert (n >= 0)
                    (cont (Success (- n) b) (count - n) None)
            ParserD.Continue n pst1 ->
                assert (n >= 0)
                    (return $ Continue (- n) (parseCont (count - n) pst1))
            ParserD.Error err ->
                cont (Failure err) count None
            ParserD.Partial _ _ -> error "Bug: parseDToK Partial unreachable"

    -- XXX Maybe we can use two separate continuations instead of using
    -- Just/Nothing cases here. That may help in avoiding the parseContJust
    -- function call.
    {-# INLINE parseCont #-}
    parseCont !cnt !pst (Single x) = parseContSingle cnt pst x
    parseCont !cnt !pst None = parseContNothing cnt pst

-- | Convert a raw byte 'Parser' to a 'ParserK'.
--
-- /Pre-release/
--
{-# INLINE_LATE fromParser #-}
fromParser :: Monad m => ParserD.Parser a m b -> ParserK a m b
fromParser (ParserD.Parser step initial extract) =
    MkParser $ parseDToK step initial extract

{-
-------------------------------------------------------------------------------
-- Convert CPS style 'Parser' to direct style 'D.Parser'
-------------------------------------------------------------------------------

-- | A continuation to extract the result when a CPS parser is done.
{-# INLINE parserDone #-}
parserDone :: Monad m => ParseResult b -> Int -> Input a -> m (Step a m b)
parserDone (Success n b) _ None = return $ Done n b
parserDone (Failure n e) _ None = return $ Error n e
parserDone _ _ _ = error "Bug: toParser: called with input"

-- | Convert a CPS style 'ParserK' to a direct style 'ParserD.Parser'.
--
-- /Pre-release/
--
{-# INLINE_LATE toParser #-}
toParser :: Monad m => ParserK a m b -> ParserD.Parser a m b
toParser parser = ParserD.Parser step initial extract

    where

    initial = pure (ParserD.IPartial (\x -> runParser parser 0 0 x parserDone))

    step cont a = do
        r <- cont (Single a)
        return $ case r of
            Done n b -> ParserD.Done n b
            Error _ e -> ParserD.Error e
            Partial n cont1 -> ParserD.Partial n cont1
            Continue n cont1 -> ParserD.Continue n cont1

    extract cont = do
        r <- cont None
        case r of
            Done n b -> return $ ParserD.Done n b
            Error _ e -> return $ ParserD.Error e
            Partial _ cont1 -> extract cont1
            Continue n cont1 -> return $ ParserD.Continue n cont1

#ifndef DISABLE_FUSION
{-# RULES "fromParser/toParser fusion" [2]
    forall s. toParser (fromParser s) = s #-}
{-# RULES "toParser/fromParser fusion" [2]
    forall s. fromParser (toParser s) = s #-}
#endif
-}
