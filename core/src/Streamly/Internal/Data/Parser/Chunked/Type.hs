{-# LANGUAGE UndecidableInstances #-}
#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Parser.Chunked.Type
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- CPS style implementation of parsers.
--
-- The CPS representation allows linear performance for Applicative, sequenceA,
-- Monad, sequence, and Alternative, choice operations compared to the
-- quadratic complexity of the corresponding direct style operations. However,
-- direct style operations allow fusion with ~10x better performance than CPS.
--
-- The direct style representation does not allow for recursive definitions of
-- "some" and "many" whereas CPS allows that.

-- XXX This code has only one line difference from the
-- Data.Parser.ParserK/Type.

module Streamly.Internal.Data.Parser.Chunked.Type
    (
      Step (..)
    , ParseResult (..)
    , ParserChunked (..)
    , fromPure
    , fromEffect
    , die
    )
where

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Streamly.Internal.Data.Array (Array)
import qualified Control.Monad.Fail as Fail

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
    -- The Int is the current stream position index wrt to the start of the
    -- array.
      Done !Int r
      -- XXX we can use a "resume" and a "stop" continuations instead of Maybe.
      -- measure if that works any better.
      -- Array a -> m (Step a m r), m (Step a m r)
      -- XXX The Array is the only difference from element parser, we can pass
      -- this as parameter?
    | Partial !Int (Maybe (Array a) -> m (Step a m r))
    | Continue !Int (Maybe (Array a) -> m (Step a m r))
    | Error !Int String

instance Functor m => Functor (Step a m) where
    fmap f (Done n r) = Done n (f r)
    fmap f (Partial n k) = Partial n (fmap (fmap f) . k)
    fmap f (Continue n k) = Continue n (fmap (fmap f) . k)
    fmap _ (Error n e) = Error n e

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

-- | A continuation passing style parser representation. A continuation of
-- 'Step's, each step passes a state and a parse result to the next 'Step'. The
-- resulting 'Step' may carry a continuation that consumes input 'a' and
-- results in another 'Step'. Essentially, the continuation may either consume
-- input without a result or return a result with no further input to be
-- consumed.
--
newtype ParserChunked a m b = MkParser
    { runParser :: forall r.
           -- XXX Maintain and pass the original position in the stream. that
           -- way we can also report better errors. Use a Context structure for
           -- passing the state.

           -- Stream position index wrt to the current input array start. If
           -- negative then backtracking is required before using the array.
           -- The parser should use "Continue -n" in this case if it needs to
           -- consume input. Negative value cannot be beyond the current
           -- backtrack buffer. Positive value cannot be beyond array length.
           -- If the parser needs to advance beyond the array length it should
           -- use "Continue +n".
           Int
           -- used elem count, a count of elements consumed by the parser. If
           -- an Alternative fails we need to backtrack by this amount.
        -> Int
           -- The second argument is the used count as described above. The
           -- current input position is carried as part of 'Success'
           -- constructor of 'ParseResult'.
        -> Maybe (Array a)
        -> (ParseResult b -> Int -> Maybe (Array a) -> m (Step a m r))
        -> m (Step a m r)
    }

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

-- XXX rewrite this using ParserD, expose rmapM from ParserD.
-- | Maps a function over the output of the parser.
--
instance Functor m => Functor (ParserChunked a m) where
    {-# INLINE fmap #-}
    fmap f parser = MkParser $ \n st arr pk ->
        let pk1 res = pk (fmap f res)
         in runParser parser n st arr pk1

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
fromPure :: b -> ParserChunked a m b
fromPure b = MkParser $ \n st arr pk -> pk (Success n b) st arr

-- | See 'Streamly.Internal.Data.Parser.fromEffect'.
--
-- /Pre-release/
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> ParserChunked a m b
fromEffect eff =
    MkParser $ \n st arr pk -> eff >>= \b -> pk (Success n b) st arr

-- | 'Applicative' form of 'Streamly.Internal.Data.Parser.serialWith'. Note that
-- this operation does not fuse, use 'Streamly.Internal.Data.Parser.serialWith'
-- when fusion is important.
--
instance Monad m => Applicative (ParserChunked a m) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = ap

    {-# INLINE (*>) #-}
    p1 *> p2 = MkParser $ \n st arr k ->
        let k1 (Success n1 _) s input = runParser p2 n1 s input k
            k1 (Failure n1 e) s input = k (Failure n1 e) s input
        in runParser p1 n st arr k1

    {-# INLINE (<*) #-}
    p1 <* p2 = MkParser $ \n st arr k ->
        let k1 (Success n1 b) s1 input =
                let k2 (Success n2 _) = k (Success n2 b)
                    k2 (Failure n2 e) = k (Failure n2 e)
                in runParser p2 n1 s1 input k2
            k1 (Failure n1 e) s1 input = k (Failure n1 e) s1 input
        in runParser p1 n st arr k1

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
die :: String -> ParserChunked a m b
die err = MkParser (\n st arr pk -> pk (Failure n err) st arr)

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
instance Monad m => Monad (ParserChunked a m) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    p >>= f = MkParser $ \n st arr pk ->
        let pk1 (Success n1 b) s1 inp = runParser (f b) n1 s1 inp pk
            pk1 (Failure n1 e) s1 inp = pk (Failure n1 e) s1 inp
         in runParser p n st arr pk1

    {-# INLINE (>>) #-}
    (>>) = (*>)

#if !(MIN_VERSION_base(4,13,0))
    -- This is redefined instead of just being Fail.fail to be
    -- compatible with base 4.8.
    {-# INLINE fail #-}
    fail = die
#endif
instance Monad m => Fail.MonadFail (ParserChunked a m) where
    {-# INLINE fail #-}
    fail = die

instance MonadIO m => MonadIO (ParserChunked a m) where
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
instance Monad m => Alternative (ParserChunked a m) where
    {-# INLINE empty #-}
    empty = die "empty"

    {-# INLINE (<|>) #-}
    p1 <|> p2 = MkParser $ \n _ arr k ->
        let
            k1 (Failure pos _) used input = runParser p2 (pos - used) 0 input k
            k1 success _ input = k success 0 input
        in runParser p1 n 0 arr k1

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
instance Monad m => MonadPlus (ParserChunked a m) where
    {-# INLINE mzero #-}
    mzero = die "mzero"

    {-# INLINE mplus #-}
    mplus = (<|>)
