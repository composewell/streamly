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
-- The CPS representation allows linear performance for Applicative, sequenceA,
-- Monad, sequence, and Alternative, choice operations compared to the
-- quadratic complexity of the corresponding direct style operations. However,
-- direct style operations allow fusion with ~10x better performance than CPS.
--
-- The direct style representation does not allow for recursive definitions of
-- "some" and "many" whereas CPS allows that.

module Streamly.Internal.Data.Parser.ParserK.Type
    (
      Step (..)
    , Input (..)
    , ParseResult (..)
    , Parser (..)
    , ParserK
    , fromParser
    -- , toParser
    , fromPure
    , fromEffect
    , die
    )
where

#include "ArrayMacros.h"
#include "assert.hs"
#include "inline.hs"

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Proxy (Proxy(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Unboxed (peekWith, sizeOf, Unbox)

import qualified Control.Monad.Fail as Fail
import qualified Streamly.Internal.Data.Array.Type as Array
import qualified Streamly.Internal.Data.Parser.ParserD.Type as ParserD

data Input a = None | Chunk {-# UNPACK #-} !(Array a)

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
    | Partial !Int (Input a -> m (Step a m r))
    | Continue !Int (Input a -> m (Step a m r))
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
newtype Parser a m b = MkParser
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
           -- XXX Use Array a, determine eof by using a nil array
        -> Input a
        -> (ParseResult b -> Int -> Input a -> m (Step a m r))
        -> m (Step a m r)
    }

type ParserK = Parser

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

-- XXX rewrite this using ParserD, expose rmapM from ParserD.
-- | Maps a function over the output of the parser.
--
instance Functor m => Functor (Parser a m) where
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
fromPure :: b -> Parser a m b
fromPure b = MkParser $ \n st arr pk -> pk (Success n b) st arr

-- | See 'Streamly.Internal.Data.Parser.fromEffect'.
--
-- /Pre-release/
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> Parser a m b
fromEffect eff =
    MkParser $ \n st arr pk -> eff >>= \b -> pk (Success n b) st arr

-- | 'Applicative' form of 'Streamly.Internal.Data.Parser.splitWith'. Note that
-- this operation does not fuse, use 'Streamly.Internal.Data.Parser.splitWith'
-- when fusion is important.
--
instance Monad m => Applicative (Parser a m) where
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
die :: String -> Parser a m b
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
instance Monad m => Monad (Parser a m) where
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
instance Monad m => Fail.MonadFail (Parser a m) where
    {-# INLINE fail #-}
    fail = die

instance MonadIO m => MonadIO (Parser a m) where
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
instance Monad m => Alternative (Parser a m) where
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
instance Monad m => MonadPlus (Parser a m) where
    {-# INLINE mzero #-}
    mzero = die "mzero"

    {-# INLINE mplus #-}
    mplus = (<|>)

{-
instance MonadTrans (Parser a) where
    {-# INLINE lift #-}
    lift = fromEffect
-}

-------------------------------------------------------------------------------
-- Convert ParserD to ParserK
-------------------------------------------------------------------------------

data ChunkResult s b =
      ChunkDone !Int !b
    | ChunkPartial !Int !s
    | ChunkContinue !Int !s
    | ChunkError !Int String

-- This is very similar to fromParserD in the Array/Unboxed/Fold module.
{-# INLINE parseChunk #-}
parseChunk
    :: forall m a s b. (MonadIO m, Unbox a)
    => (s -> a -> m (ParserD.Step s b))
    -> s
    -> Array a
    -> Int
    -> m (ChunkResult s b)
parseChunk pstep !state (Array contents start end) !offset = do
     if offset >= 0
     then go SPEC (start + offset * SIZE_OF(a)) state
     else return $ ChunkContinue offset state

    where

    {-# INLINE onBack #-}
    onBack offset1 elemSize constr pst = do
        let pos = offset1 - start
         in if pos >= 0
            then go SPEC offset1 pst
            else return $ constr (pos `div` elemSize) pst

    -- Note: div may be expensive but the alternative is to maintain an element
    -- offset in addition to a byte offset or just the element offset and use
    -- multiplication to get the byte offset every time, both these options
    -- turned out to be more expensive than using div.
    go !_ !cur !pst | cur >= end =
        return $ ChunkContinue ((end - start) `div` SIZE_OF(a))  pst
    go !_ !cur !pst = do
        x <- liftIO $ peekWith contents cur
        pRes <- pstep pst x
        let elemSize = SIZE_OF(a)
            next = INDEX_NEXT(cur,a)
            back n = next - n * elemSize
            curOff = (cur - start) `div` elemSize
            nextOff = (next - start) `div` elemSize
        case pRes of
            ParserD.Done 0 b ->
                return $ ChunkDone nextOff b
            ParserD.Done 1 b ->
                return $ ChunkDone curOff b
            ParserD.Done n b ->
                return $ ChunkDone ((back n - start) `div` elemSize) b
            ParserD.Partial 0 pst1 ->
                go SPEC next pst1
            ParserD.Partial 1 pst1 ->
                go SPEC cur pst1
            ParserD.Partial n pst1 ->
                onBack (back n) elemSize ChunkPartial pst1
            ParserD.Continue 0 pst1 ->
                go SPEC next pst1
            ParserD.Continue 1 pst1 ->
                go SPEC cur pst1
            ParserD.Continue n pst1 ->
                onBack (back n) elemSize ChunkContinue pst1
            ParserD.Error err ->
                return $ ChunkError curOff err

{-# INLINE parseDToK #-}
parseDToK
    :: forall m a s b r. (MonadIO m, Unbox a)
    => (s -> a -> m (ParserD.Step s b))
    -> m (ParserD.Initial s b)
    -> (s -> m (ParserD.Step s b))
    -> Int
    -> Int
    -> Input a
    -> (ParseResult b -> Int -> Input a -> m (Step a m r))
    -> m (Step a m r)
parseDToK pstep initial extract !offset !usedCount !input cont = do
    res <- initial
    case res of
        ParserD.IPartial pst -> do
            case input of
                Chunk arr -> parseContChunk usedCount offset pst arr
                None -> parseContNothing usedCount pst
        ParserD.IDone b -> cont (Success offset b) usedCount input
        ParserD.IError err -> cont (Failure offset err) usedCount input

    where

    -- XXX We can maintain an absolute position instead of relative that will
    -- help in reporting of error location in the stream.
    {-# NOINLINE parseContChunk #-}
    parseContChunk !count !off !pst !arr = do
        pRes <- parseChunk pstep pst arr off
        -- The "n" here is stream position index wrt the array start, and not
        -- the backtrack count as returned by byte stream parsers.
        case pRes of
            ChunkDone n b ->
                assert (n <= Array.length arr)
                    (cont (Success n b) (count + n - off) (Chunk arr))
            ChunkPartial n pst1 ->
                assert (n < 0 || n >= Array.length arr)
                    (return $ Partial n (parseCont SPEC (count + n - off) pst1))
            ChunkContinue n pst1 ->
                assert (n < 0 || n >= Array.length arr)
                    (return $ Continue n (parseCont SPEC (count + n - off) pst1))
            ChunkError n err ->
                cont (Failure n err) (count + n - off) (Chunk arr)

    {-# NOINLINE parseContNothing #-}
    parseContNothing !count !pst = do
        r <- extract pst
        case r of
            -- IMPORTANT: the n here is from the byte stream parser, that means
            -- it is the backtrack element count and not the stream position
            -- index into the current input array.
            ParserD.Done n b ->
                assert (n >= 0)
                    (cont (Success (- n) b) (count - n) None)
            ParserD.Continue n pst1 ->
                assert (n >= 0)
                    (return $ Continue (- n) (parseCont SPEC (count - n) pst1))
            ParserD.Error err ->
                -- XXX It is called only when there is no input arr. So using 0
                -- as the position is correct?
                cont (Failure 0 err) count None
            ParserD.Partial _ _ -> error "Bug: parseDToK Partial unreachable"

    -- XXX Maybe we can use two separate continuations instead of using
    -- Just/Nothing cases here. That may help in avoiding the parseContJust
    -- function call.
    {-# INLINE parseCont #-}
    parseCont _ !cnt !pst (Chunk arr) = parseContChunk cnt 0 pst arr
    parseCont _ !cnt !pst None = parseContNothing cnt pst

-- | Convert a raw byte 'Parser' to a chunked 'ParserK'.
--
-- /Pre-release/
--
{-# INLINE_LATE fromParser #-}
fromParser :: (MonadIO m, Unbox a) => ParserD.Parser a m b -> ParserK a m b
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
toParser :: Monad m => Parser a m b -> ParserD.Parser a m b
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
