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
    , adaptC
    , adapt
    , adaptCG
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
import Streamly.Internal.Data.Unbox (Unbox(..))
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Control.Monad.Fail as Fail
import qualified Streamly.Internal.Data.Array.Type as Array
import qualified Streamly.Internal.Data.MutArray.Generic as GenArr
    ( getIndexUnsafeWith
    )
import qualified Streamly.Internal.Data.Array.Generic as GenArr
import qualified Streamly.Internal.Data.Parser.Type as ParserD

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
data Input a = None | Chunk a

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
    | Partial !Int (Input a -> m (Step a m r))
    | Continue !Int (Input a -> m (Step a m r))
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
           (ParseResult b -> Int -> Input a -> m (Step a m r))
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
        -> Int
           -- used elem count, a count of elements consumed by the parser. If
           -- an Alternative fails we need to backtrack by this amount.
        -> Int
           -- The second argument is the used count as described above. The
           -- current input position is carried as part of 'Success'
           -- constructor of 'ParseResult'.
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
            k1 (Failure n1 e) s input = k (Failure n1 e) s input
        in runParser p1 k1 n st arr

    {-# INLINE (<*) #-}
    p1 <* p2 = MkParser $ \k n st arr ->
        let k1 (Success n1 b) s1 input =
                let k2 (Success n2 _) s2 input2  = k (Success n2 b) s2 input2
                    k2 (Failure n2 e) s2 input2  = k (Failure n2 e) s2 input2
                in runParser p2 k2 n1 s1 input
            k1 (Failure n1 e) s1 input = k (Failure n1 e) s1 input
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
die err = MkParser (\k n st arr -> k (Failure n err) st arr)

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
            k1 (Failure n1 e) s1 inp = k (Failure n1 e) s1 inp
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
            k1 (Failure pos _) used input = runParser p2 k (pos - used) 0 input
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

--------------------------------------------------------------------------------
-- Chunked
--------------------------------------------------------------------------------

{-# INLINE adaptCWith #-}
adaptCWith
    :: forall m a s b r. (Monad m, Unbox a)
    => (s -> a -> m (ParserD.Step s b))
    -> m (ParserD.Initial s b)
    -> (s -> m (ParserD.Step s b))
    -> (ParseResult b -> Int -> Input (Array a) -> m (Step (Array a) m r))
    -> Int
    -> Int
    -> Input (Array a)
    -> m (Step (Array a) m r)
adaptCWith pstep initial extract cont !offset0 !usedCount !input = do
    res <- initial
    case res of
        ParserD.IPartial pst -> do
            case input of
                Chunk arr -> parseContChunk usedCount offset0 pst arr
                None -> parseContNothing usedCount pst
        ParserD.IDone b -> cont (Success offset0 b) usedCount input
        ParserD.IError err -> cont (Failure offset0 err) usedCount input

    where

    -- XXX We can maintain an absolute position instead of relative that will
    -- help in reporting of error location in the stream.
    {-# NOINLINE parseContChunk #-}
    parseContChunk !count !offset !state arr@(Array contents start end) = do
         if offset >= 0
         then go SPEC (start + offset * SIZE_OF(a)) state
         else return $ Continue offset (parseCont count state)

        where

        {-# INLINE onDone #-}
        onDone n b =
            assert (n <= Array.length arr)
                (cont (Success n b) (count + n - offset) (Chunk arr))

        {-# INLINE callParseCont #-}
        callParseCont constr n pst1 =
            assert (n < 0 || n >= Array.length arr)
                (return $ constr n (parseCont (count + n - offset) pst1))

        {-# INLINE onPartial #-}
        onPartial = callParseCont Partial

        {-# INLINE onContinue #-}
        onContinue = callParseCont Continue

        {-# INLINE onError #-}
        onError n err =
            cont (Failure n err) (count + n - offset) (Chunk arr)

        {-# INLINE onBack #-}
        onBack offset1 elemSize constr pst = do
            let pos = offset1 - start
             in if pos >= 0
                then go SPEC offset1 pst
                else constr (pos `div` elemSize) pst

        -- Note: div may be expensive but the alternative is to maintain an element
        -- offset in addition to a byte offset or just the element offset and use
        -- multiplication to get the byte offset every time, both these options
        -- turned out to be more expensive than using div.
        go !_ !cur !pst | cur >= end =
            onContinue ((end - start) `div` SIZE_OF(a))  pst
        go !_ !cur !pst = do
            let !x = unsafeInlineIO $ peekByteIndex cur contents
            pRes <- pstep pst x
            let elemSize = SIZE_OF(a)
                next = INDEX_NEXT(cur,a)
                back n = next - n * elemSize
                curOff = (cur - start) `div` elemSize
                nextOff = (next - start) `div` elemSize
            -- The "n" here is stream position index wrt the array start, and
            -- not the backtrack count as returned by byte stream parsers.
            case pRes of
                ParserD.Done 0 b ->
                    onDone nextOff b
                ParserD.Done 1 b ->
                    onDone curOff b
                ParserD.Done n b ->
                    onDone ((back n - start) `div` elemSize) b
                ParserD.Partial 0 pst1 ->
                    go SPEC next pst1
                ParserD.Partial 1 pst1 ->
                    go SPEC cur pst1
                ParserD.Partial n pst1 ->
                    onBack (back n) elemSize onPartial pst1
                ParserD.Continue 0 pst1 ->
                    go SPEC next pst1
                ParserD.Continue 1 pst1 ->
                    go SPEC cur pst1
                ParserD.Continue n pst1 ->
                    onBack (back n) elemSize onContinue pst1
                ParserD.Error err ->
                    onError curOff err

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
                    (return $ Continue (- n) (parseCont (count - n) pst1))
            ParserD.Error err ->
                -- XXX It is called only when there is no input arr. So using 0
                -- as the position is correct?
                cont (Failure 0 err) count None
            ParserD.Partial _ _ -> error "Bug: adaptCWith Partial unreachable"

    -- XXX Maybe we can use two separate continuations instead of using
    -- Just/Nothing cases here. That may help in avoiding the parseContJust
    -- function call.
    {-# INLINE parseCont #-}
    parseCont !cnt !pst (Chunk arr) = parseContChunk cnt 0 pst arr
    parseCont !cnt !pst None = parseContNothing cnt pst

-- | Convert a raw byte 'Parser' to a chunked 'ParserK'.
--
-- /Pre-release/
--
{-# INLINE_LATE adaptC #-}
adaptC :: (Monad m, Unbox a) => ParserD.Parser a m b -> ParserK (Array a) m b
adaptC (ParserD.Parser step initial extract) =
    MkParser $ adaptCWith step initial extract

--------------------------------------------------------------------------------
-- Singular
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
            -- XXX can we come here with relPos 1?
            if relPos == 0
            then
                case input of
                    Chunk arr -> parseContChunk usedCount pst arr
                    None -> parseContNothing usedCount pst
            -- XXX Previous code was using Continue in this case
            else pure $ Partial relPos (parseCont usedCount pst)
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
            pRes <- pstep pst x
            case pRes of
                ParserD.Done 0 b ->
                    cont (Success 1 b) (count + 1) (Chunk x)
                ParserD.Done 1 b ->
                    cont (Success 0 b) count (Chunk x)
                ParserD.Done n b ->
                    cont (Success (1 - n) b) (count + 1 - n) (Chunk x)
                ParserD.Partial 0 pst1 ->
                    pure $ Partial 1 (parseCont (count + 1) pst1)
                ParserD.Partial 1 pst1 ->
                    -- XXX Since we got Partial, the driver should drop the
                    -- buffer, we should call the driver here?
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
                    -- XXX fix undefined
                    cont (Failure 0 err) count (Chunk x)

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
                    (return $ Continue (- n) (parseCont (count - n) pst1))
            ParserD.Error err ->
                -- XXX It is called only when there is no input arr. So using 0
                -- as the position is correct?
                cont (Failure 0 err) count None
            ParserD.Partial _ _ -> error "Bug: adaptCWith Partial unreachable"

    -- XXX Maybe we can use two separate continuations instead of using
    -- Just/Nothing cases here. That may help in avoiding the parseContJust
    -- function call.
    {-# INLINE parseCont #-}
    parseCont !cnt !pst (Chunk arr) = parseContChunk cnt pst arr
    parseCont !cnt !pst None = parseContNothing cnt pst

-- | Similar to "adaptC" but for non-chunked input.
--
{-# INLINE_LATE adapt #-}
adapt :: Monad m => ParserD.Parser a m b -> ParserK a m b
adapt (ParserD.Parser step initial extract) =
    MkParser $ adaptWith step initial extract

--------------------------------------------------------------------------------
-- Chunked Generic
--------------------------------------------------------------------------------

{-# INLINE adaptCGWith #-}
adaptCGWith
    :: forall m a s b r. (Monad m)
    => (s -> a -> m (ParserD.Step s b))
    -> m (ParserD.Initial s b)
    -> (s -> m (ParserD.Step s b))
    -> (ParseResult b -> Int -> Input (GenArr.Array a) -> m (Step (GenArr.Array a) m r))
    -> Int
    -> Int
    -> Input (GenArr.Array a)
    -> m (Step (GenArr.Array a) m r)
adaptCGWith pstep initial extract cont !offset0 !usedCount !input = do
    res <- initial
    case res of
        ParserD.IPartial pst -> do
            case input of
                Chunk arr -> parseContChunk usedCount offset0 pst arr
                None -> parseContNothing usedCount pst
        ParserD.IDone b -> cont (Success offset0 b) usedCount input
        ParserD.IError err -> cont (Failure offset0 err) usedCount input

    where

    {-# NOINLINE parseContChunk #-}
    parseContChunk !count !offset !state arr@(GenArr.Array contents start len) = do
         if offset >= 0
         then go SPEC (start + offset) state
         else return $ Continue offset (parseCont count state)

        where

        {-# INLINE end #-}
        end = start + len

        {-# INLINE onDone #-}
        onDone n b =
            assert (n <= GenArr.length arr)
                (cont (Success n b) (count + n - offset) (Chunk arr))

        {-# INLINE callParseCont #-}
        callParseCont constr n pst1 =
            assert (n < 0 || n >= GenArr.length arr)
                (return $ constr n (parseCont (count + n - offset) pst1))

        {-# INLINE onPartial #-}
        onPartial = callParseCont Partial

        {-# INLINE onContinue #-}
        onContinue = callParseCont Continue

        {-# INLINE onError #-}
        onError n err =
            cont (Failure n err) (count + n - offset) (Chunk arr)

        {-# INLINE onBack #-}
        onBack offset1 constr pst = do
            let pos = offset1 - start
             in if pos >= 0
                then go SPEC offset1 pst
                else constr pos pst

        go !_ !cur !pst | cur >= end =
            onContinue len  pst
        go !_ !cur !pst = do
            let !x = unsafeInlineIO $ GenArr.getIndexUnsafeWith contents cur
            pRes <- pstep pst x
            let next = cur + 1
                back n = next - n
                curOff = cur - start
                nextOff = next - start
            -- The "n" here is stream position index wrt the array start, and
            -- not the backtrack count as returned by byte stream parsers.
            case pRes of
                ParserD.Done 0 b ->
                    onDone nextOff b
                ParserD.Done 1 b ->
                    onDone curOff b
                ParserD.Done n b ->
                    onDone (back n - start) b
                ParserD.Partial 0 pst1 ->
                    go SPEC next pst1
                ParserD.Partial 1 pst1 ->
                    go SPEC cur pst1
                ParserD.Partial n pst1 ->
                    onBack (back n) onPartial pst1
                ParserD.Continue 0 pst1 ->
                    go SPEC next pst1
                ParserD.Continue 1 pst1 ->
                    go SPEC cur pst1
                ParserD.Continue n pst1 ->
                    onBack (back n) onContinue pst1
                ParserD.Error err ->
                    onError curOff err

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
                    (return $ Continue (- n) (parseCont (count - n) pst1))
            ParserD.Error err ->
                -- XXX It is called only when there is no input arr. So using 0
                -- as the position is correct?
                cont (Failure 0 err) count None
            ParserD.Partial _ _ -> error "Bug: adaptCGWith Partial unreachable"

    {-# INLINE parseCont #-}
    parseCont !cnt !pst (Chunk arr) = parseContChunk cnt 0 pst arr
    parseCont !cnt !pst None = parseContNothing cnt pst

-- | Similar to "adaptC" but is not constrained.
--
{-# INLINE_LATE adaptCG #-}
adaptCG ::
       Monad m => ParserD.Parser a m b -> ParserK (GenArr.Array a) m b
adaptCG (ParserD.Parser step initial extract) =
    MkParser $ adaptCGWith step initial extract

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
