#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Parser.ParserK.Types
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

module Streamly.Internal.Data.Parser.ParserK.Types
    (
      Parser (..)
    , yield
    , die

    -- * Conversion
    , toParserK
    , fromParserK
    )
where

import Control.Applicative (Alternative(..))
import Control.Exception (assert, Exception(..))
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.Catch (MonadCatch, MonadThrow(..), try)
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
#if !(MIN_VERSION_base(4,10,0))
import Data.Semigroup ((<>))
#endif
import Streamly.Internal.Data.Zipper (Zipper (..))
import Prelude hiding (splitAt)

import qualified Streamly.Internal.Data.Zipper as Z
import qualified Streamly.Internal.Data.Parser.ParserD.Types as D

-- | The parse driver result. The driver may stop with a final result, pause
-- with a continuation to resume, or fail with an error.
--
-- /Internal/
--
data Driver m a r =
      Stop !Int r
      -- XXX we can use a "resume" and a "stop" continuations instead of Maybe.
      -- measure if that works any better.
    | Partial (Maybe a -> m (Driver m a r))
    | Continue (Maybe a -> m (Driver m a r))
    | Failed String

instance Functor m => Functor (Driver m a) where
    fmap f (Stop n r) = Stop n (f r)
    fmap f (Partial yld) = Partial (fmap (fmap f) . yld)
    fmap f (Continue yld) = Continue (fmap (fmap f) . yld)
    fmap _ (Failed e) = Failed e

-- The parser's result.
--
-- /Internal/
--
data Parse b =
      Done !b           -- Done, no more input needed
    | Error !String     -- Failed

instance Functor Parse where
    fmap f (Done b) = Done (f b)
    fmap _ (Error e) = Error e

-- | A continuation passing style parser representation. The parser is supplied
-- with a 'Zipper' with an initial input, and a continuation to consume the
-- parse result. The Zipper is threaded around as a state and maintains the
-- parser backtracking buffer.
--
newtype Parser m a b = MkParser
    { runParser :: forall r.
           Zipper a
        -> (Zipper a -> Parse b -> m (Driver m a r))
        -> m (Driver m a r)
    }

-------------------------------------------------------------------------------
-- Convert direct style 'D.Parser' to CPS style 'Parser'
-------------------------------------------------------------------------------

-- Inlined definition. Without the inline "serially/parser/take" benchmark
-- degrades and splitParse does not fuse. Even using "inline" at the callsite
-- does not help.
{-# INLINE splitAt #-}
splitAt :: Int -> [a] -> ([a],[a])
splitAt n ls
  | n <= 0 = ([], ls)
  | otherwise          = splitAt' n ls
    where
        splitAt' :: Int -> [a] -> ([a], [a])
        splitAt' _  []     = ([], [])
        splitAt' 1  (x:xs) = ([x], xs)
        splitAt' m  (x:xs) = (x:xs', xs'')
          where
            (xs', xs'') = splitAt' (m - 1) xs

-- XXX Unlike the direct style folds/parsers, the initial action in CPS parsers
-- is not performed when the fold is initialized. It is performed when the
-- first element is processed by the fold or if no elements are processed then
-- at the extraction. We should either make the direct folds like this or make
-- the CPS folds behavior also like the direct ones.
--
-- | Parse a stream zipper using a direct style parser's @step@, @initial@ and
-- @extract@ functions.
--
{-# INLINE_NORMAL parse #-}
parse
    :: MonadCatch m
    => (s -> a -> m (D.Step s b))
    -> m s
    -> (s -> m b)
    -> Zipper a
    -> (Zipper a -> Parse b -> m (Driver m a r))
    -> m (Driver m a r)

-- The case when no checkpoints exist
parse pstep initial extract (Zipper [] backward forward) cont =
    case forward of
        [] -> return $ Continue (parseCont backward initial)
        _ -> goBuf backward forward initial

    where

    parseCont back acc (Just x) = goSingle back x acc
    parseCont back acc Nothing = do
        pst <- acc
        r <- try $ extract pst
        -- NOTE: Any data held in the backtrack buffer is considered as unused
        -- data if we extract the parser result without the parser finishing.
        let z = Zipper [] back []
        case r of
            Left (e :: D.ParseError) ->
                cont z (Error (displayException e))
            Right b -> cont z (Done b)

    {-# INLINE goSingle #-}
    goSingle back x !pst = do
        r <- pst
        pRes <- pstep r x
        case pRes of
            D.Done n b -> do
                assert (n <= length (x:back)) (return ())
                let fwd0 = Prelude.take n (x:back)
                    fwd  = Prelude.reverse fwd0
                cont (Zipper [] [] fwd) (Done b)
            D.Partial 0 pst1 ->
                return $ Partial (parseCont [] (return pst1))
            D.Partial n pst1 -> do
                assert (n <= length (x:back)) (return ())
                let fwd0 = Prelude.take n (x:back)
                    fwd  = Prelude.reverse fwd0
                goBuf [] fwd (return pst1)
            D.Continue 0 pst1 ->
                return $ Continue (parseCont (x:back) (return pst1))
            D.Continue n pst1 -> do
                assert (n <= length (x:back)) (return ())
                let (fwd0, bwd) = splitAt n (x:back)
                    fwd  = Prelude.reverse fwd0
                goBuf bwd fwd (return pst1)
            D.Error err ->
                cont (Zipper [] (x:back) []) (Error err)

    goBuf back [] !acc = return $ Continue (parseCont back acc)
    goBuf back (x:xs) !pst = do
        r <- pst
        pRes <- pstep r x
        case pRes of
            D.Done n b -> do
                assert (n <= length (x:back)) (return ())
                let fwd0 = Prelude.take n (x:back)
                    fwd  = Prelude.reverse fwd0
                cont (Zipper [] [] (fwd ++ xs)) (Done b)
            D.Partial 0 pst1 ->
                goBuf [] xs (return pst1)
            D.Partial n pst1 -> do
                assert (n <= length (x:back)) (return ())
                let fwd0 = Prelude.take n (x:back)
                    fwd  = Prelude.reverse fwd0
                goBuf [] (fwd ++ xs) (return pst1)
            D.Continue 0 pst1 -> goBuf (x:back) xs (return pst1)
            D.Continue n pst1 -> do
                assert (n <= length (x:back)) (return ())
                let (fwd0, bwd) = splitAt n (x:back)
                    fwd = Prelude.reverse fwd0
                goBuf bwd (fwd ++ xs) (return pst1)
            D.Error err -> cont (Zipper [] (x:back) xs) (Error err)

-- The case when checkpoints exist
-- XXX code duplication alert!
parse pstep initial extract (Zipper (cp:cps) backward forward) cont =
    case forward of
        [] -> return $ Continue (parseCont 0 backward initial)
        _ -> goBuf 0 backward forward initial

    where

    parseCont cnt back acc (Just x) = goSingle cnt back x acc
    parseCont cnt back acc Nothing = do
        pst <- acc
        r <- try $ extract pst
        let z = Zipper (cp + cnt : cps) back []
        case r of
            Left (e :: D.ParseError) ->
                cont z (Error (displayException e))
            Right b -> cont z (Done b)

    {-# INLINE goSingle #-}
    goSingle cnt back x !pst = do
        r <- pst
        pRes <- pstep r x
        let cnt1 = cnt + 1
        case pRes of
            D.Done n b -> do
                assert (n <= length (x:back)) (return ())
                let fwd0 = Prelude.take n (x:back)
                    fwd  = Prelude.reverse fwd0
                assert (cp + cnt1 - n >= 0) (return ())
                cont (Zipper (cp + cnt1 - n : cps) [] fwd) (Done b)
            D.Partial 0 pst1 -> do
                return $ Partial (parseCont cnt1 [] (return pst1))
            D.Partial n pst1 -> do
                assert (n <= length (x:back)) (return ())
                let fwd0 = Prelude.take n (x:back)
                    fwd  = Prelude.reverse fwd0
                goBuf (cnt1 - n) [] fwd (return pst1)
            D.Continue 0 pst1 ->
                return $ Continue (parseCont cnt1 (x:back) (return pst1))
            D.Continue n pst1 -> do
                assert (n <= length (x:back)) (return ())
                let (fwd0, bwd) = splitAt n (x:back)
                    fwd  = Prelude.reverse fwd0
                assert (cnt1 - n >= 0) (return ())
                goBuf (cnt1 - n) bwd fwd (return pst1)
            D.Error err ->
                cont (Zipper (cp + cnt1 : cps) (x:back) []) (Error err)

    goBuf cnt back [] !acc = return $ Continue (parseCont cnt back acc)
    goBuf cnt back (x:xs) !pst = do
        r <- pst
        pRes <- pstep r x
        let cnt1 = cnt + 1
        case pRes of
            D.Done n b -> do
                assert (n <= length (x:back)) (return ())
                let fwd0 = Prelude.take n (x:back)
                    fwd  = Prelude.reverse fwd0
                assert (cp + cnt1 - n >= 0) (return ())
                cont (Zipper (cp + cnt1 - n : cps) [] (fwd ++ xs)) (Done b)
            D.Partial 0 pst1 ->
                goBuf cnt1 [] xs (return pst1)
            D.Partial n pst1 -> do
                assert (n <= length (x:back)) (return ())
                let fwd0 = Prelude.take n (x:back)
                    fwd  = Prelude.reverse fwd0
                goBuf (cnt1 - n) [] (fwd ++ xs) (return pst1)
            D.Continue 0 pst1 -> goBuf cnt1 (x:back) xs (return pst1)
            D.Continue n pst1 -> do
                assert (n <= length (x:back)) (return ())
                let (fwd0, bwd) = splitAt n (x:back)
                    fwd  = Prelude.reverse fwd0 ++ xs
                assert (cnt1 - n >= 0) (return ())
                goBuf (cnt1 - n) bwd fwd (return pst1)
            D.Error err ->
                cont (Zipper (cp + cnt1 : cps) (x:back) xs) (Error err)

-- | Convert a direct style 'Parser' to a CPS style 'K.Parser'.
--
-- /Internal/
--
{-# INLINE_LATE toParserK #-}
toParserK :: MonadCatch m => D.Parser m a b -> Parser m a b
toParserK (D.Parser step initial extract) =
    MkParser $ parse step initial extract

-------------------------------------------------------------------------------
-- Convert CPS style 'Parser' to direct style 'D.Parser'
-------------------------------------------------------------------------------

-- | A continuation to extract the result when a CPS parser is done.
{-# INLINE parserDone #-}
parserDone :: Monad m => Zipper a -> Parse b -> m (Driver m a b)
parserDone (Z.Zipper [] bwd fwd) (Done b) =
    -- When the parser is Done, we consider the data in the
    -- backtracking buffer as well as in the forward buffer as
    -- unconsumed input.
    -- XXX Will it be more efficient/worth it to maintain a count
    -- in the Zipper?
    return $ Stop (length bwd + length fwd) b
parserDone (Z.Zipper cps _ _) (Done _) =
    error $ "Bug: fromParserK: when checkpoints exist: " ++ show cps
parserDone _ (Error e) = return $ Failed e

-- | When there is no more input to feed, extract the result from the Parser.
--
-- /Internal/
--
extractParse :: MonadThrow m => (Maybe a -> m (Driver m a b)) -> m b
extractParse cont = do
    r <- cont Nothing
    case r of
        Stop _ b -> return b
        Partial _ -> error "Bug: extractParse got Partial"
        Continue _ -> error "Bug: extractParse got Continue"
        Failed e -> throwM $ D.ParseError e

-- XXX The CPS style parsers use a zipper buffering the data, if a parserD is
-- driving the parserK then it would also be buffering the same data.  For
-- ParserD, instead of maintaining the buffer in the common driver, each parser
-- can have its own buffering and we can return the unconsumed buffer in the
-- end.  That way the zipper is maintained by the parser. If the parser fails
-- then it has to return all of the input. It is anyway maintained by
-- intermediate level parsers in a composition, so the only difference would be
-- that even the leaf levels parsers would do it. If we abstract the zipper
-- maintainance then it may not be too unwieldy.

data FromParserK b e c = FPKDone !b | FPKError !e | FPKCont c

-- | Convert a CPS style 'Parser' to a direct style 'D.Parser'.
--
-- /Internal/
--
{-# INLINE_LATE fromParserK #-}
fromParserK :: MonadThrow m => Parser m a b -> D.Parser m a b
fromParserK parser = D.Parser step initial extract

    where

    initial = do
        r <- runParser parser Z.nil parserDone
        return $ case r of
            Stop 0 b -> FPKDone b
            Stop _ _ -> error "Bug: fromParserK: Stop n in initial"
            Failed e -> FPKError e
            Partial cont -> FPKCont cont -- XXX can we get this?
            Continue cont -> FPKCont cont

    step (FPKDone b) _ = return $ D.Done 1 b
    step (FPKError e) _ = return $ D.Error e
    step (FPKCont cont) a = do
        r <- cont (Just a)
        return $ case r of
            Stop n b -> D.Done n b
            Failed e -> D.Error e
            Partial cont1 -> D.Partial 0 (FPKCont cont1)
            Continue cont1 -> D.Continue 0 (FPKCont cont1)

    extract (FPKDone b) = return b
    extract (FPKError e) = throwM $ D.ParseError e
    extract (FPKCont cont) = extractParse cont

#ifndef DISABLE_FUSION
{-# RULES "fromParserK/toParserK fusion" [2]
    forall s. toParserK (fromParserK s) = s #-}
{-# RULES "toParserK/fromParserK fusion" [2]
    forall s. fromParserK (toParserK s) = s #-}
#endif

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

-- | Maps a function over the output of the parser.
--
instance Functor m => Functor (Parser m a) where
    {-# INLINE fmap #-}
    fmap f parser = MkParser $ \zipper yieldk ->
        let yld z res = yieldk z (fmap f res)
         in runParser parser zipper yld

-------------------------------------------------------------------------------
-- Sequential applicative
-------------------------------------------------------------------------------

-- This is the dual of stream "yield".
--
-- | A parser that always yields a pure value without consuming any input.
--
-- /Internal/
--
{-# INLINE yield #-}
yield :: b -> Parser m a b
yield b = MkParser (\zipper yieldk -> yieldk zipper (Done b))

-- | 'Applicative' form of 'Streamly.Internal.Data.Parser.splitWith'. Note that
-- this operation does not fuse, use 'Streamly.Internal.Data.Parser.splitWith'
-- when fusion is important.
--
instance Monad m => Applicative (Parser m a) where
    {-# INLINE pure #-}
    pure = yield

    {-# INLINE (<*>) #-}
    (<*>) = ap

    {-# INLINE (*>) #-}
    m1 *> m2 = MkParser $ \zipper yieldk ->
        let yield1 z (Done _) = runParser m2 z yieldk
            yield1 z (Error e) = yieldk z (Error e)
        in runParser m1 zipper yield1

    {-# INLINE (<*) #-}
    m1 <* m2 = MkParser $ \zipper yieldk ->
        let
            yield1 z (Done b) = runParser m2 z $ \z1 _ -> yieldk z1 (Done b)
            yield1 z (Error e) = yieldk z (Error e)
        in runParser m1 zipper yield1

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

-- This is the dual of "nil".
--
-- | A parser that always fails with an error message without consuming
-- any input.
--
-- /Internal/
--
{-# INLINE die #-}
die :: String -> Parser m a b
die err = MkParser (\z yieldk -> yieldk z (Error err))

-- | Monad composition can be used for lookbehind parsers, we can make the
-- future parses depend on the previously parsed values.
--
-- If we have to parse "a9" or "9a" but not "99" or "aa" we can use the
-- following parser:
--
-- @
-- backtracking :: MonadCatch m => PR.Parser m Char String
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
-- lookbehind :: MonadCatch m => PR.Parser m Char String
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
instance Monad m => Monad (Parser m a) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    m >>= k = MkParser $ \zipper yieldk ->
        let
            yield1 z (Done b) = runParser (k b) z yieldk
            yield1 z (Error e) = yieldk z (Error e)
         in runParser m zipper yield1

#if !(MIN_VERSION_base(4,13,0))
    -- This is redefined instead of just being Fail.fail to be
    -- compatible with base 4.8.
    {-# INLINE fail #-}
    fail = die
#endif

#if MIN_VERSION_base(4,9,0)
instance Monad m => Fail.MonadFail (Parser m a) where
    {-# INLINE fail #-}
    fail = die
#endif

-------------------------------------------------------------------------------
-- Alternative
-------------------------------------------------------------------------------

-- XXX one way to backtrack is to ask the input driver to create and release
-- checkpoints, like we do in the zipper. The other way is to pass around the
-- zipper itself. Currently in the direct style code we manage the checkpoints
-- manually by counting the input. We need to unify the CPS and direct style
-- code by either using a zipper in both or by using checkpointing commands for
-- the driver.
--
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
instance Monad m => Alternative (Parser m a) where
    {-# INLINE empty #-}
    empty = die "empty"

    {-# INLINE (<|>) #-}
    m1 <|> m2 = MkParser $ \zipper yieldk ->
        let yield1 z (Done b) = yieldk (Z.release z) (Done b)
            yield1 z (Error _) = runParser m2 (Z.restore z) yieldk
        in runParser m1 (Z.checkpoint zipper) yield1

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
-- /Internal/
--
instance Monad m => MonadPlus (Parser m a) where
    {-# INLINE mzero #-}
    mzero = die "mzero"

    {-# INLINE mplus #-}
    mplus = (<|>)
