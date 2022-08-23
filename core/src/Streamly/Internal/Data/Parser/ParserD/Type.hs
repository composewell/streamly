{-# LANGUAGE UndecidableInstances #-}
#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Parser.ParserD.Type
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Streaming and backtracking parsers.
--
-- Parsers just extend folds.  Please read the 'Fold' design notes in
-- "Streamly.Internal.Data.Fold.Type" for background on the design.
--
-- = Parser Design
--
-- The 'Parser' type or a parsing fold is a generalization of the 'Fold' type.
-- The 'Fold' type /always/ succeeds on each input. Therefore, it does not need
-- to buffer the input. In contrast, a 'Parser' may fail and backtrack to
-- replay the input again to explore another branch of the parser. Therefore,
-- it needs to buffer the input. Therefore, a 'Parser' is a fold with some
-- additional requirements.  To summarize, unlike a 'Fold', a 'Parser':
--
-- 1. may not generate a new value of the accumulator on every input, it may
-- generate a new accumulator only after consuming multiple input elements
-- (e.g. takeEQ).
-- 2. on success may return some unconsumed input (e.g. takeWhile)
-- 3. may fail and return all input without consuming it (e.g. satisfy)
-- 4. backtrack and start inspecting the past input again (e.g. alt)
--
-- These use cases require buffering and replaying of input.  To facilitate
-- this, the step function of the 'Fold' is augmented to return the next state
-- of the fold along with a command tag using a 'Step' functor, the tag tells
-- the fold driver to manipulate the future input as the parser wishes. The
-- 'Step' functor provides the following commands to the fold driver
-- corresponding to the use cases outlined in the previous para:
--
-- 1. 'Continue': buffer the current input and optionally go back to a previous
--    position in the stream
-- 2. 'Partial': buffer the current input and optionally go back to a previous
--    position in the stream, drop the buffer before that position.
-- 3. 'Done': parser succeeded, returns how much input was leftover
-- 4. 'Error': indicates that the parser has failed without a result
--
-- = How a Parser Works?
--
-- A parser is just like a fold, it keeps consuming inputs from the stream and
-- accumulating them in an accumulator. The accumulator of the parser could be
-- a singleton value or it could be a collection of values e.g. a list.
--
-- The parser may build a new output value from multiple input items. When it
-- consumes an input item but needs more input to build a complete output item
-- it uses @Continue 0 s@, yielding the intermediate state @s@ and asking the
-- driver to provide more input.  When the parser determines that a new output
-- value is complete it can use a @Done n b@ to terminate the parser with @n@
-- items of input unused and the final value of the accumulator returned as
-- @b@. If at any time the parser determines that the parse has failed it can
-- return @Error err@.
--
-- A parser building a collection of values (e.g. a list) can use the @Partial@
-- constructor whenever a new item in the output collection is generated. If a
-- parser building a collection of values has yielded at least one value then
-- it is considered successful and cannot fail after that. In the current
-- implementation, this is not automatically enforced, there is a rule that the
-- parser MUST use only @Done@ for termination after the first @Partial@, it
-- cannot use @Error@. It may be possible to change the implementation so that
-- this rule is not required, but there may be some performance cost to it.
--
-- 'Streamly.Internal.Data.Parser.takeWhile' and
-- 'Streamly.Internal.Data.Parser.some' combinators are good examples of
-- efficient implementations using all features of this representation.  It is
-- possible to idiomatically build a collection of parsed items using a
-- singleton parser and @Alternative@ instance instead of using a
-- multi-yield parser.  However, this implementation is amenable to stream
-- fusion and can therefore be much faster.
--
-- = Error Handling
--
-- When a parser's @step@ function is invoked it may terminate by either a
-- 'Done' or an 'Error' return value. In an 'Alternative' composition an error
-- return can make the composed parser backtrack and try another parser.
--
-- If the stream stops before a parser could terminate then we use the
-- @extract@ function of the parser to retrieve the last yielded value of the
-- parser. If the parser has yielded at least one value then @extract@ MUST
-- return a value without throwing an error, otherwise it uses the 'ParseError'
-- exception to throw an error.
--
-- We chose the exception throwing mechanism for @extract@ instead of using an
-- explicit error return via an 'Either' type for keeping the interface simple
-- as most of the time we do not need to catch the error in intermediate
-- layers. Note that we cannot use exception throwing mechanism in @step@
-- function because of performance reasons. 'Error' constructor in that case
-- allows loop fusion and better performance.
--
-- = Optimizing backtracking
--
-- == Applicative Composition
--
-- If a parser once returned 'Partial' it can never fail after that. This is
-- used to reduce the buffering. A 'Partial' results in dropping the buffer and
-- we cannot backtrack before that point.
--
-- Parsers can be composed using an Alternative, if we are in an alternative
-- composition we may have to backtrack to try the other branch.  When we
-- compose two parsers using applicative @f <$> p1 <*> p2@ we can return a
-- 'Partial' result only after both the parsers have succeeded. While running
-- @p1@ we have to ensure that the input is not dropped until we have run @p2@,
-- therefore we have to return a Continue instead of a Partial.
--
-- However, if we know they both cannot fail then we know that the composed
-- parser can never fail.  For this reason we should have "backtracking folds"
-- as a separate type so that we can compose them in an efficient manner. In p1
-- itself we can drop the buffer as soon as a 'Partial' result arrives. In
-- fact, there is no Alternative composition for folds because they cannot
-- fail.
--
-- == Alternative Composition
--
-- In @p1 <|> p2@ as soon as the parser p1 returns 'Partial' we know that it
-- will not fail and we can immediately drop the buffer.
--
-- If we are not using the parser in an alternative composition we can
-- downgrade the parser to a backtracking fold and use the "backtracking
-- fold"'s applicative for more efficient implementation. To downgrade we can
-- translate the "Error" of parser to an exception.  This gives us best of both
-- worlds, the applicative as well as alternative would have optimal
-- backtracking buffer.
--
-- The "many" for parsers would be different than "many" for folds. In case of
-- folds an error would be propagated. In case of parsers the error would be
-- ignored.
--
-- = Implementation Approach
--
-- Backtracking folds have an issue with tee style composition because each
-- fold can backtrack independently, we will need independent buffers. Though
-- this may be possible to implement it may not be efficient especially for
-- folds that do not backtrack at all. Three types are possible, optimized for
-- different use cases:
--
-- * Non-backtracking folds: efficient Tee
-- * Backtracking folds: efficient applicative
-- * Parsers: alternative
--
-- Downgrade parsers to backtracking folds for applicative used without
-- alternative.  Upgrade backtracking folds to parsers when we have to use them
-- as the last alternative.
--
-- = Future Work
--
-- It may make sense to move "takeWhile" type of parsers, which cannot fail but
-- need some lookahead, to splitting folds.  This will allow such combinators
-- to be accepted where we need an unfailing "Fold" type.
--
-- Based on application requirements it should be possible to design even a
-- richer interface to manipulate the input stream/buffer. For example, we
-- could randomly seek into the stream in the forward or reverse directions or
-- we can even seek to the end or from the end or seek from the beginning.
--
-- We can distribute and scan/parse a stream using both folds and parsers and
-- merge the resulting streams using different merge strategies (e.g.
-- interleaving or serial).

module Streamly.Internal.Data.Parser.ParserD.Type
    (
    -- * Types
      Initial (..)
    , Step (..)
    , extractStep
    , bimapOverrideCount
    , Parser (..)
    , ParseError (..)
    , rmapM

    -- * Conversion to/from ParserK
    , fromParserK
    , toParserK

    -- * Constructors

    , fromPure
    , fromEffect
    , serialWith
    , split_

    , die
    , dieM
    , splitSome -- parseSome?
    , splitMany -- parseMany?
    , splitManyPost
    , alt
    , concatMap

    -- * Input transformation
    , lmap
    , lmapM
    , filter

    , noErrorUnsafeSplitWith
    , noErrorUnsafeSplit_
    , noErrorUnsafeConcatMap
    )
where

#include "assert.hs"

import Control.Applicative (Alternative(..), liftA2)
import Control.Exception (Exception(..))
import Control.Monad (MonadPlus(..), (>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.State.Class (MonadState, get, put)
import Data.Bifunctor (Bifunctor(..))
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Fold.Type (Fold(..), toList)
import Streamly.Internal.Data.Tuple.Strict (Tuple3'(..))

import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Parser.ParserK.Type as K

import Prelude hiding (concatMap, filter)
--
-- $setup
-- >>> :m
-- >>> :set -package streamly
-- >>> import Control.Applicative ((<|>))
-- >>> import Data.Bifunctor (second)
-- >>> import Prelude hiding (concatMap)
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Stream as Stream (parse)
-- >>> import qualified Streamly.Internal.Data.Parser as Parser
-- >>> import qualified Streamly.Internal.Data.Parser.ParserD as ParserD

-- XXX The only differences between Initial and Step types are:
--
-- * There are no backtracking counts in Initial
-- * Continue and Partial are the same. Ideally Partial should mean that an
-- empty result is valid and can be extracted; and Continue should mean that
-- empty would result in an error on extraction. We can possibly distinguish
-- the two cases.
--
-- If we ignore the backtracking counts we can represent the Initial type using
-- Step itself. That will also simplify the implementation of various parsers
-- where the processing in intiial is just a sepcial case of step, see
-- takeBetween for example.
--
-- | The type of a 'Parser''s initial action.
--
-- /Internal/
--
{-# ANN type Initial Fuse #-}
data Initial s b
    = IPartial !s   -- ^ Wait for step function to be called with state @s@.
    | IDone !b      -- ^ Return a result right away without an input.
    | IError !String -- ^ Return an error right away without an input.

-- | @first@ maps on 'IPartial' and @second@ maps on 'IDone'.
--
-- /Internal/
--
instance Bifunctor Initial where
    {-# INLINE bimap #-}
    bimap f _ (IPartial a) = IPartial (f a)
    bimap _ g (IDone b) = IDone (g b)
    bimap _ _ (IError err) = IError err

-- | Maps a function over the result held by 'IDone'.
--
-- >>> fmap = second
--
-- /Internal/
--
instance Functor (Initial s) where
    {-# INLINE fmap #-}
    fmap = second

-- We can simplify the Step type as follows:
--
-- Partial Int (Either s (s, b)) -- Left continue, right partial result
-- Done Int (Either String b)
--
-- In this case Error may also have a "leftover" return. This means that after
-- several successful partial results the last segment parsing failed and we
-- are returning the leftover of that. The driver may choose to restart from
-- the last segment where this parser failed or from the beginning.
--
-- Folds can only return the right values. Parsers can also return lefts.

-- | The return type of a 'Parser' step.
--
-- The parse operation feeds the input stream to the parser one element at a
-- time, representing a parse 'Step'. The parser may or may not consume the
-- item and returns a result. If the result is 'Partial' we can either extract
-- the result or feed more input to the parser. If the result is 'Continue', we
-- must feed more input in order to get a result. If the parser returns 'Done'
-- then the parser can no longer take any more input.
--
-- If the result is 'Continue', the parse operation retains the input in a
-- backtracking buffer, in case the parser may ask to backtrack in future.
-- Whenever a 'Partial n' result is returned we first backtrack by @n@ elements
-- in the input and then release any remaining backtracking buffer. Similarly,
-- 'Continue n' backtracks to @n@ elements before the current position and
-- starts feeding the input from that point for future invocations of the
-- parser.
--
-- If parser is not yet done, we can use the @extract@ operation on the @state@
-- of the parser to extract a result. If the parser has not yet yielded a
-- result, the operation fails with a 'ParseError' exception. If the parser
-- yielded a 'Partial' result in the past the last partial result is returned.
-- Therefore, if a parser yields a partial result once it cannot fail later on.
--
-- The parser can never backtrack beyond the position where the last partial
-- result left it at. The parser must ensure that the backtrack position is
-- always after that.
--
-- /Pre-release/
--
{-# ANN type Step Fuse #-}
data Step s b =
        Partial !Int !s
    -- ^ @Partial count state@. The following hold on Partial result:
    --
    -- 1. @extract@ on @state@ would succeed and give a result.
    -- 2. Input stream position is reset to @current position - count@.
    -- 3. All input before the new position is dropped. The parser can
    -- never backtrack beyond this position.

    | Continue !Int !s
    -- ^ @Continue count state@. The following hold on a Continue result:
    --
    -- 1. If there was a 'Partial' result in past, @extract@ on @state@ would
    -- give that result as 'Done' otherwise it may return 'Error' or
    -- 'Continue'.
    -- 2. Input stream position is reset to @current position - count@.
    -- 3. the input is retained in a backtrack buffer.

    | Done !Int !b
    -- ^ Done with leftover input count and result.
    --
    -- @Done count result@ means the parser has finished, it will accept no
    -- more input, last @count@ elements from the input are unused and the
    -- result of the parser is in @result@.

    | Error !String
    -- ^ Parser failed without generating any output.
    --
    -- The parsing operation may backtrack to the beginning and try another
    -- alternative.

-- | Map first function over the state and second over the result.
instance Bifunctor Step where
    {-# INLINE bimap #-}
    bimap f g step =
        case step of
            Partial n s -> Partial n (f s)
            Continue n s -> Continue n (f s)
            Done n b -> Done n (g b)
            Error err -> Error err

-- | Bimap discarding the count, and using the supplied count instead.
bimapOverrideCount :: Int -> (s -> s1) -> (b -> b1) -> Step s b -> Step s1 b1
bimapOverrideCount n f g step =
    case step of
        Partial _ s -> Partial n (f s)
        Continue _ s -> Continue n (f s)
        Done _ b -> Done n (g b)
        Error err -> Error err

-- | fmap = second
instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap = second

{-# INLINE assertStepCount #-}
assertStepCount :: Int -> Step s b -> Step s b
assertStepCount i step =
    case step of
        Partial n _ -> assert (i == n) step
        Continue n _ -> assert (i == n) step
        Done n _ -> assert (i == n) step
        Error _ -> step

-- | Map an extract function over the state of Step
--
{-# INLINE extractStep #-}
extractStep :: Monad m => (s -> m (Step s1 b)) -> Step s b -> m (Step s1 b)
extractStep f res =
    case res of
        Partial n s1 -> assertStepCount n <$> f s1
        Done n b -> return $ Done n b
        Continue n s1 -> assertStepCount n <$> f s1
        Error err -> return $ Error err

-- | Map a monadic function over the result @b@ in @Step s b@.
--
-- /Internal/
{-# INLINE mapMStep #-}
mapMStep :: Applicative m => (a -> m b) -> Step s a -> m (Step s b)
mapMStep f res =
    case res of
        Partial n s -> pure $ Partial n s
        Done n b -> Done n <$> f b
        Continue n s -> pure $ Continue n s
        Error err -> pure $ Error err

-- | A parser is a fold that can fail and is represented as @Parser step
-- initial extract@. Before we drive a parser we call the @initial@ action to
-- retrieve the initial state of the fold. The parser driver invokes @step@
-- with the state returned by the previous step and the next input element. It
-- results into a new state and a command to the driver represented by 'Step'
-- type. The driver keeps invoking the step function until it stops or fails.
-- At any point of time the driver can call @extract@ to inspect the result of
-- the fold. If the parser hits the end of input 'extract' is called.
-- It may result in an error or an output value.
--
-- /Pre-release/
--
data Parser m a b =
    forall s. Parser
        (s -> a -> m (Step s b))
        -- Initial cannot return "Partial/Done n" or "Continue". Continue 0 is
        -- same as Partial 0. In other words it cannot backtrack.
        (m (Initial s b))
        -- Extract can only return Partial or Continue n. In other words it can
        -- only backtrack or return partial result/error. But we do not return
        -- result in Partial, therefore, we have to use Done instead of Partial.
        (s -> m (Step s b))

-- | This exception is used when a parser ultimately fails, the user of the
-- parser is intimated via this exception.
--
-- /Pre-release/
--
newtype ParseError = ParseError String
    deriving Show

instance Exception ParseError where
    displayException (ParseError err) = err

instance Functor m => Functor (Parser m a) where
    {-# INLINE fmap #-}
    fmap f (Parser step1 initial1 extract) =
        Parser step initial (fmap3 f extract)

        where

        initial = fmap2 f initial1
        step s b = fmap2 f (step1 s b)
        fmap2 g = fmap (fmap g)
        fmap3 g = fmap2 (fmap g)

-------------------------------------------------------------------------------
-- Convert direct style 'Parser' to CPS style 'K.Parser'
-------------------------------------------------------------------------------

-- XXX Unlike the direct style folds/parsers, the initial action in CPS parsers
-- is not performed when the fold is initialized. It is performed when the
-- first element is processed by the fold or if no elements are processed then
-- at the extraction. We should either make the direct folds like this or make
-- the CPS folds behavior also like the direct ones.
--
-- | Convert a direct style parser ('D.Parser') to a CPS style parser
-- ('Parser').
--
{-# INLINE_NORMAL parseDToK #-}
parseDToK
    :: Monad m
    => (s -> a -> m (Step s b))
    -> m (Initial s b)
    -> (s -> m (Step s b))
    -> Int
    -> (Int, Int)
    -> ((Int, Int) -> K.Parse b -> m (K.Step m a r))
    -> m (K.Step m a r)

-- Non 'Alternative' case.
parseDToK pstep initial extract leftover (0, _) cont = do
    res <- initial
    case res of
        IPartial r -> return $ K.Continue leftover (parseCont (return r))
        IDone b -> cont state (K.Success 0 b)
        IError err -> cont state (K.Failure err)

    where

    -- The continuation is called with (0,0) state i.e. Alternative level
    -- is 0 and the used count is 0. Alternative level is 0 because the level
    -- passed in the argument above is 0, the "used" count is 0 because it is
    -- not useful when Alternative level is 0. We should probably use a Maybe
    -- type for the state but that might impact performance, need to measure.
    state = (0,0)

    parseCont pst (Just x) = do
        r <- pst
        pRes <- pstep r x
        case pRes of
            Done n b -> cont state (K.Success n b)
            Error err -> cont state (K.Failure err)
            Partial n pst1 -> return $ K.Partial n (parseCont (return pst1))
            Continue n pst1 -> return $ K.Continue n (parseCont (return pst1))

    parseCont acc Nothing = do
        pst <- acc
        r <- extract pst
        case r of
            Done n b -> cont state (K.Success n b)
            Error err -> cont state (K.Failure err)
            Partial _ _ -> error "Bug: parseDToK Partial unreachable"
            Continue n pst1 -> return $ K.Continue n (parseCont (return pst1))

-- 'Alternative' case. Used count needs to be maintained when inside an
-- Alternative.
parseDToK pstep initial extract leftover (level, count) cont = do
    res <- initial
    case res of
        IPartial r -> return $ K.Continue leftover (parseCont count (return r))
        IDone b -> cont (level,count) (K.Success 0 b)
        IError err -> cont (level,count) (K.Failure err)

    where

    parseCont !cnt pst (Just x) = do
        let !cnt1 = cnt + 1
        r <- pst
        pRes <- pstep r x
        case pRes of
            Done n b -> do
                assertM(n <= cnt1)
                cont (level, cnt1 - n) (K.Success n b)
            Error err ->
                cont (level, cnt1) (K.Failure err)
            Partial n pst1 -> do
                assertM(n <= cnt1)
                return $ K.Partial n (parseCont (cnt1 - n) (return pst1))
            Continue n pst1 -> do
                assertM(n <= cnt1)
                return $ K.Continue n (parseCont (cnt1 - n) (return pst1))
    parseCont cnt acc Nothing = do
        pst <- acc
        r <- extract pst
        let s = (level, cnt)
        case r of
            Done n b -> do
                assertM(n <= cnt)
                cont s (K.Success n b)
            Partial _ _ -> error "Bug: parseDToK Partial unreachable"
            Error e ->
                cont s (K.Failure e)
            Continue n pst1 -> do
                assertM(n <= cnt)
                return $ K.Continue n (parseCont (cnt - n) (return pst1))

-- | Convert a direct style 'Parser' to a CPS style 'K.Parser'.
--
-- /Pre-release/
--
{-# INLINE_LATE toParserK #-}
toParserK :: Monad m => Parser m a b -> K.Parser m a b
toParserK (Parser step initial extract) =
    K.MkParser $ parseDToK step initial extract

-------------------------------------------------------------------------------
-- Convert CPS style 'Parser' to direct style 'D.Parser'
-------------------------------------------------------------------------------

-- | A continuation to extract the result when a CPS parser is done.
{-# INLINE parserDone #-}
parserDone :: Monad m => (Int, Int) -> K.Parse b -> m (K.Step m a b)
parserDone (0,_) (K.Success n b) = return $ K.Done n b
parserDone st (K.Success _ _) =
    error $ "Bug: fromParserK: inside alternative: " ++ show st
parserDone _ (K.Failure e) = return $ K.Error e

-- | Convert a CPS style 'K.Parser' to a direct style 'Parser'.
--
-- "initial" returns a continuation which can be called one input at a time
-- using the "step" function.
--
-- /Pre-release/
--
{-# INLINE_LATE fromParserK #-}
fromParserK :: Monad m => K.Parser m a b -> Parser m a b
fromParserK parser = Parser step initial extract

    where

    initial = do
        r <- K.runParser parser 0 (0,0) parserDone
        return $ case r of
            K.Done n b -> assert (n == 0) (IDone b)
            K.Error e -> IError e
            K.Partial _ cont -> assert False (IPartial cont)
            K.Continue n cont -> assert (n == 0) (IPartial cont)

    step cont a = do
        r <- cont (Just a)
        return $ case r of
            K.Done n b -> Done n b
            K.Error e -> Error e
            K.Partial n cont1 -> Partial n cont1
            K.Continue n cont1 -> Continue n cont1

    extract cont = do
        r <- cont Nothing
        return $ case r of
            K.Done n b -> Done n b
            K.Error e -> Error e
            K.Partial _ _ -> error "Bug: extract got Partial"
            K.Continue n cont1 -> Continue n cont1

#ifndef DISABLE_FUSION
{-# RULES "fromParserK/toParserK fusion" [2]
    forall s. toParserK (fromParserK s) = s #-}
{-# RULES "toParserK/fromParserK fusion" [2]
    forall s. fromParserK (toParserK s) = s #-}
#endif

------------------------------------------------------------------------------
-- Mapping on the output
------------------------------------------------------------------------------

-- | Map a monadic function on the output of a parser.
--
-- /Pre-release/
{-# INLINE rmapM #-}
rmapM :: Monad m => (b -> m c) -> Parser m a b -> Parser m a c
rmapM f (Parser step initial extract) =
    Parser step1 initial1 (extract >=> mapMStep f)

    where

    initial1 = do
        res <- initial
        -- this is mapM f over result
        case res of
            IPartial x -> return $ IPartial x
            IDone a -> IDone <$> f a
            IError err -> return $ IError err
    step1 s a = step s a >>= mapMStep f

-- | See 'Streamly.Internal.Data.Parser.fromPure'.
--
-- /Pre-release/
--
{-# INLINE_NORMAL fromPure #-}
fromPure :: Monad m => b -> Parser m a b
fromPure b = Parser undefined (pure $ IDone b) undefined

-- | See 'Streamly.Internal.Data.Parser.fromEffect'.
--
-- /Pre-release/
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> Parser m a b
fromEffect b = Parser undefined (IDone <$> b) undefined

-------------------------------------------------------------------------------
-- Sequential applicative
-------------------------------------------------------------------------------

{-# ANN type SeqParseState Fuse #-}
data SeqParseState sl f sr = SeqParseL sl | SeqParseR f sr

-- | See 'Streamly.Internal.Data.Parser.serialWith'.
--
-- Note: this implementation of serialWith is fast because of stream fusion but
-- has quadratic time complexity, because each composition adds a new branch
-- that each subsequent parse's input element has to go through, therefore, it
-- cannot scale to a large number of compositions. After around 100
-- compositions the performance starts dipping rapidly beyond a CPS style
-- unfused implementation.
--
-- /Pre-release/
--
{-# INLINE serialWith #-}
serialWith :: Monad m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
serialWith func (Parser stepL initialL extractL)
               (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    initial = do
        -- XXX We can use bimap here if we make this a Step type
        resL <- initialL
        case resL of
            IPartial sl -> return $ IPartial $ SeqParseL sl
            IDone bl -> do
                resR <- initialR
                -- XXX We can use bimap here if we make this a Step type
                return $ case resR of
                    IPartial sr -> IPartial $ SeqParseR (func bl) sr
                    IDone br -> IDone (func bl br)
                    IError err -> IError err
            IError err -> return $ IError err

    -- Note: For the composed parse to terminate, the left parser has to be
    -- a terminating parser returning a Done at some point.
    step (SeqParseL st) a = do
        -- Important: Please do not use Applicative here. See
        -- https://github.com/composewell/streamly/issues/1033 and the problem
        -- defined in split_ for more info.
        -- XXX Use bimap
        resL <- stepL st a
        case resL of
            -- Note: We need to buffer the input for a possible Alternative
            -- e.g. in ((,) <$> p1 <*> p2) <|> p3, if p2 fails we have to
            -- backtrack and start running p3. So we need to keep the input
            -- buffered until we know that the applicative cannot fail.
            Partial n s -> return $ Continue n (SeqParseL s)
            Continue n s -> return $ Continue n (SeqParseL s)
            Done n b -> do
                -- XXX Use bimap if we make this a Step type
                -- fmap (bimap (SeqParseR (func b)) (func b)) initialR
                initR <- initialR
                return $ case initR of
                   IPartial sr -> Continue n $ SeqParseR (func b) sr
                   IDone br -> Done n (func b br)
                   IError err -> Error err
            Error err -> return $ Error err

    step (SeqParseR f st) a = fmap (bimap (SeqParseR f) f) (stepR st a)

    extract (SeqParseR f sR) = fmap (bimap (SeqParseR f) f) (extractR sR)
    extract (SeqParseL sL) = do
        -- XXX Use bimap here
        rL <- extractL sL
        case rL of
            Done n bL -> do
                -- XXX Use bimap here if we use Step type in Initial
                iR <- initialR
                case iR of
                    IPartial sR -> do
                        fmap
                            (bimap (SeqParseR (func bL)) (func bL))
                            (extractR sR)
                    IDone bR -> return $ Done n $ func bL bR
                    IError err -> return $ Error err
            Error err -> return $ Error err
            Partial _ _ -> error "Bug: serialWith extract 'Partial'"
            Continue n s -> return $ Continue n (SeqParseL s)

-------------------------------------------------------------------------------
-- Sequential applicative for backtracking folds
-------------------------------------------------------------------------------

-- XXX Create a newtype for nonfailing parsers and downgrade the parser to that
-- type before this operation and then upgrade.
--
-- We can do an inspection testing to reject unwanted constructors at compile
-- time.
--
-- We can use the compiler to automatically annotate accumulators, terminating
-- folds, non-failing parsers and failing parsers.

-- | Works correctly only if both the parsers are guaranteed to never fail.
{-# INLINE noErrorUnsafeSplitWith #-}
noErrorUnsafeSplitWith :: Monad m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
noErrorUnsafeSplitWith func (Parser stepL initialL extractL)
               (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    errMsg e = error $ "noErrorUnsafeSplitWith: unreachable: " ++ e

    initial = do
        resL <- initialL
        case resL of
            IPartial sl -> return $ IPartial $ SeqParseL sl
            IDone bl -> do
                resR <- initialR
                return $ bimap (SeqParseR (func bl)) (func bl) resR
            IError err -> errMsg err

    -- Note: For the composed parse to terminate, the left parser has to be
    -- a terminating parser returning a Done at some point.
    step (SeqParseL st) a = do
        r <- stepL st a
        case r of
            -- Assume that the parser can never fail, therefore, we do not
            -- need to keep the input for backtracking.
            Partial n s -> return $ Partial n (SeqParseL s)
            Continue n s -> return $ Continue n (SeqParseL s)
            Done n b -> do
                res <- initialR
                return
                    $ case res of
                          IPartial sr -> Partial n $ SeqParseR (func b) sr
                          IDone br -> Done n (func b br)
                          IError err -> errMsg err
            Error err -> errMsg err

    step (SeqParseR f st) a = fmap (bimap (SeqParseR f) f) (stepR st a)

    extract (SeqParseR f sR) = fmap (bimap (SeqParseR f) f) (extractR sR)

    extract (SeqParseL sL) = do
        rL <- extractL sL
        case rL of
            Done n bL -> do
                iR <- initialR
                case iR of
                    IPartial sR -> do
                        rR <- extractR sR
                        return
                            $ bimapOverrideCount
                                n (SeqParseR (func bL)) (func bL) rR
                    IDone bR -> return $ Done n $ func bL bR
                    IError err -> errMsg err
            Error err -> errMsg err
            Partial _ _ -> errMsg "Partial"
            Continue n s -> return $ Continue n (SeqParseL s)

{-# ANN type SeqAState Fuse #-}
data SeqAState sl sr = SeqAL sl | SeqAR sr

-- This turns out to be slightly faster than serialWith
-- | See 'Streamly.Internal.Data.Parser.split_'.
--
-- /Pre-release/
--
{-# INLINE split_ #-}
split_ :: Monad m => Parser m x a -> Parser m x b -> Parser m x b
split_ (Parser stepL initialL extractL) (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    initial = do
        resL <- initialL
        case resL of
            IPartial sl -> return $ IPartial $ SeqAL sl
            IDone _ -> do
                resR <- initialR
                return $ first SeqAR resR
            IError err -> return $ IError err

    -- Note: For the composed parse to terminate, the left parser has to be
    -- a terminating parser returning a Done at some point.
    step (SeqAL st) a = do
        -- Important: Do not use Applicative here. Applicative somehow caused
        -- the right action to run many times, not sure why though.
        resL <- stepL st a
        case resL of
            -- Note: this leads to buffering even if we are not in an
            -- Alternative composition.
            Partial n s -> return $ Continue n (SeqAL s)
            Continue n s -> return $ Continue n (SeqAL s)
            Done n _ -> do
                initR <- initialR
                return $ case initR of
                    IPartial s -> Continue n (SeqAR s)
                    IDone b -> Done n b
                    IError err -> Error err
            Error err -> return $ Error err

    step (SeqAR st) a = first SeqAR <$> stepR st a

    extract (SeqAR sR) = fmap (first SeqAR) (extractR sR)
    extract (SeqAL sL) = do
        rL <- extractL sL
        case rL of
            Done n _ -> do
                iR <- initialR
                -- XXX For initial we can have a bimap with leftover.
                case iR of
                    IPartial sR ->
                        fmap (bimapOverrideCount n SeqAR id) (extractR sR)
                    IDone bR -> return $ Done n bR
                    IError err -> return $ Error err
            Error err -> return $ Error err
            Partial _ _ -> error "split_: Partial"
            Continue n s -> return $ Continue n (SeqAL s)

-- For backtracking folds
{-# INLINE noErrorUnsafeSplit_ #-}
noErrorUnsafeSplit_ :: Monad m => Parser m x a -> Parser m x b -> Parser m x b
noErrorUnsafeSplit_
    (Parser stepL initialL extractL) (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    errMsg e = error $ "noErrorUnsafeSplit_: unreachable: " ++ e

    initial = do
        resL <- initialL
        case resL of
            IPartial sl -> return $ IPartial $ SeqAL sl
            IDone _ -> do
                resR <- initialR
                return $ first SeqAR resR
            IError err -> errMsg err

    -- Note: For the composed parse to terminate, the left parser has to be
    -- a terminating parser returning a Done at some point.
    step (SeqAL st) a = do
        -- Important: Please do not use Applicative here. Applicative somehow
        -- caused the next action to run many times in the "tar" parsing code,
        -- not sure why though.
        resL <- stepL st a
        case resL of
            Partial n s -> return $ Partial n (SeqAL s)
            Continue n s -> return $ Continue n (SeqAL s)
            Done n _ -> do
                initR <- initialR
                return $ case initR of
                    IPartial s -> Partial n (SeqAR s)
                    IDone b -> Done n b
                    IError err -> errMsg err
            Error err -> errMsg err

    step (SeqAR st) a = first SeqAR <$> stepR st a

    extract (SeqAR sR) = fmap (first SeqAR) (extractR sR)
    extract (SeqAL sL) = do
        rL <- extractL sL
        case rL of
            Done n _ -> do
                iR <- initialR
                case iR of
                    IPartial sR -> do
                        fmap (bimapOverrideCount n SeqAR id) (extractR sR)
                    IDone bR -> return $ Done n bR
                    IError err -> errMsg err
            Error err -> errMsg err
            Partial _ _ -> error "split_: Partial"
            Continue n s -> return $ Continue n (SeqAL s)

-- | 'Applicative' form of 'serialWith'.
instance Monad m => Applicative (Parser m a) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = serialWith id

    {-# INLINE (*>) #-}
    (*>) = split_

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

-------------------------------------------------------------------------------
-- Sequential Alternative
-------------------------------------------------------------------------------

{-# ANN type AltParseState Fuse #-}
data AltParseState sl sr = AltParseL Int sl | AltParseR sr

-- Note: this implementation of alt is fast because of stream fusion but has
-- quadratic time complexity, because each composition adds a new branch that
-- each subsequent alternative's input element has to go through, therefore, it
-- cannot scale to a large number of compositions

-- | See 'Streamly.Internal.Data.Parser.alt'.
--
-- /Time Complexity:/ O(n^2) where n is the number of compositions.
--
-- /Pre-release/
--
{-# INLINE alt #-}
alt :: Monad m => Parser m x a -> Parser m x a -> Parser m x a
alt (Parser stepL initialL extractL) (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    initial = do
        resL <- initialL
        case resL of
            IPartial sl -> return $ IPartial $ AltParseL 0 sl
            IDone bl -> return $ IDone bl
            IError _ -> do
                resR <- initialR
                return $ case resR of
                    IPartial sr -> IPartial $ AltParseR sr
                    IDone br -> IDone br
                    IError err -> IError err

    -- Once a parser yields at least one value it cannot fail.  This
    -- restriction helps us make backtracking more efficient, as we do not need
    -- to keep the consumed items buffered after a yield. Note that we do not
    -- enforce this and if a misbehaving parser does not honor this then we can
    -- get unexpected results. XXX Can we detect and flag this?
    step (AltParseL cnt st) a = do
        r <- stepL st a
        case r of
            Partial n s -> return $ Partial n (AltParseL 0 s)
            Continue n s -> do
                assertM(cnt + 1 - n >= 0)
                return $ Continue n (AltParseL (cnt + 1 - n) s)
            Done n b -> return $ Done n b
            Error _ -> do
                res <- initialR
                return
                    $ case res of
                          IPartial rR -> Continue (cnt + 1) (AltParseR rR)
                          IDone b -> Done (cnt + 1) b
                          IError err -> Error err

    step (AltParseR st) a = do
        r <- stepR st a
        return $ case r of
            Partial n s -> Partial n (AltParseR s)
            Continue n s -> Continue n (AltParseR s)
            Done n b -> Done n b
            Error err -> Error err

    extract (AltParseR sR) = fmap (first AltParseR) (extractR sR)

    extract (AltParseL cnt sL) = do
        rL <- extractL sL
        case rL of
            Done n b -> return $ Done n b
            Error _ -> do
                res <- initialR
                return
                    $ case res of
                          IPartial rR -> Continue cnt (AltParseR rR)
                          IDone b -> Done cnt b
                          IError err -> Error err
            Partial _ _ -> error "Bug: serialWith extract 'Partial'"
            Continue n s -> do
                assertM(n == cnt)
                return $ Continue n (AltParseL 0 s)

-- | See documentation of 'Streamly.Internal.Data.Parser.many'.
--
-- /Pre-release/
--
{-# INLINE splitMany #-}
splitMany :: Monad m =>  Parser m a b -> Fold m b c -> Parser m a c
splitMany (Parser step1 initial1 extract1) (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    -- Caution! There is mutual recursion here, inlining the right functions is
    -- important.

    handleCollect partial done fres =
        case fres of
            FL.Partial fs -> do
                pres <- initial1
                case pres of
                    IPartial ps -> return $ partial $ Tuple3' ps 0 fs
                    IDone pb ->
                        runCollectorWith (handleCollect partial done) fs pb
                    IError _ -> done <$> fextract fs
            FL.Done fb -> return $ done fb

    runCollectorWith cont fs pb = fstep fs pb >>= cont

    initial = finitial >>= handleCollect IPartial IDone

    {-# INLINE step #-}
    step (Tuple3' st cnt fs) a = do
        r <- step1 st a
        let cnt1 = cnt + 1
        case r of
            Partial n s -> do
                assertM(cnt1 - n >= 0)
                return $ Continue n (Tuple3' s (cnt1 - n) fs)
            Continue n s -> do
                assertM(cnt1 - n >= 0)
                return $ Continue n (Tuple3' s (cnt1 - n) fs)
            Done n b -> do
                assertM(cnt1 - n >= 0)
                fstep fs b >>= handleCollect (Partial n) (Done n)
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt1 xs

    extract (Tuple3' _ 0 fs) = fmap (Done 0) (fextract fs)
    extract (Tuple3' s cnt fs) = do
        r <- extract1 s
        case r of
            Error _ -> fmap (Done cnt) (fextract fs)
            Done n b -> do
                assertM(n <= cnt)
                fs1 <- fstep fs b
                case fs1 of
                    FL.Partial s1 -> fmap (Done n) (fextract s1)
                    FL.Done b1 -> return (Done n b1)
            Partial _ _ -> error "splitMany: Partial in extract"
            Continue n s1 -> do
                assertM(n == cnt)
                return (Continue n (Tuple3' s1 0 fs))

-- | Like splitMany, but inner fold emits an output at the end even if no input
-- is received.
--
-- /Internal/
--
{-# INLINE splitManyPost #-}
splitManyPost :: Monad m =>  Parser m a b -> Fold m b c -> Parser m a c
splitManyPost (Parser step1 initial1 extract1) (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    -- Caution! There is mutual recursion here, inlining the right functions is
    -- important.

    handleCollect partial done fres =
        case fres of
            FL.Partial fs -> do
                pres <- initial1
                case pres of
                    IPartial ps -> return $ partial $ Tuple3' ps 0 fs
                    IDone pb ->
                        runCollectorWith (handleCollect partial done) fs pb
                    IError _ -> done <$> fextract fs
            FL.Done fb -> return $ done fb

    runCollectorWith cont fs pb = fstep fs pb >>= cont

    initial = finitial >>= handleCollect IPartial IDone

    {-# INLINE step #-}
    step (Tuple3' st cnt fs) a = do
        r <- step1 st a
        let cnt1 = cnt + 1
        case r of
            Partial n s -> do
                assertM(cnt1 - n >= 0)
                return $ Continue n (Tuple3' s (cnt1 - n) fs)
            Continue n s -> do
                assertM(cnt1 - n >= 0)
                return $ Continue n (Tuple3' s (cnt1 - n) fs)
            Done n b -> do
                assertM(cnt1 - n >= 0)
                fstep fs b >>= handleCollect (Partial n) (Done n)
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt1 xs

    extract (Tuple3' s cnt fs) = do
        r <- extract1 s
        case r of
            Error _ -> fmap (Done cnt) (fextract fs)
            Done n b -> do
                assertM(n <= cnt)
                fs1 <- fstep fs b
                case fs1 of
                    FL.Partial s1 -> fmap (Done n) (fextract s1)
                    FL.Done b1 -> return (Done n b1)
            Partial _ _ -> error "splitMany: Partial in extract"
            Continue n s1 -> do
                assertM(n == cnt)
                return (Continue n (Tuple3' s1 0 fs))

-- | See documentation of 'Streamly.Internal.Data.Parser.some'.
--
-- /Pre-release/
--
{-# INLINE splitSome #-}
splitSome :: Monad m => Parser m a b -> Fold m b c -> Parser m a c
splitSome (Parser step1 initial1 extract1) (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    -- Caution! There is mutual recursion here, inlining the right functions is
    -- important.

    handleCollect partial done fres =
        case fres of
            FL.Partial fs -> do
                pres <- initial1
                case pres of
                    IPartial ps -> return $ partial $ Tuple3' ps 0 $ Right fs
                    IDone pb ->
                        runCollectorWith (handleCollect partial done) fs pb
                    IError _ -> done <$> fextract fs
            FL.Done fb -> return $ done fb

    runCollectorWith cont fs pb = fstep fs pb >>= cont

    initial = do
        fres <- finitial
        case fres of
            FL.Partial fs -> do
                pres <- initial1
                case pres of
                    IPartial ps -> return $ IPartial $ Tuple3' ps 0 $ Left fs
                    IDone pb ->
                        runCollectorWith (handleCollect IPartial IDone) fs pb
                    IError err -> return $ IError err
            FL.Done _ ->
                return
                    $ IError
                    $ "splitSome: The collecting fold terminated without"
                          ++ " consuming any elements."

    {-# INLINE step #-}
    step (Tuple3' st cnt (Left fs)) a = do
        r <- step1 st a
        -- In the Left state, count is used only for the assert
        let cnt1 = cnt + 1
        case r of
            Partial n s -> do
                assertM(cnt1 - n >= 0)
                return $ Continue n (Tuple3' s (cnt1 - n) (Left fs))
            Continue n s -> do
                assertM(cnt1 - n >= 0)
                return $ Continue n (Tuple3' s (cnt1 - n) (Left fs))
            Done n b -> do
                assertM(cnt1 - n >= 0)
                fstep fs b >>= handleCollect (Partial n) (Done n)
            Error err -> return $ Error err
    step (Tuple3' st cnt (Right fs)) a = do
        r <- step1 st a
        let cnt1 = cnt + 1
        case r of
            Partial n s -> do
                assertM(cnt1 - n >= 0)
                return $ Partial n (Tuple3' s (cnt1 - n) (Right fs))
            Continue n s -> do
                assertM(cnt1 - n >= 0)
                return $ Continue n (Tuple3' s (cnt1 - n) (Right fs))
            Done n b -> do
                assertM(cnt1 - n >= 0)
                fstep fs b >>= handleCollect (Partial n) (Done n)
            Error _ -> Done cnt1 <$> fextract fs

    extract (Tuple3' s cnt (Left fs)) = do
        r <- extract1 s
        case r of
            Error err -> return (Error err)
            Done n b -> do
                assertM(n <= cnt)
                fs1 <- fstep fs b
                case fs1 of
                    FL.Partial s1 -> fmap (Done n) (fextract s1)
                    FL.Done b1 -> return (Done n b1)
            Partial _ _ -> error "splitSome: Partial in extract"
            Continue n s1 -> do
                assertM(n == cnt)
                return (Continue n (Tuple3' s1 0 (Left fs)))
    extract (Tuple3' s cnt (Right fs)) = do
        r <- extract1 s
        case r of
            Error _ -> fmap (Done cnt) (fextract fs)
            Done n b -> do
                assertM(n <= cnt)
                fs1 <- fstep fs b
                case fs1 of
                    FL.Partial s1 -> fmap (Done n) (fextract s1)
                    FL.Done b1 -> return (Done n b1)
            Partial _ _ -> error "splitSome: Partial in extract"
            Continue n s1 -> do
                assertM(n == cnt)
                return (Continue n (Tuple3' s1 0 (Right fs)))

-- | See 'Streamly.Internal.Data.Parser.die'.
--
-- /Pre-release/
--
{-# INLINE_NORMAL die #-}
die :: Monad m => String -> Parser m a b
die err = Parser undefined (pure (IError err)) undefined

-- | See 'Streamly.Internal.Data.Parser.dieM'.
--
-- /Pre-release/
--
{-# INLINE dieM #-}
dieM :: Monad m => m String -> Parser m a b
dieM err = Parser undefined (IError <$> err) undefined

-- Note: The default implementations of "some" and "many" loop infinitely
-- because of the strict pattern match on both the arguments in applicative and
-- alternative. With the direct style parser type we cannot use the mutually
-- recursive definitions of "some" and "many".
--
-- Note: With the direct style parser type, the list in "some" and "many" is
-- accumulated strictly, it cannot be consumed lazily.

-- | 'Alternative' instance using 'alt'.
--
-- Note: The implementation of '<|>' is not lazy in the second
-- argument. The following code will fail:
--
-- >>> Stream.parse (Parser.satisfy (> 0) <|> undefined) $ Stream.fromList [1..10]
-- 1
--
instance Monad m => Alternative (Parser m a) where
    {-# INLINE empty #-}
    empty = die "empty"

    {-# INLINE (<|>) #-}
    (<|>) = alt

    {-# INLINE many #-}
    many = flip splitMany toList

    {-# INLINE some #-}
    some = flip splitSome toList

{-# ANN type ConcatParseState Fuse #-}
data ConcatParseState sl m a b =
      ConcatParseL sl
    | forall s. ConcatParseR (s -> a -> m (Step s b)) s (s -> m (Step s b))

-- | See 'Streamly.Internal.Data.Parser.concatMap'.
--
-- /Pre-release/
--
{-# INLINE concatMap #-}
concatMap :: Monad m =>
    (b -> Parser m a c) -> Parser m a b -> Parser m a c
concatMap func (Parser stepL initialL extractL) = Parser step initial extract

    where

    {-# INLINE initializeR #-}
    initializeR (Parser stepR initialR extractR) = do
        resR <- initialR
        return $ case resR of
            IPartial sr -> IPartial $ ConcatParseR stepR sr extractR
            IDone br -> IDone br
            IError err -> IError err

    initial = do
        res <- initialL
        case res of
            IPartial s -> return $ IPartial $ ConcatParseL s
            IDone b -> initializeR (func b)
            IError err -> return $ IError err

    {-# INLINE initializeRL #-}
    initializeRL n (Parser stepR initialR extractR) = do
        resR <- initialR
        return $ case resR of
            IPartial sr -> Continue n $ ConcatParseR stepR sr extractR
            IDone br -> Done n br
            IError err -> Error err

    step (ConcatParseL st) a = do
        r <- stepL st a
        case r of
            Partial n s -> return $ Continue n (ConcatParseL s)
            Continue n s -> return $ Continue n (ConcatParseL s)
            Done n b -> initializeRL n (func b)
            Error err -> return $ Error err

    step (ConcatParseR stepR st extractR) a = do
        r <- stepR st a
        return $ case r of
            Partial n s -> Partial n $ ConcatParseR stepR s extractR
            Continue n s -> Continue n $ ConcatParseR stepR s extractR
            Done n b -> Done n b
            Error err -> Error err

    {-# INLINE extractP #-}
    extractP n (Parser stepR initialR extractR) = do
        res <- initialR
        case res of
            IPartial s ->
                fmap
                    (first (\s1 -> ConcatParseR stepR s1 extractR))
                    (extractR s)
            IDone b -> return (Done n b)
            IError err -> return $ Error err

    extract (ConcatParseR stepR s extractR) =
        fmap (first (\s1 -> ConcatParseR stepR s1 extractR)) (extractR s)
    extract (ConcatParseL sL) = do
        rL <- extractL sL
        case rL of
            Error err -> return $ Error err
            Done n b -> extractP n $ func b
            Partial _ _ -> error "concatMap: extract Partial"
            Continue n s -> return $ Continue n (ConcatParseL s)

{-# INLINE noErrorUnsafeConcatMap #-}
noErrorUnsafeConcatMap :: Monad m =>
    (b -> Parser m a c) -> Parser m a b -> Parser m a c
noErrorUnsafeConcatMap func (Parser stepL initialL extractL) =
    Parser step initial extract

    where

    {-# INLINE initializeR #-}
    initializeR (Parser stepR initialR extractR) = do
        resR <- initialR
        return $ case resR of
            IPartial sr -> IPartial $ ConcatParseR stepR sr extractR
            IDone br -> IDone br
            IError err -> IError err

    initial = do
        res <- initialL
        case res of
            IPartial s -> return $ IPartial $ ConcatParseL s
            IDone b -> initializeR (func b)
            IError err -> return $ IError err

    {-# INLINE initializeRL #-}
    initializeRL n (Parser stepR initialR extractR) = do
        resR <- initialR
        return $ case resR of
            IPartial sr -> Partial n $ ConcatParseR stepR sr extractR
            IDone br -> Done n br
            IError err -> Error err

    step (ConcatParseL st) a = do
        r <- stepL st a
        case r of
            Partial n s -> return $ Partial n (ConcatParseL s)
            Continue n s -> return $ Continue n (ConcatParseL s)
            Done n b -> initializeRL n (func b)
            Error err -> return $ Error err

    step (ConcatParseR stepR st extractR) a = do
        r <- stepR st a
        return $ case r of
            Partial n s -> Partial n $ ConcatParseR stepR s extractR
            Continue n s -> Continue n $ ConcatParseR stepR s extractR
            Done n b -> Done n b
            Error err -> Error err

    {-# INLINE extractP #-}
    extractP n (Parser stepR initialR extractR) = do
        res <- initialR
        case res of
            IPartial s ->
                fmap
                    (first (\s1 -> ConcatParseR stepR s1 extractR))
                    (extractR s)
            IDone b -> return (Done n b)
            IError err -> return $ Error err

    extract (ConcatParseR stepR s extractR) =
        fmap (first (\s1 -> ConcatParseR stepR s1 extractR)) (extractR s)
    extract (ConcatParseL sL) = do
        rL <- extractL sL
        case rL of
            Error err -> return $ Error err
            Done n b -> extractP n $ func b
            Partial _ _ -> error "concatMap: extract Partial"
            Continue n s -> return $ Continue n (ConcatParseL s)

-- Note: The monad instance has quadratic performance complexity. It works fine
-- for small number of compositions but for a scalable implementation we need a
-- CPS version.

-- | See documentation of 'Streamly.Internal.Data.Parser.ParserK.Type.Parser'.
--
instance Monad m => Monad (Parser m a) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = flip concatMap

    {-# INLINE (>>) #-}
    (>>) = (*>)

-- | See documentation of 'Streamly.Internal.Data.Parser.ParserK.Type.Parser'.
--
instance Monad m => MonadPlus (Parser m a) where
    {-# INLINE mzero #-}
    mzero = die "mzero"

    {-# INLINE mplus #-}
    mplus = alt

instance (Monad m, MonadReader r m) => MonadReader r (Parser m a) where
    {-# INLINE ask #-}
    ask = fromEffect ask

    {-# INLINE local #-}
    local f (Parser step init' extract) =
      Parser ((local f .) . step)
             (local f init')
             (local f . extract)

instance (Monad m, MonadState s m) => MonadState s (Parser m a) where
    {-# INLINE get #-}
    get = fromEffect get

    {-# INLINE put #-}
    put = fromEffect . put

instance (Monad m, MonadIO m) => MonadIO (Parser m a) where
    {-# INLINE liftIO #-}
    liftIO = fromEffect . liftIO

------------------------------------------------------------------------------
-- Mapping on input
------------------------------------------------------------------------------

{-# INLINE lmap #-}
lmap :: (a -> b) -> Parser m b r -> Parser m a r
lmap f (Parser step begin done) = Parser step1 begin done

    where

    step1 x a = step x (f a)

{-# INLINE lmapM #-}
lmapM :: Monad m => (a -> m b) -> Parser m b r -> Parser m a r
lmapM f (Parser step begin done) = Parser step1 begin done

    where

    step1 x a = f a >>= step x

{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Parser m a b -> Parser m a b
filter f (Parser step initial extract) = Parser step1 initial extract

    where

    step1 x a = if f a then step x a else return $ Partial 0 x
