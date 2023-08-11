{-# LANGUAGE CPP #-}
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
--
-- == Naming
--
-- As far as possible, try that the names of the combinators in this module are
-- consistent with:
--
-- * <https://hackage.haskell.org/package/base/docs/Text-ParserCombinators-ReadP.html base/Text.ParserCombinators.ReadP>
-- * <http://hackage.haskell.org/package/parser-combinators parser-combinators>
-- * <http://hackage.haskell.org/package/megaparsec megaparsec>
-- * <http://hackage.haskell.org/package/attoparsec attoparsec>
-- * <http://hackage.haskell.org/package/parsec parsec>

module Streamly.Internal.Data.Parser.Type
    (
    -- * Setup
    -- $setup

    -- * Types
      Initial (..)
    , Step (..)
    , extractStep
    , bimapOverrideCount
    , Parser (..)
    , ParseError (..)
    , rmapM

    -- * Constructors

    , fromPure
    , fromEffect
    , splitWith
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

#include "inline.hs"
#include "assert.hs"

import Control.Applicative (Alternative(..), liftA2)
import Control.Exception (Exception(..))
-- import Control.Monad (MonadPlus(..), (>=>))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (Bifunctor(..))
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Fold.Type (Fold(..), toList)

import qualified Control.Monad.Fail as Fail
import qualified Streamly.Internal.Data.Fold.Type as FL

import Prelude hiding (concatMap, filter)

#include "DocTestDataParser.hs"

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
data Parser a m b =
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

instance Functor m => Functor (Parser a m) where
    {-# INLINE fmap #-}
    fmap f (Parser step1 initial1 extract) =
        Parser step initial (fmap3 f extract)

        where

        initial = fmap2 f initial1
        step s b = fmap2 f (step1 s b)
        fmap2 g = fmap (fmap g)
        fmap3 g = fmap2 (fmap g)

------------------------------------------------------------------------------
-- Mapping on the output
------------------------------------------------------------------------------

-- | @rmapM f parser@ maps the monadic function @f@ on the output of the parser.
--
-- >>> rmap = fmap
{-# INLINE rmapM #-}
rmapM :: Monad m => (b -> m c) -> Parser a m b -> Parser a m c
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

-- | A parser that always yields a pure value without consuming any input.
--
{-# INLINE_NORMAL fromPure #-}
fromPure :: Monad m => b -> Parser a m b
fromPure b = Parser undefined (pure $ IDone b) undefined

-- | A parser that always yields the result of an effectful action without
-- consuming any input.
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> Parser a m b
fromEffect b = Parser undefined (IDone <$> b) undefined

-------------------------------------------------------------------------------
-- Sequential applicative
-------------------------------------------------------------------------------

{-# ANN type SeqParseState Fuse #-}
data SeqParseState sl f sr = SeqParseL !sl | SeqParseR !f !sr

-- Note: this implementation of splitWith is fast because of stream fusion but
-- has quadratic time complexity, because each composition adds a new branch
-- that each subsequent parse's input element has to go through, therefore, it
-- cannot scale to a large number of compositions. After around 100
-- compositions the performance starts dipping rapidly beyond a CPS style
-- unfused implementation.
--
-- Note: This is a parsing dual of appending streams using
-- 'Streamly.Data.Stream.append', it splits the streams using two parsers and
-- zips the results.

-- | Sequential parser application.
--
-- Apply two parsers sequentially to an input stream. The first parser runs and
-- processes the input, the remaining input is then passed to the second
-- parser. If both parsers succeed, their outputs are combined using the
-- supplied function. If either parser fails, the operation fails.
--
-- This implementation is strict in the second argument, therefore, the
-- following will fail:
--
-- >>> Stream.parse (Parser.splitWith const (Parser.satisfy (> 0)) undefined) $ Stream.fromList [1]
-- *** Exception: Prelude.undefined
-- ...
--
-- Although this implementation allows stream fusion, it has quadratic
-- complexity, making it suitable only for a small number of compositions.
-- As a thumb rule use it for less than 8 compositions, use ParserK otherwise.
--
-- Below are some common idioms that can be expressed using 'splitWith' and
-- other parser primitives:
--
-- >>> span p f1 f2 = Parser.splitWith (,) (Parser.takeWhile p f1) (Parser.fromFold f2)
-- >>> spanBy eq f1 f2 = Parser.splitWith (,) (Parser.groupBy eq f1) (Parser.fromFold f2)
--
-- /Pre-release/
--
{-# INLINE splitWith #-}
splitWith :: Monad m
    => (a -> b -> c) -> Parser x m a -> Parser x m b -> Parser x m c
splitWith func (Parser stepL initialL extractL)
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
            Partial _ _ -> error "Bug: splitWith extract 'Partial'"
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
    => (a -> b -> c) -> Parser x m a -> Parser x m b -> Parser x m c
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
data SeqAState sl sr = SeqAL !sl | SeqAR !sr

-- This turns out to be slightly faster than splitWith

-- | Sequential parser application ignoring the output of the first parser.
-- Apply two parsers sequentially to an input stream.  The input is provided to
-- the first parser, when it is done the remaining input is provided to the
-- second parser. The output of the parser is the output of the second parser.
-- The operation fails if any of the parsers fail.
--
-- This implementation is strict in the second argument, therefore, the
-- following will fail:
--
-- >>> Stream.parse (Parser.split_ (Parser.satisfy (> 0)) undefined) $ Stream.fromList [1]
-- *** Exception: Prelude.undefined
-- ...
--
-- Although this implementation allows stream fusion, it has quadratic
-- complexity, making it suitable only for a small number of compositions.
-- As a thumb rule use it for less than 8 compositions, use ParserK otherwise.
--
-- /Pre-release/
--
{-# INLINE split_ #-}
split_ :: Monad m => Parser x m a -> Parser x m b -> Parser x m b
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
noErrorUnsafeSplit_ :: Monad m => Parser x m a -> Parser x m b -> Parser x m b
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

-- | 'Applicative' form of 'splitWith'.
instance Monad m => Applicative (Parser a m) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = splitWith id

    {-# INLINE (*>) #-}
    (*>) = split_

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

-------------------------------------------------------------------------------
-- Sequential Alternative
-------------------------------------------------------------------------------

{-# ANN type AltParseState Fuse #-}
data AltParseState sl sr = AltParseL !Int !sl | AltParseR !sr

-- Note: this implementation of alt is fast because of stream fusion but has
-- quadratic time complexity, because each composition adds a new branch that
-- each subsequent alternative's input element has to go through, therefore, it
-- cannot scale to a large number of compositions

-- | Sequential alternative. The input is first passed to the first parser, and
-- if it succeeds, the result is returned. However, if the first parser fails,
-- the parser driver backtracks and tries the same input on the second parser,
-- returning the result if it succeeds.
--
-- Note: This implementation is not lazy in the second argument. The following
-- will fail:
--
-- >> Stream.parse (Parser.satisfy (> 0) `Parser.alt` undefined) $ Stream.fromList [1..10]
-- *** Exception: Prelude.undefined
--
-- Although this implementation allows stream fusion, it has quadratic
-- complexity, making it suitable only for a small number of compositions.
-- As a thumb rule use it for less than 8 compositions, use ParserK otherwise.
--
-- /Time Complexity:/ O(n^2) where n is the number of compositions.
--
-- /Pre-release/
--
{-# INLINE alt #-}
alt :: Monad m => Parser x m a -> Parser x m a -> Parser x m a
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
            Partial _ _ -> error "Bug: alt: extractL 'Partial'"
            Continue n s -> do
                assertM(n == cnt)
                return $ Continue n (AltParseL 0 s)

{-# ANN type Fused3 Fuse #-}
data Fused3 a b c = Fused3 !a !b !c

-- | See documentation of 'Streamly.Internal.Data.Parser.many'.
--
-- /Pre-release/
--
{-# INLINE splitMany #-}
splitMany :: Monad m => Parser a m b -> Fold m b c -> Parser a m c
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
                    IPartial ps -> return $ partial $ Fused3 ps 0 fs
                    IDone pb ->
                        runCollectorWith (handleCollect partial done) fs pb
                    IError _ -> done <$> fextract fs
            FL.Done fb -> return $ done fb

    runCollectorWith cont fs pb = fstep fs pb >>= cont

    -- See notes in Fold.many for the reason why the parser must be initialized
    -- right away instead of on first input.
    initial = finitial >>= handleCollect IPartial IDone

    {-# INLINE step #-}
    step (Fused3 st cnt fs) a = do
        r <- step1 st a
        let cnt1 = cnt + 1
        case r of
            Partial n s -> do
                assertM(cnt1 - n >= 0)
                return $ Continue n (Fused3 s (cnt1 - n) fs)
            Continue n s -> do
                assertM(cnt1 - n >= 0)
                return $ Continue n (Fused3 s (cnt1 - n) fs)
            Done n b -> do
                assertM(cnt1 - n >= 0)
                fstep fs b >>= handleCollect (Partial n) (Done n)
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt xs

    extract (Fused3 _ 0 fs) = fmap (Done 0) (fextract fs)
    extract (Fused3 s cnt fs) = do
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
                return (Continue n (Fused3 s1 0 fs))

-- | Like splitMany, but inner fold emits an output at the end even if no input
-- is received.
--
-- /Internal/
--
{-# INLINE splitManyPost #-}
splitManyPost :: Monad m =>  Parser a m b -> Fold m b c -> Parser a m c
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
                    IPartial ps -> return $ partial $ Fused3 ps 0 fs
                    IDone pb ->
                        runCollectorWith (handleCollect partial done) fs pb
                    IError _ -> done <$> fextract fs
            FL.Done fb -> return $ done fb

    runCollectorWith cont fs pb = fstep fs pb >>= cont

    initial = finitial >>= handleCollect IPartial IDone

    {-# INLINE step #-}
    step (Fused3 st cnt fs) a = do
        r <- step1 st a
        let cnt1 = cnt + 1
        case r of
            Partial n s -> do
                assertM(cnt1 - n >= 0)
                return $ Continue n (Fused3 s (cnt1 - n) fs)
            Continue n s -> do
                assertM(cnt1 - n >= 0)
                return $ Continue n (Fused3 s (cnt1 - n) fs)
            Done n b -> do
                assertM(cnt1 - n >= 0)
                fstep fs b >>= handleCollect (Partial n) (Done n)
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt1 xs

    extract (Fused3 s cnt fs) = do
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
                return (Continue n (Fused3 s1 0 fs))

-- | See documentation of 'Streamly.Internal.Data.Parser.some'.
--
-- /Pre-release/
--
{-# INLINE splitSome #-}
splitSome :: Monad m => Parser a m b -> Fold m b c -> Parser a m c
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
                    IPartial ps -> return $ partial $ Fused3 ps 0 $ Right fs
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
                    IPartial ps -> return $ IPartial $ Fused3 ps 0 $ Left fs
                    IDone pb ->
                        runCollectorWith (handleCollect IPartial IDone) fs pb
                    IError err -> return $ IError err
            FL.Done _ ->
                return
                    $ IError
                    $ "splitSome: The collecting fold terminated without"
                          ++ " consuming any elements."

    {-# INLINE step #-}
    step (Fused3 st cnt (Left fs)) a = do
        r <- step1 st a
        -- In the Left state, count is used only for the assert
        let cnt1 = cnt + 1
        case r of
            Partial n s -> do
                assertM(cnt1 - n >= 0)
                return $ Continue n (Fused3 s (cnt1 - n) (Left fs))
            Continue n s -> do
                assertM(cnt1 - n >= 0)
                return $ Continue n (Fused3 s (cnt1 - n) (Left fs))
            Done n b -> do
                assertM(cnt1 - n >= 0)
                fstep fs b >>= handleCollect (Partial n) (Done n)
            Error err -> return $ Error err
    step (Fused3 st cnt (Right fs)) a = do
        r <- step1 st a
        let cnt1 = cnt + 1
        case r of
            Partial n s -> do
                assertM(cnt1 - n >= 0)
                return $ Partial n (Fused3 s (cnt1 - n) (Right fs))
            Continue n s -> do
                assertM(cnt1 - n >= 0)
                return $ Continue n (Fused3 s (cnt1 - n) (Right fs))
            Done n b -> do
                assertM(cnt1 - n >= 0)
                fstep fs b >>= handleCollect (Partial n) (Done n)
            Error _ -> Done cnt1 <$> fextract fs

    extract (Fused3 s cnt (Left fs)) = do
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
                return (Continue n (Fused3 s1 0 (Left fs)))
    extract (Fused3 s cnt (Right fs)) = do
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
                return (Continue n (Fused3 s1 0 (Right fs)))

-- | A parser that always fails with an error message without consuming
-- any input.
--
{-# INLINE_NORMAL die #-}
die :: Monad m => String -> Parser a m b
die err = Parser undefined (pure (IError err)) undefined

-- | A parser that always fails with an effectful error message and without
-- consuming any input.
--
-- /Pre-release/
--
{-# INLINE dieM #-}
dieM :: Monad m => m String -> Parser a m b
dieM err = Parser undefined (IError <$> err) undefined

-- Note: The default implementations of "some" and "many" loop infinitely
-- because of the strict pattern match on both the arguments in applicative and
-- alternative. With the direct style parser type we cannot use the mutually
-- recursive definitions of "some" and "many".
--
-- Note: With the direct style parser type, the list in "some" and "many" is
-- accumulated strictly, it cannot be consumed lazily.

-- | Sequential alternative. The input is first passed to the first parser, and
-- if it succeeds, the result is returned. However, if the first parser fails,
-- the parser driver backtracks and tries the same input on the second parser,
-- returning the result if it succeeds.
--
-- Note: The implementation of '<|>' is not lazy in the second
-- argument. The following code will fail:
--
-- >>> Stream.parse (Parser.satisfy (> 0) <|> undefined) $ Stream.fromList [1..10]
-- *** Exception: Prelude.undefined
-- ...
--
-- WARNING! this is not suitable for large scale use. As a thumb rule stream
-- fusion works well for less than 8 compositions of this operation, otherwise
-- consider using 'ParserK'. Do not use recursive parser implementations based
-- on this Alternative instance.

instance Monad m => Alternative (Parser a m) where
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
      ConcatParseL !sl
    | forall s. ConcatParseR (s -> a -> m (Step s b)) s (s -> m (Step s b))

-- | Map a 'Parser' returning function on the result of a 'Parser'.
--
-- /Pre-release/
--
{-# INLINE concatMap #-}
concatMap :: Monad m =>
    (b -> Parser a m c) -> Parser a m b -> Parser a m c
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
    (b -> Parser a m c) -> Parser a m b -> Parser a m c
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
-- Although this implementation allows stream fusion, it has quadratic
-- complexity, making it suitable only for a small number of compositions. As a
-- thumb rule use it for less than 8 compositions, use 'ParserK' otherwise.
--
instance Monad m => Monad (Parser a m) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = flip concatMap

    {-# INLINE (>>) #-}
    (>>) = (*>)

instance Monad m => Fail.MonadFail (Parser a m) where
    {-# INLINE fail #-}
    fail = die

{-
-- | See documentation of 'Streamly.Internal.Data.Parser.ParserK.Type.Parser'.
--
instance Monad m => MonadPlus (Parser a m) where
    {-# INLINE mzero #-}
    mzero = die "mzero"

    {-# INLINE mplus #-}
    mplus = alt
-}

instance (MonadIO m) => MonadIO (Parser a m) where
    {-# INLINE liftIO #-}
    liftIO = fromEffect . liftIO

------------------------------------------------------------------------------
-- Mapping on input
------------------------------------------------------------------------------

-- | @lmap f parser@ maps the function @f@ on the input of the parser.
--
-- >>> Stream.parse (Parser.lmap (\x -> x * x) (Parser.fromFold Fold.sum)) (Stream.enumerateFromTo 1 100)
-- Right 338350
--
-- > lmap = Parser.lmapM return
--
{-# INLINE lmap #-}
lmap :: (a -> b) -> Parser b m r -> Parser a m r
lmap f (Parser step begin done) = Parser step1 begin done

    where

    step1 x a = step x (f a)

-- | @lmapM f parser@ maps the monadic function @f@ on the input of the parser.
--
{-# INLINE lmapM #-}
lmapM :: Monad m => (a -> m b) -> Parser b m r -> Parser a m r
lmapM f (Parser step begin done) = Parser step1 begin done

    where

    step1 x a = f a >>= step x

-- | Include only those elements that pass a predicate.
--
-- >>> Stream.parse (Parser.filter (> 5) (Parser.fromFold Fold.sum)) $ Stream.fromList [1..10]
-- Right 40
--
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Parser a m b -> Parser a m b
filter f (Parser step initial extract) = Parser step1 initial extract

    where

    step1 x a = if f a then step x a else return $ Partial 0 x
