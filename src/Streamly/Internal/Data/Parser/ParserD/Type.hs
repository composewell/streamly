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
-- it considered successful and cannot fail after that. In the current
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
    , Parser (..)
    , ParseError (..)
    , rmapM

    -- * Constructors
    , parser
    , parserM

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

    , noErrorUnsafeSplit_
    , noErrorUnsafeSplitWith
    , noErrorUnsafeConcatMap
    )
where

import Control.Applicative (Alternative(..), liftA2)
import Control.Exception (assert, Exception(..))
import Control.Monad (MonadPlus(..), (>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Catch (MonadCatch, try, throwM, MonadThrow)
import Data.Bifunctor (Bifunctor(..))
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Fold.Type (Fold(..), toList)
import Streamly.Internal.Data.Tuple.Strict (Tuple3'(..))

import qualified Streamly.Internal.Data.Fold.Type as FL

import Prelude hiding (concatMap)
--
-- $setup
-- >>> :m
-- >>> import Control.Applicative ((<|>))
-- >>> import Prelude hiding (concatMap)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream (parse)
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
    | IError String -- ^ Return an error right away without an input.

-- | @first@ maps on 'IPartial' and @second@ maps on 'IDone'.
--
-- /Internal/
--
instance Bifunctor Initial where
    {-# INLINE bimap #-}
    bimap f _ (IPartial a) = IPartial (f a)
    bimap _ g (IDone b) = IDone (g b)
    bimap _ _ (IError err) = IError err

    {-# INLINE first #-}
    first f (IPartial a) = IPartial (f a)
    first _ (IDone x) = IDone x
    first _ (IError err) = IError err

    {-# INLINE second #-}
    second _ (IPartial x) = IPartial x
    second f (IDone a) = IDone (f a)
    second _ (IError err) = IError err

-- | Maps a function over the result held by 'IDone'.
--
-- @
-- fmap = 'second'
-- @
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
-- serveral successful partial results the last segment parsing failed and we
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
        Partial Int s
    -- ^ Partial result with an optional backtrack request.
    --
    -- @Partial count state@ means a partial result is available which
    -- can be extracted successfully, @state@ is the opaque state of the
    -- parser to be supplied to the next invocation of the step operation.
    -- The current input position is reset to @count@ elements back and any
    -- input before that is dropped from the backtrack buffer.

    | Continue Int s
    -- ^ Need more input with an optional backtrack request.
    --
    -- @Continue count state@ means the parser has consumed the current input
    -- but no new result is generated, @state@ is the next state of the parser.
    -- The current input is retained in the backtrack buffer and the input
    -- position is reset to @count@ elements back.

    | Done Int b
    -- ^ Done with leftover input count and result.
    --
    -- @Done count result@ means the parser has finished, it will accept no
    -- more input, last @count@ elements from the input are unused and the
    -- result of the parser is in @result@.

    | Error String
    -- ^ Parser failed without generating any output.
    --
    -- The parsing operation may backtrack to the beginning and try another
    -- alternative.

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap _ (Partial n s) = Partial n s
    fmap _ (Continue n s) = Continue n s
    fmap f (Done n b) = Done n (f b)
    fmap _ (Error err) = Error err

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
    forall s. Parser (s -> a -> m (Step s b)) (m (Initial s b)) (s -> m b)

-- | This exception is used for two purposes:
--
-- * When a parser ultimately fails, the user of the parser is intimated via
--    this exception.
-- * When the "extract" function of a parser needs to throw an error.
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
        Parser step initial (fmap2 f extract)

        where

        initial = fmap2 f initial1
        step s b = fmap2 f (step1 s b)
        fmap2 g = fmap (fmap g)

------------------------------------------------------------------------------
-- General parser constructors
------------------------------------------------------------------------------

-- | Make a parser using a parsing step and a starting value.
--
-- /Pre-release/
--
{-# INLINE parser #-}
parser :: Monad m => (b -> a -> Step b b) -> Initial b b -> Parser m a b
parser step initial =
    Parser (\s a -> return $ step s a) (return initial) return

-- | Like 'parser' but with a monadic step function.
--
-- >>> parserM step initial = ParserD.Parser step initial return
--
-- /Pre-release/
--
{-# INLINE parserM #-}
parserM ::
    Monad m => (b -> a -> m (Step b b)) -> m (Initial b b) -> Parser m a b
parserM step initial = Parser step initial return

------------------------------------------------------------------------------
-- Mapping on the output
------------------------------------------------------------------------------

-- | Map a monadic function on the output of a parser.
--
-- /Pre-release/
{-# INLINE rmapM #-}
rmapM :: Monad m => (b -> m c) -> Parser m a b -> Parser m a c
rmapM f (Parser step initial extract) = Parser step1 initial1 (extract >=> f)

    where

    initial1 = do
        res <- initial
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
serialWith :: MonadThrow m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
serialWith func (Parser stepL initialL extractL)
               (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    initial = do
        resL <- initialL
        case resL of
            IPartial sl -> return $ IPartial $ SeqParseL sl
            IDone bl -> do
                resR <- initialR
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
        resL <- stepL st a
        case resL of
            -- Note: We need to buffer the input for a possible Alternative
            -- e.g. in ((,) <$> p1 <*> p2) <|> p3, if p2 fails we have to
            -- backtrack and start running p3. So we need to keep the input
            -- buffered until we know that the applicative cannot fail.
            Partial n s -> return $ Continue n (SeqParseL s)
            Continue n s -> return $ Continue n (SeqParseL s)
            Done n b -> do
                initR <- initialR
                return $ case initR of
                   IPartial sr -> Continue n $ SeqParseR (func b) sr
                   IDone br -> Done n (func b br)
                   IError err -> Error err
            Error err -> return $ Error err

    step (SeqParseR f st) a = do
        resR <- stepR st a
        return $ case resR of
            Partial n s -> Partial n (SeqParseR f s)
            Continue n s -> Continue n (SeqParseR f s)
            Done n b -> Done n (f b)
            Error err -> Error err

    extract (SeqParseR f sR) = fmap f (extractR sR)
    extract (SeqParseL sL) = do
        rL <- extractL sL
        res <- initialR
        case res of
            IPartial sR -> do
                rR <- extractR sR
                return $ func rL rR
            IDone rR -> return $ func rL rR
            IError err -> throwM $ ParseError err

-- | Works correctly only if the first parser is guaranteed to never fail.
{-# INLINE noErrorUnsafeSplitWith #-}
noErrorUnsafeSplitWith :: Monad m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
noErrorUnsafeSplitWith func (Parser stepL initialL extractL)
               (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    initial = do
        resL <- initialL
        case resL of
            IPartial sl -> return $ IPartial $ SeqParseL sl
            IDone bl -> do
                resR <- initialR
                return $ case resR of
                    IPartial sr -> IPartial $ SeqParseR (func bl) sr
                    IDone br -> IDone (func bl br)
                    IError err -> IError err
            IError err -> return $ IError err

    -- Note: For the composed parse to terminate, the left parser has to be
    -- a terminating parser returning a Done at some point.
    step (SeqParseL st) a = do
        r <- stepL st a
        case r of
            -- Assume that the first parser can never fail, therefore we do not
            -- need to keep the input for backtracking.
            Partial n s -> return $ Partial n (SeqParseL s)
            Continue n s -> return $ Continue n (SeqParseL s)
            Done n b -> do
                res <- initialR
                return
                    $ case res of
                          IPartial sr -> Partial n $ SeqParseR (func b) sr
                          IDone br -> Done n (func b br)
                          IError err -> Error err
            Error err -> return $ Error err

    step (SeqParseR f st) a = do
        r <- stepR st a
        return $ case r of
            Partial n s -> Partial n (SeqParseR f s)
            Continue n s -> Continue n (SeqParseR f s)
            Done n b -> Done n (f b)
            Error err -> Error err

    extract (SeqParseR f sR) = fmap f (extractR sR)
    extract (SeqParseL sL) = do
        rL <- extractL sL
        res <- initialR
        case res of
            IPartial sR -> do
                rR <- extractR sR
                return $ func rL rR
            IDone rR -> return $ func rL rR
            IError err -> error $ "noErrorUnsafeSplitWith: cannot use a "
                ++ "failing parser. Parser failed with: " ++ err

{-# ANN type SeqAState Fuse #-}
data SeqAState sl sr = SeqAL sl | SeqAR sr

-- This turns out to be slightly faster than serialWith
-- | See 'Streamly.Internal.Data.Parser.split_'.
--
-- /Pre-release/
--
{-# INLINE split_ #-}
split_ :: MonadThrow m => Parser m x a -> Parser m x b -> Parser m x b
split_ (Parser stepL initialL extractL) (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    initial = do
        resL <- initialL
        case resL of
            IPartial sl -> return $ IPartial $ SeqAL sl
            IDone _ -> do
                resR <- initialR
                return $ case resR of
                    IPartial sr -> IPartial $ SeqAR sr
                    IDone br -> IDone br
                    IError err -> IError err
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

    step (SeqAR st) a =
        (\case
            Partial n s -> Partial n (SeqAR s)
            Continue n s -> Continue n (SeqAR s)
            Done n b -> Done n b
            Error err -> Error err) <$> stepR st a

    extract (SeqAR sR) = extractR sR
    extract (SeqAL sL) = do
        _ <- extractL sL
        res <- initialR
        case res of
            IPartial sR -> extractR sR
            IDone rR -> return rR
            IError err -> throwM $ ParseError err

{-# INLINE noErrorUnsafeSplit_ #-}
noErrorUnsafeSplit_ :: MonadThrow m => Parser m x a -> Parser m x b -> Parser m x b
noErrorUnsafeSplit_ (Parser stepL initialL extractL) (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    initial = do
        resL <- initialL
        case resL of
            IPartial sl -> return $ IPartial $ SeqAL sl
            IDone _ -> do
                resR <- initialR
                return $ case resR of
                    IPartial sr -> IPartial $ SeqAR sr
                    IDone br -> IDone br
                    IError err -> IError err
            IError err -> return $ IError err

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
                    IError err -> Error err
            Error err -> return $ Error err

    step (SeqAR st) a =
        (\case
            Partial n s -> Partial n (SeqAR s)
            Continue n s -> Continue n (SeqAR s)
            Done n b -> Done n b
            Error err -> Error err) <$> stepR st a

    extract (SeqAR sR) = extractR sR
    extract (SeqAL sL) = do
        _ <- extractL sL
        res <- initialR
        case res of
            IPartial sR -> extractR sR
            IDone rR -> return rR
            IError err -> throwM $ ParseError err

-- | 'Applicative' form of 'serialWith'.
instance MonadThrow m => Applicative (Parser m a) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = serialWith id

    {-# INLINE (*>) #-}
    (*>) = split_

#if MIN_VERSION_base(4,10,0)
    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)
#endif

-------------------------------------------------------------------------------
-- Sequential Alternative
-------------------------------------------------------------------------------

{-# ANN type AltParseState Fuse #-}
data AltParseState sl sr = AltParseL Int sl | AltParseR sr

-- Note: this implementation of alt is fast because of stream fusion but has
-- quadratic time complexity, because each composition adds a new branch that
-- each subsequent alternative's input element has to go through, therefore, it
-- cannot scale to a large number of compositions
--
-- | See 'Streamly.Internal.Data.Parser.alt'.
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
    -- get unexpected results.
    step (AltParseL cnt st) a = do
        r <- stepL st a
        case r of
            Partial n s -> return $ Partial n (AltParseL 0 s)
            Continue n s -> do
                assert (cnt + 1 - n >= 0) (return ())
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

    extract (AltParseR sR) = extractR sR
    extract (AltParseL _ sL) = extractL sL

-- | See documentation of 'Streamly.Internal.Data.Parser.many'.
--
-- /Pre-release/
--
{-# INLINE splitMany #-}
splitMany :: MonadCatch m =>  Parser m a b -> Fold m b c -> Parser m a c
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
                assert (cnt1 - n >= 0) (return ())
                return $ Continue n (Tuple3' s (cnt1 - n) fs)
            Continue n s -> do
                assert (cnt1 - n >= 0) (return ())
                return $ Continue n (Tuple3' s (cnt1 - n) fs)
            Done n b -> do
                assert (cnt1 - n >= 0) (return ())
                fstep fs b >>= handleCollect (Partial n) (Done n)
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt1 xs

    extract (Tuple3' _ 0 fs) = fextract fs
    -- XXX The "try" may impact performance if this parser is used as a scan
    extract (Tuple3' s _ fs) = do
        r <- try $ extract1 s
        case r of
            Left (_ :: ParseError) -> fextract fs
            Right b -> do
                fs1 <- fstep fs b
                case fs1 of
                    FL.Partial s1 -> fextract s1
                    FL.Done b1 -> return b1

-- | Like splitMany, but inner fold emits an output at the end even if no input
-- is received.
--
-- /Internal/
--
{-# INLINE splitManyPost #-}
splitManyPost :: MonadCatch m =>  Parser m a b -> Fold m b c -> Parser m a c
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
                assert (cnt1 - n >= 0) (return ())
                return $ Continue n (Tuple3' s (cnt1 - n) fs)
            Continue n s -> do
                assert (cnt1 - n >= 0) (return ())
                return $ Continue n (Tuple3' s (cnt1 - n) fs)
            Done n b -> do
                assert (cnt1 - n >= 0) (return ())
                fstep fs b >>= handleCollect (Partial n) (Done n)
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt1 xs

    -- XXX The "try" may impact performance if this parser is used as a scan
    extract (Tuple3' s _ fs) = do
        r <- try $ extract1 s
        case r of
            Left (_ :: ParseError) -> fextract fs
            Right b -> do
                fs1 <- fstep fs b
                case fs1 of
                    FL.Partial s1 -> fextract s1
                    FL.Done b1 -> return b1

-- | See documentation of 'Streamly.Internal.Data.Parser.some'.
--
-- /Pre-release/
--
{-# INLINE splitSome #-}
splitSome :: MonadCatch m => Parser m a b -> Fold m b c -> Parser m a c
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
                assert (cnt1 - n >= 0) (return ())
                return $ Continue n (Tuple3' s (cnt1 - n) (Left fs))
            Continue n s -> do
                assert (cnt1 - n >= 0) (return ())
                return $ Continue n (Tuple3' s (cnt1 - n) (Left fs))
            Done n b -> do
                assert (cnt1 - n >= 0) (return ())
                fstep fs b >>= handleCollect (Partial n) (Done n)
            Error err -> return $ Error err
    step (Tuple3' st cnt (Right fs)) a = do
        r <- step1 st a
        let cnt1 = cnt + 1
        case r of
            Partial n s -> do
                assert (cnt1 - n >= 0) (return ())
                return $ Partial n (Tuple3' s (cnt1 - n) (Right fs))
            Continue n s -> do
                assert (cnt1 - n >= 0) (return ())
                return $ Continue n (Tuple3' s (cnt1 - n) (Right fs))
            Done n b -> do
                assert (cnt1 - n >= 0) (return ())
                fstep fs b >>= handleCollect (Partial n) (Done n)
            Error _ -> Done cnt1 <$> fextract fs

    -- XXX The "try" may impact performance if this parser is used as a scan
    extract (Tuple3' s _ (Left fs)) = do
        b <- extract1 s
        fs1 <- fstep fs b
        case fs1 of
            FL.Partial s1 -> fextract s1
            FL.Done b1 -> return b1
    extract (Tuple3' s _ (Right fs)) = do
        r <- try $ extract1 s
        case r of
            Left (_ :: ParseError) -> fextract fs
            Right b -> do
                fs1 <- fstep fs b
                case fs1 of
                    FL.Partial s1 -> fextract s1
                    FL.Done b1 -> return b1

-- | See 'Streamly.Internal.Data.Parser.die'.
--
-- /Pre-release/
--
{-# INLINE_NORMAL die #-}
die :: MonadThrow m => String -> Parser m a b
die err = Parser undefined (pure (IError err)) undefined

-- | See 'Streamly.Internal.Data.Parser.dieM'.
--
-- /Pre-release/
--
{-# INLINE dieM #-}
dieM :: MonadThrow m => m String -> Parser m a b
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
instance MonadCatch m => Alternative (Parser m a) where
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
    | forall s. ConcatParseR (s -> a -> m (Step s b)) s (s -> m b)

-- | See 'Streamly.Internal.Data.Parser.concatMap'.
--
-- /Pre-release/
--
{-# INLINE concatMap #-}
concatMap :: MonadThrow m =>
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
    extractP (Parser _ initialR extractR) = do
        res <- initialR
        case res of
            IPartial s -> extractR s
            IDone b -> return b
            IError err -> throwM $ ParseError err

    extract (ConcatParseR _ s extractR) = extractR s
    extract (ConcatParseL sL) = extractL sL >>= extractP . func

{-# INLINE noErrorUnsafeConcatMap #-}
noErrorUnsafeConcatMap :: MonadThrow m =>
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
    extractP (Parser _ initialR extractR) = do
        res <- initialR
        case res of
            IPartial s -> extractR s
            IDone b -> return b
            IError err -> throwM $ ParseError err

    extract (ConcatParseR _ s extractR) = extractR s
    extract (ConcatParseL sL) = extractL sL >>= extractP . func

-- Note: The monad instance has quadratic performance complexity. It works fine
-- for small number of compositions but for a scalable implementation we need a
-- CPS version.

-- | See documentation of 'Streamly.Internal.Data.Parser.ParserK.Type.Parser'.
--
instance MonadThrow m => Monad (Parser m a) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = flip concatMap

    {-# INLINE (>>) #-}
    (>>) = (*>)

-- | See documentation of 'Streamly.Internal.Data.Parser.ParserK.Type.Parser'.
--
instance MonadCatch m => MonadPlus (Parser m a) where
    {-# INLINE mzero #-}
    mzero = die "mzero"

    {-# INLINE mplus #-}
    mplus = alt

instance (MonadThrow m, MonadReader r m, MonadCatch m) => MonadReader r (Parser m a) where
    {-# INLINE ask #-}
    ask = fromEffect ask
    {-# INLINE local #-}
    local f (Parser step init' extract) =
      Parser ((local f .) . step)
             (local f init')
             (local f . extract)


instance (MonadThrow m, MonadState s m) => MonadState s (Parser m a) where
    {-# INLINE get #-}
    get = fromEffect get
    {-# INLINE put #-}
    put = fromEffect . put


instance (MonadThrow m, MonadIO m) => MonadIO (Parser m a) where
    {-# INLINE liftIO #-}
    liftIO = fromEffect . liftIO
