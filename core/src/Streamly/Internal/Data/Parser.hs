{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Parser
    (
    -- * Setup
    -- $setup

      module Streamly.Internal.Data.Parser.Type
    --, module Streamly.Internal.Data.Parser.Tee 

    -- * Types
    , Parser (..)
    , ParseError (..)
    , Step (..)
    , Initial (..)

    -- * Downgrade to Fold
    , toFold

    -- First order parsers
    -- * Accumulators
    , fromFold
    , fromFoldMaybe

    -- * Map on input
    , postscan

    -- * Element parsers
    , peek

    -- All of these can be expressed in terms of either
    , one
    , oneEq
    , oneNotEq
    , oneOf
    , noneOf
    , eof
    , satisfy
    , maybe
    , either

    -- * Sequence parsers (tokenizers)
    --
    -- | Parsers chained in series, if one parser terminates the composition
    -- terminates. Currently we are using folds to collect the output of the
    -- parsers but we can use Parsers instead of folds to make the composition
    -- more powerful. For example, we can do:
    --
    -- takeEndByOrMax cond n p = takeEndBy cond (take n p)
    -- takeEndByBetween cond m n p = takeEndBy cond (takeBetween m n p)
    -- takeWhileBetween cond m n p = takeWhile cond (takeBetween m n p)
    , lookAhead

    -- ** By length
    -- | Grab a sequence of input elements without inspecting them
    , takeBetween
    -- , take -- takeBetween 0 n
    , takeEQ -- takeBetween n n
    , takeGE -- takeBetween n maxBound
    -- , takeGE1 -- take1 -- takeBetween 1 n
    , takeP

    -- Grab a sequence of input elements by inspecting them
    -- ** Exact match
    , listEq
    , listEqBy
    , streamEqBy
    , subsequenceBy

    -- ** By predicate
    , takeWhile
    , takeWhileP
    , takeWhile1
    , dropWhile

    -- ** Separated by elements
    -- | Separator could be in prefix postion ('takeStartBy'), or suffix
    -- position ('takeEndBy'). See 'deintercalate', 'sepBy' etc for infix
    -- separator parsing, also see 'intersperseQuotedBy' fold.

    -- These can be implemented modularly with refolds, using takeWhile and
    -- satisfy.
    , takeEndBy
    , takeEndBy_
    , takeEndByEsc
    -- , takeEndByEsc_
    , takeStartBy
    , takeStartBy_
    , takeEitherSepBy
    , wordBy

    -- ** Grouped by element comparison
    , groupBy
    , groupByRolling
    , groupByRollingEither

    -- ** Framed by elements
    -- | Also see 'intersperseQuotedBy' fold.
    -- Framed by a one or more ocurrences of a separator around a word like
    -- spaces or quotes. No nesting.
    , wordFramedBy -- XXX Remove this? Covered by wordWithQuotes?
    , wordWithQuotes
    , wordKeepQuotes
    , wordProcessQuotes

    -- Framed by separate start and end characters, potentially nested.
    -- blockWithQuotes allows quotes inside a block. However,
    -- takeFramedByGeneric can be used to express takeStartBy, takeEndBy and
    -- block with escaping.
    -- , takeFramedBy
    , takeFramedBy_
    , takeFramedByEsc_
    , takeFramedByGeneric
    , blockWithQuotes

    -- Matching strings
    -- , prefixOf -- match any prefix of a given string
    -- , suffixOf -- match any suffix of a given string
    -- , infixOf -- match any substring of a given string

    -- ** Spanning
    , span
    , spanBy
    , spanByRolling

    -- Second order parsers (parsers using parsers)
    -- * Binary Combinators
    {-
    -- ** Parallel Applicatives
    , teeWith
    , teeWithFst
    , teeWithMin
    -- , teeTill -- like manyTill but parallel
    -}

    {-
    -- ** Parallel Alternatives
    , shortest
    , longest
    -- , fastest
    -}

    -- * N-ary Combinators
    -- ** Sequential Collection
    , sequence

    -- ** Sequential Repetition
    , count
    , countBetween
    -- , countBetweenTill
    , manyP
    , many
    , some

    -- ** Interleaved Repetition
    -- Use two folds, run a primary parser, its rejected values go to the
    -- secondary parser.
    , deintercalate
    , deintercalate1
    , deintercalateAll
    -- , deintercalatePrefix
    -- , deintercalateSuffix

    -- *** Special cases
    -- | TODO: traditional implmentations of these may be of limited use. For
    -- example, consider parsing lines separated by @\\r\\n@. The main parser
    -- will have to detect and exclude the sequence @\\r\\n@ anyway so that we
    -- can apply the "sep" parser.
    --
    -- We can instead implement these as special cases of deintercalate.
    --
    -- @
    -- , endBy
    -- , sepEndBy
    -- , beginBy
    -- , sepBeginBy
    -- , sepAroundBy
    -- @
    , sepBy1
    , sepBy
    , sepByAll

    , manyTillP
    , manyTill
    , manyThen

    -- -- * Distribution
    --
    -- A simple and stupid impl would be to just convert the stream to an array
    -- and give the array reference to all consumers. The array can be grown on
    -- demand by any consumer and truncated when nonbody needs it.
    --
    -- -- ** Distribute to collection
    -- -- ** Distribute to repetition

    -- ** Interleaved collection
    -- |
    --
    -- 1. Round robin
    -- 2. Priority based
    , roundRobin

    -- -- ** Interleaved repetition
    -- repeat one parser and when it fails run an error recovery parser
    -- e.g. to find a key frame in the stream after an error

    -- ** Collection of Alternatives
    -- | Unimplemented
    --
    -- @
    -- , shortestN
    -- , longestN
    -- , fastestN -- first N successful in time
    -- , choiceN  -- first N successful in position
    -- @
    -- , choice   -- first successful in position

    -- ** Repeated Alternatives
    , retryMaxTotal
    , retryMaxSuccessive
    , retry

    -- ** Zipping Input
    , zipWithM
    , zip
    , indexed
    , makeIndexFilter
    , sampleFromthen

     -- * Deprecated
    , next
    )
where

#include "inline.hs"
#include "assert.hs"

import Control.Monad (when)
import Data.Bifunctor (first)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.SVar.Type (defState)
import Streamly.Internal.Data.Either.Strict (Either'(..))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Streamly.Internal.Data.Stream.Type (Stream)

import qualified Data.Foldable as Foldable
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Stream.Type as D
import qualified Streamly.Internal.Data.Stream.Generate as D

import Streamly.Internal.Data.Parser.Type
--import Streamly.Internal.Data.Parser.Tee -- It's empty

import Prelude hiding
       (any, all, take, takeWhile, sequence, concatMap, maybe, either, span
       , zip, filter, dropWhile)

#include "DocTestDataParser.hs"

-------------------------------------------------------------------------------
-- Downgrade a parser to a Fold
-------------------------------------------------------------------------------

-- | Make a 'Fold' from a 'Parser'. The fold just throws an exception if the
-- parser fails or tries to backtrack.
--
-- This can be useful in combinators that accept a Fold and we know that a
-- Parser cannot fail or failure exception is acceptable as there is no way to
-- recover.
--
-- /Pre-release/
--
{-# INLINE toFold #-}
toFold :: Monad m => Parser a m b -> Fold m a b
toFold (Parser pstep pinitial pextract) = Fold step initial extract

    where

    initial = do
        r <- pinitial
        case r of
            IPartial s -> return $ FL.Partial s
            IDone b -> return $ FL.Done b
            IError err ->
                error $ "toFold: parser throws error in initial" ++ err

    perror n = error $ "toFold: parser backtracks in Partial: " ++ show n
    cerror n = error $ "toFold: parser backtracks in Continue: " ++ show n
    derror n = error $ "toFold: parser backtracks in Done: " ++ show n
    eerror err = error $ "toFold: parser throws error: " ++ err

    step st a = do
        r <- pstep st a
        case r of
            Partial 0 s -> return $ FL.Partial s
            Continue 0 s -> return $ FL.Partial s
            Done 0 b -> return $ FL.Done b
            Partial n _ -> perror n
            Continue n _ -> cerror n
            Done n _ -> derror n
            Error err -> eerror err

    extract st = do
        r <- pextract st
        case r of
            Done 0 b -> return b
            Partial n _ -> perror n
            Continue n _ -> cerror n
            Done n _ -> derror n
            Error err -> eerror err

-------------------------------------------------------------------------------
-- Upgrade folds to parses
-------------------------------------------------------------------------------

-- | Make a 'Parser' from a 'Fold'. This parser sends all of its input to the
-- fold.
--
{-# INLINE fromFold #-}
fromFold :: Monad m => Fold m a b -> Parser a m b
fromFold (Fold fstep finitial fextract) = Parser step initial extract

    where

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s1 -> IPartial s1
                  FL.Done b -> IDone b

    step s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Partial s1 -> Partial 0 s1
                  FL.Done b -> Done 0 b

    extract = fmap (Done 0) . fextract

-- | Convert a Maybe returning fold to an error returning parser. The first
-- argument is the error message that the parser would return when the fold
-- returns Nothing.
--
-- /Pre-release/
--
{-# INLINE fromFoldMaybe #-}
fromFoldMaybe :: Monad m => String -> Fold m a (Maybe b) -> Parser a m b
fromFoldMaybe errMsg (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s1 -> IPartial s1
                  FL.Done b ->
                        case b of
                            Just x -> IDone x
                            Nothing -> IError errMsg

    step s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Partial s1 -> Partial 0 s1
                  FL.Done b ->
                        case b of
                            Just x -> Done 0 x
                            Nothing -> Error errMsg

    extract s = do
        res <- fextract s
        case res of
            Just x -> return $ Done 0 x
            Nothing -> return $ Error errMsg

-------------------------------------------------------------------------------
-- Failing Parsers
-------------------------------------------------------------------------------

-- | Peek the head element of a stream, without consuming it. Fails if it
-- encounters end of input.
--
-- >>> Stream.parse ((,) <$> Parser.peek <*> Parser.satisfy (> 0)) $ Stream.fromList [1]
-- Right (1,1)
--
-- @
-- peek = lookAhead (satisfy True)
-- @
--
{-# INLINE peek #-}
peek :: Monad m => Parser a m a
peek = Parser step initial extract

    where

    initial = return $ IPartial ()

    step () a = return $ Done 1 a

    extract () = return $ Error "peek: end of input"

-- | Succeeds if we are at the end of input, fails otherwise.
--
-- >>> Stream.parse ((,) <$> Parser.satisfy (> 0) <*> Parser.eof) $ Stream.fromList [1]
-- Right (1,())
--
{-# INLINE eof #-}
eof :: Monad m => Parser a m ()
eof = Parser step initial extract

    where

    initial = return $ IPartial ()

    step () _ = return $ Error "eof: not at end of input"

    extract () = return $ Done 0 ()

-- | Return the next element of the input. Returns 'Nothing'
-- on end of input. Also known as 'head'.
--
-- /Pre-release/
--
{-# DEPRECATED next "Please use \"fromFold Fold.one\" instead" #-}
{-# INLINE next #-}
next :: Monad m => Parser a m (Maybe a)
next = Parser step initial extract

  where

  initial = pure $ IPartial ()

  step () a = pure $ Done 0 (Just a)

  extract () = pure $ Done 0 Nothing

-- | Map an 'Either' returning function on the next element in the stream.  If
-- the function returns 'Left err', the parser fails with the error message
-- @err@ otherwise returns the 'Right' value.
--
-- /Pre-release/
--
{-# INLINE either #-}
either :: Monad m => (a -> Either String b) -> Parser a m b
either f = Parser step initial extract

    where

    initial = return $ IPartial ()

    step () a = return $
        case f a of
            Right b -> Done 0 b
            Left err -> Error err

    extract () = return $ Error "end of input"

-- | Map a 'Maybe' returning function on the next element in the stream. The
-- parser fails if the function returns 'Nothing' otherwise returns the 'Just'
-- value.
--
-- >>> toEither = Maybe.maybe (Left "maybe: predicate failed") Right
-- >>> maybe f = Parser.either (toEither . f)
--
-- >>> maybe f = Parser.fromFoldMaybe "maybe: predicate failed" (Fold.maybe f)
--
-- /Pre-release/
--
{-# INLINE maybe #-}
maybe :: Monad m => (a -> Maybe b) -> Parser a m b
-- maybe f = either (Maybe.maybe (Left "maybe: predicate failed") Right . f)
maybe parserF = Parser step initial extract

    where

    initial = return $ IPartial ()

    step () a = return $
        case parserF a of
            Just b -> Done 0 b
            Nothing -> Error "maybe: predicate failed"

    extract () = return $ Error "maybe: end of input"

-- | Returns the next element if it passes the predicate, fails otherwise.
--
-- >>> Stream.parse (Parser.satisfy (== 1)) $ Stream.fromList [1,0,1]
-- Right 1
--
-- >>> toMaybe f x = if f x then Just x else Nothing
-- >>> satisfy f = Parser.maybe (toMaybe f)
--
{-# INLINE satisfy #-}
satisfy :: Monad m => (a -> Bool) -> Parser a m a
-- satisfy predicate = maybe (\a -> if predicate a then Just a else Nothing)
satisfy predicate = Parser step initial extract

    where

    initial = return $ IPartial ()

    step () a = return $
        if predicate a
        then Done 0 a
        else Error "satisfy: predicate failed"

    extract () = return $ Error "satisfy: end of input"

-- | Consume one element from the head of the stream.  Fails if it encounters
-- end of input.
--
-- >>> one = Parser.satisfy $ const True
--
{-# INLINE one #-}
one :: Monad m => Parser a m a
one = satisfy $ const True

-- Alternate names: "only", "onlyThis".

-- | Match a specific element.
--
-- >>> oneEq x = Parser.satisfy (== x)
--
{-# INLINE oneEq #-}
oneEq :: (Monad m, Eq a) => a -> Parser a m a
oneEq x = satisfy (== x)

-- Alternate names: "exclude", "notThis".

-- | Match anything other than the supplied element.
--
-- >>> oneNotEq x = Parser.satisfy (/= x)
--
{-# INLINE oneNotEq #-}
oneNotEq :: (Monad m, Eq a) => a -> Parser a m a
oneNotEq x = satisfy (/= x)

-- | Match any one of the elements in the supplied list.
--
-- >>> oneOf xs = Parser.satisfy (`Foldable.elem` xs)
--
-- When performance matters a pattern matching predicate could be more
-- efficient than a 'Foldable' datatype:
--
-- @
-- let p x =
--    case x of
--       'a' -> True
--       'e' -> True
--        _  -> False
-- in satisfy p
-- @
--
-- GHC may use a binary search instead of linear search in the list.
-- Alternatively, you can also use an array instead of list for storage and
-- search.
--
{-# INLINE oneOf #-}
oneOf :: (Monad m, Eq a, Foldable f) => f a -> Parser a m a
oneOf xs = satisfy (`Foldable.elem` xs)

-- | See performance notes in 'oneOf'.
--
-- >>> noneOf xs = Parser.satisfy (`Foldable.notElem` xs)
--
{-# INLINE noneOf #-}
noneOf :: (Monad m, Eq a, Foldable f) => f a -> Parser a m a
noneOf xs = satisfy (`Foldable.notElem` xs)

-------------------------------------------------------------------------------
-- Taking elements
-------------------------------------------------------------------------------

-- Required to fuse "take" with "many" in "chunksOf", for ghc-9.x
{-# ANN type Tuple'Fused Fuse #-}
data Tuple'Fused a b = Tuple'Fused !a !b deriving Show

-- | @takeBetween m n@ takes a minimum of @m@ and a maximum of @n@ input
-- elements and folds them using the supplied fold.
--
-- Stops after @n@ elements.
-- Fails if the stream ends before @m@ elements could be taken.
--
-- Examples: -
--
-- @
-- >>> :{
--   takeBetween' low high ls = Stream.parse prsr (Stream.fromList ls)
--     where prsr = Parser.takeBetween low high Fold.toList
-- :}
--
-- @
--
-- >>> takeBetween' 2 4 [1, 2, 3, 4, 5]
-- Right [1,2,3,4]
--
-- >>> takeBetween' 2 4 [1, 2]
-- Right [1,2]
--
-- >>> takeBetween' 2 4 [1]
-- Left (ParseError "takeBetween: Expecting alteast 2 elements, got 1")
--
-- >>> takeBetween' 0 0 [1, 2]
-- Right []
--
-- >>> takeBetween' 0 1 []
-- Right []
--
-- @takeBetween@ is the most general take operation, other take operations can
-- be defined in terms of takeBetween. For example:
--
-- >>> take n = Parser.takeBetween 0 n
-- >>> takeEQ n = Parser.takeBetween n n
-- >>> takeGE n = Parser.takeBetween n maxBound
--
-- /Pre-release/
--
{-# INLINE takeBetween #-}
takeBetween :: Monad m => Int -> Int -> Fold m a b -> Parser a m b
takeBetween low high (Fold fstep finitial fextract) =

    Parser step initial (extract streamErr)

    where

    streamErr i =
           "takeBetween: Expecting alteast " ++ show low
        ++ " elements, got " ++ show i

    invalidRange =
        "takeBetween: lower bound - " ++ show low
            ++ " is greater than higher bound - " ++ show high

    foldErr :: Int -> String
    foldErr i =
        "takeBetween: the collecting fold terminated after"
            ++ " consuming" ++ show i ++ " elements"
            ++ " minimum" ++ show low ++ " elements needed"

    -- Exactly the same as snext except different constructors, we can possibly
    -- deduplicate the two.
    {-# INLINE inext #-}
    inext i res =
        let i1 = i + 1
        in case res of
            FL.Partial s -> do
                let s1 = Tuple'Fused i1 s
                if i1 < high
                -- XXX ideally this should be a Continue instead
                then return $ IPartial s1
                else iextract foldErr s1
            FL.Done b ->
                return
                    $ if i1 >= low
                      then IDone b
                      else IError (foldErr i1)

    initial = do
        when (low >= 0 && high >= 0 && low > high)
            $ error invalidRange

        finitial >>= inext (-1)

    -- Keep the impl same as inext
    {-# INLINE snext #-}
    snext i res =
        let i1 = i + 1
        in case res of
            FL.Partial s -> do
                let s1 = Tuple'Fused i1 s
                if i1 < high
                then return $ Continue 0 s1
                else extract foldErr s1
            FL.Done b ->
                return
                    $ if i1 >= low
                      then Done 0 b
                      else Error (foldErr i1)

    step (Tuple'Fused i s) a = fstep s a >>= snext i

    extract f (Tuple'Fused i s)
        | i >= low && i <= high = fmap (Done 0) (fextract s)
        | otherwise = return $ Error (f i)

    -- XXX Need to make Initial return type Step to deduplicate this
    iextract f (Tuple'Fused i s)
        | i >= low && i <= high = fmap IDone (fextract s)
        | otherwise = return $ IError (f i)

-- | Stops after taking exactly @n@ input elements.
--
-- * Stops - after consuming @n@ elements.
-- * Fails - if the stream or the collecting fold ends before it can collect
--           exactly @n@ elements.
--
-- >>> Stream.parse (Parser.takeEQ 2 Fold.toList) $ Stream.fromList [1,0,1]
-- Right [1,0]
--
-- >>> Stream.parse (Parser.takeEQ 4 Fold.toList) $ Stream.fromList [1,0,1]
-- Left (ParseError "takeEQ: Expecting exactly 4 elements, input terminated on 3")
--
{-# INLINE takeEQ #-}
takeEQ :: Monad m => Int -> Fold m a b -> Parser a m b
takeEQ n (Fold fstep finitial fextract) = Parser step initial extract

    where

    initial = do
        res <- finitial
        case res of
            FL.Partial s ->
                if n > 0
                then return $ IPartial $ Tuple'Fused 1 s
                else fmap IDone (fextract s)
            FL.Done b -> return $
                if n > 0
                then IError
                         $ "takeEQ: Expecting exactly " ++ show n
                             ++ " elements, fold terminated without"
                             ++ " consuming any elements"
                else IDone b

    step (Tuple'Fused i1 r) a = do
        res <- fstep r a
        if n > i1
        then
            return
                $ case res of
                    FL.Partial s -> Continue 0 $ Tuple'Fused (i1 + 1) s
                    FL.Done _ ->
                        Error
                            $ "takeEQ: Expecting exactly " ++ show n
                                ++ " elements, fold terminated on " ++ show i1
        else
            -- assert (n == i1)
            Done 0
                <$> case res of
                        FL.Partial s -> fextract s
                        FL.Done b -> return b

    extract (Tuple'Fused i _) =
        -- Using the count "i" in the message below causes large performance
        -- regression unless we use Fuse annotation on Tuple.
        return
            $ Error
            $ "takeEQ: Expecting exactly " ++ show n
                ++ " elements, input terminated on " ++ show (i - 1)

{-# ANN type TakeGEState Fuse #-}
data TakeGEState s =
      TakeGELT !Int !s
    | TakeGEGE !s

-- | Take at least @n@ input elements, but can collect more.
--
-- * Stops - when the collecting fold stops.
-- * Fails - if the stream or the collecting fold ends before producing @n@
--           elements.
--
-- >>> Stream.parse (Parser.takeGE 4 Fold.toList) $ Stream.fromList [1,0,1]
-- Left (ParseError "takeGE: Expecting at least 4 elements, input terminated on 3")
--
-- >>> Stream.parse (Parser.takeGE 4 Fold.toList) $ Stream.fromList [1,0,1,0,1]
-- Right [1,0,1,0,1]
--
-- /Pre-release/
--
{-# INLINE takeGE #-}
takeGE :: Monad m => Int -> Fold m a b -> Parser a m b
takeGE n (Fold fstep finitial fextract) = Parser step initial extract

    where

    initial = do
        res <- finitial
        case res of
            FL.Partial s ->
                if n > 0
                then return $ IPartial $ TakeGELT 1 s
                else return $ IPartial $ TakeGEGE s
            FL.Done b -> return $
                if n > 0
                then IError
                         $ "takeGE: Expecting at least " ++ show n
                             ++ " elements, fold terminated without"
                             ++ " consuming any elements"
                else IDone b

    step (TakeGELT i1 r) a = do
        res <- fstep r a
        if n > i1
        then
            return
                $ case res of
                      FL.Partial s -> Continue 0 $ TakeGELT (i1 + 1) s
                      FL.Done _ ->
                        Error
                            $ "takeGE: Expecting at least " ++ show n
                                ++ " elements, fold terminated on " ++ show i1
        else
            -- assert (n <= i1)
            return
                $ case res of
                      FL.Partial s -> Partial 0 $ TakeGEGE s
                      FL.Done b -> Done 0 b
    step (TakeGEGE r) a = do
        res <- fstep r a
        return
            $ case res of
                  FL.Partial s -> Partial 0 $ TakeGEGE s
                  FL.Done b -> Done 0 b

    extract (TakeGELT i _) =
        return
            $ Error
            $ "takeGE: Expecting at least " ++ show n
                ++ " elements, input terminated on " ++ show (i - 1)
    extract (TakeGEGE r) = fmap (Done 0) $ fextract r

-------------------------------------------------------------------------------
-- Conditional splitting
-------------------------------------------------------------------------------

-- XXX We should perhaps use only takeWhileP and rename it to takeWhile.

-- | Like 'takeWhile' but uses a 'Parser' instead of a 'Fold' to collect the
-- input. The combinator stops when the condition fails or if the collecting
-- parser stops.
--
-- Other interesting parsers can be implemented in terms of this parser:
--
-- >>> takeWhile1 cond p = Parser.takeWhileP cond (Parser.takeBetween 1 maxBound p)
-- >>> takeWhileBetween cond m n p = Parser.takeWhileP cond (Parser.takeBetween m n p)
--
-- Stops: when the condition fails or the collecting parser stops.
-- Fails: when the collecting parser fails.
--
-- /Pre-release/
--
{-# INLINE takeWhileP #-}
takeWhileP :: Monad m => (a -> Bool) -> Parser a m b -> Parser a m b
takeWhileP predicate (Parser pstep pinitial pextract) =
    Parser step pinitial pextract

    where

    step s a =
        if predicate a
        then pstep s a
        else do
            r <- pextract s
            -- XXX need a map on count
            case r of
                Error err -> return $ Error err
                Done n s1 -> return $ Done (n + 1) s1
                Partial _ _ -> error "Bug: takeWhileP: Partial in extract"
                Continue n s1 -> return $ Continue (n + 1) s1

-- | Collect stream elements until an element fails the predicate. The element
-- on which the predicate fails is returned back to the input stream.
--
-- * Stops - when the predicate fails or the collecting fold stops.
-- * Fails - never.
--
-- >>> Stream.parse (Parser.takeWhile (== 0) Fold.toList) $ Stream.fromList [0,0,1,0,1]
-- Right [0,0]
--
-- >>> takeWhile cond f = Parser.takeWhileP cond (Parser.fromFold f)
--
-- We can implement a @breakOn@ using 'takeWhile':
--
-- @
-- breakOn p = takeWhile (not p)
-- @
--
{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Fold m a b -> Parser a m b
-- takeWhile cond f = takeWhileP cond (fromFold f)
takeWhile predicate (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial = do
        res <- finitial
        return $ case res of
            FL.Partial s -> IPartial s
            FL.Done b -> IDone b

    step s a =
        if predicate a
        then do
            fres <- fstep s a
            return
                $ case fres of
                      FL.Partial s1 -> Partial 0 s1
                      FL.Done b -> Done 0 b
        else Done 1 <$> fextract s

    extract s = fmap (Done 0) (fextract s)

{-
-- XXX This may not be composable because of the b argument. We can instead
-- return a "Reparse b a m b" so that those can be composed.
{-# INLINE takeWhile1X #-}
takeWhile1 :: Monad m => b -> (a -> Bool) -> Refold m b a b -> Parser a m b
-- We can implement this using satisfy and takeWhile. We can use "satisfy
-- p", fold the result with the refold and then use the "takeWhile p" and
-- fold that using the refold.
takeWhile1 acc cond f = undefined
-}

-- | Like 'takeWhile' but takes at least one element otherwise fails.
--
-- >>> takeWhile1 cond p = Parser.takeWhileP cond (Parser.takeBetween 1 maxBound p)
--
{-# INLINE takeWhile1 #-}
takeWhile1 :: Monad m => (a -> Bool) -> Fold m a b -> Parser a m b
-- takeWhile1 cond f = takeWhileP cond (takeBetween 1 maxBound f)
takeWhile1 predicate (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial = do
        res <- finitial
        return $ case res of
            FL.Partial s -> IPartial (Left' s)
            FL.Done _ ->
                IError
                    $ "takeWhile1: fold terminated without consuming:"
                          ++ " any element"

    {-# INLINE process #-}
    process s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Partial s1 -> Partial 0 (Right' s1)
                  FL.Done b -> Done 0 b

    step (Left' s) a =
        if predicate a
        then process s a
        else return $ Error "takeWhile1: predicate failed on first element"
    step (Right' s) a =
        if predicate a
        then process s a
        else do
            b <- fextract s
            return $ Done 1 b

    extract (Left' _) = return $ Error "takeWhile1: end of input"
    extract (Right' s) = fmap (Done 0) (fextract s)

-- | Drain the input as long as the predicate succeeds, running the effects and
-- discarding the results.
--
-- This is also called @skipWhile@ in some parsing libraries.
--
-- >>> dropWhile p = Parser.takeWhile p Fold.drain
--
{-# INLINE dropWhile #-}
dropWhile :: Monad m => (a -> Bool) -> Parser a m ()
dropWhile p = takeWhile p FL.drain

-------------------------------------------------------------------------------
-- Separators
-------------------------------------------------------------------------------

{-# ANN type FramedEscState Fuse #-}
data FramedEscState s =
    FrameEscInit !s | FrameEscGo !s !Int | FrameEscEsc !s !Int

-- XXX We can remove Maybe from esc
{-# INLINE takeFramedByGeneric #-}
takeFramedByGeneric :: Monad m =>
       Maybe (a -> Bool) -- is escape char?
    -> Maybe (a -> Bool) -- is frame begin?
    -> Maybe (a -> Bool) -- is frame end?
    -> Fold m a b
    -> Parser a m b
takeFramedByGeneric esc begin end (Fold fstep finitial fextract) =

    Parser step initial extract

    where

    initial =  do
        res <- finitial
        return $
            case res of
                FL.Partial s -> IPartial (FrameEscInit s)
                FL.Done _ ->
                    error "takeFramedByGeneric: fold done without input"

    {-# INLINE process #-}
    process s a n = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Continue 0 (FrameEscGo s1 n)
                FL.Done b -> Done 0 b

    {-# INLINE processNoEsc #-}
    processNoEsc s a n =
        case end of
            Just isEnd ->
                case begin of
                    Just isBegin ->
                        -- takeFramedBy case
                        if isEnd a
                        then
                            if n == 0
                            then Done 0 <$> fextract s
                            else process s a (n - 1)
                        else
                            let n1 = if isBegin a then n + 1 else n
                             in process s a n1
                    Nothing -> -- takeEndBy case
                        if isEnd a
                        then Done 0 <$> fextract s
                        else process s a n
            Nothing -> -- takeStartBy case
                case begin of
                    Just isBegin ->
                        if isBegin a
                        then Done 0 <$> fextract s
                        else process s a n
                    Nothing ->
                        error $ "takeFramedByGeneric: "
                            ++ "Both begin and end frame predicate missing"

    {-# INLINE processCheckEsc #-}
    processCheckEsc s a n =
        case esc of
            Just isEsc ->
                if isEsc a
                then return $ Partial 0 $ FrameEscEsc s n
                else processNoEsc s a n
            Nothing -> processNoEsc s a n

    step (FrameEscInit s) a =
        case begin of
            Just isBegin ->
                if isBegin a
                then return $ Partial 0 (FrameEscGo s 0)
                else return $ Error "takeFramedByGeneric: missing frame start"
            Nothing ->
                case end of
                    Just isEnd ->
                        if isEnd a
                        then Done 0 <$> fextract s
                        else processCheckEsc s a 0
                    Nothing ->
                        error "Both begin and end frame predicate missing"
    step (FrameEscGo s n) a = processCheckEsc s a n
    step (FrameEscEsc s n) a = process s a n

    err = return . Error

    extract (FrameEscInit _) =
        err "takeFramedByGeneric: empty token"
    extract (FrameEscGo s _) =
        case begin of
            Just _ ->
                case end of
                    Nothing -> fmap (Done 0) $ fextract s
                    Just _ -> err "takeFramedByGeneric: missing frame end"
            Nothing -> err "takeFramedByGeneric: missing closing frame"
    extract (FrameEscEsc _ _) = err "takeFramedByGeneric: trailing escape"

data BlockParseState s =
      BlockInit !s
    | BlockUnquoted !Int !s
    | BlockQuoted !Int !s
    | BlockQuotedEsc !Int !s

-- Blocks can be of different types e.g. {} or (). We only parse from the
-- perspective of the outermost block type. The nesting of that block are
-- checked. Any other block types nested inside it are opaque to us and can be
-- parsed when the contents of the block are parsed.

-- XXX Put a limit on nest level to keep the API safe.

-- | Parse a block enclosed within open, close brackets. Block contents may be
-- quoted, brackets inside quotes are ignored. Quoting characters can be used
-- within quotes if escaped. A block can have a nested block inside it.
--
-- Quote begin and end chars are the same. Block brackets and quote chars must
-- not overlap. Block start and end brackets must be different for nesting
-- blocks within blocks.
--
-- >>> p = Parser.blockWithQuotes (== '\\') (== '"') '{' '}' Fold.toList
-- >>> Stream.parse p $ Stream.fromList "{msg: \"hello world\"}"
-- Right "msg: \"hello world\""
--
{-# INLINE blockWithQuotes #-}
blockWithQuotes :: (Monad m, Eq a) =>
       (a -> Bool)  -- ^ escape char
    -> (a -> Bool)  -- ^ quote char, to quote inside brackets
    -> a  -- ^ Block opening bracket
    -> a  -- ^ Block closing bracket
    -> Fold m a b
    -> Parser a m b
blockWithQuotes isEsc isQuote bopen bclose
    (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial = do
        res <- finitial
        return $
            case res of
                FL.Partial s -> IPartial (BlockInit s)
                FL.Done _ ->
                    error "blockWithQuotes: fold finished without input"

    {-# INLINE process #-}
    process s a nextState = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Continue 0 (nextState s1)
                FL.Done b -> Done 0 b

    step (BlockInit s) a =
        return
            $ if a == bopen
              then Continue 0 $ BlockUnquoted 1 s
              else Error "blockWithQuotes: missing block start"
    step (BlockUnquoted level s) a
        | a == bopen = process s a (BlockUnquoted (level + 1))
        | a == bclose =
            if level == 1
            then fmap (Done 0) (fextract s)
            else process s a (BlockUnquoted (level - 1))
        | isQuote a = process s a (BlockQuoted level)
        | otherwise = process s a (BlockUnquoted level)
    step (BlockQuoted level s) a
        | isEsc a = process s a (BlockQuotedEsc level)
        | otherwise =
            if isQuote a
            then process s a (BlockUnquoted level)
            else process s a (BlockQuoted level)
    step (BlockQuotedEsc level s) a = process s a (BlockQuoted level)

    err = return . Error

    extract (BlockInit s) = fmap (Done 0) $ fextract s
    extract (BlockUnquoted level _) =
        err $ "blockWithQuotes: finished at block nest level " ++ show level
    extract (BlockQuoted level _) =
        err $ "blockWithQuotes: finished, inside an unfinished quote, "
            ++ "at block nest level " ++ show level
    extract (BlockQuotedEsc level _) =
        err $ "blockWithQuotes: finished, inside an unfinished quote, "
            ++ "after an escape char, at block nest level " ++ show level

-- | @takeEndBy cond parser@ parses a token that ends by a separator chosen by
-- the supplied predicate. The separator is also taken with the token.
--
-- This can be combined with other parsers to implement other interesting
-- parsers as follows:
--
-- >>> takeEndByLE cond n p = Parser.takeEndBy cond (Parser.fromFold $ Fold.take n p)
-- >>> takeEndByBetween cond m n p = Parser.takeEndBy cond (Parser.takeBetween m n p)
--
-- >>> takeEndBy = Parser.takeEndByEsc (const False)
--
-- See also "Streamly.Data.Fold.takeEndBy". Unlike the fold, the collecting
-- parser in the takeEndBy parser can decide whether to fail or not if the
-- stream does not end with separator.
--
-- /Pre-release/
--
{-# INLINE takeEndBy #-}
takeEndBy :: Monad m => (a -> Bool) -> Parser a m b -> Parser a m b
-- takeEndBy = takeEndByEsc (const False)
takeEndBy cond (Parser pstep pinitial pextract) =

    Parser step initial pextract

    where

    initial = pinitial

    step s a = do
        res <- pstep s a
        if not (cond a)
        then return res
        else extractStep pextract res

-- | Like 'takeEndBy' but the separator elements can be escaped using an
-- escape char determined by the first predicate. The escape characters are
-- removed.
--
-- /pre-release/
{-# INLINE takeEndByEsc #-}
takeEndByEsc :: Monad m =>
    (a -> Bool) -> (a -> Bool) -> Parser a m b -> Parser a m b
takeEndByEsc isEsc isSep (Parser pstep pinitial pextract) =

    Parser step initial extract

    where

    initial = first Left' <$> pinitial

    step (Left' s) a = do
        if isEsc a
        then return $ Partial 0 $ Right' s
        else do
            res <- pstep s a
            if not (isSep a)
            then return $ first Left' res
            else fmap (first Left') $ extractStep pextract res

    step (Right' s) a = do
        res <- pstep s a
        return $ first Left' res

    extract (Left' s) = fmap (first Left') $ pextract s
    extract (Right' _) =
        return $ Error "takeEndByEsc: trailing escape"

-- | Like 'takeEndBy' but the separator is dropped.
--
-- See also "Streamly.Data.Fold.takeEndBy_".
--
-- /Pre-release/
--
{-# INLINE takeEndBy_ #-}
takeEndBy_ :: (a -> Bool) -> Parser a m b -> Parser a m b
{-
takeEndBy_ isEnd p =
    takeFramedByGeneric Nothing Nothing (Just isEnd) (toFold p)
-}
takeEndBy_ cond (Parser pstep pinitial pextract) =

    Parser step pinitial pextract

    where

    step s a =
        if cond a
        then pextract s
        else pstep s a

-- | Take either the separator or the token. Separator is a Left value and
-- token is Right value.
--
-- /Unimplemented/
{-# INLINE takeEitherSepBy #-}
takeEitherSepBy :: -- Monad m =>
    (a -> Bool) -> Fold m (Either a b) c -> Parser a m c
takeEitherSepBy _cond = undefined -- D.toParserK . D.takeEitherSepBy cond

-- | Parse a token that starts with an element chosen by the predicate.  The
-- parser fails if the input does not start with the selected element.
--
-- * Stops - when the predicate succeeds in non-leading position.
-- * Fails - when the predicate fails in the leading position.
--
-- >>> splitWithPrefix p f = Stream.parseMany (Parser.takeStartBy p f)
--
-- Examples: -
--
-- >>> p = Parser.takeStartBy (== ',') Fold.toList
-- >>> leadingComma = Stream.parse p . Stream.fromList
-- >>> leadingComma "a,b"
-- Left (ParseError "takeStartBy: missing frame start")
-- ...
-- >>> leadingComma ",,"
-- Right ","
-- >>> leadingComma ",a,b"
-- Right ",a"
-- >>> leadingComma ""
-- Right ""
--
-- /Pre-release/
--
{-# INLINE takeStartBy #-}
takeStartBy :: Monad m => (a -> Bool) -> Fold m a b -> Parser a m b
takeStartBy cond (Fold fstep finitial fextract) =

    Parser step initial extract

    where

    initial =  do
        res <- finitial
        return $
            case res of
                FL.Partial s -> IPartial (Left' s)
                FL.Done _ -> IError "takeStartBy: fold done without input"

    {-# INLINE process #-}
    process s a = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Partial 0 (Right' s1)
                FL.Done b -> Done 0 b

    step (Left' s) a =
        if cond a
        then process s a
        else return $ Error "takeStartBy: missing frame start"
    step (Right' s) a =
        if not (cond a)
        then process s a
        else Done 1 <$> fextract s

    extract (Left' s) = fmap (Done 0) $ fextract s
    extract (Right' s) = fmap (Done 0) $ fextract s

-- | Like 'takeStartBy' but drops the separator.
--
-- >>> takeStartBy_ isBegin = Parser.takeFramedByGeneric Nothing (Just isBegin) Nothing
--
{-# INLINE takeStartBy_ #-}
takeStartBy_ :: Monad m => (a -> Bool) -> Fold m a b -> Parser a m b
takeStartBy_ isBegin = takeFramedByGeneric Nothing (Just isBegin) Nothing

-- | @takeFramedByEsc_ isEsc isBegin isEnd fold@ parses a token framed using a
-- begin and end predicate, and an escape character. The frame begin and end
-- characters lose their special meaning if preceded by the escape character.
--
-- Nested frames are allowed if begin and end markers are different, nested
-- frames must be balanced unless escaped, nested frame markers are emitted as
-- it is.
--
-- For example,
--
-- >>> p = Parser.takeFramedByEsc_ (== '\\') (== '{') (== '}') Fold.toList
-- >>> Stream.parse p $ Stream.fromList "{hello}"
-- Right "hello"
-- >>> Stream.parse p $ Stream.fromList "{hello {world}}"
-- Right "hello {world}"
-- >>> Stream.parse p $ Stream.fromList "{hello \\{world}"
-- Right "hello {world"
-- >>> Stream.parse p $ Stream.fromList "{hello {world}"
-- Left (ParseError "takeFramedByEsc_: missing frame end")
--
-- /Pre-release/
{-# INLINE takeFramedByEsc_ #-}
takeFramedByEsc_ :: Monad m =>
    (a -> Bool) -> (a -> Bool) -> (a -> Bool) -> Fold m a b -> Parser a m b
-- takeFramedByEsc_ isEsc isEnd p =
--    takeFramedByGeneric (Just isEsc) Nothing (Just isEnd) (toFold p)
takeFramedByEsc_ isEsc isBegin isEnd (Fold fstep finitial fextract) =

    Parser step initial extract

    where

    initial =  do
        res <- finitial
        return $
            case res of
                FL.Partial s -> IPartial (FrameEscInit s)
                FL.Done _ ->
                    error "takeFramedByEsc_: fold done without input"

    {-# INLINE process #-}
    process s a n = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Continue 0 (FrameEscGo s1 n)
                FL.Done b -> Done 0 b

    step (FrameEscInit s) a =
        if isBegin a
        then return $ Partial 0 (FrameEscGo s 0)
        else return $ Error "takeFramedByEsc_: missing frame start"
    step (FrameEscGo s n) a =
        if isEsc a
        then return $ Partial 0 $ FrameEscEsc s n
        else do
            if not (isEnd a)
            then
                let n1 = if isBegin a then n + 1 else n
                 in process s a n1
            else
                if n == 0
                then Done 0 <$> fextract s
                else process s a (n - 1)
    step (FrameEscEsc s n) a = process s a n

    err = return . Error

    extract (FrameEscInit _) = err "takeFramedByEsc_: empty token"
    extract (FrameEscGo _ _) = err "takeFramedByEsc_: missing frame end"
    extract (FrameEscEsc _ _) = err "takeFramedByEsc_: trailing escape"

data FramedState s = FrameInit !s | FrameGo !s Int

-- | @takeFramedBy_ isBegin isEnd fold@ parses a token framed by a begin and an
-- end predicate.
--
-- >>> takeFramedBy_ = Parser.takeFramedByEsc_ (const False)
--
{-# INLINE takeFramedBy_ #-}
takeFramedBy_ :: Monad m =>
    (a -> Bool) -> (a -> Bool) -> Fold m a b -> Parser a m b
-- takeFramedBy_ isBegin isEnd =
--    takeFramedByGeneric (Just (const False)) (Just isBegin) (Just isEnd)
takeFramedBy_ isBegin isEnd (Fold fstep finitial fextract) =

    Parser step initial extract

    where

    initial =  do
        res <- finitial
        return $
            case res of
                FL.Partial s -> IPartial (FrameInit s)
                FL.Done _ ->
                    error "takeFramedBy_: fold done without input"

    {-# INLINE process #-}
    process s a n = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Continue 0 (FrameGo s1 n)
                FL.Done b -> Done 0 b

    step (FrameInit s) a =
        if isBegin a
        then return $ Continue 0 (FrameGo s 0)
        else return $ Error "takeFramedBy_: missing frame start"
    step (FrameGo s n) a
        | not (isEnd a) =
            let n1 = if isBegin a then n + 1 else n
             in process s a n1
        | n == 0 = Done 0 <$> fextract s
        | otherwise = process s a (n - 1)

    err = return . Error

    extract (FrameInit _) = err "takeFramedBy_: empty token"
    extract (FrameGo _ _) = err "takeFramedBy_: missing frame end"

-------------------------------------------------------------------------------
-- Grouping and words
-------------------------------------------------------------------------------

data WordByState s b = WBLeft !s | WBWord !s | WBRight !b

-- Note we can also get words using something like:
-- sepBy FL.toList (takeWhile (not . p) Fold.toList) (dropWhile p)
--
-- But that won't be as efficient and ergonomic.

-- | Like 'splitOn' but strips leading, trailing, and repeated separators.
-- Therefore, @".a..b."@ having '.' as the separator would be parsed as
-- @["a","b"]@.  In other words, its like parsing words from whitespace
-- separated text.
--
-- * Stops - when it finds a word separator after a non-word element
-- * Fails - never.
--
-- >>> wordBy = Parser.wordFramedBy (const False) (const False) (const False)
--
-- @
-- S.wordsBy pred f = S.parseMany (PR.wordBy pred f)
-- @
--
{-# INLINE wordBy #-}
wordBy :: Monad m => (a -> Bool) -> Fold m a b -> Parser a m b
wordBy predicate (Fold fstep finitial fextract) = Parser step initial extract

    where

    {-# INLINE worder #-}
    worder s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Partial s1 -> Partial 0 $ WBWord s1
                  FL.Done b -> Done 0 b

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s -> IPartial $ WBLeft s
                  FL.Done b -> IDone b

    step (WBLeft s) a =
        if not (predicate a)
        then worder s a
        else return $ Partial 0 $ WBLeft s
    step (WBWord s) a =
        if not (predicate a)
        then worder s a
        else do
            b <- fextract s
            return $ Partial 0 $ WBRight b
    step (WBRight b) a =
        return
            $ if not (predicate a)
              then Done 1 b
              else Partial 0 $ WBRight b

    extract (WBLeft s) = fmap (Done 0) $ fextract s
    extract (WBWord s) = fmap (Done 0) $ fextract s
    extract (WBRight b) = return (Done 0 b)

data WordFramedState s b =
      WordFramedSkipPre !s
    | WordFramedWord !s !Int
    | WordFramedEsc !s !Int
    | WordFramedSkipPost !b

-- | Like 'wordBy' but treats anything inside a pair of quotes as a single
-- word, the quotes can be escaped by an escape character.  Recursive quotes
-- are possible if quote begin and end characters are different, quotes must be
-- balanced. Outermost quotes are stripped.
--
-- >>> braces = Parser.wordFramedBy (== '\\') (== '{') (== '}') isSpace Fold.toList
-- >>> Stream.parse braces $ Stream.fromList "{ab} cd"
-- Right "ab"
-- >>> Stream.parse braces $ Stream.fromList "{ab}{cd}"
-- Right "abcd"
-- >>> Stream.parse braces $ Stream.fromList "a{b} cd"
-- Right "ab"
-- >>> Stream.parse braces $ Stream.fromList "a{{b}} cd"
-- Right "a{b}"
--
-- >>> quotes = Parser.wordFramedBy (== '\\') (== '"') (== '"') isSpace Fold.toList
-- >>> Stream.parse quotes $ Stream.fromList "\"a\"\"b\""
-- Right "ab"
--
{-# INLINE wordFramedBy #-}
wordFramedBy :: Monad m =>
       (a -> Bool)  -- ^ Matches escape elem?
    -> (a -> Bool)  -- ^ Matches left quote?
    -> (a -> Bool)  -- ^ matches right quote?
    -> (a -> Bool)  -- ^ matches word separator?
    -> Fold m a b
    -> Parser a m b
wordFramedBy isEsc isBegin isEnd isSep
    (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial =  do
        res <- finitial
        return $
            case res of
                FL.Partial s -> IPartial (WordFramedSkipPre s)
                FL.Done _ ->
                    error "wordFramedBy: fold done without input"

    {-# INLINE process #-}
    process s a n = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Continue 0 (WordFramedWord s1 n)
                FL.Done b -> Done 0 b

    step (WordFramedSkipPre s) a
        | isEsc a = return $ Continue 0 $ WordFramedEsc s 0
        | isSep a = return $ Partial 0 $ WordFramedSkipPre s
        | isBegin a = return $ Continue 0 $ WordFramedWord s 1
        | isEnd a =
            return $ Error "wordFramedBy: missing frame start"
        | otherwise = process s a 0
    step (WordFramedWord s n) a
        | isEsc a = return $ Continue 0 $ WordFramedEsc s n
        | n == 0 && isSep a = do
            b <- fextract s
            return $ Partial 0 $ WordFramedSkipPost b
        | otherwise = do
            -- We need to use different order for checking begin and end for
            -- the n == 0 and n == 1 case so that when the begin and end
            -- character is the same we treat the one after begin as end.
            if n == 0
            then
               -- Need to check isBegin first
               if isBegin a
               then return $ Continue 0 $ WordFramedWord s 1
               else if isEnd a
                    then return $ Error "wordFramedBy: missing frame start"
                    else process s a n
            else
               -- Need to check isEnd first
                if isEnd a
                then
                   if n == 1
                   then return $ Continue 0 $ WordFramedWord s 0
                   else process s a (n - 1)
                else if isBegin a
                     then process s a (n + 1)
                     else process s a n
    step (WordFramedEsc s n) a = process s a n
    step (WordFramedSkipPost b) a =
        return
            $ if not (isSep a)
              then Done 1 b
              else Partial 0 $ WordFramedSkipPost b

    err = return . Error

    extract (WordFramedSkipPre s) = fmap (Done 0) $ fextract s
    extract (WordFramedWord s n) =
        if n == 0
        then fmap (Done 0) $ fextract s
        else err "wordFramedBy: missing frame end"
    extract (WordFramedEsc _ _) =
        err "wordFramedBy: trailing escape"
    extract (WordFramedSkipPost b) = return (Done 0 b)

data WordQuotedState s b a =
      WordQuotedSkipPre !s
    | WordUnquotedWord !s
    | WordQuotedWord !s !Int !a !a
    | WordUnquotedEsc !s
    | WordQuotedEsc !s !Int !a !a
    | WordQuotedSkipPost !b

-- | Quote and bracket aware word splitting with escaping. Like 'wordBy' but
-- word separators within specified quotes or brackets are ignored. Quotes and
-- escape characters can be processed. If the end quote is different from the
-- start quote it is called a bracket. The following quoting rules apply:
--
-- * In an unquoted string a character may be preceded by an escape character.
-- The escape character is removed and the character following it is treated
-- literally with no special meaning e.g. e.g. h\ e\ l\ l\ o is a single word,
-- \n is same as n.
-- * Any part of the word can be placed within quotes. Inside quotes all
-- characters are treated literally with no special meaning. Quoting character
-- itself cannot be used within quotes unless escape processing within quotes
-- is applied to allow it.
-- * Optionally escape processing for quoted part can be specified. Escape
-- character has no special meaning inside quotes unless it is followed by a
-- character that has a escape translation specified, in that case the escape
-- character is removed, and the specified translation is applied to the
-- character following it. This can be used to escape the quoting character
-- itself within quotes.
-- * There can be multiple quoting characters, when a quote starts, all other
-- quoting characters within that quote lose any special meaning until the
-- quote is closed.
-- * A starting quote char without an ending char generates a parse error. An
-- ending bracket char without a corresponding bracket begin is ignored.
-- * Brackets can be nested.
--
-- We should note that unquoted and quoted escape processing are different. In
-- unquoted part escape character is always removed. In quoted part it is
-- removed only if followed by a special meaning character. This is consistent
-- with how shell performs escape processing.

-- Examples of quotes - "double quotes", 'single quotes', (parens), {braces},
-- ((nested) brackets).
--
-- Example:
--
-- >>> :{
-- >>> q x =
-- >>>     case x of
-- >>>         '"' -> Just x
-- >>>         '\'' -> Just x
-- >>>         _ -> Nothing
-- >>> :}
--
-- >>> p = Parser.wordKeepQuotes (== '\\') q isSpace Fold.toList
-- >>> Stream.parse p $ Stream.fromList "a\"b'c\";'d\"e'f ghi"
-- Right "a\"b'c\";'d\"e'f"
--
-- Note that outer quotes and backslashes from the input string are consumed by
-- Haskell, therefore, the actual input string passed to the parser is:
-- a"b'c";'d"e'f ghi
--
-- Similarly, when printing, double quotes are escaped by Haskell.
--
-- Limitations:
--
-- Shell like quote processing can be performed by using quote char specific
-- escape processing, single quotes with no escapes, and double quotes with
-- escapes.
--
-- JSON string processing can also be achieved except the "\uXXXX" style
-- escaping for Unicode characters.
--
{-# INLINE wordWithQuotes #-}
wordWithQuotes :: (Monad m, Eq a) =>
       Bool            -- ^ Retain the quotes and escape chars in the output
    -> (a -> a -> Maybe a)  -- ^ quote char -> escaped char -> translated char
    -> a               -- ^ Matches an escape elem?
    -> (a -> Maybe a)  -- ^ If left quote, return right quote, else Nothing.
    -> (a -> Bool)     -- ^ Matches a word separator?
    -> Fold m a b
    -> Parser a m b
wordWithQuotes keepQuotes tr escChar toRight isSep
    (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    -- Can be used to generate parse error for a bracket end without a bracket
    -- begin.
    isInvalid = const False

    isEsc = (== escChar)

    initial =  do
        res <- finitial
        return $
            case res of
                FL.Partial s -> IPartial (WordQuotedSkipPre s)
                FL.Done _ ->
                    error "wordKeepQuotes: fold done without input"

    {-# INLINE processQuoted #-}
    processQuoted s a n ql qr = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Continue 0 (WordQuotedWord s1 n ql qr)
                FL.Done b -> Done 0 b

    {-# INLINE processUnquoted #-}
    processUnquoted s a = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Continue 0 (WordUnquotedWord s1)
                FL.Done b -> Done 0 b

    step (WordQuotedSkipPre s) a
        | isEsc a = return $ Continue 0 $ WordUnquotedEsc s
        | isSep a = return $ Partial 0 $ WordQuotedSkipPre s
        | otherwise =
            case toRight a of
                Just qr ->
                  if keepQuotes
                  then processQuoted s a 1 a qr
                  else return $ Continue 0 $ WordQuotedWord s 1 a qr
                Nothing
                    | isInvalid a ->
                        return $ Error "wordKeepQuotes: invalid unquoted char"
                    | otherwise -> processUnquoted s a
    step (WordUnquotedWord s) a
        | isEsc a = return $ Continue 0 $ WordUnquotedEsc s
        | isSep a = do
            b <- fextract s
            return $ Partial 0 $ WordQuotedSkipPost b
        | otherwise = do
            case toRight a of
                Just qr ->
                    if keepQuotes
                    then processQuoted s a 1 a qr
                    else return $ Continue 0 $ WordQuotedWord s 1 a qr
                Nothing ->
                    if isInvalid a
                    then return $ Error "wordKeepQuotes: invalid unquoted char"
                    else processUnquoted s a
    step (WordQuotedWord s n ql qr) a
        | isEsc a = return $ Continue 0 $ WordQuotedEsc s n ql qr
        {-
        -- XXX Will this ever occur? Will n ever be 0?
        | n == 0 && isSep a = do
            b <- fextract s
            return $ Partial 0 $ WordQuotedSkipPost b
        -}
        | otherwise = do
                if a == qr
                then
                   if n == 1
                   then if keepQuotes
                        then processUnquoted s a
                        else return $ Continue 0 $ WordUnquotedWord s
                   else processQuoted s a (n - 1) ql qr
                else if a == ql
                     then processQuoted s a (n + 1) ql qr
                     else processQuoted s a n ql qr
    step (WordUnquotedEsc s) a = processUnquoted s a
    step (WordQuotedEsc s n ql qr) a =
        case tr ql a of
            Nothing -> do
                res <- fstep s escChar
                case res of
                    FL.Partial s1 -> processQuoted s1 a n ql qr
                    FL.Done b -> return $ Done 0 b
            Just x -> processQuoted s x n ql qr
    step (WordQuotedSkipPost b) a =
        return
            $ if not (isSep a)
              then Done 1 b
              else Partial 0 $ WordQuotedSkipPost b

    err = return . Error

    extract (WordQuotedSkipPre s) = fmap (Done 0) $ fextract s
    extract (WordUnquotedWord s) = fmap (Done 0) $ fextract s
    extract (WordQuotedWord s n _ _) =
        if n == 0
        then fmap (Done 0) $ fextract s
        else err "wordWithQuotes: missing frame end"
    extract WordQuotedEsc {} =
        err "wordWithQuotes: trailing escape"
    extract (WordUnquotedEsc _) =
        err "wordWithQuotes: trailing escape"
    extract (WordQuotedSkipPost b) = return (Done 0 b)

-- | 'wordWithQuotes' without processing the quotes and escape function
-- supplied to escape the quote char within a quote. Can be used to parse words
-- keeping the quotes and escapes intact.
--
-- >>> wordKeepQuotes = Parser.wordWithQuotes True (\_ _ -> Nothing)
--
{-# INLINE wordKeepQuotes #-}
wordKeepQuotes :: (Monad m, Eq a) =>
       a               -- ^ Escape char
    -> (a -> Maybe a)  -- ^ If left quote, return right quote, else Nothing.
    -> (a -> Bool)     -- ^ Matches a word separator?
    -> Fold m a b
    -> Parser a m b
wordKeepQuotes =
    -- Escape the quote char itself
    wordWithQuotes True (\q x -> if q == x then Just x else Nothing)

-- See the "Quoting Rules" section in the "bash" manual page for a primer on
-- how quotes are used by shells.

-- | 'wordWithQuotes' with quote processing applied and escape function
-- supplied to escape the quote char within a quote. Can be ysed to parse words
-- and processing the quoting and escaping at the same time.
--
-- >>> wordProcessQuotes = Parser.wordWithQuotes False (\_ _ -> Nothing)
--
{-# INLINE wordProcessQuotes #-}
wordProcessQuotes :: (Monad m, Eq a) =>
        a              -- ^ Escape char
    -> (a -> Maybe a)  -- ^ If left quote, return right quote, else Nothing.
    -> (a -> Bool)     -- ^ Matches a word separator?
    -> Fold m a b
    -> Parser a m b
wordProcessQuotes =
    -- Escape the quote char itself
    wordWithQuotes False (\q x -> if q == x then Just x else Nothing)

{-# ANN type GroupByState Fuse #-}
data GroupByState a s
    = GroupByInit !s
    | GroupByGrouping !a !s

-- | Given an input stream @[a,b,c,...]@ and a comparison function @cmp@, the
-- parser assigns the element @a@ to the first group, then if @a \`cmp` b@ is
-- 'True' @b@ is also assigned to the same group.  If @a \`cmp` c@ is 'True'
-- then @c@ is also assigned to the same group and so on. When the comparison
-- fails the parser is terminated. Each group is folded using the 'Fold' @f@ and
-- the result of the fold is the result of the parser.
--
-- * Stops - when the comparison fails.
-- * Fails - never.
--
-- >>> :{
--  runGroupsBy eq =
--      Stream.fold Fold.toList
--          . Stream.parseMany (Parser.groupBy eq Fold.toList)
--          . Stream.fromList
-- :}
--
-- >>> runGroupsBy (<) []
-- []
--
-- >>> runGroupsBy (<) [1]
-- [Right [1]]
--
-- >>> runGroupsBy (<) [3, 5, 4, 1, 2, 0]
-- [Right [3,5,4],Right [1,2],Right [0]]
--
{-# INLINE groupBy #-}
groupBy :: Monad m => (a -> a -> Bool) -> Fold m a b -> Parser a m b
groupBy eq (Fold fstep finitial fextract) = Parser step initial extract

    where

    {-# INLINE grouper #-}
    grouper s a0 a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Done b -> Done 0 b
                  FL.Partial s1 -> Partial 0 (GroupByGrouping a0 s1)

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s -> IPartial $ GroupByInit s
                  FL.Done b -> IDone b

    step (GroupByInit s) a = grouper s a a
    step (GroupByGrouping a0 s) a =
        if eq a0 a
        then grouper s a0 a
        else Done 1 <$> fextract s

    extract (GroupByInit s) = fmap (Done 0) $ fextract s
    extract (GroupByGrouping _ s) = fmap (Done 0) $ fextract s

-- | Unlike 'groupBy' this combinator performs a rolling comparison of two
-- successive elements in the input stream.  Assuming the input stream
-- is @[a,b,c,...]@ and the comparison function is @cmp@, the parser
-- first assigns the element @a@ to the first group, then if @a \`cmp` b@ is
-- 'True' @b@ is also assigned to the same group.  If @b \`cmp` c@ is 'True'
-- then @c@ is also assigned to the same group and so on. When the comparison
-- fails the parser is terminated. Each group is folded using the 'Fold' @f@ and
-- the result of the fold is the result of the parser.
--
-- * Stops - when the comparison fails.
-- * Fails - never.
--
-- >>> :{
--  runGroupsByRolling eq =
--      Stream.fold Fold.toList
--          . Stream.parseMany (Parser.groupByRolling eq Fold.toList)
--          . Stream.fromList
-- :}
--
-- >>> runGroupsByRolling (<) []
-- []
--
-- >>> runGroupsByRolling (<) [1]
-- [Right [1]]
--
-- >>> runGroupsByRolling (<) [3, 5, 4, 1, 2, 0]
-- [Right [3,5],Right [4],Right [1,2],Right [0]]
--
-- /Pre-release/
--
{-# INLINE groupByRolling #-}
groupByRolling :: Monad m => (a -> a -> Bool) -> Fold m a b -> Parser a m b
groupByRolling eq (Fold fstep finitial fextract) = Parser step initial extract

    where

    {-# INLINE grouper #-}
    grouper s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Done b -> Done 0 b
                  FL.Partial s1 -> Partial 0 (GroupByGrouping a s1)

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s -> IPartial $ GroupByInit s
                  FL.Done b -> IDone b

    step (GroupByInit s) a = grouper s a
    step (GroupByGrouping a0 s) a =
        if eq a0 a
        then grouper s a
        else Done 1 <$> fextract s

    extract (GroupByInit s) = fmap (Done 0) $ fextract s
    extract (GroupByGrouping _ s) = fmap (Done 0) $ fextract s

{-# ANN type GroupByStatePair Fuse #-}
data GroupByStatePair a s1 s2
    = GroupByInitPair !s1 !s2
    | GroupByGroupingPair !a !s1 !s2
    | GroupByGroupingPairL !a !s1 !s2
    | GroupByGroupingPairR !a !s1 !s2

-- | Like 'groupByRolling', but if the predicate is 'True' then collects using
-- the first fold as long as the predicate holds 'True', if the predicate is
-- 'False' collects using the second fold as long as it remains 'False'.
-- Returns 'Left' for the first case and 'Right' for the second case.
--
-- For example, if we want to detect sorted sequences in a stream, both
-- ascending and descending cases we can use 'groupByRollingEither (<=)
-- Fold.toList Fold.toList'.
--
-- /Pre-release/
{-# INLINE groupByRollingEither #-}
groupByRollingEither :: Monad m =>
    (a -> a -> Bool) -> Fold m a b -> Fold m a c -> Parser a m (Either b c)
groupByRollingEither
    eq
    (Fold fstep1 finitial1 fextract1)
    (Fold fstep2 finitial2 fextract2) = Parser step initial extract

    where

    {-# INLINE grouper #-}
    grouper s1 s2 a = do
        return $ Continue 0 (GroupByGroupingPair a s1 s2)

    {-# INLINE grouperL2 #-}
    grouperL2 s1 s2 a = do
        res <- fstep1 s1 a
        return
            $ case res of
                FL.Done b -> Done 0 (Left b)
                FL.Partial s11 -> Partial 0 (GroupByGroupingPairL a s11 s2)

    {-# INLINE grouperL #-}
    grouperL s1 s2 a0 a = do
        res <- fstep1 s1 a0
        case res of
            FL.Done b -> return $ Done 0 (Left b)
            FL.Partial s11 -> grouperL2 s11 s2 a

    {-# INLINE grouperR2 #-}
    grouperR2 s1 s2 a = do
        res <- fstep2 s2 a
        return
            $ case res of
                FL.Done b -> Done 0 (Right b)
                FL.Partial s21 -> Partial 0 (GroupByGroupingPairR a s1 s21)

    {-# INLINE grouperR #-}
    grouperR s1 s2 a0 a = do
        res <- fstep2 s2 a0
        case res of
            FL.Done b -> return $ Done 0 (Right b)
            FL.Partial s21 -> grouperR2 s1 s21 a

    initial = do
        res1 <- finitial1
        res2 <- finitial2
        return
            $ case res1 of
                FL.Partial s1 ->
                    case res2 of
                        FL.Partial s2 -> IPartial $ GroupByInitPair s1 s2
                        FL.Done b -> IDone (Right b)
                FL.Done b -> IDone (Left b)

    step (GroupByInitPair s1 s2) a = grouper s1 s2 a

    step (GroupByGroupingPair a0 s1 s2) a =
        if not (eq a0 a)
        then grouperL s1 s2 a0 a
        else grouperR s1 s2 a0 a

    step (GroupByGroupingPairL a0 s1 s2) a =
        if not (eq a0 a)
        then grouperL2 s1 s2 a
        else Done 1 . Left <$> fextract1 s1

    step (GroupByGroupingPairR a0 s1 s2) a =
        if eq a0 a
        then grouperR2 s1 s2 a
        else Done 1 . Right <$> fextract2 s2

    extract (GroupByInitPair s1 _) = Done 0 . Left <$> fextract1 s1
    extract (GroupByGroupingPairL _ s1 _) = Done 0 . Left <$> fextract1 s1
    extract (GroupByGroupingPairR _ _ s2) = Done 0 . Right <$> fextract2 s2
    extract (GroupByGroupingPair a s1 _) = do
                res <- fstep1 s1 a
                case res of
                    FL.Done b -> return $ Done 0 (Left b)
                    FL.Partial s11 -> Done 0 . Left <$> fextract1 s11

-- XXX use an Unfold instead of a list?
-- XXX custom combinators for matching list, array and stream?
-- XXX rename to listBy?

-- | Match the given sequence of elements using the given comparison function.
-- Returns the original sequence if successful.
--
-- Definition:
--
-- >>> listEqBy cmp xs = Parser.streamEqBy cmp (Stream.fromList xs) *> Parser.fromPure xs
--
-- Examples:
--
-- >>> Stream.parse (Parser.listEqBy (==) "string") $ Stream.fromList "string"
-- Right "string"
--
-- >>> Stream.parse (Parser.listEqBy (==) "mismatch") $ Stream.fromList "match"
-- Left (ParseError "streamEqBy: mismtach occurred")
--
{-# INLINE listEqBy #-}
listEqBy :: Monad m => (a -> a -> Bool) -> [a] -> Parser a m [a]
listEqBy cmp xs = streamEqByInternal cmp (D.fromList xs) *> fromPure xs
{-
listEqBy cmp str = Parser step initial extract

    where

    -- XXX Should return IDone in initial for [] case
    initial = return $ IPartial str

    step [] _ = return $ Done 0 str
    step [x] a =
        return
            $ if x `cmp` a
              then Done 0 str
              else Error "listEqBy: failed, yet to match the last element"
    step (x:xs) a =
        return
            $ if x `cmp` a
              then Continue 0 xs
              else Error
                       $ "listEqBy: failed, yet to match "
                       ++ show (length xs + 1) ++ " elements"

    extract xs =
        return
            $ Error
            $ "listEqBy: end of input, yet to match "
            ++ show (length xs) ++ " elements"
-}

{-# INLINE streamEqByInternal #-}
streamEqByInternal :: Monad m => (a -> a -> Bool) -> D.Stream m a -> Parser a m ()
streamEqByInternal cmp (D.Stream sstep state) = Parser step initial extract

    where

    initial = do
        r <- sstep defState state
        case r of
            D.Yield x s -> return $ IPartial (Just' x, s)
            D.Stop -> return $ IDone ()
            -- Need Skip/Continue in initial to loop right here
            D.Skip s -> return $ IPartial (Nothing', s)

    step (Just' x, st) a =
        if x `cmp` a
          then do
            r <- sstep defState st
            return
                $ case r of
                    D.Yield x1 s -> Continue 0 (Just' x1, s)
                    D.Stop -> Done 0 ()
                    D.Skip s -> Continue 1 (Nothing', s)
          else return $ Error "streamEqBy: mismtach occurred"
    step (Nothing', st) a = do
        r <- sstep defState st
        return
            $ case r of
                D.Yield x s -> do
                    if x `cmp` a
                    then Continue 0 (Nothing', s)
                    else Error "streamEqBy: mismatch occurred"
                D.Stop -> Done 1 ()
                D.Skip s -> Continue 1 (Nothing', s)

    extract _ = return $ Error "streamEqBy: end of input"

-- | Like 'listEqBy' but uses a stream instead of a list and does not return
-- the stream.
--
{-# INLINE streamEqBy #-}
streamEqBy :: Monad m => (a -> a -> Bool) -> D.Stream m a -> Parser a m ()
-- XXX Somehow composing this with "*>" is much faster on the microbenchmark.
-- Need to investigate why.
streamEqBy cmp stream = streamEqByInternal cmp stream *> fromPure ()

-- Rename to "list".
-- | Match the input sequence with the supplied list and return it if
-- successful.
--
-- >>> listEq = Parser.listEqBy (==)
--
{-# INLINE listEq #-}
listEq :: (Monad m, Eq a) => [a] -> Parser a m [a]
listEq = listEqBy (==)

-- | Match if the input stream is a subsequence of the argument stream i.e. all
-- the elements of the input stream occur, in order, in the argument stream.
-- The elements do not have to occur consecutively. A sequence is considered a
-- subsequence of itself.
{-# INLINE subsequenceBy #-}
subsequenceBy :: -- Monad m =>
    (a -> a -> Bool) -> Stream m a -> Parser a m ()
subsequenceBy = undefined

{-
-- Should go in Data.Parser.Regex in streamly package so that it can depend on
-- regex backends.
{-# INLINE regexPosix #-}
regexPosix :: -- Monad m =>
    Regex -> Parser m a (Maybe (Array (MatchOffset, MatchLength)))
regexPosix = undefined

{-# INLINE regexPCRE #-}
regexPCRE :: -- Monad m =>
    Regex -> Parser m a (Maybe (Array (MatchOffset, MatchLength)))
regexPCRE = undefined
-}

-------------------------------------------------------------------------------
-- Transformations on input
-------------------------------------------------------------------------------

-- Initial needs a "Continue" constructor to implement scans on parsers. As a
-- parser can always return a Continue in initial when we feed the fold's
-- initial result to it. We can work this around for postscan by introducing an
-- initial state and calling "initial" only on the first input.

-- | Stateful scan on the input of a parser using a Fold.
--
-- /Unimplemented/
--
{-# INLINE postscan #-}
postscan :: -- Monad m =>
    Fold m a b -> Parser b m c -> Parser a m c
postscan = undefined

{-# INLINE zipWithM #-}
zipWithM :: Monad m =>
    (a -> b -> m c) -> D.Stream m a -> Fold m c x -> Parser b m x
zipWithM zf (D.Stream sstep state) (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial = do
        fres <- finitial
        case fres of
            FL.Partial fs -> do
                r <- sstep defState state
                case r of
                    D.Yield x s -> return $ IPartial (Just' x, s, fs)
                    D.Stop -> do
                        x <- fextract fs
                        return $ IDone x
                    -- Need Skip/Continue in initial to loop right here
                    D.Skip s -> return $ IPartial (Nothing', s, fs)
            FL.Done x -> return $ IDone x

    step (Just' a, st, fs) b = do
        c <- zf a b
        fres <- fstep fs c
        case fres of
            FL.Partial fs1 -> do
                r <- sstep defState st
                case r of
                    D.Yield x1 s -> return $ Continue 0 (Just' x1, s, fs1)
                    D.Stop -> do
                        x <- fextract fs1
                        return $ Done 0 x
                    D.Skip s -> return $ Continue 1 (Nothing', s, fs1)
            FL.Done x -> return $ Done 0 x
    step (Nothing', st, fs) b = do
        r <- sstep defState st
        case r of
                D.Yield a s -> do
                    c <- zf a b
                    fres <- fstep fs c
                    case fres of
                        FL.Partial fs1 ->
                            return $ Continue 0 (Nothing', s, fs1)
                        FL.Done x -> return $ Done 0 x
                D.Stop -> do
                    x <- fextract fs
                    return $ Done 1 x
                D.Skip s -> return $ Continue 1 (Nothing', s, fs)

    extract _ = return $ Error "zipWithM: end of input"

-- | Zip the input of a fold with a stream.
--
-- /Pre-release/
--
{-# INLINE zip #-}
zip :: Monad m => D.Stream m a -> Fold m (a, b) x -> Parser b m x
zip = zipWithM (curry return)

-- | Pair each element of a fold input with its index, starting from index 0.
--
-- /Pre-release/
{-# INLINE indexed #-}
indexed :: forall m a b. Monad m => Fold m (Int, a) b -> Parser a m b
indexed = zip (D.enumerateFromIntegral 0 :: D.Stream m Int)

-- | @makeIndexFilter indexer filter predicate@ generates a fold filtering
-- function using a fold indexing function that attaches an index to each input
-- element and a filtering function that filters using @(index, element) ->
-- Bool) as predicate.
--
-- For example:
--
-- @
-- filterWithIndex = makeIndexFilter indexed filter
-- filterWithAbsTime = makeIndexFilter timestamped filter
-- filterWithRelTime = makeIndexFilter timeIndexed filter
-- @
--
-- /Pre-release/
{-# INLINE makeIndexFilter #-}
makeIndexFilter ::
       (Fold m (s, a) b -> Parser a m b)
    -> (((s, a) -> Bool) -> Fold m (s, a) b -> Fold m (s, a) b)
    -> (((s, a) -> Bool) -> Fold m a b -> Parser a m b)
makeIndexFilter f comb g = f . comb g . FL.lmap snd

-- | @sampleFromthen offset stride@ samples the element at @offset@ index and
-- then every element at strides of @stride@.
--
-- /Pre-release/
{-# INLINE sampleFromthen #-}
sampleFromthen :: Monad m => Int -> Int -> Fold m a b -> Parser a m b
sampleFromthen offset size =
    makeIndexFilter indexed FL.filter (\(i, _) -> (i + offset) `mod` size == 0)

--------------------------------------------------------------------------------
--- Spanning
--------------------------------------------------------------------------------

-- | @span p f1 f2@ composes folds @f1@ and @f2@ such that @f1@ consumes the
-- input as long as the predicate @p@ is 'True'.  @f2@ consumes the rest of the
-- input.
--
-- @
-- > let span_ p xs = Stream.parse (Parser.span p Fold.toList Fold.toList) $ Stream.fromList xs
--
-- > span_ (< 1) [1,2,3]
-- ([],[1,2,3])
--
-- > span_ (< 2) [1,2,3]
-- ([1],[2,3])
--
-- > span_ (< 4) [1,2,3]
-- ([1,2,3],[])
--
-- @
--
-- /Pre-release/
{-# INLINE span #-}
span :: Monad m => (a -> Bool) -> Fold m a b -> Fold m a c -> Parser a m (b, c)
span p f1 f2 = noErrorUnsafeSplitWith (,) (takeWhile p f1) (fromFold f2)

-- | Break the input stream into two groups, the first group takes the input as
-- long as the predicate applied to the first element of the stream and next
-- input element holds 'True', the second group takes the rest of the input.
--
-- /Pre-release/
--
{-# INLINE spanBy #-}
spanBy ::
       Monad m
    => (a -> a -> Bool) -> Fold m a b -> Fold m a c -> Parser a m (b, c)
spanBy eq f1 f2 = noErrorUnsafeSplitWith (,) (groupBy eq f1) (fromFold f2)

-- | Like 'spanBy' but applies the predicate in a rolling fashion i.e.
-- predicate is applied to the previous and the next input elements.
--
-- /Pre-release/
{-# INLINE spanByRolling #-}
spanByRolling ::
       Monad m
    => (a -> a -> Bool) -> Fold m a b -> Fold m a c -> Parser a m (b, c)
spanByRolling eq f1 f2 =
    noErrorUnsafeSplitWith (,) (groupByRolling eq f1) (fromFold f2)

-------------------------------------------------------------------------------
-- nested parsers
-------------------------------------------------------------------------------

-- | Takes at-most @n@ input elements.
--
-- * Stops - when the collecting parser stops.
-- * Fails - when the collecting parser fails.
--
-- >>> Stream.parse (Parser.takeP 4 (Parser.takeEQ 2 Fold.toList)) $ Stream.fromList [1, 2, 3, 4, 5]
-- Right [1,2]
--
-- >>> Stream.parse (Parser.takeP 4 (Parser.takeEQ 5 Fold.toList)) $ Stream.fromList [1, 2, 3, 4, 5]
-- Left (ParseError "takeEQ: Expecting exactly 5 elements, input terminated on 4")
--
-- /Internal/
{-# INLINE takeP #-}
takeP :: Monad m => Int -> Parser a m b -> Parser a m b
takeP lim (Parser pstep pinitial pextract) = Parser step initial extract

    where

    initial = do
        res <- pinitial
        case res of
            IPartial s ->
                if lim > 0
                then return $ IPartial $ Tuple' 0 s
                else iextract s
            IDone b -> return $ IDone b
            IError e -> return $ IError e

    step (Tuple' cnt r) a = do
        assertM(cnt < lim)
        res <- pstep r a
        let cnt1 = cnt + 1
        case res of
            Partial 0 s -> do
                assertM(cnt1 >= 0)
                if cnt1 < lim
                then return $ Partial 0 $ Tuple' cnt1 s
                else do
                    r1 <- pextract s
                    return $ case r1 of
                        Done n b -> Done n b
                        Continue n s1 -> Continue n (Tuple' (cnt1 - n) s1)
                        Error err -> Error err
                        Partial _ _ -> error "takeP: Partial in extract"

            Continue 0 s -> do
                assertM(cnt1 >= 0)
                if cnt1 < lim
                then return $ Continue 0 $ Tuple' cnt1 s
                else do
                    r1 <- pextract s
                    return $ case r1 of
                        Done n b -> Done n b
                        Continue n s1 -> Continue n (Tuple' (cnt1 - n) s1)
                        Error err -> Error err
                        Partial _ _ -> error "takeP: Partial in extract"
            Partial n s -> do
                let taken = cnt1 - n
                assertM(taken >= 0)
                return $ Partial n $ Tuple' taken s
            Continue n s -> do
                let taken = cnt1 - n
                assertM(taken >= 0)
                return $ Continue n $ Tuple' taken s
            Done n b -> return $ Done n b
            Error str -> return $ Error str

    extract (Tuple' cnt r) = do
        r1 <- pextract r
        return $ case r1 of
            Done n b -> Done n b
            Continue n s1 -> Continue n (Tuple' (cnt - n) s1)
            Error err -> Error err
            Partial _ _ -> error "takeP: Partial in extract"

    -- XXX Need to make the Initial type Step to remove this
    iextract s = do
        r <- pextract s
        return $ case r of
            Done _ b -> IDone b
            Error err -> IError err
            _ -> error "Bug: takeP invalid state in initial"

-- | Run a parser without consuming the input.
--
{-# INLINE lookAhead #-}
lookAhead :: Monad m => Parser a m b -> Parser a m b
lookAhead (Parser step1 initial1 _) = Parser step initial extract

    where

    initial = do
        res <- initial1
        return $ case res of
            IPartial s -> IPartial (Tuple'Fused 0 s)
            IDone b -> IDone b
            IError e -> IError e

    step (Tuple'Fused cnt st) a = do
        r <- step1 st a
        let cnt1 = cnt + 1
        return
            $ case r of
                  Partial n s -> Continue n (Tuple'Fused (cnt1 - n) s)
                  Continue n s -> Continue n (Tuple'Fused (cnt1 - n) s)
                  Done _ b -> Done cnt1 b
                  Error err -> Error err

    -- XXX returning an error let's us backtrack.  To implement it in a way so
    -- that it terminates on eof without an error then we need a way to
    -- backtrack on eof, that will require extract to return 'Step' type.
    extract (Tuple'Fused n _) =
        return
            $ Error
            $ "lookAhead: end of input after consuming "
            ++ show n ++ " elements"

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------
--
-- To deinterleave we can chain two parsers one behind the other. The input is
-- given to the first parser and the input definitively rejected by the first
-- parser is given to the second parser.
--
-- We can either have the parsers themselves buffer the input or use the shared
-- global buffer to hold it until none of the parsers need it. When the first
-- parser returns Skip (i.e. rewind) we let the second parser consume the
-- rejected input and when it is done we move the cursor forward to the first
-- parser again. This will require a "move forward" command as well.
--
-- To implement grep we can use three parsers, one to find the pattern, one
-- to store the context behind the pattern and one to store the context in
-- front of the pattern. When a match occurs we need to emit the accumulator of
-- all the three parsers. One parser can count the line numbers to provide the
-- line number info.

{-# ANN type DeintercalateAllState Fuse #-}
data DeintercalateAllState fs sp ss =
      DeintercalateAllInitL !fs
    | DeintercalateAllL !fs !sp
    | DeintercalateAllInitR !fs
    | DeintercalateAllR !fs !ss

-- XXX rename this to intercalate

-- Having deintercalateAll for accepting or rejecting entire input could be
-- useful. For example, in case of JSON parsing we get an entire block of
-- key-value pairs which we need to verify. This version may be simpler, more
-- efficient. We could implement this as a stream operation like parseMany.
--
-- XXX Also, it may be a good idea to provide a parse driver for a fold. For
-- example, in case of csv parsing as we are feeding a line to a fold we can
-- parse it.

-- | Like 'deintercalate' but the entire input must satisfy the pattern
-- otherwise the parser fails. This is many times faster than deintercalate.
--
-- >>> p1 = Parser.takeWhile1 (not . (== '+')) Fold.toList
-- >>> p2 = Parser.satisfy (== '+')
-- >>> p = Parser.deintercalateAll p1 p2 Fold.toList
-- >>> Stream.parse p $ Stream.fromList ""
-- Right []
-- >>> Stream.parse p $ Stream.fromList "1"
-- Right [Left "1"]
-- >>> Stream.parse p $ Stream.fromList "1+"
-- Left (ParseError "takeWhile1: end of input")
-- >>> Stream.parse p $ Stream.fromList "1+2+3"
-- Right [Left "1",Right '+',Left "2",Right '+',Left "3"]
--
{-# INLINE deintercalateAll #-}
deintercalateAll :: Monad m =>
       Parser a m x
    -> Parser a m y
    -> Fold m (Either x y) z
    -> Parser a m z
deintercalateAll
    (Parser stepL initialL extractL)
    (Parser stepR initialR _)
    (Fold fstep finitial fextract) = Parser step initial extract

    where

    errMsg p status =
        error $ "deintercalate: " ++ p ++ " parser cannot "
                ++ status ++ " without input"

    initial = do
        res <- finitial
        case res of
            FL.Partial fs -> return $ IPartial $ DeintercalateAllInitL fs
            FL.Done c -> return $ IDone c

    {-# INLINE processL #-}
    processL foldAction n nextState = do
        fres <- foldAction
        case fres of
            FL.Partial fs1 -> return $ Partial n (nextState fs1)
            FL.Done c -> return $ Done n c

    {-# INLINE runStepL #-}
    runStepL fs sL a = do
        r <- stepL sL a
        case r of
            Partial n s -> return $ Partial n (DeintercalateAllL fs s)
            Continue n s -> return $ Continue n (DeintercalateAllL fs s)
            Done n b ->
                processL (fstep fs (Left b)) n DeintercalateAllInitR
            Error err -> return $ Error err

    {-# INLINE processR #-}
    processR foldAction n = do
        fres <- foldAction
        case fres of
            FL.Partial fs1 -> do
                res <- initialL
                case res of
                    IPartial ps -> return $ Partial n (DeintercalateAllL fs1 ps)
                    IDone _ -> errMsg "left" "succeed"
                    IError _ -> errMsg "left" "fail"
            FL.Done c -> return $ Done n c

    {-# INLINE runStepR #-}
    runStepR fs sR a = do
        r <- stepR sR a
        case r of
            Partial n s -> return $ Partial n (DeintercalateAllR fs s)
            Continue n s -> return $ Continue n (DeintercalateAllR fs s)
            Done n b -> processR (fstep fs (Right b)) n
            Error err -> return $ Error err

    step (DeintercalateAllInitL fs) a = do
        res <- initialL
        case res of
            IPartial s -> runStepL fs s a
            IDone _ -> errMsg "left" "succeed"
            IError _ -> errMsg "left" "fail"
    step (DeintercalateAllL fs sL) a = runStepL fs sL a
    step (DeintercalateAllInitR fs) a = do
        res <- initialR
        case res of
            IPartial s -> runStepR fs s a
            IDone _ -> errMsg "right" "succeed"
            IError _ -> errMsg "right" "fail"
    step (DeintercalateAllR fs sR) a = runStepR fs sR a

    {-# INLINE extractResult #-}
    extractResult n fs r = do
        res <- fstep fs r
        case res of
            FL.Partial fs1 -> fmap (Done n) $ fextract fs1
            FL.Done c -> return (Done n c)
    extract (DeintercalateAllInitL fs) = fmap (Done 0) $ fextract fs
    extract (DeintercalateAllL fs sL) = do
        r <- extractL sL
        case r of
            Done n b -> extractResult n fs (Left b)
            Error err -> return $ Error err
            Continue n s -> return $ Continue n (DeintercalateAllL fs s)
            Partial _ _ -> error "Partial in extract"
    extract (DeintercalateAllInitR fs) = fmap (Done 0) $ fextract fs
    extract (DeintercalateAllR _ _) =
        return $ Error "deintercalateAll: input ended at 'Right' value"

{-# ANN type DeintercalateState Fuse #-}
data DeintercalateState b fs sp ss =
      DeintercalateInitL !fs
    | DeintercalateL !Int !fs !sp
    | DeintercalateInitR !fs
    | DeintercalateR !Int !fs !ss
    | DeintercalateRL !Int !b !fs !sp

-- XXX Add tests that the next character that we take after running a parser is
-- correct. Especially for the parsers that maintain a count. In the stream
-- finished case (extract) as well as not finished case.

-- | Apply two parsers alternately to an input stream. The input stream is
-- considered an interleaving of two patterns. The two parsers represent the
-- two patterns. Parsing starts at the first parser and stops at the first
-- parser. It can be used to parse a infix style pattern e.g. p1 p2 p1 . Empty
-- input or single parse of the first parser is accepted.
--
-- >>> p1 = Parser.takeWhile1 (not . (== '+')) Fold.toList
-- >>> p2 = Parser.satisfy (== '+')
-- >>> p = Parser.deintercalate p1 p2 Fold.toList
-- >>> Stream.parse p $ Stream.fromList ""
-- Right []
-- >>> Stream.parse p $ Stream.fromList "1"
-- Right [Left "1"]
-- >>> Stream.parse p $ Stream.fromList "1+"
-- Right [Left "1"]
-- >>> Stream.parse p $ Stream.fromList "1+2+3"
-- Right [Left "1",Right '+',Left "2",Right '+',Left "3"]
--
{-# INLINE deintercalate #-}
deintercalate :: Monad m =>
       Parser a m x
    -> Parser a m y
    -> Fold m (Either x y) z
    -> Parser a m z
deintercalate
    (Parser stepL initialL extractL)
    (Parser stepR initialR _)
    (Fold fstep finitial fextract) = Parser step initial extract

    where

    errMsg p status =
        error $ "deintercalate: " ++ p ++ " parser cannot "
                ++ status ++ " without input"

    initial = do
        res <- finitial
        case res of
            FL.Partial fs -> return $ IPartial $ DeintercalateInitL fs
            FL.Done c -> return $ IDone c

    {-# INLINE processL #-}
    processL foldAction n nextState = do
        fres <- foldAction
        case fres of
            FL.Partial fs1 -> return $ Partial n (nextState fs1)
            FL.Done c -> return $ Done n c

    {-# INLINE runStepL #-}
    runStepL cnt fs sL a = do
        let cnt1 = cnt + 1
        r <- stepL sL a
        case r of
            Partial n s -> return $ Continue n (DeintercalateL (cnt1 - n) fs s)
            Continue n s -> return $ Continue n (DeintercalateL (cnt1 - n) fs s)
            Done n b ->
                processL (fstep fs (Left b)) n DeintercalateInitR
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt1 xs

    {-# INLINE processR #-}
    processR cnt b fs n = do
        res <- initialL
        case res of
            IPartial ps -> return $ Continue n (DeintercalateRL cnt b fs ps)
            IDone _ -> errMsg "left" "succeed"
            IError _ -> errMsg "left" "fail"

    {-# INLINE runStepR #-}
    runStepR cnt fs sR a = do
        let cnt1 = cnt + 1
        r <- stepR sR a
        case r of
            Partial n s -> return $ Continue n (DeintercalateR (cnt1 - n) fs s)
            Continue n s -> return $ Continue n (DeintercalateR (cnt1 - n) fs s)
            Done n b -> processR (cnt1 - n) b fs n
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt1 xs

    step (DeintercalateInitL fs) a = do
        res <- initialL
        case res of
            IPartial s -> runStepL 0 fs s a
            IDone _ -> errMsg "left" "succeed"
            IError _ -> errMsg "left" "fail"
    step (DeintercalateL cnt fs sL) a = runStepL cnt fs sL a
    step (DeintercalateInitR fs) a = do
        res <- initialR
        case res of
            IPartial s -> runStepR 0 fs s a
            IDone _ -> errMsg "right" "succeed"
            IError _ -> errMsg "right" "fail"
    step (DeintercalateR cnt fs sR) a = runStepR cnt fs sR a
    step (DeintercalateRL cnt bR fs sL) a = do
        let cnt1 = cnt + 1
        r <- stepL sL a
        case r of
            Partial n s -> return $ Continue n (DeintercalateRL (cnt1 - n) bR fs s)
            Continue n s -> return $ Continue n (DeintercalateRL (cnt1 - n) bR fs s)
            Done n bL -> do
                res <- fstep fs (Right bR)
                case res of
                    FL.Partial fs1 -> do
                        fres <- fstep fs1 (Left bL)
                        case fres of
                            FL.Partial fs2 ->
                                return $ Partial n (DeintercalateInitR fs2)
                            FL.Done c -> return $ Done n c
                    -- XXX We could have the fold accept pairs of (bR, bL)
                    FL.Done _ -> error "Fold terminated consuming partial input"
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt1 xs

    {-# INLINE extractResult #-}
    extractResult n fs r = do
        res <- fstep fs r
        case res of
            FL.Partial fs1 -> fmap (Done n) $ fextract fs1
            FL.Done c -> return (Done n c)

    extract (DeintercalateInitL fs) = fmap (Done 0) $ fextract fs
    extract (DeintercalateL cnt fs sL) = do
        r <- extractL sL
        case r of
            Done n b -> extractResult n fs (Left b)
            Continue n s -> return $ Continue n (DeintercalateL (cnt - n) fs s)
            Partial _ _ -> error "Partial in extract"
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt xs
    extract (DeintercalateInitR fs) = fmap (Done 0) $ fextract fs
    extract (DeintercalateR cnt fs _) = fmap (Done cnt) $ fextract fs
    extract (DeintercalateRL cnt bR fs sL) = do
        r <- extractL sL
        case r of
            Done n bL -> do
                res <- fstep fs (Right bR)
                case res of
                    FL.Partial fs1 -> extractResult n fs1 (Left bL)
                    FL.Done _ -> error "Fold terminated consuming partial input"
            Continue n s -> return $ Continue n (DeintercalateRL (cnt - n) bR fs s)
            Partial _ _ -> error "Partial in extract"
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt xs

{-# ANN type Deintercalate1State Fuse #-}
data Deintercalate1State b fs sp ss =
      Deintercalate1InitL !Int !fs !sp
    | Deintercalate1InitR !fs
    | Deintercalate1R !Int !fs !ss
    | Deintercalate1RL !Int !b !fs !sp

-- | Apply two parsers alternately to an input stream. The input stream is
-- considered an interleaving of two patterns. The two parsers represent the
-- two patterns. Parsing starts at the first parser and stops at the first
-- parser. It can be used to parse a infix style pattern e.g. p1 p2 p1 . Empty
-- input or single parse of the first parser is accepted.
--
-- >>> p1 = Parser.takeWhile1 (not . (== '+')) Fold.toList
-- >>> p2 = Parser.satisfy (== '+')
-- >>> p = Parser.deintercalate1 p1 p2 Fold.toList
-- >>> Stream.parse p $ Stream.fromList ""
-- Left (ParseError "takeWhile1: end of input")
-- >>> Stream.parse p $ Stream.fromList "1"
-- Right [Left "1"]
-- >>> Stream.parse p $ Stream.fromList "1+"
-- Right [Left "1"]
-- >>> Stream.parse p $ Stream.fromList "1+2+3"
-- Right [Left "1",Right '+',Left "2",Right '+',Left "3"]
--
{-# INLINE deintercalate1 #-}
deintercalate1 :: Monad m =>
       Parser a m x
    -> Parser a m y
    -> Fold m (Either x y) z
    -> Parser a m z
deintercalate1
    (Parser stepL initialL extractL)
    (Parser stepR initialR _)
    (Fold fstep finitial fextract) = Parser step initial extract

    where

    errMsg p status =
        error $ "deintercalate: " ++ p ++ " parser cannot "
                ++ status ++ " without input"

    initial = do
        res <- finitial
        case res of
            FL.Partial fs -> do
                pres <- initialL
                case pres of
                    IPartial s -> return $ IPartial $ Deintercalate1InitL 0 fs s
                    IDone _ -> errMsg "left" "succeed"
                    IError _ -> errMsg "left" "fail"
            FL.Done c -> return $ IDone c

    {-# INLINE processL #-}
    processL foldAction n nextState = do
        fres <- foldAction
        case fres of
            FL.Partial fs1 -> return $ Partial n (nextState fs1)
            FL.Done c -> return $ Done n c

    {-# INLINE runStepInitL #-}
    runStepInitL cnt fs sL a = do
        let cnt1 = cnt + 1
        r <- stepL sL a
        case r of
            Partial n s -> return $ Continue n (Deintercalate1InitL (cnt1 - n) fs s)
            Continue n s -> return $ Continue n (Deintercalate1InitL (cnt1 - n) fs s)
            Done n b ->
                processL (fstep fs (Left b)) n Deintercalate1InitR
            Error err -> return $ Error err

    {-# INLINE processR #-}
    processR cnt b fs n = do
        res <- initialL
        case res of
            IPartial ps -> return $ Continue n (Deintercalate1RL cnt b fs ps)
            IDone _ -> errMsg "left" "succeed"
            IError _ -> errMsg "left" "fail"

    {-# INLINE runStepR #-}
    runStepR cnt fs sR a = do
        let cnt1 = cnt + 1
        r <- stepR sR a
        case r of
            Partial n s -> return $ Continue n (Deintercalate1R (cnt1 - n) fs s)
            Continue n s -> return $ Continue n (Deintercalate1R (cnt1 - n) fs s)
            Done n b -> processR (cnt1 - n) b fs n
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt1 xs

    step (Deintercalate1InitL cnt fs sL) a = runStepInitL cnt fs sL a
    step (Deintercalate1InitR fs) a = do
        res <- initialR
        case res of
            IPartial s -> runStepR 0 fs s a
            IDone _ -> errMsg "right" "succeed"
            IError _ -> errMsg "right" "fail"
    step (Deintercalate1R cnt fs sR) a = runStepR cnt fs sR a
    step (Deintercalate1RL cnt bR fs sL) a = do
        let cnt1 = cnt + 1
        r <- stepL sL a
        case r of
            Partial n s -> return $ Continue n (Deintercalate1RL (cnt1 - n) bR fs s)
            Continue n s -> return $ Continue n (Deintercalate1RL (cnt1 - n) bR fs s)
            Done n bL -> do
                res <- fstep fs (Right bR)
                case res of
                    FL.Partial fs1 -> do
                        fres <- fstep fs1 (Left bL)
                        case fres of
                            FL.Partial fs2 ->
                                return $ Partial n (Deintercalate1InitR fs2)
                            FL.Done c -> return $ Done n c
                    -- XXX We could have the fold accept pairs of (bR, bL)
                    FL.Done _ -> error "Fold terminated consuming partial input"
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt1 xs

    {-# INLINE extractResult #-}
    extractResult n fs r = do
        res <- fstep fs r
        case res of
            FL.Partial fs1 -> fmap (Done n) $ fextract fs1
            FL.Done c -> return (Done n c)

    extract (Deintercalate1InitL cnt fs sL) = do
        r <- extractL sL
        case r of
            Done n b -> extractResult n fs (Left b)
            Continue n s -> return $ Continue n (Deintercalate1InitL (cnt - n) fs s)
            Partial _ _ -> error "Partial in extract"
            Error err -> return $ Error err
    extract (Deintercalate1InitR fs) = fmap (Done 0) $ fextract fs
    extract (Deintercalate1R cnt fs _) = fmap (Done cnt) $ fextract fs
    extract (Deintercalate1RL cnt bR fs sL) = do
        r <- extractL sL
        case r of
            Done n bL -> do
                res <- fstep fs (Right bR)
                case res of
                    FL.Partial fs1 -> extractResult n fs1 (Left bL)
                    FL.Done _ -> error "Fold terminated consuming partial input"
            Continue n s -> return $ Continue n (Deintercalate1RL (cnt - n) bR fs s)
            Partial _ _ -> error "Partial in extract"
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt xs

{-# ANN type SepByState Fuse #-}
data SepByState fs sp ss =
      SepByInitL !fs
    | SepByL !Int !fs !sp
    | SepByInitR !fs
    | SepByR !Int !fs !ss

-- | Apply two parsers alternately to an input stream. Parsing starts at the
-- first parser and stops at the first parser. The output of the first parser
-- is emiited and the output of the second parser is discarded. It can be used
-- to parse a infix style pattern e.g. p1 p2 p1 . Empty input or single parse
-- of the first parser is accepted.
--
-- Definitions:
--
-- >>> sepBy p1 p2 f = Parser.deintercalate p1 p2 (Fold.catLefts f)
-- >>> sepBy p1 p2 f = Parser.sepBy1 p1 p2 f <|> Parser.fromEffect (Fold.extractM f)
--
-- Examples:
--
-- >>> p1 = Parser.takeWhile1 (not . (== '+')) Fold.toList
-- >>> p2 = Parser.satisfy (== '+')
-- >>> p = Parser.sepBy p1 p2 Fold.toList
-- >>> Stream.parse p $ Stream.fromList ""
-- Right []
-- >>> Stream.parse p $ Stream.fromList "1"
-- Right ["1"]
-- >>> Stream.parse p $ Stream.fromList "1+"
-- Right ["1"]
-- >>> Stream.parse p $ Stream.fromList "1+2+3"
-- Right ["1","2","3"]
--
{-# INLINE sepBy #-}
sepBy :: Monad m =>
    Parser a m b -> Parser a m x -> Fold m b c -> Parser a m c
-- This has similar performance as the custom impl below.
-- sepBy p1 p2 f = deintercalate p1 p2 (FL.catLefts f)
sepBy
    (Parser stepL initialL extractL)
    (Parser stepR initialR _)
    (Fold fstep finitial fextract) = Parser step initial extract

    where

    errMsg p status =
        error $ "sepBy: " ++ p ++ " parser cannot "
                ++ status ++ " without input"

    initial = do
        res <- finitial
        case res of
            FL.Partial fs -> return $ IPartial $ SepByInitL fs
            FL.Done c -> return $ IDone c

    {-# INLINE processL #-}
    processL foldAction n nextState = do
        fres <- foldAction
        case fres of
            FL.Partial fs1 -> return $ Partial n (nextState fs1)
            FL.Done c -> return $ Done n c

    {-# INLINE runStepL #-}
    runStepL cnt fs sL a = do
        let cnt1 = cnt + 1
        r <- stepL sL a
        case r of
            Partial n s -> return $ Continue n (SepByL (cnt1 - n) fs s)
            Continue n s -> return $ Continue n (SepByL (cnt1 - n) fs s)
            Done n b ->
                processL (fstep fs b) n SepByInitR
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt1 xs

    {-# INLINE processR #-}
    processR cnt fs n = do
        res <- initialL
        case res of
            IPartial ps -> return $ Continue n (SepByL cnt fs ps)
            IDone _ -> errMsg "left" "succeed"
            IError _ -> errMsg "left" "fail"

    {-# INLINE runStepR #-}
    runStepR cnt fs sR a = do
        let cnt1 = cnt + 1
        r <- stepR sR a
        case r of
            Partial n s -> return $ Continue n (SepByR (cnt1 - n) fs s)
            Continue n s -> return $ Continue n (SepByR (cnt1 - n) fs s)
            Done n _ -> processR (cnt1 - n) fs n
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt1 xs

    step (SepByInitL fs) a = do
        res <- initialL
        case res of
            IPartial s -> runStepL 0 fs s a
            IDone _ -> errMsg "left" "succeed"
            IError _ -> errMsg "left" "fail"
    step (SepByL cnt fs sL) a = runStepL cnt fs sL a
    step (SepByInitR fs) a = do
        res <- initialR
        case res of
            IPartial s -> runStepR 0 fs s a
            IDone _ -> errMsg "right" "succeed"
            IError _ -> errMsg "right" "fail"
    step (SepByR cnt fs sR) a = runStepR cnt fs sR a

    {-# INLINE extractResult #-}
    extractResult n fs r = do
        res <- fstep fs r
        case res of
            FL.Partial fs1 -> fmap (Done n) $ fextract fs1
            FL.Done c -> return (Done n c)

    extract (SepByInitL fs) = fmap (Done 0) $ fextract fs
    extract (SepByL cnt fs sL) = do
        r <- extractL sL
        case r of
            Done n b -> extractResult n fs b
            Continue n s -> return $ Continue n (SepByL (cnt - n) fs s)
            Partial _ _ -> error "Partial in extract"
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt xs
    extract (SepByInitR fs) = fmap (Done 0) $ fextract fs
    extract (SepByR cnt fs _) = fmap (Done cnt) $ fextract fs

-- | Non-backtracking version of sepBy. Several times faster.
{-# INLINE sepByAll #-}
sepByAll :: Monad m =>
    Parser a m b -> Parser a m x -> Fold m b c -> Parser a m c
sepByAll p1 p2 f = deintercalateAll p1 p2 (FL.catLefts f)

-- XXX This can be implemented using refold, parse one and then continue
-- collecting the rest in that.

{-# ANN type SepBy1State Fuse #-}
data SepBy1State fs sp ss =
      SepBy1InitL !Int !fs sp
    | SepBy1L !Int !fs !sp
    | SepBy1InitR !fs
    | SepBy1R !Int !fs !ss

{-
{-# INLINE sepBy1 #-}
sepBy1 :: Monad m =>
    Parser a m b -> Parser a m x -> Fold m b c -> Parser a m c
sepBy1 p sep sink = do
    x <- p
    f <- fromEffect $ FL.reduce sink
    f1 <- fromEffect $ FL.snoc f x
    many (sep >> p) f1
-}

-- | Like 'sepBy' but requires at least one successful parse.
--
-- Definition:
--
-- >>> sepBy1 p1 p2 f = Parser.deintercalate1 p1 p2 (Fold.catLefts f)
--
-- Examples:
--
-- >>> p1 = Parser.takeWhile1 (not . (== '+')) Fold.toList
-- >>> p2 = Parser.satisfy (== '+')
-- >>> p = Parser.sepBy1 p1 p2 Fold.toList
-- >>> Stream.parse p $ Stream.fromList ""
-- Left (ParseError "takeWhile1: end of input")
-- >>> Stream.parse p $ Stream.fromList "1"
-- Right ["1"]
-- >>> Stream.parse p $ Stream.fromList "1+"
-- Right ["1"]
-- >>> Stream.parse p $ Stream.fromList "1+2+3"
-- Right ["1","2","3"]
--
{-# INLINE sepBy1 #-}
sepBy1 :: Monad m =>
    Parser a m b -> Parser a m x -> Fold m b c -> Parser a m c
sepBy1
    (Parser stepL initialL extractL)
    (Parser stepR initialR _)
    (Fold fstep finitial fextract) = Parser step initial extract

    where

    errMsg p status =
        error $ "sepBy: " ++ p ++ " parser cannot "
                ++ status ++ " without input"

    initial = do
        res <- finitial
        case res of
            FL.Partial fs -> do
                pres <- initialL
                case pres of
                    IPartial s -> return $ IPartial $ SepBy1InitL 0 fs s
                    IDone _ -> errMsg "left" "succeed"
                    IError _ -> errMsg "left" "fail"
            FL.Done c -> return $ IDone c

    {-# INLINE processL #-}
    processL foldAction n nextState = do
        fres <- foldAction
        case fres of
            FL.Partial fs1 -> return $ Partial n (nextState fs1)
            FL.Done c -> return $ Done n c

    {-# INLINE runStepInitL #-}
    runStepInitL cnt fs sL a = do
        let cnt1 = cnt + 1
        r <- stepL sL a
        case r of
            Partial n s -> return $ Continue n (SepBy1InitL (cnt1 - n) fs s)
            Continue n s -> return $ Continue n (SepBy1InitL (cnt1 - n) fs s)
            Done n b ->
                processL (fstep fs b) n SepBy1InitR
            Error err -> return $ Error err

    {-# INLINE runStepL #-}
    runStepL cnt fs sL a = do
        let cnt1 = cnt + 1
        r <- stepL sL a
        case r of
            Partial n s -> return $ Continue n (SepBy1L (cnt1 - n) fs s)
            Continue n s -> return $ Continue n (SepBy1L (cnt1 - n) fs s)
            Done n b ->
                processL (fstep fs b) n SepBy1InitR
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt1 xs

    {-# INLINE processR #-}
    processR cnt fs n = do
        res <- initialL
        case res of
            IPartial ps -> return $ Continue n (SepBy1L cnt fs ps)
            IDone _ -> errMsg "left" "succeed"
            IError _ -> errMsg "left" "fail"

    {-# INLINE runStepR #-}
    runStepR cnt fs sR a = do
        let cnt1 = cnt + 1
        r <- stepR sR a
        case r of
            Partial n s -> return $ Continue n (SepBy1R (cnt1 - n) fs s)
            Continue n s -> return $ Continue n (SepBy1R (cnt1 - n) fs s)
            Done n _ -> processR (cnt1 - n) fs n
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt1 xs

    step (SepBy1InitL cnt fs sL) a = runStepInitL cnt fs sL a
    step (SepBy1L cnt fs sL) a = runStepL cnt fs sL a
    step (SepBy1InitR fs) a = do
        res <- initialR
        case res of
            IPartial s -> runStepR 0 fs s a
            IDone _ -> errMsg "right" "succeed"
            IError _ -> errMsg "right" "fail"
    step (SepBy1R cnt fs sR) a = runStepR cnt fs sR a

    {-# INLINE extractResult #-}
    extractResult n fs r = do
        res <- fstep fs r
        case res of
            FL.Partial fs1 -> fmap (Done n) $ fextract fs1
            FL.Done c -> return (Done n c)

    extract (SepBy1InitL cnt fs sL) = do
        r <- extractL sL
        case r of
            Done n b -> extractResult n fs b
            Continue n s -> return $ Continue n (SepBy1InitL (cnt - n) fs s)
            Partial _ _ -> error "Partial in extract"
            Error err -> return $ Error err
    extract (SepBy1L cnt fs sL) = do
        r <- extractL sL
        case r of
            Done n b -> extractResult n fs b
            Continue n s -> return $ Continue n (SepBy1L (cnt - n) fs s)
            Partial _ _ -> error "Partial in extract"
            Error _ -> do
                xs <- fextract fs
                return $ Done cnt xs
    extract (SepBy1InitR fs) = fmap (Done 0) $ fextract fs
    extract (SepBy1R cnt fs _) = fmap (Done cnt) $ fextract fs

-------------------------------------------------------------------------------
-- Interleaving a collection of parsers
-------------------------------------------------------------------------------
--
-- | Apply a collection of parsers to an input stream in a round robin fashion.
-- Each parser is applied until it stops and then we repeat starting with the
-- the first parser again.
--
-- /Unimplemented/
--
{-# INLINE roundRobin #-}
roundRobin :: -- (Foldable t, Monad m) =>
    t (Parser a m b) -> Fold m b c -> Parser a m c
roundRobin _ps _f = undefined

-------------------------------------------------------------------------------
-- Sequential Collection
-------------------------------------------------------------------------------

-- | @sequence f p@ collects sequential parses of parsers in a
-- serial stream @p@ using the fold @f@. Fails if the input ends or any
-- of the parsers fail.
--
-- /Pre-release/
--
{-# INLINE sequence #-}
sequence :: Monad m =>
    D.Stream m (Parser a m b) -> Fold m b c -> Parser a m c
sequence (D.Stream sstep sstate) (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial = do
        fres <- finitial
        case fres of
            FL.Partial fs -> return $ IPartial (Nothing', sstate, fs)
            FL.Done c -> return $ IDone c

    -- state does not contain any parser
    -- yield a new parser from the stream
    step (Nothing', ss, fs) _ = do
        sres <- sstep defState ss
        case sres of
            D.Yield p ss1 -> return $ Continue 1 (Just' p, ss1, fs)
            D.Stop -> do
                c <- fextract fs
                return $ Done 1 c
            D.Skip ss1 -> return $ Continue 1 (Nothing', ss1, fs)

    -- state holds a parser that may or may not have been
    -- initialized. pinit holds the initial parser state
    -- or modified parser state respectively
    step (Just' (Parser pstep pinit pextr), ss, fs) a = do
        ps <- pinit
        case ps of
            IPartial ps1 -> do
                pres <- pstep ps1 a
                case pres of
                    Partial n ps2 ->
                        let newP =
                              Just' $ Parser pstep (return $ IPartial ps2) pextr
                        in return $ Partial n (newP, ss, fs)
                    Continue n ps2 ->
                        let newP =
                              Just' $ Parser pstep (return $ IPartial ps2) pextr
                        in return $ Continue n (newP, ss, fs)
                    Done n b -> do
                        fres <- fstep fs b
                        case fres of
                            FL.Partial fs1 ->
                                return $ Partial n (Nothing', ss, fs1)
                            FL.Done c -> return $ Done n c
                    Error msg -> return $ Error msg
            IDone b -> do
                fres <- fstep fs b
                case fres of
                    FL.Partial fs1 ->
                        return $ Partial 1 (Nothing', ss, fs1)
                    FL.Done c -> return $ Done 1 c
            IError err -> return $ Error err

    extract (Nothing', _, fs) = fmap (Done 0) $ fextract fs
    extract (Just' (Parser pstep pinit pextr), ss, fs) = do
        ps <- pinit
        case ps of
            IPartial ps1 ->  do
                r <- pextr ps1
                case r of
                    Done n b -> do
                        res <- fstep fs b
                        case res of
                            FL.Partial fs1 -> fmap (Done n) $ fextract fs1
                            FL.Done c -> return (Done n c)
                    Error err -> return $ Error err
                    Continue n s -> return $ Continue n (Just' (Parser pstep (return (IPartial s)) pextr), ss, fs)
                    Partial _ _ -> error "Partial in extract"
            IDone b -> do
                fres <- fstep fs b
                case fres of
                    FL.Partial fs1 -> fmap (Done 0) $ fextract fs1
                    FL.Done c -> return (Done 0 c)
            IError err -> return $ Error err

-------------------------------------------------------------------------------
-- Alternative Collection
-------------------------------------------------------------------------------

{-
-- | @choice parsers@ applies the @parsers@ in order and returns the first
-- successful parse.
--
-- This is same as 'asum' but more efficient.
--
-- /Broken/
--
{-# INLINE choice #-}
choice :: (MonadCatch m, Foldable t) => t (Parser a m b) -> Parser a m b
choice = foldl1 shortest
-}

-------------------------------------------------------------------------------
-- Sequential Repetition
-------------------------------------------------------------------------------

-- | Like 'many' but uses a 'Parser' instead of a 'Fold' to collect the
-- results. Parsing stops or fails if the collecting parser stops or fails.
--
-- /Unimplemented/
--
{-# INLINE manyP #-}
manyP :: -- MonadCatch m =>
    Parser a m b -> Parser b m c -> Parser a m c
manyP _p _f = undefined

-- | Collect zero or more parses. Apply the supplied parser repeatedly on the
-- input stream and push the parse results to a downstream fold.
--
--  Stops: when the downstream fold stops or the parser fails.
--  Fails: never, produces zero or more results.
--
-- >>> many = Parser.countBetween 0 maxBound
--
-- Compare with 'Control.Applicative.many'.
--
{-# INLINE many #-}
many :: Monad m => Parser a m b -> Fold m b c -> Parser a m c
many = splitMany
-- many = countBetween 0 maxBound

-- Note: many1 would perhaps be a better name for this and consistent with
-- other names like takeWhile1. But we retain the name "some" for
-- compatibility.

-- | Collect one or more parses. Apply the supplied parser repeatedly on the
-- input stream and push the parse results to a downstream fold.
--
--  Stops: when the downstream fold stops or the parser fails.
--  Fails: if it stops without producing a single result.
--
-- >>> some p f = Parser.manyP p (Parser.takeGE 1 f)
-- >>> some = Parser.countBetween 1 maxBound
--
-- Compare with 'Control.Applicative.some'.
--
{-# INLINE some #-}
some :: Monad m => Parser a m b -> Fold m b c -> Parser a m c
some = splitSome
-- some p f = manyP p (takeGE 1 f)
-- some = countBetween 1 maxBound

-- | @countBetween m n f p@ collects between @m@ and @n@ sequential parses of
-- parser @p@ using the fold @f@. Stop after collecting @n@ results. Fails if
-- the input ends or the parser fails before @m@ results are collected.
--
-- >>> countBetween m n p f = Parser.manyP p (Parser.takeBetween m n f)
--
-- /Unimplemented/
--
{-# INLINE countBetween #-}
countBetween :: -- MonadCatch m =>
    Int -> Int -> Parser a m b -> Fold m b c -> Parser a m c
countBetween _m _n _p = undefined
-- countBetween m n p f = manyP p (takeBetween m n f)

-- | @count n f p@ collects exactly @n@ sequential parses of parser @p@ using
-- the fold @f@.  Fails if the input ends or the parser fails before @n@
-- results are collected.
--
-- >>> count n = Parser.countBetween n n
-- >>> count n p f = Parser.manyP p (Parser.takeEQ n f)
--
-- /Unimplemented/
--
{-# INLINE count #-}
count :: -- MonadCatch m =>
    Int -> Parser a m b -> Fold m b c -> Parser a m c
count n = countBetween n n
-- count n p f = manyP p (takeEQ n f)

-- | Like 'manyTill' but uses a 'Parser' to collect the results instead of a
-- 'Fold'.  Parsing stops or fails if the collecting parser stops or fails.
--
-- We can implemnent parsers like the following using 'manyTillP':
--
-- @
-- countBetweenTill m n f p = manyTillP (takeBetween m n f) p
-- @
--
-- /Unimplemented/
--
{-# INLINE manyTillP #-}
manyTillP :: -- Monad m =>
    Parser a m b -> Parser a m x -> Parser b m c -> Parser a m c
manyTillP _p1 _p2 _f = undefined
    -- D.toParserK $ D.manyTillP (D.fromParserK p1) (D.fromParserK p2) f

{-# ANN type ManyTillState Fuse #-}
data ManyTillState fs sr sl
    = ManyTillR !Int !fs !sr
    | ManyTillL !fs !sl

-- | @manyTill chunking test f@ tries the parser @test@ on the input, if @test@
-- fails it backtracks and tries @chunking@, after @chunking@ succeeds @test@ is
-- tried again and so on. The parser stops when @test@ succeeds.  The output of
-- @test@ is discarded and the output of @chunking@ is accumulated by the
-- supplied fold. The parser fails if @chunking@ fails.
--
-- Stops when the fold @f@ stops.
--
{-# INLINE manyTill #-}
manyTill :: Monad m
    => Parser a m b -> Parser a m x -> Fold m b c -> Parser a m c
manyTill (Parser stepL initialL extractL)
         (Parser stepR initialR _)
         (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    -- Caution: Mutual recursion

    scrutL fs p c d e = do
        resL <- initialL
        case resL of
            IPartial sl -> return $ c (ManyTillL fs sl)
            IDone bl -> do
                fr <- fstep fs bl
                case fr of
                    FL.Partial fs1 -> scrutR fs1 p c d e
                    FL.Done fb -> return $ d fb
            IError err -> return $ e err

    scrutR fs p c d e = do
        resR <- initialR
        case resR of
            IPartial sr -> return $ p (ManyTillR 0 fs sr)
            IDone _ -> d <$> fextract fs
            IError _ -> scrutL fs p c d e

    initial = do
        res <- finitial
        case res of
            FL.Partial fs -> scrutR fs IPartial IPartial IDone IError
            FL.Done b -> return $ IDone b

    step (ManyTillR cnt fs st) a = do
        r <- stepR st a
        case r of
            Partial n s -> return $ Partial n (ManyTillR 0 fs s)
            Continue n s -> do
                assertM(cnt + 1 - n >= 0)
                return $ Continue n (ManyTillR (cnt + 1 - n) fs s)
            Done n _ -> do
                b <- fextract fs
                return $ Done n b
            Error _ -> do
                resL <- initialL
                case resL of
                    IPartial sl ->
                        return $ Continue (cnt + 1) (ManyTillL fs sl)
                    IDone bl -> do
                        fr <- fstep fs bl
                        let cnt1 = cnt + 1
                        case fr of
                            FL.Partial fs1 ->
                                scrutR
                                    fs1
                                    (Partial cnt1)
                                    (Continue cnt1)
                                    (Done cnt1)
                                    Error
                            FL.Done fb -> return $ Done cnt1 fb
                    IError err -> return $ Error err
    step (ManyTillL fs st) a = do
        r <- stepL st a
        case r of
            Partial n s -> return $ Partial n (ManyTillL fs s)
            Continue n s -> return $ Continue n (ManyTillL fs s)
            Done n b -> do
                fs1 <- fstep fs b
                case fs1 of
                    FL.Partial s ->
                        scrutR s (Partial n) (Continue n) (Done n) Error
                    FL.Done b1 -> return $ Done n b1
            Error err -> return $ Error err

    extract (ManyTillL fs sR) = do
        res <- extractL sR
        case res of
            Done n b -> do
                r <- fstep fs b
                case r of
                    FL.Partial fs1 -> fmap (Done n) $ fextract fs1
                    FL.Done c -> return (Done n c)
            Error err -> return $ Error err
            Continue n s -> return $ Continue n (ManyTillL fs s)
            Partial _ _ -> error "Partial in extract"
    extract (ManyTillR _ fs _) = fmap (Done 0) $ fextract fs

-- | @manyThen f collect recover@ repeats the parser @collect@ on the input and
-- collects the output in the supplied fold. If the the parser @collect@ fails,
-- parser @recover@ is run until it stops and then we start repeating the
-- parser @collect@ again. The parser fails if the recovery parser fails.
--
-- For example, this can be used to find a key frame in a video stream after an
-- error.
--
-- /Unimplemented/
--
{-# INLINE manyThen #-}
manyThen :: -- (Foldable t, Monad m) =>
    Parser a m b -> Parser a m x -> Fold m b c -> Parser a m c
manyThen _parser _recover _f = undefined

-------------------------------------------------------------------------------
-- Repeated Alternatives
-------------------------------------------------------------------------------

-- | Keep trying a parser up to a maximum of @n@ failures.  When the parser
-- fails the input consumed till now is dropped and the new instance is tried
-- on the fresh input.
--
-- /Unimplemented/
--
{-# INLINE retryMaxTotal #-}
retryMaxTotal :: -- (Monad m) =>
    Int -> Parser a m b -> Fold m b c -> Parser a m c
retryMaxTotal _n _p _f  = undefined

-- | Like 'retryMaxTotal' but aborts after @n@ successive failures.
--
-- /Unimplemented/
--
{-# INLINE retryMaxSuccessive #-}
retryMaxSuccessive :: -- (Monad m) =>
    Int -> Parser a m b -> Fold m b c -> Parser a m c
retryMaxSuccessive _n _p _f = undefined

-- | Keep trying a parser until it succeeds.  When the parser fails the input
-- consumed till now is dropped and the new instance is tried on the fresh
-- input.
--
-- /Unimplemented/
--
{-# INLINE retry #-}
retry :: -- (Monad m) =>
    Parser a m b -> Parser a m b
retry _p = undefined
