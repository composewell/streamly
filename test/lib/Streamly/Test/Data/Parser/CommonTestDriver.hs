{-# Language NoMonoLocalBinds #-}
-- | Common infrastructure shared by the Parser and ParserK common test
-- suites: the test-case types, the test driver that adapts a single test to
-- the various parser/stream backends, and the shared size constants.
module Streamly.Test.Data.Parser.CommonTestDriver
    ( TestMode(..)
    , ParserTestCase
    , ParserTestCase_Temp
    , runParserTC
    , runParserTC_temp
    , min_value
    , mid_value
    , max_value
    , max_length
    ) where

import Streamly.Internal.Data.MutByteArray (Unbox)
import Streamly.Internal.Data.Parser (ParseErrorPos)

import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.Array.Generic as GA
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as P
import qualified Streamly.Internal.Data.ParserK as PK
import qualified Streamly.Internal.Data.StreamK as K

min_value :: Int
min_value = 0

mid_value :: Int
mid_value = 5000

max_value :: Int
max_value = 10000

max_length :: Int
max_length = 1000

-- TODO: Replace ParserTestCase_Temp with ParserTestCase in all the test cases.

type ParserTestCase a m b c =
        (P.Parser a m b -> [a] -> m (Either ParseErrorPos b, [a])) -> c

type ParserTestCase_Temp a m b c =
        forall t. ([a] -> t)
        -> (P.Parser a m b -> t -> m (Either ParseErrorPos b))
        -> c

-------------------------------------------------------------------------------
-- Test driver
-------------------------------------------------------------------------------

data TestMode
    = TMParserStream
    | TMParserKStreamK
    | TMParserKStreamKChunks
    | TMParserKStreamKChunksGeneric
    deriving (Show)

runParserTC :: (Unbox a, Monad m) => TestMode -> ParserTestCase a m b c -> c
runParserTC tm runner =
    case tm of
        TMParserStream ->
            runner $ \p -> mapMTup S.toList . S.parseBreakPos p . S.fromList
        TMParserKStreamK ->
            runner $ \p ->
                mapMTup K.toList . K.parseBreakPos (PK.toParserK p) . K.fromList
        TMParserKStreamKChunks ->
            runner $ \p ->
                mapMTup
                    (fmap (concatMap A.toList) . K.toList)
                        . A.parseBreakPos (A.toParserK p)
                        . producerChunks A.fromList
        TMParserKStreamKChunksGeneric ->
            runner $ \p ->
                mapMTup
                    (fmap (concatMap GA.toList) . K.toList)
                        . GA.parseBreakPos (GA.toParserK p)
                        . producerChunks GA.fromList

    where
    mapMTup f tupM = do
        (t, a) <- tupM
        (t,) <$> f a

    cSize = 50
    -- Not using A.createOf here because of the MonadIO constraint
    producerChunks fl =
        K.fromStream
             . S.groupsOf cSize (fl <$> FL.toList)
             . S.fromList

runParserTC_temp :: (Unbox a, Monad m) => TestMode -> ParserTestCase_Temp a m b c -> c
runParserTC_temp tm runner =
    case tm of
        TMParserStream -> runner S.fromList S.parsePos
        TMParserKStreamK -> runner K.fromList (K.parsePos . PK.toParserK)
        TMParserKStreamKChunks ->
            runner (producerChunks A.fromList) (A.parsePos . A.toParserK)
        TMParserKStreamKChunksGeneric ->
            runner
                (producerChunks GA.fromList)
                (GA.parsePos . GA.toParserK)

    where
    cSize = 50
    -- Not using A.createOf here because of the MonadIO constraint
    producerChunks fl =
        K.fromStream
             . S.groupsOf cSize (fl <$> FL.toList)
             . S.fromList
