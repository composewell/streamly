-- |
-- Module      : Streamly.Internal.Data.Unicode.Parser.Extra
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

module Streamly.Internal.Data.Unicode.Parser.Extra
  ( scientific
  )
where

import Streamly.Internal.Data.Parser.ParserD (Step(..), Initial(..), Parser(..))
import Data.Scientific (Scientific)
import Data.Char (ord)

import qualified Data.Scientific as Scientific

type Multiplier = Int
type Number = Int
type DecimalPlaces = Int
type PowerMultiplier = Int
type Power = Int

data ScientificParseState
  = SPInitial
  | SPSign Multiplier
  | SPAfterSign Multiplier Number
  | SPDot Multiplier Number
  | SPAfterDot Multiplier Number DecimalPlaces
  | SPExponent Multiplier Number DecimalPlaces
  | SPExponentWithSign Multiplier Number DecimalPlaces PowerMultiplier
  | SPAfterExponent Multiplier Number DecimalPlaces PowerMultiplier Power

-- | Parses a stream of chars into to Scientific Notation
--
-- >>> import qualified Data.Scientific as Scientific
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Unicode.Parser.Extra as Parser
--
-- >>> formatter = Scientific.formatScientific Scientific.Fixed Nothing
-- >>> scientificParser = Stream.parse (formatter <$> Parser.scientific) . Stream.fromList
--
-- >>> scientificParser "3"
-- Right "3.0"
--
-- >>> scientificParser "3.1"
-- Right "3.1"
--
-- >>> scientificParser "3e4"
-- Right "30000.0"
--
-- >>> scientificParser "3.1e4"
-- Right "31000.0"
--
-- >>> scientificParser "31e+3"
-- Right "31000.0"
--
-- >>> scientificParser "-3.1e4"
-- Right "-31000.0"
--
-- >>> scientificParser "-3.1e-4"
-- Right "-0.00031"
scientific :: (Monad m) => Parser Char m Scientific
scientific = Parser step initial extract
    where

    intToInteger :: Int -> Integer
    intToInteger = fromIntegral

    combineNum buf num = buf * 10 + num

    initial = pure $ IPartial SPInitial

    step SPInitial val =
        case val of
          '+' -> pure $ Continue 0 $ SPSign 1
          '-' -> pure $ Continue 0 $ SPSign (-1)
          _ -> do
              let num = ord val - 48
              if num >= 0 && num <= 9
              then pure $ Partial 0 $ SPAfterSign 1 num
              else pure $ Error "Unable to parse the starting part"
    step (SPSign multiplier) val = do
        let num = ord val - 48
        if num >= 0 && num <= 9
        then pure $ Partial 0 $ SPAfterSign multiplier num
        else pure $ Error "No number found after sign"
    step (SPAfterSign multiplier buf) val = do
        case val of
            '.' -> pure $ Partial 0 $ SPDot multiplier buf
            'e' -> pure $ Partial 0 $ SPExponent multiplier buf 0
            _ -> do
                let num = ord val - 48
                if num >= 0 && num <= 9
                then
                    pure
                        $ Partial 0
                        $ SPAfterSign multiplier (combineNum buf num)
                else
                    pure
                        $ Done 1
                        $ Scientific.scientific
                              (intToInteger (multiplier * buf)) 0
    step (SPDot multiplier buf) val = do
        let num = ord val - 48
        if num >= 0 && num <= 9
        then pure $ Partial 0 $ SPAfterDot multiplier (combineNum buf num) 1
        else pure $ Error "No number found after '.'"
    step (SPAfterDot multiplier buf decimalPlaces) val = do
        case val of
            'e' -> pure $ Continue 0 $ SPExponent multiplier buf decimalPlaces
            _ -> do
                let num = ord val - 48
                if num >= 0 && num <= 9
                then
                    pure
                        $ Partial 0
                        $ SPAfterDot
                              multiplier
                              (combineNum buf num)
                              (decimalPlaces + 1)
                else
                    pure
                        $ Done 1
                        $ Scientific.scientific
                              (intToInteger (multiplier * buf))
                              (-1 * decimalPlaces)
    step (SPExponent multiplier number decimalPlaces) val = do
        case val of
            '+' ->
                pure
                    $ Continue 0
                    $ SPExponentWithSign multiplier number decimalPlaces 1
            '-' ->
                pure
                    $ Continue 0
                    $ SPExponentWithSign multiplier number decimalPlaces (-1)
            _ -> do
                let num = ord val - 48
                if num >= 0 && num <= 9
                then
                    pure
                        $ Partial 0
                        $ SPAfterExponent multiplier number decimalPlaces 1 num
                else pure $ Error "No number found after exponent"
    step (SPExponentWithSign mult number decimalPlaces powerMult) val = do
        let num = ord val - 48
        if num >= 0 && num <= 9
        then
            pure
                $ Partial 0
                $ SPAfterExponent mult number decimalPlaces powerMult num
        else pure $ Error "No number found after exponent and sign"
    step (SPAfterExponent mult number decimalPlaces powerMult buf) val = do
        let num = ord val - 48
        if num >= 0 && num <= 9
        then
            pure
                $ Partial 0
                $ SPAfterExponent
                      mult number decimalPlaces powerMult (combineNum buf num)
        else
            pure
                $ Done 1
                $ Scientific.scientific
                      (intToInteger (mult * number))
                      (powerMult * buf - decimalPlaces)

    extract SPInitial = pure $ Error "Failed with no input"
    extract (SPSign _) = pure $ Error "No number found after sign"
    extract (SPAfterSign mult buf) =
        pure $ Done 0 $ Scientific.scientific (intToInteger (mult * buf)) 0
    extract (SPDot _ _) = pure $ Error "No number found after '.'"
    extract (SPAfterDot mult buf decimalPlaces) = do
        pure
            $ Done 0
            $ Scientific.scientific
                  (intToInteger (mult * buf))
                  (-1 * decimalPlaces)
    extract (SPExponent _ _ _) = pure $ Error "No number found after exponent"
    extract (SPExponentWithSign _ _ _ _) =
        pure $ Error "No number found after exponent and sign"
    extract (SPAfterExponent mult number decimalPlaces powerMult buf) = do
        pure
            $ Done 0
            $ Scientific.scientific
                  (intToInteger (mult * number))
                  (powerMult * buf - decimalPlaces)
