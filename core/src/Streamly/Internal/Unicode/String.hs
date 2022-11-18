{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Streamly.Internal.Unicode.String
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Convenient template Haskell quasiquoters to format strings.

-- Design Notes:
--
-- Essential requirements are:
--
-- Haskell expression expansion
-- Newline treatment (continue without introducing a newline)
-- Indentation treatment
--
-- We choose #{expr} for patching a Haskell expression's value in a string. "$"
-- instead of "#" was another option (like in neat-interpolation package) but
-- we did not use that to avoid conflict with strings that are used as shell
-- commands. Another option was to use just "{}" (like in PyF package) but we
-- did not use that to avoid conflict with "${}" used in shell.
--
-- We use a "#" at the end of line to continue the line. We could use a "\"
-- as well but that may interfere with CPP.
--
-- Stripping is not part of the quasiquoter as it can be done by a Haskell
-- function. Other type of formatting on the Haskell expression can be done
-- using Haskell functions.

module Streamly.Internal.Unicode.String
    ( str
    ) where


import Control.Applicative (Alternative(..))
import Control.Exception (displayException)
import Data.Functor.Identity (runIdentity)
import Streamly.Internal.Data.Parser (Parser)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
    (some, many, takeWhile1)
import qualified Streamly.Data.Stream as Stream  (fromList, parse)
import qualified Streamly.Internal.Unicode.Char.Parser as Parser

-- $setup
-- >>> :m
-- >>> :set -XQuasiQuotes
-- >>> import Streamly.Internal.Unicode.String
--
--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

data StrSegment
    = StrText String
    | StrVar String
    deriving (Show, Eq)

haskellIdentifier :: Monad m => Parser Char m String
haskellIdentifier =
    let p = Parser.alphaNum <|> Parser.char '\'' <|> Parser.char '_'
     in Parser.some p Fold.toList

strParser :: Monad m => Parser Char m [StrSegment]
strParser = Parser.many content Fold.toList

    where

    plainText = StrText <$> Parser.takeWhile1 (/= '#') Fold.toList
    escHash = StrText . (: []) <$> (Parser.char '#' *> Parser.char '#')
    lineCont = StrText [] <$ (Parser.char '#' *> Parser.char '\n')
    var = StrVar <$>
            (  Parser.char '#'
            *> Parser.char '{'
            *> haskellIdentifier
            <* Parser.char '}'
            )
    plainHash = StrText . (: []) <$> Parser.char '#'

    -- order is important
    content = plainText <|> escHash <|> lineCont <|> var <|> plainHash

strSegmentExp :: StrSegment -> Q Exp
strSegmentExp (StrText text) = stringE text
strSegmentExp (StrVar name) = do
    valueName <- lookupValueName name
    case valueName of
        Just vn -> varE vn
        Nothing ->
            fail
                $ "str quote: Haskell symbol `" ++ name
                ++ "` is not in scope"

strExp :: [StrSegment] -> Q Exp
strExp xs = appE [| concat |] $ listE $ map strSegmentExp xs

expandVars :: String -> Q Exp
expandVars ln =
    case runIdentity $ Stream.parse strParser (Stream.fromList ln) of
        Left e ->
            fail $ "str QuasiQuoter parse error: " ++ displayException e
        Right x ->
            strExp x

-- | A QuasiQuoter that treats the input as a string literal:
--
-- >>> [str|x|]
-- "x"
--
-- Any @#{symbol}@ is replaced by the value of the Haskell symbol @symbol@
-- which is in scope:
--
-- >>> x = "hello"
-- >>> [str|#{x} world!|]
-- "hello world!"
--
-- @##@ means a literal @#@ without the special meaning for referencing
-- haskell symbols:
--
-- >>> [str|##{x} world!|]
-- "#{x} world!"
--
-- A @#@ at the end of line means the line continues to the next line without
-- introducing a newline character:
--
-- >>> :{
-- [str|hello#
-- world!|]
-- :}
-- "hello world!"
--
-- Bugs: because of a bug in parsers, a lone # at the end of input gets
-- removed.
--
str :: QuasiQuoter
str =
    QuasiQuoter
        { quoteExp = expandVars
        , quotePat = notSupported
        , quoteType = notSupported
        , quoteDec = notSupported
        }

    where

    notSupported = error "str: Not supported."
