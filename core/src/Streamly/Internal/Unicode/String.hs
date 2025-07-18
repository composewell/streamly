{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE CPP #-}
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
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

      str

    -- * Internals
    , strWith
    ) where


import Control.Applicative (Alternative(..))
import Control.Exception (displayException)
import Data.Functor.Identity (runIdentity)
import Streamly.Internal.Data.Parser (Parser, ParseError)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
    (some, many, takeWhile1)
import qualified Streamly.Data.Stream as Stream  (fromList, parse)
import qualified Streamly.Internal.Unicode.Parser as Parser

#include "DocTestUnicodeString.hs"

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

strSegmentExp ::
    (Q Exp -> Q Exp) -> (Q Exp -> Q Exp) -> StrSegment -> Q Exp
strSegmentExp f _ (StrText text) = f (stringE text)
strSegmentExp _ f (StrVar name) = do
    valueName <- lookupValueName name
    case valueName of
        Just vn -> f (varE vn)
        Nothing ->
            fail
                $ "str quote: Haskell symbol `" ++ name
                ++ "` is not in scope"

strExp :: Q Exp -> (Q Exp -> Q Exp) -> (Q Exp -> Q Exp) -> [StrSegment] -> Q Exp
strExp c f g xs = appE c $ listE $ map (strSegmentExp f g) xs

parseStr :: String -> Either ParseError [StrSegment]
parseStr = runIdentity . Stream.parse strParser . Stream.fromList

expandVars :: Q Exp -> (Q Exp -> Q Exp) -> (Q Exp -> Q Exp) -> String -> Q Exp
expandVars c f g input =
    case parseStr input of
        Left e -> fail $ "str QuasiQuoter parse error: " ++ displayException e
        Right x -> strExp c f g x

strWith :: Q Exp -> (Q Exp -> Q Exp) -> (Q Exp -> Q Exp) -> QuasiQuoter
strWith c f g =
    QuasiQuoter
        { quoteExp = expandVars c f g
        , quotePat = notSupported
        , quoteType = notSupported
        , quoteDec = notSupported
        }

    where

    notSupported = error "str: Not supported."

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
str :: QuasiQuoter
str = strWith  [|concat|] id id
