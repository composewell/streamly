{- $setup
>>> :m
>>> import Control.Applicative ((<|>))
>>> import Data.Bifunctor (second)
>>> import Data.Char (isSpace)
>>> import qualified Data.Foldable as Foldable
>>> import qualified Data.Maybe as Maybe

>>> import Streamly.Data.Fold (Fold)
>>> import Streamly.Data.Parser (Parser)

>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.Parser as Parser
>>> import qualified Streamly.Data.Stream as Stream

For APIs that have not been released yet.

>>> import qualified Streamly.Internal.Data.Fold as Fold
>>> import qualified Streamly.Internal.Data.Parser as Parser
>>> import qualified Streamly.Internal.Data.Stream as Stream (parsePos)
-}
