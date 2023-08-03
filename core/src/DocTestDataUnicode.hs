{- $setup
>>> :m
>>> :set -XQuasiQuotes
>>> :set -XMagicHash
>>> :set -XOverloadedStrings

>>> import Streamly.Internal.Unicode.Stream
>>> import Streamly.Internal.Unicode.String

>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.Stream as Stream
>>> import qualified Streamly.Internal.Unicode.Array as Array
>>> import qualified Streamly.Internal.Unicode.Stream as Unicode
>>> import qualified Streamly.Unicode.Parser as Unicode
>>> import qualified Streamly.Internal.Unicode.Parser as Unicode (number, mkDouble)

>>> import Prelude hiding (String, lines, words, unlines, unwords)
-}
