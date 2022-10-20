{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.Zip
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To run examples in this module:
--
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.Zip as Stream
--
module Streamly.Internal.Data.Stream.Zip
    (
      ZipStream (..)
    , ZipSerialM
    , ZipSerial
    )
where

import Control.DeepSeq (NFData(..), NFData1(..))
import Data.Functor.Identity (Identity(..))
import GHC.Exts (IsList(..), IsString(..))
import Streamly.Internal.Data.Stream.Type (Stream)
import Text.Read
       ( Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec
       , readListPrecDefault)

import qualified Streamly.Internal.Data.Stream as Stream

-- $setup
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.Zip as Stream

------------------------------------------------------------------------------
-- Serially Zipping Streams
------------------------------------------------------------------------------

-- | For 'ZipStream':
--
-- @
-- (<>) = 'Streamly.Data.Stream.append'
-- (<*>) = 'Streamly.Data.Stream.zipWith' id
-- @
--
-- Applicative evaluates the streams being zipped serially:
--
-- >>> s1 = Stream.ZipStream $ Stream.fromFoldable [1, 2]
-- >>> s2 = Stream.ZipStream $ Stream.fromFoldable [3, 4]
-- >>> s3 = Stream.ZipStream $ Stream.fromFoldable [5, 6]
-- >>> s = (,,) <$> s1 <*> s2 <*> s3
-- >>> Stream.fold Fold.toList (Stream.getZipStream s)
-- [(1,3,5),(2,4,6)]
--
newtype ZipStream m a = ZipStream {getZipStream :: Stream m a}
        deriving (Functor, Semigroup, Monoid)

deriving instance NFData a => NFData (ZipStream Identity a)
deriving instance NFData1 (ZipStream Identity)
deriving instance IsList (ZipStream Identity a)
deriving instance (a ~ Char) => IsString (ZipStream Identity a)
deriving instance Eq a => Eq (ZipStream Identity a)
deriving instance Ord a => Ord (ZipStream Identity a)
deriving instance (Foldable m, Monad m) => Foldable (ZipStream m)
deriving instance Traversable (ZipStream Identity)

instance Show a => Show (ZipStream Identity a) where
    showsPrec p dl = showParen (p > 10) $
        showString "fromList " . shows (toList dl)

instance Read a => Read (ZipStream Identity a) where
    readPrec = parens $ prec 10 $ do
        Ident "fromList" <- lexP
        fromList <$> readPrec
    readListPrec = readListPrecDefault

type ZipSerialM = ZipStream

-- | An IO stream whose applicative instance zips streams serially.
--
type ZipSerial = ZipSerialM IO

instance Monad m => Applicative (ZipStream m) where
    pure = ZipStream . Stream.repeat

    {-# INLINE (<*>) #-}
    ZipStream m1 <*> ZipStream m2 = ZipStream $ Stream.zipWith id m1 m2
