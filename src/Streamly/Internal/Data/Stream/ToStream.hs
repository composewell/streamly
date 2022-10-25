-- |
-- Module      : Streamly.Internal.Data.Stream.ToStream
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- These type classes allow use to use the same APIs to perform UTF-8 encoding
-- decoding using three common container types namely, Streams, Lists and
-- Arrays.
--
-- These type classes are not exact inverse of each other. The reason is as
-- follows.  When a monadic stream is converted to a list we get a @m []@
-- instead of a @[]@ type. On the other hand when we convert a list to a stream
-- we can work with a pure type. There we have two separate type classes
-- 'ToChars' and 'FromChars' so that we can allow this difference in usage. We
-- can unify these if we accept @m []@ type in 'FromChars' APIs but that would
-- be a tad incovnenient.

module Streamly.Internal.Data.Stream.ToStream
    ( {- ToStream (..)
    , FromStream (..)
    , IsFold(..)
    , IsUnfold(..)
    -}
    )
where

{-
import Control.Monad.IO.Class (MonadIO)
import Streamly.Data.Array.Foreign (Array)
import Streamly.Prelude (SerialT) -- , IsStream) -- , adapt)
import Foreign.Storable (Storable)
import Streamly.Internal.Data.Unfold (Unfold)
import Streamly.Internal.Data.Fold (Fold)
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Prelude as Stream

-- XXX Instead of ToChars use ToString and FromString to convert a type to
-- SerialT m Char.

-------------------------------------------------------------------------------
-- ToStream
-------------------------------------------------------------------------------

-- If we have a ToStream type class then we can define a generic instance for
-- 't Char' given that 't Char' is an instance of ToStream.

-- unfold
-- XXX Should we rather use Unfolds here instead of streams?
class ToStream m a b where
    toStream :: (Monad m, Storable b) => a -> SerialT m b

instance ToStream m (SerialT m a) a where
    toStream = id

instance ToStream m [a] a where
    toStream = Stream.fromList

instance ToStream m (Array a) a where
    toStream = Array.toStream

class IsUnfold m a b where
    generator :: (Monad m, Storable b) => Unfold m a b

instance IsUnfold m (SerialT m a) a where
    -- XXX Fold.toStream uses two different monads for the fold and the stream,
    -- so we could detach the stream monad from the folds monad and that way we
    -- can remove the monad parameter from the IsFold type class. But that is
    -- not the case for unfolds. We cannot detach the two monads in
    -- Unfold.fromStream. Is it possible? We can use "SerialT Identity a"
    -- though.
    generator = Unfold.fromStream

instance IsUnfold m [a] a where
    generator = Unfold.fromList

instance IsUnfold m (Array a) a where
    generator = Array.read

-- fold
-- XXX should we rather use Folds here instead of streams?
class FromStream m a b where
    -- The MonadIO constraint is for Array.fromStream
    fromStream :: (MonadIO m, Storable a) => SerialT m a -> m b

-- XXX It is possible to just return the stream if we parameterize the type
-- class with Monad m. However, if we remove the monad parameter we could fold
-- the stream to a stream in any monad, but that will be reconstructing the
-- stream, perf?
instance FromStream m a (SerialT m a) where
    fromStream = return -- Stream.fold Fold.toStream

instance FromStream m a [a] where
    fromStream = Stream.toList

instance FromStream m a (Array a) where
    fromStream = Array.fromStream

class IsFold m a b where
    -- The MonadIO constraint is for Array.fromStream
    eliminator :: (MonadIO m, Storable a) => Fold m a b

instance IsFold m a (SerialT m a) where
    eliminator = Fold.toStream

instance IsFold m a [a] where
    eliminator = Fold.toList

instance IsFold m a (Array a) where
    eliminator = Array.write

{-
-------------------------------------------------------------------------------
-- ToChars
-------------------------------------------------------------------------------

-- | Types (@t@) that can be encoded to a UTF-8 byte stream under 'Monad' @m@.
class ToChars m a where
    toChars :: Monad m => a -> SerialT m Char

instance ToChars m (SerialT m Char) where
    toChars = id

instance ToChars m [Char] where
    toChars = Stream.fromList

instance ToChars m (Array Char) where
    toChars = Array.toStream

-- XXX DList and Mutable arrays can also be added for builder use case.

-------------------------------------------------------------------------------
-- FromChars
-------------------------------------------------------------------------------

-- | Types (@t@) that can be created by decoding a UTF-8 byte stream under
-- 'Monad' @m@.
class FromChars m a where
    -- The MonadIO constraint is for Array.fromStream
    fromChars :: MonadIO m => SerialT m Char -> a

instance FromChars m (SerialT m Char) where
    fromChars = id

instance FromChars m (m [Char]) where
    fromChars = Stream.toList

instance FromChars m (m (Array Char)) where
    fromChars = Array.fromStream
    -}
    -}
