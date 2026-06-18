import Data.Function ((&))
import Control.Category ((>>>))
import Streamly.Data.Stream (Stream)
import Streamly.Data.Fold (Fold)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold

-- Reading direction and data flow direction for Stream and Fold types:
--
--            (&) (>>>)    ($) (.)
--   Stream    straight    inverted
--   Fold      inverted    straight

-------------------------------------------------------------------------------
-- Stream data flow is straight when composed with (&) and (>>>)
-------------------------------------------------------------------------------

f1 :: Monad m => Stream m Int -> Stream m Int
f1 s =
      Stream.filter odd s -- data in
    & fmap (+1)
    & fmap (\x -> x * x) -- data out

f2 :: Monad m => Stream m Int -> Stream m Int
f2 =
        Stream.filter odd -- data in
    >>> fmap (+1)
    >>> fmap (\x -> x * x) -- data out

-------------------------------------------------------------------------------
-- Stream data flow is inverted when composed with ($) and (.)
-------------------------------------------------------------------------------

f3 :: Monad m => Stream m Int -> Stream m Int
f3 s =
      fmap (\x -> x * x) -- data out
    $ fmap (+1)
    $ Stream.filter odd s --  data in

f4 :: Monad m => Stream m Int -> Stream m Int
f4 =
      fmap (\x -> x * x) -- data out
    . fmap (+1)
    . Stream.filter odd -- data in

-------------------------------------------------------------------------------
-- Fold data flow is inverted when composed with (&) and (>>>)
-------------------------------------------------------------------------------

g1 :: Monad m => Fold m Int Int -> Fold m Int Int
g1 f =
      Fold.lmap (\x -> x * x) f -- data out
    & Fold.lmap (+1)
    & Fold.filter odd -- data in

g2 :: Monad m => Fold m Int Int -> Fold m Int Int
g2 =
        Fold.lmap (\x -> x * x) -- data out
    >>> Fold.lmap (+1)
    >>> Fold.filter odd -- data in

-------------------------------------------------------------------------------
-- Fold data flow is straight when composed with ($) and (.)
-------------------------------------------------------------------------------

g3 :: Monad m => Fold m Int Int -> Fold m Int Int
g3 f =
      Fold.filter odd -- data in
    $ Fold.lmap (+1)
    $ Fold.lmap (\x -> x * x) f -- initial fold

g4 :: Monad m => Fold m Int Int -> Fold m Int Int
g4 =
      Fold.filter odd -- data in
    . Fold.lmap (+1)
    . Fold.lmap (\x -> x * x) -- data out

main = putStrLn "hello"
