{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, try)
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad.Error.Class (throwError, MonadError)
import Control.Monad.Trans.Except (runExceptT, ExceptT)
import Data.Foldable (forM_)
import Data.List (sort)
import Test.Hspec

import Streamly
import Streamly.Prelude ((.:), nil)
import qualified Streamly.Prelude as A

singleton :: IsStream t => a -> t m a
singleton a = a .: nil

toListSerial :: StreamT IO a -> IO [a]
toListSerial = A.toList . streamly

toListInterleaved :: CostreamT IO a -> IO [a]
toListInterleaved = A.toList . costreamly

toListAsync :: CoparallelT IO a -> IO [a]
toListAsync = A.toList . coparallely

toListParallel :: ParallelT IO a -> IO [a]
toListParallel = A.toList . parallely

main :: IO ()
main = hspec $ do
    describe "Runners" $ do
        -- XXX move these to property tests
        -- XXX use an IORef to store and check the side effects
        it "simple streamly" $
            (runStream . streamly) (return (0 :: Int)) `shouldReturn` ()
        it "simple streamly with IO" $
            (runStream . streamly) (liftIO $ putStrLn "hello") `shouldReturn` ()

    describe "Empty" $ do
        it "Monoid - mempty" $
            (toListSerial mempty) `shouldReturn` ([] :: [Int])
        -- it "Alternative - empty" $
        --     (toListSerial empty) `shouldReturn` ([] :: [Int])
        -- it "MonadPlus - mzero" $
        --     (toListSerial mzero) `shouldReturn` ([] :: [Int])

    ---------------------------------------------------------------------------
    -- Functor
    ---------------------------------------------------------------------------

    describe "Functor (fmap)" $ do
        -- XXX we should do these through property tests by using a
        -- construction via list fold construction method.
        it "fmap on composed (<>)" $
            (toListSerial $ fmap (+1) (return 1 <> return 2))
                `shouldReturn` ([2,3] :: [Int])

        it "fmap on composed (<>)" $
            ((toListParallel $ fmap (+1) (return 1 <> return 2)) >>= return .  sort)
                `shouldReturn` ([2,3] :: [Int])

    ---------------------------------------------------------------------------
    -- Applicative
    ---------------------------------------------------------------------------

    describe "Applicative" $ do
        -- XXX we should do these through property tests by using a
        -- construction via list fold construction method.
        it "Apply - serial composed first argument" $
            (toListSerial $ (,) <$> (return 1 <> return 2) <*> (return 3))
                `shouldReturn` ([(1,3),(2,3)] :: [(Int, Int)])

        it "Apply - serial composed second argument" $
            (toListSerial $ (,) <$> (return 1) <*> (return 2 <> return 3))
                `shouldReturn` ([(1,2),(1,3)] :: [(Int, Int)])

        it "Apply - parallel composed first argument" $
            (toListParallel ((,) <$> (return 1 <> return 2) <*> (return 3)) >>= return . sort)
                `shouldReturn` ([(1,3),(2,3)] :: [(Int, Int)])

        it "Apply - parallel composed second argument" $
            (toListParallel ((,) <$> (return 1) <*> (return 2 <> return 3)) >>= return . sort)
                `shouldReturn` ([(1,2),(1,3)] :: [(Int, Int)])

    ---------------------------------------------------------------------------
    -- Semigroup/Monoidal Composition strict ordering checks
    ---------------------------------------------------------------------------

    -- test both (<>) and mappend to make sure we are using correct instance
    -- for Monoid that is using the right version of semigroup. Instance
    -- deriving can cause us to pick wrong instances sometimes.

    describe "Serial interleaved (<>) ordering check" $ interleaveCheck costreamly (<>)
    describe "Serial interleaved mappend ordering check" $ interleaveCheck costreamly mappend

    describe "Parallel interleaved (<>) ordering check" $ interleaveCheck parallely (<>)
    describe "Parallel interleaved mappend ordering check" $ interleaveCheck parallely mappend

    describe "Coparallel (<>) time order check" $ parallelCheck coparallely (<>)
    describe "Coparallel mappend time order check" $ parallelCheck coparallely mappend
    describe "Parallel (<>) time order check" $ parallelCheck parallely (<>)
    describe "Parallel mappend time order check" $ parallelCheck parallely mappend

    ---------------------------------------------------------------------------
    -- Monoidal Compositions, multiset equality checks
    ---------------------------------------------------------------------------

    describe "Serial Composition" $ compose streamly mempty id
    describe "Interleaved Composition" $ compose costreamly mempty sort
    describe "Left biased parallel Composition" $ compose coparallely mempty sort
    describe "Fair parallel Composition" $ compose parallely mempty sort
    describe "Semigroup Composition for ZipSerial" $ compose zipStreamly mempty id
    describe "Semigroup Composition for ZipAsync" $ compose zipParallely mempty id
    -- XXX need to check alternative compositions as well

    ---------------------------------------------------------------------------
    -- TBD Monoidal composition combinations
    ---------------------------------------------------------------------------

    -- TBD need more such combinations to be tested.
    describe "<> and <>" $ composeAndComposeSimple streamly streamly (cycle [[1 .. 9]])

    describe "<> and <=>" $ composeAndComposeSimple
      streamly
      costreamly
      ([ [1 .. 9]
       , [1 .. 9]
       , [1, 3, 2, 4, 6, 5, 7, 9, 8]
       , [1, 3, 2, 4, 6, 5, 7, 9, 8]
       ])

    describe "<=> and <=>" $ composeAndComposeSimple
      costreamly
      costreamly
      ([ [1, 4, 2, 7, 3, 5, 8, 6, 9]
       , [1, 7, 4, 8, 2, 9, 5, 3, 6]
       , [1, 4, 3, 7, 2, 6, 9, 5, 8]
       , [1, 7, 4, 9, 3, 8, 6, 2, 5]
       ])

    describe "<=> and <>" $ composeAndComposeSimple
      costreamly
      streamly
      ([ [1, 4, 2, 7, 3, 5, 8, 6, 9]
       , [1, 7, 4, 8, 2, 9, 5, 3, 6]
       , [1, 4, 2, 7, 3, 5, 8, 6, 9]
       , [1, 7, 4, 8, 2, 9, 5, 3, 6]
       ])

    describe "Nested parallel and serial compositions" $ do
        let t = timed
            p = parallely
            s = streamly
        {-
        -- This is not correct, the result can also be [4,4,8,0,8,0,2,2]
        -- because of parallelism of [8,0] and [8,0].
        it "Nest <|>, <>, <|> (1)" $
            let t = timed
             in toListSerial (
                    ((t 8 <|> t 4) <> (t 2 <|> t 0))
                <|> ((t 8 <|> t 4) <> (t 2 <|> t 0)))
            `shouldReturn` ([4,4,8,8,0,0,2,2])
        -}
        it "Nest <|>, <>, <|> (2)" $
            (A.toList . parallely) (
                   s (p (t 4 <> t 8) <> p (t 1 <> t 2))
                <> s (p (t 4 <> t 8) <> p (t 1 <> t 2)))
            `shouldReturn` ([4,4,8,8,1,1,2,2])
        -- FIXME: These two keep failing intermittently on Mac OS X
        -- Need to examine and fix the tests.
        {-
        it "Nest <|>, <=>, <|> (1)" $
            let t = timed
             in toListSerial (
                    ((t 8 <|> t 4) <=> (t 2 <|> t 0))
                <|> ((t 9 <|> t 4) <=> (t 2 <|> t 0)))
            `shouldReturn` ([4,4,0,0,8,2,9,2])
        it "Nest <|>, <=>, <|> (2)" $
            let t = timed
             in toListSerial (
                    ((t 4 <|> t 8) <=> (t 1 <|> t 2))
                <|> ((t 4 <|> t 9) <=> (t 1 <|> t 2)))
            `shouldReturn` ([4,4,1,1,8,2,9,2])
        -}
        it "Nest <|>, <|>, <|>" $
            (A.toList . parallely) (
                    ((t 4 <> t 8) <> (t 0 <> t 2))
                <> ((t 4 <> t 8) <> (t 0 <> t 2)))
            `shouldReturn` ([0,0,2,2,4,4,8,8])

    ---------------------------------------------------------------------------
    -- Monoidal composition recursion loops
    ---------------------------------------------------------------------------

    describe "Serial loops (<>)" $ loops streamly id reverse
    describe "Left biased parallel loops (<|)" $ loops coparallely sort sort
    describe "Fair parallel loops (<|>)" $ loops parallely sort sort

    ---------------------------------------------------------------------------
    -- Bind and monoidal composition combinations
    ---------------------------------------------------------------------------

    describe "Bind and compose1" $ bindAndComposeSimple streamly streamly
    describe "Bind and compose2" $ bindAndComposeSimple streamly costreamly
    describe "Bind and compose3" $ bindAndComposeSimple streamly coparallely
    describe "Bind and compose4" $ bindAndComposeSimple streamly parallely

    describe "Bind and compose1" $ bindAndComposeSimple costreamly streamly
    describe "Bind and compose2" $ bindAndComposeSimple costreamly costreamly
    describe "Bind and compose3" $ bindAndComposeSimple costreamly coparallely
    describe "Bind and compose4" $ bindAndComposeSimple costreamly parallely

    describe "Bind and compose1" $ bindAndComposeSimple coparallely streamly
    describe "Bind and compose2" $ bindAndComposeSimple coparallely costreamly
    describe "Bind and compose3" $ bindAndComposeSimple coparallely coparallely
    describe "Bind and compose4" $ bindAndComposeSimple coparallely parallely

    describe "Bind and compose1" $ bindAndComposeSimple parallely streamly
    describe "Bind and compose2" $ bindAndComposeSimple parallely costreamly
    describe "Bind and compose3" $ bindAndComposeSimple parallely coparallely
    describe "Bind and compose4" $ bindAndComposeSimple parallely parallely

    let fldr, fldl :: (IsStream t, Semigroup (t IO Int)) => [t IO Int] -> t IO Int
        fldr = foldr (<>) nil
        fldl = foldl (<>) nil

    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy streamly streamly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy streamly costreamly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy streamly coparallely k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy streamly parallely k

    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy costreamly streamly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy costreamly costreamly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy costreamly coparallely k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy costreamly parallely k

    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy coparallely streamly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy coparallely costreamly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy coparallely coparallely k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy coparallely parallely k

    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy parallely streamly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy parallely costreamly k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy parallely coparallely k
    forM_ [fldr, fldl] $ \k ->
        describe "Bind and compose" $ bindAndComposeHierarchy parallely parallely k

    -- Nest two lists using different styles of product compositions
    it "Nests two streams using monadic serial composition" nestTwoSerial
    it "Nests two streams using monadic interleaved composition" nestTwoInterleaved
    it "Nests two streams using monadic async composition" nestTwoAsync
    it "Nests two streams using monadic parallel composition" nestTwoParallel

    it "Nests two streams using applicative serial composition" nestTwoSerialApp
    it "Nests two streams using applicative interleaved composition" nestTwoInterleavedApp
    it "Nests two streams using applicative async composition" nestTwoAsyncApp
    it "Nests two streams using applicative parallel composition" nestTwoParallelApp

    ---------------------------------------------------------------------------
    -- TBD Bind and Bind combinations
    ---------------------------------------------------------------------------

    -- TBD combine all binds and all compose in one example
    describe "Miscellaneous combined examples" mixedOps
    describe "Simple MonadError and MonadThrow" simpleMonadError

    {-
    describe "Composed MonadError streamly" $ composeWithMonadError streamly
    describe "Composed MonadError costreamly" $ composeWithMonadError costreamly
    describe "Composed MonadError coparallely" $ composeWithMonadError coparallely
    describe "Composed MonadError parallely" $ composeWithMonadError parallely
    -}

    describe "Composed MonadThrow streamly" $ composeWithMonadThrow streamly
    describe "Composed MonadThrow costreamly" $ composeWithMonadThrow costreamly
    describe "Composed MonadThrow coparallely" $ composeWithMonadThrow coparallely
    describe "Composed MonadThrow parallely" $ composeWithMonadThrow parallely

-- XXX need to test that we have promptly cleaned up everything after the error
-- XXX We can also check the output that we are expected to get before the
-- error occurs.

data ExampleException = ExampleException String deriving (Eq, Show)

instance Exception ExampleException

simpleMonadError :: Spec
simpleMonadError = do
{-
    it "simple runExceptT" $ do
        (runExceptT $ runStream $ return ())
        `shouldReturn` (Right () :: Either String ())
    it "simple runExceptT with error" $ do
        (runExceptT $ runStream $ throwError "E") `shouldReturn` Left "E"
        -}
    it "simple try" $ do
        (try $ runStream $ return ())
        `shouldReturn` (Right () :: Either ExampleException ())
    it "simple try with throw error" $ do
        (try $ runStream $ throwM $ ExampleException "E")
        `shouldReturn` (Left (ExampleException "E") :: Either ExampleException ())

composeWithMonadThrow
    :: ( IsStream t
       , Semigroup (t IO Int)
       , MonadThrow (t IO)
       )
    => (t IO Int -> StreamT IO Int) -> Spec
composeWithMonadThrow t = do
    it "Compose throwM, nil" $
        (try $ tl (throwM (ExampleException "E") <> A.nil))
        `shouldReturn` (Left (ExampleException "E") :: Either ExampleException [Int])
    it "Compose nil, throwM" $
        (try $ tl (A.nil <> throwM (ExampleException "E")))
        `shouldReturn` (Left (ExampleException "E") :: Either ExampleException [Int])
    oneLevelNestedSum "streamly" streamly
    oneLevelNestedSum "costreamly" costreamly
    oneLevelNestedSum "coparallely" coparallely
    oneLevelNestedSum "parallely" parallely
    -- XXX add two level nesting

    oneLevelNestedProduct "streamly"   streamly
    oneLevelNestedProduct "costreamly" costreamly
    oneLevelNestedProduct "coparallely" coparallely
    oneLevelNestedProduct "parallely"  parallely

    where
    tl = A.toList . t
    oneLevelNestedSum desc t1 =
        it ("One level nested sum " ++ desc) $ do
            let nested = (A.fromFoldable [1..10] <> throwM (ExampleException "E")
                         <> A.fromFoldable [1..10])
            (try $ tl (A.nil <> t1 nested <> A.fromFoldable [1..10]))
            `shouldReturn` (Left (ExampleException "E") :: Either ExampleException [Int])

    oneLevelNestedProduct desc t1 =
        it ("One level nested product" ++ desc) $ do
            let s1 = t $ foldMapWith (<>) return [1..4]
                s2 = t1 $ foldMapWith (<>) return [5..8]
            try $ tl (do
                x <- adapt s1
                y <- s2
                if (x + y > 10)
                then throwM (ExampleException "E")
                else return (x + y)
                )
            `shouldReturn` (Left (ExampleException "E") :: Either ExampleException [Int])

_composeWithMonadError
    :: ( IsStream t
       , Semigroup (t (ExceptT String IO) Int)
       , MonadError String (t (ExceptT String IO))
       )
    => (t (ExceptT String IO) Int -> StreamT (ExceptT String IO) Int) -> Spec
_composeWithMonadError t = do
    let tl = A.toList . t
    it "Compose throwError, nil" $
        (runExceptT $ tl (throwError "E" <> A.nil)) `shouldReturn` Left "E"
    it "Compose nil, error" $
        (runExceptT $ tl (A.nil <> throwError "E")) `shouldReturn` Left "E"

nestTwoSerial :: Expectation
nestTwoSerial =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in toListSerial (do
        x <- s1
        y <- s2
        return (x + y)
        ) `shouldReturn` ([6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12] :: [Int])

nestTwoSerialApp :: Expectation
nestTwoSerialApp =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in toListSerial ((+) <$> s1 <*> s2)
        `shouldReturn` ([6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12] :: [Int])

nestTwoInterleaved :: Expectation
nestTwoInterleaved =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in toListInterleaved (do
        x <- s1
        y <- s2
        return (x + y)
        ) `shouldReturn` ([6,7,7,8,8,8,9,9,9,9,10,10,10,11,11,12] :: [Int])

nestTwoInterleavedApp :: Expectation
nestTwoInterleavedApp =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in toListInterleaved ((+) <$> s1 <*> s2)
        `shouldReturn` ([6,7,7,8,8,8,9,9,9,9,10,10,10,11,11,12] :: [Int])

nestTwoAsync :: Expectation
nestTwoAsync =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in (toListAsync (do
        x <- s1
        y <- s2
        return (x + y)
        ) >>= return . sort)
    `shouldReturn` sort ([6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12] :: [Int])

nestTwoAsyncApp :: Expectation
nestTwoAsyncApp =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in (toListAsync ((+) <$> s1 <*> s2) >>= return . sort)
        `shouldReturn` sort ([6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12] :: [Int])

nestTwoParallel :: Expectation
nestTwoParallel =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in (toListParallel (do
        x <- s1
        y <- s2
        return (x + y)
        ) >>= return . sort)
    `shouldReturn` sort ([6,7,7,8,8,8,9,9,9,9,10,10,10,11,11,12] :: [Int])

nestTwoParallelApp :: Expectation
nestTwoParallelApp =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in (toListParallel ((+) <$> s1 <*> s2) >>= return . sort)
        `shouldReturn` sort ([6,7,7,8,8,8,9,9,9,9,10,10,10,11,11,12] :: [Int])

timed :: MonadIO (t IO) => Int -> t IO Int
timed x = liftIO (threadDelay (x * 100000)) >> return x

interleaveCheck :: IsStream t
    => (t IO Int -> StreamT IO Int)
    -> (t IO Int -> t IO Int -> t IO Int)
    -> Spec
interleaveCheck t f =
    it "Interleave four" $
        (A.toList . t) ((singleton 0 `f` singleton 1) `f` (singleton 100 `f` singleton 101))
            `shouldReturn` ([0, 100, 1, 101])

parallelCheck :: MonadIO (t IO)
    => (t IO Int -> StreamT IO Int)
    -> (t IO Int -> t IO Int -> t IO Int)
    -> Spec
parallelCheck t f = do
    it "Parallel ordering left associated" $
        (A.toList . t) (((event 4 `f` event 3) `f` event 2) `f` event 1)
            `shouldReturn` ([1..4])

    it "Parallel ordering right associated" $
        (A.toList . t) (event 4 `f` (event 3 `f` (event 2 `f` event 1)))
            `shouldReturn` ([1..4])

    where event n = (liftIO $ threadDelay (n * 100000)) >> (return n)

compose :: (IsStream t, Semigroup (t IO Int))
    => (t IO Int -> StreamT IO Int) -> t IO Int -> ([Int] -> [Int]) -> Spec
compose t z srt = do
    -- XXX these should get covered by the property tests
    it "Compose mempty, mempty" $
        (tl (z <> z)) `shouldReturn` ([] :: [Int])
    it "Compose empty at the beginning" $
        (tl $ (z <> singleton 1)) `shouldReturn` [1]
    it "Compose empty at the end" $
        (tl $ (singleton 1 <> z)) `shouldReturn` [1]
    it "Compose two" $
        (tl (singleton 0 <> singleton 1) >>= return . srt)
            `shouldReturn` [0, 1]
    it "Compose many" $
        ((tl $ forEachWith (<>) [1..100] singleton) >>= return . srt)
            `shouldReturn` [1..100]

    -- These are not covered by the property tests
    it "Compose three - empty in the middle" $
        ((tl $ (singleton 0 <> z <> singleton 1)) >>= return . srt)
            `shouldReturn` [0, 1]
    it "Compose left associated" $
        ((tl $ (((singleton 0 <> singleton 1) <> singleton 2) <> singleton 3))
            >>= return . srt) `shouldReturn` [0, 1, 2, 3]
    it "Compose right associated" $
        ((tl $ (singleton 0 <> (singleton 1 <> (singleton 2 <> singleton 3))))
            >>= return . srt) `shouldReturn` [0, 1, 2, 3]
    it "Compose hierarchical (multiple levels)" $
        ((tl $ (((singleton 0 <> singleton 1) <> (singleton 2 <> singleton 3))
                <> ((singleton 4 <> singleton 5) <> (singleton 6 <> singleton 7)))
            ) >>= return . srt) `shouldReturn` [0..7]
    where tl = A.toList . t

composeAndComposeSimple
    :: ( IsStream t1, Semigroup (t1 IO Int)
       , IsStream t2, Monoid (t2 IO Int), Monad (t2 IO)
#if __GLASGOW_HASKELL__ < 804
       , Semigroup (t2 IO Int)
#endif
       )
    => (t1 IO Int -> StreamT IO Int)
    -> (t2 IO Int -> t2 IO Int)
    -> [[Int]] -> Spec
composeAndComposeSimple t1 t2 answer = do
    let rfold = adapt . t2 . foldMapWith (<>) return
    it "Compose right associated outer expr, right folded inner" $
         ((A.toList . t1) (rfold [1,2,3] <> (rfold [4,5,6] <> rfold [7,8,9])))
            `shouldReturn` (answer !! 0)

    it "Compose left associated outer expr, right folded inner" $
         ((A.toList . t1) ((rfold [1,2,3] <> rfold [4,5,6]) <> rfold [7,8,9]))
            `shouldReturn` (answer !! 1)

    let lfold xs = adapt $ t2 $ foldl (<>) mempty $ map return xs
    it "Compose right associated outer expr, left folded inner" $
         ((A.toList . t1) (lfold [1,2,3] <> (lfold [4,5,6] <> lfold [7,8,9])))
            `shouldReturn` (answer !! 2)

    it "Compose left associated outer expr, left folded inner" $
         ((A.toList . t1) ((lfold [1,2,3] <> lfold [4,5,6]) <> lfold [7,8,9]))
            `shouldReturn` (answer !! 3)

loops
    :: (IsStream t, Semigroup (t IO Int), MonadIO (t IO))
    => (t IO Int -> t IO Int)
    -> ([Int] -> [Int])
    -> ([Int] -> [Int])
    -> Spec
loops t tsrt hsrt = do
    it "Tail recursive loop" $ ((A.toList . adapt) (loopTail 0) >>= return . tsrt)
            `shouldReturn` [0..3]

    it "Head recursive loop" $ ((A.toList . adapt) (loopHead 0) >>= return . hsrt)
            `shouldReturn` [0..3]

    where
        loopHead x = do
            -- this print line is important for the test (causes a bind)
            liftIO $ putStrLn "LoopHead..."
            t $ (if x < 3 then loopHead (x + 1) else nil) <> return x

        loopTail x = do
            -- this print line is important for the test (causes a bind)
            liftIO $ putStrLn "LoopTail..."
            t $ return x <> (if x < 3 then loopTail (x + 1) else nil)

bindAndComposeSimple
    :: ( IsStream t1, IsStream t2, Semigroup (t2 IO Int), Monad (t2 IO))
    => (t1 IO Int -> StreamT IO Int)
    -> (t2 IO Int -> t2 IO Int)
    -> Spec
bindAndComposeSimple t1 t2 = do
    -- XXX need a bind in the body of forEachWith instead of a simple return
    it "Compose many (right fold) with bind" $
        ((A.toList . t1) (adapt . t2 $ forEachWith (<>) [1..10 :: Int] return)
            >>= return . sort) `shouldReturn` [1..10]

    it "Compose many (left fold) with bind" $
        let forL xs k = foldl (<>) nil $ map k xs
         in ((A.toList . t1) (adapt . t2 $ forL [1..10 :: Int] return)
                >>= return . sort) `shouldReturn` [1..10]

bindAndComposeHierarchy
    :: ( IsStream t1, Monad (t1 IO)
       , IsStream t2, Monad (t2 IO))
    => (t1 IO Int -> StreamT IO Int)
    -> (t2 IO Int -> t2 IO Int)
    -> ([t2 IO Int] -> t2 IO Int)
    -> Spec
bindAndComposeHierarchy t1 t2 g = do
    it "Bind and compose nested" $
        ((A.toList . t1) bindComposeNested >>= return . sort)
            `shouldReturn` (sort (
                   [12, 18]
                ++ replicate 3 13
                ++ replicate 3 17
                ++ replicate 6 14
                ++ replicate 6 16
                ++ replicate 7 15) :: [Int])

    where

    -- bindComposeNested :: AParallelT IO Int
    bindComposeNested =
        let c1 = tripleCompose (return 1) (return 2) (return 3)
            c2 = tripleCompose (return 4) (return 5) (return 6)
            c3 = tripleCompose (return 7) (return 8) (return 9)
            b = tripleBind c1 c2 c3
-- it seems to be causing a huge space leak in hspec so disabling this for now
--            c = tripleCompose b b b
--            m = tripleBind c c c
--         in m
         in b

    tripleCompose a b c = adapt . t2 $ g [a, b, c]
    tripleBind mx my mz =
        mx >>= \x -> my
           >>= \y -> mz
           >>= \z -> return (x + y + z)

mixedOps :: Spec
mixedOps = do
    it "Compose many ops" $
        (toListSerial composeMixed >>= return . sort)
            `shouldReturn` ([8,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,11,11
                            ,11,11,11,11,11,11,11,11,12,12,12,12,12,13
                            ] :: [Int])
    where

    composeMixed :: StreamT IO Int
    composeMixed = do
        liftIO $ return ()
        liftIO $ putStr ""
        x <- return 1
        y <- return 2
        z <- do
                x1 <- parallely $ return 1 <> return 2
                liftIO $ return ()
                liftIO $ putStr ""
                y1 <- coparallely $ return 1 <> return 2
                z1 <- do
                    x11 <- return 1 <> return 2
                    y11 <- coparallely $ return 1 <> return 2
                    z11 <- costreamly $ return 1 <> return 2
                    liftIO $ return ()
                    liftIO $ putStr ""
                    return (x11 + y11 + z11)
                return (x1 + y1 + z1)
        return (x + y + z)
