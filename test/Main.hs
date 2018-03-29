{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import Data.Foldable (forM_)
import Data.List (sort)
import Data.Maybe (fromJust)
import Test.Hspec

import Streamly
import qualified Streamly.Prelude as A

toListSerial :: StreamT IO a -> IO [a]
toListSerial = A.toList . serially

toListReversely :: ReverseT IO a -> IO [a]
toListReversely = A.toList . reversely

toListInterleaved :: InterleavedT IO a -> IO [a]
toListInterleaved = A.toList . interleaving

toListAsync :: AsyncT IO a -> IO [a]
toListAsync = A.toList . asyncly

toListParallel :: Ord a => ParallelT IO a -> IO [a]
toListParallel = fmap sort . A.toList . parallely

main :: IO ()
main = hspec $ do
    describe "Runners" $ do
        it "simple serially" $
            (runStreaming . serially) (return (0 :: Int)) `shouldReturn` ()
        it "simple serially with IO" $
            (runStreaming . serially) (liftIO $ putStrLn "hello") `shouldReturn` ()
        it "Captures a return value using toList" $
            toListSerial (return 0) `shouldReturn` ([0] :: [Int])

    describe "Empty" $ do
        it "Monoid - mempty" $
            (toListSerial mempty) `shouldReturn` ([] :: [Int])
        it "Alternative - empty" $
            (toListSerial empty) `shouldReturn` ([] :: [Int])
        it "MonadPlus - mzero" $
            (toListSerial mzero) `shouldReturn` ([] :: [Int])

    ---------------------------------------------------------------------------
    -- Functor
    ---------------------------------------------------------------------------

    describe "Functor (fmap)" $ do
        it "Simple fmap" $
            (toListSerial $ fmap (+1) (return 1)) `shouldReturn` ([2] :: [Int])
        it "fmap on composed (<>)" $
            (toListSerial $ fmap (+1) (return 1 <> return 2))
                `shouldReturn` ([2,3] :: [Int])
        it "fmap on composed (<|>)" $
            (toListSerial $ fmap (+1) (return 1 <|> return 2))
                `shouldReturn` ([2,3] :: [Int])

    ---------------------------------------------------------------------------
    -- Applicative
    ---------------------------------------------------------------------------

    describe "Applicative" $ do
        it "Simple apply" $
            (toListSerial $ (,) <$> (return 1) <*> (return 2))
                `shouldReturn` ([(1,2)] :: [(Int, Int)])

        it "Apply - serial composed first argument" $
            (toListSerial $ (,) <$> (return 1 <> return 2) <*> (return 3))
                `shouldReturn` ([(1,3),(2,3)] :: [(Int, Int)])

        it "Apply - serial composed second argument" $
            (toListSerial $ (,) <$> (return 1) <*> (return 2 <> return 3))
                `shouldReturn` ([(1,2),(1,3)] :: [(Int, Int)])

        it "Apply - parallel composed first argument" $
            (toListSerial $ (,) <$> (return 1 <|> return 2) <*> (return 3))
                `shouldReturn` ([(1,3),(2,3)] :: [(Int, Int)])

        it "Apply - parallel composed second argument" $
            (toListSerial $ (,) <$> (return 1) <*> (return 2 <|> return 3))
                `shouldReturn` ([(1,2),(1,3)] :: [(Int, Int)])

    ---------------------------------------------------------------------------
    -- Binds
    ---------------------------------------------------------------------------

    describe "Bind then" thenBind
    describe "Pure bind serial" $ pureBind toListSerial
    describe "Pure bind serial interleaved" $ pureBind toListInterleaved
    describe "Pure bind parallel DFS" $ pureBind toListAsync
    describe "Pure bind parallel BFS" $ pureBind toListParallel

    describe "Bind (>>=) with empty" $ bindEmpty toListSerial
    describe "Bind (>->) with empty" $ bindEmpty toListInterleaved
    describe "Bind (>|>) with empty" $ bindEmpty toListAsync
    describe "Bind (>>|) with empty" $ bindEmpty toListParallel

    ---------------------------------------------------------------------------
    -- ReverseT
    ---------------------------------------------------------------------------

    describe "Dual to StreamT" $ do

        it "empty reversely" $
          toListReversely (do
            x <- A.each []
            y <- A.each []
            return (x, y))
          `shouldReturn` ([] :: [(Int, Int)])

        it "empty inner reversely" $
          toListReversely (do
            x <- A.each []
            y <- A.each [1, 2]
            return (x, y))
          `shouldReturn` ([] :: [(Int, Int)])

        it "empty outer reversely" $
          toListReversely (do
            x <- A.each [1, 2]
            y <- A.each []
            return (x, y))
          `shouldReturn` ([] :: [(Int, Int)])

        it "produce reversely" $
          toListReversely (do
            x <- A.each [1, 2, 3]
            y <- A.each [4, 5, 6]
            return (x, y))
          `shouldReturn` ([(1, 4), (2, 4), (3, 4), (1, 5), (2, 5), (3, 5),(1, 6), (2, 6), (3, 6)] :: [(Int, Int)])

        it "produce layers reversely" $
          toListReversely (do
            x <- A.each [1, 2]
            y <- A.each [3, 4]
            z <- A.each [5, 6]
            return (x, y, z))
          `shouldReturn` ([(1, 3, 5), (2, 3, 5), (1, 4, 5), (2, 4, 5), (1, 3, 6), (2, 3, 6), (1, 4, 6), (2, 4, 6)]:: [(Int, Int, Int)])

    ---------------------------------------------------------------------------
    -- Monoidal Compositions
    ---------------------------------------------------------------------------

    describe "Serial Composition (<>)" $ compose (<>) id
    describe "Serial Composition (mappend)" $ compose mappend id
    describe "Interleaved Composition (<>)" $ compose (<=>) sort
    describe "Left biased parallel Composition (<|)" $ compose (<|) sort
    describe "Fair parallel Composition (<|>)" $ compose (<|>) sort
    describe "Fair parallel Composition (mplus)" $ compose mplus sort

    ---------------------------------------------------------------------------
    -- Monoidal Composition ordering checks
    ---------------------------------------------------------------------------

    describe "Serial interleaved ordering check (<=>)" $ interleaveCheck (<=>)
    describe "Parallel interleaved ordering check (<|>)" $ interleaveCheck (<|>)
    describe "Left biased parallel time order check" $ parallelCheck (<|)
    describe "Fair parallel time order check" $ parallelCheck (<|>)

    ---------------------------------------------------------------------------
    -- TBD Monoidal composition combinations
    ---------------------------------------------------------------------------

    -- TBD need more such combinations to be tested.
    describe "<> and <>" $ composeAndComposeSimple (<>) (<>) (cycle [[1 .. 9]])

    describe "<> and <=>" $ composeAndComposeSimple
      (<>)
      (<=>)
      ([ [1 .. 9]
       , [1 .. 9]
       , [1, 3, 2, 4, 6, 5, 7, 9, 8]
       , [1, 3, 2, 4, 6, 5, 7, 9, 8]
       ])

    describe "<=> and <=>" $ composeAndComposeSimple
      (<=>)
      (<=>)
      ([ [1, 4, 2, 7, 3, 5, 8, 6, 9]
       , [1, 7, 4, 8, 2, 9, 5, 3, 6]
       , [1, 4, 3, 7, 2, 6, 9, 5, 8]
       , [1, 7, 4, 9, 3, 8, 6, 2, 5]
       ])

    describe "<=> and <>" $ composeAndComposeSimple
      (<=>)
      (<>)
      ([ [1, 4, 2, 7, 3, 5, 8, 6, 9]
       , [1, 7, 4, 8, 2, 9, 5, 3, 6]
       , [1, 4, 2, 7, 3, 5, 8, 6, 9]
       , [1, 7, 4, 8, 2, 9, 5, 3, 6]
       ])

    describe "Nested parallel and serial compositions" $ do
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
            let t = timed
             in toListSerial (
                    ((t 4 <|> t 8) <> (t 1 <|> t 2))
                <|> ((t 4 <|> t 8) <> (t 1 <|> t 2)))
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
            let t = timed
             in toListSerial (
                    ((t 4 <|> t 8) <|> (t 0 <|> t 2))
                <|> ((t 4 <|> t 8) <|> (t 0 <|> t 2)))
            `shouldReturn` ([0,0,2,2,4,4,8,8])

    ---------------------------------------------------------------------------
    -- Monoidal composition recursion loops
    ---------------------------------------------------------------------------

    describe "Serial loops (<>)" $ loops (<>) id reverse
    describe "Left biased parallel loops (<|)" $ loops (<|) sort sort
    describe "Fair parallel loops (<|>)" $ loops (<|>) sort sort

    ---------------------------------------------------------------------------
    -- Bind and monoidal composition combinations
    ---------------------------------------------------------------------------

    forM_ [(<>), (<=>), (<|), (<|>)] $ \g ->
        describe "Bind and compose" $ bindAndComposeSimple toListSerial g

    forM_ [(<>), (<=>), (<|), (<|>)] $ \g ->
        describe "Bind and compose" $ bindAndComposeSimple toListInterleaved g

    forM_ [(<>), (<=>), (<|), (<|>)] $ \g ->
        describe "Bind and compose" $ bindAndComposeSimple toListAsync g

    forM_ [(<>), (<=>), (<|), (<|>)] $ \g ->
        describe "Bind and compose" $ bindAndComposeSimple toListParallel g

    let fldr f = foldr f empty
        fldl f = foldl f empty
     in do
        forM_ [(<>), (<=>), (<|), (<|>)] $ \g ->
            forM_ [fldr, fldl] $ \k ->
                describe "Bind and compose" $
                    bindAndComposeHierarchy toListSerial (k g)
        forM_ [(<>), (<=>), (<|), (<|>)] $ \g ->
            forM_ [fldr, fldl] $ \k ->
                describe "Bind and compose" $
                    bindAndComposeHierarchy toListInterleaved (k g)
        forM_ [(<>), (<=>), (<|), (<|>)] $ \g ->
            forM_ [fldr, fldl] $ \k ->
                describe "Bind and compose" $
                    bindAndComposeHierarchy toListAsync (k g)
        forM_ [(<>), (<=>), (<|), (<|>)] $ \g ->
            forM_ [fldr, fldl] $ \k ->
                describe "Bind and compose" $
                    bindAndComposeHierarchy toListParallel (k g)

    -- Nest two lists using different styles of product compositions
    it "Nests two streams using monadic serial composition" nestTwoSerial
    it "Nests two streams using monadic interleaved composition" nestTwoInterleaved
    it "Nests two streams using monadic async composition" nestTwoAsync
    it "Nests two streams using monadic parallel composition" nestTwoParallel

    it "Nests two streams using applicative serial composition" nestTwoSerialApp
    it "Nests two streams using applicative interleaved composition" nestTwoInterleavedApp
    it "Nests two streams using applicative async composition" nestTwoAsyncApp
    it "Nests two streams using applicative parallel composition" nestTwoParallelApp

    it "Nests two streams using Num serial composition" nestTwoSerialNum
    it "Nests two streams using Num interleaved composition" nestTwoInterleavedNum
    it "Nests two streams using Num async composition" nestTwoAsyncNum
    -- This test fails intermittently, need to investigate
    -- it "Nests two streams using Num parallel composition" nestTwoParallelNum

    ---------------------------------------------------------------------------
    -- TBD Bind and Bind combinations
    ---------------------------------------------------------------------------

    -- TBD combine all binds and all compose in one example
    describe "Miscellaneous combined examples" mixedOps

    ---------------------------------------------------------------------------
    -- Stream operations
    ---------------------------------------------------------------------------

    -- XXX for streams other than StreamT
    describe "Stream Ops empty" $ streamOperations makeEmptyStream
    describe "Stream ops singleton constr" $ streamOperations makeSingletonStream1
    describe "Stream ops singleton folded" $ streamOperations makeSingletonStream2
    describe "Stream Ops constr" $ streamOperations makeStream1
    describe "Stream Ops folded" $ streamOperations $ makeStream2
          ((<>) :: StreamT IO Int -> StreamT IO Int -> StreamT IO Int)

    describe "Serial zipping" $
        zipOps A.zipWith A.zipWithM zipping
    describe "Async zipping" $
        zipOps A.zipAsyncWith A.zipAsyncWithM zippingAsync

makeEmptyStream :: (StreamT IO Int, [Int], Int)
makeEmptyStream = (A.nil, [], 0)

makeSingletonStream1 :: (StreamT IO Int, [Int], Int)
makeSingletonStream1 = (1 `A.cons` A.nil, [1], 1)

makeSingletonStream2 :: (StreamT IO Int, [Int], Int)
makeSingletonStream2 = (return 1, [1], 1)

-- Streams that indicate an end via the stop continuation
makeStream1 :: (StreamT IO Int, [Int], Int)
makeStream1 =
    let list = [1..10]
        stream = A.each list
    in (stream, list, 10)

-- Streams that indicate an end via the yield continuation
makeStream2 :: (Streaming t, Monad (t IO))
    => (t IO Int -> t IO Int -> t IO Int)
    -> (t IO Int, [Int], Int)
makeStream2 f =
    let list = [1..10]
        stream = foldMapWith f return list
    in (stream, list, 10)

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

nestTwoSerialNum :: Expectation
nestTwoSerialNum =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in toListSerial (s1 + s2)
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

nestTwoInterleavedNum :: Expectation
nestTwoInterleavedNum =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in toListInterleaved (s1 + s2)
        `shouldReturn` ([6,7,7,8,8,8,9,9,9,9,10,10,10,11,11,12] :: [Int])

nestTwoAsync :: Expectation
nestTwoAsync =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in toListAsync (do
        x <- s1
        y <- s2
        return (x + y)
        ) `shouldReturn` ([6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12] :: [Int])

nestTwoAsyncApp :: Expectation
nestTwoAsyncApp =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in toListAsync ((+) <$> s1 <*> s2)
        `shouldReturn` ([6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12] :: [Int])

nestTwoAsyncNum :: Expectation
nestTwoAsyncNum =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in toListAsync (s1 + s2)
        `shouldReturn` ([6,7,8,9,7,8,9,10,8,9,10,11,9,10,11,12] :: [Int])

nestTwoParallel :: Expectation
nestTwoParallel =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in toListParallel (do
        x <- s1
        y <- s2
        return (x + y)
        ) `shouldReturn` ([6,7,7,8,8,8,9,9,9,9,10,10,10,11,11,12] :: [Int])

nestTwoParallelApp :: Expectation
nestTwoParallelApp =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in toListParallel ((+) <$> s1 <*> s2)
        `shouldReturn` ([6,7,7,8,8,8,9,9,9,9,10,10,10,11,11,12] :: [Int])

{-
nestTwoParallelNum :: Expectation
nestTwoParallelNum =
    let s1 = foldMapWith (<>) return [1..4]
        s2 = foldMapWith (<>) return [5..8]
    in toListParallel (s1 + s2)
        `shouldReturn` ([6,7,7,8,8,8,9,9,9,9,10,10,10,11,11,12] :: [Int])
-}

zipOps :: (Streaming t, Applicative (t IO))
    => (forall a b c. (a -> b -> c)
        -> StreamT IO a -> StreamT IO b -> StreamT IO c)
    -> (forall a b c. (a -> b -> StreamT IO c)
        -> StreamT IO a -> StreamT IO b -> StreamT IO c)
    -> (forall a. t IO a -> t IO a)
    -> Spec
zipOps z zM app = do
    it "zipWith" $
        let s1 = foldMapWith (<>) return [1..10]
            s2 = foldMapWith (<>) return [1..]
         in toListSerial (z (+) s1 s2)
        `shouldReturn` ([2,4..20] :: [Int])

    it "zipWithM" $
        let s1 = foldMapWith (<>) return [1..10]
            s2 = foldMapWith (<>) return [1..]
         in toListSerial (zM (\a b -> return (a + b)) s1 s2)
        `shouldReturn` ([2,4..20] :: [Int])

    it "Applicative zip" $
        let s1 = adapt $ serially $ foldMapWith (<>) return [1..10]
            s2 = adapt $ serially $ foldMapWith (<>) return [1..]
            f = A.toList . app
            functorial = f $ (+) <$> s1 <*> s2
            applicative = f $ pure (+) <*> s1 <*> s2
            expected = ([2,4..20] :: [Int])
         in (,) <$> functorial <*> applicative
        `shouldReturn` (expected, expected)

timed :: Int -> StreamT IO Int
timed x = liftIO (threadDelay (x * 100000)) >> return x

thenBind :: Spec
thenBind = do
    it "Simple runStreaming and 'then' with IO" $
        (runStreaming . serially) (liftIO (putStrLn "hello") >> liftIO (putStrLn "world"))
            `shouldReturn` ()
    it "Then and toList" $
        toListSerial (return (1 :: Int) >> return 2) `shouldReturn` ([2] :: [Int])

type ToListType s = (forall a. Ord a => s IO a -> IO [a])
pureBind :: Monad (s IO) => ToListType s -> Spec
pureBind l = do
    it "Bind and toList" $
        l (return 1 `f` \x -> return 2 `f` \y -> return (x + y))
            `shouldReturn` ([3] :: [Int])
    where f = (>>=)

bindEmpty :: (Monad (s IO), Alternative (s IO)) => ToListType s -> Spec
bindEmpty l = it "Binds with empty" $
    (l (return (1 :: Int) `f` \_ -> empty `f` \_ -> return 2))
        `shouldReturn` ([] :: [Int])
    where f = (>>=)

interleaveCheck
    :: (StreamT IO Int -> StreamT IO Int -> StreamT IO Int)
    -> Spec
interleaveCheck f =
    it "Interleave four" $
        toListSerial ((return 0 <> return 1) `f` (return 100 <> return 101))
            `shouldReturn` ([0, 100, 1, 101])

parallelCheck :: (StreamT IO Int -> StreamT IO Int -> StreamT IO Int) -> Spec
parallelCheck f = do
    it "Parallel ordering left associated" $
        toListSerial (((event 4 `f` event 3) `f` event 2) `f` event 1)
            `shouldReturn` ([1..4])

    it "Parallel ordering right associated" $
        toListSerial (event 4 `f` (event 3 `f` (event 2 `f` event 1)))
            `shouldReturn` ([1..4])

    where event n = (liftIO $ threadDelay (n * 100000)) >> (return n)

compose
    :: (StreamT IO Int -> StreamT IO Int -> StreamT IO Int)
    -> ([Int] -> [Int])
    -> Spec
compose f srt = do
    it "Compose mempty, mempty" $
        (tl (mempty `f` mempty)) `shouldReturn` []
    it "Compose empty, empty" $
        (tl (empty `f` empty)) `shouldReturn` []
    it "Compose empty at the beginning" $
        (tl $ (empty `f` return 1)) `shouldReturn` [1]
    it "Compose empty at the end" $
        (tl $ (return 1 `f` empty)) `shouldReturn` [1]
    it "Compose two" $
        (tl (return 0 `f` return 1) >>= return . srt)
            `shouldReturn` [0, 1]
    it "Compose three - empty in the middle" $
        ((tl $ (return 0 `f` empty `f` return 1)) >>= return . srt)
            `shouldReturn` [0, 1]
    it "Compose left associated" $
        ((tl $ (((return 0 `f` return 1) `f` return 2) `f` return 3))
            >>= return . srt) `shouldReturn` [0, 1, 2, 3]
    it "Compose right associated" $
        ((tl $ (return 0 `f` (return 1 `f` (return 2 `f` return 3))))
            >>= return . srt) `shouldReturn` [0, 1, 2, 3]
    it "Compose many" $
        ((tl $ forEachWith f [1..100] return) >>= return . srt)
            `shouldReturn` [1..100]
    it "Compose hierarchical (multiple levels)" $
        ((tl $ (((return 0 `f` return 1) `f` (return 2 `f` return 3))
                `f` ((return 4 `f` return 5) `f` (return 6 `f` return 7)))
            ) >>= return . srt) `shouldReturn` [0..7]
    where tl = toListSerial

composeAndComposeSimple
    :: (StreamT IO Int -> StreamT IO Int -> StreamT IO Int)
    -> (StreamT IO Int -> StreamT IO Int -> StreamT IO Int)
    -> [[Int]]
    -> Spec
composeAndComposeSimple f g answer = do
    it "Compose right associated outer expr, right folded inner" $
        let fold = foldMapWith g return
         in (toListSerial (fold [1,2,3] `f` (fold [4,5,6] `f` fold [7,8,9])))
            `shouldReturn` (answer !! 0)

    it "Compose left associated outer expr, right folded inner" $
        let fold = foldMapWith g return
         in (toListSerial ((fold [1,2,3] `f` fold [4,5,6]) `f` fold [7,8,9]))
            `shouldReturn` (answer !! 1)

    it "Compose right associated outer expr, left folded inner" $
        let fold xs = foldl g empty $ map return xs
         in (toListSerial (fold [1,2,3] `f` (fold [4,5,6] `f` fold [7,8,9])))
            `shouldReturn` (answer !! 2)

    it "Compose left associated outer expr, left folded inner" $
        let fold xs = foldl g empty $ map return xs
         in (toListSerial ((fold [1,2,3] `f` fold [4,5,6]) `f` fold [7,8,9]))
            `shouldReturn` (answer !! 3)


loops
    :: (StreamT IO Int -> StreamT IO Int -> StreamT IO Int)
    -> ([Int] -> [Int])
    -> ([Int] -> [Int])
    -> Spec
loops f tsrt hsrt = do
    it "Tail recursive loop" $ (toListSerial (loopTail 0) >>= return . tsrt)
            `shouldReturn` [0..3]

    it "Head recursive loop" $ (toListSerial (loopHead 0) >>= return . hsrt)
            `shouldReturn` [0..3]

    where
        loopHead x = do
            -- this print line is important for the test (causes a bind)
            liftIO $ putStrLn "LoopHead..."
            (if x < 3 then loopHead (x + 1) else empty) `f` return x

        loopTail x = do
            -- this print line is important for the test (causes a bind)
            liftIO $ putStrLn "LoopTail..."
            return x `f` (if x < 3 then loopTail (x + 1) else empty)

bindAndComposeSimple
    :: (Streaming t, Alternative (t IO), Monad (t IO))
    => (forall a. Ord a => t IO a -> IO [a])
    -> (t IO Int -> t IO Int -> t IO Int)
    -> Spec
bindAndComposeSimple tl g = do
    it "Compose many (right fold) with bind" $
        (tl (forEachWith g [1..10 :: Int] $ \x -> return x `f` (return .  id))
            >>= return . sort) `shouldReturn` [1..10]

    it "Compose many (left fold) with bind" $
        let forL xs k = foldl g empty $ map k xs
         in (tl (forL [1..10 :: Int] $ \x -> return x `f` (return . id))
                >>= return . sort) `shouldReturn` [1..10]
    where f = (>>=)

bindAndComposeHierarchy
    :: Monad (s IO) => (forall a. Ord a => s IO a -> IO [a])
    -> ([s IO Int] -> s IO Int)
    -> Spec
bindAndComposeHierarchy tl g = do
    it "Bind and compose nested" $
        (tl bindComposeNested >>= return . sort)
            `shouldReturn` (sort (
                   [12, 18]
                ++ replicate 3 13
                ++ replicate 3 17
                ++ replicate 6 14
                ++ replicate 6 16
                ++ replicate 7 15) :: [Int])

    where

    -- bindComposeNested :: AsyncT IO Int
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

    tripleCompose a b c = g [a, b, c]
    tripleBind mx my mz =
        mx `f` \x -> my
           `f` \y -> mz
           `f` \z -> return (x + y + z)
    f = (>>=)

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
                x1 <- return 1 <|> return 2
                liftIO $ return ()
                liftIO $ putStr ""
                y1 <- return 1 <| return 2
                z1 <- do
                    x11 <- return 1 <> return 2
                    y11 <- return 1 <| return 2
                    z11 <- return 1 <=> return 2
                    liftIO $ return ()
                    liftIO $ putStr ""
                    return (x11 + y11 + z11)
                return (x1 + y1 + z1)
        return (x + y + z)

streamOperations :: Streaming t => (t IO Int, [Int], Int) -> Spec
streamOperations (stream, list, len) = do

    -- Generation
    it "replicateM" $ do
            let x = return (1 :: Int)
            str <- A.toList . serially $ A.replicateM len x
            lst <- replicateM len x
            return $ str == lst
        `shouldReturn` True

    it "iterate" $
            (A.toList . serially . (A.take len) $ (A.iterate (+ 1) (0 :: Int)))
            `shouldReturn` (take len $ iterate (+ 1) 0)

    it "iterateM" $ do
              let addM = (\ y -> return (y + 1))
              A.toList . serially . (A.take len) $ A.iterateM addM (0 :: Int)
              `shouldReturn` (take len $ iterate (+ 1) 0)


    -- Filtering
    it "filter all out" $ transform (A.filter (> len)) (filter (> len))
    it "filter all in"  $ transform (A.filter (<= len)) (filter (<= len))
    it "filter even"    $ transform (A.filter even)  (filter even)

    it "take all"  $ transform (A.take len) (take len)
    it "take none" $ transform (A.take 0) (take 0)
    it "take some" $ transform (A.take $ len - 1) (take $ len - 1)
    it "take one" $ transform (A.take 1) (take 1)

    it "takeWhile true"  $ transform (A.takeWhile (const True))
                                     (takeWhile (const True))
    it "takeWhile false" $ transform (A.takeWhile (const False))
                                     (takeWhile (const False))
    it "takeWhile < some" $ transform (A.takeWhile (< (len `div` 2)))
                                      (takeWhile (< (len `div` 2)))

    it "drop all"  $ transform (A.drop len) (drop len)
    it "drop none" $ transform (A.drop 0)  (drop 0)
    it "drop some" $ transform (A.drop $ len - 1)  (drop $ len - 1)
    it "drop one"  $ transform (A.drop 1)  (drop 1)

    it "dropWhile true"  $ transform (A.dropWhile (const True))
                                     (dropWhile (const True))
    it "dropWhile false" $ transform (A.dropWhile (const False))
                                     (dropWhile (const False))
    it "dropWhile < some" $ transform (A.dropWhile (< (len `div` 2)))
                                      (dropWhile (< (len `div` 2)))

    -- Transformations
    it "scan left"  $ transform (A.scan (+) 0 id) (scanl (+) 0)
    it "reverse" $ transform A.reverse reverse

    -- Elimination
    it "foldl" $ elimination (A.foldl (+) 0 id) (foldl (+) 0)
    it "all" $ elimination (A.all even) (all even)
    it "any" $ elimination (A.any even) (any even)
    it "length" $ elimination A.length length
    it "elem" $ elimination (A.elem (len - 1)) (elem (len - 1))
    it "elem" $ elimination (A.elem (len + 1)) (elem (len + 1))
    it "notElem" $ elimination (A.notElem (len - 1)) (notElem (len - 1))
    it "notElem" $ elimination (A.notElem (len + 1)) (notElem (len + 1))
    it "sum" $ elimination A.sum sum
    it "product" $ elimination A.product product

    if list == []
    then do
        it "head empty" $ A.head stream `shouldReturn` Nothing
        it "last empty" $ A.last stream `shouldReturn` Nothing
        it "maximum empty" $ A.maximum stream `shouldReturn` Nothing
        it "minimum empty" $ A.minimum stream `shouldReturn` Nothing
        it "null empty" $ A.null stream `shouldReturn` True
        it "tail empty" $ (A.tail stream >>= return . maybe True (const False))
            `shouldReturn` True
    else do
        it "head nonEmpty" $ A.head stream `shouldReturn` Just (head list)
        it "last nonEmpty" $ A.last stream `shouldReturn` Just (last list)
        it "maximum nonEmpty" $ A.maximum stream
            `shouldReturn` Just (maximum list)
        it "minimum nonEmpty" $ A.minimum stream
            `shouldReturn` Just (minimum list)
        it "null nonEmpty" $ A.null stream `shouldReturn` False
        it "tail nonEmpty" $ (A.tail stream >>= A.toList . fromJust)
            `shouldReturn` tail list

    where
    -- XXX run on empty stream as well
    transform streamOp listOp =
        (A.toList $ streamOp stream) `shouldReturn` listOp list

    elimination streamOp listOp = (streamOp stream) `shouldReturn` listOp list
