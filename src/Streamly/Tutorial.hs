{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- |
-- Module      : Streamly.Tutorial
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
--
-- Streamly, short for stream concurrently, combines the essence of streaming
-- and concurrency in functional programming. You can write concurrent as well
-- as non-concurrent applications with streamly. Streaming enables writing
-- modular, composable and scalable applications with ease and concurrency
-- allows you to make them scale and perform well.
-- Streamly enables writing concurrent applications without being aware of
-- threads or synchronization. No explicit thread control is needed, where
-- applicable the concurrency rate is automatically controlled based on the
-- demand by the consumer. However, combinators are provided to fine tune the
-- concurrency control.
-- Streaming and concurrency together enable expressing reactive applications
-- conveniently. See 'Streamly.Examples' for a simple SDL based FRP example.
--
-- In this tutorial we will go over the basic concepts and how to use the
-- library.  For examples and other ways to use the library see
-- the module @Streamly.Examples@ as well.

module Streamly.Tutorial
    (
    -- * Streams
    -- $streams

    -- ** Generating Streams
    -- $generating

    -- ** Eliminating Streams
    -- $eliminating

    -- * Semigroup Style Composition
    -- $semigroup

    -- ** Serial composition ('<>')
    -- $serial

    -- ** Interleaved composition ('<=>')
    -- $interleaved

    -- ** Async composition ('<|')
    -- $parallel

    -- ** Parallel composition ('<|>')
    -- $fairParallel

    -- ** Custom composition
    -- $custom

    -- * Monoid Style composition
    -- $monoid

    -- * Transforming Streams
    -- $transforming

    -- ** Functor
    -- $functor

    -- ** Applicative
    -- $applicative

    -- ** Monad
    -- $listt

    -- * Alternate Monadic Compositions
    -- $monadicAlternatives

    -- ** Serial Iterations
    -- $regularSerial

    -- ** Interleaved Iterations
    -- $interleavedNesting

    -- ** Concurrent Iterations
    -- $concurrentNesting

    -- ** Fairly Concurrent Iterations
    -- $fairlyConcurrentNesting

    -- * Folding Streams
    -- $folding

    -- * Zipping Streams
    -- $zipping

    -- * Concurrent Programming Examples
    -- $concurrent

    -- * Reactive Programming
    -- $reactive

    -- * State Machine Model
    -- $statemachine

    -- * Performance
    -- $performance

    -- * Interworking with Streaming Libraries
    -- $interwork

    -- * Comparison with Existing Packages
    -- $comparison
    )
where

import Streamly
import Streamly.Prelude
-- import Streamly.Examples
import Data.Semigroup
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class      (MonadIO(..))
import Control.Monad.Trans.Class   (MonadTrans (lift))

-- $streams
--
-- Streamly provides many different stream types depending on the desired
-- composition style. The simplest type is 'StreamT'. 'StreamT' is a monad
-- transformer, the type @StreamT m a@ represents a stream of values of type
-- 'a' in some underlying monad 'm'. For example, @StreamT IO Int@ is a stream
-- of 'Int' in 'IO' monad.

-- $generating
--
-- Pure values can be placed into the stream type using 'return' or 'pure'.
-- Effects in the IO monad can be lifted to the stream type using the 'liftIO'
-- combinator. In a transformer stack we can lift actions from the lower monad
-- using the 'lift' combinator. Some examples of streams with a single element:
--
-- @
--  return 1 :: StreamT IO Int
-- @
-- @
--  liftIO $ putStrLn "Hello world!" :: StreamT IO ()
-- @
--
-- We can combine streams using '<>' to create streams of many elements:
--
-- @
--  return 1 <> return 2 <> return 3 :: StreamT IO Int
-- @

-- $eliminating
--
-- 'runStreamT' runs a composed 'StreamT' computation, lowering the type into
-- the underlying monad and discarding the result stream:
--
-- @
-- import Streamly
--
-- main = runStreamT $ liftIO $ putStrLn "Hello world!"
-- @
--
-- 'toList' runs a stream computation and collects the result stream in a list
-- in the underlying monad.  'toList' is a polymorphic function that works on
-- multiple stream types belonging to the class 'Streaming'. Therefore, before
-- you run a stream you need to tell how you want to interpret the stream by
-- using one of the stream type combinators ('serially', 'asyncly', 'parallely'
-- etc.). The combinator 'serially' is equivalent to annotating the type as @::
-- StreamT@.
--
-- @
-- import Streamly
--
-- main = do
--  xs \<- toList $ serially $ return 1 <> return 2
--  print xs
-- @
--
-- For other ways to eliminate a stream see the module @Streamly.Prelude@.

-- $semigroup
-- Streams of the same type can be combined into a composite stream in many
-- different ways using one of the semigroup style binary composition operators
-- i.e. '<>', '<=>', '<|', '<|>', 'mplus'. These operators work on all stream
-- types ('StreamT', 'AsyncT' etc.) uniformly.
--
-- To illustrate the concurrent aspects, we will use the following @delay@
-- function to introduce a delay specified in seconds.
--
-- @
-- import Streamly
-- import Control.Concurrent
--
-- delay n = liftIO $ do
--  threadDelay (n * 1000000)
--  tid \<- myThreadId
--  putStrLn (show tid ++ ": Delay " ++ show n)
-- @

-- $serial
-- As we have already seen, the '<>' operator composes multiple streams in
-- series i.e. the next element is yielded only after the previous one has been
-- processed completely. The following example prints the sequence 3, 2, 1 and
-- takes a total of 6 seconds because everything is serial:
--
-- @
-- main = runStreamT $ delay 3 <> delay 2 <> delay 1
-- @
-- @
-- ThreadId 36: Delay 3
-- ThreadId 36: Delay 2
-- ThreadId 36: Delay 1
-- @

-- $interleaved
-- The '<=>' operator interleaves the two computations i.e. it yields one
-- element from the first stream and then one element from the second stream,
-- and so on.  The following example prints the sequence 1, 3, 2, 4 and takes a
-- total of 10 seconds because everything is serial:
--
-- @
-- main = runStreamT $ (delay 1 <> delay 2) \<=> (delay 3 <> delay 4)
-- @
-- @
-- ThreadId 36: Delay 1
-- ThreadId 36: Delay 3
-- ThreadId 36: Delay 2
-- ThreadId 36: Delay 4
-- @

-- $parallel
--
-- The '<|' operator can run computations concurrently, preferring the first
-- computation over the second. The second computation is run concurrently with
-- the first only if the first computation is not producing enough output to
-- keep the consumer busy otherwise the second computation is run serially
-- after the previous one. The number of concurrent threads is adapted
-- dynamically based on the pull rate of the consumer of the stream.
--
-- Note that the left bias of the operator '<|' is suggested by its shape.
-- It is also an unbalanced version of the fairly parallel operator '<|>'.
--
-- The following example runs all the parallel computations in a single thread
-- one after another, because none of them blocks. Note that if the consumer
-- were faster than the producer this would start new threads for each
-- computation to keep up even if none of them blocks:
--
-- @
-- main = runStreamT $ traced (sqrt 9) <| traced (sqrt 16) <| traced (sqrt 25)
-- @
-- @
-- ThreadId 40
-- ThreadId 40
-- ThreadId 40
-- @
--
-- In the following example since the first computation blocks we start the
-- next one in a separate thread and so on:
--
-- @
-- main = runStreamT $ delay 3 <| delay 2 <| delay 1
-- @
-- @
-- ThreadId 42: Delay 1
-- ThreadId 41: Delay 2
-- ThreadId 40: Delay 3
-- @
--
-- When we have a tree of computations composed using this operator, the tree
-- is traversed in DFS style because it always prefers the left computation:
--
-- @
-- main = runStreamT $ (p 1 <| p 2) <| (p 3 <| p 4)
--  where p = liftIO . print
-- @
-- @
-- 1
-- 2
-- 3
-- 4
-- @
--
-- Note that since this operator is left biased it cannot be used when the
-- composed computations have timers that are relative to each other because
-- all computations may not be started at the same time and therefore timers in
-- all of them may not start at the same time.  When relative timing among all
-- computations is important or when we need to start all computations at once
-- for some reason '<|>' must be used instead.  However, '<|' is useful in
-- situations when we want to optimally utilize the resources and we know that
-- the computations can run in parallel but we do not care if they actually run
-- in parallel or not, that decision is left to the scheduler. Also, note that
-- this operator can be used to fold infinite containers in contrast to '<|>',
-- because it does not require us to run all of them at the same time.

-- $fairParallel
--
-- The 'Alternative' composition operator '<|>', like '<|', runs the composed
-- computations in parallel. However, unlike '<|' it runs all of the
-- computations in fairly parallel manner using a round robin scheduling
-- mechanism. Note that this should not be used on infinite containers, as it
-- will lead to an infinite sized scheduling queue.
--
-- The following example sends a query to three search engines and prints the
-- name of the search engine as a response arrives:
--
-- @
-- import Streamly
-- import Network.HTTP.Simple
--
-- main = runStreamT $ google \<|> bing \<|> duckduckgo
--     where
--         google     = get "https://www.google.com/search?q=haskell"
--         bing       = get "https://www.bing.com/search?q=haskell"
--         duckduckgo = get "https://www.duckduckgo.com/?q=haskell"
--         get s = liftIO (httpNoBody (parseRequest_ s) >> putStrLn (show s))
-- @

-- $custom
--
-- The 'async' API can be used to create references to asynchronously running
-- stream computations. We can then use 'uncons' to explore the streams
-- arbitrarily and then recompose individual elements to create a new stream.
-- This way we can dynamically decide which stream to explore at any given
-- time.  Take an example of a merge sort of two sorted streams. We need to
-- keep consuming items from the stream which has the lowest item in the sort
-- order.  This can be achieved using async references to streams. See
-- 'Streamly.Examples.MergeSortedStreams'.

-- $monoid
--
-- Each of the semigroup compositions described has an identity that can be
-- used to fold a possibly empty container. An empty stream is represented by
-- 'nil' which can be represented in various standard forms as 'mempty',
-- 'empty' or 'mzero'.
-- Some fold utilities are also provided by the library for convenience:
--
-- * 'foldWith' folds a 'Foldable' container of stream computations using the
-- given composition operator.
-- * 'foldMapWith' folds like foldWith but also maps a function before folding.
-- * 'forEachWith' is like foldMapwith but the container argument comes before
-- the function argument.
--
-- @
-- import Streamly
--
-- main = do
--  runStreaming . serially $ liftIO $ putStrLn "Hello world!"
--  xs \<- toList . serially $ return 1 <> return 2;               print xs
--  xs \<- toList . serially $ foldWith (<>) (map return [1..10]); print xs
--  xs \<- toList . serially $ foldMapWith (<>) return [1..10];    print xs
--  xs \<- toList . serially $ forEachWith (<>) [1..10] return;    print xs
-- @

-- $transforming
--
-- The previous section discussed ways to merge the elements of two streams
-- without doing any transformation on them. In this section we will explore
-- how to transform streams using 'Functor', 'Applicative' or 'Monad' style
-- compositions. The applicative and monad composition of all 'Streaming' types
-- behave exactly the same way as a list transformer.  For simplicity of
-- illustration we are using streams of pure values in the following examples.
-- However, the real application of streams arises when these streams are
-- generated using monadic actions. In the pure stream cases 'StreamT' is
-- equivalent to the list monad.

-- $functor
--
-- The simplest transformation on a stream is mapping a function on
-- all elements of the stream using 'fmap'.
--
-- @
-- import Streamly
--
-- main = do
--     let nums    = each [1..10]
--     let strings = fmap show nums
--     (toList $ serially $ fmap putStrLn strings) >>= sequence
-- @

-- $applicative
--
-- In functional programmer's parlance the 'Applicative' instance of
-- 'Streaming' types implement non-determinism, exploring all possible
-- combination of choices from both the streams. From an imperative
-- programmer's point of view the applicative behaves like nested loops i.e.
-- for each element in the first argument and for each element in the second
-- argument apply the body of the loop. Note that this behavior of applicative
-- instance of all 'Streaming' types is exactly the same as that of a list
-- transformer.
--
-- The following example prints all combinations of size, color and shape:
--
-- @
-- main = (toList . serially $ (,,) \<$> sizes \<*> colors \<*> shapes) >>= print
--
--     where
--
--     sizes  = each [1, 2, 3]
--     colors = each ["red", "green", "blue"]
--     shapes = each ["triangle", "square", "circle"]
-- @

-- $listt
--
-- The monadic composition of 'Streaming' types is similar to 'Applicative'
-- composition and just the same as that of list transformer. It is like nested
-- loops in the imperative parlance. The monadic continuation is executed for
-- each element in a stream.
-- For example the following program repeatedly reads strings from standard
-- input and echoes them to the standard output. This is like one infinite
-- loop:
--
-- @
-- import Streamly
-- import Data.Semigroup (cycle1)
--
-- main = runStreamT $ do
--  str <- cycle1 (liftIO getLine)
--  liftIO $ putStrLn str
-- @

-- $monadicAlternatives
--
-- Just like we saw multiple non-concurrent and concurrent variants of sum
-- style compositions earlier, monadic composition also has multiple variants
-- each of which exactly corresponds to one of the sum style composition
-- variant.
--
-- We will use the following polymorphic code which is agnostic of a specific
-- 'Streaming' type to illustrate various variants of monadic stream
-- composition. This code can be interpreted in many different ways depending
-- on the actual type used.
--
-- @
-- import Streamly
--
-- composed :: Streaming t => t m a
-- composed = do
--     sz <- sizes
--     cl <- colors
--     sh <- shapes
--     liftIO $ putStrLn $ show (sz, cl, sh)
--
--     where
--
--     sizes  = each [1, 2, 3]
--     colors = each ["red", "green", "blue"]
--     shapes = each ["triangle", "square", "circle"]
-- @

-- $regularSerial
--
-- When we interpret the code as 'StreamT' we get the regular list transformer
-- like serial composition.  The following is equivalent to three nested loops,
-- the first one for sizes, the second one for colors and the third one for
-- shapes. For each size we iterate each color and for each color we iterate
-- each shape.
--
-- @
-- main = runStreaming $ serially $ composed
-- @
--

-- $interleavedNesting
--
-- @
-- main = runStreaming $ interleaved $ composed
-- @

-- $concurrentNesting
--
-- @
-- import Streamly
-- import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
-- import Control.Concurrent
--
-- printWithTid s = do
--     threadDelay 500
--     tid <- myThreadId
--     putStrLn (show tid ++ ": " ++ s)
--
-- main = runStreaming $ asyncly $ do
--     liftIO $ hSetBuffering stdout LineBuffering
--     sz <- sizes
--     cl <- colors
--     sh <- shapes
--     liftIO $ putStrLn $ show (sz, cl, sh)
--
--     where
--
--     toStream xs = foldMapWith (<>) return xs
--     sizes  = toStream [1, 2, 3]
--     colors = toStream ["red", "green", "blue"]
--     shapes = toStream ["triangle", "square", "circle"]
-- @

-- $fairlyConcurrentNesting
--
-- @
-- import Streamly
-- import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
-- import Control.Concurrent
--
-- printWithTid s = do
--     threadDelay 500
--     tid <- myThreadId
--     putStrLn (show tid ++ ": " ++ s)
--
-- main = runStreaming $ parallely $ do
--     liftIO $ hSetBuffering stdout LineBuffering
--     sz <- sizes
--     cl <- colors
--     sh <- shapes
--     liftIO $ putStrLn $ show (sz, cl, sh)
--
--     where
--
--     toStream xs = foldMapWith (<>) return xs
--     sizes  = toStream [1, 2, 3]
--     colors = toStream ["red", "green", "blue"]
--     shapes = toStream ["triangle", "square", "circle"]
-- @

-- $folding
-- Folds

-- $zipping
-- Applicative composition

-- $concurrent
--
-- There are two ways to achieve concurrency. We can generate individual
-- elements of a stream concurrently by folding with parallel composition
-- operators i.e.  '<|' or '<|>'. 'forEachWith' can be useful in such cases.
--
-- In the following example, we square each number concurrently but then
-- sum and print them serially:
--
-- @
-- import Streamly
-- import Data.List (sum)
--
-- main = do
--     squares \<- toList $ serially $ foldMapWith (<|) (\x -\> return $ x * x) [1..100]
--     print $ sum squares
-- @
--
-- The following example not just computes the squares concurrently but also
-- computes the square root of their sums concurrently by using the parallel
-- monadic bind.
--
-- @
-- import Streamly
-- import Data.List (sum)
--
-- main = do
--     z \<- toList $ asyncly $ foldMapWith (\<|) (\\x -> return $ x * x) [1..100] >>= \\xsq ->
--         foldMapWith (\<|) (\\x -> return $ x * x) [1..100] >>= \\ysq ->
--         return $ sqrt (xsq + ysq)
--     print $ sum z
-- @

-- $reactive
--
-- Let us see a reactive programming example:
--
-- @
-- import Streamly
-- import Control.Concurrent (threadDelay)
-- import Control.Monad (when)
-- import Control.Monad.State
-- import Data.Semigroup (cycle1)
--
-- data Event = Harm Int | Heal Int | Quit deriving (Show)
--
-- userAction :: MonadIO m => AsyncT m Event
-- userAction = cycle1 $ liftIO askUser
--     where
--     askUser = do
--         command <- getLine
--         case command of
--             "potion" -> return (Heal 10)
--             "quit"   -> return  Quit
--             _        -> putStrLn "What?" >> askUser
--
-- acidRain :: MonadIO m => AsyncT m Event
-- acidRain = cycle1 $ liftIO (threadDelay 1000000) >> return (Harm 1)
--
-- game :: (MonadAsync m, MonadState Int m) => AsyncT m ()
-- game = do
--     event \<- userAction \<|> acidRain
--     case event of
--         Harm n -> modify $ \h -> h - n
--         Heal n -> modify $ \h -> h + n
--         Quit   -> fail "quit"
--
--     h <- get
--     when (h <= 0) $ fail "You die!"
--     liftIO $ putStrLn $ "Health = " ++ show h
--
-- main = do
--     putStrLn "Your health is deteriorating due to acid rain,\
--              \ type \"potion\" or \"quit\""
--     runStateT (runStreaming $ serially $ game) 60
-- @

-- $statemachine
-- State machine stuff

-- $comparison
--
-- Even though streamly covers all that is provided by the 'async' package or
-- most of what is provided by the 'streaming', 'pipes' or 'conduit' packages,
-- I would not say that it renders those useless. Streamly is like monad if
-- 'async' is applicative and monads and applicatives both have their use
-- cases. It can completely repalce 'async', the ZipAsync type is equivalent to
-- the functionality provided by 'async'.
--
-- pipes and conduit are like Arrows and streamly is like monad to them. You
-- would use pipes and conduit when you do not need the product style
-- composition and the implicit concurrency.
