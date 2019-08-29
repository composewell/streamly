{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification          #-}
{-# LANGUAGE FlexibleContexts                   #-}

-- |
-- Module      : Streamly.StreamDK.Type
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- A CPS style stream using a constructor based representation instead of a
-- function based representation.
--
-- StreamD is a non-recursive data type in which the state of the stream and
-- the step function are separate. When the step function is called, a stream
-- element and the new stream state is yielded. The generated element and the
-- state are passed to the next consumer in the loop. The state is threaded
-- around in the loop until control returns back to the original step function
-- to run the next step. This creates a closed loop representation with state
-- of each step being hidden/abstracted or existential within that step. This
-- creates a loop reprentation identical to the for an while loop constructs in
-- imperative languages, the states of the steps combined together constitutes
-- the state of the loop iteration.
--
-- Because of a closed representation consing too many elements in this type of
-- stream does not scale. Each cons creates a layer that needs to return the
-- control back to the caller. However, this representation can be optimized
-- very efficiently by the compiler because the state is explicitly separated
-- from the functions, represented using pure data constructors and visible to
-- the compiler, the stream steps can be fused using case-of-case
-- transformations and the state can be specialized using spec-constructor
-- optimization, yielding a C like tight loop/state machine with no
-- constructors and therefore no unnecessary allocation. Because of the closed
-- nature of the stream, exception handling is disciplined in this model
-- because control flow is structured in the loop and cannot be arbitrary.
-- Also, because of the closed nature of the loop, it is not possible to uncons
-- an item from the stream.
--
-- This type is amenable to stream fusion.
--
-- StreamDK i.e. the stream defined in this module, like StreamK, is a
-- recursive data type which has no explicit state, each step yields an element
-- and a computation representing the rest of the stream.  Stream state is part
-- of the function representing the rest of the stream.  This creates an open
-- computation representation, or essentially a continuation passing style
-- computation.  After the stream step is executed, the caller is free to
-- consume the produced element and then send the control wherever it wants,
-- there is no restriction on the control to return back somewhere, the control
-- is free to go anywhere. The caller may decide not to consume the rest of the
-- stream.
--
-- This model allows generating arbitrarily large streams lazily using the cons
-- operation. Addition of more elements does not add any cost. Similarly uncons
-- operation is possible in this stream representation. The control flow can be
-- manipulated arbitrarily, there is no structure dictated by the stream. This
-- allows interoperation/interconversion with other monads, and other stream
-- types. Scalable cons operation and the possibility of uncons operation makes
-- this representation very similar to standard lists. Also, this type
-- facilitates a scalable monadic composition.
--
-- This model has a cost because the open representation requires a constructor
-- which may not always be eliminated.
--
-- This type is amenable to foldr/build fusion.

module Streamly.Streams.StreamDK.Type
    ( Step(..)
    , Stream (..)
    )
where

-- XXX Use Cons and Nil instead of Yield and Stop?
data Step m a = Yield a (Stream m a) | Stop

data Stream m a = Stream (m (Step m a))
