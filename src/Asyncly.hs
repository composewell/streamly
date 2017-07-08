-- |
-- Module      : Asyncly
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : MIT-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--

-- | Asyncly allows writing and composing asynchronous event driven
-- applications conveniently. It can be thought of as a non-deterministic
-- continuation monad or a combination of ContT and ListT. It allows to capture
-- the state of the application at any point and trigger arbitrary number of
-- continuations from the capture point. Non-determinism or parallel
-- continuations are introduced using the <|> operator. It provides a
-- convenient way to implement and compose state machines without using
-- callbacks. An event in the state machine corresponds to a continuation.
-- There are no cycles in the state machine as each transition in the state
-- machine is an independent instance of the state machine. It is an immutable
-- state machine!

module Asyncly
    ( AsyncT
    , MonadAsync
    , wait
    , wait_
    , threads

    , async
    , makeAsync
    , each
    , gather

    , Log
    , Loggable
    , waitLogged
    , waitLogged_
    , logged
    , suspend
    , withLog
    , eachWithLog
    )
where

import Asyncly.AsyncT
import Asyncly.Threads
