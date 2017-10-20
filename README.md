# Asyncly

[![Build Status](https://travis-ci.org/harendra-kumar/asyncly.svg?branch=master)](https://travis-ci.org/harendra-kumar/asyncly)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/h2bcgw7xa2jxe222?svg=true)](https://ci.appveyor.com/project/harendra-kumar/asyncly)
[![Coverage Status](https://coveralls.io/repos/harendra-kumar/asyncly/badge.svg?branch=master&service=github)](https://coveralls.io/github/harendra-kumar/asyncly?branch=master)

Asyncly is a general streaming and concurrent programming
monad allowing you to write a wide variety of applications ranging from
simple applications utilizing streaming IO to massively parallel concurrent
applications as well as reactive (FRP) applications with the same ease.
Asyncly provides high level concurrency primitives and hides the low level
concurrency details completely from the programmer.  The programmer just
expresses whether a task can run in parallel with another. Threads,
synchronization and concurrency rate control are handled automatically. The
library has been written keeping high performance applications in mind.

Asyncly subsumes all the use cases of list transformer (list-t) and logic
programming (logict) monads and adds concurrency on top. It is a streaming
library and therefore can be used for most of the streaming use cases as well
(i.e. where you would use conduit or pipes) with concurrency added. It is
also a first class reactive programming library and can be used where you
would use libraries like Yampa or reflex. One way to think about it is to
think of it as composable state machines with concurrency. You can write from
really simple to complex applications, from streaming to FRP or massively
concurrent applications using the same simple and concise abstractions.

To get a taste of the simplicity and power of the APIs, here is a full
program to concurrently and recursively list the contents of a directory
tree:

``` haskell
import Path.IO (listDir, getCurrentDir)
import Asyncly

main = runAsyncly $ getCurrentDir >>= readdir
   where readdir d = do
            (dirs, files) <- lift $ listDir d
            liftIO $ mapM_ putStrLn $ map show files
            foldMapWith (<|>) readdir dirs
```

See "Asyncly.Tutorial" and "Asyncly.Examples" for more details.

This library was originally inspired by the `transient`
package authored by Alberto G. Corona.
