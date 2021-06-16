# Monad Transformers

In the stream tutorials we mostly used streams in the IO monad.  In
general, the type `SerialT` is a monad transformer, @SerialT m a@
represents a stream of values of type 'a' in some underlying monad
'm'. For example, @SerialT IO Int@ is a stream of 'Int' in 'IO'
monad. Similarly, `SerialT Identity Int` would be a pure stream
equivalent to `[a]`.

Similarly we have monad transformer types for other stream types as well
viz.  'WSerialT', 'AsyncT', 'WAsyncT' and 'ParallelT'.

To lift a value from an underlying monad in a monad transformer stack into a
singleton stream use 'lift' and to lift from an IO action use 'liftIO'.

```
>>> import Control.Monad.IO.Class (liftIO)
>>> Stream.drain $ liftIO $ putStrLn "Hello world!"
Hello world!

>>> import Control.Monad.Trans.Class (MonadTrans(lift))
>>> Stream.drain $ lift $ putStrLn "Hello world!"
Hello world!
```

## Using Monad Transformers

Common monad transformers can be used with streamly serial streams, without any
issues. `ReaderT` can be used with concurrent streams as well without any
issues.

The semantics of monads other than `ReaderT` with concurrent streams are
not yet finalized and will change in future, therefore, as of now they are not
recommended to be used with concurrent streams.

## Ordering of Monad Transformers

In most cases it is a good idea to keep streamly as the top level monad.
[This
example](https://github.com/composewell/streamly-examples/blob/master/examples/ControlFlow.hs)
demonstrates how various control flow modifying monads can be combined
with streamly stream monads.

## State Sharing
### Serial Applications

Read only global state can always be shared using the `Reader` monad.
Read-write global state can be shared either using an `IORef` in the `Reader`
monad or using the `State` monad.

See `AcidRain.hs` example for a usage of `StateT` in the serially executing
portion of the program.

### Concurrent Applications

The current recommended method for sharing modifiable global state across
concurrent tasks is to put the shared state inside an `IORef` in a `Reader`
monad or just share the `IORef` by passing it to the required functions. The
`IORef` can be updated atomically using `atomicModifyIORef`.

The `CirclingSquare.hs` example shares an `IORef` across parallel tasks.

## See also

* [Examples of control flow monads with Streamly](https://github.com/composewell/streamly-examples/blob/master/examples/ControlFlow.hs)
