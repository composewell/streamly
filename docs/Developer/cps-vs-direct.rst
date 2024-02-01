Structured Loops (Direct Style)
===============================

::

  -- | Result of taking a single step in a stream
  data Step s a where
    Yield :: a -> s -> Step s a
    Stop  :: Step s a

  -- | Representation of a loop, step and state. Step returns the next value and
  -- the next state. We iterate on the state to keep producing more values.
  data Stream m a = forall s. Stream (s -> m (Step s a)) s

Generating
----------

::

  nil :: Monad m => Stream m a
  nil = Stream (const (return Stop)) ()

  -- | A single value
  fromPure :: Applicative m => a -> Stream m a
  fromPure x = Stream step True

      where

      step True  = return $ Yield x False
      step False = return $ Stop

  fromList :: Applicative m => [a] -> Stream m a
  fromList = Stream step

      where

      step _ (x:xs) = pure $ Yield x xs
      step _ []     = pure Stop

Eliminating
-----------

::

  foldrM :: Monad m => (a -> m b -> m b) -> m b -> Stream m a -> m b
  foldrM f z (Stream step state) = go state

      where

      go st = do
            r <- step st
            case r of
              Yield x s -> f x (go s)
              Stop      -> z

In the above code the state is explicit and being threaded around in a
recursive loop. The state from the previous iteration is to be consumed
by the next iteration to generate the next value.  This mandates a
closed recursive loop. This is straightforward translation of imperative
loops to functional paradigm.

Transforming Loops
------------------

::

  mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
  mapM f (Stream step state) = Stream step' state

      where

      step' st = do
          r <- step st
          case r of
              Yield x s -> f x >>= \a -> return $ Yield a s
              Stop      -> return Stop

State wrapping
--------------

Some operations like "drop" (uniq, intersperse, deleteBy, insertBy) may
have to introduce a branch in the code by wrapping the state in another
layer::

  drop :: Monad m => Int -> Stream m a -> Stream m a
  drop n (Stream step state) = Stream step' (state, Just n)

      where

      step' (st, Just i)
        | i > 0 = do
            r <- step st
            return $
              case r of
                Yield _ s -> step' (s, Just (i - 1))
                Stop      -> Stop
        | otherwise = step' (st, Nothing)

      step' (st, Nothing) = do
        r <- step st
        return $
          case r of
            Yield x s -> Yield x (s, Nothing)
            Stop      -> Stop

Note that the branch is always checked for every element in the stream.
It is still quite efficient because the code fuses.

We should use rewrite rules to fuse such consecutive operations together as
they introduce branching which could be costly.

Composing Loops
---------------

Composing two independent loops together is not scalable, we need
to create a wrapper state, wrapping the state of the old stream:

::

  cons :: Monad m => m a -> Stream m a -> Stream m a
  cons m (Stream step state) = Stream step1 Nothing

      where

      step1 Nothing   = m >>= \x -> return $ Yield x (Just state)
      step1 (Just st) = do
          r <- step st
          return $
            case r of
              Yield a s -> Yield a (Just s)
              Stop      -> Stop

As we keep consing we keep creating more layers wrapping the state in
``Maybe``. These layers need to be traversed every time we run the step
function of the composed stream. In the above example ``step1`` needs
to always branch on ``Nothing/Just`` to reach to the wrapped stream. More
layers we add the more branching needs to occur to generate an element
of the stream. If we have ``n`` "cons" operations we need to go through:

* 1 branch at the top level to generate the first element
* 2 branches to generate the next element
* 3 branches to generate the third element
* n branches to generate the nth element

The total number of branches that we need to take is: ``1 + 2 + 3 ... n`` i.e.
``n * (n + 1)/2 = O(n^2)`` where ``n`` is the number of cons operations.

Conceptually, to avoid the introduction of a branch we could use a
mutable step function and state to modify step1/state after yielding
the first element. The next time we call it, it would be a different
function that i.e. "step" and its state "st". However, that would introduce
an indirection and mutability. There is a better way to do it with
immutability i.e. CPS.

CPS representation
==================

In the direct representation we represented a stream using a step and a
state. This model requires us to iterate the step on the state creating
an explicit loop. The state machine implemented by the step function is
incrementally modified by adding new layers in the state which introduce
branches to be traversed every time we go through the loop.

::

  newtype Stream m a = Stream
      { runStream :: forall r. (a -> Stream m a -> m r) -> m r -> m r }

Here we represent the stream as a single function.  Instead, the
function is provided with functions to be called next.

Notice, the function does not have to be called again and again to
iterate on a state for generating new values.  Therefore, there is no
closed recursion. There is no explicit loop.

We (the current ``runStream`` function) can choose which one of the supplied
functions (continuations) to call next.  If we decide to terminate
the stream execution we call the "stop" continuation. If we decide to
generate a value we call the "yield" continuation.

A stream execution is composed of a progression of such continuations
until one of those decides to call the stop continuation.  It is a
composition of functions, a tree of functions composed together.

Yield continuation
------------------

The yield continuation is provided with the generated value "a" and the
"Stream m a", the function representing the rest of the stream. Notice
that the stream function is done with one shot execution, there is no
closed loop or recursion, the future execution of the stream is the
responsibility of the continuation.

The continuation consumes the element "a" and then proceeds to call
"Stream m a" using a "yield" and "stop" continuation. By modifying the
"yield" and "stop" continuations that it passes to call "Stream m a", it
can control the execution of the stream.

Generating
----------

::

  nil :: Stream m a
  nil = Stream $ \_ stp -> stp

  fromPure :: a -> Stream m a
  fromPure a = Stream $ \yield _ -> yield a nil

  cons :: a -> Stream m a -> Stream m a
  cons a r = Stream $ \yld _ -> yld a r

  fromList :: [a] -> Stream m a
  fromList = Prelude.foldr cons nil

Eliminating
-----------

::

  foldrM :: (a -> m b -> m b) -> m b -> Stream m a -> m b
  foldrM step acc m = go m
      where
      go m1 =
          let stop = acc
              yieldk a r = step a (go r)
          in runStream yieldk stop m1

Note that unlike in direct style fold, there is no generator state being
threaded around here instead the function yielded by the continuation is
being executed.

Transforming
------------

::

  map :: (a -> b) -> Stream m a -> Stream m b
  map f = go

      where

      go m1 =
          Stream $ \yld stp ->
            let yieldk a r = yld (f a) (go r)
            in runStream yieldk stp m1

In direct style we had to examine the constructors to determine the
current state and execute code based on that. Here, we have to make
the next function call at each step. The former is much more efficient
because the compiler can optimize well to remove the constructors and
generate code with direct branches not involving the constructors. On
the other hand placing a function call is costlier. Though in some cases
it can be avoided by using foldr/build fusion but not always.

goto
----

::

  drop :: Int -> Stream m a -> Stream m a
  drop n = Stream $ go n

      where

      go n1 m1 =
        Stream $ \yld stp ->
          let yieldk _ r = runStream yld stp $ go (n1 - 1) r
          in if n1 <= 0
             then runStream yld stp m1
             else runStream yieldk stp m1

This shows a crucial difference between direct style and CPS. In direct
style we have to always check for the branch to determine if we are
dropping the elements or consuming. In this case we can see that once
we have taken the "then" path we never have to check the condition "n1
<= 0", it is out of the way.

Basically CPS provides us the ability to take an exit path and forget
about the past code forever. So if we have a million "drop" composed
together CPS would have no problem, after the final drop there won't be
any branches in the way whereas direct style would introduce a million
branches to be traversed forever.

Combining Streams
-----------------

::

  cons :: a -> Stream m a -> Stream m a
  cons a r = Stream $ \yld _ -> yld a r

Unlike in direct style representation, the performance of stream
generation is independent of the number of "cons" operations. There is
no quadratic complexity, we simply call the next continuation at each
step.

Similarly appending streams is independent of number of appends.

::

  serial :: Stream m a -> Stream m a -> Stream m a
  serial m1 m2 = go m1

      where

      go m =
        Stream $ \yld stp ->
           let stop       = runStream yld stp m2
               yieldk a r = yld a (go r)
           in runStream yieldk stop m

Interleaved production and consumption
--------------------------------------

In the direct style we have an explicit stream generator. A consumer
generates values using the generator and consumes them. In CPS the
consumer and generator are interleaved. The ``runStream`` function is a
generator and the "yield" continuation is a consumer. The consumer then
calls the generator again and so on.

Loop vs goto
------------

The direct style representation is like a structured loop with well
defined exit points. Whereas the CPS representation can exit from
anywhere. Therefore, exception handling and resource management in
direct style is much simpler to implement.

CPS vs Direct Style
-------------------

We should say goto vs structured loops. In structured looping we always create
frame and we return back to the point where we called the function from or
where we entered the loop. CPS is like goto, it has the powerful ability to jum
to arbitrary locations, it is like longjmp in C.

StreamD is like strcutured loops and StreamK is like goto. In streamD we cannot
use cons too many times as each one creates a new layer on top. Whereas with
StreamK we can do as many cons because we do not need to remember where we need
to return, it is a one way street.

With CPS we get this powerful ability to modify code at runtime. We can take a
branch once to enter a different code which will forget about that branch. For
example, in runOne in Parallel stream the worker can choose the runOne or
runOneLimited function just once and we do not have to pay for checking that
condition again and again. We can change the course of the program without
having to look back.

Open vs Closed Iteration
------------------------

In StreamK or CPS the state is "open" i.e. we pass the state explicitly and
there is no hidden state inside a computation. State is passed from one
computation to the next. On the other hand in the direct style the state is
"closed" or hidden inside the computation and it is not visible outside it.

Both are state machines but one is closed state and the other is open state.
This is fundamental difference in CPS and direct computations. This is also the
difference between IO and ST monads. In fact, the state that we use in the
StreamK can be replaced by an IORef in IO.

Direct style can easily implement exception operations like
before/after/bracket without leaking the state or without requiring an external
state.  There is already an enclosed state in any scope and the control is
guaranteed to come back to us after we yield an element. The state is visible
only in our scope. We nest computations like closed nested egg shells i.e a
closed world.

However, bracket is not possible in CPS without requiring an external state
to be passed to the computation and a possibly modified state yielded on each
yield. Also, in this case since each computation needs to identify its own
state in that global state passed around, it will have to store the state using
a signature (like in transient it uses the type to index in a map). In this
case the control is passed to the next computation and we can never guarantee
that control will ever come back to us. Therefore, if any cleanup has to be
done then it has to be recorded in the state that we are passing to the next
computation and someone has to do the cleanup for everyone else in case an
exception occurs. This is like inside out nested eggshells i.e. open world.

The key difference in Direct vs CPS is that in direct control is guaranteed to
come back to us whereas in CPS control is free to go anywhere. Direct is like
structured for loop and CPS is like unbridled goto.

CPS is like mealy machine and direct style is like moore machine.

* CPS: Control is free. We yield a value and a continuation to be called. We
  can chain another computation which can process the value and may or may not
  call the continuation when it wants.  We can yield multiple continuations.
  For example, one for a case when we want to continue to yield values and
  another one for the case when we want to stop or cleanup.
* Direct: Control is restricted. We yield a value and an opaque state, another
  computation can be chained to the output and then returning back to the
  parent state machine. Similar to multiple continuations in CPS style we can
  have multiple types of values yielded in direct, one case for a normal value
  and another case for an exception value (Error constructor).

  In CPS there are no layers of state added when we call a nested computation,
  whereas in Direct another state layer gets added. Therefore unbridled nesting
  is not possible in direct whereas it is possible in CPS. In fact, CPS is not
  nesting, it is an open iteration to the next step.

In case of CPS if we yield a cleanup continuation then we have to compose all
the cleanup continuations from the past. This cannot go on indefinitely, so we
need a guarantee for termination or when we loop back we need to remove the old
cleanup continuation for the same guy and replace it with new one. For this
reason it is better to use Direct style for exception handling where we are
guaranteed to have structured layers of cleanup states.

Understanding The Stream Type
-----------------------------

We can first start with a direct style representation so that we can understand
it better. First explain incrementally building a direct style stream and then
come to the function composition based representation. In the end we can tie
the two (StreamK and StreamD for best performance).

The simplest type would be:
newtype Stream m a = Stream {
    runStream :: (forall r.
            -> (a -> m r) -- yield
            -> m r
            )
  }

This is the codensity type. It represents just a collection of one or more "a"
elements.  This is a functor and a Monad. We can run it by passing a yield
continuation to lower it to the monad m::
  runStream s yield :: m r

We can append two streams. Particularly, left and right associated appends
behave in the same way.

append s1 s2 = Stream $ \yld ->
  runStream s1 (\a -> yld a >> runStream s2 yld)

So we can get a Semigroup instance with this type but not a Monoid as we do not
have an identity element.  We cannot represent a nil stream with this type.
Also, if we create a product of streams i.e. a list-t monad with this type we
have no way to stop an iteration in the middle.

But we cannot cons an element at the head of the stream. It is a pre-determined
stream. We can only run and run all of it as we cannot deconstruct a single
element as well.

However, to implement a cons we can wrap an element into a stream and use
"append" to add it to an existing stream. But there is no way to implement an
uncons though.

If we add a stop continution to this type. Now we get our Identity and the
Monoid instance as well.

newtype Stream m a = Stream {
    runStream :: (forall r.
            -> m r        -- stop
            -> (a -> m r) -- yield
            -> m r
            )
  }

This is the logict type.  However, this type does not allow incremental
addition or deconstruction (cons and uncons). At each yield step we can inspect
the element "a" but the rest of the stream is opaque.

Now we can construct a stream by wrapping a yield and stop in one segement and
appending all those segments. Now we get the ability to do something at each
stop. Therefore we can fold or traverse the stream.

However, we can uncons a cons implemented with "append" if we use a free
construction instead of a rigid one to append. We will have to interpret the
Cons as an append.

Stream m a = Stream (forall r. (a -> m r) > m r)
  | Cons (Stream m a) (Stream m a)

With a slight modification we can add the ability to cons and uncons:

newtype Stream m a =
    MkStream (forall r.
            -> m r                      -- stop
            -> (a -> Stream m a -> m r) -- yield
            -> m r
            )

Here we added a Functorial layer at each step. We can inspect the element as
well as the rest of the stream.

This is a variant of codensity, with an identity element (stop) and the ability
to construct and deconstruct incrementally using a cons operation, therefore,
we have (a -> Stream m a -> m r) instead of (a -> m r). (a -> m r) allows
continuous yielding of values, whereas (a -> Stream m a -> m r) allows yielding
and the rest of the stream, so we can stop and compute at each yield.

We can add a new element at the head of the stream:
  cons x s = Stream $ \yld ->
    let yield a = f a >> y
    runStream s yield


newtype Stream m a =
    MkStream (forall r.
               State Stream m a         -- state
            -> m r                      -- stop
            -> m r                      -- skip
            -> (a -> Stream m a -> m r) -- yield
            -> m r
            )
