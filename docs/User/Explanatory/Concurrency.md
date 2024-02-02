Concurency Conjecture: A computation can be evaluated concurrently if and
only if we can express it as a sequence of indepndent monadic computations.
The generation of any action should not depend on the result of a past
action.

Even if the next step apparently depends on the previous step it is often
possible to parallelize the computation by making each step an incremental
computation. For example if we are adding numbers from 1 to 1000, at each
step we need to add the sum from previous step. However, if we make each
step a function that adds something to a previous value, irrespective of
whatever value that is, then we can make each step independent of the
previous step. Thus each step can be evaluated independently. Basically
internal parts of the computation can be evaluated concurrently and then
remaining part where the result is combined with other results can be done
sequentially. So the problem reduces to generating functions instead of
concrete results, and then we can combine the functions together. Chunking
of a problem becomes creation of such independent functions (map) and then
combine the resulting functions (reduce).

Each concurrency problem can be modeled as:

* Unfold i.e. chunking - a serial step
* Map i.e. evaluation - a concurrent step
* Fold i.e. reduce - a serial step

If we see unfoldr step is (b -> (a, b)), therefore it is inherently serial
because the next step depends on the result of the previous. A map is (a ->
b), stateless, inherently parallel, a fold (b -> a -> b), again inherently
serial.

If we can independently combine two values using a -> a -> a, we can keep on
doing this recursively and reduce the any number of values to a single
value. Then the resulting values can also be reduced like that until we
reach the final value. Thus we get a tree of dependencies in which leaves at
the same level can be evaluated concurrently. Therefore concurrent
evaluation can be modeled as a Monoid.

In general, instead of a Monoid we can use a fold (b -> a -> b) to
accumulate a number of a's to b.

Let us take an example of adding n numbers.

* Unfold - generate ranges (a,a)
* Map - add the ranges using (a -> a -> a)
* Fold - add the sums using (a -> a -> a)
