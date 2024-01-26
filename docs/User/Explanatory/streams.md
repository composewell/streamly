# Stream Fusion

The fused 'Stream' type employs stream fusion for C-like performance when
looping over data. It represents the stream as a state machine using an
explicit state, and a step function working on the state. A typical stream
operation consumes elements from the previous state machine in a stream
pipeline, transforms the elements and yields new values for the next stage to
consume. One stream operation typically represents one stage of a modular
pipeline, representing a single task, stages in the pipeline have no knowledge
of the state of the previous or next stage.

A typical stream pipeline consists of a stream producer, several stream
transformation operations and a stream consumer. All these operations taken
together form a closed loop (like a for or while loop) processing the stream
elements. Elements are transferred between stages using a boxed data
constructor. However, all the stages of the pipeline are "fused" together by
GHC, eliminating the boxing of intermediate constructors, and thus forming a
tight C like loop without any boxed data being used in the loop.

Stream fusion works effectively when:

* the stream pipeline is composed statically (known at compile time)
* all the operations forming the loop are inlined
* the loop is not recursively defined, recursion breaks inlining

If these conditions cannot be met, the CPS style stream type 'StreamK' may
turn out to be a better choice than the fused stream type 'Stream'.

## Stream vs StreamK

The fused stream model avoids constructor allocations and function call
overheads. However, the stream is represented as a state machine, and to
generate stream elements it has to navigate the decision tree of the state
machine. Moreover, the state machine is cranked for each element in the stream.
This performs extremely well when the number of states are limited. The state
machine starts getting expensive as the number of states increase. For example,
generating a stream from a list requires a single state and is very efficient,
even if it has millions of elements, there is only one state machine. However,
using 'cons' to construct a million element stream would be a disaster because
it is statically fusing a million state machines.

A typical worst case scenario for fused stream model is a large number of
`cons` or `append` operations. A few static `cons` or `append` operations
are very fast, much faster than a CPS style stream because CPS involves a
function call for each element whereas fused stream involves a few
conditional branches in the state machine. However, constructing a large
stream using `cons` introduces as many states in the state machine as the
number of elements. If we compose `cons` as a balanced binary tree it will
take @n * log n@ time to navigate the tree, and @n * n@ if it is a right
associative composition.

Operations like 'cons' or 'append' are typically recursively called to
construct a lazy infinite stream. For such use cases the CPS style 'StreamK'
should be used. CPS streams do not use an explicit state machine that needs to be
cranked for each element, past state has no effect on the future element
processing. However, CPS incurs a function call overhead for each element
processed, the overhead could be large compared to a fused state machine
even if it has many states. However, because of its linear performance
characteristics, after a certain threshold of stream compositions the CPS
stream would perform much better than the quadratic fused stream operations.

As a general guideline, you need to use 'StreamK' when you have to use
'cons', 'append' or other operations having quadratic complexity at a large
scale. Typically, in such cases you need to compose the stream recursively,
by calling an operation in a loop. The decision to compose the stream is
taken at run time rather than statically at compile time.

Typically you would compose a 'StreamK' of chunks of data so that the StreamK
function call overhead is mitigated, and then process each chunk using 'Stream'
type operations by using statically fused stream pipeline operations on the
chunks.

'Stream' and 'StreamK' types can be interconverted. See
"Streamly.Data.StreamK" module for conversion operations.

## Fold Fusion

Folds support stream fusion for generating loops comparable to the speed of
C. However, it has some limitations. For fusion to work, the folds must be
inlined, folds must be statically known and not generated dynamically, folds
should not be passed recursively.

Another limitation is due to the quadratic complexity causing slowdown when
too many nested compositions are used. Especially, the performance of the
Applicative instance and splitting operations (e.g. 'splitWith') degrades
quadratically (O(n^2)) when combined @n@ times, roughly 8 or less sequenced
operations are fine. For these cases folds can be converted to parsers and
then used as ParserK.
