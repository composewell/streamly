# JSON

JSON parsing.

Anything inside {} is an object, "" act as escaping quotes inside {}. Nested
{} are independent nested objects with exactly the same semantics. The outer
object ends when a balanced "}" is found.

The data inside {} is a list separated by ",", again "" acts as escaping
quotes for "," as well.

Each token in the comma separated list is a key value pair separated by ":",
again "" acts as a escaping quote for ":".

Keys must be strings.

Values can be:

* strings
* booleans
* numbers
* objects

Whitespace is insignificant except inside "".

# Serialization with schema

Streamly binary serialization with encodeConstrNames and encodeRecordFields
options. Also see https://hackage.haskell.org/package/winery which serializes
the schema along with data.

Nested stream serialization
---------------------------

For example, how to stream cells in rows in worksheets of an excel sheet.
Or how to send a stream of pages in a web site server pager.
We need to basically have nested framing.

In the program we frame using constructors. We form a nested tree of
constructor inside a constructor.

On the network or in a stream we have two ways of framing:
    - use a trailing marker to end a frame and escape the marker inside the frame
    - use a length header to tell how much more to come in the frame

When we have to send streams of unknown length then marker based framing is the
only option. For example, we can use a base64 encoding and use a newline based
framing.

For nested framing:
    - We will need multiple markers which becomes complicated as markers need
      to be escaped.
    - Use a frame type and length in the header. Then we reassemble
      similar frames together. There could be errors if there are out of place
      frames with wrong nesting order. Common example is utf-8.

Examples:
    - C strings are marker based framing.
    - Arrays are length based framing
    - utf-8 is length based nested framing
    - paragraphs, lines, words (nested)

Marker based framing
--------------------

* The simplest is to use a single element frame marker e.g. "\n".
* There could also be multiple markers e.g. whitespace to separate words

Marker based framing is natural for humans. that's why we have paragraphs,
lines, words as marker based framing.

    - whitespace other than \n or \n\n => words
    - \n => lines
    - \n\n => para
    - \n\n\n => page?

Note that there is no escaping in this case. In general, if we choose a marker
element with no escaping its easy to parse. We can use the number of markers to
denote the nesting level. This can be generalized into an API. We can use SIMD
to expedite parsing.

When we have escaping nested levels can become too complicated. We can have a
single level framing with escaping. This is shell quoting, Json string quoting
or string quuoting in general.

Length Based framing
--------------------

* Arrays
* representing ranges using arrays i.e. a tree of arrays
    - the top level tree could have the full range
    - then segments inside it

Basically trees can be streamed using nested framing.

A directory tree is a good example of nested framing.

Each frame could have a different semantic meaning when we have different types
of elements inside the frames e.g. frames of type A, nested in frames of type B
nested in frames of type C. Or we can have frames recursively nested i.e.
each nested frame is of the same type in which it is nested e.g. in case of
directory trees, only the leaf frame is different, this would be type B
inside type A inside A inside A.

When we have to stream a directory tree, we can:
    - use the directory path as the frame identifier in the header
    - the frames could be chunked arrays of filenames ( but this would be BFS
      serialization)

    - For DFS serialization, we can create a dictionary of the directory names
      and represent the path using indices that map to the directory names.

Automatic nested stream serialization
-------------------------------------

when the frames are very small the overhead of header based framing could be
high and needs to be optimized. One example is utf-8. Another way to frame in
this situation is to use a marker e.g. c strings.

So if we use header based framing we should keep frames as large as possible.

Goals:
    - frame header should be minimal overhead

String literal framing
----------------------

String literals can be framed in two ways.
    - using the C style frame marker (null char) based framing
    - bytearray literals to store the length along with the string

Statically Sized Arrays
-----------------------

We can use the length based framing efficiently when we know the size
statically. We do not need to store the length, just implicitly assume it. We
can statically check whether the indices into the array are valid statically
unless the indices are themselves generated dynamically.
