# Resources

* See APL
* https://github.com/google-research/dex-lang another array processing lang

Transpose
---------

interleaving roundRobin n streams would transpose them. To transpose arrays we
can just stream the rows and interleave them and then chunk by column size and
turn the stream into an array of arrays.
