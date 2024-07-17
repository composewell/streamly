# StreamK

THIS DOC IS NOT READY.

The 'Serial' type is almost a drop in replacement for pure lists,
pure lists are a special case of monadic streams. If you use 'nil' in place
of '[]' and '|:' in place ':' you can replace a list with a 'Serial' stream.
The only difference is that the elements must be monadic type and to operate
on the streams we must use the corresponding functions from
"Streamly.Prelude" instead of using the base "Prelude".
