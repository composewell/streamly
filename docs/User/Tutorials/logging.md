You can easily do thread-safe logging in a multithreaded program
using streamly. Create a callback and stream pair using `entangled`
or `fromCallback`. Pass around the callback wherever you want to log
something. Any logged events will appear the stream associated with the
callback. Drain the stream to actually log to whichever device you want
to log it to.

Logging to a file, example:

Logging to a ring buffer in memory, example:
