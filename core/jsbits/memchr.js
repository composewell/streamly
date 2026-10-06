// JavaScript version of memchr_index in src/Streamly/Internal/Data/MutArray/Lib.c.
// Find the byte "c" in the "len" bytes starting at "dst + off", return its
// index relative to "dst + off", or "len" if it is not found.
function h$memchr_index(dst, dst_off, off, c, len) {
  var start = dst_off + off;
  var i = dst.u8.subarray(start, start + len).indexOf(c);
  return i < 0 ? len : i;
}
