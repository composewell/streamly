// Writes seconds and nanoseconds as two 32-bit ints, matching the JS
// Storable TimeSpec instance in Streamly.Internal.Data.Time.TimeSpec.
function h$clock_gettime_js(when, p_d, p_o) {
  var o  = p_o >> 2,
      t  = Date.now(),
      tf = Math.floor(t / 1000),
      tn = 1000000 * (t - (1000 * tf));
  p_d.i3[o]   = tf|0;
  p_d.i3[o+1] = tn|0;
  return 0;
}
/* Hack! Supporting code for "clock" package
 * "hspec" depends on clock.
 */
function h$hs_clock_darwin_gettime(when, p_d, p_o) {
      h$clock_gettime_js(when, p_d, p_o);
}
