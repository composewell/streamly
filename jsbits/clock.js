/* Hack! Supporting code for "clock" package
 * "hspec" depends on clock.
 */
function h$hs_clock_darwin_gettime(when, p_d, p_o) {
      h$clock_gettime(when, p_d, p_o);
}
