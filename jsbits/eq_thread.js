// The Eq ThreadId instance in base calls eq_thread, the JavaScript runtime of
// GHC up to 9.14 does not provide it. Same as the definition in GHC master.
function h$eq_thread(t1, t2) {
  return t1 === t2 ? 1 : 0;
}
