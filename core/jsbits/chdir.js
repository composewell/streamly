// The GHC 9.10 JavaScript backend does not provide chdir, setCwd in
// Streamly.Internal.Syscall.Posix uses it.
function h$chdir(path, path_off) {
  if (h$isNode()) {
    try {
      process.chdir(h$decodeUtf8z(path, path_off));
      return 0;
    } catch (e) {
      h$setErrno(e);
      return -1;
    }
  } else
    h$unsupported(-1);
}
