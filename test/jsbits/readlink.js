// The GHC 9.10 JavaScript backend does not provide readlink, which the unix
// package uses for readSymbolicLink. hspec reaches it through
// canonicalizePath when it looks for its config files, so every hspec
// program fails at startup without it.
function h$readlink(path, path_off, buf, buf_off, buf_size) {
  if (h$isNode()) {
    try {
      var target =
        h$encodeUtf8(h$fs.readlinkSync(h$decodeUtf8z(path, path_off)));
      // h$encodeUtf8 adds a terminating NUL, readlink does not
      var len = Math.min(target.len - 1, buf_size);
      h$copyMutableByteArray(target, 0, buf, buf_off, len);
      return len;
    } catch (e) {
      h$setErrno(e);
      return -1;
    }
  } else
    h$unsupported(-1);
}
