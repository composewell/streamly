// The GHC 9.10 JavaScript backend does not provide symlink, which the unix
// package uses for createSymbolicLink. The DirIO tests create symbolic links
// using createDirectoryLink from the directory package.
function h$symlink(target, target_off, path, path_off) {
  if (h$isNode()) {
    try {
      h$fs.symlinkSync(h$decodeUtf8z(target, target_off),
                       h$decodeUtf8z(path, path_off));
      return 0;
    } catch (e) {
      h$setErrno(e);
      return -1;
    }
  } else
    h$unsupported(-1);
}
