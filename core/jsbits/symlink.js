// The GHC JavaScript runtime does not provide symlink, which the unix package
// uses for createSymbolicLink, e.g. createFileLink and createDirectoryLink of
// the directory package fail without it.
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
