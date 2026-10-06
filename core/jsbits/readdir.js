// Support for Streamly.Internal.Syscall.Posix.ReadDir with the GHC 9.10
// JavaScript backend.

// The runtime returns a node fs.Dirent object as the struct dirent pointer.
// Returns 1 for a directory, 2 for a symbolic link, 3 for a regular file and
// 0 otherwise.
function h$streamly_dirent_type(d, d_off) {
  if (d.isDirectory()) return 1;
  if (d.isSymbolicLink()) return 2;
  if (d.isFile()) return 3;
  return 0;
}

// The runtime provides lstat but not stat.
function h$stat(file, file_off, stat, stat_off) {
  if (h$isNode()) {
    try {
      var fs = h$fs.statSync(h$decodeUtf8z(file, file_off));
      h$base_fillStat(fs, stat, stat_off);
      return 0;
    } catch (e) {
      h$setErrno(e);
      return -1;
    }
  } else
    return h$unsupported(-1);
}
