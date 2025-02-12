#include <sys/stat.h>

int lstat_is_directory(const char *path) {
    struct stat statbuf;

    // XXX Using fstatat with a dirfd and relative path would be faster.
    // Call lstat to get the file status
    // We use lstat instead of stat for correctness with symbolic link
    if (lstat(path, &statbuf) == 0) {
        // Check if the file is a directory using S_ISDIR macro
        if (S_ISDIR(statbuf.st_mode)) {
            return 1; // It is a directory
        } else {
            return 0; // Not a directory
        }
    }
    return -1;  // An error occurred (stat failed)
}

int stat_is_directory(const char *path) {
    struct stat statbuf;

    if (stat(path, &statbuf) == 0) {
        // Check if the file is a directory using S_ISDIR macro
        if (S_ISDIR(statbuf.st_mode)) {
            return 1; // It is a directory
        } else {
            return 0; // Not a directory
        }
    }
    return -1;  // An error occurred (stat failed)
}
