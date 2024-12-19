#include <sys/stat.h>

int stat_is_directory(const char *path) {
    struct stat statbuf;

    // XXX Should use lstat instead for correctness.
    // XXX Using fstatat with a dirfd and relative path would be faster.
    // Call stat to get the file status
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
