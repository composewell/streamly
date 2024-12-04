#include <string.h>

// Find the char "c" starting from "dst+off" and up to "len" chars from
// it, returns the index of the character found, considering dst + off
// as the base pointer. If index is greater than or equal to len the
// char is not found.
size_t memchr_index(const void *dst, size_t off, int c, size_t len) {
    void *p = memchr ((char *)dst + off, c, len);

    if (p) {
        return (size_t) (p - dst - off);
    } else {
        return len;
    }
}

int memcmp_index(const void *p1, size_t p1_off, const void *p2, size_t p2_off, size_t len) {
    int cmp = memcmp((char *)p1 + p1_off, (char *)p2 + p2_off, len);
    return cmp;
}
