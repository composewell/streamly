#ifndef __TEST_H
#define __TEST_H

#include <immintrin.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>

bool mm_movemask_word8X16(uint8_t const*);
int movemask_fail_at_word8X32(uint8_t const* mem_addr, size_t len);
void mm_copy_word8X32(uint8_t const* src_addr, uint8_t* dst_addr, size_t len);

#endif
