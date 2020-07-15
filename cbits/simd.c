#include "simd.h"

bool mm_movemask_word8X16(uint8_t const* mem_addr) {
  __m128i loaded = _mm_loadu_si128((__m128i*) mem_addr);
  return _mm_movemask_epi8(loaded) == 0 ? true: false;
}

/*
bool mm_movemask_word8X32(uint8_t const* mem_addr) {
  __m256i loaded;
  bool result = true;

  while(strlen(mem_addr) >= 256) {
    loaded = _mm256_loadu_si256((__m256i*) mem_addr);
    result = result && (_mm256_movemask_epi8(loaded) == 0 ? true: false);
    mem_addr += 255;
  }
}
*/

int movemask_fail_at_word8X32(uint8_t const* mem_addr, size_t len) {
  size_t aligned_len = (len / 32) * 32;
  size_t remaining_len = len - aligned_len;
  size_t i = 0;
  bool failed = false;
  __m256i loaded;

  while (i < aligned_len && failed == false) {
    loaded = _mm256_loadu_si256((__m256i*) mem_addr + i);
    failed = failed || _mm256_movemask_epi8(loaded);
    i += 32;
  }

  if (failed == true) {
    i = i - 32;
    remaining_len = len - i;
    failed = false;
    //    return (i - 32);
  }

  while(remaining_len > 0 && failed == false) {
   failed = failed || mem_addr[i] >> 7;
   i += 1;
   remaining_len -= 1;
  }

  if (failed == true) {
    i -= 1;
  }

  return i;
}

void mm_copy_word8X32(uint8_t const* src_addr, uint8_t* dst_addr, size_t len) {
  size_t aligned_len    = (len / 32) * 32;
  size_t remaining_len  = len - aligned_len;

  size_t i;

  for (i = 0; i < aligned_len; i += 32) {
    __m128i v0 = *(__m128i*)(src_addr + i     );
    __m128i v1 = *(__m128i*)(src_addr + i + 16);

    *(__m128i*)(dst_addr + i      ) = v0;
    *(__m128i*)(dst_addr + i + 16 ) = v1;
  }

  memcpy(dst_addr + aligned_len, src_addr + aligned_len, remaining_len);
}
