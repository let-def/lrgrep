#include <assert.h>
#include <stdint.h>
#include <caml/mlvalues.h>

static inline uintptr_t pop_count(uintptr_t x)
{
  if (sizeof(x) == 8)
    return __builtin_popcountll(x);
  else if (sizeof(x) == 4)
    return __builtin_popcount(x);
  else
    assert(0);
}

uintptr_t bit_lib_pop_count(uintptr_t v)
{
  return pop_count(v << 1);
}

value bit_lib_pop_count_tagged(uintptr_t x)
{
  return Val_long(pop_count(x >> 1));
}

static inline uintptr_t msb_index(uintptr_t x)
{
  if (sizeof(x) == 8)
    return 62 - __builtin_clzll(x);
  else if (sizeof(x) == 4)
    return 30 - __builtin_clz(x);
  else
    assert(0);
}

uintptr_t bit_lib_msb_index(uintptr_t v)
{
  return msb_index(v << 1);
}

value bit_lib_msb_index_tagged(value x)
{
  return Val_long(msb_index(x));
}

static inline uintptr_t lsb_index(uintptr_t x)
{
  if (sizeof(x) == 8)
    return __builtin_ctzll(x);
  else if (sizeof(x) == 4)
    return __builtin_ctz(x);
  else
    assert(0);
}

uintptr_t bit_lib_lsb_index(uintptr_t v)
{
  return lsb_index(v);
}

value bit_lib_lsb_index_tagged(value x)
{
  return Val_long(lsb_index((uintptr_t)x >> 1));
}

static inline unsigned long long extract_msb(unsigned long long x)
{
  const unsigned int N = sizeof(unsigned long long) * CHAR_BIT;
  unsigned int leading_zeros = __builtin_clzll(x);
  unsigned int msb_position = (N - 1) - leading_zeros;
  return (unsigned long long)1 << msb_position;
}

uintptr_t bit_lib_extract_msb(uintptr_t x)
{
  return extract_msb((unsigned long long)x & (~(unsigned long long)0 >> 1));
}

value bit_lib_extract_msb_tagged(uintptr_t x)
{
  return Val_long(extract_msb(x >> 1));
}
