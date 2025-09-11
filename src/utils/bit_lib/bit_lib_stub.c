#include <assert.h>
#include <caml/mlvalues.h>

uintptr_t bit_lib_pop_count(value v)
{
  uintptr_t x = v;
  x >>= 1;
  if (sizeof(x) == 8)
    return __builtin_popcountll(x);
  else if (sizeof(x) == 4)
    return __builtin_popcount(x);
  else
    assert(0);
}

value bit_lib_pop_count_tagged(value x)
{
  return Val_long(bit_lib_pop_count(x));
}

uintptr_t bit_lib_msb_index(value v)
{
  uintptr_t x = v;
  x >>= 1;
  if (sizeof(x) == 8)
    return 63 - __builtin_clzll(x);
  else if (sizeof(x) == 4)
    return 31 - __builtin_clz(x);
  else
    assert(0);
}

value bit_lib_msb_index_tagged(value x)
{
  return Val_long(bit_lib_msb_index(x));
}

uintptr_t bit_lib_lsb_index(value v)
{
  uintptr_t x = v;
  x >>= 1;
  if (sizeof(x) == 8)
    return __builtin_ctzll(x);
  else if (sizeof(x) == 4)
    return __builtin_ctz(x);
  else
    assert(0);
}

value bit_lib_lsb_index_tagged(value x)
{
  return Val_long(bit_lib_lsb_index(x));
}

uintptr_t bit_lib_extract_msb(value v)
{
  uintptr_t x = v;
  x >>= 1;
  return __builtin_stdc_bit_floor(x);
}

value bit_lib_extract_msb_tagged(value x)
{
  return Val_long(bit_lib_extract_msb(x));
}
