#include <caml/mlvalues.h>

CAMLprim value ml_ctz(value n)
{
  return Val_long(__builtin_ctzll(Long_val(n)));
}

CAMLprim intnat ml_ctz_untagged(intnat x)
{
  return __builtin_ctzll (x);
}
