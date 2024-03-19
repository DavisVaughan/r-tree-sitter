#include "utils.h"

r_obj* r_arg_as_string(r_obj* x, const char* arg) {
  if (r_typeof(x) != R_TYPE_character) {
    r_abort("%s must be a string.", arg);
  }

  if (r_length(x) != 1) {
    const int length = (int) r_length(x);
    r_abort("%s must be length 1, not length %i.", arg, length);
  }

  return r_chr_get(x, 0);
}

uint32_t r_ssize_as_uint32(r_ssize x) {
  if (x > (r_ssize) UINT32_MAX || x < 0) {
    r_abort(
        "Can't convert `x` to `uint32_t`. `x` must be within the range of `[0, "
        "UINT32_MAX]`."
    );
  }
  return (uint32_t) x;
}

uint32_t r_dbl_as_uint32(double x, const char* arg) {
  if (isnan(x)) {
    r_abort("`%s` can't be missing", arg);
  }
  if (x > (double) UINT32_MAX || x < 0) {
    r_abort(
        "Can't convert `%s` to `uint32_t`. `%s` must be within the range of "
        "`[0, "
        "UINT32_MAX]`.",
        arg,
        arg
    );
  }
  if (floor(x) != x) {
    r_abort("`%s` must be a whole number.", arg);
  }
  return (uint32_t) x;
}

// `TSSymbol` is `uint16_t`
TSSymbol r_int_as_TSSymbol(int x, const char* arg) {
  if (x > (int) UINT16_MAX || x < 0) {
    r_abort(
        "Can't convert `%s` to `TSSymbol`. `%s` must be within the range of "
        "`[0, "
        "UINT16_MAX]`.",
        arg,
        arg
    );
  }
  return (TSSymbol) x;
}

bool r_lgl_any_missing(r_obj* x) {
  const r_ssize size = r_length(x);
  const int* v_x = r_lgl_cbegin(x);

  for (r_ssize i = 0; i < size; ++i) {
    if (v_x[i] == r_globals.na_lgl) {
      return true;
    }
  }

  return false;
}

bool r_int_any_missing(r_obj* x) {
  const r_ssize size = r_length(x);
  const int* v_x = r_int_cbegin(x);

  for (r_ssize i = 0; i < size; ++i) {
    if (v_x[i] == r_globals.na_int) {
      return true;
    }
  }

  return false;
}
