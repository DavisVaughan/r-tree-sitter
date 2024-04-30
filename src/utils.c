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
  // On 32 bit Windows `r_ssize = int < uint32_t`
  // On 64 bit Windows `r_ssize = ptrdiff_t > uint32_t`
  // So we can't use a static directional cast. Instead we try and cast both
  // "up" to a fixed larger type to compare them.
  if (x < 0 || (uint64_t) x > (uint64_t) UINT32_MAX) {
    r_abort(
        "Can't convert `x` to `uint32_t`. `x` must be within the range of `[0, "
        "UINT32_MAX]`."
    );
  }
  return (uint32_t) x;
}

r_ssize r_uint32_as_r_ssize(uint32_t x) {
  return (r_ssize) x;
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

double r_uint32_as_dbl(uint32_t x) {
  return (double) x;
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

// `TSSymbol` is `uint16_t`
int r_TSSymbol_as_int(TSSymbol x) {
  return (int) x;
}

// `TSFieldId` is `uint16_t`
TSFieldId r_int_as_TSFieldId(int x, const char* arg) {
  if (x > (int) UINT16_MAX || x < 0) {
    r_abort(
        "Can't convert `%s` to `TSFieldId`. `%s` must be within the range of "
        "`[0, "
        "UINT16_MAX]`.",
        arg,
        arg
    );
  }
  return (TSFieldId) x;
}

// `TSFieldId` is `uint16_t`
int r_TSFieldId_as_int(TSFieldId x) {
  return (int) x;
}

// `TSStateId` is `uint16_t`
TSStateId r_int_as_TSStateId(int x, const char* arg) {
  if (x > (int) UINT16_MAX || x < 0) {
    r_abort(
        "Can't convert `%s` to `TSStateId`. `%s` must be within the range of "
        "`[0, "
        "UINT16_MAX]`.",
        arg,
        arg
    );
  }
  return (TSStateId) x;
}

// `TSStateId` is `uint16_t`
int r_TSStateId_as_int(TSStateId x) {
  return (int) x;
}

int r_uint32_as_int(uint32_t x, const char* arg) {
  if (x > (uint32_t) INT_MAX || x < 0) {
    r_abort(
        "Can't convert `%s` to `int`. `%s` must be within the range of "
        "`[0, "
        "INT_MAX]`.",
        arg,
        arg
    );
  }
  return (int) x;
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

bool r_chr_any_missing(r_obj* x) {
  const r_ssize size = r_length(x);
  r_obj* const* v_x = r_chr_cbegin(x);

  for (r_ssize i = 0; i < size; ++i) {
    if (v_x[i] == r_globals.na_str) {
      return true;
    }
  }

  return false;
}

bool str_equal(const char* x, const char* y) {
  return strcmp(x, y) == 0;
}

// Compares up to `n` characters of `x` and `y` for equality
//
// Checks pairs of `x` and `y` characters for equality until:
// - A character differs
// - A nul terminating character is reached
// - `n` characters have been processed
// (whichever happens first)
bool str_equal_up_to(const char* x, const char* y, size_t n) {
  return strncmp(x, y, n) == 0;
}
