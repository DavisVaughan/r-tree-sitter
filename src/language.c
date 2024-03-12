#include "language.h"

r_obj* ffi_language_version(r_obj* x) {
  const TSLanguage* language = ts_language_from_external_pointer(x);

  uint32_t version = ts_language_version(language);

  r_obj* out = KEEP(r_alloc_double(1));
  r_dbl_poke(out, 0, (double) version);

  FREE(1);
  return out;
}

const TSLanguage* ts_language_from_external_pointer(r_obj* x) {
  if (r_typeof(x) != R_TYPE_pointer) {
    r_abort("`x` must be an external pointer.");
  }

  const TSLanguage* out = (const TSLanguage*) R_ExternalPtrAddr(x);

  if (out == NULL) {
    r_abort("`x` must point to a `TSLanguage`.");
  }

  return out;
}
