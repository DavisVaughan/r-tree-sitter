#include "external-pointer.h"

r_obj* new_external_pointer(void* x, void(fn_finalize)(r_obj*)) {
  r_obj* out = KEEP(R_MakeExternalPtr(x, R_NilValue, R_NilValue));

  const Rboolean finalize_on_exit = TRUE;
  R_RegisterCFinalizerEx(out, fn_finalize, finalize_on_exit);

  FREE(1);
  return out;
}
