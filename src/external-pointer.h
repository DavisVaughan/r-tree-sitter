#ifndef R_TREE_SITTER_EXTERNAL_POINTER_H
#define R_TREE_SITTER_EXTERNAL_POINTER_H

#include "rlang.h"

r_obj* new_external_pointer(void* x, void(fn_finalize)(r_obj*));

#define TS_OBJECT_FROM_EXTERNAL_POINTER(X, TSType)                             \
  if (TYPEOF(X) != EXTPTRSXP) {                                                \
    Rf_errorcall(R_NilValue, "Input must be an external pointer.");            \
  }                                                                            \
                                                                               \
  TSType out = (TSType) R_ExternalPtrAddr(X);                                  \
                                                                               \
  if (out == NULL) {                                                           \
    Rf_errorcall(R_NilValue, "Input must point to a valid tree sitter type."); \
  }                                                                            \
                                                                               \
  return out                                                                   \


#endif
