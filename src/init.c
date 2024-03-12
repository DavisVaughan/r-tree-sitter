#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <stdlib.h>  // for NULL

#include "rlang.h"

// Defined below
r_obj* ffi_initialize(r_obj* ns);

extern r_obj* ffi_language_version(r_obj*);
extern r_obj* ffi_language_id_for_node_kind(r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_language_node_kind_for_id(r_obj*, r_obj*);

static const R_CallMethodDef CallEntries[] = {
    {"ffi_initialize", (DL_FUNC) &ffi_initialize, 1},
    {"ffi_language_version", (DL_FUNC) &ffi_language_version, 1},
    {"ffi_language_id_for_node_kind",
     (DL_FUNC) &ffi_language_id_for_node_kind,
     3},
    {"ffi_language_node_kind_for_id",
     (DL_FUNC) &ffi_language_node_kind_for_id,
     2},
    {NULL, NULL, 0}};

void R_init_treesitter(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

r_obj* ffi_initialize(r_obj* ns) {
  r_init_library(ns);
  return r_null;
}
