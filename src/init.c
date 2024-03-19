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
extern r_obj* ffi_parser_initialize(void);
extern r_obj* ffi_parser_set_language(r_obj*, r_obj*);
extern r_obj* ffi_parser_parse(r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_tree_root_node(r_obj*);
extern r_obj* ffi_node_s_expression(r_obj*);

static const R_CallMethodDef CallEntries[] = {
    {"ffi_initialize", (DL_FUNC) &ffi_initialize, 1},
    {"ffi_language_version", (DL_FUNC) &ffi_language_version, 1},
    {"ffi_language_id_for_node_kind",
     (DL_FUNC) &ffi_language_id_for_node_kind,
     3},
    {"ffi_language_node_kind_for_id",
     (DL_FUNC) &ffi_language_node_kind_for_id,
     2},
    {"ffi_parser_initialize", (DL_FUNC) &ffi_parser_initialize, 0},
    {"ffi_parser_set_language", (DL_FUNC) &ffi_parser_set_language, 2},
    {"ffi_parser_parse", (DL_FUNC) &ffi_parser_parse, 3},
    {"ffi_tree_root_node", (DL_FUNC) &ffi_tree_root_node, 1},
    {"ffi_node_s_expression", (DL_FUNC) &ffi_node_s_expression, 1},
    {NULL, NULL, 0}};

void R_init_treesitter(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

extern void init_alloc(void);

r_obj* ffi_initialize(r_obj* ns) {
  r_init_library(ns);
  init_alloc();
  return r_null;
}
