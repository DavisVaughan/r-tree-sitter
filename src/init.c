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
extern r_obj*
ffi_tree_edit(r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_node_s_expression(r_obj*);
extern r_obj* ffi_node_child(r_obj*, r_obj*);
extern r_obj* ffi_node_text(r_obj*, r_obj*);
extern r_obj* ffi_node_is_named(r_obj*);
extern r_obj* ffi_node_children(r_obj*);
extern r_obj* ffi_node_named_children(r_obj*);
extern r_obj* ffi_node_start_byte(r_obj*);
extern r_obj* ffi_node_end_byte(r_obj*);
extern r_obj* ffi_node_start_point(r_obj*);
extern r_obj* ffi_node_end_point(r_obj*);

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
    {"ffi_tree_edit", (DL_FUNC) &ffi_tree_edit, 10},
    {"ffi_node_s_expression", (DL_FUNC) &ffi_node_s_expression, 1},
    {"ffi_node_child", (DL_FUNC) &ffi_node_child, 2},
    {"ffi_node_text", (DL_FUNC) &ffi_node_text, 2},
    {"ffi_node_is_named", (DL_FUNC) &ffi_node_is_named, 1},
    {"ffi_node_children", (DL_FUNC) &ffi_node_children, 1},
    {"ffi_node_named_children", (DL_FUNC) &ffi_node_named_children, 1},
    {"ffi_node_start_byte", (DL_FUNC) &ffi_node_start_byte, 1},
    {"ffi_node_end_byte", (DL_FUNC) &ffi_node_end_byte, 1},
    {"ffi_node_start_point", (DL_FUNC) &ffi_node_start_point, 1},
    {"ffi_node_end_point", (DL_FUNC) &ffi_node_end_point, 1},
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
