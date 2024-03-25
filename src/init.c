#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <stdlib.h>  // for NULL

#include "rlang.h"

// Defined below
r_obj* ffi_initialize(r_obj* ns);

extern r_obj* ffi_language_version(r_obj*);
extern r_obj* ffi_language_symbol_for_name(r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_language_symbol_name(r_obj*, r_obj*);
extern r_obj* ffi_language_field_id_for_name(r_obj*, r_obj*);
extern r_obj* ffi_parser_new(r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_parser_parse(r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_tree_root_node(r_obj*);
extern r_obj*
ffi_tree_edit(r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_node_s_expression(r_obj*);
extern r_obj* ffi_node_parent(r_obj*);
extern r_obj* ffi_node_child(r_obj*, r_obj*);
extern r_obj* ffi_node_named_child(r_obj*, r_obj*);
extern r_obj* ffi_node_child_count(r_obj*);
extern r_obj* ffi_node_named_child_count(r_obj*);
extern r_obj* ffi_node_text(r_obj*, r_obj*);
extern r_obj* ffi_node_type(r_obj*);
extern r_obj* ffi_node_symbol(r_obj*);
extern r_obj* ffi_node_grammar_type(r_obj*);
extern r_obj* ffi_node_grammar_symbol(r_obj*);
extern r_obj* ffi_node_is_named(r_obj*);
extern r_obj* ffi_node_children(r_obj*);
extern r_obj* ffi_node_named_children(r_obj*);
extern r_obj* ffi_node_child_by_field_id(r_obj*, r_obj*);
extern r_obj* ffi_node_child_by_field_name(r_obj*, r_obj*);
extern r_obj* ffi_node_field_name_for_child(r_obj*, r_obj*);
extern r_obj* ffi_node_first_child_for_byte(r_obj*, r_obj*);
extern r_obj* ffi_node_first_named_child_for_byte(r_obj*, r_obj*);
extern r_obj* ffi_node_start_byte(r_obj*);
extern r_obj* ffi_node_end_byte(r_obj*);
extern r_obj* ffi_node_start_point(r_obj*);
extern r_obj* ffi_node_end_point(r_obj*);
extern r_obj* ffi_node_next_sibling(r_obj*);
extern r_obj* ffi_node_previous_sibling(r_obj*);
extern r_obj* ffi_node_next_named_sibling(r_obj*);
extern r_obj* ffi_node_previous_named_sibling(r_obj*);
extern r_obj* ffi_node_is_missing(r_obj*);
extern r_obj* ffi_node_is_extra(r_obj*);
extern r_obj* ffi_node_is_error(r_obj*);
extern r_obj* ffi_node_has_error(r_obj*);
extern r_obj* ffi_node_parse_state(r_obj*);
extern r_obj* ffi_node_next_parse_state(r_obj*);
extern r_obj* ffi_node_descendent_count(r_obj*);
extern r_obj* ffi_node_descendent_for_byte_range(r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_node_named_descendent_for_byte_range(r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_tree_cursor_initialize(r_obj*);
extern r_obj* ffi_tree_cursor_reset(r_obj*, r_obj*);
extern r_obj* ffi_tree_cursor_node(r_obj*);
extern r_obj* ffi_tree_cursor_goto_parent(r_obj*);
extern r_obj* ffi_tree_cursor_goto_next_sibling(r_obj*);
extern r_obj* ffi_tree_cursor_goto_previous_sibling(r_obj*);
extern r_obj* ffi_tree_cursor_goto_first_child(r_obj*);
extern r_obj* ffi_tree_cursor_goto_last_child(r_obj*);
extern r_obj* ffi_tree_cursor_depth(r_obj*);
extern r_obj* ffi_tree_cursor_goto_first_child_for_byte(r_obj*, r_obj*);
extern r_obj*
ffi_tree_cursor_goto_first_child_for_point(r_obj*, r_obj*, r_obj*);
extern r_obj* ffi_tree_cursor_finalize(r_obj*);

static const R_CallMethodDef CallEntries[] = {
    {"ffi_initialize", (DL_FUNC) &ffi_initialize, 1},
    {"ffi_language_version", (DL_FUNC) &ffi_language_version, 1},
    {"ffi_language_symbol_for_name",
     (DL_FUNC) &ffi_language_symbol_for_name,
     3},
    {"ffi_language_symbol_name", (DL_FUNC) &ffi_language_symbol_name, 2},
    {"ffi_language_field_id_for_name",
     (DL_FUNC) &ffi_language_field_id_for_name,
     2},
    {"ffi_parser_new", (DL_FUNC) &ffi_parser_new, 3},
    {"ffi_parser_parse", (DL_FUNC) &ffi_parser_parse, 4},
    {"ffi_tree_root_node", (DL_FUNC) &ffi_tree_root_node, 1},
    {"ffi_tree_edit", (DL_FUNC) &ffi_tree_edit, 10},
    {"ffi_node_s_expression", (DL_FUNC) &ffi_node_s_expression, 1},
    {"ffi_node_parent", (DL_FUNC) &ffi_node_parent, 1},
    {"ffi_node_child", (DL_FUNC) &ffi_node_child, 2},
    {"ffi_node_named_child", (DL_FUNC) &ffi_node_named_child, 2},
    {"ffi_node_child_count", (DL_FUNC) &ffi_node_child_count, 1},
    {"ffi_node_named_child_count", (DL_FUNC) &ffi_node_named_child_count, 1},
    {"ffi_node_text", (DL_FUNC) &ffi_node_text, 2},
    {"ffi_node_type", (DL_FUNC) &ffi_node_type, 1},
    {"ffi_node_symbol", (DL_FUNC) &ffi_node_symbol, 1},
    {"ffi_node_grammar_type", (DL_FUNC) &ffi_node_grammar_type, 1},
    {"ffi_node_grammar_symbol", (DL_FUNC) &ffi_node_grammar_symbol, 1},
    {"ffi_node_is_named", (DL_FUNC) &ffi_node_is_named, 1},
    {"ffi_node_children", (DL_FUNC) &ffi_node_children, 1},
    {"ffi_node_named_children", (DL_FUNC) &ffi_node_named_children, 1},
    {"ffi_node_child_by_field_id", (DL_FUNC) &ffi_node_child_by_field_id, 2},
    {"ffi_node_child_by_field_name",
     (DL_FUNC) &ffi_node_child_by_field_name,
     2},
    {"ffi_node_field_name_for_child",
     (DL_FUNC) &ffi_node_field_name_for_child,
     2},
    {"ffi_node_first_child_for_byte",
     (DL_FUNC) &ffi_node_first_child_for_byte,
     2},
    {"ffi_node_first_named_child_for_byte",
     (DL_FUNC) &ffi_node_first_named_child_for_byte,
     2},
    {"ffi_node_start_byte", (DL_FUNC) &ffi_node_start_byte, 1},
    {"ffi_node_end_byte", (DL_FUNC) &ffi_node_end_byte, 1},
    {"ffi_node_start_point", (DL_FUNC) &ffi_node_start_point, 1},
    {"ffi_node_end_point", (DL_FUNC) &ffi_node_end_point, 1},
    {"ffi_node_next_sibling", (DL_FUNC) &ffi_node_next_sibling, 1},
    {"ffi_node_previous_sibling", (DL_FUNC) &ffi_node_previous_sibling, 1},
    {"ffi_node_next_named_sibling", (DL_FUNC) &ffi_node_next_named_sibling, 1},
    {"ffi_node_previous_named_sibling",
     (DL_FUNC) &ffi_node_previous_named_sibling,
     1},
    {"ffi_node_is_missing", (DL_FUNC) &ffi_node_is_missing, 1},
    {"ffi_node_is_extra", (DL_FUNC) &ffi_node_is_extra, 1},
    {"ffi_node_is_error", (DL_FUNC) &ffi_node_is_error, 1},
    {"ffi_node_has_error", (DL_FUNC) &ffi_node_has_error, 1},
    {"ffi_node_parse_state", (DL_FUNC) &ffi_node_parse_state, 1},
    {"ffi_node_next_parse_state", (DL_FUNC) &ffi_node_next_parse_state, 1},
    {"ffi_node_descendant_count", (DL_FUNC) &ffi_node_descendent_count, 1},
    {"ffi_node_descendent_for_byte_range",
     (DL_FUNC) &ffi_node_descendent_for_byte_range,
     3},
    {"ffi_node_named_descendent_for_byte_range",
     (DL_FUNC) &ffi_node_named_descendent_for_byte_range,
     3},
    {"ffi_tree_cursor_initialize", (DL_FUNC) &ffi_tree_cursor_initialize, 1},
    {"ffi_tree_cursor_reset", (DL_FUNC) &ffi_tree_cursor_reset, 2},
    {"ffi_tree_cursor_node", (DL_FUNC) &ffi_tree_cursor_node, 1},
    {"ffi_tree_cursor_goto_parent", (DL_FUNC) &ffi_tree_cursor_goto_parent, 1},
    {"ffi_tree_cursor_goto_next_sibling",
     (DL_FUNC) &ffi_tree_cursor_goto_next_sibling,
     1},
    {"ffi_tree_cursor_goto_previous_sibling",
     (DL_FUNC) &ffi_tree_cursor_goto_previous_sibling,
     1},
    {"ffi_tree_cursor_goto_first_child",
     (DL_FUNC) &ffi_tree_cursor_goto_first_child,
     1},
    {"ffi_tree_cursor_goto_last_child",
     (DL_FUNC) &ffi_tree_cursor_goto_last_child,
     1},
    {"ffi_tree_cursor_depth", (DL_FUNC) &ffi_tree_cursor_depth, 1},
    {"ffi_tree_cursor_goto_first_child_for_byte",
     (DL_FUNC) &ffi_tree_cursor_goto_first_child_for_byte,
     2},
    {"ffi_tree_cursor_goto_first_child_for_point",
     (DL_FUNC) &ffi_tree_cursor_goto_first_child_for_point,
     3},
    {"ffi_tree_cursor_finalize", (DL_FUNC) &ffi_tree_cursor_finalize, 1},
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
