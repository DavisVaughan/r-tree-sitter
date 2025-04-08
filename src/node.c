#include "node.h"

#include "decl/node-decl.h"
#include "utils.h"

r_obj* ffi_node_s_expression(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);

  char* string = ts_node_string(*x);

  r_obj* out = KEEP(r_chr(string));

  free(string);

  FREE(1);
  return out;
}

r_obj* ffi_node_text(r_obj* ffi_x, r_obj* ffi_text) {
  TSNode* x = ts_node_from_raw(ffi_x);

  r_obj* c_text = r_chr_get(ffi_text, 0);
  const char* text = r_str_c_string(c_text);
  uint32_t text_size = r_ssize_as_uint32(r_length(c_text));

  uint32_t size = 0;
  const char* source = node_text(*x, text, text_size, &size);

  r_obj* out = KEEP(r_alloc_character(1));
  r_chr_poke(
      out, 0, Rf_mkCharLenCE(source, r_uint32_as_int(size, "size"), CE_UTF8)
  );

  FREE(1);
  return out;
}

const char*
node_text(TSNode x, const char* text, uint32_t text_size, uint32_t* size) {
  // `[start_byte, end_byte)`
  uint32_t start_byte = ts_node_start_byte(x);
  uint32_t end_byte = ts_node_end_byte(x);

  if (start_byte > text_size) {
    r_stop_internal("Node `start_byte` exceeds `text` size.");
  }
  if (end_byte > text_size) {
    r_stop_internal("Node `end_byte` exceeds `text` size.");
  }
  if (end_byte < start_byte) {
    r_stop_internal("Node `end_byte` is somehow less than the `start_byte`.");
  }

  *size = end_byte - start_byte;

  return text + start_byte;
}

r_obj* ffi_node_parent(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  TSNode out = ts_node_parent(*x);
  return ts_node_is_null(out) ? r_null : ts_node_as_raw(out);
}

r_obj* ffi_node_child(r_obj* ffi_x, r_obj* ffi_i) {
  TSNode* x = ts_node_from_raw(ffi_x);
  // Validated on R side to be positive whole double of length 1, 1-indexed
  const uint32_t i = r_dbl_as_uint32(r_dbl_get(ffi_i, 0), "i") - 1;
  TSNode out = ts_node_child(*x, i);
  return ts_node_is_null(out) ? r_null : ts_node_as_raw(out);
}

r_obj* ffi_node_named_child(r_obj* ffi_x, r_obj* ffi_i) {
  TSNode* x = ts_node_from_raw(ffi_x);
  // Validated on R side to be positive whole double of length 1, 1-indexed
  const uint32_t i = r_dbl_as_uint32(r_dbl_get(ffi_i, 0), "i") - 1;
  TSNode out = ts_node_named_child(*x, i);
  return ts_node_is_null(out) ? r_null : ts_node_as_raw(out);
}

r_obj* ffi_node_child_count(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  uint32_t count = ts_node_child_count(*x);
  return r_dbl((double) count);
}

r_obj* ffi_node_named_child_count(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  uint32_t count = ts_node_named_child_count(*x);
  return r_dbl((double) count);
}

r_obj* ffi_node_children(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  return node_children(*x, false);
}

r_obj* ffi_node_named_children(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  return node_children(*x, true);
}

static r_obj* node_children(TSNode x, bool named) {
  const uint32_t count =
      named ? ts_node_named_child_count(x) : ts_node_child_count(x);

  r_obj* out = KEEP(r_alloc_list((r_ssize) count));
  r_ssize i = 0;

  TSTreeCursor cursor = ts_tree_cursor_new(x);
  bool ok = ts_tree_cursor_goto_first_child(&cursor);

  while (ok) {
    TSNode elt = ts_tree_cursor_current_node(&cursor);

    if (!named || ts_node_is_named(elt)) {
      r_list_poke(out, i, ts_node_as_raw(elt));
      ++i;
    }

    ok = ts_tree_cursor_goto_next_sibling(&cursor);
  }

  FREE(1);
  return out;
}

r_obj* ffi_node_child_by_field_id(r_obj* ffi_x, r_obj* ffi_id) {
  TSNode* x = ts_node_from_raw(ffi_x);
  const TSFieldId id = r_int_as_TSFieldId(r_int_get(ffi_id, 0), "id");
  TSNode out = ts_node_child_by_field_id(*x, id);
  return ts_node_is_null(out) ? r_null : ts_node_as_raw(out);
}

r_obj* ffi_node_child_by_field_name(r_obj* ffi_x, r_obj* ffi_name) {
  TSNode* x = ts_node_from_raw(ffi_x);

  r_obj* name = r_chr_get(ffi_name, 0);
  const char* name_c = r_str_c_string(name);
  const uint32_t name_size = r_ssize_as_uint32(r_length(name));

  TSNode out = ts_node_child_by_field_name(*x, name_c, name_size);
  return ts_node_is_null(out) ? r_null : ts_node_as_raw(out);
}

r_obj* ffi_node_field_name_for_child(r_obj* ffi_x, r_obj* ffi_i) {
  TSNode* x = ts_node_from_raw(ffi_x);

  // Validated on R side to be positive whole double of length 1, 1-indexed
  const uint32_t i = r_dbl_as_uint32(r_dbl_get(ffi_i, 0), "i") - 1;

  const char* name = ts_node_field_name_for_child(*x, i);

  r_obj* out = KEEP(r_alloc_character(1));
  r_chr_poke(out, 0, (name == NULL) ? r_globals.na_str : r_str(name));

  FREE(1);
  return out;
}

r_obj* ffi_node_first_child_for_byte(r_obj* ffi_x, r_obj* ffi_byte) {
  TSNode* x = ts_node_from_raw(ffi_x);
  const uint32_t byte = r_dbl_as_uint32(r_dbl_get(ffi_byte, 0), "byte");
  TSNode out = ts_node_first_child_for_byte(*x, byte);
  return ts_node_is_null(out) ? r_null : ts_node_as_raw(out);
}

r_obj* ffi_node_first_named_child_for_byte(r_obj* ffi_x, r_obj* ffi_byte) {
  TSNode* x = ts_node_from_raw(ffi_x);
  const uint32_t byte = r_dbl_as_uint32(r_dbl_get(ffi_byte, 0), "byte");
  TSNode out = ts_node_first_named_child_for_byte(*x, byte);
  return ts_node_is_null(out) ? r_null : ts_node_as_raw(out);
}

r_obj* ffi_node_type(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  const char* out = ts_node_type(*x);
  return r_chr(out);
}

r_obj* ffi_node_symbol(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  const TSSymbol out = ts_node_symbol(*x);
  return r_int(r_TSSymbol_as_int(out));
}

r_obj* ffi_node_grammar_type(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  const char* out = ts_node_grammar_type(*x);
  return r_chr(out);
}

r_obj* ffi_node_grammar_symbol(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  const TSSymbol out = ts_node_grammar_symbol(*x);
  return r_int(r_TSSymbol_as_int(out));
}

r_obj* ffi_node_is_named(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  bool out = ts_node_is_named(*x);
  return r_lgl(out);
}

r_obj* ffi_node_start_byte(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  uint32_t byte = ts_node_start_byte(*x);
  return r_dbl((double) byte);
}

r_obj* ffi_node_end_byte(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  uint32_t byte = ts_node_end_byte(*x);
  return r_dbl((double) byte);
}

r_obj* ffi_node_start_point(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  return node_point(*x, true);
}

r_obj* ffi_node_end_point(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  return node_point(*x, false);
}

static r_obj* node_point(TSNode x, bool start) {
  TSPoint point = start ? ts_node_start_point(x) : ts_node_end_point(x);

  r_obj* out = KEEP(r_alloc_list(2));
  r_list_poke(out, 0, r_dbl((double) point.row));
  r_list_poke(out, 1, r_dbl((double) point.column));

  r_obj* names = r_alloc_character(2);
  r_attrib_poke_names(out, names);
  r_chr_poke(names, 0, r_str("row"));
  r_chr_poke(names, 1, r_str("column"));

  FREE(1);
  return out;
}

r_obj* ffi_node_next_sibling(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  TSNode node = ts_node_next_sibling(*x);
  return ts_node_is_null(node) ? r_null : ts_node_as_raw(node);
}

r_obj* ffi_node_previous_sibling(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  TSNode node = ts_node_prev_sibling(*x);
  return ts_node_is_null(node) ? r_null : ts_node_as_raw(node);
}

r_obj* ffi_node_next_named_sibling(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  TSNode node = ts_node_next_named_sibling(*x);
  return ts_node_is_null(node) ? r_null : ts_node_as_raw(node);
}

r_obj* ffi_node_previous_named_sibling(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  TSNode node = ts_node_prev_named_sibling(*x);
  return ts_node_is_null(node) ? r_null : ts_node_as_raw(node);
}

r_obj* ffi_node_is_missing(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  return r_lgl(ts_node_is_missing(*x));
}

r_obj* ffi_node_is_extra(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  return r_lgl(ts_node_is_extra(*x));
}

r_obj* ffi_node_is_error(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  return r_lgl(ts_node_is_error(*x));
}

r_obj* ffi_node_has_error(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  return r_lgl(ts_node_has_error(*x));
}

r_obj* ffi_node_parse_state(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  const TSStateId out = ts_node_parse_state(*x);
  return r_int(r_TSStateId_as_int(out));
}

r_obj* ffi_node_next_parse_state(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  const TSStateId out = ts_node_next_parse_state(*x);
  return r_int(r_TSStateId_as_int(out));
}

r_obj* ffi_node_descendant_count(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);
  const uint32_t out = ts_node_descendant_count(*x);
  return r_dbl(r_uint32_as_dbl(out));
}

r_obj* ffi_node_descendant_for_byte_range(
    r_obj* ffi_x,
    r_obj* ffi_start,
    r_obj* ffi_end
) {
  TSNode* x = ts_node_from_raw(ffi_x);
  const uint32_t start = r_dbl_as_uint32(r_dbl_get(ffi_start, 0), "start");
  const uint32_t end = r_dbl_as_uint32(r_dbl_get(ffi_end, 0), "end");
  const TSNode out = ts_node_descendant_for_byte_range(*x, start, end);
  return ts_node_as_raw(out);
}

r_obj* ffi_node_named_descendant_for_byte_range(
    r_obj* ffi_x,
    r_obj* ffi_start,
    r_obj* ffi_end
) {
  TSNode* x = ts_node_from_raw(ffi_x);
  const uint32_t start = r_dbl_as_uint32(r_dbl_get(ffi_start, 0), "start");
  const uint32_t end = r_dbl_as_uint32(r_dbl_get(ffi_end, 0), "end");
  const TSNode out = ts_node_named_descendant_for_byte_range(*x, start, end);
  return ts_node_as_raw(out);
}

r_obj* ffi_node_descendant_for_point_range(
    r_obj* ffi_x,
    r_obj* ffi_start_row,
    r_obj* ffi_start_column,
    r_obj* ffi_end_row,
    r_obj* ffi_end_column
) {
  return node_descendant_for_point_range(
      ffi_x, ffi_start_row, ffi_start_column, ffi_end_row, ffi_end_column, false
  );
}

r_obj* ffi_node_named_descendant_for_point_range(
    r_obj* ffi_x,
    r_obj* ffi_start_row,
    r_obj* ffi_start_column,
    r_obj* ffi_end_row,
    r_obj* ffi_end_column
) {
  return node_descendant_for_point_range(
      ffi_x, ffi_start_row, ffi_start_column, ffi_end_row, ffi_end_column, true
  );
}

static r_obj* node_descendant_for_point_range(
    r_obj* ffi_x,
    r_obj* ffi_start_row,
    r_obj* ffi_start_column,
    r_obj* ffi_end_row,
    r_obj* ffi_end_column,
    bool named
) {
  TSNode* x = ts_node_from_raw(ffi_x);

  const uint32_t start_row =
      r_dbl_as_uint32(r_dbl_get(ffi_start_row, 0), "start_row");
  const uint32_t start_column =
      r_dbl_as_uint32(r_dbl_get(ffi_start_column, 0), "start_column");
  const uint32_t end_row =
      r_dbl_as_uint32(r_dbl_get(ffi_end_row, 0), "end_row");
  const uint32_t end_column =
      r_dbl_as_uint32(r_dbl_get(ffi_end_column, 0), "end_column");

  const TSPoint start = {.row = start_row, .column = start_column};
  const TSPoint end = {.row = end_row, .column = end_column};

  const TSNode out =
      named ? ts_node_named_descendant_for_point_range(*x, start, end)
            : ts_node_descendant_for_point_range(*x, start, end);

  return ts_node_as_raw(out);
}

// Call `treesitter:::new_node()`
r_obj* r_exec_new_node(TSNode x, r_obj* tree) {
  static r_obj* call = NULL;
  static r_obj* env = NULL;
  static r_obj* raw_sym = NULL;
  static r_obj* tree_sym = NULL;

  if (call == NULL) {
    raw_sym = r_sym("raw");
    tree_sym = r_sym("tree");

    // Technically can allocate (may get a fresh object with a user database or
    // active binding) even though it won't here, but rchk will complain
    r_obj* ns = KEEP(r_env_find(R_NamespaceRegistry, r_sym("treesitter")));

    r_obj* fn = r_env_find(ns, r_sym("new_node"));
    call = r_call3(fn, raw_sym, tree_sym);
    r_preserve(call);

    env = r_alloc_environment(2, ns);
    r_preserve(env);

    FREE(1);
  }

  r_env_poke(env, raw_sym, ts_node_as_raw(x));
  r_env_poke(env, tree_sym, tree);

  return r_eval(call, env);
}

r_obj* ts_node_as_raw(TSNode x) {
  // Unlike other tree-sitter objects, these aren't on the heap.
  // We represent nodes with raw vectors.
  // Lifetime management (i.e. tied to a tree) is done on the R side.
  r_obj* out = KEEP(r_alloc_raw(sizeof(TSNode)));
  TSNode* p_out = (TSNode*) r_raw_begin(out);

  memcpy(p_out, &x, sizeof(TSNode));

  FREE(1);
  return out;
}

TSNode* ts_node_from_raw(r_obj* x) {
  if (r_typeof(x) != R_TYPE_raw) {
    r_abort("`x` must be a raw vector.");
  }

  return r_raw_begin(x);
}
