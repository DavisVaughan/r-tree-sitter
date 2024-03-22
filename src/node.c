#include "node.h"

#include "decl/node-decl.h"

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
  r_ssize text_size = r_length(c_text);

  // `[start_byte, end_byte)`
  uint32_t start_byte = ts_node_start_byte(*x);
  uint32_t end_byte = ts_node_end_byte(*x);

  if ((r_ssize) start_byte > text_size) {
    r_stop_internal("Node `start_byte` exceeds `text` size.");
  }
  if ((r_ssize) end_byte > text_size) {
    r_stop_internal("Node `end_byte` exceeds `text` size.");
  }
  if (end_byte < start_byte) {
    r_stop_internal("Node `end_byte` is somehow less than the `start_byte`.");
  }

  const char* source = text + start_byte;
  size_t size = end_byte - start_byte;

  r_obj* out = KEEP(r_alloc_character(1));
  r_chr_poke(out, 0, Rf_mkCharLenCE(source, size, CE_UTF8));

  FREE(1);
  return out;
}

r_obj* ffi_node_child(r_obj* ffi_x, r_obj* ffi_i) {
  TSNode* x = ts_node_from_raw(ffi_x);

  // Validated on R side to be positive integer of length 1, 1-indexed
  const uint32_t child_index = (uint32_t) r_int_get(ffi_i, 0) - 1;

  TSNode out = ts_node_child(*x, child_index);

  if (ts_node_is_null(out)) {
    return r_null;
  } else {
    return ts_node_as_raw(out);
  }
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
