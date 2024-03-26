#include "tree-cursor.h"

#include "external-pointer.h"
#include "node.h"
#include "utils.h"

r_obj* ffi_tree_cursor_initialize(r_obj* ffi_node) {
  TSNode* node = ts_node_from_raw(ffi_node);
  TSTreeCursor out = ts_tree_cursor_new(*node);
  return ts_tree_cursor_as_raw(out);
}

r_obj* ffi_tree_cursor_reset(r_obj* ffi_x, r_obj* ffi_node) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);
  TSNode* node = ts_node_from_raw(ffi_node);
  ts_tree_cursor_reset(x, *node);
  return r_null;
}

r_obj* ffi_tree_cursor_node(r_obj* ffi_x) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);
  TSNode node = ts_tree_cursor_current_node(x);
  return ts_node_as_raw(node);
}

r_obj* ffi_tree_cursor_field_name(r_obj* ffi_x) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);

  const char* name = ts_tree_cursor_current_field_name(x);

  r_obj* out = KEEP(r_alloc_character(1));
  r_chr_poke(out, 0, (name == NULL) ? r_globals.na_str : r_str(name));

  FREE(1);
  return out;
}

r_obj* ffi_tree_cursor_field_id(r_obj* ffi_x) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);
  const TSFieldId out = ts_tree_cursor_current_field_id(x);
  return r_int(
      (out == (TSFieldId) 0) ? r_globals.na_int : r_TSFieldId_as_int(out)
  );
}

r_obj* ffi_tree_cursor_descendant_index(r_obj* ffi_x) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);
  const uint32_t out = ts_tree_cursor_current_descendant_index(x);
  return r_dbl(r_uint32_as_dbl(out));
}

r_obj* ffi_tree_cursor_goto_parent(r_obj* ffi_x) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);
  bool out = ts_tree_cursor_goto_parent(x);
  return r_lgl(out);
}

r_obj* ffi_tree_cursor_goto_next_sibling(r_obj* ffi_x) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);
  bool out = ts_tree_cursor_goto_next_sibling(x);
  return r_lgl(out);
}

r_obj* ffi_tree_cursor_goto_previous_sibling(r_obj* ffi_x) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);
  bool out = ts_tree_cursor_goto_previous_sibling(x);
  return r_lgl(out);
}

r_obj* ffi_tree_cursor_goto_first_child(r_obj* ffi_x) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);
  bool out = ts_tree_cursor_goto_first_child(x);
  return r_lgl(out);
}

r_obj* ffi_tree_cursor_goto_last_child(r_obj* ffi_x) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);
  bool out = ts_tree_cursor_goto_last_child(x);
  return r_lgl(out);
}

r_obj* ffi_tree_cursor_depth(r_obj* ffi_x) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);
  uint32_t out = ts_tree_cursor_current_depth(x);
  return r_dbl((double) out);
}

r_obj*
ffi_tree_cursor_goto_first_child_for_byte(r_obj* ffi_x, r_obj* ffi_byte) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);
  const uint32_t byte = r_dbl_as_uint32(r_dbl_get(ffi_byte, 0), "byte");

  // Rather than dealing with converting `int64_t` to an R type, we just return
  // `true` if found and `false` if not found, like the other goto functions.
  const int64_t index = ts_tree_cursor_goto_first_child_for_byte(x, byte);

  return r_lgl(index != -1);
}

r_obj* ffi_tree_cursor_goto_first_child_for_point(
    r_obj* ffi_x,
    r_obj* ffi_row,
    r_obj* ffi_column
) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);

  const uint32_t row = r_dbl_as_uint32(r_dbl_get(ffi_row, 0), "row");
  const uint32_t column = r_dbl_as_uint32(r_dbl_get(ffi_column, 0), "column");

  const TSPoint point = {.row = row, .column = column};

  // Rather than dealing with converting `int64_t` to an R type, we just return
  // `true` if found and `false` if not found, like the other goto functions.
  const int64_t index = ts_tree_cursor_goto_first_child_for_point(x, point);

  return r_lgl(index != -1);
}

r_obj* ffi_tree_cursor_finalize(r_obj* ffi_x) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);
  ts_tree_cursor_delete(x);
  return r_null;
}

r_obj* ts_tree_cursor_as_raw(TSTreeCursor x) {
  // Unlike other tree-sitter objects, these aren't on the heap.
  // We represent tree cursors with raw vectors.
  // Lifetime management (i.e. tied to a tree) is done on the R side.
  // We still use a finalizer for tree cursors to call the delete helper.
  r_obj* out = KEEP(r_alloc_raw(sizeof(TSTreeCursor)));
  TSTreeCursor* p_out = (TSTreeCursor*) r_raw_begin(out);

  memcpy(p_out, &x, sizeof(TSTreeCursor));

  FREE(1);
  return out;
}

TSTreeCursor* ts_tree_cursor_from_raw(r_obj* x) {
  if (r_typeof(x) != R_TYPE_raw) {
    r_abort("`x` must be a raw vector.");
  }

  return r_raw_begin(x);
}
