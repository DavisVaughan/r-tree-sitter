#include "tree-cursor.h"

#include "external-pointer.h"
#include "node.h"

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

r_obj* ffi_tree_cursor_current_node(r_obj* ffi_x) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);
  TSNode node = ts_tree_cursor_current_node(x);
  return ts_node_as_raw(node);
}

r_obj* ffi_tree_cursor_goto_first_child(r_obj* ffi_x) {
  TSTreeCursor* x = ts_tree_cursor_from_raw(ffi_x);
  bool out = ts_tree_cursor_goto_first_child(x);
  return r_lgl(out);
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