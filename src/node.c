#include "node.h"

r_obj* ffi_node_s_expression(r_obj* ffi_x) {
  TSNode* x = ts_node_from_raw(ffi_x);

  char* string = ts_node_string(*x);

  r_obj* out = KEEP(r_chr(string));

  free(string);

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
