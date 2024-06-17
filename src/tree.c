#include "tree.h"

#include "decl/tree-decl.h"
#include "external-pointer.h"
#include "node.h"
#include "utils.h"

r_obj* ffi_tree_root_node(r_obj* ffi_x) {
  TSTree* x = ts_tree_from_external_pointer(ffi_x);
  TSNode node = ts_tree_root_node(x);
  return ts_node_as_raw(node);
}

r_obj* ffi_tree_root_node_with_offset(
    r_obj* ffi_x,
    r_obj* ffi_byte,
    r_obj* ffi_row,
    r_obj* ffi_column
) {
  TSTree* x = ts_tree_from_external_pointer(ffi_x);

  const uint32_t byte = r_dbl_as_uint32(r_dbl_get(ffi_byte, 0), "byte");

  const uint32_t row = r_dbl_as_uint32(r_dbl_get(ffi_row, 0), "row");
  const uint32_t column = r_dbl_as_uint32(r_dbl_get(ffi_column, 0), "column");

  const TSPoint point = {.row = row, .column = column};

  TSNode node = ts_tree_root_node_with_offset(x, byte, point);

  return ts_node_as_raw(node);
}

r_obj* ffi_tree_included_ranges(r_obj* ffi_x) {
  TSTree* x = ts_tree_from_external_pointer(ffi_x);

  uint32_t ts_size = 0;
  TSRange* v_ranges = ts_tree_included_ranges(x, &ts_size);

  const r_ssize size = (r_ssize) ts_size;

  r_obj* out = KEEP(r_alloc_list(6));

  r_obj* start_bytes = r_alloc_double(size);
  r_list_poke(out, 0, start_bytes);
  double* v_start_bytes = r_dbl_begin(start_bytes);

  r_obj* start_rows = r_alloc_double(size);
  r_list_poke(out, 1, start_rows);
  double* v_start_rows = r_dbl_begin(start_rows);

  r_obj* start_columns = r_alloc_double(size);
  r_list_poke(out, 2, start_columns);
  double* v_start_columns = r_dbl_begin(start_columns);

  r_obj* end_bytes = r_alloc_double(size);
  r_list_poke(out, 3, end_bytes);
  double* v_end_bytes = r_dbl_begin(end_bytes);

  r_obj* end_rows = r_alloc_double(size);
  r_list_poke(out, 4, end_rows);
  double* v_end_rows = r_dbl_begin(end_rows);

  r_obj* end_columns = r_alloc_double(size);
  r_list_poke(out, 5, end_columns);
  double* v_end_columns = r_dbl_begin(end_columns);

  for (r_ssize i = 0; i < size; ++i) {
    TSRange range = v_ranges[i];

    v_start_bytes[i] = r_uint32_as_dbl(range.start_byte);
    v_start_rows[i] = r_uint32_as_dbl(range.start_point.row);
    v_start_columns[i] = r_uint32_as_dbl(range.start_point.column);

    v_end_bytes[i] = r_uint32_as_dbl(range.end_byte);
    v_end_rows[i] = r_uint32_as_dbl(range.end_point.row);
    v_end_columns[i] = r_uint32_as_dbl(range.end_point.column);
  }

  free(v_ranges);
  FREE(1);
  return out;
}

// Only used from C level by `tree_reparse()`, because this produces
// a tree that no longer aligns with the `text` saved in the R object.
r_obj* ffi_tree_edit(
    r_obj* ffi_x,
    r_obj* ffi_start_byte,
    r_obj* ffi_start_row,
    r_obj* ffi_start_column,
    r_obj* ffi_old_end_byte,
    r_obj* ffi_old_end_row,
    r_obj* ffi_old_end_column,
    r_obj* ffi_new_end_byte,
    r_obj* ffi_new_end_row,
    r_obj* ffi_new_end_column
) {
  TSTree* x = ts_tree_from_external_pointer(ffi_x);

  // All important shallow copy of the tree which bumps the ref count.
  // This allows us to edit the copy of the tree and return a new one,
  // allowing nodes created from the previous tree to remain valid.
  // `ts_tree_delete()` will free the memory specific to the copy, and
  // decrement the ref count and eventually free a larger chunk of memory
  // when the ref count hits 0.
  x = ts_tree_copy(x);

  uint32_t start_byte =
      r_dbl_as_uint32(r_dbl_get(ffi_start_byte, 0), "start_byte");
  uint32_t start_row =
      r_dbl_as_uint32(r_dbl_get(ffi_start_row, 0), "start_row");
  uint32_t start_column =
      r_dbl_as_uint32(r_dbl_get(ffi_start_column, 0), "start_column");
  uint32_t old_end_byte =
      r_dbl_as_uint32(r_dbl_get(ffi_old_end_byte, 0), "old_end_byte");
  uint32_t old_end_row =
      r_dbl_as_uint32(r_dbl_get(ffi_old_end_row, 0), "old_end_row");
  uint32_t old_end_column =
      r_dbl_as_uint32(r_dbl_get(ffi_old_end_column, 0), "old_end_column");
  uint32_t new_end_byte =
      r_dbl_as_uint32(r_dbl_get(ffi_new_end_byte, 0), "new_end_byte");
  uint32_t new_end_row =
      r_dbl_as_uint32(r_dbl_get(ffi_new_end_row, 0), "new_end_row");
  uint32_t new_end_column =
      r_dbl_as_uint32(r_dbl_get(ffi_new_end_column, 0), "new_end_column");

  TSPoint start_point = {.row = start_row, .column = start_column};
  TSPoint old_end_point = {.row = old_end_row, .column = old_end_column};
  TSPoint new_end_point = {.row = new_end_row, .column = new_end_column};

  const TSInputEdit edit = {
      .start_byte = start_byte,
      .old_end_byte = old_end_byte,
      .new_end_byte = new_end_byte,
      .start_point = start_point,
      .old_end_point = old_end_point,
      .new_end_point = new_end_point};

  ts_tree_edit(x, &edit);

  return ts_tree_as_external_pointer(x);
}

r_obj* ts_tree_as_external_pointer(TSTree* x) {
  return new_external_pointer((void*) x, tree_finalize);
}

TSTree* ts_tree_from_external_pointer(r_obj* x) {
  TS_OBJECT_FROM_EXTERNAL_POINTER(x, TSTree*);
}

static void tree_finalize(r_obj* x) {
  if (r_typeof(x) != R_TYPE_pointer) {
    return;
  }

  TSTree* tree = (TSTree*) R_ExternalPtrAddr(x);

  if (tree == NULL) {
    return;
  }

  ts_tree_delete(tree);

  R_ClearExternalPtr(x);
}
