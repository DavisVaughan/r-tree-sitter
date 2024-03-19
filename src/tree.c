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

  return r_null;
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
