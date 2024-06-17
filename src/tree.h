#ifndef R_TREE_SITTER_TREE_H
#define R_TREE_SITTER_TREE_H

#include "rlang.h"
#include "tree_sitter/api.h"

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
);

r_obj* ts_tree_as_external_pointer(TSTree* x);
TSTree* ts_tree_from_external_pointer(r_obj* tree);

#endif
