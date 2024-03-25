#include "rlang.h"
#include "tree_sitter/api.h"

static r_obj* node_children(TSNode x, bool named);
static r_obj* node_point(TSNode x, bool start);
static r_obj* node_descendent_for_point_range(
    r_obj* ffi_x,
    r_obj* ffi_start_row,
    r_obj* ffi_start_column,
    r_obj* ffi_end_row,
    r_obj* ffi_end_column,
    bool named
);
