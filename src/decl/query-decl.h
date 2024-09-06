#include "rlang.h"
#include "tree_sitter/api.h"

static r_obj*
query_byte_for_pattern(r_obj* ffi_query, r_obj* ffi_i, bool start);
static r_obj* query_error(uint32_t error_offset, TSQueryError error_type);
static void query_finalize(r_obj* x);
