#include "rlang.h"
#include "tree_sitter/api.h"

static r_obj* query_error(uint32_t error_offset, TSQueryError error_type);
static void query_finalize(r_obj* x);
