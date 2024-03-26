#include "rlang.h"
#include "tree_sitter/api.h"

static bool
parser_set_included_ranges(TSParser* x, r_obj* included_range_vectors);
static void parser_finalize(r_obj* pointer);
static inline TSInputEncoding as_encoding(r_obj* x);
