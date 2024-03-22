#include "rlang.h"
#include "tree_sitter/api.h"

static void parser_finalize(r_obj* pointer);
static inline TSInputEncoding as_encoding(r_obj* x);
