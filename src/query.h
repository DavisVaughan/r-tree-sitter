#ifndef R_TREE_SITTER_QUERY_H
#define R_TREE_SITTER_QUERY_H

#include "rlang.h"
#include "tree_sitter/api.h"

r_obj* ts_query_as_external_pointer(TSQuery* x);
TSQuery* ts_query_from_external_pointer(r_obj* x);

#endif
