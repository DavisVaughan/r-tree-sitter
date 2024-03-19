#ifndef R_TREE_SITTER_PARSER_H
#define R_TREE_SITTER_PARSER_H

#include "rlang.h"
#include "tree_sitter/api.h"

r_obj* ts_parser_as_external_pointer(TSParser* x);
TSParser* ts_parser_from_external_pointer(r_obj* x);

#endif
