#ifndef R_TREE_SITTER_TREE_H
#define R_TREE_SITTER_TREE_H

#include "rlang.h"
#include "tree_sitter/api.h"

r_obj* ts_tree_as_external_pointer(TSTree* x);
TSTree* ts_tree_from_external_pointer(r_obj* tree);

#endif
