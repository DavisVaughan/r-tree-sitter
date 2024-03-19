#ifndef R_TREE_SITTER_NODE_H
#define R_TREE_SITTER_NODE_H

#include "rlang.h"
#include "tree_sitter/api.h"

r_obj* ts_node_as_raw(TSNode x);
TSNode* ts_node_from_raw(r_obj* x);

#endif
