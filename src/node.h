#ifndef R_TREE_SITTER_NODE_H
#define R_TREE_SITTER_NODE_H

#include "rlang.h"
#include "tree_sitter/api.h"

r_obj* ts_node_as_raw(TSNode x);
TSNode* ts_node_from_raw(r_obj* x);

r_obj* r_exec_new_node(TSNode x, r_obj* tree);

// Returns a pointer to the start of `x`'s text representation inside `text`,
// and updates `size` to be the size of that text.
const char*
node_text(TSNode x, const char* text, uint32_t text_size, uint32_t* size);

#endif
