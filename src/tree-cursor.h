#ifndef R_TREE_SITTER_TREE_CURSOR_H
#define R_TREE_SITTER_TREE_CURSOR_H

#include "rlang.h"
#include "tree_sitter/api.h"

r_obj* ts_tree_cursor_as_raw(TSTreeCursor x);
TSTreeCursor* ts_tree_cursor_from_raw(r_obj* x);

#endif
