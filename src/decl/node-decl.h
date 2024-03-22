#include "rlang.h"
#include "tree_sitter/api.h"

static r_obj* node_children(TSNode x, bool named);
