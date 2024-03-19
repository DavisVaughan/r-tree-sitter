#ifndef R_TREE_SITTER_EXTERNAL_POINTER_H
#define R_TREE_SITTER_EXTERNAL_POINTER_H

#include "rlang.h"

r_obj* new_external_pointer(void* x, void(fn_finalize)(r_obj*));

#endif
