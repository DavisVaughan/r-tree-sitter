#ifndef R_TREE_SITTER_LANGUAGE_H
#define R_TREE_SITTER_LANGUAGE_H

#include "rlang.h"
#include "tree_sitter/api.h"

const TSLanguage* ts_language_from_external_pointer(r_obj* x);

#endif
