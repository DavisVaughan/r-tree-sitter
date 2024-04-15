#include "rlang.h"
#include "tree_sitter/api.h"

static r_obj* predicate_eq(
    const char* predicate_type,
    uint32_t n_predicate_steps,
    const TSQueryPredicateStep* steps,
    const TSQuery* query
);
static r_obj* predicate_eq_capture(
    uint32_t capture_name_value_id,
    uint32_t capture_value_id,
    bool capture_invert
);
static r_obj* predicate_eq_string(
    uint32_t capture_name_value_id,
    const char* capture_value,
    uint32_t capture_value_length,
    bool capture_invert
);

static r_obj* predicate_match(
    const char* predicate_type,
    uint32_t n_predicate_steps,
    const TSQueryPredicateStep* steps,
    const TSQuery* query
);
static r_obj* predicate_match_string(
    uint32_t capture_name_value_id,
    const char* capture_value,
    uint32_t capture_value_length,
    bool capture_invert
);

static r_obj* query_error(uint32_t error_offset, TSQueryError error_type);
static void query_finalize(r_obj* x);
