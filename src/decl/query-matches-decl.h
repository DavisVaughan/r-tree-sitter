#include "rlang.h"
#include "tree_sitter/api.h"

static bool satisfies_pattern_predicates(
    TSQueryMatch* match,
    r_obj* pattern_predicates,
    const char* text,
    uint32_t text_size
);

static r_obj* predicate_eq(
    const char* predicate_type,
    uint32_t n_predicate_steps,
    const TSQueryPredicateStep* steps,
    const TSQuery* query
);

static r_obj* predicate_match(
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

static bool is_predicate_eq_capture(r_obj* x);

static bool check_predicate_eq_capture(
    r_obj* predicate,
    TSQueryMatch* match,
    const char* text,
    uint32_t text_size
);

static r_obj* predicate_eq_string(
    uint32_t capture_name_value_id,
    const char* capture_value,
    uint32_t capture_value_length,
    bool capture_invert
);

static bool is_predicate_eq_string(r_obj* x);

static bool check_predicate_eq_string(
    r_obj* predicate,
    TSQueryMatch* match,
    const char* text,
    uint32_t text_size
);

static r_obj* predicate_match_string(
    uint32_t capture_name_value_id,
    const char* capture_value,
    uint32_t capture_value_length,
    bool capture_invert
);

static bool is_predicate_match_string(r_obj* x);

static bool check_predicate_match_string(
    r_obj* predicate,
    TSQueryMatch* match,
    const char* text,
    uint32_t text_size
);

static r_obj*
capture_indices_for_value_id(TSQueryMatch* match, uint32_t value_id);

static bool r_grepl(r_obj* x, r_obj* pattern);
