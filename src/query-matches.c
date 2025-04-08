#include "query-matches.h"

#include "decl/query-matches-decl.h"
#include "node.h"
#include "query.h"
#include "utils.h"

// -----------------------------------------------------------------------------
// Matches and Captures

r_obj* ffi_query_matches(
    r_obj* ffi_query,
    r_obj* ffi_capture_names,
    r_obj* ffi_pattern_predicates,
    r_obj* ffi_node,
    r_obj* ffi_tree,
    r_obj* ffi_text,
    r_obj* ffi_start_byte,
    r_obj* ffi_start_row,
    r_obj* ffi_start_column,
    r_obj* ffi_end_byte,
    r_obj* ffi_end_row,
    r_obj* ffi_end_column
) {
  const TSQuery* query = ts_query_from_external_pointer(ffi_query);
  const TSNode* node = ts_node_from_raw(ffi_node);

  r_obj* const* v_capture_names = r_chr_cbegin(ffi_capture_names);

  const char* text = r_chr_get_c_string(ffi_text, 0);
  const uint32_t text_size =
      r_ssize_as_uint32(r_length(r_chr_get(ffi_text, 0)));

  const r_ssize size = r_uint32_as_r_ssize(ts_query_pattern_count(query));

  r_obj* out = KEEP(r_alloc_list(size));

  // Pre-load each pattern container with a dynamically sized list for holding
  // that pattern's matches
  for (r_ssize i = 0; i < size; ++i) {
    struct r_dyn_array* p_elt = r_new_dyn_vector(R_TYPE_list, size);
    r_list_poke(out, i, p_elt->shelter);
  }

  struct TSQueryCursor* cursor = ts_query_cursor_new();

  if (ffi_start_byte != r_null) {
    // Expect that if one is non-null, they all are
    uint32_t start_byte =
        r_dbl_as_uint32(r_dbl_get(ffi_start_byte, 0), "start_byte");
    uint32_t start_row =
        r_dbl_as_uint32(r_dbl_get(ffi_start_row, 0), "start_row");
    uint32_t start_column =
        r_dbl_as_uint32(r_dbl_get(ffi_start_column, 0), "start_column");
    uint32_t end_byte = r_dbl_as_uint32(r_dbl_get(ffi_end_byte, 0), "end_byte");
    uint32_t end_row = r_dbl_as_uint32(r_dbl_get(ffi_end_row, 0), "end_row");
    uint32_t end_column =
        r_dbl_as_uint32(r_dbl_get(ffi_end_column, 0), "end_column");

    TSPoint start_point = {.row = start_row, .column = start_column};
    TSPoint end_point = {.row = end_row, .column = end_column};

    ts_query_cursor_set_byte_range(cursor, start_byte, end_byte);
    ts_query_cursor_set_point_range(cursor, start_point, end_point);
  }

  ts_query_cursor_exec(cursor, query, *node);

  r_obj* elt_names = KEEP(r_alloc_character(2));
  r_chr_poke(elt_names, 0, r_str("name"));
  r_chr_poke(elt_names, 1, r_str("node"));

  TSQueryMatch match;

  while (ts_query_cursor_next_match(cursor, &match)) {
    if (!satisfies_pattern_predicates(
            &match, ffi_pattern_predicates, text, text_size
        )) {
      continue;
    }

    // Figure out which pattern container this match set goes with
    const uint16_t pattern_index = match.pattern_index;

    struct r_dyn_array* p_pattern =
        (struct r_dyn_array*) r_shelter_deref(r_list_get(out, pattern_index));

    const r_ssize count = (r_ssize) match.capture_count;

    // Because we don't call `SET_VECTOR_ELT()` directly and instead call it
    // through `r_dyn_list_push_back()`, rchk gets confused and thinks `elt`
    // isn't protected, even though it definitely is. See PR #34 for more.
    r_obj* elt = KEEP(r_alloc_list(2));
    r_dyn_list_push_back(p_pattern, elt);
    r_attrib_poke_names(elt, elt_names);

    r_obj* names = r_alloc_character(count);
    r_list_poke(elt, 0, names);

    r_obj* nodes = r_alloc_list(count);
    r_list_poke(elt, 1, nodes);

    for (r_ssize i = 0; i < count; ++i) {
      const TSQueryCapture capture = match.captures[i];
      r_chr_poke(names, i, v_capture_names[capture.index]);
      r_list_poke(nodes, i, r_exec_new_node(capture.node, ffi_tree));
    }

    FREE(1);
  }

  ts_query_cursor_delete(cursor);

  // Unwrap all dynamically sized sublists
  for (r_ssize i = 0; i < size; ++i) {
    r_obj* elt = r_list_get(out, i);
    struct r_dyn_array* p_elt = r_shelter_deref(elt);
    r_list_poke(out, i, r_dyn_unwrap(p_elt));
  }

  FREE(2);
  return out;
}

r_obj* ffi_query_captures(
    r_obj* ffi_query,
    r_obj* ffi_capture_names,
    r_obj* ffi_pattern_predicates,
    r_obj* ffi_node,
    r_obj* ffi_tree,
    r_obj* ffi_text,
    r_obj* ffi_start_byte,
    r_obj* ffi_start_row,
    r_obj* ffi_start_column,
    r_obj* ffi_end_byte,
    r_obj* ffi_end_row,
    r_obj* ffi_end_column
) {
  const TSQuery* query = ts_query_from_external_pointer(ffi_query);
  const TSNode* node = ts_node_from_raw(ffi_node);

  r_obj* const* v_capture_names = r_chr_cbegin(ffi_capture_names);

  const char* text = r_chr_get_c_string(ffi_text, 0);
  const uint32_t text_size =
      r_ssize_as_uint32(r_length(r_chr_get(ffi_text, 0)));

  struct r_dyn_array* p_nodes = r_new_dyn_vector(R_TYPE_list, 5);
  KEEP(p_nodes->shelter);

  struct r_dyn_array* p_names = r_new_dyn_vector(R_TYPE_character, 5);
  KEEP(p_names->shelter);

  struct TSQueryCursor* cursor = ts_query_cursor_new();

  if (ffi_start_byte != r_null) {
    // Expect that if one is non-null, they all are
    uint32_t start_byte =
        r_dbl_as_uint32(r_dbl_get(ffi_start_byte, 0), "start_byte");
    uint32_t start_row =
        r_dbl_as_uint32(r_dbl_get(ffi_start_row, 0), "start_row");
    uint32_t start_column =
        r_dbl_as_uint32(r_dbl_get(ffi_start_column, 0), "start_column");
    uint32_t end_byte = r_dbl_as_uint32(r_dbl_get(ffi_end_byte, 0), "end_byte");
    uint32_t end_row = r_dbl_as_uint32(r_dbl_get(ffi_end_row, 0), "end_row");
    uint32_t end_column =
        r_dbl_as_uint32(r_dbl_get(ffi_end_column, 0), "end_column");

    TSPoint start_point = {.row = start_row, .column = start_column};
    TSPoint end_point = {.row = end_row, .column = end_column};

    ts_query_cursor_set_byte_range(cursor, start_byte, end_byte);
    ts_query_cursor_set_point_range(cursor, start_point, end_point);
  }

  ts_query_cursor_exec(cursor, query, *node);

  uint32_t capture_index;
  TSQueryMatch match;

  while (ts_query_cursor_next_capture(cursor, &match, &capture_index)) {
    if (!satisfies_pattern_predicates(
            &match, ffi_pattern_predicates, text, text_size
        )) {
      continue;
    }

    const TSQueryCapture capture = match.captures[capture_index];

    r_dyn_list_push_back(p_nodes, r_exec_new_node(capture.node, ffi_tree));
    r_dyn_chr_push_back(p_names, v_capture_names[capture.index]);
  }

  ts_query_cursor_delete(cursor);

  r_obj* out = KEEP(r_alloc_list(2));
  r_list_poke(out, 0, r_dyn_unwrap(p_names));
  r_list_poke(out, 1, r_dyn_unwrap(p_nodes));

  r_obj* names = r_alloc_character(2);
  r_attrib_poke_names(out, names);
  r_chr_poke(names, 0, r_str("name"));
  r_chr_poke(names, 1, r_str("node"));

  FREE(3);
  return out;
}

static bool satisfies_pattern_predicates(
    TSQueryMatch* match,
    r_obj* pattern_predicates,
    const char* text,
    uint32_t text_size
) {
  r_obj* const* v_pattern_predicates = r_list_cbegin(pattern_predicates);
  r_obj* predicates = v_pattern_predicates[match->pattern_index];

  const r_ssize size = r_length(predicates);
  r_obj* const* v_predicates = r_list_cbegin(predicates);

  for (r_ssize i = 0; i < size; ++i) {
    r_obj* predicate = v_predicates[i];

    if (is_predicate_eq_capture(predicate)) {
      if (check_predicate_eq_capture(predicate, match, text, text_size)) {
        continue;
      }
      return false;
    }

    if (is_predicate_eq_string(predicate)) {
      if (check_predicate_eq_string(predicate, match, text, text_size)) {
        continue;
      }
      return false;
    }

    if (is_predicate_match_string(predicate)) {
      if (check_predicate_match_string(predicate, match, text, text_size)) {
        continue;
      }
      return false;
    }

    r_abort(
        "Unknown `predicate` in pattern %i, predicate %i.",
        match->pattern_index + 1,
        i + 1
    );
  }

  return true;
}

// -----------------------------------------------------------------------------
// Pattern predicates

r_obj* ffi_query_pattern_predicates(r_obj* ffi_query) {
  const TSQuery* query = ts_query_from_external_pointer(ffi_query);

  uint32_t size = ts_query_pattern_count(query);

  r_obj* out = KEEP(r_alloc_list(r_uint32_as_r_ssize(size)));

  for (uint32_t i = 0; i < size; ++i) {
    uint32_t n_total_steps = 0;

    const TSQueryPredicateStep* steps =
        ts_query_predicates_for_pattern(query, i, &n_total_steps);

    // Predicate length is generally 4 steps (3 informational steps + 1 done
    // step), so this gives a decent estimate, and we don't ever want 0 so we
    // add 1 to be safe
    const r_ssize capacity = r_uint32_as_r_ssize(n_total_steps) / 4 + 1;
    struct r_dyn_array* p_predicates = r_new_dyn_vector(R_TYPE_list, capacity);
    r_list_poke(out, i, p_predicates->shelter);

    uint32_t step = 0;

    while (step < n_total_steps) {
      if (steps == NULL) {
        // Should be an error if this ever happens
        r_abort("Failed to extract predicates for pattern %i.", i + 1);
      }

      if (steps->type != TSQueryPredicateStepTypeString) {
        r_abort(
            "All predicates must start with a string describing the "
            "predicate type. In pattern %i, a predicate did not start with a "
            "string.",
            i + 1
        );
      }

      // Get this predicate's type
      uint32_t predicate_type_size = 0;
      const char* predicate_type = ts_query_string_value_for_id(
          query, steps->value_id, &predicate_type_size
      );

      // Get the number of steps in this predicate
      uint32_t n_predicate_steps = 0;
      while ((steps + n_predicate_steps)->type != TSQueryPredicateStepTypeDone
      ) {
        n_predicate_steps++;
      }

      // Dispatch to helpers based on known predicate types
      if (str_equal(predicate_type, "eq?") ||
          str_equal(predicate_type, "not-eq?") ||
          str_equal(predicate_type, "any-eq?") ||
          str_equal(predicate_type, "any-not-eq?")) {
        r_dyn_list_push_back(
            p_predicates,
            predicate_eq(predicate_type, n_predicate_steps, steps, query)
        );
      } else if (str_equal(predicate_type, "match?") ||
                 str_equal(predicate_type, "not-match?") ||
                 str_equal(predicate_type, "any-match?") ||
                 str_equal(predicate_type, "any-not-match?")) {
        r_dyn_list_push_back(
            p_predicates,
            predicate_match(predicate_type, n_predicate_steps, steps, query)
        );
      } else {
        r_abort(
            "Unknown predicate type '%s' in pattern %i.", predicate_type, i + 1
        );
      }

      // Move to the start of the next predicate.
      // `+ 1` to include the `Done` step of the current predicate.
      step += n_predicate_steps + 1;
      steps += n_predicate_steps + 1;
    }

    r_list_poke(out, i, r_dyn_unwrap(p_predicates));
  }

  FREE(1);
  return out;
}

static r_obj* predicate_eq(
    const char* predicate_type,
    uint32_t n_predicate_steps,
    const TSQueryPredicateStep* steps,
    const TSQuery* query
) {
  if (n_predicate_steps != 3) {
    // Number of arguments doesn't include predicate type step
    uint32_t expected = 2;
    uint32_t actual = n_predicate_steps - 1;

    r_abort(
        "Expected %i arguments for '%s' predicate. Received %i arguments.",
        expected,
        predicate_type,
        actual
    );
  }

  const bool capture_invert = str_equal(predicate_type, "not-eq?") ||
                              str_equal(predicate_type, "any-not-eq?");
  const bool capture_any = str_equal(predicate_type, "any-eq?") ||
                           str_equal(predicate_type, "any-not-eq?");
  const TSQueryPredicateStepType capture_name_type = steps[1].type;
  const uint32_t capture_name_value_id = steps[1].value_id;
  const TSQueryPredicateStepType capture_type = steps[2].type;
  const uint32_t capture_value_id = steps[2].value_id;

  if (capture_name_type != TSQueryPredicateStepTypeCapture) {
    r_abort(
        "First argument to '%s' predicate must be a capture name.",
        predicate_type
    );
  }

  switch (capture_type) {
    case TSQueryPredicateStepTypeCapture: {
      return predicate_eq_capture(
          capture_name_value_id, capture_value_id, capture_invert, capture_any
      );
    }
    case TSQueryPredicateStepTypeString: {
      uint32_t capture_value_length = 0;
      const char* capture_value = ts_query_string_value_for_id(
          query, capture_value_id, &capture_value_length
      );
      return predicate_eq_string(
          capture_name_value_id,
          capture_value,
          capture_value_length,
          capture_invert,
          capture_any
      );
    }
    case TSQueryPredicateStepTypeDone: {
      r_stop_internal("Unexpected `Done` step.");
    }
    default: {
      r_stop_unreachable();
    }
  }
}

static r_obj* predicate_match(
    const char* predicate_type,
    uint32_t n_predicate_steps,
    const TSQueryPredicateStep* steps,
    const TSQuery* query
) {
  if (n_predicate_steps != 3) {
    // Number of arguments doesn't include predicate type step
    uint32_t expected = 2;
    uint32_t actual = n_predicate_steps - 1;

    r_abort(
        "Expected %i arguments for '%s' predicate. Received %i arguments.",
        expected,
        predicate_type,
        actual
    );
  }

  const bool capture_invert = str_equal(predicate_type, "not-match?") ||
                              str_equal(predicate_type, "any-not-match?");
  const bool capture_any = str_equal(predicate_type, "any-match?") ||
                           str_equal(predicate_type, "any-not-match?");
  const TSQueryPredicateStepType capture_name_type = steps[1].type;
  const uint32_t capture_name_value_id = steps[1].value_id;
  const TSQueryPredicateStepType capture_type = steps[2].type;
  const uint32_t capture_value_id = steps[2].value_id;

  if (capture_name_type != TSQueryPredicateStepTypeCapture) {
    r_abort(
        "First argument to '%s' predicate must be a capture name.",
        predicate_type
    );
  }
  if (capture_type != TSQueryPredicateStepTypeString) {
    r_abort(
        "Second argument to '%s' predicate must be a regex string.",
        predicate_type
    );
  }

  uint32_t capture_value_length = 0;

  const char* capture_value = ts_query_string_value_for_id(
      query, capture_value_id, &capture_value_length
  );

  return predicate_match_string(
      capture_name_value_id,
      capture_value,
      capture_value_length,
      capture_invert,
      capture_any
  );
}

// -----------------------------------------------------------------------------
// Predicate - `#eq?` with capture

static r_obj* predicate_eq_capture(
    uint32_t capture_name_value_id,
    uint32_t capture_value_id,
    bool capture_invert,
    bool capture_any
) {
  r_obj* out = KEEP(r_alloc_list(4));
  r_list_poke(out, 0, r_dbl(r_uint32_as_dbl(capture_name_value_id)));
  r_list_poke(out, 1, r_dbl(r_uint32_as_dbl(capture_value_id)));
  r_list_poke(out, 2, r_lgl(capture_invert));
  r_list_poke(out, 3, r_lgl(capture_any));

  r_obj* names = r_alloc_character(4);
  r_attrib_poke_names(out, names);
  r_chr_poke(names, 0, r_str("capture_name_value_id"));
  r_chr_poke(names, 1, r_str("capture_value_id"));
  r_chr_poke(names, 2, r_str("capture_invert"));
  r_chr_poke(names, 3, r_str("capture_any"));

  r_attrib_poke_class(out, r_chr("tree_sitter_predicate_eq_capture"));

  FREE(1);
  return out;
}

static bool is_predicate_eq_capture(r_obj* x) {
  return r_inherits(x, "tree_sitter_predicate_eq_capture");
}

static bool check_predicate_eq_capture(
    r_obj* predicate,
    TSQueryMatch* match,
    const char* text,
    uint32_t text_size
) {
  const uint32_t capture_name_value_id = r_dbl_as_uint32(
      r_dbl_get(r_list_get(predicate, 0), 0), "capture_name_value_id"
  );

  const uint32_t capture_value_id = r_dbl_as_uint32(
      r_dbl_get(r_list_get(predicate, 1), 0), "capture_value_id"
  );

  const bool capture_invert =
      r_arg_as_bool(r_list_get(predicate, 2), "capture_invert");
  const bool capture_any =
      r_arg_as_bool(r_list_get(predicate, 3), "capture_any");

  // First yank out all `node` indices that match the capture `value_id`s.
  // Relevant when there are "zero or more" or "one or more" predicate types.
  r_obj* capture_name_indices =
      KEEP(capture_indices_for_value_id(match, capture_name_value_id));
  r_obj* capture_indices =
      KEEP(capture_indices_for_value_id(match, capture_value_id));

  const int* v_capture_name_indices = r_int_cbegin(capture_name_indices);
  const int* v_capture_indices = r_int_cbegin(capture_indices);

  // If the lengths differ, then we fail the predicate check
  const r_ssize size = r_length(capture_name_indices);

  if (size != r_length(capture_indices)) {
    FREE(2);
    return false;
  }

  bool any_passed = false;
  bool all_passed = true;

  // Go through each `capture_name` and `capture` pair and check that the
  // captured text exactly matches (or doesn't match, if using
  // `capture_invert`).
  for (r_ssize i = 0; i < size; ++i) {
    const int capture_name_index = v_capture_name_indices[i];
    const int capture_index = v_capture_indices[i];

    const TSQueryCapture capture_name = match->captures[capture_name_index];
    const TSQueryCapture capture = match->captures[capture_index];

    // Note that `capture_name_text` and `capture_text` don't have a nul
    // terminator, so we use `str_equal_sized()`
    uint32_t capture_name_size = 0;
    const char* capture_name_text =
        node_text(capture_name.node, text, text_size, &capture_name_size);

    uint32_t capture_size = 0;
    const char* capture_text =
        node_text(capture.node, text, text_size, &capture_size);

    // Exact match
    const bool passed = str_equal_sized(
        capture_name_text,
        capture_text,
        (size_t) capture_name_size,
        (size_t) capture_size
    );

    const bool done = apply_predicate_result(
        passed, capture_invert, capture_any, &any_passed, &all_passed
    );

    if (done) {
      break;
    }
  }

  FREE(2);
  return capture_any ? any_passed : all_passed;
}

static r_obj*
capture_indices_for_value_id(TSQueryMatch* match, uint32_t value_id) {
  uint16_t size = match->capture_count;
  const TSQueryCapture* captures = match->captures;

  r_ssize out_size = 0;

  for (uint16_t i = 0; i < size; ++i) {
    out_size += captures[i].index == value_id;
  }

  r_ssize j = 0;
  r_obj* out = KEEP(r_alloc_integer(out_size));
  int* v_out = r_int_begin(out);

  for (uint16_t i = 0; i < size; ++i) {
    if (captures[i].index == value_id) {
      v_out[j] = (int) i;
      ++j;
    }
  }

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------
// Predicate - `#eq?` with string

static r_obj* predicate_eq_string(
    uint32_t capture_name_value_id,
    const char* capture_value,
    uint32_t capture_value_length,
    bool capture_invert,
    bool capture_any
) {
  r_obj* capture_value_sexp = KEEP(r_alloc_character(1));
  r_chr_poke(
      capture_value_sexp,
      0,
      Rf_mkCharLenCE(
          capture_value, r_uint32_as_r_ssize(capture_value_length), CE_UTF8
      )
  );

  r_obj* out = KEEP(r_alloc_list(4));
  r_list_poke(out, 0, r_dbl(r_uint32_as_dbl(capture_name_value_id)));
  r_list_poke(out, 1, capture_value_sexp);
  r_list_poke(out, 2, r_lgl(capture_invert));
  r_list_poke(out, 3, r_lgl(capture_any));

  r_obj* names = r_alloc_character(4);
  r_attrib_poke_names(out, names);
  r_chr_poke(names, 0, r_str("capture_name_value_id"));
  r_chr_poke(names, 1, r_str("capture_value"));
  r_chr_poke(names, 2, r_str("capture_invert"));
  r_chr_poke(names, 3, r_str("capture_any"));

  r_attrib_poke_class(out, r_chr("tree_sitter_predicate_eq_string"));

  FREE(2);
  return out;
}

static bool is_predicate_eq_string(r_obj* x) {
  return r_inherits(x, "tree_sitter_predicate_eq_string");
}

static bool check_predicate_eq_string(
    r_obj* predicate,
    TSQueryMatch* match,
    const char* text,
    uint32_t text_size
) {
  const uint32_t capture_name_value_id = r_dbl_as_uint32(
      r_dbl_get(r_list_get(predicate, 0), 0), "capture_name_value_id"
  );

  // Note `capture_value_size` is size without nul terminator
  r_obj* capture_value_sexp = r_list_get(predicate, 1);
  const char* capture_value = r_chr_get_c_string(capture_value_sexp, 0);
  const uint32_t capture_value_size =
      r_ssize_as_uint32(r_length(r_chr_get(capture_value_sexp, 0)));

  const bool capture_invert =
      r_arg_as_bool(r_list_get(predicate, 2), "capture_invert");
  const bool capture_any =
      r_arg_as_bool(r_list_get(predicate, 3), "capture_any");

  const uint16_t capture_count = match->capture_count;

  bool any_passed = false;
  bool all_passed = true;

  // Go through each `capture` that matches this predicate
  // `capture_name_value_id` and check that the captured `node`'s text exactly
  // matches the `capture_value` (or doesn't match, if using `capture_invert`).
  for (uint16_t i = 0; i < capture_count; ++i) {
    const TSQueryCapture capture = match->captures[i];

    if (capture.index != capture_name_value_id) {
      // Nothing to do
      continue;
    }

    // Extract out location of this `node`s text
    // Note that `elt` does not have a nul terminator, so we use
    // `str_equal_sized()`
    uint32_t elt_size = 0;
    const char* elt = node_text(capture.node, text, text_size, &elt_size);

    // Exact match
    const bool passed = str_equal_sized(
        elt, capture_value, (size_t) elt_size, (size_t) capture_value_size
    );

    const bool done = apply_predicate_result(
        passed, capture_invert, capture_any, &any_passed, &all_passed
    );

    if (done) {
      break;
    }
  }

  return capture_any ? any_passed : all_passed;
}

// -----------------------------------------------------------------------------
// Predicate - `#match?` with regex string

static r_obj* predicate_match_string(
    uint32_t capture_name_value_id,
    const char* capture_value,
    uint32_t capture_value_length,
    bool capture_invert,
    bool capture_any
) {
  r_obj* capture_value_sexp = KEEP(r_alloc_character(1));
  r_chr_poke(
      capture_value_sexp,
      0,
      Rf_mkCharLenCE(
          capture_value, r_uint32_as_r_ssize(capture_value_length), CE_UTF8
      )
  );

  r_obj* out = KEEP(r_alloc_list(4));
  r_list_poke(out, 0, r_dbl(r_uint32_as_dbl(capture_name_value_id)));
  r_list_poke(out, 1, capture_value_sexp);
  r_list_poke(out, 2, r_lgl(capture_invert));
  r_list_poke(out, 3, r_lgl(capture_any));

  r_obj* names = r_alloc_character(4);
  r_attrib_poke_names(out, names);
  r_chr_poke(names, 0, r_str("capture_name_value_id"));
  r_chr_poke(names, 1, r_str("capture_value"));
  r_chr_poke(names, 2, r_str("capture_invert"));
  r_chr_poke(names, 3, r_str("capture_any"));

  r_attrib_poke_class(out, r_chr("tree_sitter_predicate_match_string"));

  FREE(2);
  return out;
}

static bool is_predicate_match_string(r_obj* x) {
  return r_inherits(x, "tree_sitter_predicate_match_string");
}

static bool check_predicate_match_string(
    r_obj* predicate,
    TSQueryMatch* match,
    const char* text,
    uint32_t text_size
) {
  const uint32_t capture_name_value_id = r_dbl_as_uint32(
      r_dbl_get(r_list_get(predicate, 0), 0), "capture_name_value_id"
  );

  r_obj* pattern = r_list_get(predicate, 1);

  const bool capture_invert =
      r_arg_as_bool(r_list_get(predicate, 2), "capture_invert");
  const bool capture_any =
      r_arg_as_bool(r_list_get(predicate, 3), "capture_any");

  const uint16_t capture_count = match->capture_count;

  r_obj* x = KEEP(r_alloc_character(1));

  bool any_passed = false;
  bool all_passed = true;

  // Go through each `capture` that matches this predicate
  // `capture_name_value_id` and check that the captured `node`'s text regex
  // matches the `capture_value` (or doesn't match, if using
  // `capture_invert`).
  for (uint16_t i = 0; i < capture_count; ++i) {
    const TSQueryCapture capture = match->captures[i];

    if (capture.index != capture_name_value_id) {
      // Nothing to do
      continue;
    }

    // Extract out location of this `node`s text
    uint32_t elt_size = 0;
    const char* elt = node_text(capture.node, text, text_size, &elt_size);

    r_chr_poke(
        x,
        0,
        Rf_mkCharLenCE(elt, r_uint32_as_int(elt_size, "elt_size"), CE_UTF8)
    );

    const bool passed = r_grepl(x, pattern);

    const bool done = apply_predicate_result(
        passed, capture_invert, capture_any, &any_passed, &all_passed
    );

    if (done) {
      break;
    }
  }

  FREE(1);
  return capture_any ? any_passed : all_passed;
}

static bool r_grepl(r_obj* x, r_obj* pattern) {
  static SEXP call = NULL;
  static SEXP env = NULL;
  static SEXP x_sym = NULL;
  static SEXP pattern_sym = NULL;

  if (call == NULL) {
    x_sym = r_sym("x");
    pattern_sym = r_sym("pattern");

    SEXP fn = r_env_find(r_envs.base, r_sym("grepl"));
    call = r_call3(fn, pattern_sym, x_sym);
    r_preserve(call);

    env = r_alloc_environment(2, r_envs.global);
    r_preserve(env);
  }

  r_env_poke(env, x_sym, x);
  r_env_poke(env, pattern_sym, pattern);

  r_obj* out = KEEP(r_eval(call, env));

  if (!r_is_bool(out)) {
    r_stop_internal(
        "`grepl()` call should have returned a single `TRUE` or `FALSE`."
    );
  }

  FREE(1);
  return r_as_bool(out);
}

// -----------------------------------------------------------------------------
// Predicate helpers

// Finalizes the result of applying a single predicate to a single capture.
// Returns a boolean indicating whether or not we can finish predicate checks
// early.
static bool apply_predicate_result(
    bool passed,
    bool capture_invert,
    bool capture_any,
    bool* any_passed,
    bool* all_passed
) {
  if (capture_invert) {
    passed = !passed;
  }

  // Apply this result
  *any_passed |= passed;
  *all_passed &= passed;

  // Figure out if we can finish early or not.
  // If we are doing `capture_all` and someone doesn't pass, we are done.
  return !capture_any && !passed;
}
