#include "query.h"

#include "decl/query-decl.h"
#include "external-pointer.h"
#include "language.h"
#include "node.h"
#include "utils.h"

r_obj* ffi_query_new(r_obj* ffi_source, r_obj* ffi_language) {
  const TSLanguage* language = ts_language_from_external_pointer(ffi_language);

  r_obj* source = r_chr_get(ffi_source, 0);
  const char* source_c = r_str_c_string(source);
  uint32_t source_size = r_ssize_as_uint32(r_length(source));

  uint32_t error_offset = 0;
  TSQueryError error_type = TSQueryErrorNone;

  TSQuery* query =
      ts_query_new(language, source_c, source_size, &error_offset, &error_type);

  if (query == NULL) {
    return query_error(error_offset, error_type);
  } else {
    return ts_query_as_external_pointer(query);
  }
}

r_obj* ffi_query_pattern_count(r_obj* ffi_query) {
  const TSQuery* query = ts_query_from_external_pointer(ffi_query);
  uint32_t out = ts_query_pattern_count(query);
  return r_dbl(r_uint32_as_dbl(out));
}

r_obj* ffi_query_capture_count(r_obj* ffi_query) {
  const TSQuery* query = ts_query_from_external_pointer(ffi_query);
  uint32_t out = ts_query_capture_count(query);
  return r_dbl(r_uint32_as_dbl(out));
}

r_obj* ffi_query_string_count(r_obj* ffi_query) {
  const TSQuery* query = ts_query_from_external_pointer(ffi_query);
  uint32_t out = ts_query_string_count(query);
  return r_dbl(r_uint32_as_dbl(out));
}

r_obj* ffi_query_capture_names(r_obj* ffi_query) {
  const TSQuery* query = ts_query_from_external_pointer(ffi_query);

  uint32_t size = ts_query_capture_count(query);

  r_obj* out = KEEP(r_alloc_character(r_uint32_as_r_ssize(size)));

  for (uint32_t i = 0; i < size; ++i) {
    uint32_t length = 0;
    const char* name = ts_query_capture_name_for_id(query, i, &length);
    r_chr_poke(
        out,
        r_uint32_as_r_ssize(i),
        Rf_mkCharLenCE(name, r_uint32_as_int(length, "length"), CE_UTF8)
    );
  }

  FREE(1);
  return out;
}

r_obj* ffi_query_start_byte_for_pattern(r_obj* ffi_query, r_obj* ffi_i) {
  const TSQuery* query = ts_query_from_external_pointer(ffi_query);

  // Validated on R side to be positive whole double of length 1, 1-indexed
  uint32_t i = r_dbl_as_uint32(r_dbl_get(ffi_i, 0) - 1, "i");

  uint32_t out = ts_query_start_byte_for_pattern(query, i);

  return r_dbl(r_uint32_as_dbl(out));
}

static r_obj* query_error(uint32_t error_offset, TSQueryError error_type) {
  const char* error_type_name = NULL;

  switch (error_type) {
    case TSQueryErrorNone:
      r_stop_internal("Unexpected `None` case for `TSQueryError`.");
    case TSQueryErrorSyntax: {
      error_type_name = "Syntax";
      break;
    }
    case TSQueryErrorNodeType: {
      error_type_name = "Node type";
      break;
    }
    case TSQueryErrorField: {
      error_type_name = "Field";
      break;
    }
    case TSQueryErrorCapture: {
      error_type_name = "Capture";
      break;
    }
    case TSQueryErrorStructure: {
      error_type_name = "Structure";
      break;
    }
    case TSQueryErrorLanguage: {
      error_type_name = "Language";
      break;
    }
  }

  r_obj* out = KEEP(r_alloc_list(2));

  r_obj* names = r_alloc_character(2);
  r_attrib_poke_names(out, names);
  r_chr_poke(names, 0, r_str("offset"));
  r_chr_poke(names, 1, r_str("type"));

  r_list_poke(out, 0, r_dbl(error_offset + 1));
  r_list_poke(out, 1, r_chr(error_type_name));

  FREE(1);
  return out;
}

r_obj* ts_query_as_external_pointer(TSQuery* x) {
  return new_external_pointer((void*) x, query_finalize);
}

TSQuery* ts_query_from_external_pointer(r_obj* x) {
  TS_OBJECT_FROM_EXTERNAL_POINTER(x, TSQuery*);
}

static void query_finalize(r_obj* x) {
  if (r_typeof(x) != R_TYPE_pointer) {
    return;
  }

  TSQuery* query = (TSQuery*) R_ExternalPtrAddr(x);

  if (query == NULL) {
    return;
  }

  ts_query_delete(query);

  R_ClearExternalPtr(x);
}

// -----------------------------------------------------------------------------
// Captures

r_obj* ffi_query_captures(
    r_obj* ffi_query,
    r_obj* ffi_capture_names,
    r_obj* ffi_node,
    r_obj* ffi_start_byte,
    r_obj* ffi_start_row,
    r_obj* ffi_start_column,
    r_obj* ffi_end_byte,
    r_obj* ffi_end_row,
    r_obj* ffi_end_column
) {
  const TSQuery* query = ts_query_from_external_pointer(ffi_query);
  const TSNode* node = ts_node_from_raw(ffi_node);

  const r_ssize size = r_length(ffi_capture_names);
  r_obj* const* v_capture_names = r_chr_cbegin(ffi_capture_names);

  r_obj* out = KEEP(r_alloc_list(size));
  r_attrib_poke_names(out, ffi_capture_names);

  // Pre-load each `out` element with a dynamically sized list
  for (r_ssize i = 0; i < size; ++i) {
    struct r_dyn_array* p_elt = r_new_dyn_vector(R_TYPE_list, 2);
    r_list_poke(out, i, p_elt->shelter);
  }

  // TODO: Use globally allocated query cursor?
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
    const TSQueryCapture capture = match.captures[capture_index];

    // Loop over `v_capture_names`, figure out where to push the `node`
    r_obj* name = v_capture_names[capture.index];

    for (r_ssize i = 0; i < size; ++i) {
      if (v_capture_names[i] == name) {
        r_obj* elt = r_list_get(out, i);
        struct r_dyn_array* p_elt = r_shelter_deref(elt);
        r_dyn_list_push_back(p_elt, ts_node_as_raw(capture.node));
        break;
      }
    }
  }

  ts_query_cursor_delete(cursor);

  // Unwrap all dynamically sized sublists
  for (r_ssize i = 0; i < size; ++i) {
    r_obj* elt = r_list_get(out, i);
    struct r_dyn_array* p_elt = r_shelter_deref(elt);
    r_list_poke(out, i, r_dyn_unwrap(p_elt));
  }

  FREE(1);
  return out;
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
    KEEP(p_predicates->shelter);

    uint32_t step = 0;

    while (step < n_total_steps) {
      if (steps == NULL) {
        // Should be an error if this ever happens
        r_abort("Failed to extract predicates for pattern %i.", i + 1);
      }

      if (steps->type != TSQueryPredicateStepTypeString) {
        r_abort(
            "All predicates must start with a string describing the predicate "
            "type. In pattern %i, a predicate did not start with a string.",
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
          str_equal(predicate_type, "not-eq?")) {
        r_obj* predicate =
            predicate_eq(predicate_type, n_predicate_steps, steps, query);
        r_dyn_list_push_back(p_predicates, predicate);
      } else if (str_equal(predicate_type, "match?") || str_equal(predicate_type, "not-match?")) {
        r_obj* predicate =
            predicate_match(predicate_type, n_predicate_steps, steps, query);
        r_dyn_list_push_back(p_predicates, predicate);
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

    r_obj* predicates = r_dyn_unwrap(p_predicates);
    r_list_poke(out, i, predicates);
    FREE(1);
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

  const bool capture_invert = str_equal(predicate_type, "not-eq?");
  const TSQueryPredicateStepType capture_name_type = steps[1].type;
  const uint32_t capture_name_value_id = steps[1].value_id;
  const TSQueryPredicateStepType capture_type = steps[2].type;
  const uint32_t capture_value_id = steps[2].value_id;

  if (capture_name_type != TSQueryPredicateStepTypeCapture) {
    r_abort("First argument to '%s' predicate must be a capture name.");
  }

  switch (capture_type) {
    case TSQueryPredicateStepTypeCapture: {
      return predicate_eq_capture(
          capture_name_value_id, capture_value_id, capture_invert
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
          capture_invert
      );
    }
    case TSQueryPredicateStepTypeDone: {
      r_stop_internal("Unexpected `Done` step.");
    }
  }
}

static r_obj* predicate_eq_capture(
    uint32_t capture_name_value_id,
    uint32_t capture_value_id,
    bool capture_invert
) {
  r_obj* out = KEEP(r_alloc_list(3));
  r_list_poke(out, 0, r_dbl(r_uint32_as_dbl(capture_name_value_id)));
  r_list_poke(out, 1, r_dbl(r_uint32_as_dbl(capture_value_id)));
  r_list_poke(out, 2, r_lgl(capture_invert));

  r_obj* names = r_alloc_character(3);
  r_attrib_poke_names(out, names);
  r_chr_poke(names, 0, r_str("capture_name_value_id"));
  r_chr_poke(names, 1, r_str("capture_value_id"));
  r_chr_poke(names, 2, r_str("capture_invert"));

  r_attrib_poke_class(out, r_chr("tree_sitter_predicate_eq_capture"));

  FREE(1);
  return out;
}

static r_obj* predicate_eq_string(
    uint32_t capture_name_value_id,
    const char* capture_value,
    uint32_t capture_value_length,
    bool capture_invert
) {
  r_obj* capture_value_sexp = KEEP(r_alloc_character(1));
  r_chr_poke(
      capture_value_sexp,
      0,
      Rf_mkCharLenCE(
          capture_value, r_uint32_as_r_ssize(capture_value_length), CE_UTF8
      )
  );

  r_obj* out = KEEP(r_alloc_list(3));
  r_list_poke(out, 0, r_dbl(r_uint32_as_dbl(capture_name_value_id)));
  r_list_poke(out, 1, capture_value_sexp);
  r_list_poke(out, 2, r_lgl(capture_invert));

  r_obj* names = r_alloc_character(3);
  r_attrib_poke_names(out, names);
  r_chr_poke(names, 0, r_str("capture_name_value_id"));
  r_chr_poke(names, 1, r_str("capture_value"));
  r_chr_poke(names, 2, r_str("capture_invert"));

  r_attrib_poke_class(out, r_chr("tree_sitter_predicate_eq_string"));

  FREE(2);
  return out;
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

  const bool capture_invert = str_equal(predicate_type, "not-match?");
  const TSQueryPredicateStepType capture_name_type = steps[1].type;
  const uint32_t capture_name_value_id = steps[1].value_id;
  const TSQueryPredicateStepType capture_type = steps[2].type;
  const uint32_t capture_value_id = steps[2].value_id;

  if (capture_name_type != TSQueryPredicateStepTypeCapture) {
    r_abort("First argument to '%s' predicate must be a capture name.");
  }
  if (capture_type != TSQueryPredicateStepTypeString) {
    r_abort("Second argument to '%s' predicate must be a regex string.");
  }

  uint32_t capture_value_length = 0;

  const char* capture_value = ts_query_string_value_for_id(
      query, capture_value_id, &capture_value_length
  );

  return predicate_match_string(
      capture_name_value_id, capture_value, capture_value_length, capture_invert
  );
}

static r_obj* predicate_match_string(
    uint32_t capture_name_value_id,
    const char* capture_value,
    uint32_t capture_value_length,
    bool capture_invert
) {
  r_obj* capture_value_sexp = KEEP(r_alloc_character(1));
  r_chr_poke(
      capture_value_sexp,
      0,
      Rf_mkCharLenCE(
          capture_value, r_uint32_as_r_ssize(capture_value_length), CE_UTF8
      )
  );

  r_obj* out = KEEP(r_alloc_list(3));
  r_list_poke(out, 0, r_dbl(r_uint32_as_dbl(capture_name_value_id)));
  r_list_poke(out, 1, capture_value_sexp);
  r_list_poke(out, 2, r_lgl(capture_invert));

  r_obj* names = r_alloc_character(3);
  r_attrib_poke_names(out, names);
  r_chr_poke(names, 0, r_str("capture_name_value_id"));
  r_chr_poke(names, 1, r_str("capture_value"));
  r_chr_poke(names, 2, r_str("capture_invert"));

  r_attrib_poke_class(out, r_chr("tree_sitter_predicate_match_string"));

  FREE(2);
  return out;
}
