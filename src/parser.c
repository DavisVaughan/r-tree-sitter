#include "parser.h"

#include "decl/parser-decl.h"
#include "external-pointer.h"
#include "language.h"
#include "tree.h"
#include "utils.h"

r_obj* ffi_parser_new(
    r_obj* ffi_language,
    r_obj* ffi_timeout,
    r_obj* ffi_included_range_vectors
) {
  TSParser* parser = ts_parser_new();

  const TSLanguage* language = ts_language_from_external_pointer(ffi_language);

  if (!ts_parser_set_language(parser, language)) {
    ts_parser_delete(parser);
    r_abort("Failed to set the parser language.");
  }

  // Validated on R side (0 turns off the timeout)
  const uint64_t timeout = (uint64_t) r_dbl_get(ffi_timeout, 0);
  ts_parser_set_timeout_micros(parser, timeout);

  // Validated on R side
  if (!parser_set_included_ranges(parser, ffi_included_range_vectors)) {
    ts_parser_delete(parser);
    r_abort(
        "Failed to set the `included_ranges`. Make sure they ordered earliest "
        "to latest, and don't overlap."
    );
  }

  return ts_parser_as_external_pointer(parser);
}

static bool
parser_set_included_ranges(TSParser* x, r_obj* included_range_vectors) {
  r_obj* start_bytes = r_list_get(included_range_vectors, 0);
  const double* v_start_bytes = r_dbl_cbegin(start_bytes);

  r_obj* start_rows = r_list_get(included_range_vectors, 1);
  const double* v_start_rows = r_dbl_cbegin(start_rows);

  r_obj* start_columns = r_list_get(included_range_vectors, 2);
  const double* v_start_columns = r_dbl_cbegin(start_columns);

  r_obj* end_bytes = r_list_get(included_range_vectors, 3);
  const double* v_end_bytes = r_dbl_cbegin(end_bytes);

  r_obj* end_rows = r_list_get(included_range_vectors, 4);
  const double* v_end_rows = r_dbl_cbegin(end_rows);

  r_obj* end_columns = r_list_get(included_range_vectors, 5);
  const double* v_end_columns = r_dbl_cbegin(end_columns);

  const r_ssize size = r_length(start_bytes);

  r_obj* ranges = KEEP(r_alloc_raw(size * sizeof(TSRange)));
  TSRange* v_ranges = (TSRange*) r_raw_begin(ranges);

  for (r_ssize i = 0; i < size; ++i) {
    const double start_byte = v_start_bytes[i];
    const double start_row = v_start_rows[i];
    const double start_column = v_start_columns[i];

    const double end_byte = v_end_bytes[i];
    const double end_row = v_end_rows[i];
    const double end_column = v_end_columns[i];

    TSPoint start_point = {
        .row = r_dbl_as_uint32(start_row, "start_row"),
        .column = r_dbl_as_uint32(start_column, "start_column")};
    TSPoint end_point = {
        .row = r_dbl_as_uint32(end_row, "end_row"),
        .column = r_dbl_as_uint32(end_column, "end_column")};

    TSRange range = {
        .start_point = start_point,
        .end_point = end_point,
        .start_byte = start_byte,
        .end_byte = end_byte};

    v_ranges[i] = range;
  }

  bool out =
      ts_parser_set_included_ranges(x, v_ranges, r_ssize_as_uint32(size));

  FREE(1);
  return out;
}

static r_obj* parser_parse(
    r_obj* ffi_x,
    r_obj* ffi_text,
    r_obj* ffi_encoding,
    const TSTree* old_tree
) {
  TSParser* x = ts_parser_from_external_pointer(ffi_x);

  r_obj* c_text = r_chr_get(ffi_text, 0);
  const char* text = r_str_c_string(c_text);
  const uint32_t size = (uint32_t) r_length(c_text);

  const TSInputEncoding encoding = as_encoding(ffi_encoding);

  TSTree* tree =
      ts_parser_parse_string_encoding(x, old_tree, text, size, encoding);

  if (tree == NULL) {
    r_abort("Failed to parse text.");
  }

  return ts_tree_as_external_pointer(tree);
}

r_obj* ffi_parser_parse(r_obj* ffi_x, r_obj* ffi_text, r_obj* ffi_encoding) {
  const TSTree* old_tree = NULL;
  return parser_parse(ffi_x, ffi_text, ffi_encoding, old_tree);
}

r_obj* ffi_parser_reparse(
    r_obj* ffi_x,
    r_obj* ffi_text,
    r_obj* ffi_encoding,
    r_obj* ffi_tree,
    r_obj* ffi_start_byte,
    r_obj* ffi_start_row,
    r_obj* ffi_start_column,
    r_obj* ffi_old_end_byte,
    r_obj* ffi_old_end_row,
    r_obj* ffi_old_end_column,
    r_obj* ffi_new_end_byte,
    r_obj* ffi_new_end_row,
    r_obj* ffi_new_end_column
) {
  // Prepare the old tree by applying the edit in a non-destructive way
  // (i.e. this makes a cheap copy of the tree)
  r_obj* ffi_old_tree = KEEP(ffi_tree_edit(
      ffi_tree,
      ffi_start_byte,
      ffi_start_row,
      ffi_start_column,
      ffi_old_end_byte,
      ffi_old_end_row,
      ffi_old_end_column,
      ffi_new_end_byte,
      ffi_new_end_row,
      ffi_new_end_column
  ));

  const TSTree* old_tree = ts_tree_from_external_pointer(ffi_old_tree);

  r_obj* out = parser_parse(ffi_x, ffi_text, ffi_encoding, old_tree);

  FREE(1);
  return out;
}

r_obj* ts_parser_as_external_pointer(TSParser* x) {
  return new_external_pointer((void*) x, parser_finalize);
}

TSParser* ts_parser_from_external_pointer(r_obj* x) {
  TS_OBJECT_FROM_EXTERNAL_POINTER(x, TSParser*);
}

static void parser_finalize(r_obj* x) {
  if (TYPEOF(x) != EXTPTRSXP) {
    return;
  }

  TSParser* parser = (TSParser*) R_ExternalPtrAddr(x);

  if (parser == NULL) {
    return;
  }

  ts_parser_delete(parser);

  R_ClearExternalPtr(x);
}

static inline TSInputEncoding as_encoding(r_obj* x) {
  const char* text = r_chr_get_c_string(x, 0);

  if (strcmp(text, "UTF-8") == 0) {
    return TSInputEncodingUTF8;
  }
  if (strcmp(text, "UTF-16") == 0) {
    return TSInputEncodingUTF16;
  }

  r_stop_internal("Unknown input encoding string '%s'.", text);
}
