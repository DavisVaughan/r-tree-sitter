#include "parser.h"

#include "decl/parser-decl.h"
#include "external-pointer.h"
#include "language.h"
#include "tree.h"

r_obj* ffi_parser_new(
    r_obj* ffi_language,
    r_obj* ffi_timeout,
    r_obj* ffi_included_ranges
) {
  TSParser* parser = ts_parser_new();

  const TSLanguage* language = ts_language_from_external_pointer(ffi_language);

  if (!ts_parser_set_language(parser, language)) {
    ts_parser_delete(parser);
    r_abort("Failed to set the parser language.");
  }

  if (ffi_timeout != r_null) {
    // Validated on R side
    const uint64_t timeout = (uint64_t) r_dbl_get(ffi_timeout, 0);
    ts_parser_set_timeout_micros(parser, timeout);
  }

  if (ffi_included_ranges != r_null) {
    // Validated on R side
    ts_parser_delete(parser);
    r_stop_internal("TODO: Add support for `included_ranges`.");
  }

  return ts_parser_as_external_pointer(parser);
}

r_obj* ffi_parser_parse(
    r_obj* ffi_x,
    r_obj* ffi_text,
    r_obj* ffi_encoding,
    r_obj* ffi_tree
) {
  TSParser* x = ts_parser_from_external_pointer(ffi_x);

  r_obj* c_text = r_chr_get(ffi_text, 0);
  const char* text = r_str_c_string(c_text);
  const uint32_t size = (uint32_t) r_length(c_text);

  const TSTree* old_tree =
      ffi_tree == r_null ? NULL : ts_tree_from_external_pointer(ffi_tree);

  const TSInputEncoding encoding = as_encoding(ffi_encoding);

  TSTree* tree =
      ts_parser_parse_string_encoding(x, old_tree, text, size, encoding);

  if (tree == NULL) {
    r_abort("Failed to parse text.");
  }

  return ts_tree_as_external_pointer(tree);
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
