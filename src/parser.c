#include "parser.h"

#include "decl/parser-decl.h"
#include "external-pointer.h"
#include "language.h"
#include "tree.h"

r_obj* ffi_parser_initialize(void) {
  TSParser* parser = ts_parser_new();
  return ts_parser_as_external_pointer(parser);
}

r_obj* ffi_parser_set_language(r_obj* ffi_x, r_obj* ffi_language) {
  TSParser* x = ts_parser_from_external_pointer(ffi_x);
  const TSLanguage* language = ts_language_from_external_pointer(ffi_language);

  const bool ok = ts_parser_set_language(x, language);
  if (!ok) {
    r_abort("Failed to set language of parser.");
  }

  return r_null;
}

r_obj* ffi_parser_parse(r_obj* ffi_x, r_obj* ffi_text, r_obj* ffi_tree) {
  TSParser* x = ts_parser_from_external_pointer(ffi_x);

  r_obj* c_text = r_chr_get(ffi_text, 0);
  const char* text = r_str_c_string(c_text);
  uint32_t size = (uint32_t) r_length(c_text);

  TSTree* old_tree =
      ffi_tree == r_null ? NULL : ts_tree_from_external_pointer(ffi_tree);

  // Called `enc2utf8()` on the frontend side
  const TSInputEncoding encoding = TSInputEncodingUTF8;

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
