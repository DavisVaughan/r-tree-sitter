#include "parser.h"

#include "decl/parser.h"
#include "external-pointer.h"
#include "language.h"

r_obj* ffi_parser_initialize(void) {
  TSParser* parser = ts_parser_new();
  return new_external_pointer((void*) parser, parser_finalize);
}

r_obj* ffi_parser_set_language(r_obj* ffi_parser, r_obj* ffi_language) {
  TSParser* parser = ts_parser_from_external_pointer(ffi_parser);
  const TSLanguage* language = ts_language_from_external_pointer(ffi_language);

  const bool ok = ts_parser_set_language(parser, language);
  if (!ok) {
    r_abort("Failed to set language of parser.");
  }

  return r_null;
}

TSParser* ts_parser_from_external_pointer(r_obj* x) {
  if (r_typeof(x) != R_TYPE_pointer) {
    r_abort("`x` must be an external pointer.");
  }

  TSParser* out = (TSParser*) R_ExternalPtrAddr(x);

  if (out == NULL) {
    r_abort("`x` must point to a `TSParser`.");
  }

  return out;
}

static void parser_finalize(r_obj* pointer) {
  if (TYPEOF(pointer) != EXTPTRSXP) {
    return;
  }

  TSParser* parser = (TSParser*) R_ExternalPtrAddr(pointer);

  if (parser == NULL) {
    return;
  }

  ts_parser_delete(parser);

  R_ClearExternalPtr(pointer);
}
