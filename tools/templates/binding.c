#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "tree_sitter/api.h"

// From `parser.c`
extern const TSLanguage* tree_sitter_TEMPLATE_NAME_LANGUAGE(void);

SEXP ffi_language_TEMPLATE_NAME_LANGUAGE(void) {
  static SEXP language = NULL;

  if (language == NULL) {
    const TSLanguage* pointer = tree_sitter_TEMPLATE_NAME_LANGUAGE();
    language = R_MakeExternalPtr((void*) pointer, R_NilValue, R_NilValue);
    R_PreserveObject(language);
  }

  return language;
}
