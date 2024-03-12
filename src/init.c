#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <stdlib.h>  // for NULL

static const R_CallMethodDef CallEntries[] = {{NULL, NULL, 0}};

void R_init_treesitter(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
