#ifndef R_TREE_SITTER_UTILS_H
#define R_TREE_SITTER_UTILS_H

#include "rlang.h"
#include "tree_sitter/api.h"

r_obj* r_arg_as_string(r_obj* x, const char* arg);

uint32_t r_ssize_as_uint32(r_ssize x);

uint32_t r_dbl_as_uint32(double x, const char* arg);
double r_uint32_as_dbl(uint32_t x);

TSSymbol r_int_as_TSSymbol(int x, const char* arg);
int r_TSSymbol_as_int(TSSymbol x);

TSFieldId r_int_as_TSFieldId(int x, const char* arg);
int r_TSFieldId_as_int(TSFieldId x);

int r_TSStateId_as_int(TSStateId x);

bool r_lgl_any_missing(r_obj* x);
bool r_int_any_missing(r_obj* x);
bool r_chr_any_missing(r_obj* x);

#endif
