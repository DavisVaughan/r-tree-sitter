#ifndef R_TREE_SITTER_UTILS_H
#define R_TREE_SITTER_UTILS_H

#include "rlang.h"
#include "tree_sitter/api.h"

r_obj* r_arg_as_string(r_obj* x, const char* arg);

uint32_t r_ssize_as_uint32(r_ssize x);
r_ssize r_uint32_as_r_ssize(uint32_t x);

uint32_t r_dbl_as_uint32(double x, const char* arg);
double r_uint32_as_dbl(uint32_t x);

TSSymbol r_int_as_TSSymbol(int x, const char* arg);
int r_TSSymbol_as_int(TSSymbol x);

TSFieldId r_int_as_TSFieldId(int x, const char* arg);
int r_TSFieldId_as_int(TSFieldId x);

TSStateId r_int_as_TSStateId(int x, const char* arg);
int r_TSStateId_as_int(TSStateId x);

int r_uint32_as_int(uint32_t x, const char* arg);

bool r_lgl_any_missing(r_obj* x);
bool r_int_any_missing(r_obj* x);
bool r_chr_any_missing(r_obj* x);

bool str_equal(const char* x, const char* y);
bool str_equal_sized(
    const char* x,
    const char* y,
    size_t x_size,
    size_t y_size
);

static inline uint32_t r_uint32_max(uint32_t x, uint32_t y) {
    return (x > y) ? x : y;
}

#endif
