#include <stddef.h>
#include <stdlib.h>

#include "rlang.h"
#include "tree_sitter/api.h"

static void* r_tree_sitter_malloc(size_t size) {
  void* result = malloc(size);
  if (size > 0 && !result) {
    r_abort("tree-sitter failed to allocate %zu bytes", size);
  }
  return result;
}

static void* r_tree_sitter_calloc(size_t count, size_t size) {
  void* result = calloc(count, size);
  if (count > 0 && !result) {
    r_abort("tree-sitter failed to allocate %zu bytes", count * size);
  }
  return result;
}

static void* r_tree_sitter_realloc(void* buffer, size_t size) {
  void* result = realloc(buffer, size);
  if (size > 0 && !result) {
    r_abort("tree-sitter failed to reallocate %zu bytes", size);
  }
  return result;
}

static void r_tree_sitter_free(void* buffer) {
  free(buffer);
}

void init_alloc(void) {
  // Overwrite the error handling of the default allocators to
  // instead go through `r_abort()`, but othewise keep them the same.
  ts_set_allocator(
      r_tree_sitter_malloc,
      r_tree_sitter_calloc,
      r_tree_sitter_realloc,
      r_tree_sitter_free
  );
}
