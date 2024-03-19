#include "tree.h"

#include "decl/tree-decl.h"
#include "external-pointer.h"
#include "node.h"

r_obj* ffi_tree_root_node(r_obj* ffi_x) {
  TSTree* x = ts_tree_from_external_pointer(ffi_x);
  TSNode node = ts_tree_root_node(x);
  return ts_node_as_raw(node);
}

r_obj* ts_tree_as_external_pointer(TSTree* x) {
  return new_external_pointer((void*) x, tree_finalize);
}

TSTree* ts_tree_from_external_pointer(r_obj* x) {
  TS_OBJECT_FROM_EXTERNAL_POINTER(x, TSTree*);
}

static void tree_finalize(r_obj* x) {
  if (r_typeof(x) != R_TYPE_pointer) {
    return;
  }

  TSTree* tree = (TSTree*) R_ExternalPtrAddr(x);

  if (tree == NULL) {
    return;
  }

  ts_tree_delete(tree);

  R_ClearExternalPtr(x);
}
