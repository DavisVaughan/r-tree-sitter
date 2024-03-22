TreeCursor <- R6::R6Class(
  "tree_sitter_tree_cursor",
  private = list(
    .tree = NULL,
    .raw = NULL,

    finalize = function() {
      tree_cursor_finalize(self, private)
    }
  ),
  public = list(
    initialize = function(node) {
      tree_cursor_initialize(self, private, node)
    },
    reset = function(node) {
      tree_cursor_reset(self, private, node)
    },
    current_node = function() {
      tree_cursor_current_node(self, private)
    },
    goto_first_child = function() {
      tree_cursor_goto_first_child(self, private)
    }
  )
)

tree_cursor_initialize <- function(self, private, node) {
  check_node(node)

  tree <- node_tree(node)
  node <- node_raw(node)

  private$.tree <- tree
  private$.raw <- .Call(ffi_tree_cursor_initialize, node)

  self
}

tree_cursor_reset <- function(self, private, node) {
  check_node(node)

  tree <- node_tree(node)
  node <- node_raw(node)

  raw <- private$.raw
  .Call(ffi_tree_cursor_reset, raw, node)

  private$.tree <- tree

  self
}

tree_cursor_current_node <- function(self, private) {
  tree <- private$.tree
  raw <- private$.raw
  raw <- .Call(ffi_tree_cursor_current_node, raw)
  new_node(raw, tree)
}

tree_cursor_goto_first_child <- function(self, private) {
  raw <- private$.raw
  .Call(ffi_tree_cursor_goto_first_child, raw)
}

tree_cursor_finalize <- function(self, private) {
  raw <- private$.raw
  .Call(ffi_tree_cursor_finalize, raw)
  invisible(NULL)
}
