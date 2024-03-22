#' @export
TreeCursor <- R6::R6Class(
  "tree_sitter_tree_cursor",
  cloneable = FALSE,
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
    goto_parent = function() {
      tree_cursor_goto_parent(self, private)
    },
    goto_next_sibling = function() {
      tree_cursor_goto_next_sibling(self, private)
    },
    goto_previous_sibling = function() {
      tree_cursor_goto_previous_sibling(self, private)
    },
    goto_first_child = function() {
      tree_cursor_goto_first_child(self, private)
    },
    goto_last_child = function() {
      tree_cursor_goto_last_child(self, private)
    },
    current_depth = function() {
      tree_cursor_current_depth(self, private)
    },
    goto_first_child_for_byte = function(byte) {
      tree_cursor_goto_first_child_for_byte(self, private, byte)
    },
    goto_first_child_for_point = function(row, column) {
      tree_cursor_goto_first_child_for_point(self, private, row, column)
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

  .Call(ffi_tree_cursor_reset, private$.raw, node)

  private$.tree <- tree

  self
}

tree_cursor_current_node <- function(self, private) {
  tree <- private$.tree
  raw <- .Call(ffi_tree_cursor_current_node, private$.raw)
  new_node(raw, tree)
}

tree_cursor_goto_parent <- function(self, private) {
  .Call(ffi_tree_cursor_goto_parent, private$.raw)
}

tree_cursor_goto_next_sibling <- function(self, private) {
  .Call(ffi_tree_cursor_goto_next_sibling, private$.raw)
}

tree_cursor_goto_previous_sibling <- function(self, private) {
  .Call(ffi_tree_cursor_goto_previous_sibling, private$.raw)
}

tree_cursor_goto_first_child <- function(self, private) {
  .Call(ffi_tree_cursor_goto_first_child, private$.raw)
}

tree_cursor_goto_last_child <- function(self, private) {
  .Call(ffi_tree_cursor_goto_last_child, private$.raw)
}

tree_cursor_current_depth <- function(self, private) {
  .Call(ffi_tree_cursor_current_depth, private$.raw)
}

tree_cursor_goto_first_child_for_byte <- function(self, private, byte) {
  check_number_whole(byte, min = 0)
  .Call(ffi_tree_cursor_goto_first_child_for_byte, private$.raw, byte)
}

tree_cursor_goto_first_child_for_point <- function(self, private, row, column) {
  check_number_whole(row, min = 0)
  check_number_whole(column, min = 0)
  .Call(ffi_tree_cursor_goto_first_child_for_point, private$.raw, row, column)
}

tree_cursor_finalize <- function(self, private) {
  .Call(ffi_tree_cursor_finalize, private$.raw)
  invisible(NULL)
}
