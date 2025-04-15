#' Tree cursors
#'
#' @description
#' `TreeCursor` is an R6 class that allows you to walk a tree in a more
#' efficient way than calling `node_*()` functions like [node_child()]
#' repeatedly.
#'
#' You can also more elegantly create a cursor with [node_walk()] and
#' [tree_walk()].
#'
#' @param node `[tree_sitter_node]`
#'
#'   The node to start walking from.
#'
#' @param byte `[double(1)]`
#'
#'   The byte to move the cursor past.
#'
#' @param point `[tree_sitter_point]`
#'
#'   The point to move the cursor past.
#'
#' @returns
#' R6 object representing the tree cursor.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function(a, b) { a + b }"
#'
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' cursor <- TreeCursor$new(node)
#'
#' cursor$node()
#' cursor$goto_first_child()
#' cursor$goto_first_child()
#' cursor$node()
#' cursor$goto_next_sibling()
#' cursor$node()
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
    #' @description
    #' Create a new tree cursor.
    initialize = function(node) {
      tree_cursor_initialize(self, private, node)
    },

    #' @description
    #' Reset the tree cursor to a new root node.
    reset = function(node) {
      tree_cursor_reset(self, private, node)
    },

    #' @description
    #' Get the current node that the cursor points to.
    node = function() {
      tree_cursor_node(self, private)
    },

    #' @description
    #' Get the field name of the current node.
    field_name = function() {
      tree_cursor_field_name(self, private)
    },

    #' @description
    #' Get the field id of the current node.
    field_id = function() {
      tree_cursor_field_id(self, private)
    },

    #' @description
    #' Get the descendent index of the current node.
    descendant_index = function() {
      tree_cursor_descendant_index(self, private)
    },

    #' @description
    #' Go to the current node's parent.
    #'
    #' Returns `TRUE` if a parent was found, and `FALSE` if not.
    goto_parent = function() {
      tree_cursor_goto_parent(self, private)
    },

    #' @description
    #' Go to the current node's next sibling.
    #'
    #' Returns `TRUE` if a sibling was found, and `FALSE` if not.
    goto_next_sibling = function() {
      tree_cursor_goto_next_sibling(self, private)
    },

    #' @description
    #' Go to the current node's previous sibling.
    #'
    #' Returns `TRUE` if a sibling was found, and `FALSE` if not.
    goto_previous_sibling = function() {
      tree_cursor_goto_previous_sibling(self, private)
    },

    #' @description
    #' Go to the current node's first child.
    #'
    #' Returns `TRUE` if a child was found, and `FALSE` if not.
    goto_first_child = function() {
      tree_cursor_goto_first_child(self, private)
    },

    #' @description
    #' Go to the current node's last child.
    #'
    #' Returns `TRUE` if a child was found, and `FALSE` if not.
    goto_last_child = function() {
      tree_cursor_goto_last_child(self, private)
    },

    #' @description
    #' Get the depth of the current node.
    depth = function() {
      tree_cursor_depth(self, private)
    },

    #' @description
    #' Move the cursor to the first child of its current node that extends
    #' beyond the given byte offset.
    #'
    #' Returns `TRUE` if a child was found, and `FALSE` if not.
    goto_first_child_for_byte = function(byte) {
      tree_cursor_goto_first_child_for_byte(self, private, byte)
    },

    #' @description
    #' Move the cursor to the first child of its current node that extends
    #' beyond the given point.
    #'
    #' Returns `TRUE` if a child was found, and `FALSE` if not.
    goto_first_child_for_point = function(point) {
      tree_cursor_goto_first_child_for_point(self, private, point)
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

tree_cursor_node <- function(self, private) {
  tree <- private$.tree
  raw <- .Call(ffi_tree_cursor_node, private$.raw)
  new_node(raw, tree)
}

tree_cursor_field_name <- function(self, private) {
  .Call(ffi_tree_cursor_field_name, private$.raw)
}

tree_cursor_field_id <- function(self, private) {
  .Call(ffi_tree_cursor_field_id, private$.raw)
}

tree_cursor_descendant_index <- function(self, private) {
  .Call(ffi_tree_cursor_descendant_index, private$.raw)
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

tree_cursor_depth <- function(self, private) {
  .Call(ffi_tree_cursor_depth, private$.raw)
}

tree_cursor_goto_first_child_for_byte <- function(self, private, byte) {
  byte <- coerce_byte(byte)
  .Call(ffi_tree_cursor_goto_first_child_for_byte, private$.raw, byte)
}

tree_cursor_goto_first_child_for_point <- function(self, private, point) {
  check_point(point)

  row <- point_row0(point)
  column <- point_column0(point)

  .Call(ffi_tree_cursor_goto_first_child_for_point, private$.raw, row, column)
}

tree_cursor_finalize <- function(self, private) {
  .Call(ffi_tree_cursor_finalize, private$.raw)
  invisible(NULL)
}
