#' Retrieve the root node of the tree
#'
#' @description
#' `tree_root_node()` is the entry point for accessing nodes within
#' a specific tree. It returns the "root" of the tree, from which you
#' can use other `node_*()` functions to navigate around.
#'
#' @inheritParams x_tree_sitter_tree
#'
#' @returns
#' A node.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Trees and nodes have a similar print method, but you can
#' # only use other `node_*()` functions on nodes.
#' tree
#' node
#'
#' node |>
#'   node_child(1) |>
#'   node_children()
tree_root_node <- function(x) {
  check_tree(x)
  pointer <- tree_pointer(x)
  raw <- .Call(ffi_tree_root_node, pointer)
  new_node(raw, x)
}

#' Retrieve an offset root node
#'
#' @description
#' `tree_root_node_with_offset()` is similar to [tree_root_node()],
#' but the returned root node's position has been shifted by the given number of
#' bytes, rows, and columns.
#'
#' This function allows you to parse a subset of a document with
#' [parser_parse()] as if it were a self-contained document, but then later
#' access the syntax tree in the coordinate space of the larger document.
#'
#' Note that the underlying `text` within `x` is not what you are offsetting
#' into. Instead, you should assume that the `text` you provided to
#' [parser_parse()] already contained the entire subset of the document you care
#' about, and the offset you are providing is how far into the document the
#' beginning of `text` is.
#'
#' @inheritParams x_tree_sitter_tree
#'
#' @param byte,point `[double(1), tree_sitter_point]`
#'
#'   A byte and point offset combination.
#'
#' @returns
#' An offset root node.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#'
#' # If `text` was the whole document, you can just use `tree_root_node()`
#' node <- tree_root_node(tree)
#'
#' # If `text` represents a subset of the document, use
#' # `tree_root_node_with_offset()` to be able to get positions in the
#' # coordinate space of the original document.
#' byte <- 5
#' point <- point(5, 0)
#' node_offset <- tree_root_node_with_offset(tree, byte, point)
#'
#' # The position of `fn` if you treat `text` as the whole document
#' node |>
#'   node_child(1) |>
#'   node_child(1)
#'
#' # The position of `fn` if you treat `text` as a subset of a larger document
#' node_offset |>
#'   node_child(1) |>
#'   node_child(1)
tree_root_node_with_offset <- function(x, byte, point) {
  check_tree(x)
  check_point(point)

  byte <- coerce_byte(byte)
  row <- point_row0(point)
  column <- point_column0(point)

  text <- tree_text0(x)
  language <- tree_language0(x)
  pointer <- tree_pointer(x)

  # Shift the `text` "forward" by the `byte` amount by padding with whitespace.
  # This ensures that views into the `text` still work with the start/end byte
  # indices returned by the offset tree nodes.
  padding <- strrep(" ", byte)
  text <- paste0(padding, text)

  # SAFETY: We don't currently export `node_tree()` to get the tree out of
  # a node object. If we provided that, then we'd have to document that it is
  # not safe to get the tree out of an offset root node. The `text` has been
  # shifted to align with the offset root node, but the tree itself doesn't
  # contain this shift, so to extract out the tree on its own would be unsafe.
  x <- new_tree(pointer, text, language)

  raw <- .Call(ffi_tree_root_node_with_offset, pointer, byte, row, column)

  new_node(raw, x)
}

#' Generate a `TreeCursor` iterator
#'
#' `tree_walk()` creates a [TreeCursor] starting at the root node. You can
#' use it to "walk" the tree more efficiently than using [node_child()] and
#' other similar node functions.
#'
#' @inheritParams x_tree_sitter_tree
#'
#' @returns
#' A `TreeCursor` object.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "1 + foo"
#' tree <- parser_parse(parser, text)
#'
#' cursor <- tree_walk(tree)
#'
#' cursor$goto_first_child()
#' cursor$goto_first_child()
#' cursor$node()
#' cursor$goto_next_sibling()
#' cursor$node()
tree_walk <- function(x) {
  check_tree(x)
  node <- tree_root_node(x)
  node_walk(node)
}

#' Tree accessors
#'
#' @description
#' - `tree_text()` retrieves the tree's `text` that it was parsed with.
#'
#' - `tree_language()` retrieves the tree's `language` that it was parsed with.
#'
#' - `tree_included_ranges()` retrieves the tree's `included_ranges` that were
#'   provided to [parser_set_included_ranges()]. Note that if no ranges were
#'   provided originally, then this still returns a default that always covers
#'   the entire document.
#'
#' @inheritParams x_tree_sitter_tree
#'
#' @returns
#' - `tree_text()` returns a string.
#'
#' - `tree_language()` returns a `tree_sitter_language`.
#'
#' - `tree_included_ranges()` returns a list of [range()] objects.
#'
#' @name tree-accessors
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "1 + foo"
#' tree <- parser_parse(parser, text)
#'
#' tree_text(tree)
#' tree_language(tree)
#' tree_included_ranges(tree)
NULL

#' @rdname tree-accessors
#' @export
tree_included_ranges <- function(x) {
  check_tree(x)
  x <- tree_pointer(x)
  info <- .Call(ffi_tree_included_ranges, x)

  start_bytes <- info[[1L]]
  start_rows <- info[[2L]]
  start_columns <- info[[3L]]
  end_bytes <- info[[4L]]
  end_rows <- info[[5L]]
  end_columns <- info[[6L]]

  size <- length(start_bytes)
  out <- vector("list", size)

  for (i in seq_len(size)) {
    start_byte <- start_bytes[[i]]
    start_row <- start_rows[[i]]
    start_column <- start_columns[[i]]

    end_byte <- end_bytes[[i]]
    end_row <- end_rows[[i]]
    end_column <- end_columns[[i]]

    start_point <- new_point(start_row, start_column)
    end_point <- new_point(end_row, end_column)

    range <- new_range(
      start_byte = start_byte,
      start_point = start_point,
      end_byte = end_byte,
      end_point = end_point
    )

    out[[i]] <- range
  }

  out
}

#' @rdname tree-accessors
#' @export
tree_text <- function(x) {
  check_tree(x)
  tree_text0(x)
}

#' @rdname tree-accessors
#' @export
tree_language <- function(x) {
  check_tree(x)
  tree_language0(x)
}

#' Is `x` a tree?
#'
#' @description
#' Checks if `x` is a `tree_sitter_tree` or not.
#'
#' @param x `[object]`
#'
#'   An object.
#'
#' @returns
#' `TRUE` if `x` is a `tree_sitter_tree`, otherwise `FALSE`.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#'
#' is_tree(tree)
#'
#' is_tree(1)
is_tree <- function(x) {
  inherits(x, "tree_sitter_tree")
}

#' @export
print.tree_sitter_tree <- function(x, ...) {
  x <- tree_root_node(x)
  cat_line("<tree_sitter_tree>")
  cat_line()
  node_print_body(x)
  invisible(x)
}

tree_pointer <- function(x) {
  .subset2(x, "pointer")
}

tree_text0 <- function(x) {
  .subset2(x, "text")
}

tree_language0 <- function(x) {
  .subset2(x, "language")
}

new_tree <- function(pointer, text, language) {
  out <- list(
    pointer = pointer,
    text = text,
    language = language
  )

  class(out) <- "tree_sitter_tree"

  out
}

check_tree <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is_tree(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a <tree_sitter_tree>",
    ...,
    arg = arg,
    call = call
  )
}
