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
#' @examplesIf treesitter:::has_r_grammar()
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

# TODO: When documenting, pull from this PR to talk about potential use cases
# for this https://github.com/tree-sitter/tree-sitter/pull/1845
#' @export
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
#' @examplesIf treesitter:::has_r_grammar()
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

#' Edit a tree in preparation for an incremental parse
#'
#' @description
#' Before calling [parser_parse()] with an existing `tree`, you must first
#' edit the existing tree using `tree_edit()` to prepare the tree for the
#' updated `text`.
#'
#' All bytes and points are 0-indexed.
#'
#' Note that editing a tree is likely to put it into a state where the print
#' method no longer works, because the tree's start and end boundaries will
#' be out of sync with its existing text.
#'
#' @inheritParams x_tree_sitter_tree
#'
#' @param start_byte,start_point `[double(1) / tree_sitter_point]`
#'
#'   The starting byte and starting point of the edit location.
#'
#' @param old_end_byte,old_end_point `[double(1) / tree_sitter_point]`
#'
#'   The old ending byte and old ending point of the edit location.
#'
#' @param new_end_byte,new_end_point `[double(1) / tree_sitter_point]`
#'
#'   The new ending byte and new ending point of the edit location.
#'
#' @returns
#' A new `tree` that can now be used with [parser_parse()].
#'
#' @export
#' @examplesIf treesitter:::has_r_grammar()
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "1 + foo"
#' tree <- parser_parse(parser, text)
#' tree
#'
#' text <- "1 + bar(foo)"
#' tree <- tree_edit(
#'   tree,
#'   start_byte = 4,
#'   start_point = point(0, 4),
#'   old_end_byte = 7,
#'   old_end_point = point(0, 7),
#'   new_end_byte = 12,
#'   new_end_point = point(0, 12)
#' )
#'
#' parser_parse(parser, text, tree = tree)
tree_edit <- function(
  x,
  start_byte,
  start_point,
  old_end_byte,
  old_end_point,
  new_end_byte,
  new_end_point
) {
  check_tree(x)
  pointer <- tree_pointer(x)
  text <- tree_text0(x)
  language <- tree_language0(x)

  check_point(start_point)
  start_row <- point_row0(start_point)
  start_column <- point_row0(start_point)

  check_point(old_end_point)
  old_end_row <- point_row0(old_end_point)
  old_end_column <- point_row0(old_end_point)

  check_point(new_end_point)
  new_end_row <- point_row0(new_end_point)
  new_end_column <- point_row0(new_end_point)

  start_byte <- coerce_byte(start_byte)
  old_end_byte <- coerce_byte(old_end_byte)
  new_end_byte <- coerce_byte(new_end_byte)

  pointer <- .Call(
    ffi_tree_edit,
    pointer,
    start_byte,
    start_row,
    start_column,
    old_end_byte,
    old_end_row,
    old_end_column,
    new_end_byte,
    new_end_row,
    new_end_column
  )

  new_tree(
    pointer = pointer,
    text = text,
    language = language
  )
}

# TODO: Document that the default includes a range that covers the
# whole document
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

#' @export
tree_text <- function(x) {
  check_tree(x)
  tree_text0(x)
}

#' @export
tree_language <- function(x) {
  check_tree(x)
  tree_language0(x)
}

#' @export
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
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is_tree(x)) {
    return(invisible(NULL))
  }
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a <tree_sitter_tree>",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
