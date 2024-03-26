#' @export
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

#' @export
tree_walk <- function(x) {
  check_tree(x)
  node <- tree_root_node(x)
  node_walk(node)
}

#' @export
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
  cat_line("<tree_sitter_tree>")
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
