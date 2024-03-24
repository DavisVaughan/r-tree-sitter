#' @export
tree_root_node <- function(x) {
  check_tree(x)
  pointer <- tree_pointer(x)
  raw <- .Call(ffi_tree_root_node, pointer)
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
  text <- tree_text(x)
  language <- tree_language(x)

  check_point(start_point)
  start_row <- point_row0(start_point)
  start_column <- point_row0(start_point)

  check_point(old_end_point)
  old_end_row <- point_row0(old_end_point)
  old_end_column <- point_row0(old_end_point)

  check_point(new_end_point)
  new_end_row <- point_row0(new_end_point)
  new_end_column <- point_row0(new_end_point)

  start_byte <- vec_cast(start_byte, double())
  check_number_whole(start_byte, min = 0)

  old_end_byte <- vec_cast(old_end_byte, double())
  check_number_whole(old_end_byte, min = 0)

  new_end_byte <- vec_cast(new_end_byte, double())
  check_number_whole(new_end_byte, min = 0)

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

tree_text <- function(x) {
  .subset2(x, "text")
}

tree_language <- function(x) {
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
