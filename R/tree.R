tree_root_node <- function(x) {
  check_tree(x)
  pointer <- tree_pointer(x)
  raw <- .Call(ffi_tree_root_node, pointer)
  new_node(raw, x)
}

tree_walk <- function(x) {
  check_tree(x)
  node <- tree_root_node(x)
  TreeCursor$new(node)
}

tree_edit <- function(
  x,
  start_byte,
  start_row,
  start_column,
  old_end_byte,
  old_end_row,
  old_end_column,
  new_end_byte,
  new_end_row,
  new_end_column
) {
  check_tree(x)

  pointer <- tree_pointer(x)
  text <- tree_text(x)
  language <- tree_language(x)

  args <- vec_cast_common(
    start_byte = start_byte,
    start_row = start_row,
    start_column = start_column,
    old_end_byte = old_end_byte,
    old_end_row = old_end_row,
    old_end_column = old_end_column,
    new_end_byte = new_end_byte,
    new_end_row = new_end_row,
    new_end_column = new_end_column,
    .to = double()
  )

  start_byte <- args$start_byte
  start_row <- args$start_row
  start_column <- args$start_column
  old_end_byte <- args$old_end_byte
  old_end_row <- args$old_end_row
  old_end_column <- args$old_end_column
  new_end_byte <- args$new_end_byte
  new_end_row <- args$new_end_row
  new_end_column <- args$new_end_column

  check_number_whole(start_byte, min = 0)
  check_number_whole(start_row, min = 0)
  check_number_whole(start_column, min = 0)
  check_number_whole(old_end_byte, min = 0)
  check_number_whole(old_end_row, min = 0)
  check_number_whole(old_end_column, min = 0)
  check_number_whole(new_end_byte, min = 0)
  check_number_whole(new_end_row, min = 0)
  check_number_whole(new_end_column, min = 0)

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

tree_pointer <- function(x) {
  x$pointer
}

tree_text <- function(x) {
  x$text
}

tree_language <- function(x) {
  x$language
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

is_tree <- function(x) {
  inherits(x, "tree_sitter_tree")
}

#' @export
print.tree_sitter_tree <- function(x, ...) {
  cat_line("<tree_sitter_tree>")
}
