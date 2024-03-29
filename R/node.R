#' @export
node_s_expression <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_s_expression, x)
}

#' @export
node_text <- function(x) {
  check_node(x)

  raw <- node_raw(x)

  tree <- node_tree(x)
  text <- tree_text0(tree)

  .Call(ffi_node_text, raw, text)
}

#' @export
node_language <- function(x) {
  check_node(x)
  tree <- node_tree(x)
  tree_language0(tree)
}

#' @export
node_walk <- function(x) {
  check_node(x)
  TreeCursor$new(x)
}

#' @export
node_parent <- function(x) {
  check_node(x)

  tree <- node_tree(x)
  x <- node_raw(x)

  raw <- .Call(ffi_node_parent, x)

  new_node_or_null(raw, tree)
}

#' @export
node_child <- function(x, i) {
  node_child_impl(x, i, ffi_node_child)
}

#' @export
node_named_child <- function(x, i) {
  node_child_impl(x, i, ffi_node_named_child)
}

node_child_impl <- function(x, i, fn, call = caller_env()) {
  check_node(x, call = call)

  tree <- node_tree(x)
  x <- node_raw(x)

  i <- vec_cast(i, double(), call = call)
  check_number_whole(i, min = 0, call = call)

  raw <- .Call(fn, x, i)

  new_node_or_null(raw, tree)
}

#' @export
node_child_count <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_child_count, x)
}

#' @export
node_named_child_count <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_named_child_count, x)
}

#' @export
node_children <- function(x) {
  node_children_impl(x, ffi_node_children)
}

#' @export
node_named_children <- function(x) {
  node_children_impl(x, ffi_node_named_children)
}

node_children_impl <- function(x, fn, call = caller_env()) {
  check_node(x, call = call)

  tree <- node_tree(x)
  x <- node_raw(x)

  out <- .Call(fn, x)

  for (i in seq_along(out)) {
    out[[i]] <- new_node(out[[i]], tree)
  }

  out
}

#' @export
node_child_by_field_id <- function(x, id) {
  check_node(x)

  tree <- node_tree(x)
  x <- node_raw(x)

  id <- vec_cast(id, integer())
  check_number_whole(id, min = 0)

  raw <- .Call(ffi_node_child_by_field_id, x, id)

  new_node_or_null(raw, tree)
}

#' @export
node_child_by_field_name <- function(x, name) {
  check_node(x)

  tree <- node_tree(x)
  x <- node_raw(x)

  check_string(name)

  raw <- .Call(ffi_node_child_by_field_name, x, name)

  new_node_or_null(raw, tree)
}

#' @export
node_field_name_for_child <- function(x, i) {
  check_node(x)
  x <- node_raw(x)

  i <- vec_cast(i, double())
  check_number_whole(i, min = 0)

  .Call(ffi_node_field_name_for_child, x, i)
}

# TODO: Document that this is 0-indexed, like `tree_edit()` and other
# byte related functions
#' @export
node_first_child_for_byte <- function(x, byte) {
  node_first_child_for_byte_impl(x, byte, ffi_node_first_child_for_byte)
}

#' @export
node_first_named_child_for_byte <- function(x, byte) {
  node_first_child_for_byte_impl(x, byte, ffi_node_first_named_child_for_byte)
}

node_first_child_for_byte_impl <- function(x, byte, fn, call = caller_env()) {
  check_node(x, call = call)

  tree <- node_tree(x)
  x <- node_raw(x)

  byte <- coerce_byte(byte, call = call)

  raw <- .Call(fn, x, byte)

  new_node_or_null(raw, tree)
}

#' @export
node_start_byte <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_start_byte, x)
}

#' @export
node_end_byte <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_end_byte, x)
}

#' @export
node_start_point <- function(x) {
  check_node(x)
  x <- node_raw(x)

  point <- .Call(ffi_node_start_point, x)

  row <- point$row
  column <- point$column

  new_point(row, column)
}

#' @export
node_end_point <- function(x) {
  check_node(x)
  x <- node_raw(x)

  point <- .Call(ffi_node_end_point, x)

  row <- point$row
  column <- point$column

  new_point(row, column)
}

#' @export
node_range <- function(x) {
  check_node(x)

  start_byte <- node_start_byte(x)
  start_point <- node_start_point(x)

  end_byte <- node_end_byte(x)
  end_point <- node_end_point(x)

  new_range(
    start_byte = start_byte,
    start_point = start_point,
    end_byte = end_byte,
    end_point = end_point
  )
}

#' @export
node_next_sibling <- function(x) {
  node_sibling(x, ffi_node_next_sibling)
}

#' @export
node_previous_sibling <- function(x) {
  node_sibling(x, ffi_node_previous_sibling)
}

#' @export
node_next_named_sibling <- function(x) {
  node_sibling(x, ffi_node_next_named_sibling)
}

#' @export
node_previous_named_sibling <- function(x) {
  node_sibling(x, ffi_node_previous_named_sibling)
}

node_sibling <- function(x, fn, call = caller_env()) {
  check_node(x, call = call)

  tree <- node_tree(x)
  x <- node_raw(x)

  raw <- .Call(fn, x)

  new_node_or_null(raw, tree)
}

#' @export
node_is_missing <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_is_missing, x)
}

#' @export
node_is_extra <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_is_extra, x)
}

#' @export
node_is_error <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_is_error, x)
}

# TODO: Document that MISSING is considered a syntax error here
#' @export
node_has_error <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_has_error, x)
}

#' @export
node_parse_state <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_parse_state, x)
}

#' @export
node_next_parse_state <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_next_parse_state, x)
}

#' @export
node_type <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_type, x)
}

#' @export
node_symbol <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_symbol, x)
}

#' @export
node_grammar_type <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_grammar_type, x)
}

#' @export
node_grammar_symbol <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_grammar_symbol, x)
}

#' @export
node_is_named <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_is_named, x)
}

#' @export
node_descendant_count <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_descendant_count, x)
}

# TODO: Document that it does not seem like this returns `NULL` even
# with OOB byte ranges
#' @export
node_descendant_for_byte_range <- function(x, start, end) {
  node_descendant_for_byte_range_impl(x, start, end, ffi_node_descendant_for_byte_range)
}

#' @export
node_named_descendant_for_byte_range <- function(x, start, end) {
  node_descendant_for_byte_range_impl(x, start, end, ffi_node_named_descendant_for_byte_range)
}

node_descendant_for_byte_range_impl <- function(x, start, end, fn, call = caller_env()) {
  check_node(x, call = call)

  tree <- node_tree(x)
  x <- node_raw(x)

  start <- coerce_byte(start, call = call)
  end <- coerce_byte(end, call = call)

  raw <- .Call(fn, x, start, end)

  new_node_or_null(raw, tree)
}

#' @export
node_descendant_for_point_range <- function(x, start, end) {
  node_descendant_for_point_range_impl(x, start, end, ffi_node_descendant_for_point_range)
}

#' @export
node_named_descendant_for_point_range <- function(x, start, end) {
  node_descendant_for_point_range_impl(x, start, end, ffi_node_named_descendant_for_point_range)
}

node_descendant_for_point_range_impl <- function(x, start, end, fn, call = caller_env()) {
  check_node(x, call = call)

  tree <- node_tree(x)
  x <- node_raw(x)

  check_point(start, call = call)
  start_row <- point_row0(start)
  start_column <- point_column0(start)

  check_point(end, call = call)
  end_row <- point_row0(end)
  end_column <- point_column0(end)

  raw <- .Call(fn, x, start_row, start_column, end_row, end_column)

  new_node_or_null(raw, tree)
}

#' @export
is_node <- function(x) {
  inherits(x, "tree_sitter_node")
}

#' @export
print.tree_sitter_node <- function(x, ...) {
  text <- node_text(x)
  text <- truncate(text)

  info <- node_format_s_expression(
    x = x,
    anonymous = TRUE,
    compact = FALSE,
    locations = TRUE,
    color_parentheses = TRUE,
    color_locations = TRUE,
    max_lines = 25L,
  )
  sexp <- info$text
  truncated <- info$truncated

  cat_line("<tree_sitter_node>")
  cat_line()
  cli::cat_rule("S-Expression")
  cat_line(sexp)
  if (truncated) {
    cat_line(cli::style_italic("<truncated>"))
  }
  cat_line()
  cli::cat_rule("Text")
  cat_line(text)

  invisible(x)
}

truncate <- function(x) {
  n <- nchar(x)

  if (n > 200L) {
    x <- substr(x, 1L, 200L)
    x <- paste0(x, "\n", cli::style_italic("<truncated>"))
  }

  x
}

node_raw <- function(x) {
  .subset2(x, "raw")
}

node_tree <- function(x) {
  .subset2(x, "tree")
}

new_node <- function(raw, tree) {
  out <- list(
    raw = raw,
    tree = tree
  )

  class(out) <- "tree_sitter_node"

  out
}

new_node_or_null <- function(raw, tree) {
  if (is.null(raw)) {
    NULL
  } else {
    new_node(raw, tree)
  }
}

check_node <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is_node(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a <tree_sitter_node>",
    ...,
    arg = arg,
    call = call
  )
}
