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
  text <- tree_text(tree)

  .Call(ffi_node_text, raw, text)
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
node_is_named <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_is_named, x)
}

#' @export
is_node <- function(x) {
  inherits(x, "tree_sitter_node")
}

#' @export
print.tree_sitter_node <- function(x, ...) {
  cat_line("<tree_sitter_node>")

  sexp <- node_s_expression(x)
  sexp <- truncate(sexp)

  text <- node_text(x)
  text <- truncate(text)

  cat_line()
  cli::cat_rule("S-Expression")
  cat_line(sexp)
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
