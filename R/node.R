new_node <- function(raw, tree) {
  out <- list(
    raw = raw,
    tree = tree
  )

  class(out) <- "tree_sitter_node"

  out
}

node_raw <- function(x) {
  .subset2(x, "raw")
}

node_tree <- function(x) {
  check_node(x)
  .subset2(x, "tree")
}

node_s_expression <- function(x) {
  check_node(x)
  check_tree_unedited(x)
  x <- node_raw(x)
  .Call(ffi_node_s_expression, x)
}

node_text <- function(x) {
  check_node(x)
  check_tree_unedited(x)

  raw <- node_raw(x)

  tree <- node_tree(x)
  text <- tree$text

  .Call(ffi_node_text, raw, text)
}

node_child <- function(x, i) {
  check_node(x)
  check_tree_unedited(x)

  tree <- node_tree(x)
  x <- node_raw(x)

  i <- vec_cast(i, integer())
  vec_check_size(i, 1L)
  check_positive(i)

  out <- .Call(ffi_node_child, x, i)

  if (is.null(out)) {
    NULL
  } else {
    new_node(out, tree)
  }
}

node_child_count <- function(x) {
  check_node(x)
  check_tree_unedited(x)
  x <- node_raw(x)
  .Call(ffi_node_child_count, x)
}

node_named_child_count <- function(x) {
  check_node(x)
  check_tree_unedited(x)
  x <- node_raw(x)
  .Call(ffi_node_named_child_count, x)
}

node_children <- function(x) {
  node_children_impl(x, ffi_node_children)
}

node_named_children <- function(x) {
  node_children_impl(x, ffi_node_named_children)
}

node_children_impl <- function(x, fn, call = caller_env()) {
  check_node(x, call = call)
  check_tree_unedited(x, call = call)

  tree <- node_tree(x)
  x <- node_raw(x)

  out <- .Call(fn, x)

  for (i in seq_along(out)) {
    out[[i]] <- new_node(out[[i]], tree)
  }

  out
}

node_start_byte <- function(x) {
  check_node(x)
  check_tree_unedited(x)
  x <- node_raw(x)
  .Call(ffi_node_start_byte, x)
}

node_end_byte <- function(x) {
  check_node(x)
  check_tree_unedited(x)
  x <- node_raw(x)
  .Call(ffi_node_end_byte, x)
}

node_start_point <- function(x) {
  check_node(x)
  check_tree_unedited(x)
  x <- node_raw(x)
  .Call(ffi_node_start_point, x)
}

node_end_point <- function(x) {
  check_node(x)
  check_tree_unedited(x)
  x <- node_raw(x)
  .Call(ffi_node_end_point, x)
}

node_is_named <- function(x) {
  check_node(x)
  check_tree_unedited(x)

  x <- node_raw(x)

  .Call(ffi_node_is_named, x)
}

#' @export
print.tree_sitter_node <- function(x, ...) {
  cat_line("<node>")

  if (node_tree_edited(x)) {
    cat_line("Upstream tree has been edited, this node is invalid.")
    return(invisible(x))
  }

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
    "a node",
    ...,
    arg = arg,
    call = call
  )
}

is_node <- function(x) {
  inherits(x, "tree_sitter_node")
}

check_tree_unedited <- function(x, call = caller_env()) {
  if (!node_tree_edited(x)) {
    return(invisible(NULL))
  }
  abort(
    "This node is attached to a tree that has been edited and is no longer valid.",
    call = call
  )
}

node_tree_edited <- function(x) {
  tree <- node_tree(x)
  tree$edited()
}