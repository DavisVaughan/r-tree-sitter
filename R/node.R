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

#' @export
print.tree_sitter_node <- function(x, ...) {
  cat_line("<node>")

  if (node_tree_edited(x)) {
    cat_line("Upstream tree has been edited, this node is invalid.")
    return(invisible(x))
  }

  cat_line(node_s_expression(x))
  invisible(x)
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
