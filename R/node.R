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
  x <- node_raw(x)
  .Call(ffi_node_s_expression, x)
}

#' @export
print.tree_sitter_node <- function(x, ...) {
  cat(node_s_expression(x))
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
