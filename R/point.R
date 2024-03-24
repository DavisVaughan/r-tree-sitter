#' @export
point <- function(row, column) {
  row <- vec_cast(row, double())
  column <- vec_cast(column, double())

  check_number_whole(row, min = 0)
  check_number_whole(column, min = 0)

  new_point(row, column)
}

#' @export
point_row <- function(x) {
  check_point(x)
  point_row0(x)
}

#' @export
point_column <- function(x) {
  check_point(x)
  point_column0(x)
}

#' @export
is_point <- function(x) {
  inherits(x, "tree_sitter_point")
}

#' @export
print.tree_sitter_point <- function(x, ...) {
  row <- point_row0(x)
  column <- point_column0(x)

  cat_line("<tree_sitter_point>")
  cat_line(cli::format_inline("Row: {row}"))
  cat_line(cli::format_inline("Column: {column}"))

  invisible(x)
}

new_point <- function(row, column) {
  out <- list(row = row, column = column)
  class(out) <- "tree_sitter_point"
  out
}

point_row0 <- function(x) {
  .subset2(x, "row")
}

point_column0 <- function(x) {
  .subset2(x, "column")
}

check_point <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is_point(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a <tree_sitter_point>",
    ...,
    arg = arg,
    call = call
  )
}
