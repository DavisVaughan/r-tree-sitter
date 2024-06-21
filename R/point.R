#' Points
#'
#' @description
#' - `point()` creates a new tree-sitter point.
#'
#' - `point_row()` and `point_column()` access a point's row and column value,
#'   respectively.
#'
#' - `is_point()` determines whether or not an object is a point.
#'
#' Note that points are 0-indexed. This is typically the easiest form to work
#' with them in, since most of the time when you are provided row and column
#' information from third party libraries, they will already be 0-indexed. It
#' is also consistent with bytes, which are also 0-indexed and are often
#' provided alongside their corresponding points.
#'
#' @param row `[double(1)]`
#'
#'   A 0-indexed row to place the point at.
#'
#' @param column `[double(1)]`
#'
#'   A 0-indexed column to place the point at.
#'
#' @param x `[tree_sitter_point]`
#'
#'   A point.
#'
#' @returns
#' - `point()` returns a new point.
#'
#' - `point_row()` and `point_column()` return a single double.
#'
#' - `is_point()` returns `TRUE` or `FALSE`.
#'
#' @name points
#' @examples
#' x <- point(1, 2)
#'
#' point_row(x)
#' point_column(x)
#'
#' is_point(x)
NULL

#' @rdname points
#' @export
point <- function(row, column) {
  row <- vec_cast(row, double())
  column <- vec_cast(column, double())

  check_number_whole(row, min = 0)
  check_number_whole(column, min = 0)

  new_point(row, column)
}

#' @rdname points
#' @export
point_row <- function(x) {
  check_point(x)
  point_row0(x)
}

#' @rdname points
#' @export
point_column <- function(x) {
  check_point(x)
  point_column0(x)
}

#' @rdname points
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
