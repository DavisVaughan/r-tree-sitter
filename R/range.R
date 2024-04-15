#' @export
range <- function(
  start_byte,
  start_point,
  end_byte,
  end_point
) {
  start_byte <- coerce_byte(start_byte)
  end_byte <- coerce_byte(end_byte)

  check_point(start_point)
  check_point(end_point)

  new_range(start_byte, start_point, end_byte, end_point)
}

#' @export
range_start_byte <- function(x) {
  check_range(x)
  range_start_byte0(x)
}

#' @export
range_start_point <- function(x) {
  check_range(x)
  range_start_point0(x)
}

#' @export
range_end_byte <- function(x) {
  check_range(x)
  range_end_byte0(x)
}

#' @export
range_end_point <- function(x) {
  check_range(x)
  range_end_point0(x)
}

#' @export
is_range <- function(x) {
  inherits(x, "tree_sitter_range")
}

#' @export
print.tree_sitter_range <- function(x, ...) {
  start_byte <- range_start_byte0(x)
  end_byte <- range_end_byte0(x)

  start_point <- range_start_point0(x)
  start_row <- point_row0(start_point)
  start_column <- point_column0(start_point)

  end_point <- range_end_point0(x)
  end_row <- point_row0(end_point)
  end_column <- point_column0(end_point)

  cat_line("<tree_sitter_range>")
  cat_line(cli::format_inline("Start <byte: {start_byte}, row: {start_row}, column: {start_column}>"))
  cat_line(cli::format_inline("End <byte: {end_byte}, row: {end_row}, column: {end_column}>"))

  invisible(x)
}

new_range <- function(start_byte, start_point, end_byte, end_point) {
  out <- list(
    start_byte = start_byte,
    start_point = start_point,
    end_byte = end_byte,
    end_point = end_point
  )
  class(out) <- "tree_sitter_range"
  out
}

range_start_byte0 <- function(x) {
  .subset2(x, "start_byte")
}

range_start_point0 <- function(x) {
  .subset2(x, "start_point")
}

range_end_byte0 <- function(x) {
  .subset2(x, "end_byte")
}

range_end_point0 <- function(x) {
  .subset2(x, "end_point")
}

check_range <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is_range(x)) {
    return(invisible(NULL))
  }
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a <tree_sitter_range>",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

list_check_all_ranges <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (list_is_all_ranges(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a list of <tree_sitter_range>",
    ...,
    arg = arg,
    call = call
  )
}

list_is_all_ranges <- function(x) {
  all(vapply(x, is_range, logical(1)))
}
