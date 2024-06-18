#' Create a new parser
#'
#' `parser()` constructs a parser from a tree-sitter `language` object. You can
#' use [parser_parse()] to parse language specific text with it.
#'
#' @param language `[tree_sitter_language]`
#'
#'   A language object.
#'
#' @returns
#' A new parser.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#' parser
#'
#' text <- "1 + foo"
#' tree <- parser_parse(parser, text)
#' tree
parser <- function(language) {
  check_language(language)
  new_parser(language)
}

#' Parser adjustments
#'
#' @description
#' - `parser_set_language()` sets the language of the parser. This is usually
#'   done by [parser()] though.
#'
#' - `parser_set_timeout()` sets an optional timeout used when calling
#'   [parser_parse()] or [parser_reparse()]. If the timeout is hit, an error
#'   occurs.
#'
#' - `parser_set_included_ranges()` sets an optional list of ranges that are
#'   the only locations considered when parsing. The ranges are created by
#'   [range()].
#'
#' @inheritParams x_tree_sitter_parser
#'
#' @param language `[tree_sitter_language]`
#'
#'   A language.
#'
#' @param timeout `[double(1)]`
#'
#'   A single whole number corresponding to a timeout in microseconds to use
#'   when parsing.
#'
#' @param included_ranges `[list_of<tree_sitter_range>]`
#'
#'   A list of ranges constructed by [range()]. These are the only locations
#'   that will be considered when parsing.
#'
#'   An empty list can be used to clear any existing ranges so that the parser
#'   will again parse the entire document.
#'
#' @returns
#' A new parser.
#'
#' @name parser-adjustments
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#' parser_set_timeout(parser, 10000)
NULL

#' @rdname parser-adjustments
#' @export
parser_set_language <- function(x, language) {
  check_parser(x)
  check_language(x)

  new_parser(
    language = language,
    timeout = x$timeout,
    included_ranges = x$included_ranges
  )
}

#' @rdname parser-adjustments
#' @export
parser_set_timeout <- function(x, timeout) {
  check_parser(x)

  # `max` is the largest representable whole double value that is below the
  # max `uint64_t` value that can be parsed by the R parser
  check_number_whole(
    x = timeout,
    min = 0,
    max = 18446744073709547520
  )
  timeout <- vec_cast(timeout, double())

  new_parser(
    language = x$language,
    timeout = timeout,
    included_ranges = x$included_ranges
  )
}

# TODO: Document that an empty list is a valid way to clear the included ranges
# so that it again parses the whole document
#' @rdname parser-adjustments
#' @export
parser_set_included_ranges <- function(x, included_ranges) {
  check_parser(x)

  obj_check_list(included_ranges)
  list_check_all_ranges(included_ranges)

  new_parser(
    language = x$language,
    timeout = x$timeout,
    included_ranges = included_ranges
  )
}

#' Parse or reparse text
#'
#' @description
#' - `parser_parse()` performs an initial parse of `text`, a string typically
#'   containing contents of a file. It returns a `tree` for further
#'   manipulations.
#'
#' - `parser_reparse()` performs a fast incremental reparse. `text` is typically
#'   a slightly modified version of the original `text` with a new "edit"
#'   applied. The position of the edit is described by the byte and point
#'   arguments to this function. The `tree` argument corresponds to the original
#'   `tree` returned by `parser_parse()`.
#'
#' All bytes and points should be 0-indexed.
#'
#' @inheritParams x_tree_sitter_parser
#' @inheritParams rlang::args_dots_empty
#'
#' @param text `[string]`
#'
#'   The text to parse.
#'
#' @param encoding `[string]`
#'
#'   The expected encoding of the `text`. Either `"UTF-8"` or `"UTF-16"`.
#'
#' @param tree `[tree_sitter_tree]`
#'
#'   The original tree returned by `parser_parse()`. Components of the tree
#'   will be reused to perform the incremental reparse.
#'
#' @param start_byte,start_point `[double(1) / tree_sitter_point]`
#'
#'   The starting byte and starting point of the edit location.
#'
#' @param old_end_byte,old_end_point `[double(1) / tree_sitter_point]`
#'
#'   The old ending byte and old ending point of the edit location.
#'
#' @param new_end_byte,new_end_point `[double(1) / tree_sitter_point]`
#'
#'   The new ending byte and new ending point of the edit location.
#'
#' @returns
#' A new `tree`.
#'
#' @name parser-parse
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "1 + foo"
#' tree <- parser_parse(parser, text)
#' tree
#'
#' text <- "1 + bar(foo)"
#' parser_reparse(
#'   parser,
#'   text,
#'   tree,
#'   start_byte = 4,
#'   start_point = point(0, 4),
#'   old_end_byte = 7,
#'   old_end_point = point(0, 7),
#'   new_end_byte = 12,
#'   new_end_point = point(0, 12)
#' )
NULL

#' @export
#' @rdname parser-parse
parser_parse <- function(
  x,
  text,
  ...,
  encoding = "UTF-8"
) {
  check_dots_empty0(...)
  check_parser(x)
  check_string(text)

  encoding <- arg_match_encoding(encoding)

  language <- parser_language0(x)
  pointer <- parser_pointer0(x)

  pointer <- .Call(
    ffi_parser_parse,
    pointer,
    text,
    encoding
  )

  new_tree(pointer, text, language)
}

#' @export
#' @rdname parser-parse
parser_reparse <- function(
  x,
  text,
  tree,
  start_byte,
  start_point,
  old_end_byte,
  old_end_point,
  new_end_byte,
  new_end_point,
  ...,
  encoding = "UTF-8"
) {
  check_dots_empty0(...)
  check_parser(x)
  check_string(text)
  check_tree(tree)

  start_byte <- coerce_byte(start_byte)
  old_end_byte <- coerce_byte(old_end_byte)
  new_end_byte <- coerce_byte(new_end_byte)

  check_point(start_point)
  start_row <- point_row0(start_point)
  start_column <- point_row0(start_point)

  check_point(old_end_point)
  old_end_row <- point_row0(old_end_point)
  old_end_column <- point_row0(old_end_point)

  check_point(new_end_point)
  new_end_row <- point_row0(new_end_point)
  new_end_column <- point_row0(new_end_point)

  encoding <- arg_match_encoding(encoding)

  language <- parser_language0(x)
  pointer <- parser_pointer0(x)

  tree <- tree_pointer(tree)

  pointer <- .Call(
    ffi_parser_reparse,
    pointer,
    text,
    encoding,
    tree,
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

  new_tree(pointer, text, language)
}

#' Is `x` a parser?
#'
#' @description
#' Checks if `x` is a `tree_sitter_parser` or not.
#'
#' @param x `[object]`
#'
#'   An object.
#'
#' @returns
#' `TRUE` if `x` is a `tree_sitter_parser`, otherwise `FALSE`.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' is_parser(parser)
#'
#' is_parser(1)
is_parser <- function(x) {
  inherits(x, "tree_sitter_parser")
}

#' @export
print.tree_sitter_parser <- function(x, ...) {
  language <- parser_language0(x)
  name <- language_name(language)
  cat_line("<tree_sitter_parser>")
  cat_line(sprintf("Language: %s", name))
  invisible(x)
}

parser_language0 <- function(x) {
  .subset2(x, "language")
}

parser_pointer0 <- function(x) {
  .subset2(x, "pointer")
}

new_parser <- function(language, ..., timeout = 0, included_ranges = list()) {
  check_dots_empty0(...)

  pointer <- language$pointer

  included_range_vectors <- transpose_list_of_ranges(included_ranges)

  pointer <- .Call(
    ffi_parser_new,
    pointer,
    timeout,
    included_range_vectors
  )

  out <- list(
    language = language,
    timeout = timeout,
    included_ranges = included_ranges,
    pointer = pointer
  )

  class(out) <- "tree_sitter_parser"

  out
}

check_parser <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is_parser(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a <tree_sitter_parser>",
    ...,
    arg = arg,
    call = call
  )
}

# Puts `included_ranges` in a form that is easier to process at the C level
transpose_list_of_ranges <- function(x) {
  start_bytes <- vapply(x, range_start_byte0, double(1))
  end_bytes <- vapply(x, range_end_byte0, double(1))

  start_points <- lapply(x, range_start_point0)
  start_rows <- vapply(start_points, point_row0, double(1))
  start_columns <- vapply(start_points, point_column0, double(1))

  end_points <- lapply(x, range_end_point0)
  end_rows <- vapply(end_points, point_row0, double(1))
  end_columns <- vapply(end_points, point_column0, double(1))

  # Field ordering matters for C level indexing
  list(
    start_bytes = start_bytes,
    start_rows = start_rows,
    start_columns = start_columns,
    end_bytes = end_bytes,
    end_rows = end_rows,
    end_columns = end_columns
  )
}
