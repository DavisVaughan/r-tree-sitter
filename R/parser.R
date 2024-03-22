#' @export
parser <- function(language) {
  check_language(language)
  new_parser(language)
}

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

#' @export
parser_set_included_ranges <- function(x, included_ranges) {
  check_parser(x)
  abort("Setting `included_ranges` is not yet supported.")
}

#' @export
parser_parse <- function(
  x,
  text,
  ...,
  encoding = "UTF-8",
  tree = NULL
) {
  check_dots_empty0(...)
  check_parser(x)
  check_string(text)
  check_tree(tree, allow_null = TRUE)

  encoding <- arg_match0(
    arg = encoding,
    values = c("UTF-8", "UTF-16"),
    arg_nm = "encoding"
  )

  if (!is.null(tree)) {
    tree <- tree_pointer(tree)
  }

  language <- parser_language(x)
  pointer <- parser_pointer(x)

  pointer <- .Call(
    ffi_parser_parse,
    pointer,
    text,
    encoding,
    tree
  )

  new_tree(pointer, text, language)
}

#' @export
is_parser <- function(x) {
  inherits(x, "tree_sitter_parser")
}

parser_language <- function(x) {
  .subset2(x, "language")
}

parser_pointer <- function(x) {
  .subset2(x, "pointer")
}

new_parser <- function(language, ..., timeout = NULL, included_ranges = NULL) {
  check_dots_empty0(...)

  pointer <- language$pointer

  pointer <- .Call(
    ffi_parser_new,
    pointer,
    timeout,
    included_ranges
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
