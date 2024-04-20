#' @export
query <- function(language, source) {
  check_language(language)
  check_string(source)

  pointer <- language_pointer(language)
  pointer <- .Call(ffi_query_new, source, pointer)

  if (is.list(pointer)) {
    # It's not a real query, something went wrong
    query_error(pointer, source)
  }

  # Collect once, use for multiple query requests
  capture_names <- .Call(ffi_query_capture_names, pointer)
  pattern_predicates <- .Call(ffi_query_pattern_predicates, pointer)

  new_query(pointer, capture_names, pattern_predicates, source, language)
}

#' Query matches and captures
#' 
#' @description
#' These two functions execute a query on a given `node`, and return the
#' captures of the query for further use. Both functions return the same
#' information, just structured differently depending on your use case. 
#' 
#' - `query_matches()` returns the captures first grouped by _predicate_, and
#'   further grouped by _match_ within each predicate. This is useful if you
#'   include multiple predicates in your query or if you expect multiple
#'   captures per match.
#' 
#' - `query_captures()` returns a flat list of captures ordered by their node
#'   location in the original text. This is normally the easiest structure to
#'   use if you have a single predicate with one capture per match.
#' 
#' Both also return the name of the capture, i.e. the `@name` you specified in
#' your query.
#' 
#' @section Predicates: 
#' 
#' There are 3 core types of predicates supported:
#' 
#' - `#eq? @capture "string"`
#' - `#eq? @capture1 @capture2`
#' - `#match? @capture "regex"`
#' 
#' Each of these predicates can also be inverted with a `not-` prefix, i.e.
#' `#not-eq?` and `#not-match?`.
#' 
#' ### String double quotes
#' 
#' The underlying tree-sitter predicate parser requires that strings supplied
#' in a query must use double quotes, i.e. `"string"` not `'string'`. If you
#' try and use single quotes, you will get a query error.
#' 
#' ### `#match?` regex
#' 
#' The regex support provided by `#match?` is powered by [grepl()].
#' 
#' Escapes are a little tricky to get right within these match regex strings.
#' To use something like `\s` in the regex string, you need the literal text
#' `\\s` to appear in the string to tell the tree-sitter regex engine to escape
#' the backslash so you end up with just `\s` in the captured string. This
#' requires putting two literal backslash characters in the R string itself,
#' which can be accomplished with either `"\\\\s"` or using a raw string like
#' `r'["\\s"]'` which is typically a little easier. You can also write your
#' queries in a separate file (typically called `queries.scm`) and read them
#' into R, which is also a little more straightforward because you can just
#' write something like `(#match? @id "^\\s$")` and that will be read in
#' correctly.
#' 
#' @inheritParams rlang::args_dots_empty
#' 
#' @param x `[tree_sitter_query]`
#' 
#'   A query.
#' 
#' @param node `[tree_sitter_node]`
#' 
#'   A node to run the query over.
#' 
#' @param range `[tree_sitter_range / NULL]`
#' 
#'   An optional range to restrict the query to.
#' 
#' @name query-matches-and-captures
#' 
#' @examplesIf rlang::is_installed("treesitter.r")
#' text <- "
#' foo + b + a + ab
#' and(a)
#' "
#' 
#' source <- "(identifier) @id"
#' 
#' language <- treesitter.r::language()
#' 
#' query <- query(language, source)
#' 
#' node <- language |>
#'   parser() |>
#'   parser_parse(text) |>
#'   tree_root_node()
#' 
#' # A flat ordered list of captures, that's most useful here since
#' # we only have 1 pattern!
#' captures <- query_captures(query, node)
#' captures$node
NULL

#' @rdname query-matches-and-captures
#' @export
query_matches <- function(x, node, ..., range = NULL) {
  check_dots_empty0(...)

  check_query(x)
  check_node(node)
  check_range(range, allow_null = TRUE)

  capture_names <- query_capture_names(x)
  pattern_predicates <- query_pattern_predicates(x)
  x <- query_pointer(x)

  tree <- node_tree(node)
  node <- node_raw(node)

  text <- tree_text0(tree)

  if (is.null(range)) {
    start_byte <- NULL
    start_row <- NULL
    start_column <- NULL
    end_byte <- NULL
    end_row <- NULL
    end_column <- NULL
  } else {
    start_byte <- range_start_byte0(range)
    start_point <- range_start_point0(range)
    start_row <- point_row0(start_point)
    start_column <- point_column0(start_point)
    end_byte <- range_end_byte0(range)
    end_point <- range_end_point0(range)
    end_row <- point_row0(end_point)
    end_column <- point_column0(end_point)
  }

  out <- .Call(
    ffi_query_matches,
    x,
    capture_names,
    pattern_predicates,
    node,
    tree,
    text,
    start_byte,
    start_row,
    start_column,
    end_byte,
    end_row,
    end_column
  )

  out
}

#' @rdname query-matches-and-captures
#' @export
query_captures <- function(x, node, ..., range = NULL) {
  check_dots_empty0(...)

  check_query(x)
  check_node(node)
  check_range(range, allow_null = TRUE)

  capture_names <- query_capture_names(x)
  pattern_predicates <- query_pattern_predicates(x)
  x <- query_pointer(x)

  tree <- node_tree(node)
  node <- node_raw(node)

  text <- tree_text0(tree)

  if (is.null(range)) {
    start_byte <- NULL
    start_row <- NULL
    start_column <- NULL
    end_byte <- NULL
    end_row <- NULL
    end_column <- NULL
  } else {
    start_byte <- range_start_byte0(range)
    start_point <- range_start_point0(range)
    start_row <- point_row0(start_point)
    start_column <- point_column0(start_point)
    end_byte <- range_end_byte0(range)
    end_point <- range_end_point0(range)
    end_row <- point_row0(end_point)
    end_column <- point_column0(end_point)
  }

  out <- .Call(
    ffi_query_captures,
    x,
    capture_names,
    pattern_predicates,
    node,
    tree,
    text,
    start_byte,
    start_row,
    start_column,
    end_byte,
    end_row,
    end_column
  )

  out
}

#' @export
query_pattern_count <- function(x) {
  check_query(x)
  x <- query_pointer(x)
  .Call(ffi_query_pattern_count, x)
}

#' @export
query_capture_count <- function(x) {
  check_query(x)
  x <- query_pointer(x)
  .Call(ffi_query_capture_count, x)
}

#' @export
query_string_count <- function(x) {
  check_query(x)
  x <- query_pointer(x)
  .Call(ffi_query_string_count, x)
}

#' @export
query_start_byte_for_pattern <- function(x, i) {
  check_query(x)
  x <- query_pointer(x)

  i <- vec_cast(i, double())
  check_number_whole(i, min = 1)

  .Call(ffi_query_start_byte_for_pattern, x, i)
}

#' @export
is_query <- function(x) {
  inherits(x, "tree_sitter_query")
}

query_pointer <- function(x) {
  .subset2(x, "pointer")
}

query_capture_names <- function(x) {
  .subset2(x, "capture_names")
} 

query_pattern_predicates <- function(x) {
  .subset2(x, "pattern_predicates")
}

query_error <- function(info, source, call = caller_env()) {
  offset <- info$offset
  type <- info$type

  header <- "Can't initialize this query."

  if (type == "Language") {
    # Not at any `offset`, general issue with the `language`
    bullets <- c(i = "`language` is invalid.")
  } else {
    start <- offset
    stop <- pmin(nchar(source), start + 20L)

    source <- paste0(
      substr(source, 1L, offset - 1L),
      cli::col_red("<HERE>"),
      substr(source, offset, nchar(source))
    )

    bullets <- c(
      i = cli::format_inline("{type} error at offset {offset} in `source`."),
      # `format_inline()` strips out newlines, which we want to keep here
      sprintf("\n```\n%s\n```", source)
    )
  }

  message <- c(header, bullets)

  abort(message, call = call)
}

new_query <- function(
  pointer, 
  capture_names, 
  pattern_predicates, 
  source, 
  language
) {
  out <- list(
    pointer = pointer,
    capture_names = capture_names,
    pattern_predicates = pattern_predicates,
    source = source,
    language = language
  )

  class(out) <- "tree_sitter_query"

  out
}

check_query <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is_query(x)) {
    return(invisible(NULL))
  }
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a <tree_sitter_query>",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

is_predicate_eq_capture <- function(x) {
  inherits(x, "tree_sitter_predicate_eq_capture") 
}

is_predicate_eq_string <- function(x) {
  inherits(x, "tree_sitter_predicate_eq_string") 
}

is_predicate_match_string <- function(x) {
  inherits(x, "tree_sitter_predicate_match_string") 
}
