#' Queries
#'
#' @description
#' `query()` lets you specify a query `source` string for use with
#' [query_captures()] and [query_matches()]. The `source` string is written in a
#' way that is somewhat similar to the idea of capture groups in regular
#' expressions. You write out one or more query patterns that match nodes in a
#' tree, and then you "capture" parts of those patterns with `@name` tags. The
#' captures are the values returned by [query_captures()] and [query_matches()].
#' There are also a series of _predicates_ that can be used to further refine
#' the query. Those are described in the [query_matches()] help page.
#'
#' Read the [tree-sitter documentation](https://tree-sitter.github.io/tree-sitter/using-parsers/queries/index.html)
#' to learn more about the query syntax.
#'
#' @section Storing queries:
#' Query objects contain _external pointers_, so they cannot be saved to disk
#' and reloaded. One consequence of this is you cannot create them at build
#' time inside your package. For example, to precompile a query you may assume
#' you can create a global variable in your package with top level code like
#' this:
#'
#' ```r
#' QUERY <- treesitter::query(treesitter.r::language(), "query_source_text")
#' ```
#'
#' This won't work for two reasons:
#'
#' - The external query in `QUERY` is created at package build time, and is no
#'   longer valid at package load time.
#'
#' - The version of treesitter and treesitter.r are locked to the version used
#'   at build time, rather than at package load time.
#'
#' The correct way to do this is to create the query on package load, like this:
#'
#' ```r
#' QUERY <- NULL
#'
#' .onLoad <- function(libname, pkgname) {
#'   QUERY <<- treesitter::query(treesitter.r::language(), "query_source_text")
#' }
#' ```
#'
#' This is one place where usage of `<<-` is acceptable.
#'
#' @param language `[tree_sitter_language]`
#'
#'   A language.
#'
#' @param source `[string]`
#'
#'   A query source string.
#'
#' @returns
#' A query.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' # This query looks for binary operators where the left hand side is an
#' # identifier named `fn`, and the right hand side is a function definition.
#' # The operator can be `<-` or `=` (technically it can also be things like
#' # `+` as well in this example).
#' source <- '(binary_operator
#'   lhs: (identifier) @lhs
#'   operator: _ @operator
#'   rhs: (function_definition) @rhs
#'   (#eq? @lhs "fn")
#' )'
#'
#' language <- treesitter.r::language()
#'
#' query <- query(language, source)
#'
#' text <- "
#'   fn <- function() {}
#'   fn2 <- function() {}
#'   fn <- 5
#'   fn = function(a, b, c) { a + b + c }
#' "
#' parser <- parser(language)
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' query_matches(query, node)
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
#' - `query_matches()` returns the captures first grouped by _pattern_, and
#'   further grouped by _match_ within each pattern. This is useful if you
#'   include multiple patterns in your query.
#'
#' - `query_captures()` returns a flat list of captures ordered by their node
#'   location in the original text. This is normally the easiest structure to
#'   use if you have a single pattern without any alternations that would
#'   benefit from having individual captures split by match.
#'
#' Both also return the capture name, i.e. the `@name` you specified in your
#' query.
#'
#' @section Predicates:
#'
#' There are 3 core types of predicates supported:
#'
#' - `#eq? @capture "string"`
#' - `#eq? @capture1 @capture2`
#' - `#match? @capture "regex"`
#'
#' Here are a few examples:
#'
#' ```
#' # Match an identifier named `"name-of-interest"`
#' (
#'   (identifier) @id
#'   (#eq? @id "name-of-interest")
#' )
#'
#' # Match a binary operator where the left and right sides are the same name
#' (
#'   (binary_operator
#'     lhs: (identifier) @id1
#'     rhs: (identifier) @id2
#'   )
#'   (#eq? @id1 @id2)
#' )
#'
#' # Match a name with a `_` in it
#' (
#'   (identifier) @id
#'   (#match? @id "_")
#' )
#' ```
#'
#' Each of these predicates can be inverted with a `not-` prefix.
#'
#' ```
#' (
#'   (identifier) @id
#'   (#not-eq? @id "name-of-interest")
#' )
#' ```
#'
#' Each of these predicates can be converted from an _all_ style predicate to an
#' _any_ style predicate with an `any-` prefix. This is only useful with
#' _quantified_ captures, i.e. `(comment)+`, where the `+` specifies "one or
#' more comment".
#'
#' ```
#' # Finds a block of comments where ALL comments are empty comments
#' (
#'   (comment)+ @comment
#'   (#eq? @comment "#")
#' )
#'
#' # Finds a block of comments where ANY comments are empty comments
#' (
#'   (comment)+ @comment
#'   (#any-eq? @comment "#")
#' )
#' ```
#'
#' This is the full list of possible predicate permutations:
#'
#' - `#eq?`
#' - `#not-eq?`
#' - `#any-eq?`
#' - `#any-not-eq?`
#' - `#match?`
#' - `#not-match?`
#' - `#any-match?`
#' - `#any-not-match?`
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
#' @inheritParams x_tree_sitter_query
#' @inheritParams rlang::args_dots_empty
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
#' # ---------------------------------------------------------------------------
#' # Simple query
#'
#' text <- "
#' foo + b + a + ab
#' and(a)
#' "
#'
#' source <- "
#' (identifier) @id
#' "
#'
#' language <- treesitter.r::language()
#'
#' query <- query(language, source)
#' parser <- parser(language)
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # A flat ordered list of captures, that's most useful here since
#' # we only have 1 pattern!
#' captures <- query_captures(query, node)
#' captures$node
#'
#' # ---------------------------------------------------------------------------
#' # Quantified query
#'
#' text <- "
#' # this
#' # that
#' NULL
#'
#' # and
#' # here
#' 1 + 1
#'
#' # there
#' 2
#' "
#'
#' # Find blocks of one or more comments
#' # The `+` is a regex `+` meaning "one or more" comments in a row
#' source <- "
#' (comment)+ @comment
#' "
#'
#' language <- treesitter.r::language()
#'
#' query <- query(language, source)
#' parser <- parser(language)
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # The extra structure provided by `query_matches()` is useful here so
#' # we can see the 3 distinct blocks of comments
#' matches <- query_matches(query, node)
#'
#' # We provided one query pattern, so lets extract that
#' matches <- matches[[1]]
#'
#' # 3 blocks of comments
#' matches[[1]]
#' matches[[2]]
#' matches[[3]]
#'
#' # ---------------------------------------------------------------------------
#' # Multiple query patterns
#'
#' # If you know you need to run multiple queries, you can run them all at once
#' # in one pass over the tree by providing multiple query patterns.
#'
#' text <- "
#' a <- 1
#' b <- function() {}
#' c <- b
#' "
#'
#' # Use an extra set of `()` to separate multiple query patterns
#' source <- "
#' (
#'   (identifier) @id
#' )
#' (
#'   (binary_operator) @binary
#' )
#' "
#'
#' language <- treesitter.r::language()
#'
#' query <- query(language, source)
#' parser <- parser(language)
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # The extra structure provided by `query_matches()` is useful here so
#' # we can separate the two queries
#' matches <- query_matches(query, node)
#'
#' # First query - all identifiers
#' matches[[1]]
#'
#' # Second query - all binary operators
#' matches[[2]]
#'
#' # ---------------------------------------------------------------------------
#' # The `#eq?` and `#match?` predicates
#'
#' text <- '
#' fn(a, b)
#'
#' test_that("this", {
#'   test
#' })
#'
#' fn_name(args)
#'
#' test_that("that", {
#'   test
#' })
#'
#' fn2_(args)
#' '
#'
#' language <- treesitter.r::language()
#' parser <- parser(language)
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Use an extra set of outer `()` when you are applying a predicate to ensure
#' # the query pattern is grouped with the query predicate.
#' # This one finds all function calls where the function name is `test_that`.
#' source <- '
#' (
#'   (call
#'     function: (identifier) @name
#'   ) @call
#'   (#eq? @name "test_that")
#' )
#' '
#'
#' query <- query(language, source)
#'
#' # It's fine to have a flat list of captures here, but we probably want to
#' # remove the `@name` captures and just retain the full `@call` captures.
#' captures <- query_captures(query, node)
#' captures$node[captures$name == "call"]
#'
#' # This one finds all functions with a `_` in their name. It uses the R
#' # level `grepl()` for the regex processing.
#' source <- '
#' (
#'   (call
#'     function: (identifier) @name
#'   ) @call
#'   (#match? @name "_")
#' )
#' '
#'
#' query <- query(language, source)
#'
#' captures <- query_captures(query, node)
#' captures$node[captures$name == "call"]
#'
#' # ---------------------------------------------------------------------------
#' # The `any-` and `not-` predicate modifiers
#'
#' text <- '
#' # 1
#' #
#' # 2
#' NULL
#'
#' # 3
#' # 4
#' NULL
#'
#' #
#' #
#' NULL
#'
#' #
#' # 5
#' #
#' # 6
#' #
#' NULL
#' '
#'
#' language <- treesitter.r::language()
#' parser <- parser(language)
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Two queries:
#' # - Find comment blocks where there is at least one empty comment
#' # - Find comment blocks where there is at least one non-empty comment
#' source <- '
#' (
#'   (comment)+ @comment
#'   (#any-eq? @comment "#")
#' )
#' (
#'   (comment)+ @comment
#'   (#any-not-eq? @comment "#")
#' )
#' '
#'
#' query <- query(language, source)
#'
#' matches <- query_matches(query, node)
#'
#' # Query 1 has 3 comment blocks that match
#' query1 <- matches[[1]]
#' query1[[1]]
#' query1[[2]]
#' query1[[3]]
#'
#' # Query 2 has 3 comment blocks that match (a different set than query 1!)
#' query2 <- matches[[2]]
#' query2[[1]]
#' query2[[2]]
#' query2[[3]]
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

#' Query accessors
#'
#' @description
#' - `query_pattern_count()` returns the number of patterns in a query.
#'
#' - `query_capture_count()` returns the number of captures in a query.
#'
#' - `query_string_count()` returns the number of string literals in a query.
#'
#' - `query_start_byte_for_pattern()` and `query_end_byte_for_pattern()` return
#'   the byte where the `i`th pattern starts/ends in the query `source`.
#'
#' @inheritParams x_tree_sitter_query
#'
#' @param i `[double(1)]`
#'
#'   The `i`th pattern to extract the byte for.
#'
#' @returns
#' - `query_pattern_count()`, `query_capture_count()`, and
#'   `query_string_count()` return a single double count value.
#'
#' - `query_start_byte_for_pattern()` and `query_end_byte_for_pattern()` return
#'   a single double for their respective byte if there was an `i`th pattern,
#'   otherwise they return `NA`.
#'
#' @name query-accessors
#' @examplesIf rlang::is_installed("treesitter.r")
#' source <- '(binary_operator
#'   lhs: (identifier) @lhs
#'   operator: _ @operator
#'   rhs: (function_definition) @rhs
#'   (#eq? @lhs "fn")
#' )'
#' language <- treesitter.r::language()
#'
#' query <- query(language, source)
#'
#' query_pattern_count(query)
#' query_capture_count(query)
#' query_string_count(query)
#'
#' query_start_byte_for_pattern(query, 1)
#' query_end_byte_for_pattern(query, 1)
#'
#' text <- "
#'   fn <- function() {}
#'   fn2 <- function() {}
#'   fn <- 5
#'   fn <- function(a, b, c) { a + b + c }
#' "
#' parser <- parser(language)
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' query_matches(query, node)
NULL

#' @rdname query-accessors
#' @export
query_pattern_count <- function(x) {
  check_query(x)
  x <- query_pointer(x)
  .Call(ffi_query_pattern_count, x)
}

#' @rdname query-accessors
#' @export
query_capture_count <- function(x) {
  check_query(x)
  x <- query_pointer(x)
  .Call(ffi_query_capture_count, x)
}

#' @rdname query-accessors
#' @export
query_string_count <- function(x) {
  check_query(x)
  x <- query_pointer(x)
  .Call(ffi_query_string_count, x)
}

#' @rdname query-accessors
#' @export
query_start_byte_for_pattern <- function(x, i) {
  check_query(x)
  x <- query_pointer(x)

  i <- vec_cast(i, double())
  check_number_whole(i, min = 1)

  .Call(ffi_query_start_byte_for_pattern, x, i)
}

#' @rdname query-accessors
#' @export
query_end_byte_for_pattern <- function(x, i) {
  check_query(x)
  x <- query_pointer(x)

  i <- vec_cast(i, double())
  check_number_whole(i, min = 1)

  .Call(ffi_query_end_byte_for_pattern, x, i)
}

#' Is `x` a query?
#'
#' @description
#' Checks if `x` is a `tree_sitter_query` or not.
#'
#' @param x `[object]`
#'
#'   An object.
#'
#' @returns
#' `TRUE` if `x` is a `tree_sitter_query`, otherwise `FALSE`.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' source <- "(identifier) @id"
#' language <- treesitter.r::language()
#'
#' query <- query(language, source)
#'
#' is_query(query)
#'
#' is_query(1)
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
