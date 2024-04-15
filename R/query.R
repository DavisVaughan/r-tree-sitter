#' @export
query <- function(language, source) {
  check_language(language)
  check_string(source)

  pointer <- language_pointer(language)
  pointer <- .Call(ffi_query_new, source, pointer)

  if (is.list(pointer)) {
    # It's not a real query, something went wrong
    query_error(pointer)
  }

  # Collect once, use for multiple query requests
  capture_names <- .Call(ffi_query_capture_names, pointer)
  pattern_predicates <- .Call(ffi_query_pattern_predicates, pointer)

  new_query(pointer, capture_names, pattern_predicates, source, language)
}

query_captures <- function(x, node, ..., range = NULL) {
  check_dots_empty0(...)

  check_query(x)
  check_node(node)
  check_range(range, allow_null = TRUE)

  capture_names <- query_capture_names(x)
  x <- query_pointer(x)

  tree <- node_tree(node)
  node <- node_raw(node)

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
    node,
    start_byte,
    start_row,
    start_column,
    end_byte,
    end_row,
    end_column
  )

  for (i in seq_along(out)) {
    captures <- out[[i]]
    
    for (j in seq_along(captures)) {
      captures[[j]] <- new_node(captures[[j]], tree)
    }

    out[[i]] <- captures
  }

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

query_error <- function(info, call = caller_env()) {
  offset <- info$offset
  type <- info$type

  header <- "Can't initialize this query."

  if (type == "Language") {
    # Not at any `offset`, general issue with the `language`
    bullet <- "`language` is invalid."
  } else {
    bullet <- cli::format_inline("{type} error at offset {offset} in `source`.")
  }

  message <- c(header, i = bullet)

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
