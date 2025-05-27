#' "Raw" S-expression
#'
#' `node_raw_s_expression()` returns the "raw" s-expression as seen by
#' tree-sitter. Most of the time, [node_show_s_expression()] provides a better
#' view of the tree, but occasionally it can be useful to see exactly what the
#' underlying C library is using.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @returns
#' A single string containing the raw s-expression.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "1 + foo"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' node_raw_s_expression(node)
node_raw_s_expression <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_s_expression, x)
}

#' Get a node's underlying text
#'
#' `node_text()` returns the document text underlying a node.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @returns
#' A single string containing the node's text.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "1 + foo"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' node |>
#'   node_child(1) |>
#'   node_child_by_field_name("rhs") |>
#'   node_text()
node_text <- function(x) {
  check_node(x)

  raw <- node_raw(x)

  tree <- node_tree(x)
  text <- tree_text0(tree)

  .Call(ffi_node_text, raw, text)
}

#' Get a node's underlying language
#'
#' `node_language()` returns the document text underlying a node.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @returns
#' A `tree_sitter_language` object.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "1 + foo"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' node_language(node)
node_language <- function(x) {
  check_node(x)
  tree <- node_tree(x)
  tree_language0(tree)
}

#' Generate a `TreeCursor` iterator
#'
#' `node_walk()` creates a [TreeCursor] starting at the current node. You can
#' use it to "walk" the tree more efficiently than using [node_child()] and
#' other similar node functions.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @returns
#' A `TreeCursor` object.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "1 + foo"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' cursor <- node_walk(node)
#'
#' cursor$goto_first_child()
#' cursor$goto_first_child()
#' cursor$node()
#' cursor$goto_next_sibling()
#' cursor$node()
node_walk <- function(x) {
  check_node(x)
  TreeCursor$new(x)
}

#' Get a node's parent
#'
#' `node_parent()` looks up the tree and returns the current node's parent.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @returns
#' The parent node of `x` or `NULL` if there is no parent.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Parent of a root node is `NULL`
#' node_parent(node)
#'
#' node_function <- node |>
#'   node_child(1) |>
#'   node_child(3)
#'
#' node_function
#'
#' node_parent(node_function)
node_parent <- function(x) {
  check_node(x)

  tree <- node_tree(x)
  x <- node_raw(x)

  raw <- .Call(ffi_node_parent, x)

  new_node_or_null(raw, tree)
}

#' Get a node's child by index
#'
#' @description
#' These functions return the `i`th child of `x`.
#'
#' - `node_child()` considers both named and anonymous children.
#'
#' - `node_named_child()` considers only named children.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @param i `[integer(1)]`
#'
#'   The index of the child to return.
#'
#' @returns
#' The `i`th child node of `x` or `NULL` if there is no child at that index.
#'
#' @name node-child
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Starts with `program` node for the whole document
#' node
#'
#' # Navigate to first child
#' node <- node_child(node, 1)
#' node
#'
#' # Note how the named variant skips the anonymous operator node
#' node_child(node, 2)
#' node_named_child(node, 2)
#'
#' # OOB indices return `NULL`
#' node_child(node, 5)
NULL

#' @export
#' @rdname node-child
node_child <- function(x, i) {
  node_child_impl(x, i, ffi_node_child)
}

#' @export
#' @rdname node-child
node_named_child <- function(x, i) {
  node_child_impl(x, i, ffi_node_named_child)
}

node_child_impl <- function(x, i, fn, call = caller_env()) {
  check_node(x, call = call)

  tree <- node_tree(x)
  x <- node_raw(x)

  i <- vec_cast(i, double(), call = call)
  check_number_whole(i, min = 1, call = call)

  # Work around `.Call(fn)` check complaints
  .call <- .Call
  raw <- .call(fn, x, i)

  new_node_or_null(raw, tree)
}

#' Get a node's child count
#'
#' @description
#' These functions return the number of children of `x`.
#'
#' - `node_child_count()` considers both named and anonymous children.
#'
#' - `node_named_child_count()` considers only named children.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @returns
#' A single integer, the number of children of `x`.
#'
#' @name node-child-count
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Navigate to first child
#' node <- node_child(node, 1)
#' node
#'
#' # Note how the named variant doesn't count the anonymous operator node
#' node_child_count(node)
#' node_named_child_count(node)
NULL

#' @rdname node-child-count
#' @export
node_child_count <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_child_count, x)
}

#' @rdname node-child-count
#' @export
node_named_child_count <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_named_child_count, x)
}

#' Get a node's children
#'
#' @description
#' These functions return the children of `x` within a list.
#'
#' - `node_children()` considers both named and anonymous children.
#'
#' - `node_named_children()` considers only named children.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @returns
#' The children of `x` as a list.
#'
#' @name node-children
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Navigate to first child
#' node <- node_child(node, 1)
#' node
#'
#' # Note how the named variant doesn't include the anonymous operator node
#' node_children(node)
#' node_named_children(node)
NULL

#' @rdname node-children
#' @export
node_children <- function(x) {
  node_children_impl(x, ffi_node_children)
}

#' @rdname node-children
#' @export
node_named_children <- function(x) {
  node_children_impl(x, ffi_node_named_children)
}

node_children_impl <- function(x, fn, call = caller_env()) {
  check_node(x, call = call)

  tree <- node_tree(x)
  x <- node_raw(x)

  # Work around `.Call(fn)` check complaints
  .call <- .Call
  out <- .call(fn, x)

  for (i in seq_along(out)) {
    out[[i]] <- new_node(out[[i]], tree)
  }

  out
}

#' Get a node's child by field id or name
#'
#' @description
#' These functions return children of `x` by field id or name.
#'
#' - `node_child_by_field_id()` retrieves a child by field id.
#'
#' - `node_child_by_field_name()` retrieves a child by field name.
#'
#' Use [language_field_id_for_name()] to get the field id for a field name.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @param id `[integer(1)]`
#'
#'   The field id of the child to return.
#'
#' @param name `[character(1)]`
#'
#'   The field name of the child to return.
#'
#' @returns
#' A child of `x`, or `NULL` if no matching child can be found.
#'
#' @name node-child-by-field
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Navigate to first child
#' node <- node_child(node, 1)
#' node
#'
#' # Get the field name of the first child
#' name <- node_field_name_for_child(node, 1)
#' name
#'
#' # Now get the child again by that field name
#' node_child_by_field_name(node, name)
#'
#' # If you need to look up by field name many times, you can look up the
#' # more direct field id first and use that instead
#' id <- language_field_id_for_name(language, name)
#' id
#'
#' node_child_by_field_id(node, id)
#'
#' # Returns `NULL` if no matching child
#' node_child_by_field_id(node, 10000)
NULL

#' @rdname node-child-by-field
#' @export
node_child_by_field_id <- function(x, id) {
  check_node(x)

  tree <- node_tree(x)
  x <- node_raw(x)

  id <- vec_cast(id, integer())
  check_number_whole(id, min = 0)

  raw <- .Call(ffi_node_child_by_field_id, x, id)

  new_node_or_null(raw, tree)
}

#' @rdname node-child-by-field
#' @export
node_child_by_field_name <- function(x, name) {
  check_node(x)

  tree <- node_tree(x)
  x <- node_raw(x)

  check_string(name)

  raw <- .Call(ffi_node_child_by_field_name, x, name)

  new_node_or_null(raw, tree)
}

#' Get a child's field name by index
#'
#' @description
#' These functions return the field name for the `i`th child of `x`.
#'
#' - `node_field_name_for_child()` considers both named and anonymous children.
#'
#' - `node_field_name_for_named_child()` considers only named children.
#'
#' Nodes themselves don't know their own field names, because they don't know
#' if they are fields or not. You must have access to their parents to query
#' their field names.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @param i `[integer(1)]`
#'
#'   The index of the child to get the field name for.
#'
#' @returns
#' The field name for the `i`th child of `x`, or `NA_character_` if that child
#' doesn't exist.
#'
#' @name node-field-name-for-child
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Navigate to first child
#' node <- node_child(node, 1)
#' node
#'
#' # Get the field name of the first few children (note that anonymous children
#' # are considered)
#' node_field_name_for_child(node, 1)
#' node_field_name_for_child(node, 2)
#'
#' # Get the field name of the first few named children (note that anonymous
#' # children are not considered)
#' node_field_name_for_named_child(node, 1)
#' node_field_name_for_named_child(node, 2)
#'
#' # 10th child doesn't exist, this returns `NA_character_`
#' node_field_name_for_child(node, 10)
NULL

#' @rdname node-field-name-for-child
#' @export
node_field_name_for_child <- function(x, i) {
  node_field_name_for_child_impl(x, i, ffi_node_field_name_for_child)
}

#' @rdname node-field-name-for-child
#' @export
node_field_name_for_named_child <- function(x, i) {
  node_field_name_for_child_impl(x, i, ffi_node_field_name_for_named_child)
}

node_field_name_for_child_impl <- function(x, i, fn, call = caller_env()) {
  check_node(x, call = call)
  x <- node_raw(x)

  i <- vec_cast(i, double(), call = call)
  check_number_whole(i, min = 1, call = call)

  # Work around `.Call(fn)` check complaints
  .call <- .Call
  .call(fn, x, i)
}

#' Get the first child that extends beyond the given byte offset
#'
#' @description
#' These functions return the first child of `x` that extends beyond the given
#' `byte` offset. Note that `byte` is a 0-indexed offset.
#'
#' - `node_first_child_for_byte()` considers both named and anonymous nodes.
#'
#' - `node_first_named_child_for_byte()` considers only named nodes.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @param byte `[integer(1)]`
#'
#'   The byte to start the search from.
#'
#'   Note that `byte` is 0-indexed!
#'
#' @returns
#' A new node, or `NULL` if there is no node past the `byte` offset.
#'
#' @name node-first-child-byte
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Navigate to first child
#' node <- node_child(node, 1)
#' node
#'
#' # `fn {here}<- function()`
#' node_first_child_for_byte(node, 3)
#' node_first_named_child_for_byte(node, 3)
#'
#' # Past any node
#' node_first_child_for_byte(node, 100)
NULL

#' @rdname node-first-child-byte
#' @export
node_first_child_for_byte <- function(x, byte) {
  node_first_child_for_byte_impl(x, byte, ffi_node_first_child_for_byte)
}

#' @rdname node-first-child-byte
#' @export
node_first_named_child_for_byte <- function(x, byte) {
  node_first_child_for_byte_impl(x, byte, ffi_node_first_named_child_for_byte)
}

node_first_child_for_byte_impl <- function(x, byte, fn, call = caller_env()) {
  check_node(x, call = call)

  tree <- node_tree(x)
  x <- node_raw(x)

  byte <- coerce_byte(byte, call = call)

  # Work around `.Call(fn)` check complaints
  .call <- .Call
  raw <- .call(fn, x, byte)

  new_node_or_null(raw, tree)
}

#' Node byte and point accessors
#'
#' @description
#' These functions return information about the location of `x` in the document.
#' The byte, row, and column locations are all 0-indexed.
#'
#' - `node_start_byte()` returns the start byte.
#'
#' - `node_end_byte()` returns the end byte.
#'
#' - `node_start_point()` returns the start point, containing a row and column
#'   location within the document. Use accessors like [point_row()] to extract
#'   the row and column positions.
#'
#' - `node_end_point()` returns the end point, containing a row and column
#'   location within the document. Use accessors like [point_row()] to extract
#'   the row and column positions.
#'
#' - `node_range()` returns a range object that contains all of the above
#'   information. Use accessors like [range_start_point()] to extract
#'   individual pieces from the range.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @returns
#' - `node_start_byte()` and `node_end_byte()` return a single numeric value.
#'
#' - `node_start_point()` and `node_end_point()` return single points.
#'
#' - `node_range()` returns a range.
#'
#' @name node-location
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Navigate to first child
#' node <- node_child(node, 1)
#'
#' # Navigate to function definition node
#' node <- node_child(node, 3)
#' node
#'
#' node_start_byte(node)
#' node_end_byte(node)
#'
#' node_start_point(node)
#' node_end_point(node)
#'
#' node_range(node)
NULL

#' @rdname node-location
#' @export
node_start_byte <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_start_byte, x)
}

#' @rdname node-location
#' @export
node_end_byte <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_end_byte, x)
}

#' @rdname node-location
#' @export
node_start_point <- function(x) {
  check_node(x)
  x <- node_raw(x)

  point <- .Call(ffi_node_start_point, x)

  row <- point$row
  column <- point$column

  new_point(row, column)
}

#' @rdname node-location
#' @export
node_end_point <- function(x) {
  check_node(x)
  x <- node_raw(x)

  point <- .Call(ffi_node_end_point, x)

  row <- point$row
  column <- point$column

  new_point(row, column)
}

#' @rdname node-location
#' @export
node_range <- function(x) {
  check_node(x)

  start_byte <- node_start_byte(x)
  start_point <- node_start_point(x)

  end_byte <- node_end_byte(x)
  end_point <- node_end_point(x)

  new_range(
    start_byte = start_byte,
    start_point = start_point,
    end_byte = end_byte,
    end_point = end_point
  )
}

#' Node sibling accessors
#'
#' @description
#' These functions return siblings of the current node, i.e. if you looked
#' "left" or "right" from the current node rather "up" (parent) or "down"
#' (child).
#'
#' - `node_next_sibling()` and `node_next_named_sibling()` return the next
#'   sibling.
#'
#' - `node_previous_sibling()` and `node_previous_named_sibling()` return the
#'   previous sibling.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @returns
#' A sibling node, or `NULL` if there is no sibling node.
#'
#' @name node-sibling
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Navigate to first child
#' node <- node_child(node, 1)
#'
#' # Navigate to function definition node
#' node <- node_child(node, 3)
#' node
#'
#' node_previous_sibling(node)
#'
#' # Skip anonymous operator node
#' node_previous_named_sibling(node)
#'
#' # There isn't one!
#' node_next_sibling(node)
NULL

#' @rdname node-sibling
#' @export
node_next_sibling <- function(x) {
  node_sibling(x, ffi_node_next_sibling)
}

#' @rdname node-sibling
#' @export
node_next_named_sibling <- function(x) {
  node_sibling(x, ffi_node_next_named_sibling)
}

#' @rdname node-sibling
#' @export
node_previous_sibling <- function(x) {
  node_sibling(x, ffi_node_previous_sibling)
}

#' @rdname node-sibling
#' @export
node_previous_named_sibling <- function(x) {
  node_sibling(x, ffi_node_previous_named_sibling)
}

node_sibling <- function(x, fn, call = caller_env()) {
  check_node(x, call = call)

  tree <- node_tree(x)
  x <- node_raw(x)

  # Work around `.Call(fn)` check complaints
  .call <- .Call
  raw <- .call(fn, x)

  new_node_or_null(raw, tree)
}

#' Node metadata
#'
#' @description
#' These functions return metadata about the current node.
#'
#' - `node_is_named()` reports if the current node is named or anonymous.
#'
#' - `node_is_missing()` reports if the current node is `MISSING`, i.e.
#'   if it was implied through error recovery.
#'
#' - `node_is_extra()` reports if the current node is an "extra" from the
#'   grammar.
#'
#' - `node_is_error()` reports if the current node is an `ERROR` node.
#'
#' - `node_has_error()` reports if the current node is an `ERROR` node, or if
#'   any descendants of the current node are `ERROR` or `MISSING` nodes.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @returns
#' `TRUE` or `FALSE`.
#'
#' @name node-metadata
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' node <- node_child(node, 1)
#'
#' fn <- node_child(node, 1)
#' operator <- node_child(node, 2)
#'
#' fn
#' node_is_named(fn)
#'
#' operator
#' node_is_named(operator)
#'
#' # Examples of `TRUE` cases for these are a bit hard to come up with, because
#' # they are dependent on the exact state of the grammar and the error recovery
#' # algorithm
#' node_is_missing(node)
#' node_is_extra(node)
NULL

#' @rdname node-metadata
#' @export
node_is_named <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_is_named, x)
}

#' @rdname node-metadata
#' @export
node_is_missing <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_is_missing, x)
}

#' @rdname node-metadata
#' @export
node_is_extra <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_is_extra, x)
}

#' @rdname node-metadata
#' @export
node_is_error <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_is_error, x)
}

#' @rdname node-metadata
#' @export
node_has_error <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_has_error, x)
}

#' Node parse states
#'
#' @description
#' These are advanced functions that return information about the internal parse
#' states.
#'
#' - `node_parse_state()` returns the parse state of the current node.
#'
#' - `node_next_parse_state()` returns the parse state after this node.
#'
#' See [language_next_state()] for more information.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @returns
#' A single integer representing a parse state.
#'
#' @name node-parse-state
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' node <- node_child(node, 1)
#'
#' # Parse states are grammar dependent
#' node_parse_state(node)
#' node_next_parse_state(node)
NULL

#' @rdname node-parse-state
#' @export
node_parse_state <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_parse_state, x)
}

#' @rdname node-parse-state
#' @export
node_next_parse_state <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_next_parse_state, x)
}

#' Node type
#'
#' @description
#' `node_type()` returns the "type" of the current node as a string.
#'
#' This is a very useful function for making decisions about how to handle
#' the current node.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @returns
#' A single string.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Top level program node
#' node_type(node)
#'
#' # The whole `<-` binary operator node
#' node <- node_child(node, 1)
#' node
#' node_type(node)
#'
#' # Just the literal `<-` operator itself
#' node <- node_child_by_field_name(node, "operator")
#' node
#' node_type(node)
node_type <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_type, x)
}

#' Node symbol
#'
#' @description
#' `node_symbol()` returns the symbol id of the current node as an integer.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @returns
#' A single integer.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Top level program node
#' node_symbol(node)
#'
#' # The whole `<-` binary operator node
#' node <- node_child(node, 1)
#' node_symbol(node)
#'
#' # Just the literal `<-` operator itself
#' node <- node_child_by_field_name(node, "operator")
#' node_symbol(node)
node_symbol <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_symbol, x)
}

#' Node grammar types and symbols
#'
#' @description
#' - `node_grammar_type()` gets the node's type as it appears in the grammar,
#'   _ignoring aliases_.
#'
#' - `node_grammar_symbol()` gets the node's symbol (the type as a numeric id)
#'   as it appears in the grammar, _ignoring aliases_. This should be used in
#'   [language_next_state()] rather than [node_symbol()].
#'
#' @inheritParams x_tree_sitter_node
#'
#' @seealso [node_type()], [node_symbol()]
#'
#' @name node-grammar
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Examples for these functions are highly specific to the grammar,
#' # because they relies on the placement of `alias()` calls in the grammar.
#' node_grammar_type(node)
#' node_grammar_symbol(node)
NULL

#' @rdname node-grammar
#' @export
node_grammar_type <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_grammar_type, x)
}

#' @rdname node-grammar
#' @export
node_grammar_symbol <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_grammar_symbol, x)
}

#' Node descendant count
#'
#' @description
#' Returns the number of descendants of this node, including this node in the
#' count.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @returns
#' A single double.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # Top level program node
#' node_descendant_count(node)
#'
#' # The whole `<-` binary operator node
#' node <- node_child(node, 1)
#' node_descendant_count(node)
#'
#' # Just the literal `<-` operator itself
#' node <- node_child_by_field_name(node, "operator")
#' node_descendant_count(node)
node_descendant_count <- function(x) {
  check_node(x)
  x <- node_raw(x)
  .Call(ffi_node_descendant_count, x)
}

#' Node descendants
#'
#' @description
#' These functions return the smallest node within this node that spans the
#' given range of bytes or points. If the ranges are out of bounds, or no
#' smaller node can be determined, the input is returned.
#'
#' @inheritParams x_tree_sitter_node
#'
#' @param start,end `[integer(1) / tree_sitter_point]`
#'
#'   For the byte range functions, start and end bytes to search within.
#'
#'   For the point range functions, start and end points created by [point()] to
#'   search within.
#'
#' @returns
#' A node.
#'
#' @name node-descendant
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' # The whole `<-` binary operator node
#' node <- node_child(node, 1)
#' node
#'
#' # The byte range points to a location in the word `function`
#' node_descendant_for_byte_range(node, 7, 9)
#' node_named_descendant_for_byte_range(node, 7, 9)
#'
#' start <- point(0, 14)
#' end <- point(0, 15)
#'
#' node_descendant_for_point_range(node, start, end)
#' node_named_descendant_for_point_range(node, start, end)
#'
#' # OOB returns the input
#' node_descendant_for_byte_range(node, 25, 29)
NULL

#' @rdname node-descendant
#' @export
node_descendant_for_byte_range <- function(x, start, end) {
  node_descendant_for_byte_range_impl(
    x,
    start,
    end,
    ffi_node_descendant_for_byte_range
  )
}

#' @rdname node-descendant
#' @export
node_named_descendant_for_byte_range <- function(x, start, end) {
  node_descendant_for_byte_range_impl(
    x,
    start,
    end,
    ffi_node_named_descendant_for_byte_range
  )
}

node_descendant_for_byte_range_impl <- function(
  x,
  start,
  end,
  fn,
  call = caller_env()
) {
  check_node(x, call = call)

  tree <- node_tree(x)
  x <- node_raw(x)

  start <- coerce_byte(start, call = call)
  end <- coerce_byte(end, call = call)

  # Work around `.Call(fn)` check complaints
  .call <- .Call
  raw <- .call(fn, x, start, end)

  new_node(raw, tree)
}

#' @rdname node-descendant
#' @export
node_descendant_for_point_range <- function(x, start, end) {
  node_descendant_for_point_range_impl(
    x,
    start,
    end,
    ffi_node_descendant_for_point_range
  )
}

#' @rdname node-descendant
#' @export
node_named_descendant_for_point_range <- function(x, start, end) {
  node_descendant_for_point_range_impl(
    x,
    start,
    end,
    ffi_node_named_descendant_for_point_range
  )
}

node_descendant_for_point_range_impl <- function(
  x,
  start,
  end,
  fn,
  call = caller_env()
) {
  check_node(x, call = call)

  tree <- node_tree(x)
  x <- node_raw(x)

  check_point(start, call = call)
  start_row <- point_row0(start)
  start_column <- point_column0(start)

  check_point(end, call = call)
  end_row <- point_row0(end)
  end_column <- point_column0(end)

  # Work around `.Call(fn)` check complaints
  .call <- .Call
  raw <- .call(fn, x, start_row, start_column, end_row, end_column)

  new_node(raw, tree)
}

#' Is `x` a node?
#'
#' @description
#' Checks if `x` is a `tree_sitter_node` or not.
#'
#' @param x `[object]`
#'
#'   An object.
#'
#' @returns
#' `TRUE` if `x` is a `tree_sitter_node`, otherwise `FALSE`.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function() { 1 + 1 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' is_node(node)
#'
#' is_node(1)
is_node <- function(x) {
  inherits(x, "tree_sitter_node")
}

#' @export
print.tree_sitter_node <- function(x, ...) {
  cat_line("<tree_sitter_node>")
  cat_line()
  node_print_body(x)
  invisible(x)
}

node_print_body <- function(x) {
  cli::cat_rule("Text")
  text <- node_text(x)
  text <- lines_truncate(text)
  cat_line(text)

  cat_line()

  cli::cat_rule("S-Expression")
  node_show_s_expression(x, max_lines = 25L)

  invisible(x)
}

lines_truncate <- function(x, n = 25L) {
  locs <- gregexpr("\\n", x)[[1L]]
  n_newlines <- length(locs)

  if (n_newlines <= n) {
    return(x)
  }

  start <- 1L
  stop <- locs[[n]]

  x <- substr(x, start, stop)
  x <- paste0(x, cli::style_italic("<truncated>"))

  x
}

node_raw <- function(x) {
  .subset2(x, "raw")
}

node_tree <- function(x) {
  .subset2(x, "tree")
}

new_node <- function(raw, tree) {
  out <- list(
    raw = raw,
    tree = tree
  )

  class(out) <- "tree_sitter_node"

  out
}

new_node_or_null <- function(raw, tree) {
  if (is.null(raw)) {
    NULL
  } else {
    new_node(raw, tree)
  }
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
    "a <tree_sitter_node>",
    ...,
    arg = arg,
    call = call
  )
}
