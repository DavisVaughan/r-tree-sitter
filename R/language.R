#' Language name
#'
#' Extract a language object's language name.
#'
#' @param x `[tree_sitter_language]`
#'
#'   A tree-sitter language object.
#'
#' @returns
#' A string.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' language_name(language)
language_name <- function(x) {
  check_language(x)
  x$name
}

#' Language version
#'
#' Get the ABI version for this language object.
#'
#' @param x `[tree_sitter_language]`
#'
#'   A tree-sitter language object.
#'
#' @returns
#' A scalar double value.
#'
#' @noRd
language_version <- function(x) {
  check_language(x)
  .Call(ffi_language_version, language_pointer(x))
}

#' Language symbols
#'
#' Get the integer symbol ID for a particular node name. Can be useful
#' for exploring the grammar.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x `[tree_sitter_language]`
#'
#'   A tree-sitter language object.
#'
#' @param name `[character]`
#'
#'   The names to look up symbols for.
#'
#' @param named `[logical]`
#'
#'   Should named or anonymous nodes be looked up? Recycled to the
#'   size of `name`.
#'
#' @returns
#' An integer vector the same size as `name` containing either:
#' - The integer symbol ID of the node name, if known.
#' - `NA` if the node name was not known.
#'
#' @seealso [language_symbol_name()]
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' language_symbol_for_name(language, "identifier")
language_symbol_for_name <- function(x, name, ..., named = TRUE) {
  check_dots_empty0(...)
  check_language(x)

  name <- vec_cast(name, character())
  named <- vec_cast(named, logical())

  size <- vec_size(name)
  named <- vec_recycle(named, size, x_arg = "named")

  .Call(ffi_language_symbol_for_name, language_pointer(x), name, named)
}

#' Language symbol names
#'
#' Get the name for a particular language symbol ID. Can be useful for
#' exploring a grammar.
#'
#' @param x `[tree_sitter_language]`
#'
#'   A tree-sitter language object.
#'
#' @param symbol `[positive integer]`
#'
#'   The language symbols to look up names for.
#'
#' @returns
#' A character vector the same length as `symbol` containing:
#' - The name of the symbol, if known.
#' - `NA`, if the symbol was not known.
#'
#' @seealso [language_symbol_for_name()]
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' language_symbol_name(language, 1)
language_symbol_name <- function(x, symbol) {
  check_language(x)
  symbol <- vec_cast(symbol, integer())
  .Call(ffi_language_symbol_name, language_pointer(x), symbol)
}

#' Language field identifiers
#'
#' Get the integer field identifier for a field name. If you are going to be
#' using a field name repeatedly, it is often a little faster to use the
#' corresponding field identifier instead.
#'
#' @param x `[tree_sitter_language]`
#'
#'   A tree-sitter language object.
#'
#' @param name `[character]`
#'
#'   The language field names to look up field identifiers for.
#'
#' @returns
#' An integer vector the same length as `name` containing:
#' - The field identifier for the field name, if known.
#' - `NA`, if the field name was not known.
#'
#' @seealso [language_field_name_for_id()]
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' language_field_id_for_name(language, "lhs")
language_field_id_for_name <- function(x, name) {
  check_language(x)

  name <- vec_cast(name, character())
  check_no_missing(name)

  .Call(ffi_language_field_id_for_name, language_pointer(x), name)
}

#' Language field names
#'
#' Get the field name for a field identifier.
#'
#' @param x `[tree_sitter_language]`
#'
#'   A tree-sitter language object.
#'
#' @param id `[integer]`
#'
#'   The language field identifiers to look up field names for.
#'
#' @returns
#' A character vector the same length as `id` containing:
#' - The field name for the field identifier, if known.
#' - `NA`, if the field identifier was not known.
#'
#' @seealso [language_field_id_for_name()]
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' language_field_name_for_id(language, 1)
language_field_name_for_id <- function(x, id) {
  check_language(x)

  id <- vec_cast(id, integer())
  check_no_missing(id)

  .Call(ffi_language_field_name_for_id, language_pointer(x), id)
}

#' Language symbol count
#'
#' Get the number of symbols contained within a language.
#'
#' @param x `[tree_sitter_language]`
#'
#'   A tree-sitter language object.
#'
#' @returns
#' A single double value.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' language_symbol_count(language)
language_symbol_count <- function(x) {
  check_language(x)
  .Call(ffi_language_symbol_count, language_pointer(x))
}

#' Language state count
#'
#' Get the number of states traversable within a language.
#'
#' @param x `[tree_sitter_language]`
#'
#'   A tree-sitter language object.
#'
#' @returns
#' A single double value.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' language_state_count(language)
language_state_count <- function(x) {
  check_language(x)
  .Call(ffi_language_state_count, language_pointer(x))
}

#' Language field count
#'
#' Get the number of fields contained within a language.
#'
#' @param x `[tree_sitter_language]`
#'
#'   A tree-sitter language object.
#'
#' @returns
#' A single double value.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' language_field_count(language)
language_field_count <- function(x) {
  check_language(x)
  .Call(ffi_language_field_count, language_pointer(x))
}

#' Language state advancement
#'
#' Get the next state in the grammar.
#'
#' @param x `[tree_sitter_language]`
#'
#'   A tree-sitter language object.
#'
#' @param state,symbol `[integer]`
#'
#'   Vectors of equal length containing the current state and symbol
#'   information.
#'
#' @returns
#' A single integer representing the next state.
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
#' # Navigate to function definition
#' node <- node_child(node, 1)
#' node <- node_child(node, 3)
#' node
#'
#' state <- node_parse_state(node)
#' symbol <- node_grammar_symbol(node)
#'
#' # Function definition symbol
#' language_symbol_name(language, 85)
#'
#' # Next state (this is all grammar dependent)
#' language_next_state(language, state, symbol)
language_next_state <- function(x, state, symbol) {
  check_language(x)

  state <- vec_cast(state, integer())
  check_no_missing(state)

  symbol <- vec_cast(symbol, integer())
  check_no_missing(symbol)

  args <- vec_recycle_common(state = state, symbol = symbol)
  state <- args$state
  symbol <- args$symbol

  .Call(ffi_language_next_state, language_pointer(x), state, symbol)
}

#' Is `x` a language?
#'
#' Use `is_language()` to determine if an object has a class of
#' `"tree_sitter_language"`.
#'
#' @param x `[object]`
#'
#'   An object.
#'
#' @returns
#' - `TRUE` if `x` is a `"tree_sitter_language"`.
#' - `FALSE` otherwise.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' is_language(language)
is_language <- function(x) {
  inherits(x, "tree_sitter_language")
}

#' @export
print.tree_sitter_language <- function(x, ...) {
  name <- language_name(x)
  cat_line("<tree_sitter_language>")
  cat_line(sprintf("Language: %s", name))
  invisible(x)
}

language_pointer <- function(x) {
  x$pointer
}

language_abi <- function(x) {
  x$abi
}

check_language <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!is_language(x)) {
    stop_input_type(
      x,
      "a <tree_sitter_language>",
      ...,
      arg = arg,
      call = call
    )
  }

  check_language_abi(x, arg = arg, call = call)

  invisible(NULL)
}

check_language_abi <- function(
  x,
  ...,
  min = tree_sitter_minimum_compatible_abi(),
  max = tree_sitter_abi(),
  arg = caller_arg(x),
  call = caller_env()
) {
  abi <- language_abi(x)
  check_number_whole(abi, .internal = TRUE)

  if (abi > max) {
    message <- c(
      "{.arg {arg}} is an incompatible tree-sitter language object.",
      i = "{.arg {arg}} has an ABI version of {abi}.",
      i = "Maximum allowed ABI version is {max}.",
      i = paste0(
        "This typically means you should update the {.pkg treesitter} R package. ",
        "If that doesn't work, please open an issue on GitHub."
      )
    )
    cli::cli_abort(message, call = call)
  }

  if (abi < min) {
    message <- c(
      "{.arg {arg}} is an incompatible tree-sitter language object.",
      i = "{.arg {arg}} has an ABI version of {abi}.",
      i = "Minimum allowed ABI version is {min}.",
      i = paste0(
        "This typically means you should update the grammar R package associated ",
        "with this language object, i.e. {.pkg treesitter.{{language}}}. ",
        "If that doesn't work, please open an issue on GitHub."
      )
    )
    cli::cli_abort(message, call = call)
  }

  invisible(NULL)
}
