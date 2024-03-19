#' Construct a new tree-sitter language object
#'
#' @description
#' This function is a developer tool to wrap an external pointer to a
#' C level tree-sitter `const TSLanguage*`.
#'
#' The `language()` function exported from your grammar specific R package
#' should call this, providing the language name and an external pointer to
#' the result of the C level `tree_sitter_{name}()` function.
#'
#' @param name `[string]`
#'
#'   The name of the language being wrapped.
#'
#' @param pointer `[external_pointer]`
#'
#'   An external pointer to a `const TSLanguage*`.
#'
#' @returns
#' A `tree_sitter_language` object.
#'
#' @export
new_language <- function(name, pointer) {
  # TODO: Remove `name` argument if name is accessible in language object
  check_string(name)
  check_language_pointer(pointer)

  out <- list(name = name, pointer = pointer)
  class(out) <- "tree_sitter_language"

  out
}

#' Language name
#'
#' Extract a language object's language name.
#'
#' @param x `[tree_sitter_language]`
#'
#'   A tree-sitter language object.
#'
#' @export
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
language_version <- function(x) {
  check_language(x)
  .Call(ffi_language_version, language_pointer(x))
}

#' Language IDs
#'
#' Get the integer language ID for a particular node kind. Can be useful
#' for exploring the grammar.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x `[tree_sitter_language]`
#'
#'   A tree-sitter language object.
#'
#' @param kind `[character]`
#'
#'   The node kinds to look up the ID for.
#'
#' @param named `[logical]`
#'
#'   Should named or anonymous nodes be looked up? Recycled to the
#'   size of `kind`.
#'
#' @returns
#' An integer vector the same size as `kind` containing either:
#' - The integer ID of the node kind, if known.
#' - `NA` if the node kind was not known.
#'
#' @export
#' @seealso [language_node_kind_for_id()]
language_id_for_node_kind <- function(x, kind, ..., named = TRUE) {
  check_dots_empty0(...)
  check_language(x)

  kind <- vec_cast(kind, character())
  named <- vec_cast(named, logical())

  size <- vec_size(kind)
  named <- vec_recycle(named, size, x_arg = "named")

  .Call(ffi_language_id_for_node_kind, language_pointer(x), kind, named)
}

#' Language node kinds
#'
#' Get the string node kind for a particular language ID. Can be useful for
#' exploring a grammar.
#'
#' @param x `[tree_sitter_language]`
#'
#'   A tree-sitter language object.
#'
#' @param id `[positive integer]`
#'
#'   The language IDs to look up node kinds for.
#'
#' @export
#' @seealso [language_id_for_node_kind()]
#' @returns
#' A character vector the same length as `id` containing:
#' - The name of the node kind, if known.
#' - `NA`, if the ID was not known.
language_node_kind_for_id <- function(x, id) {
  check_language(x)
  id <- vec_cast(id, integer())
  .Call(ffi_language_node_kind_for_id, language_pointer(x), id)
}

#' @export
print.tree_sitter_language <- function(x, ...) {
  name <- language_name(x)
  cat(sprintf("language<%s>", name))
  invisible(x)
}

language_pointer <- function(x) {
  x$pointer
}

is_language <- function(x) {
  inherits(x, "tree_sitter_language")
}

check_language <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is_language(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a <tree_sitter_language>",
    ...,
    arg = arg,
    call = call
  )
}
