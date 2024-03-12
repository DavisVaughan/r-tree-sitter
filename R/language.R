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
