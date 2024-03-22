# ---
# repo: DavisVaughan/r-tree-sitter
# file: standalone-language.R
# last-updated: 2024-03-22
# license: https://unlicense.org
# ---
#
# ## Changelog
#
# nocov start

#' Construct a new tree-sitter language object
#'
#' @description
#' This function is a developer tool to wrap an external pointer to a
#' C level tree-sitter `const TSLanguage*`. It is not exported, but should
#' be copied into grammar specific R packages and called by them, providing
#' the language name and an external pointer to the result of their C level
#' `tree_sitter_{name}()` function.
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
#' @noRd
new_language <- function(name, pointer) {
  # TODO: Remove `name` argument if name is accessible in language object
  # https://github.com/tree-sitter/tree-sitter/pull/3184
  stopifnot(is.character(name), length(name) == 1L, !is.na(name))
  stopifnot(typeof(pointer) == "externalptr")

  out <- list(name = name, pointer = pointer)
  class(out) <- "tree_sitter_language"

  out
}

# nocov end
