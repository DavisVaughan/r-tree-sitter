#' R tree-sitter language
#'
#' Generates a `tree_sitter_language` object for R.
#' 
#' @details
#' This language object was built from the following:
#' - URL: https://github.com/r-lib/tree-sitter-r
#' - SHA: 03e6c381ba3d3d4802865e0108c56c86a6ef3f85
#'
#' @returns A `tree_sitter_language` object.
#'
#' @export
#' @examples
#' language_r()
language_r <- function() {
  pointer <- .Call(ffi_language_r)
  new_language(pointer, name = "r")
}
