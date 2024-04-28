#' TEMPLATE_NAME_PRETTY tree-sitter language
#'
#' Generates a `tree_sitter_language` object for TEMPLATE_NAME_PRETTY.
#' 
#' @details
#' This language object was built from the following:
#' - URL: TEMPLATE_URL
#' - SHA: TEMPLATE_SHA
#'
#' @returns A `tree_sitter_language` object.
#'
#' @export
#' @examples
#' language_TEMPLATE_NAME_LANGUAGE()
language_TEMPLATE_NAME_LANGUAGE <- function() {
  pointer <- .Call(ffi_language_TEMPLATE_NAME_LANGUAGE)
  new_language(pointer, name = "TEMPLATE_NAME_LANGUAGE")
}
