Parser <- R6::R6Class(
  "tree_sitter_parser",
  cloneable = FALSE,
  private = list(
    .pointer = NULL,
    .language = NULL
  ),
  public = list(
    initialize = function() {
      parser_initialize(self, private)
    },
    set_language = function(language) {
      parser_set_language(self, private, language)
    },
    parse = function(text, ..., tree = NULL) {
      check_dots_empty0(...)
      parser_parse(self, private, text, tree)
    }
  ),
  active = list(
    language = function(x) {
      parser_language(self, private, x)
    }
  )
)

parser_initialize <- function(self, private) {
  private$.pointer <- .Call(ffi_parser_initialize)
  self
}

parser_set_language <- function(self, private, language) {
  check_language(language)

  private$.language <- language
  language <- language_pointer(language)

  pointer <- private$.pointer

  .Call(ffi_parser_set_language, pointer, language)

  self
}

parser_language <- function(self, private, x) {
  if (missing(x)) {
    # Getter
    private$.language
  } else {
    # Setter
    parser_set_language(self, private, x)
  }
}

parser_parse <- function(self, private, text, tree) {
  check_string(text)
  check_tree(tree, allow_null = TRUE)

  pointer <- private$.pointer
  language <- private$.language

  if (is.null(language)) {
    abort("`language` must be set with `$set_language()` before calling `$parse()`.")
  }

  if (!is.null(tree)) {
    tree <- tree$pointer
  }

  text <- enc2utf8(text)

  pointer <- .Call(ffi_parser_parse, pointer, text, tree)

  Tree$new(text, language, pointer)
}