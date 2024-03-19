Parser <- R6::R6Class(
  "Parser",
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
    language = function() {
      parser_language(self, private)
    },
    parse = function(text) {
      parser_parse(self, private, text)
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

parser_language <- function(self, private) {
  private$.language
}

parser_parse <- function(self, private, text) {
  pointer <- private$.pointer
  language <- private$.language

  text <- enc2utf8(text)

  pointer <- .Call(ffi_parser_parse, pointer, text)

  # Tree$new(pointer, language)
}
