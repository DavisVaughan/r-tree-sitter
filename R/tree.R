Tree <- R6::R6Class(
  "Tree",
  cloneable = FALSE,
  private = list(
    .text = NULL,
    .language = NULL,
    .pointer = NULL
  ),
  public = list(
    initialize = function(text, language, pointer) {
      tree_initialize(self, private, text, language, pointer)
    },
    language = function() {
      tree_language(self, private)
    },
    text = function() {
      tree_text(self, private)
    },
    set_text = function(text) {
      tree_set_text(self, pointer, text)
    },
    root_node = function() {
      tree_root_node(self, private)
    }
  )
)

tree_initialize <- function(self, private, text, language, pointer) {
  private$.text <- text
  private$.language <- language
  private$.pointer <- pointer
  self
}

tree_language <- function(self, private) {
  private$.language
}

tree_text <- function(self, private) {
  private$.text
}

tree_set_text <- function(self, pointer, text) {
  check_string(text, allow_null = TRUE)
  private$.text <- text
  self
}

tree_root_node <- function(self, private) {
  pointer <- private$.pointer
  raw <- .Call(ffi_tree_root_node, pointer)
  new_node(raw, self)
}

check_tree <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is_tree(x)) {
    return(invisible(NULL))
  }
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a tree",
    ...,
    arg = arg,
    call = call
  )
}

is_tree <- function(x) {
  inherits(x, "Tree")
}
