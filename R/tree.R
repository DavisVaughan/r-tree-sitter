Tree <- R6::R6Class(
  "tree_sitter_tree",
  cloneable = FALSE,
  private = list(
    .text = NULL,
    .edited = FALSE,
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
    pointer = function() {
      tree_pointer(self, private)
    },
    text = function() {
      tree_text(self, private)
    },
    root_node = function() {
      tree_root_node(self, private)
    },
    edit = function(
      start_byte,
      start_row,
      start_column,
      old_end_byte,
      old_end_row,
      old_end_column,
      new_end_byte,
      new_end_row,
      new_end_column
    ) {
      tree_edit(
        self,
        private,
        start_byte,
        start_row,
        start_column,
        old_end_byte,
        old_end_row,
        old_end_column,
        new_end_byte,
        new_end_row,
        new_end_column
      )
    },
    edited = function() {
      tree_edited(self, private)
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

tree_pointer <- function(self, private) {
  private$.pointer
}

tree_text <- function(self, private) {
  private$.text
}

tree_root_node <- function(self, private) {
  pointer <- private$.pointer
  raw <- .Call(ffi_tree_root_node, pointer)
  new_node(raw, self)
}

tree_edit <- function(
  self,
  private,
  start_byte,
  start_row,
  start_column,
  old_end_byte,
  old_end_row,
  old_end_column,
  new_end_byte,
  new_end_row,
  new_end_column
) {
  pointer <- private$.pointer

  args <- vec_cast_common(
    start_byte = start_byte,
    start_row = start_row,
    start_column = start_column,
    old_end_byte = old_end_byte,
    old_end_row = old_end_row,
    old_end_column = old_end_column,
    new_end_byte = new_end_byte,
    new_end_row = new_end_row,
    new_end_column = new_end_column,
    .to = double()
  )

  start_byte <- args$start_byte
  start_row <- args$start_row
  start_column <- args$start_column
  old_end_byte <- args$old_end_byte
  old_end_row <- args$old_end_row
  old_end_column <- args$old_end_column
  new_end_byte <- args$new_end_byte
  new_end_row <- args$new_end_row
  new_end_column <- args$new_end_column

  check_number_whole(start_byte, min = 0)
  check_number_whole(start_row, min = 0)
  check_number_whole(start_column, min = 0)
  check_number_whole(old_end_byte, min = 0)
  check_number_whole(old_end_row, min = 0)
  check_number_whole(old_end_column, min = 0)
  check_number_whole(new_end_byte, min = 0)
  check_number_whole(new_end_row, min = 0)
  check_number_whole(new_end_column, min = 0)

  .Call(
    ffi_tree_edit,
    pointer,
    start_byte,
    start_row,
    start_column,
    old_end_byte,
    old_end_row,
    old_end_column,
    new_end_byte,
    new_end_row,
    new_end_column
  )

  # Important! No longer valid to look at `text` after editing.
  private$.text <- NULL
  private$.edited <- TRUE

  self
}

tree_edited <- function(self, private) {
  private$.edited
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
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

is_tree <- function(x) {
  inherits(x, "tree_sitter_tree")
}
