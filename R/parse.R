#' Parse a snippet of text
#'
#' @description
#' `text_parse()` is a convenience utility for quickly parsing a small snippet
#' of text using a particular language and getting access to its root node. It
#' is meant for demonstration purposes. If you are going to need to reparse the
#' text after an edit has been made, you should create a full parser with
#' [parser()] and use [parser_parse()] instead.
#'
#' @param x `[string]`
#'
#'   The text to parse.
#'
#' @param language `[tree_sitter_language]`
#'
#'   The language to parse with.
#'
#' @returns
#' A root node.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' text <- "map(xs, function(x) 1 + 1)"
#'
#' # Note that this directly returns the root node, not the tree
#' text_parse(text, language)
text_parse <- function(x, language) {
  check_string(x)
  check_language(language)

  parser <- parser(language)

  tree <- parser_parse(parser, x)
  node <- tree_root_node(tree)

  node
}
