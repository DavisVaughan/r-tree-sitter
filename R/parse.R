text_parse <- function(x, language) {
  check_string(x)
  check_language(language)

  parser <- parser(language)

  tree <- parser_parse(parser, x)
  node <- tree_root_node(tree)

  node
}
