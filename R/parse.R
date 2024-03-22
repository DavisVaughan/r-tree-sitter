parse_text <- function(x, language) {
  check_string(x)
  check_language(language)

  parser <- Parser$new()
  parser$set_language(language)

  tree <- parser$parse(x)
  node <- tree$root_node()

  node
}
