read_file <- function(file) {
  paste(readLines(test_path("fixtures", file)), collapse = '')
}

get_captures <- function(code_source, query_source) {
  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, code_source)
  node <- tree_root_node(tree)
  query <- query(language, query_source)
  query_captures(query, node)
}

get_matches <- function(code_source, query_source) {
  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, code_source)
  node <- tree_root_node(tree)
  query <- query(language, query_source)
  query_matches(query, node)[[1]]
}

expect_top_level <- function(node) {
  expect_equal(
    node_type(node_parent(node)),
    "program"
  )
}
