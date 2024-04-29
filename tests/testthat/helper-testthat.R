read_file <- function(file) {
  paste(readLines(test_path("fixtures", file)), collapse = '')
}

test_that_captures <- function(code_source) {
  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, code_source)
  node <- tree_root_node(tree)
  query <- query(language, read_file("test_that.scm"))
  query_captures(query, node)
}
  
expect_test_that_captures <-function(code_source, desc, captures) {
  # make sure we captured something
  expect_gt(length(captures$node), 0)

  # the capture names that should always be present
  expect_contains(captures$name, c("call", "function", "desc"))
  # other captures, such as "pkg" or "param" could also be here

  expect_equal(
    node_text(captures[["node"]][[which(captures$name == "call")]]),
    code_source
  )
  expect_equal(
    node_text(captures[["node"]][[which(captures$name == "function")]]),
    "test_that"
  )
  expect_equal(
    node_text(captures[["node"]][[which(captures$name == "desc")]]),
    desc
  )
  if ("pkg" %in% captures$name) {
    expect_equal(
      node_text(captures[["node"]][[which(captures$name == "pkg")]]),
      "testthat"
    )
  }
  if ("param" %in% captures$name) {
    expect_equal(
      node_text(captures[["node"]][[which(captures$name == "param")]]),
      "desc"
    )
  }
}

expect_top_level <- function(node) {
  expect_equal(
    node_type(node_parent(node)),
    "program"
  )
}