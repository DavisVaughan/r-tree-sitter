test_that("can pretty print `node`s", {
  text <- "
  fn <- function() {
    a <- 1 + 1
    if (a > 3) {
      TRUE
    }
  }

  3 + 3
  "

  parser <- parser(r())
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)

  expect_snapshot({
    node_print_s_expression(node)
  })
  expect_snapshot({
    node_print_s_expression(node, compact = FALSE)
  })
  expect_snapshot({
    node_print_s_expression(node, locations = FALSE)
  })
  expect_snapshot({
    node_print_s_expression(node, compact = FALSE, locations = FALSE)
  })
})
