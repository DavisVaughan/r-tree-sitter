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
    node_show_s_expression(node)
  })
  expect_snapshot({
    node_show_s_expression(node, anonymous = FALSE)
  })
  expect_snapshot({
    node_show_s_expression(node, compact = FALSE)
  })
  expect_snapshot({
    node_show_s_expression(node, locations = FALSE)
  })
  # With `compact = FALSE` and `anonymous = FALSE`, `parameters` node with only
  # anonymous children should not move the closing `)` to the next line
  expect_snapshot({
    node_show_s_expression(node, compact = FALSE, anonymous = FALSE)
  })
  expect_snapshot({
    node_show_s_expression(node, compact = FALSE, locations = FALSE)
  })
  expect_snapshot({
    node_show_s_expression(node, anonymous = FALSE, compact = FALSE, locations = FALSE)
  })
  expect_snapshot({
    node_show_s_expression(node, max_lines = 1)
  })
  expect_snapshot({
    node_show_s_expression(node, max_lines = 10)
  })
  expect_snapshot({
    node_show_s_expression(node, max_lines = 10, compact = FALSE)
  })
  expect_snapshot({
    node_show_s_expression(node, max_lines = 10, compact = FALSE, anonymous = FALSE)
  })
})

test_that("truncation doesn't show if you are exactly at `max_lines`", {
  text <- "1"

  parser <- parser(r())
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)

  expect_snapshot({
    node_show_s_expression(node, max_lines = 2)
  })
})
