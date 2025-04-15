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
    node_show_s_expression(node, show_anonymous = FALSE)
  })
  expect_snapshot({
    node_show_s_expression(node, show_parentheses = FALSE)
  })
  expect_snapshot({
    node_show_s_expression(
      node,
      show_parentheses = FALSE,
      show_anonymous = FALSE,
      show_locations = FALSE
    )
  })
  expect_snapshot({
    node_show_s_expression(node, dangling_parenthesis = FALSE)
  })
  expect_snapshot({
    node_show_s_expression(node, show_locations = FALSE)
  })
  # With `dangling_parenthesis = TRUE` and `show_anonymous = FALSE`, `parameters`
  # node with only anonymous children should not move the closing `)` to the next line
  expect_snapshot({
    node_show_s_expression(
      node,
      dangling_parenthesis = TRUE,
      show_anonymous = FALSE
    )
  })
  expect_snapshot({
    node_show_s_expression(node, show_locations = FALSE)
  })
  expect_snapshot({
    node_show_s_expression(node, show_anonymous = FALSE, show_locations = FALSE)
  })
  expect_snapshot({
    node_show_s_expression(node, max_lines = 1)
  })
  expect_snapshot({
    node_show_s_expression(node, max_lines = 10)
  })
  expect_snapshot({
    node_show_s_expression(node, max_lines = 10, dangling_parenthesis = FALSE)
  })
  expect_snapshot({
    node_show_s_expression(
      node,
      max_lines = 10,
      dangling_parenthesis = FALSE,
      show_anonymous = FALSE
    )
  })
})

test_that("truncation doesn't show if you are exactly at `max_lines`", {
  text <- "1"

  parser <- parser(r())
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)

  expect_snapshot({
    node_show_s_expression(node, max_lines = 2, dangling_parenthesis = FALSE)
  })
})

test_that("Named `MISSING` nodes are shown", {
  # Missing body, error recovered a named `identifier`
  text <- "while (a > b)"

  parser <- parser(r())
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)

  expect_snapshot({
    node_show_s_expression(node)
  })
})

test_that("Anonymous `MISSING` nodes are shown", {
  # Missing anonymous `}`
  text <- "
  {{
    1
  }"

  parser <- parser(r())
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)

  expect_snapshot({
    node_show_s_expression(node)
  })
})
