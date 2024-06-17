test_that("can parse and reparse non-destructively", {
  text <- "1 + 1"
  parser <- parser(r())
  tree <- parser_parse(parser, text)

  expect_identical(tree_text(tree), text)

  new_text <- "xy + 1"
  start_byte <- 0
  start_point <- point(0, 0)
  old_end_byte <- 1
  old_end_point <- point(0, 1)
  new_end_byte <- 2
  new_end_point <- point(0, 2)

  new_tree <- parser_reparse(
    x = parser,
    text = new_text,
    tree = tree,
    start_byte = start_byte,
    start_point = start_point,
    old_end_byte = old_end_byte,
    old_end_point = old_end_point,
    new_end_byte = new_end_byte,
    new_end_point = new_end_point
  )

  expect_identical(tree_text(tree), text)
  expect_identical(tree_text(new_tree), new_text)

  expect_identical(node_end_byte(tree_root_node(tree)), 5)
  expect_identical(node_end_byte(tree_root_node(new_tree)), 6)

  expect_snapshot(tree)
  expect_snapshot(new_tree)
})

# ------------------------------------------------------------------------------
# print()

test_that("expected print method", {
  expect_snapshot(parser(r()))
})
