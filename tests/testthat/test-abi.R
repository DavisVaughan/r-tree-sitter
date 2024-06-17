test_that("Current ABI version is as expected", {
  expect_identical(tree_sitter_abi(), 14L)
})

test_that("Minimum ABI version is as expected", {
  expect_identical(tree_sitter_minimum_compatible_abi(), 13L)
})
