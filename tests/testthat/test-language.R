# ------------------------------------------------------------------------------
# language_name()

test_that("can get language name", {
  expect_identical(language_name(r()), "r")
})

test_that("checks for valid object", {
  expect_snapshot(error = TRUE, {
    language_name(1)
  })
})

# ------------------------------------------------------------------------------
# language_version()

test_that("can get language version", {
  version <- language_version(r())
  expect_vector(version, ptype = double(), size = 1L)
})

test_that("checks for valid object", {
  expect_snapshot(error = TRUE, {
    language_version(1)
  })
})

# ------------------------------------------------------------------------------
# language_id_for_node_kind()

test_that("can get language id for named node", {
  id <- language_id_for_node_kind(r(), "program")
  expect_vector(id, ptype = integer(), size = 1L)
  expect_true(!is.na(id))
})

test_that("can get language id for unnamed node", {
  id <- language_id_for_node_kind(r(), "else", named = FALSE)
  expect_vector(id, ptype = integer(), size = 1L)
  expect_true(!is.na(id))
})

test_that("incorrect node kind returns NA", {
  expect_identical(language_id_for_node_kind(r(), "ffffoo"), NA_integer_)
})

test_that("recycles `named` to size of `kind`", {
  expect_snapshot(error = TRUE, {
    language_id_for_node_kind(r(), c("program", "else"), named = c(TRUE, FALSE, TRUE))
  })

  id <- language_id_for_node_kind(r(), c("program", "else"), named = TRUE)
  expect_vector(id, ptype = integer(), size = 2L)
  expect_true(!is.na(id)[[1L]])
  expect_true(is.na(id)[[2L]])

  id <- language_id_for_node_kind(r(), c("program", "else"), named = c(TRUE, FALSE))
  expect_vector(id, ptype = integer(), size = 2L)
  expect_true(all(!is.na(id)))
})

test_that("language_id_for_node_kind() validates inputs", {
  expect_snapshot(error = TRUE, {
    language_id_for_node_kind(1, "foo")
  })
  expect_snapshot(error = TRUE, {
    language_id_for_node_kind(r(), 1)
  })
  expect_snapshot(error = TRUE, {
    language_id_for_node_kind(r(), "foo", named = "x")
  })
})

# ------------------------------------------------------------------------------
# language_node_kind_for_id()

test_that("can get node kind for an id", {
  kind <- language_node_kind_for_id(r(), 1:2)
  expect_vector(kind, ptype = character(), size = 2L)
  expect_true(!any(is.na(kind)))
})

test_that("errors on bad IDs", {
  expect_snapshot(error = TRUE, {
    language_node_kind_for_id(r(), -1L)
  })
})

test_that("checks language type", {
  expect_snapshot(error = TRUE, {
    language_node_kind_for_id(1, 1L)
  })
})
