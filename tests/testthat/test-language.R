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
# language_symbol_for_name()

test_that("can get language id for named node", {
  id <- language_symbol_for_name(r(), "program")
  expect_vector(id, ptype = integer(), size = 1L)
  expect_true(!is.na(id))
})

test_that("can get language id for unnamed node", {
  id <- language_symbol_for_name(r(), "else", named = FALSE)
  expect_vector(id, ptype = integer(), size = 1L)
  expect_true(!is.na(id))
})

test_that("incorrect node name returns NA", {
  expect_identical(language_symbol_for_name(r(), "ffffoo"), NA_integer_)
})

test_that("recycles `named` to size of `name`", {
  expect_snapshot(error = TRUE, {
    language_symbol_for_name(r(), c("program", "else"), named = c(TRUE, FALSE, TRUE))
  })

  id <- language_symbol_for_name(r(), c("program", "else"), named = TRUE)
  expect_vector(id, ptype = integer(), size = 2L)
  expect_true(!is.na(id)[[1L]])
  expect_true(is.na(id)[[2L]])

  id <- language_symbol_for_name(r(), c("program", "else"), named = c(TRUE, FALSE))
  expect_vector(id, ptype = integer(), size = 2L)
  expect_true(all(!is.na(id)))
})

test_that("language_symbol_for_name() validates inputs", {
  expect_snapshot(error = TRUE, {
    language_symbol_for_name(1, "foo")
  })
  expect_snapshot(error = TRUE, {
    language_symbol_for_name(r(), 1)
  })
  expect_snapshot(error = TRUE, {
    language_symbol_for_name(r(), "foo", named = "x")
  })
})

# ------------------------------------------------------------------------------
# language_symbol_name()

test_that("can get symbol name for a symbol ID", {
  kind <- language_symbol_name(r(), 1:2)
  expect_vector(kind, ptype = character(), size = 2L)
  expect_true(!any(is.na(kind)))
})

test_that("errors on bad symbol IDs", {
  expect_snapshot(error = TRUE, {
    language_symbol_name(r(), -1L)
  })
})

test_that("checks language type", {
  expect_snapshot(error = TRUE, {
    language_symbol_name(1, 1L)
  })
})

# ------------------------------------------------------------------------------
# check_language_abi()

test_that("Language objects that are too old can be caught", {
  x <- list(abi = 12L)

  expect_snapshot(error = TRUE, {
    check_language_abi(x, min = 13L, max = 14L)
  })
})

test_that("Language objects that are too new can be caught", {
  x <- list(abi = 12L)

  expect_snapshot(error = TRUE, {
    check_language_abi(x, min = 10L, max = 11L)
  })
})

# ------------------------------------------------------------------------------
# print()

test_that("expected print method", {
  expect_snapshot(r())
})
