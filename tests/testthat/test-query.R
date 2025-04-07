# ------------------------------------------------------------------------------
# query()

test_that("can create basic query with correct structure", {
  source <- r'[
  (
    (identifier) @id
    (identifier) @id2
    (#eq? @id "blah")
    (#eq? @id @id2)
  )
  (
    (identifier) @id3
    (#match? @id3 "^\\s$")
  )
  ]'

  query <- query(r(), source)

  expect_identical(query$capture_names, c("id", "id2", "id3"))

  # First pattern
  predicates <- query$pattern_predicates[[1]]

  eq_string <- predicates[[1]]
  expect_true(is_predicate_eq_string(eq_string))
  expect_identical(eq_string$capture_name_value_id, 0)
  expect_identical(eq_string$capture_value, "blah")
  expect_identical(eq_string$capture_invert, FALSE)

  eq_capture <- predicates[[2]]
  expect_true(is_predicate_eq_capture(eq_capture))
  expect_identical(eq_capture$capture_name_value_id, 0)
  expect_identical(eq_capture$capture_value_id, 1)
  expect_identical(eq_capture$capture_invert, FALSE)

  # Second pattern
  predicates <- query$pattern_predicates[[2]]

  # Note - tree sitter does the escaping, so the `capture_value` now only
  # has 1 backtick
  match_string <- predicates[[1]]
  expect_true(is_predicate_match_string(match_string))
  expect_identical(match_string$capture_name_value_id, 2)
  expect_identical(match_string$capture_value, r"[^\s$]")
  expect_identical(match_string$capture_invert, FALSE)
})

test_that("can detect `not-` cases", {
  source <- '[((identifier) @id (#not-eq? @id "blah"))]'
  query <- query(r(), source)
  predicate <- query$pattern_predicates[[1]][[1]]
  expect_identical(predicate$capture_invert, TRUE)

  source <- '[((identifier) @id (#not-match? @id "blah"))]'
  query <- query(r(), source)
  predicate <- query$pattern_predicates[[1]][[1]]
  expect_identical(predicate$capture_invert, TRUE)
})

test_that("single quoted strings throw an error", {
  source <- "
(binary_operator
  operator: '+'
)
  "

  language <- r()

  expect_snapshot(error = TRUE, {
    query(language, source)
  })
})

# ------------------------------------------------------------------------------
# query_matches()

test_that("using `*` always results in a match, even if its capture count is 0", {
  # Because `*` is defined as "0 or more", so `0` counts!
  text <- "
# hi1
# hi2
fn <- function() {}
  "

  # Must get an escaped `\\s` into the string itself (so, two literal `\`
  # characters followed by an `s`, which tree-sitter then reinterprets), easiest
  # way is a raw string.
  source <- R'[
  (
    (comment)* @roxygen
    (binary_operator
      operator: "<-"
    )
    (#match? @roxygen "^#'\\s.*")
  )
  ]'

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)
  matches <- query_matches(query, node)

  # Only 1 pattern
  matches <- matches[[1]]

  expect_identical(matches, list())
})

test_that("using `*` with no other restricting condition results in 1 match per node", {
  # No comments here, so no matches
  text <- "
1 + x
fn(y)
  "

  source <- R'[
  (
    (comment)* @comment
  )
  ]'

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)
  matches <- query_matches(query, node)

  # Only 1 pattern
  matches <- matches[[1]]

  expect_identical(length(matches), as.integer(node_descendant_count(node)))

  # Each one is an empty match - the `*` results in a match even with 0 hits
  # because it is "zero or more"
  expect_identical(matches[[1]]$name, character())
  expect_identical(matches[[1]]$node, list())
})

test_that("can restrict `range`", {
  text <- "
# hi
# hi again
  "

  source <- "
  (
    (comment) @comment
  )
  "

  range <- range(0, point(0, 0), 5, point(1, 4))

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)
  matches <- query_matches(query, node, range = range)

  # Only 1 pattern
  matches <- matches[[1]]

  # Only finds first `# hi`
  expect_length(matches, 1L)
  expect_identical(matches[[1]]$name, "comment")
  expect_identical(node_text(matches[[1]]$node[[1]]), "# hi")
})

# ------------------------------------------------------------------------------
# query_captures()

test_that("returns ordered list of captures", {
  text <- "
a + b + a + ab
and(a)
  "

  source <- "
  (
    (identifier) @id
    (#eq? @id a)
  )
  (
    (identifier) @id2
    (#eq? @id2 b)
  )
  "

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)
  captures <- query_captures(query, node)

  # Returns ordered list of captures, regardless of pattern
  expect_identical(captures$name, c("id", "id2", "id", "id"))

  expect_snapshot(captures$node)
})

test_that("can restrict `range`", {
  text <- "
# hi
# hi again
  "

  source <- "
  (
    (comment) @comment
  )
  "

  range <- range(0, point(0, 0), 5, point(1, 4))

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)
  captures <- query_captures(query, node, range = range)

  # Only finds first `# hi`
  expect_identical(captures$name, "comment")
  expect_identical(node_text(captures$node[[1]]), "# hi")
})

# ------------------------------------------------------------------------------
# `#eq?`, `#not-eq?`, `#any-eq?`, `#any-not-eq?` - strings

test_that("can use `#eq?` with string", {
  text <- "
a + b + a + ab
and(a)
  "

  source <- '
  (
    (identifier) @id
    (#eq? @id "a")
  )
  '

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)
  captures <- query_captures(query, node)

  # Returns ordered list of captures, regardless of pattern
  expect_length(captures$name, 3)

  expect_identical(node_range(captures$node[[1]]), range(1, point(1, 0), 2, point(1, 1)))
  expect_identical(node_range(captures$node[[2]]), range(9, point(1, 8), 10, point(1, 9)))
  expect_identical(node_range(captures$node[[3]]), range(20, point(2, 4), 21, point(2, 5)))
})

test_that("can use `#not-eq?` with string", {
  text <- "
a + b + a + ab
and(a)
  "

  source <- "
  (
    (identifier) @id
    (#not-eq? @id a)
  )
  "

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)
  captures <- query_captures(query, node)

  expect_identical(
    vapply(captures$node, node_text, character(1)),
    c("b", "ab", "and")
  )
})

test_that("can use `#any-eq?` with string", {
  text <- "
# this
#
# one
NULL

# dont match
# this one
NULL

#
# this
#
# one
#
NULL
  "

  # Find comment blocks where at least one line is an empty comment
  source <- '
(
  (comment)+ @comment
  (#any-eq? @comment "#")
)
  '

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)
  matches <- query_matches(query, node)

  # One query
  matches <- matches[[1]]

  # Two sets of comment blocks that match
  expect_length(matches, 2)

  expect_length(matches[[1]]$node, 3)
  expect_identical(node_range(matches[[1]]$node[[1]]), range(1, point(1, 0), 7, point(1, 6)))
  expect_identical(node_range(matches[[1]]$node[[2]]), range(8, point(2, 0), 9, point(2, 1)))
  expect_identical(node_range(matches[[1]]$node[[3]]), range(10, point(3, 0), 15, point(3, 5)))

  expect_length(matches[[2]]$node, 5)
  expect_identical(node_range(matches[[2]]$node[[1]]), range(52, point(10, 0), 53, point(10, 1)))
  expect_identical(node_range(matches[[2]]$node[[5]]), range(69, point(14, 0), 70, point(14, 1)))
})

test_that("can use `#any-not-eq?` with string", {
  text <- "
#
#
#
NULL

# match
# this one
NULL

#
# match this
#
# one
#
NULL
  "

  # Find comment blocks where at least one line is not an empty comment
  source <- '
(
  (comment)+ @comment
  (#any-not-eq? @comment "#")
)
  '

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)
  matches <- query_matches(query, node)

  # One query
  matches <- matches[[1]]

  # Two sets of comment blocks that match
  expect_length(matches, 2)

  # First set has 2 comments, second has 5 comments
  expect_length(matches[[1]]$node, 2)
  expect_length(matches[[2]]$node, 5)

  # Just test a few
  expect_identical(node_range(matches[[1]]$node[[1]]), range(13, point(6, 0), 20, point(6, 7)))
  expect_identical(node_range(matches[[1]]$node[[2]]), range(21, point(7, 0), 31, point(7, 10)))
})

test_that("can repeat capture name across patterns", {
  text <- "b + a"

  source <- "
  (
    (identifier) @id
    (#eq? @id a)
  )
  (
    (identifier) @id
    (#eq? @id b)
  )
  "

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)

  matches <- query_matches(query, node)
  captures <- query_captures(query, node)

  # Split by pattern order
  pattern <- matches[[1]]
  expect_identical(pattern[[1]]$name, "id")
  expect_identical(node_text(pattern[[1]]$node[[1]]), "a")

  pattern <- matches[[2]]
  expect_identical(pattern[[1]]$name, "id")
  expect_identical(node_text(pattern[[1]]$node[[1]]), "b")

  # Returns list of captures ordered by node position
  expect_identical(captures$name, c("id", "id"))
  expect_identical(node_text(captures$node[[1]]), "b")
  expect_identical(node_text(captures$node[[2]]), "a")
})

test_that("can use alternations with `#eq?`", {
  text <- "
x + y
1 + y
z + y
fn(x) + y
  "

  source <- '
  (
    (binary_operator
      lhs: [
        (identifier) @id
        (call) @call
      ]
      operator: "+"
    )
    (#eq? @id x)
  )
  '

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)
  matches <- query_matches(query, node)

  # Only 1 pattern
  matches <- matches[[1]]

  # 2 matches
  expect_length(matches, 2)

  match <- matches[[1]]
  expect_identical(match$name, "id")
  expect_identical(node_text(match$node[[1]]), "x")

  match <- matches[[2]]
  expect_identical(match$name, "call")
  expect_identical(node_text(match$node[[1]]), "fn(x)")
})

test_that("`#eq?` with string performs exact equality check (#28)", {
  text <- "
a + b + a + ab
and(a)
  "

  # Should match only `ab`, not also `a`
  source <- '
  (
    (identifier) @id
    (#eq? @id "ab")
  )
  '

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)
  captures <- query_captures(query, node)

  expect_length(captures$name, 1)

  expect_identical(node_range(captures$node[[1]]), range(13, point(1, 12), 15, point(1, 14)))
})

# ------------------------------------------------------------------------------
# `#eq?`, `#not-eq?`, `#any-eq?`, `#any-not-eq?` - captures

test_that("can use `#eq?` and `#not-eq?` with capture", {
  text <- "
x + y
x + x
a + b
xy + xy
  "

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)

  source <- '
  (
    (binary_operator
      lhs: (identifier) @id1
      operator: "+"
      rhs: (identifier) @id2
    )
    (#eq? @id1 @id2)
  )
  '
  query <- query(language, source)
  matches <- query_matches(query, node)

  # Only 1 pattern
  matches <- matches[[1]]

  # 2 matches
  match <- matches[[1]]
  expect_identical(match$name, c("id1", "id2"))
  expect_identical(vapply(match$node, node_text, character(1)), c("x", "x"))
  expect_identical(node_start_point(match$node[[1]]), point(2, 0))
  expect_identical(node_start_point(match$node[[2]]), point(2, 4))

  match <- matches[[2]]
  expect_identical(match$name, c("id1", "id2"))
  expect_identical(vapply(match$node, node_text, character(1)), c("xy", "xy"))
  expect_identical(node_start_point(match$node[[1]]), point(4, 0))
  expect_identical(node_start_point(match$node[[2]]), point(4, 5))

  source <- '
  (
    (binary_operator
      lhs: (identifier) @id1
      operator: "+"
      rhs: (identifier) @id2
    )
    (#not-eq? @id1 @id2)
  )
  '
  query <- query(language, source)
  matches <- query_matches(query, node)

  # Only 1 pattern
  matches <- matches[[1]]

  # 2 matches
  match <- matches[[1]]
  expect_identical(match$name, c("id1", "id2"))
  expect_identical(vapply(match$node, node_text, character(1)), c("x", "y"))

  match <- matches[[2]]
  expect_identical(match$name, c("id1", "id2"))
  expect_identical(vapply(match$node, node_text, character(1)), c("a", "b"))
})

test_that("can use `#any-eq?` and `#any-not-eq?` with capture", {
  text <- "
# This matches any-eq
# This matches any-not-eq
a <- a
b <- c

NULL

# This matches any-not-eq
a <- b
b <- c

NULL

# This matches any-eq
a <- a
b <- b

NULL

# This matches any-eq
# This matches any-not-eq
b <- c
c <- c
  "

  # Two queries:
  # - Find chunks of assignments where at least one assigns a name to itself
  # - Find chunks of assignments where at least one doesn't assign a name to itself
  source <- '
(
  (binary_operator
    lhs: (identifier) @lhs
    operator: "<-"
    rhs: (identifier) @rhs
  )+
  (#any-eq? @lhs @rhs)
)
(
  (binary_operator
    lhs: (identifier) @lhs
    operator: "<-"
    rhs: (identifier) @rhs
  )+
  (#any-not-eq? @lhs @rhs)
)
  '

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)
  matches <- query_matches(query, node)

  # Two queries
  expect_length(matches, 2)

  # Both sets of query matches have 3 distinct matches
  expect_length(matches[[1]], 3)
  expect_length(matches[[2]], 3)

  # Query 1 - #any-eq?
  match <- matches[[1]]

  expect_identical(
    vapply(match[[1]]$node, node_text, FUN.VALUE = character(1)),
    c("a", "a", "b", "c")
  )
  expect_identical(
    vapply(match[[2]]$node, node_text, FUN.VALUE = character(1)),
    c("a", "a", "b", "b")
  )
  expect_identical(
    vapply(match[[3]]$node, node_text, FUN.VALUE = character(1)),
    c("b", "c", "c", "c")
  )

  # Query 2 - #any-not-eq?
  match <- matches[[2]]

  expect_identical(
    vapply(match[[1]]$node, node_text, FUN.VALUE = character(1)),
    c("a", "a", "b", "c")
  )
  expect_identical(
    vapply(match[[2]]$node, node_text, FUN.VALUE = character(1)),
    c("a", "b", "b", "c")
  )
  expect_identical(
    vapply(match[[3]]$node, node_text, FUN.VALUE = character(1)),
    c("b", "c", "c", "c")
  )
})

test_that("can use `#eq?` capture predicate on fairly complicated case", {
  text <- "
ab = abc + 1
def = de + 1
ghi = ghi + 1 # this
ghi = ghi - 1
x = x + match(a, b) # and this
  "

  source <- '
  (
    (binary_operator
      lhs: (identifier) @id1
      operator: "="
      rhs: (binary_operator
        lhs: (identifier) @id2
        operator: "+"
        rhs: (_) @expr
      )
    )
    (#eq? @id1 @id2)
  )
  '

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)
  captures <- query_captures(query, node)

  expect_identical(captures$name, c("id1", "id2", "expr", "id1", "id2", "expr"))

  capture <- captures$node[[1]]
  expect_identical(node_text(capture), "ghi")
  expect_identical(node_start_point(capture), point(3, 0))
  expect_identical(node_end_point(capture), point(3, 3))

  capture <- captures$node[[2]]
  expect_identical(node_text(capture), "ghi")
  expect_identical(node_start_point(capture), point(3, 6))
  expect_identical(node_end_point(capture), point(3, 9))

  capture <- captures$node[[3]]
  expect_identical(node_text(capture), "1")

  capture <- captures$node[[4]]
  expect_identical(node_text(capture), "x")
  expect_identical(node_start_point(capture), point(5, 0))
  expect_identical(node_end_point(capture), point(5, 1))

  capture <- captures$node[[6]]
  expect_identical(node_text(capture), "match(a, b)")
})

test_that("`#eq?` with capture performs exact equality check (#28)", {
  text <- "
a + a
a + ab
  "

  # Should match only `a + a`, not also `a + ab`
  source <- '
  (
    (binary_operator
      lhs: (identifier) @id
      rhs: (identifier) @id2
    )
    (#eq? @id @id2)
  )
  '

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)
  captures <- query_captures(query, node)

  expect_identical(captures$name, c("id", "id2"))

  expect_identical(node_range(captures$node[[1]]), range(1, point(1, 0), 2, point(1, 1)))
  expect_identical(node_range(captures$node[[2]]), range(5, point(1, 4), 6, point(1, 5)))
})

# ------------------------------------------------------------------------------
# `#match?`, `#not-match?`, `#any-match?`, `#any-not-match?`

test_that("can use `#match?` and `#not-match?` with pattern", {
  text <- "
# comment
#' roxygen
#' another
#' hi
# comment2
#' roxy
#' roxy again
  "

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)

  source <- r'[
  (
    (comment) @roxygen
    (#match? @roxygen "^#'.*")
  )
  ]'
  query <- query(language, source)
  captures <- query_captures(query, node)

  expect_length(captures$node, 5)

  expect_identical(
    vapply(captures$node, node_text, character(1)),
    c("#' roxygen", "#' another", "#' hi", "#' roxy", "#' roxy again")
  )

  source <- r'[
    (
      (comment) @roxygen
      (#not-match? @roxygen "^#'.*")
    )
    ]'
    query <- query(language, source)
    captures <- query_captures(query, node)

    expect_length(captures$node, 2)

    expect_identical(
      vapply(captures$node, node_text, character(1)),
      c("# comment", "# comment2")
    )
})

test_that("can use `#any-match?` and `#any-not-match?` with pattern", {
  text <- "
# This matches any-match
# This matches any-not-match
a_ish <- a
b <- c

NULL

# This matches any-not-match
a <- b
b <- c

NULL

# This matches any-match
a_ish <- a_ish
b_ish <- b_ish

NULL

# This matches any-match
# This matches any-not-match
b <- c
c <- c_ish
  "

  # Two queries:
  # - Find chunks of assignments where at least one name contains `_`
  # - Find chunks of assignments where at least one name doesn't contain `_`
  source <- '
(
  (binary_operator
    lhs: (identifier) @name
    operator: "<-"
    rhs: (identifier) @name
  )+
  (#any-match? @name "_")
)
(
  (binary_operator
    lhs: (identifier) @name
    operator: "<-"
    rhs: (identifier) @name
  )+
  (#any-not-match? @name "_")
)
  '

  language <- r()
  parser <- parser(language)
  tree <- parser_parse(parser, text)
  node <- tree_root_node(tree)
  query <- query(language, source)
  matches <- query_matches(query, node)

  # Two queries
  expect_length(matches, 2)

  # Both sets of query matches have 3 distinct matches
  expect_length(matches[[1]], 3)
  expect_length(matches[[2]], 3)

  # Query 1 - #any-match?
  match <- matches[[1]]

  expect_identical(
    vapply(match[[1]]$node, node_text, FUN.VALUE = character(1)),
    c("a_ish", "a", "b", "c")
  )
  expect_identical(
    vapply(match[[2]]$node, node_text, FUN.VALUE = character(1)),
    c("a_ish", "a_ish", "b_ish", "b_ish")
  )
  expect_identical(
    vapply(match[[3]]$node, node_text, FUN.VALUE = character(1)),
    c("b", "c", "c", "c_ish")
  )

  # Query 2 - #any-not-match?
  match <- matches[[2]]

  expect_identical(
    vapply(match[[1]]$node, node_text, FUN.VALUE = character(1)),
    c("a_ish", "a", "b", "c")
  )
  expect_identical(
    vapply(match[[2]]$node, node_text, FUN.VALUE = character(1)),
    c("a", "b", "b", "c")
  )
  expect_identical(
    vapply(match[[3]]$node, node_text, FUN.VALUE = character(1)),
    c("b", "c", "c", "c_ish")
  )
})

# ------------------------------------------------------------------------------
# query_start_byte_for_pattern() / query_end_byte_for_pattern()

test_that("has OOB handling built in", {
  source <- "(identifier) @id"
  language <- r()
  query <- query(language, source)

  expect_snapshot(error = TRUE, {
    query_start_byte_for_pattern(query, 0)
  })
  expect_snapshot(error = TRUE, {
    query_end_byte_for_pattern(query, 0)
  })

  expect_identical(query_start_byte_for_pattern(query, 1), 0)
  expect_identical(query_start_byte_for_pattern(query, 2), NA_real_)
  expect_identical(query_start_byte_for_pattern(query, 3), NA_real_)

  expect_identical(query_end_byte_for_pattern(query, 1), 16)
  expect_identical(query_end_byte_for_pattern(query, 2), NA_real_)
  expect_identical(query_end_byte_for_pattern(query, 3), NA_real_)
})
