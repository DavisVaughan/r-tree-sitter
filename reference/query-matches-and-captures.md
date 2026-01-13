# Query matches and captures

These two functions execute a query on a given `node`, and return the
captures of the query for further use. Both functions return the same
information, just structured differently depending on your use case.

- `query_matches()` returns the captures first grouped by *pattern*, and
  further grouped by *match* within each pattern. This is useful if you
  include multiple patterns in your query.

- `query_captures()` returns a flat list of captures ordered by their
  node location in the original text. This is normally the easiest
  structure to use if you have a single pattern without any alternations
  that would benefit from having individual captures split by match.

Both also return the capture name, i.e. the `@name` you specified in
your query.

## Usage

``` r
query_matches(x, node, ..., range = NULL)

query_captures(x, node, ..., range = NULL)
```

## Arguments

- x:

  `[tree_sitter_query]`

  A query.

- node:

  `[tree_sitter_node]`

  A node to run the query over.

- ...:

  These dots are for future extensions and must be empty.

- range:

  `[tree_sitter_range / NULL]`

  An optional range to restrict the query to.

## Predicates

There are 3 core types of predicates supported:

- `#eq? @capture "string"`

- `#eq? @capture1 @capture2`

- `#match? @capture "regex"`

Here are a few examples:

    # Match an identifier named `"name-of-interest"`
    (
      (identifier) @id
      (#eq? @id "name-of-interest")
    )

    # Match a binary operator where the left and right sides are the same name
    (
      (binary_operator
        lhs: (identifier) @id1
        rhs: (identifier) @id2
      )
      (#eq? @id1 @id2)
    )

    # Match a name with a `_` in it
    (
      (identifier) @id
      (#match? @id "_")
    )

Each of these predicates can be inverted with a `not-` prefix.

    (
      (identifier) @id
      (#not-eq? @id "name-of-interest")
    )

Each of these predicates can be converted from an *all* style predicate
to an *any* style predicate with an `any-` prefix. This is only useful
with *quantified* captures, i.e. `(comment)+`, where the `+` specifies
"one or more comment".

    # Finds a block of comments where ALL comments are empty comments
    (
      (comment)+ @comment
      (#eq? @comment "#")
    )

    # Finds a block of comments where ANY comments are empty comments
    (
      (comment)+ @comment
      (#any-eq? @comment "#")
    )

This is the full list of possible predicate permutations:

- `#eq?`

- `#not-eq?`

- `#any-eq?`

- `#any-not-eq?`

- `#match?`

- `#not-match?`

- `#any-match?`

- `#any-not-match?`

### String double quotes

The underlying tree-sitter predicate parser requires that strings
supplied in a query must use double quotes, i.e. `"string"` not
`'string'`. If you try and use single quotes, you will get a query
error.

### `#match?` regex

The regex support provided by `#match?` is powered by
[`grepl()`](https://rdrr.io/r/base/grep.html).

Escapes are a little tricky to get right within these match regex
strings. To use something like `\s` in the regex string, you need the
literal text `\\s` to appear in the string to tell the tree-sitter regex
engine to escape the backslash so you end up with just `\s` in the
captured string. This requires putting two literal backslash characters
in the R string itself, which can be accomplished with either `"\\\\s"`
or using a raw string like `r'["\\\\s"]'` which is typically a little
easier. You can also write your queries in a separate file (typically
called `queries.scm`) and read them into R, which is also a little more
straightforward because you can just write something like
`(#match? @id "^\\s$")` and that will be read in correctly.

## Examples

``` r
# ---------------------------------------------------------------------------
# Simple query

text <- "
foo + b + a + ab
and(a)
"

source <- "
(identifier) @id
"

language <- treesitter.r::language()

query <- query(language, source)
parser <- parser(language)
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# A flat ordered list of captures, that's most useful here since
# we only have 1 pattern!
captures <- query_captures(query, node)
captures$node
#> [[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> foo
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(1, 0), (1, 3)])
#> 
#> [[2]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> b
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(1, 6), (1, 7)])
#> 
#> [[3]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> a
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(1, 10), (1, 11)])
#> 
#> [[4]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> ab
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(1, 14), (1, 16)])
#> 
#> [[5]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> and
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(2, 0), (2, 3)])
#> 
#> [[6]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> a
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(2, 4), (2, 5)])
#> 

# ---------------------------------------------------------------------------
# Quantified query

text <- "
# this
# that
NULL

# and
# here
1 + 1

# there
2
"

# Find blocks of one or more comments
# The `+` is a regex `+` meaning "one or more" comments in a row
source <- "
(comment)+ @comment
"

language <- treesitter.r::language()

query <- query(language, source)
parser <- parser(language)
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# The extra structure provided by `query_matches()` is useful here so
# we can see the 3 distinct blocks of comments
matches <- query_matches(query, node)

# We provided one query pattern, so lets extract that
matches <- matches[[1]]

# 3 blocks of comments
matches[[1]]
#> $name
#> [1] "comment" "comment"
#> 
#> $node
#> $node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> # this
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(1, 0), (1, 6)])
#> 
#> $node[[2]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> # that
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(2, 0), (2, 6)])
#> 
#> 
matches[[2]]
#> $name
#> [1] "comment" "comment"
#> 
#> $node
#> $node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> # and
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(5, 0), (5, 5)])
#> 
#> $node[[2]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> # here
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(6, 0), (6, 6)])
#> 
#> 
matches[[3]]
#> $name
#> [1] "comment"
#> 
#> $node
#> $node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> # there
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(9, 0), (9, 7)])
#> 
#> 

# ---------------------------------------------------------------------------
# Multiple query patterns

# If you know you need to run multiple queries, you can run them all at once
# in one pass over the tree by providing multiple query patterns.

text <- "
a <- 1
b <- function() {}
c <- b
"

# Use an extra set of `()` to separate multiple query patterns
source <- "
(
  (identifier) @id
)
(
  (binary_operator) @binary
)
"

language <- treesitter.r::language()

query <- query(language, source)
parser <- parser(language)
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# The extra structure provided by `query_matches()` is useful here so
# we can separate the two queries
matches <- query_matches(query, node)

# First query - all identifiers
matches[[1]]
#> [[1]]
#> [[1]]$name
#> [1] "id"
#> 
#> [[1]]$node
#> [[1]]$node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> a
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(1, 0), (1, 1)])
#> 
#> 
#> 
#> [[2]]
#> [[2]]$name
#> [1] "id"
#> 
#> [[2]]$node
#> [[2]]$node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> b
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(2, 0), (2, 1)])
#> 
#> 
#> 
#> [[3]]
#> [[3]]$name
#> [1] "id"
#> 
#> [[3]]$node
#> [[3]]$node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> c
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(3, 0), (3, 1)])
#> 
#> 
#> 
#> [[4]]
#> [[4]]$name
#> [1] "id"
#> 
#> [[4]]$node
#> [[4]]$node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> b
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(3, 5), (3, 6)])
#> 
#> 
#> 

# Second query - all binary operators
matches[[2]]
#> [[1]]
#> [[1]]$name
#> [1] "binary"
#> 
#> [[1]]$node
#> [[1]]$node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> a <- 1
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (binary_operator [(1, 0), (1, 6)]
#>   lhs: (identifier [(1, 0), (1, 1)])
#>   operator: "<-" [(1, 2), (1, 4)]
#>   rhs: (float [(1, 5), (1, 6)])
#> )
#> 
#> 
#> 
#> [[2]]
#> [[2]]$name
#> [1] "binary"
#> 
#> [[2]]$node
#> [[2]]$node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> b <- function() {}
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (binary_operator [(2, 0), (2, 18)]
#>   lhs: (identifier [(2, 0), (2, 1)])
#>   operator: "<-" [(2, 2), (2, 4)]
#>   rhs: (function_definition [(2, 5), (2, 18)]
#>     name: "function" [(2, 5), (2, 13)]
#>     parameters: (parameters [(2, 13), (2, 15)]
#>       open: "(" [(2, 13), (2, 14)]
#>       close: ")" [(2, 14), (2, 15)]
#>     )
#>     body: (braced_expression [(2, 16), (2, 18)]
#>       open: "{" [(2, 16), (2, 17)]
#>       close: "}" [(2, 17), (2, 18)]
#>     )
#>   )
#> )
#> 
#> 
#> 
#> [[3]]
#> [[3]]$name
#> [1] "binary"
#> 
#> [[3]]$node
#> [[3]]$node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> c <- b
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (binary_operator [(3, 0), (3, 6)]
#>   lhs: (identifier [(3, 0), (3, 1)])
#>   operator: "<-" [(3, 2), (3, 4)]
#>   rhs: (identifier [(3, 5), (3, 6)])
#> )
#> 
#> 
#> 

# ---------------------------------------------------------------------------
# The `#eq?` and `#match?` predicates

text <- '
fn(a, b)

test_that("this", {
  test
})

fn_name(args)

test_that("that", {
  test
})

fn2_(args)
'

language <- treesitter.r::language()
parser <- parser(language)
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# Use an extra set of outer `()` when you are applying a predicate to ensure
# the query pattern is grouped with the query predicate.
# This one finds all function calls where the function name is `test_that`.
source <- '
(
  (call
    function: (identifier) @name
  ) @call
  (#eq? @name "test_that")
)
'

query <- query(language, source)

# It's fine to have a flat list of captures here, but we probably want to
# remove the `@name` captures and just retain the full `@call` captures.
captures <- query_captures(query, node)
captures$node[captures$name == "call"]
#> [[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> test_that("this", {
#>   test
#> })
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (call [(3, 0), (5, 2)]
#>   function: (identifier [(3, 0), (3, 9)])
#>   arguments: (arguments [(3, 9), (5, 2)]
#>     open: "(" [(3, 9), (3, 10)]
#>     argument: (argument [(3, 10), (3, 16)]
#>       value: (string [(3, 10), (3, 16)]
#>         open: "\"" [(3, 10), (3, 11)]
#>         content: (string_content [(3, 11), (3, 15)])
#>         close: "\"" [(3, 15), (3, 16)]
#>       )
#>     )
#>     (comma [(3, 16), (3, 17)])
#>     argument: (argument [(3, 18), (5, 1)]
#>       value: (braced_expression [(3, 18), (5, 1)]
#>         open: "{" [(3, 18), (3, 19)]
#>         body: (identifier [(4, 2), (4, 6)])
#>         close: "}" [(5, 0), (5, 1)]
#>       )
#>     )
#>     close: ")" [(5, 1), (5, 2)]
#>   )
#> )
#> 
#> [[2]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> test_that("that", {
#>   test
#> })
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (call [(9, 0), (11, 2)]
#>   function: (identifier [(9, 0), (9, 9)])
#>   arguments: (arguments [(9, 9), (11, 2)]
#>     open: "(" [(9, 9), (9, 10)]
#>     argument: (argument [(9, 10), (9, 16)]
#>       value: (string [(9, 10), (9, 16)]
#>         open: "\"" [(9, 10), (9, 11)]
#>         content: (string_content [(9, 11), (9, 15)])
#>         close: "\"" [(9, 15), (9, 16)]
#>       )
#>     )
#>     (comma [(9, 16), (9, 17)])
#>     argument: (argument [(9, 18), (11, 1)]
#>       value: (braced_expression [(9, 18), (11, 1)]
#>         open: "{" [(9, 18), (9, 19)]
#>         body: (identifier [(10, 2), (10, 6)])
#>         close: "}" [(11, 0), (11, 1)]
#>       )
#>     )
#>     close: ")" [(11, 1), (11, 2)]
#>   )
#> )
#> 

# This one finds all functions with a `_` in their name. It uses the R
# level `grepl()` for the regex processing.
source <- '
(
  (call
    function: (identifier) @name
  ) @call
  (#match? @name "_")
)
'

query <- query(language, source)

captures <- query_captures(query, node)
captures$node[captures$name == "call"]
#> [[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> test_that("this", {
#>   test
#> })
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (call [(3, 0), (5, 2)]
#>   function: (identifier [(3, 0), (3, 9)])
#>   arguments: (arguments [(3, 9), (5, 2)]
#>     open: "(" [(3, 9), (3, 10)]
#>     argument: (argument [(3, 10), (3, 16)]
#>       value: (string [(3, 10), (3, 16)]
#>         open: "\"" [(3, 10), (3, 11)]
#>         content: (string_content [(3, 11), (3, 15)])
#>         close: "\"" [(3, 15), (3, 16)]
#>       )
#>     )
#>     (comma [(3, 16), (3, 17)])
#>     argument: (argument [(3, 18), (5, 1)]
#>       value: (braced_expression [(3, 18), (5, 1)]
#>         open: "{" [(3, 18), (3, 19)]
#>         body: (identifier [(4, 2), (4, 6)])
#>         close: "}" [(5, 0), (5, 1)]
#>       )
#>     )
#>     close: ")" [(5, 1), (5, 2)]
#>   )
#> )
#> 
#> [[2]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn_name(args)
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (call [(7, 0), (7, 13)]
#>   function: (identifier [(7, 0), (7, 7)])
#>   arguments: (arguments [(7, 7), (7, 13)]
#>     open: "(" [(7, 7), (7, 8)]
#>     argument: (argument [(7, 8), (7, 12)]
#>       value: (identifier [(7, 8), (7, 12)])
#>     )
#>     close: ")" [(7, 12), (7, 13)]
#>   )
#> )
#> 
#> [[3]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> test_that("that", {
#>   test
#> })
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (call [(9, 0), (11, 2)]
#>   function: (identifier [(9, 0), (9, 9)])
#>   arguments: (arguments [(9, 9), (11, 2)]
#>     open: "(" [(9, 9), (9, 10)]
#>     argument: (argument [(9, 10), (9, 16)]
#>       value: (string [(9, 10), (9, 16)]
#>         open: "\"" [(9, 10), (9, 11)]
#>         content: (string_content [(9, 11), (9, 15)])
#>         close: "\"" [(9, 15), (9, 16)]
#>       )
#>     )
#>     (comma [(9, 16), (9, 17)])
#>     argument: (argument [(9, 18), (11, 1)]
#>       value: (braced_expression [(9, 18), (11, 1)]
#>         open: "{" [(9, 18), (9, 19)]
#>         body: (identifier [(10, 2), (10, 6)])
#>         close: "}" [(11, 0), (11, 1)]
#>       )
#>     )
#>     close: ")" [(11, 1), (11, 2)]
#>   )
#> )
#> 
#> [[4]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn2_(args)
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (call [(13, 0), (13, 10)]
#>   function: (identifier [(13, 0), (13, 4)])
#>   arguments: (arguments [(13, 4), (13, 10)]
#>     open: "(" [(13, 4), (13, 5)]
#>     argument: (argument [(13, 5), (13, 9)]
#>       value: (identifier [(13, 5), (13, 9)])
#>     )
#>     close: ")" [(13, 9), (13, 10)]
#>   )
#> )
#> 

# ---------------------------------------------------------------------------
# The `any-` and `not-` predicate modifiers

text <- '
# 1
#
# 2
NULL

# 3
# 4
NULL

#
#
NULL

#
# 5
#
# 6
#
NULL
'

language <- treesitter.r::language()
parser <- parser(language)
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# Two queries:
# - Find comment blocks where there is at least one empty comment
# - Find comment blocks where there is at least one non-empty comment
source <- '
(
  (comment)+ @comment
  (#any-eq? @comment "#")
)
(
  (comment)+ @comment
  (#any-not-eq? @comment "#")
)
'

query <- query(language, source)

matches <- query_matches(query, node)

# Query 1 has 3 comment blocks that match
query1 <- matches[[1]]
query1[[1]]
#> $name
#> [1] "comment" "comment" "comment"
#> 
#> $node
#> $node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> # 1
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(1, 0), (1, 3)])
#> 
#> $node[[2]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> #
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(2, 0), (2, 1)])
#> 
#> $node[[3]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> # 2
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(3, 0), (3, 3)])
#> 
#> 
query1[[2]]
#> $name
#> [1] "comment" "comment"
#> 
#> $node
#> $node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> #
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(10, 0), (10, 1)])
#> 
#> $node[[2]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> #
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(11, 0), (11, 1)])
#> 
#> 
query1[[3]]
#> $name
#> [1] "comment" "comment" "comment" "comment" "comment"
#> 
#> $node
#> $node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> #
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(14, 0), (14, 1)])
#> 
#> $node[[2]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> # 5
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(15, 0), (15, 3)])
#> 
#> $node[[3]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> #
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(16, 0), (16, 1)])
#> 
#> $node[[4]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> # 6
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(17, 0), (17, 3)])
#> 
#> $node[[5]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> #
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(18, 0), (18, 1)])
#> 
#> 

# Query 2 has 3 comment blocks that match (a different set than query 1!)
query2 <- matches[[2]]
query2[[1]]
#> $name
#> [1] "comment" "comment" "comment"
#> 
#> $node
#> $node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> # 1
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(1, 0), (1, 3)])
#> 
#> $node[[2]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> #
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(2, 0), (2, 1)])
#> 
#> $node[[3]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> # 2
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(3, 0), (3, 3)])
#> 
#> 
query2[[2]]
#> $name
#> [1] "comment" "comment"
#> 
#> $node
#> $node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> # 3
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(6, 0), (6, 3)])
#> 
#> $node[[2]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> # 4
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(7, 0), (7, 3)])
#> 
#> 
query2[[3]]
#> $name
#> [1] "comment" "comment" "comment" "comment" "comment"
#> 
#> $node
#> $node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> #
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(14, 0), (14, 1)])
#> 
#> $node[[2]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> # 5
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(15, 0), (15, 3)])
#> 
#> $node[[3]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> #
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(16, 0), (16, 1)])
#> 
#> $node[[4]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> # 6
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(17, 0), (17, 3)])
#> 
#> $node[[5]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> #
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (comment [(18, 0), (18, 1)])
#> 
#> 
```
