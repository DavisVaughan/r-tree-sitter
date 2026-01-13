# Query accessors

- `query_pattern_count()` returns the number of patterns in a query.

- `query_capture_count()` returns the number of captures in a query.

- `query_string_count()` returns the number of string literals in a
  query.

- `query_start_byte_for_pattern()` and `query_end_byte_for_pattern()`
  return the byte where the `i`th pattern starts/ends in the query
  `source`.

## Usage

``` r
query_pattern_count(x)

query_capture_count(x)

query_string_count(x)

query_start_byte_for_pattern(x, i)

query_end_byte_for_pattern(x, i)
```

## Arguments

- x:

  `[tree_sitter_query]`

  A query.

- i:

  `[double(1)]`

  The `i`th pattern to extract the byte for.

## Value

- `query_pattern_count()`, `query_capture_count()`, and
  `query_string_count()` return a single double count value.

- `query_start_byte_for_pattern()` and `query_end_byte_for_pattern()`
  return a single double for their respective byte if there was an `i`th
  pattern, otherwise they return `NA`.

## Examples

``` r
source <- '(binary_operator
  lhs: (identifier) @lhs
  operator: _ @operator
  rhs: (function_definition) @rhs
  (#eq? @lhs "fn")
)'
language <- treesitter.r::language()

query <- query(language, source)

query_pattern_count(query)
#> [1] 1
query_capture_count(query)
#> [1] 3
query_string_count(query)
#> [1] 2

query_start_byte_for_pattern(query, 1)
#> [1] 0
query_end_byte_for_pattern(query, 1)
#> [1] 120

text <- "
  fn <- function() {}
  fn2 <- function() {}
  fn <- 5
  fn <- function(a, b, c) { a + b + c }
"
parser <- parser(language)
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

query_matches(query, node)
#> [[1]]
#> [[1]][[1]]
#> [[1]][[1]]$name
#> [1] "lhs"      "operator" "rhs"     
#> 
#> [[1]][[1]]$node
#> [[1]][[1]]$node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(1, 2), (1, 4)])
#> 
#> [[1]][[1]]$node[[2]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> <-
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> "<-" [(1, 5), (1, 7)]
#> 
#> [[1]][[1]]$node[[3]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> function() {}
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (function_definition [(1, 8), (1, 21)]
#>   name: "function" [(1, 8), (1, 16)]
#>   parameters: (parameters [(1, 16), (1, 18)]
#>     open: "(" [(1, 16), (1, 17)]
#>     close: ")" [(1, 17), (1, 18)]
#>   )
#>   body: (braced_expression [(1, 19), (1, 21)]
#>     open: "{" [(1, 19), (1, 20)]
#>     close: "}" [(1, 20), (1, 21)]
#>   )
#> )
#> 
#> 
#> 
#> [[1]][[2]]
#> [[1]][[2]]$name
#> [1] "lhs"      "operator" "rhs"     
#> 
#> [[1]][[2]]$node
#> [[1]][[2]]$node[[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(4, 2), (4, 4)])
#> 
#> [[1]][[2]]$node[[2]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> <-
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> "<-" [(4, 5), (4, 7)]
#> 
#> [[1]][[2]]$node[[3]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> function(a, b, c) { a + b + c }
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (function_definition [(4, 8), (4, 39)]
#>   name: "function" [(4, 8), (4, 16)]
#>   parameters: (parameters [(4, 16), (4, 25)]
#>     open: "(" [(4, 16), (4, 17)]
#>     parameter: (parameter [(4, 17), (4, 18)]
#>       name: (identifier [(4, 17), (4, 18)])
#>     )
#>     (comma [(4, 18), (4, 19)])
#>     parameter: (parameter [(4, 20), (4, 21)]
#>       name: (identifier [(4, 20), (4, 21)])
#>     )
#>     (comma [(4, 21), (4, 22)])
#>     parameter: (parameter [(4, 23), (4, 24)]
#>       name: (identifier [(4, 23), (4, 24)])
#>     )
#>     close: ")" [(4, 24), (4, 25)]
#>   )
#>   body: (braced_expression [(4, 26), (4, 39)]
#>     open: "{" [(4, 26), (4, 27)]
#>     body: (binary_operator [(4, 28), (4, 37)]
#>       lhs: (binary_operator [(4, 28), (4, 33)]
#>         lhs: (identifier [(4, 28), (4, 29)])
#>         operator: "+" [(4, 30), (4, 31)]
#>         rhs: (identifier [(4, 32), (4, 33)])
#>       )
#> <truncated>
#> 
#> 
#> 
#> 
```
