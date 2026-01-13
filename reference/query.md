# Queries

`query()` lets you specify a query `source` string for use with
[`query_captures()`](https://davisvaughan.github.io/r-tree-sitter/reference/query-matches-and-captures.md)
and
[`query_matches()`](https://davisvaughan.github.io/r-tree-sitter/reference/query-matches-and-captures.md).
The `source` string is written in a way that is somewhat similar to the
idea of capture groups in regular expressions. You write out one or more
query patterns that match nodes in a tree, and then you "capture" parts
of those patterns with `@name` tags. The captures are the values
returned by
[`query_captures()`](https://davisvaughan.github.io/r-tree-sitter/reference/query-matches-and-captures.md)
and
[`query_matches()`](https://davisvaughan.github.io/r-tree-sitter/reference/query-matches-and-captures.md).
There are also a series of *predicates* that can be used to further
refine the query. Those are described in the
[`query_matches()`](https://davisvaughan.github.io/r-tree-sitter/reference/query-matches-and-captures.md)
help page.

Read the [tree-sitter
documentation](https://tree-sitter.github.io/tree-sitter/using-parsers/queries/index.html)
to learn more about the query syntax.

## Usage

``` r
query(language, source)
```

## Arguments

- language:

  `[tree_sitter_language]`

  A language.

- source:

  `[string]`

  A query source string.

## Value

A query.

## Storing queries

Query objects contain *external pointers*, so they cannot be saved to
disk and reloaded. One consequence of this is you cannot create them at
build time inside your package. For example, to precompile a query you
may assume you can create a global variable in your package with top
level code like this:

    QUERY <- treesitter::query(treesitter.r::language(), "query_source_text")

This won't work for two reasons:

- The external query in `QUERY` is created at package build time, and is
  no longer valid at package load time.

- The version of treesitter and treesitter.r are locked to the version
  used at build time, rather than at package load time.

The correct way to do this is to create the query on package load, like
this:

    QUERY <- NULL

    .onLoad <- function(libname, pkgname) {
      QUERY <<- treesitter::query(treesitter.r::language(), "query_source_text")
    }

This is one place where usage of `<<-` is acceptable.

## Examples

``` r
# This query looks for binary operators where the left hand side is an
# identifier named `fn`, and the right hand side is a function definition.
# The operator can be `<-` or `=` (technically it can also be things like
# `+` as well in this example).
source <- '(binary_operator
  lhs: (identifier) @lhs
  operator: _ @operator
  rhs: (function_definition) @rhs
  (#eq? @lhs "fn")
)'

language <- treesitter.r::language()

query <- query(language, source)

text <- "
  fn <- function() {}
  fn2 <- function() {}
  fn <- 5
  fn = function(a, b, c) { a + b + c }
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
#> =
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> "=" [(4, 5), (4, 6)]
#> 
#> [[1]][[2]]$node[[3]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> function(a, b, c) { a + b + c }
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (function_definition [(4, 7), (4, 38)]
#>   name: "function" [(4, 7), (4, 15)]
#>   parameters: (parameters [(4, 15), (4, 24)]
#>     open: "(" [(4, 15), (4, 16)]
#>     parameter: (parameter [(4, 16), (4, 17)]
#>       name: (identifier [(4, 16), (4, 17)])
#>     )
#>     (comma [(4, 17), (4, 18)])
#>     parameter: (parameter [(4, 19), (4, 20)]
#>       name: (identifier [(4, 19), (4, 20)])
#>     )
#>     (comma [(4, 20), (4, 21)])
#>     parameter: (parameter [(4, 22), (4, 23)]
#>       name: (identifier [(4, 22), (4, 23)])
#>     )
#>     close: ")" [(4, 23), (4, 24)]
#>   )
#>   body: (braced_expression [(4, 25), (4, 38)]
#>     open: "{" [(4, 25), (4, 26)]
#>     body: (binary_operator [(4, 27), (4, 36)]
#>       lhs: (binary_operator [(4, 27), (4, 32)]
#>         lhs: (identifier [(4, 27), (4, 28)])
#>         operator: "+" [(4, 29), (4, 30)]
#>         rhs: (identifier [(4, 31), (4, 32)])
#>       )
#> <truncated>
#> 
#> 
#> 
#> 
```
