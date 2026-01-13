# Get a node's child by index

These functions return the `i`th child of `x`.

- `node_child()` considers both named and anonymous children.

- `node_named_child()` considers only named children.

## Usage

``` r
node_child(x, i)

node_named_child(x, i)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

- i:

  `[integer(1)]`

  The index of the child to return.

## Value

The `i`th child node of `x` or `NULL` if there is no child at that
index.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# Starts with `program` node for the whole document
node
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn <- function() { 1 + 1 }
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (program [(0, 0), (0, 26)]
#>   (binary_operator [(0, 0), (0, 26)]
#>     lhs: (identifier [(0, 0), (0, 2)])
#>     operator: "<-" [(0, 3), (0, 5)]
#>     rhs: (function_definition [(0, 6), (0, 26)]
#>       name: "function" [(0, 6), (0, 14)]
#>       parameters: (parameters [(0, 14), (0, 16)]
#>         open: "(" [(0, 14), (0, 15)]
#>         close: ")" [(0, 15), (0, 16)]
#>       )
#>       body: (braced_expression [(0, 17), (0, 26)]
#>         open: "{" [(0, 17), (0, 18)]
#>         body: (binary_operator [(0, 19), (0, 24)]
#>           lhs: (float [(0, 19), (0, 20)])
#>           operator: "+" [(0, 21), (0, 22)]
#>           rhs: (float [(0, 23), (0, 24)])
#>         )
#>         close: "}" [(0, 25), (0, 26)]
#>       )
#>     )
#>   )
#> )

# Navigate to first child
node <- node_child(node, 1)
node
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn <- function() { 1 + 1 }
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (binary_operator [(0, 0), (0, 26)]
#>   lhs: (identifier [(0, 0), (0, 2)])
#>   operator: "<-" [(0, 3), (0, 5)]
#>   rhs: (function_definition [(0, 6), (0, 26)]
#>     name: "function" [(0, 6), (0, 14)]
#>     parameters: (parameters [(0, 14), (0, 16)]
#>       open: "(" [(0, 14), (0, 15)]
#>       close: ")" [(0, 15), (0, 16)]
#>     )
#>     body: (braced_expression [(0, 17), (0, 26)]
#>       open: "{" [(0, 17), (0, 18)]
#>       body: (binary_operator [(0, 19), (0, 24)]
#>         lhs: (float [(0, 19), (0, 20)])
#>         operator: "+" [(0, 21), (0, 22)]
#>         rhs: (float [(0, 23), (0, 24)])
#>       )
#>       close: "}" [(0, 25), (0, 26)]
#>     )
#>   )
#> )

# Note how the named variant skips the anonymous operator node
node_child(node, 2)
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> <-
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> "<-" [(0, 3), (0, 5)]
node_named_child(node, 2)
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> function() { 1 + 1 }
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (function_definition [(0, 6), (0, 26)]
#>   name: "function" [(0, 6), (0, 14)]
#>   parameters: (parameters [(0, 14), (0, 16)]
#>     open: "(" [(0, 14), (0, 15)]
#>     close: ")" [(0, 15), (0, 16)]
#>   )
#>   body: (braced_expression [(0, 17), (0, 26)]
#>     open: "{" [(0, 17), (0, 18)]
#>     body: (binary_operator [(0, 19), (0, 24)]
#>       lhs: (float [(0, 19), (0, 20)])
#>       operator: "+" [(0, 21), (0, 22)]
#>       rhs: (float [(0, 23), (0, 24)])
#>     )
#>     close: "}" [(0, 25), (0, 26)]
#>   )
#> )

# OOB indices return `NULL`
node_child(node, 5)
#> NULL
```
