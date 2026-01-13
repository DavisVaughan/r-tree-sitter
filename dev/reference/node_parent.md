# Get a node's parent

`node_parent()` looks up the tree and returns the current node's parent.

## Usage

``` r
node_parent(x)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

## Value

The parent node of `x` or `NULL` if there is no parent.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# Parent of a root node is `NULL`
node_parent(node)
#> NULL

node_function <- node |>
  node_child(1) |>
  node_child(3)

node_function
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

node_parent(node_function)
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
```
