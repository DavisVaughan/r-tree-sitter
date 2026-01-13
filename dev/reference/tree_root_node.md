# Retrieve the root node of the tree

`tree_root_node()` is the entry point for accessing nodes within a
specific tree. It returns the "root" of the tree, from which you can use
other `node_*()` functions to navigate around.

## Usage

``` r
tree_root_node(x)
```

## Arguments

- x:

  `[tree_sitter_tree]`

  A tree.

## Value

A node.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# Trees and nodes have a similar print method, but you can
# only use other `node_*()` functions on nodes.
tree
#> <tree_sitter_tree>
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

node |>
  node_child(1) |>
  node_children()
#> [[1]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(0, 0), (0, 2)])
#> 
#> [[2]]
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> <-
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> "<-" [(0, 3), (0, 5)]
#> 
#> [[3]]
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
#> 
```
